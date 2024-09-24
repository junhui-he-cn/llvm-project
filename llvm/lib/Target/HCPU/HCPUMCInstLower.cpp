//===-- HCPUMCInstLower.cpp - Convert HCPU MachineInstr to MCInst ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains code to lower HCPU MachineInstrs to their corresponding
// MCInst records.
//
//===----------------------------------------------------------------------===//

#include "HCPUMCInstLower.h"

#include "HCPUAsmPrinter.h"
#include "HCPUInstrInfo.h"
#include "MCTargetDesc/HCPUBaseInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/IR/Mangler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"

using namespace llvm;

HCPUMCInstLower::HCPUMCInstLower(HCPUAsmPrinter &asmprinter)
    : AsmPrinter(asmprinter) {}

void HCPUMCInstLower::Initialize(MCContext *C) { Ctx = C; }

static void CreateMCInst(MCInst &Inst, unsigned Opc, const MCOperand &Opnd0,
                         const MCOperand &Opnd1,
                         const MCOperand &Opnd2 = MCOperand()) {
  Inst.setOpcode(Opc);
  Inst.addOperand(Opnd0);
  Inst.addOperand(Opnd1);
  if (Opnd2.isValid())
    Inst.addOperand(Opnd2);
}

//@LowerOperand {
MCOperand HCPUMCInstLower::LowerOperand(const MachineOperand &MO,
                                        unsigned offset) const {
  MachineOperandType MOTy = MO.getType();

  switch (MOTy) {
  //@2
  default:
    llvm_unreachable("unknown operand type");
  case MachineOperand::MO_Register:
    // Ignore all implicit register operands.
    if (MO.isImplicit())
      break;
    return MCOperand::createReg(MO.getReg());
  case MachineOperand::MO_Immediate:
    return MCOperand::createImm(MO.getImm() + offset);
  case MachineOperand::MO_RegisterMask:
    break;
  case MachineOperand::MO_GlobalAddress:
  case MachineOperand::MO_ExternalSymbol:
    return LowerSymbolOperand(MO, MOTy, offset);
  }

  return MCOperand();
}

void HCPUMCInstLower::Lower(const MachineInstr *MI, MCInst &OutMI) const {
  OutMI.setOpcode(MI->getOpcode());

  if (lowerLongBranch(MI, OutMI))
    return;

  for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
    const MachineOperand &MO = MI->getOperand(i);
    MCOperand MCOp = LowerOperand(MO);

    if (MCOp.isValid())
      OutMI.addOperand(MCOp);
  }
}

// Lower ".cpload $reg" to
//  "lui   $gp, %hi(_gp_disp)"
//  "addiu $gp, $gp, %lo(_gp_disp)"
//  "addu  $gp, $gp, $t9"
void HCPUMCInstLower::LowerCPLOAD(SmallVector<MCInst, 4> &MCInsts) {
  MCOperand GPReg = MCOperand::createReg(HCPU::GP);
  MCOperand T9Reg = MCOperand::createReg(HCPU::T9);
  StringRef SymName("_gp_disp");
  const MCSymbol *Sym = Ctx->getOrCreateSymbol(SymName);
  const HCPUMCExpr *MCSym;

  MCSym = HCPUMCExpr::create(Sym, HCPUMCExpr::CEK_ABS_HI, *Ctx);
  MCOperand SymHi = MCOperand::createExpr(MCSym);
  MCSym = HCPUMCExpr::create(Sym, HCPUMCExpr::CEK_ABS_LO, *Ctx);
  MCOperand SymLo = MCOperand::createExpr(MCSym);

  MCInsts.resize(3);

  CreateMCInst(MCInsts[0], HCPU::LUi, GPReg, SymHi);
  CreateMCInst(MCInsts[1], HCPU::ORi, GPReg, GPReg, SymLo);
  CreateMCInst(MCInsts[2], HCPU::ADD, GPReg, GPReg, T9Reg);
}

MCOperand HCPUMCInstLower::LowerSymbolOperand(const MachineOperand &MO,
                                              MachineOperandType MOTy,
                                              unsigned Offset) const {
  MCSymbolRefExpr::VariantKind Kind = MCSymbolRefExpr::VK_None;
  HCPUMCExpr::HCPUExprKind TargetKind = HCPUMCExpr::CEK_None;
  const MCSymbol *Symbol;

  switch (MO.getTargetFlags()) {
  default:
    llvm_unreachable("Invalid target flag!");
  case HCPUII::MO_NO_FLAG:
    break;

    // HCPU_GPREL is for llc -march=HCPU -relocation-model=static -HCPU-islinux-
    //  format=false (global var in .sdata).
  case HCPUII::MO_GPREL:
    TargetKind = HCPUMCExpr::CEK_GPREL;
    break;

  case HCPUII::MO_GOT:
    TargetKind = HCPUMCExpr::CEK_GOT;
    break;
    // ABS_HI and ABS_LO is for llc -march=HCPU -relocation-model=static (global
    //  var in .data).
  case HCPUII::MO_ABS_HI:
    TargetKind = HCPUMCExpr::CEK_ABS_HI;
    break;
  case HCPUII::MO_ABS_LO:
    TargetKind = HCPUMCExpr::CEK_ABS_LO;
    break;
  case HCPUII::MO_GOT_HI16:
    TargetKind = HCPUMCExpr::CEK_GOT_HI16;
    break;
  case HCPUII::MO_GOT_LO16:
    TargetKind = HCPUMCExpr::CEK_GOT_LO16;
    break;
  case HCPUII::MO_GOT_CALL:
    TargetKind = HCPUMCExpr::CEK_GOT_CALL;
    break;
  }

  switch (MOTy) {
  case MachineOperand::MO_GlobalAddress:
    Symbol = AsmPrinter.getSymbol(MO.getGlobal());
    Offset += MO.getOffset();
    break;
  case MachineOperand::MO_ExternalSymbol:
    Symbol = AsmPrinter.GetExternalSymbolSymbol(MO.getSymbolName());
    Offset += MO.getOffset();
    break;
  default:
    llvm_unreachable("<unknown operand type>");
  }

  const MCExpr *Expr = MCSymbolRefExpr::create(Symbol, Kind, *Ctx);

  if (Offset) {
    // Assume offset is never negative.
    assert(Offset > 0);
    Expr = MCBinaryExpr::createAdd(Expr, MCConstantExpr::create(Offset, *Ctx),
                                   *Ctx);
  }

  if (TargetKind != HCPUMCExpr::CEK_None)
    Expr = HCPUMCExpr::create(TargetKind, Expr, *Ctx);

  return MCOperand::createExpr(Expr);
}

MCOperand HCPUMCInstLower::createSub(MachineBasicBlock *BB1,
                                     MachineBasicBlock *BB2,
                                     HCPUMCExpr::HCPUExprKind Kind) const {
  const MCSymbolRefExpr *Sym1 = MCSymbolRefExpr::create(BB1->getSymbol(), *Ctx);
  const MCSymbolRefExpr *Sym2 = MCSymbolRefExpr::create(BB2->getSymbol(), *Ctx);
  const MCBinaryExpr *Sub = MCBinaryExpr::createSub(Sym1, Sym2, *Ctx);

  return MCOperand::createExpr(HCPUMCExpr::create(Kind, Sub, *Ctx));
}

void HCPUMCInstLower::lowerLongBranchLUi(const MachineInstr *MI,
                                         MCInst &OutMI) const {
  OutMI.setOpcode(HCPU::LUi);

  // Lower register operand.
  OutMI.addOperand(LowerOperand(MI->getOperand(0)));

  // Create %hi($tgt-$baltgt).
  OutMI.addOperand(createSub(MI->getOperand(1).getMBB(),
                             MI->getOperand(2).getMBB(),
                             HCPUMCExpr::CEK_ABS_HI));
}

void HCPUMCInstLower::lowerLongBranchADDiu(
    const MachineInstr *MI, MCInst &OutMI, int Opcode,
    HCPUMCExpr::HCPUExprKind Kind) const {
  OutMI.setOpcode(Opcode);

  // Lower two register operands.
  for (unsigned I = 0, E = 2; I != E; ++I) {
    const MachineOperand &MO = MI->getOperand(I);
    OutMI.addOperand(LowerOperand(MO));
  }

  // Create %lo($tgt-$baltgt) or %hi($tgt-$baltgt).
  OutMI.addOperand(
      createSub(MI->getOperand(2).getMBB(), MI->getOperand(3).getMBB(), Kind));
}

bool HCPUMCInstLower::lowerLongBranch(const MachineInstr *MI,
                                      MCInst &OutMI) const {
  switch (MI->getOpcode()) {
  default:
    return false;
  case HCPU::LONG_BRANCH_LUi:
    lowerLongBranchLUi(MI, OutMI);
    return true;
  case HCPU::LONG_BRANCH_ADDiu:
    lowerLongBranchADDiu(MI, OutMI, HCPU::ADDiu, HCPUMCExpr::CEK_ABS_LO);
    return true;
  }
}

#ifdef ENABLE_GPRESTORE
// Lower ".cprestore offset" to "st $gp, offset($sp)".
void HCPUMCInstLower::LowerCPRESTORE(int64_t Offset,
                                     SmallVector<MCInst, 4>& MCInsts) {
  assert(isInt<32>(Offset) && (Offset >= 0) &&
         "Imm operand of .cprestore must be a non-negative 32-bit value.");

  MCOperand SPReg = MCOperand::createReg(HCPU::SP), BaseReg = SPReg;
  MCOperand GPReg = MCOperand::createReg(HCPU::GP);
  MCOperand ZEROReg = MCOperand::createReg(HCPU::ZERO);

  if (!isInt<16>(Offset)) {
    unsigned Hi = ((Offset + 0x8000) >> 16) & 0xffff;
    Offset &= 0xffff;
    MCOperand ATReg = MCOperand::createReg(HCPU::AT);
    BaseReg = ATReg;

    // lui   at,hi
    // add   at,at,sp
    MCInsts.resize(2);
    CreateMCInst(MCInsts[0], HCPU::LUi, ATReg, ZEROReg, MCOperand::createImm(Hi));
    CreateMCInst(MCInsts[1], HCPU::ADD, ATReg, ATReg, SPReg);
  }

  MCInst St;
  CreateMCInst(St, HCPU::ST, GPReg, BaseReg, MCOperand::createImm(Offset));
  MCInsts.push_back(St);
}
#endif