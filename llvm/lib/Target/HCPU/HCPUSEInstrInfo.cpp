//===-- HCPUSEInstrInfo.cpp - HCPU32/64 Instruction Information -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the HCPU32/64 implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "HCPUSEInstrInfo.h"

#include "HCPUMachineFunction.h"
#include "HCPUTargetMachine.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

HCPUSEInstrInfo::HCPUSEInstrInfo(const HCPUSubtarget &STI)
    : HCPUInstrInfo(STI), RI(STI) {}

const HCPURegisterInfo &HCPUSEInstrInfo::getRegisterInfo() const { return RI; }

const HCPUInstrInfo *llvm::createHCPUSEInstrInfo(const HCPUSubtarget &STI) {
  return new HCPUSEInstrInfo(STI);
}

/// Expand Pseudo instructions into real backend instructions
bool HCPUSEInstrInfo::expandPostRAPseudo(MachineInstr &MI) const {
  //@expandPostRAPseudo-body
  MachineBasicBlock &MBB = *MI.getParent();

  switch (MI.getDesc().getOpcode()) {
  default:
    return false;
  case HCPU::RetLR:
    expandRetLR(MBB, MI);
    break;
  case HCPU::HCPUeh_return32:
    expandEhReturn(MBB, MI);
    break;
  }

  MBB.erase(MI);
  return true;
}
void HCPUSEInstrInfo::expandRetLR(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator I) const {
  BuildMI(MBB, I, I->getDebugLoc(), get(HCPU::RET)).addReg(HCPU::LR);
}

/// Adjust SP by Amount bytes.
void HCPUSEInstrInfo::adjustStackPtr(unsigned SP, int64_t Amount,
                                     MachineBasicBlock &MBB,
                                     MachineBasicBlock::iterator I) const {
  DebugLoc DL = I != MBB.end() ? I->getDebugLoc() : DebugLoc();
  unsigned ADDu = HCPU::ADDu;
  unsigned ADDiu = HCPU::ADDiu;

  if (isInt<16>(Amount)) {
    // addiu sp, sp, amount
    BuildMI(MBB, I, DL, get(ADDiu), SP).addReg(SP).addImm(Amount);
  } else { // Expand immediate that doesn't fit in 16-bit.
    unsigned Reg = loadImmediate(Amount, MBB, I, DL, nullptr);
    BuildMI(MBB, I, DL, get(ADDu), SP).addReg(SP).addReg(Reg, RegState::Kill);
  }
}

/// This function generates the sequence of instructions needed to get the
/// result of adding register REG and immediate IMM.
unsigned HCPUSEInstrInfo::loadImmediate(int64_t Imm, MachineBasicBlock &MBB,
                                        MachineBasicBlock::iterator II,
                                        const DebugLoc &DL,
                                        unsigned *NewImm) const {
  HCPUAnalyzeImmediate AnalyzeImm;
  unsigned Size = 32;
  unsigned LUi = HCPU::LUi;
  unsigned ZEROReg = HCPU::ZERO;
  unsigned ATReg = HCPU::AT;
  bool LastInstrIsADDiu = NewImm;

  const HCPUAnalyzeImmediate::InstSeq &Seq =
      AnalyzeImm.Analyze(Imm, Size, LastInstrIsADDiu);
  HCPUAnalyzeImmediate::InstSeq::const_iterator Inst = Seq.begin();

  assert(Seq.size() && (!LastInstrIsADDiu || (Seq.size() > 1)));

  // The first instruction can be a LUi, which is different from other
  // instructions (ADDiu, ORI and SLL) in that it does not have a register
  // operand.
  if (Inst->Opc == LUi)
    BuildMI(MBB, II, DL, get(LUi), ATReg)
        .addImm(SignExtend64<16>(Inst->ImmOpnd));
  else
    BuildMI(MBB, II, DL, get(Inst->Opc), ATReg)
        .addReg(ZEROReg)
        .addImm(SignExtend64<16>(Inst->ImmOpnd));

  // Build the remaining instructions in Seq.
  for (++Inst; Inst != Seq.end() - LastInstrIsADDiu; ++Inst)
    BuildMI(MBB, II, DL, get(Inst->Opc), ATReg)
        .addReg(ATReg)
        .addImm(SignExtend64<16>(Inst->ImmOpnd));

  if (LastInstrIsADDiu)
    *NewImm = Inst->ImmOpnd;

  return ATReg;
}

void HCPUSEInstrInfo::storeRegToStack(MachineBasicBlock &MBB,
                                      MachineBasicBlock::iterator I,
                                      Register SrcReg, bool isKill, int FI,
                                      const TargetRegisterClass *RC,
                                      const TargetRegisterInfo *TRI,
                                      int64_t Offset) const {
  DebugLoc DL;
  MachineMemOperand *MMO = GetMemOperand(MBB, FI, MachineMemOperand::MOStore);

  unsigned Opc = 0;

  Opc = HCPU::ST;
  assert(Opc && "Register class not handled!");
  BuildMI(MBB, I, DL, get(Opc))
      .addReg(SrcReg, getKillRegState(isKill))
      .addFrameIndex(FI)
      .addImm(Offset)
      .addMemOperand(MMO);
}

void HCPUSEInstrInfo::loadRegFromStack(MachineBasicBlock &MBB,
                                       MachineBasicBlock::iterator I,
                                       Register DestReg, int FI,
                                       const TargetRegisterClass *RC,
                                       const TargetRegisterInfo *TRI,
                                       int64_t Offset) const {
  DebugLoc DL;
  if (I != MBB.end())
    DL = I->getDebugLoc();
  MachineMemOperand *MMO = GetMemOperand(MBB, FI, MachineMemOperand::MOLoad);
  unsigned Opc = 0;

  Opc = HCPU::LD;
  assert(Opc && "Register class not handled!");
  BuildMI(MBB, I, DL, get(Opc), DestReg)
      .addFrameIndex(FI)
      .addImm(Offset)
      .addMemOperand(MMO);
}

void HCPUSEInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator I,
                                  const DebugLoc &DL, MCRegister DestReg,
                                  MCRegister SrcReg, bool KillSrc) const {
  unsigned Opc = 0, ZeroReg = 0;

  if (HCPU::CPURegsRegClass.contains(DestReg)) { // Copy to CPU Reg.
    if (HCPU::CPURegsRegClass.contains(SrcReg))
      Opc = HCPU::ADDu, ZeroReg = HCPU::ZERO;
    else if (SrcReg == HCPU::HI)
      Opc = HCPU::MFHI, SrcReg = 0;
    else if (SrcReg == HCPU::LO)
      Opc = HCPU::MFLO, SrcReg = 0;
  } else if (HCPU::CPURegsRegClass.contains(SrcReg)) { // Copy from CPU Reg.
    if (DestReg == HCPU::HI)
      Opc = HCPU::MTHI, DestReg = 0;
    else if (DestReg == HCPU::LO)
      Opc = HCPU::MTLO, DestReg = 0;
  }

  assert(Opc && "Cannot copy registers");

  MachineInstrBuilder MIB = BuildMI(MBB, I, DL, get(Opc));

  if (DestReg)
    MIB.addReg(DestReg, RegState::Define);

  if (ZeroReg)
    MIB.addReg(ZeroReg);

  if (SrcReg)
    MIB.addReg(SrcReg, getKillRegState(KillSrc));
}

/// getOppositeBranchOpc - Return the inverse of the specified
/// opcode, e.g. turning BEQ to BNE.
unsigned HCPUSEInstrInfo::getOppositeBranchOpc(unsigned Opc) const {
  switch (Opc) {
  default:
    llvm_unreachable("Illegal opcode!");
  case HCPU::BEQ:
    return HCPU::BNE;
  case HCPU::BNE:
    return HCPU::BEQ;
  }
}


void HCPUSEInstrInfo::expandEhReturn(MachineBasicBlock &MBB,
                                     MachineBasicBlock::iterator I) const {
  // This pseudo instruction is generated as part of the lowering of
  // ISD::EH_RETURN. We convert it to a stack increment by OffsetReg, and
  // indirect jump to TargetReg
  unsigned ADDU = HCPU::ADDu;
  unsigned SP = HCPU::SP;
  unsigned LR = HCPU::LR;
  unsigned T9 = HCPU::T9;
  unsigned ZERO = HCPU::ZERO;
  unsigned OffsetReg = I->getOperand(0).getReg();
  unsigned TargetReg = I->getOperand(1).getReg();

  // addu $lr, $v0, $zero
  // addu $sp, $sp, $v1
  // jr   $lr (via RetLR)
  const TargetMachine &TM = MBB.getParent()->getTarget();
  if (TM.isPositionIndependent())
    BuildMI(MBB, I, I->getDebugLoc(), get(ADDU), T9)
        .addReg(TargetReg)
        .addReg(ZERO);
  BuildMI(MBB, I, I->getDebugLoc(), get(ADDU), LR)
      .addReg(TargetReg)
      .addReg(ZERO);
  BuildMI(MBB, I, I->getDebugLoc(), get(ADDU), SP).addReg(SP).addReg(OffsetReg);
  expandRetLR(MBB, I);
}