//===-- HCPUSEISelDAGToDAG.cpp - A Dag to Dag Inst Selector for HCPUSE ----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Subclass of HCPUDAGToDAGISel specialized for HCPU32.
//
//===----------------------------------------------------------------------===//

#include "HCPUSEISelDAGToDAG.h"

#include "HCPU.h"
#include "HCPUISelDAGToDAG.h"
#include "HCPUMachineFunction.h"
#include "HCPURegisterInfo.h"
#include "MCTargetDesc/HCPUBaseInfo.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include <memory>
using namespace llvm;

#define DEBUG_TYPE "hcpu-se-isel"

void HCPUSEDAGToDAGISelLegacy::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<DominatorTreeWrapperPass>();
  SelectionDAGISelLegacy::getAnalysisUsage(AU);
}

bool HCPUSEDAGToDAGISel::runOnMachineFunction(MachineFunction &MF) {
  Subtarget = &static_cast<const HCPUSubtarget &>(MF.getSubtarget());
  return HCPUDAGToDAGISel::runOnMachineFunction(MF);
}

void HCPUSEDAGToDAGISel::processFunctionAfterISel(MachineFunction &MF) {}

/// Select multiply instructions.
std::pair<SDNode *, SDNode *>
HCPUSEDAGToDAGISel::selectMULT(SDNode *N, unsigned Opc, const SDLoc &DL, EVT Ty,
                               bool HasLo, bool HasHi) {
  SDNode *Lo = 0, *Hi = 0;
  SDNode *Mul = CurDAG->getMachineNode(Opc, DL, MVT::Glue, N->getOperand(0),
                                       N->getOperand(1));
  SDValue InFlag = SDValue(Mul, 0);

  if (HasLo) {
    Lo = CurDAG->getMachineNode(HCPU::MFLO, DL, Ty, MVT::Glue, InFlag);
    InFlag = SDValue(Lo, 1);
  }
  if (HasHi)
    Hi = CurDAG->getMachineNode(HCPU::MFHI, DL, Ty, InFlag);

  return std::make_pair(Lo, Hi);
}

void HCPUSEDAGToDAGISel::selectAddESubE(unsigned MOp, SDValue InFlag,
                                        SDValue CmpLHS, const SDLoc &DL,
                                        SDNode *Node) const {
  unsigned Opc = InFlag.getOpcode();
  (void)Opc;
  assert(((Opc == ISD::ADDC || Opc == ISD::ADDE) ||
          (Opc == ISD::SUBC || Opc == ISD::SUBE)) &&
         "(ADD|SUB)E flag operand must come from (ADD|SUB)C/E insn");

  SDValue Ops[] = {CmpLHS, InFlag.getOperand(1)};
  SDValue LHS = Node->getOperand(0), RHS = Node->getOperand(1);
  EVT VT = LHS.getValueType();

  SDNode *Carry;
  if (Subtarget->hasHCPU32II())
    Carry = CurDAG->getMachineNode(HCPU::SLTu, DL, VT, Ops);
  else {
    SDNode *StatusWord = CurDAG->getMachineNode(HCPU::CMP, DL, VT, Ops);
    SDValue Constant1 = CurDAG->getTargetConstant(1, DL, VT);
    Carry = CurDAG->getMachineNode(HCPU::ANDi, DL, VT, SDValue(StatusWord, 0),
                                   Constant1);
  }
  SDNode *AddCarry =
      CurDAG->getMachineNode(HCPU::ADDu, DL, VT, SDValue(Carry, 0), RHS);

  CurDAG->SelectNodeTo(Node, MOp, VT, MVT::Glue, LHS, SDValue(AddCarry, 0));
}

//@selectNode
bool HCPUSEDAGToDAGISel::trySelect(SDNode *Node) {
  unsigned Opcode = Node->getOpcode();
  SDLoc DL(Node);

  ///
  // Instruction Selection not handled by the auto-generated
  // tablegen selection should be handled here.
  ///

  ///
  // Instruction Selection not handled by the auto-generated
  // tablegen selection should be handled here.
  ///
  EVT NodeTy = Node->getValueType(0);
  unsigned MultOpc;

  switch (Opcode) {
  default:
    break;
  case ISD::MULHS:
  case ISD::MULHU: {
    MultOpc = (Opcode == ISD::MULHU ? HCPU::MULTu : HCPU::MULT);
    auto LoHi = selectMULT(Node, MultOpc, DL, NodeTy, false, true);
    ReplaceNode(Node, LoHi.second);
    return true;
  }
  case ISD::Constant: {
    const ConstantSDNode *CN = dyn_cast<ConstantSDNode>(Node);
    unsigned Size = CN->getValueSizeInBits(0);

    if (Size == 32)
      break;

    return true;
  }
  case ISD::SUBE: {
    SDValue InFlag = Node->getOperand(2);
    selectAddESubE(HCPU::SUBu, InFlag, InFlag.getOperand(0), DL, Node);
    return true;
  }

  case ISD::ADDE: {
    SDValue InFlag = Node->getOperand(2);
    selectAddESubE(HCPU::ADDu, InFlag, InFlag.getValue(0), DL, Node);
    return true;
  }

  /// Mul with two results
  case ISD::SMUL_LOHI:
  case ISD::UMUL_LOHI: {
    MultOpc = (Opcode == ISD::UMUL_LOHI ? HCPU::MULTu : HCPU::MULT);

    std::pair<SDNode *, SDNode *> LoHi =
        selectMULT(Node, MultOpc, DL, NodeTy, true, true);

    if (!SDValue(Node, 0).use_empty())
      ReplaceUses(SDValue(Node, 0), SDValue(LoHi.first, 0));

    if (!SDValue(Node, 1).use_empty())
      ReplaceUses(SDValue(Node, 1), SDValue(LoHi.second, 0));

    CurDAG->RemoveDeadNode(Node);
    return true;
  }
  }

  return false;
}

HCPUSEDAGToDAGISelLegacy::HCPUSEDAGToDAGISelLegacy(HCPUTargetMachine &TM,
                                                   CodeGenOptLevel OptLevel)
    : HCPUDAGToDAGISelLegacy(
          std::make_unique<HCPUSEDAGToDAGISel>(TM, OptLevel)) {}

FunctionPass *llvm::createHCPUSEISelDag(HCPUTargetMachine &TM,
                                        CodeGenOptLevel OptLevel) {
  return new HCPUSEDAGToDAGISelLegacy(TM, OptLevel);
}
