//===-- HCPUISelDAGToDAG.cpp - A Dag to Dag Inst Selector for HCPU --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines an instruction selector for the HCPU target.
//
//===----------------------------------------------------------------------===//

#include "HCPUISelDAGToDAG.h"
#include "HCPU.h"

#include "HCPUMachineFunction.h"
#include "HCPURegisterInfo.h"
#include "HCPUSEISelDAGToDAG.h"
#include "HCPUTargetMachine.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/StackProtector.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Type.h"
#include "llvm/Pass.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include <cstddef>
using namespace llvm;

#define DEBUG_TYPE "hcpu-isel"

#define PASS_NAME "HCPU DAG->DAG Pattern Instruction Selection"

//===----------------------------------------------------------------------===//
// Instruction Selector Implementation
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// HCPUDAGToDAGISel - HCPU specific code to select HCPU machine
// instructions for SelectionDAG operations.
//===----------------------------------------------------------------------===//

void HCPUDAGToDAGISelLegacy::getAnalysisUsage(AnalysisUsage &AU) const {
  // There are multiple HCPUDAGToDAGISel instances added to the pass pipeline.
  // We need to preserve StackProtector for the next one.
  AU.addPreserved<StackProtector>();
  SelectionDAGISelLegacy::getAnalysisUsage(AU);
}

bool HCPUDAGToDAGISel::runOnMachineFunction(MachineFunction &MF) {
  bool Ret = SelectionDAGISel::runOnMachineFunction(MF);

  return Ret;
}

//@SelectAddr {
/// ComplexPattern used on HCPUInstrInfo
/// Used on HCPU Load/Store instructions
bool HCPUDAGToDAGISel::SelectAddr(SDNode *Parent, SDValue Addr, SDValue &Base,
                                  SDValue &Offset) {
  //@SelectAddr }
  EVT ValTy = Addr.getValueType();
  SDLoc DL(Addr);

  // If Parent is an unaligned f32 load or store, select a (base + index)
  // floating point load/store instruction (luxc1 or suxc1).
  const LSBaseSDNode *LS = 0;

  if (Parent && (LS = dyn_cast<LSBaseSDNode>(Parent))) {
    EVT VT = LS->getMemoryVT();

    if (VT.getSizeInBits() / 8 > LS->getAlign().value()) {
      assert(0 && "Unaligned loads/stores not supported for this type.");
      if (VT == MVT::f32)
        return false;
    }
  }

  // if Address is FI, get the TargetFrameIndex.
  if (FrameIndexSDNode *FIN = dyn_cast<FrameIndexSDNode>(Addr)) {
    Base = CurDAG->getTargetFrameIndex(FIN->getIndex(), ValTy);
    Offset = CurDAG->getTargetConstant(0, DL, ValTy);
    return true;
  }

  Base = Addr;
  Offset = CurDAG->getTargetConstant(0, DL, ValTy);
  return true;
}

//@Select {
/// Select instructions not customized! Used for
/// expanded, promoted and normal instructions
void HCPUDAGToDAGISel::Select(SDNode *Node) {
  //@Select }
  unsigned Opcode = Node->getOpcode();

  // If we have a custom node, we already have selected!
  if (Node->isMachineOpcode()) {
    LLVM_DEBUG(errs() << "== "; Node->dump(CurDAG); errs() << "\n");
    Node->setNodeId(-1);
    return;
  }

  // See if subclasses can handle this node.
  if (trySelect(Node))
    return;

  switch (Opcode) {
  default:
    break;
  }

  // Select the default instruction
  SelectCode(Node);
}

char HCPUDAGToDAGISelLegacy::ID = 0;

HCPUDAGToDAGISelLegacy::HCPUDAGToDAGISelLegacy(
    std::unique_ptr<SelectionDAGISel> S)
    : SelectionDAGISelLegacy(ID, std::move(S)) {}

INITIALIZE_PASS(HCPUDAGToDAGISelLegacy, DEBUG_TYPE, PASS_NAME, false, false)
