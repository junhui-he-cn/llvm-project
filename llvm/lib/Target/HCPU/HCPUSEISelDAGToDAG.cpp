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

#define DEBUG_TYPE "HCPU-isel"

void HCPUSEDAGToDAGISelLegacy::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<DominatorTreeWrapperPass>();
  SelectionDAGISelLegacy::getAnalysisUsage(AU);
}

bool HCPUSEDAGToDAGISel::runOnMachineFunction(MachineFunction &MF) {
  Subtarget = &static_cast<const HCPUSubtarget &>(MF.getSubtarget());
  return HCPUDAGToDAGISel::runOnMachineFunction(MF);
}

void HCPUSEDAGToDAGISel::processFunctionAfterISel(MachineFunction &MF) {}

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

