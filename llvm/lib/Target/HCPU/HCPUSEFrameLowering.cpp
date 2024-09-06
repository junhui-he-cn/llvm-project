//===-- HCPUSEFrameLowering.cpp - HCPU Frame Information ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the HCPU implementation of TargetFrameLowering class.
//
//===----------------------------------------------------------------------===//
#include "HCPUSEFrameLowering.h"

#include "HCPUFrameLowering.h"
#include "HCPUMachineFunction.h"
#include "HCPUSubtarget.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Target/TargetOptions.h"

using namespace llvm;

HCPUSEFrameLowering::HCPUSEFrameLowering(const HCPUSubtarget &STI)
  : HCPUFrameLowering(STI, STI.stackAlignment()) {}

void HCPUSEFrameLowering::emitPrologue(MachineFunction &MF, MachineBasicBlock &MBB) const {

}

void HCPUSEFrameLowering::emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const {

}

const HCPUFrameLowering *llvm::createHCPUSEFrameLowering(const HCPUSubtarget &ST) {
  return new HCPUSEFrameLowering(ST);
}