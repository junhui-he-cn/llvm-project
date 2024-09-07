//===-- HCPURegisterInfo.cpp - HCPU Register Information -== --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the HCPU implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#include "HCPURegisterInfo.h"

#include "HCPU.h"
#include "HCPUMachineFunction.h"
#include "HCPUSubtarget.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <cstdint>

#define DEBUG_TYPE "HCPU-reg-info"

#define GET_REGINFO_TARGET_DESC
#include "HCPUGenRegisterInfo.inc"

using namespace llvm;

HCPURegisterInfo::HCPURegisterInfo(const HCPUSubtarget &ST)
    : HCPUGenRegisterInfo(HCPU::LR), Subtarget(ST) { }

//===----------------------------------------------------------------------===//
// Callee Saved Registers methods
//===----------------------------------------------------------------------===//
/// Cpu0 Callee Saved Registers
// In Cpu0CallConv.td,
// def CSR_O32 : CalleeSavedRegs<(add LR, FP,
//                                   (sequence "S%u", 2, 0))>;
// llc create CSR_O32_SaveList and CSR_O32_RegMask from above defined.
const MCPhysReg *
HCPURegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  return CSR_O32_SaveList;
}

const uint32_t *
HCPURegisterInfo::getCallPreservedMask(const MachineFunction &MF,
                                       CallingConv::ID) const {
  return CSR_O32_RegMask;
}

BitVector HCPURegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  static const uint16_t ReservedCPURegs[] = {HCPU::ZERO, HCPU::AT, HCPU::SP,
                                             HCPU::LR, /*HCPU::SW,*/ HCPU::PC};
  BitVector Reserved(getNumRegs());

  for (unsigned I = 0; I < (sizeof(ReservedCPURegs) / sizeof(ReservedCPURegs[0])); ++I) 
    Reserved.set(ReservedCPURegs[I]);

  return Reserved;
}


//- If no eliminateFrameIndex(), it will hang on run. 
// pure virtual method
// FrameIndex represent objects inside a abstract stack.
// We must replace FrameIndex with an stack/frame pointer
// direct reference.
bool HCPURegisterInfo::
eliminateFrameIndex(MachineBasicBlock::iterator II, int SPAdj,
unsigned FIOperandNum, RegScavenger *RS) const {

}

bool HCPURegisterInfo::requiresRegisterScavenging(const MachineFunction &MF) const {
  return true;
}

bool HCPURegisterInfo::trackLivenessAfterRegAlloc(const MachineFunction &MF) const {
  return true;
}

Register HCPURegisterInfo::
getFrameRegister(const MachineFunction &MF) const {
  const TargetFrameLowering *TFI = MF.getSubtarget().getFrameLowering();
  return TFI->hasFP(MF) ? (HCPU::FP) : (HCPU::SP);
}