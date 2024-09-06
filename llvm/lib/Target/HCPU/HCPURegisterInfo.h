//===-- HCPURegisterInfo.h - HCPU Register Information Impl -----*- C++ -*-===//
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

#ifndef LLVM_LIB_TARGET_HCPU_HCPUREGISTERINFO_H
#define LLVM_LIB_TARGET_HCPU_HCPUREGISTERINFO_H

#include "HCPU.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"

#define GET_REGINFO_HEADER
#include "HCPUGenRegisterInfo.inc"

namespace llvm {
class HCPUSubtarget;
class TargetInstrInfo;
class Type;

class HCPURegisterInfo : public HCPUGenRegisterInfo {
protected:
  const HCPUSubtarget &Subtarget;

public:
  HCPURegisterInfo(const HCPUSubtarget &Subtarget);

  const MCPhysReg *getCalleeSavedRegs(const MachineFunction *MF) const override;

  const uint32_t *getCallPreservedMask(const MachineFunction &MF,
                                       CallingConv::ID) const override;

  BitVector getReservedRegs(const MachineFunction &MF) const override;

  bool requiresRegisterScavenging(const MachineFunction &MF) const override;

  bool trackLivenessAfterRegAlloc(const MachineFunction &MF) const override;

  bool eliminateFrameIndex(MachineBasicBlock::iterator II, int SPAdj,
                           unsigned FIOperandNum,
                           RegScavenger *RS = nullptr) const override;

  Register getFrameRegister(const MachineFunction &MF) const override;

  virtual const TargetRegisterClass *intRegClass(unsigned Size) const = 0;
};
} // namespace llvm

#endif