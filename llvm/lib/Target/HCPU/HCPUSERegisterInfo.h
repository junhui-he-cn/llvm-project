//===-- HCPUSERegisterInfo.h - HCPU32 Register Information ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the HCPU32/64 implementation of the TargetRegisterInfo
// class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_HCPU_HCPUSEREGISTERINFO_H
#define LLVM_LIB_TARGET_HCPU_HCPUSEREGISTERINFO_H

#include "HCPURegisterInfo.h"
#include "HCPUSubtarget.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"

namespace llvm {
class HCPUSEInstrInfo;
class HCPUSERegisterInfo : public HCPURegisterInfo {
public:
  HCPUSERegisterInfo(const HCPUSubtarget &Subtarget);

  const TargetRegisterClass *intRegClass(unsigned Size) const override;
};
} // namespace llvm

#endif