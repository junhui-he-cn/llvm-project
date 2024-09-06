//===-- HCPUSEInstrInfo.h - HCPU32/64 Instruction Information ---*- C++ -*-===//
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

#ifndef LLVM_LIB_TARGET_HCPU_HCPUSEINSTRINFO_H
#define LLVM_LIB_TARGET_HCPU_HCPUSEINSTRINFO_H

#include "HCPUInstrInfo.h"
#include "HCPUSERegisterInfo.h"
#include "HCPUMachineFunction.h"

namespace llvm {

class HCPUSEInstrInfo : public HCPUInstrInfo {
  const HCPUSERegisterInfo RI;

public:
  explicit HCPUSEInstrInfo(const HCPUSubtarget &STI);

  const HCPURegisterInfo &getRegisterInfo() const override;
  
};

}

#endif

