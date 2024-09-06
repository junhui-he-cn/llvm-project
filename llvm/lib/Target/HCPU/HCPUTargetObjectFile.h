//===-- llvm/Target/HCPUTargetObjectFile.h - HCPU Object Info ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_HCPU_HCPUTARGETOBJECTFILE_H
#define LLVM_LIB_TARGET_HCPU_HCPUTARGETOBJECTFILE_H

#include "HCPU.h"
#include "HCPUTargetMachine.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/MC/MCSection.h"

namespace llvm {
class HCPUTargetMachine;
  class HCPUTargetObjectFile : public TargetLoweringObjectFileELF {
    MCSection *SmallDataSection;
    MCSection *SmallBSSSection;
    const HCPUTargetMachine *TM;

  public:
    void Initialize(MCContext &Ctx, const TargetMachine &TM) override;
  };
}


#endif