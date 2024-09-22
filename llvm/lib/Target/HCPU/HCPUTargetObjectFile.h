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

  bool IsGlobalInSmallSection(const GlobalObject *GO, const TargetMachine &TM,
                              SectionKind Kind) const;
  bool IsGlobalInSmallSectionImpl(const GlobalObject *GO,
                                  const TargetMachine &TM) const;

public:
  void Initialize(MCContext &Ctx, const TargetMachine &TM) override;

  /// IsGlobalInSmallSection - Return true if this global address should be
  /// placed into small data/bss section.
  bool IsGlobalInSmallSection(const GlobalObject *GV,
                              const TargetMachine &TM) const;

  MCSection *SelectSectionForGlobal(const GlobalObject *GO, SectionKind Kind,
                                    const TargetMachine &TM) const override;
};
} // namespace llvm

#endif