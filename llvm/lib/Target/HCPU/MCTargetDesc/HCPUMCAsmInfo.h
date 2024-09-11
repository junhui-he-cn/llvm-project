//===-- HCPUMCAsmInfo.h - HCPU Asm Info ------------------------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the HCPUMCAsmInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_HCPU_MCTARGETDESC_HCPUMCASMINFO_H
#define LLVM_LIB_TARGET_HCPU_MCTARGETDESC_HCPUMCASMINFO_H


#include "llvm/MC/MCAsmInfoELF.h"

namespace llvm {
  class Triple;

  class HCPUMCAsmInfo : public MCAsmInfoELF {
    void anchor() override;
  public:
    explicit HCPUMCAsmInfo(const Triple &TheTriple);
  };

} // namespace llvm


#endif