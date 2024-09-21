//===-- HCPUFixupKinds.h - HCPU Specific Fixup Entries ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_HCPU_MCTARGETDESC_HCPUFIXUPKINDS_H
#define LLVM_LIB_TARGET_HCPU_MCTARGETDESC_HCPUFIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

namespace llvm {
namespace HCPU {
  // Although most of the current fixup types reflect a unique relocation
  // one can have multiple fixup types for a given relocation and thus need
  // to be uniquely named.
  //
  // This table *must* be in the save order of
  // MCFixupKindInfo Infos[HCPU::NumTargetFixupKinds]
  // in HCPUAsmBackend.cpp.
  //@Fixups {
  enum Fixups {
    //@ Pure upper 32 bit fixup resulting in - R_HCPU_32.
    fixup_HCPU_32 = FirstTargetFixupKind,

    // Pure upper 16 bit fixup resulting in - R_HCPU_HI16.
    fixup_HCPU_HI16,

    // Pure lower 16 bit fixup resulting in - R_HCPU_LO16.
    fixup_HCPU_LO16,

    // 16 bit fixup for GP offest resulting in - R_HCPU_GPREL16.
    fixup_HCPU_GPREL16,

    // GOT (Global Offset Table)
    // Symbol fixup resulting in - R_HCPU_GOT16.
    fixup_HCPU_GOT,

    

    // resulting in - R_HCPU_GOT_HI16
    fixup_HCPU_GOT_HI16,

    // resulting in - R_HCPU_GOT_LO16
    fixup_HCPU_GOT_LO16,

    // Marker
    LastTargetFixupKind,
    NumTargetFixupKinds = LastTargetFixupKind - FirstTargetFixupKind
  };
  //@Fixups }
} // namespace HCPU
} // namespace llvm

#endif // LLVM_HCPU_HCPUFIXUPKINDS_H