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

  // PC relative branch fixup resulting in - R_HCPU_PC16.
  // HCPU PC16, e.g. beq
  fixup_HCPU_PC16,

  // PC relative branch fixup resulting in - R_HCPU_PC24.
  // HCPU PC24, e.g. jeq, jmp
  fixup_HCPU_PC24,

  // resulting in - R_HCPU_GOT_HI16
  fixup_HCPU_GOT_HI16,

  // resulting in - R_HCPU_GOT_LO16
  fixup_HCPU_GOT_LO16,

  // resulting in - R_HCPU_CALL16.
  fixup_HCPU_CALL16,

      // resulting in - R_HCPU_TLS_GD.
    fixup_HCPU_TLSGD,

    // resulting in - R_HCPU_TLS_GOTTPREL.
    fixup_HCPU_GOTTPREL,

    // resulting in - R_HCPU_TLS_TPREL_HI16.
    fixup_HCPU_TP_HI,

    // resulting in - R_HCPU_TLS_TPREL_LO16.
    fixup_HCPU_TP_LO,

    // resulting in - R_HCPU_TLS_LDM.
    fixup_HCPU_TLSLDM,

    // resulting in - R_HCPU_TLS_DTP_HI16.
    fixup_HCPU_DTP_HI,

    // resulting in - R_HCPU_TLS_DTP_LO16.
    fixup_HCPU_DTP_LO,

  // Marker
  LastTargetFixupKind,
  NumTargetFixupKinds = LastTargetFixupKind - FirstTargetFixupKind
};
//@Fixups }
} // namespace HCPU
} // namespace llvm

#endif // LLVM_HCPU_HCPUFIXUPKINDS_H