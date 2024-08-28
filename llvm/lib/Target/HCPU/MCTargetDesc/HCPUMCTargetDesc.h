//===-- HCPUMCTargetDesc.h - HCPU Target Descriptions -----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides HCPU specific target descriptions.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_HCPU_MCTARGETDESC_HCPUMCTARGETDESC_H
#define LLVM_LIB_TARGET_HCPU_MCTARGETDESC_HCPUMCTARGETDESC_H

#include "llvm/Support/DataTypes.h"


namespace llvm {
  class Target;
  class Triple;
}

#define GET_REGINFO_ENUM
#include "HCPUGenRegisterInfo.inc"

#define GET_INSTRINFO_ENUM
#include "HCPUGenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "HCPUGenSubtargetInfo.inc"

#endif