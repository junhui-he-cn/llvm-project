//===-- HCPUSERegisterInfo.cpp - HCPU Register Information ------== -------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the HCPU implementation of the TargetRegisterInfo
// class.
//
//===----------------------------------------------------------------------===//

#include "HCPUSERegisterInfo.h"

using namespace llvm;

#define DEBUG_TYPE "hcpu-reg-info"

HCPUSERegisterInfo::HCPUSERegisterInfo(const HCPUSubtarget &ST)
  : HCPURegisterInfo(ST) {}

const TargetRegisterClass *
HCPUSERegisterInfo::intRegClass(unsigned Size) const {
  return &HCPU::CPURegsRegClass;
}