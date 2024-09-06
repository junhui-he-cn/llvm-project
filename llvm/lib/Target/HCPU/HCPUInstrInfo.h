//===-- HCPUInstrInfo.h - HCPU Instruction Information ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the HCPU implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_HCPU_HCPUINSTRINFO_H
#define LLVM_LIB_TARGET_HCPU_HCPUINSTRINFO_H

#include "HCPU.h"
#include "HCPURegisterInfo.h"
#include "llvm/CodeGen/TargetInstrInfo.h"


#define GET_INSTRINFO_HEADER
#include "HCPUGenInstrInfo.inc"

namespace llvm {

class HCPUInstrInfo : public HCPUGenInstrInfo {
  virtual void anchor();

protected:
  const HCPUSubtarget &Subtarget;

public:
  explicit HCPUInstrInfo(const HCPUSubtarget &STI);

  static const HCPUInstrInfo *create(HCPUSubtarget &STI);

  /// getRegisterInfo - TargetInstrInfo is a superset of MRegister info.  As
  /// such, whenever a client has an instance of instruction info, it should
  /// always be able to get register info as well (through this method).
  ///
  virtual const HCPURegisterInfo &getRegisterInfo() const = 0;

  /// Return the number of bytes of code the specified instruction may be.
  unsigned GetInstSizeInBytes(const MachineInstr &MI) const;
};

const HCPUInstrInfo *createHCPUSEInstrInfo(const HCPUSubtarget &STI);
} // namespace llvm

#endif