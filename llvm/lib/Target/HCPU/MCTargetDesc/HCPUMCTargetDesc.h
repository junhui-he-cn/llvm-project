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
#include <memory>

namespace llvm {
class MCAsmBackend;
class MCCodeEmitter;
class MCContext;
class MCInstrInfo;
class MCObjectTargetWriter;
class MCRegisterInfo;
class MCSubtargetInfo;
class MCTargetOptions;
class StringRef;
class Target;
class Triple;
class raw_ostream;
class raw_pwrite_stream;

MCCodeEmitter *createHCPUMCCodeEmitterEB(const MCInstrInfo &MCII,
                                         MCContext &Ctx);
                                         
MCCodeEmitter *createHCPUMCCodeEmitterEL(const MCInstrInfo &MCII,
                                         MCContext &Ctx);

MCAsmBackend *createHCPUAsmBackend(const Target &T, const MCSubtargetInfo &STI,
                                   const MCRegisterInfo &MRI,
                                   const MCTargetOptions &Options);

std::unique_ptr<MCObjectTargetWriter>
createHCPUELFObjectWriter(const Triple &TT);

} // namespace llvm

#define GET_REGINFO_ENUM
#include "HCPUGenRegisterInfo.inc"

#define GET_INSTRINFO_ENUM
#include "HCPUGenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "HCPUGenSubtargetInfo.inc"

#endif