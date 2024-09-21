//===-- HCPUTargetStreamer.h - HCPU Target Streamer ------------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_HCPU_HCPUTARGETSTREAMER_H
#define LLVM_LIB_TARGET_HCPU_HCPUTARGETSTREAMER_H


#include "llvm/MC/MCELFStreamer.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/FormattedStream.h"

namespace llvm {

class HCPUTargetStreamer : public MCTargetStreamer {
public:
  HCPUTargetStreamer(MCStreamer &S);
};

// This part is for ascii assembly output
class HCPUTargetAsmStreamer : public HCPUTargetStreamer {
  formatted_raw_ostream &OS;

public:
  HCPUTargetAsmStreamer(MCStreamer &S, formatted_raw_ostream &OS);
};

}

#endif

