//===-- HCPUTargetStreamer.cpp - HCPU Target Streamer Methods -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides HCPU specific target streamer methods.
//
//===----------------------------------------------------------------------===//

#include "InstPrinter/HCPUInstPrinter.h"
#include "HCPUMCTargetDesc.h"
#include "HCPUTargetObjectFile.h"
#include "HCPUTargetStreamer.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSectionELF.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

HCPUTargetStreamer::HCPUTargetStreamer(MCStreamer &S)
    : MCTargetStreamer(S) {
}

HCPUTargetAsmStreamer::HCPUTargetAsmStreamer(MCStreamer &S,
                                             formatted_raw_ostream &OS)
    : HCPUTargetStreamer(S), OS(OS) {}

