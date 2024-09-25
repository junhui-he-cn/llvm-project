//===-- HCPU.td - Describe the HCPU Target Machine ---------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Calling Conv, Instruction Descriptions
//===----------------------------------------------------------------------===//

include "HCPUSchedule.td"
include "HCPUInstrInfo.td"
include "HCPUCallingConv.td"

def HCPUAsmParser : AsmParser {
  let ShouldEmitMatchRegisterName = 0;
}

def HCPUAsmParserVariant : AsmParserVariant {
  int Variant = 0;

  // Recognize hard coded registers.
  string RegisterPrefix = "$";
}

def HCPUInstrInfo : InstrInfo;
//===----------------------------------------------------------------------===//
// HCPU processors supported.
//===----------------------------------------------------------------------===//

// Will generate HCPUGenAsmWrite.inc included by HCPUInstPrinter.cpp, contents 
//  as follows,
// void HCPUInstPrinter::printInstruction(const MCInst *MI, raw_ostream &O) {...}
// const char *HCPUInstPrinter::getRegisterName(unsigned RegNo) {...}
def HCPU : Target {
// def HCPUInstrInfo : InstrInfo as before.
  let InstructionSet = HCPUInstrInfo;
  let AssemblyParsers = [HCPUAsmParser];
  let AssemblyParserVariants = [HCPUAsmParserVariant];
}

def : ProcessorModel<"generic", NoSchedModel, []>;

def FeatureCmp : SubtargetFeature<"cmp", "HasCmp", "true", "Enable cmp instructions">;
def FeatureSlt : SubtargetFeature<"slt", "HasSlt", "true", "Enable slt instructions">;

def FeatureHCPU32I : SubtargetFeature<"hcpu32I", "HCPUArchVersion", "HCPU32I",
  "HCPU32I ISA Support", [FeatureCmp]>;

def FeatureHCPU32II : SubtargetFeature<"hcpu32II", "HCPUArchVersion", "HCPU32II",
  "HCPU32I ISA Support (slt)", [FeatureCmp, FeatureSlt]>;

class Proc<string Name, list<SubtargetFeature> Features>
 :Processor<Name, HCPUGenericItineraries, Features>;

def : Proc<"hcpu32I", [FeatureHCPU32I]>;
def : Proc<"hcpu32II", [FeatureHCPU32II]>;