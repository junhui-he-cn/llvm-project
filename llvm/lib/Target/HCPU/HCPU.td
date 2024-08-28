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
}

def : ProcessorModel<"generic", NoSchedModel, []>;