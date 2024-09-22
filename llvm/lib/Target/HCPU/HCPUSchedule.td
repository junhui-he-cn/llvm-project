//===-- HCPUSchedule.td - HCPU Scheduling Definitions ------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Functional units across HCPU chips sets. Based on GCC/HCPU backend files.
//===----------------------------------------------------------------------===//

def ALU : FuncUnit;
def IMULDIV : FuncUnit;

//===----------------------------------------------------------------------===//
// Instruction Itinerary classes used for HCPU
//===----------------------------------------------------------------------===//

def IIAlu : InstrItinClass;
def II_CLO : InstrItinClass;
def II_CLZ : InstrItinClass;
def IILoad : InstrItinClass;
def IIStore : InstrItinClass;
def IIBranch : InstrItinClass;

def IIPseudo : InstrItinClass;

def IIHiLo : InstrItinClass;
def IIImul : InstrItinClass;
def IIIdiv : InstrItinClass;

//===----------------------------------------------------------------------===//
// Cpu0 Generic instruction itineraries.
//===----------------------------------------------------------------------===//

def HCPUGenericItineraries : ProcessorItineraries<[ALU, IMULDIV], [], [
  InstrItinData<IIAlu, [InstrStage<1, [ALU]>]>,
  InstrItinData<II_CLO, [InstrStage<1, [ALU]>]>,
  InstrItinData<II_CLZ, [InstrStage<1, [ALU]>]>,
  InstrItinData<IILoad, [InstrStage<3, [ALU]>]>,
  InstrItinData<IIStore, [InstrStage<1, [ALU]>]>,
  InstrItinData<IIBranch, [InstrStage<1, [ALU]>]>,
  InstrItinData<IIHiLo, [InstrStage<1,  [IMULDIV]>]>,
  InstrItinData<IIImul, [InstrStage<17, [IMULDIV]>]>,
  InstrItinData<IIIdiv, [InstrStage<38, [IMULDIV]>]>,
]>;