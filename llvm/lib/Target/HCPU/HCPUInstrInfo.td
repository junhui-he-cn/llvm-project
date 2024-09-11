//===- HCPUInstrInfo.td - Target Description for HCPU Target -*- tablegen -*-=//
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

//===----------------------------------------------------------------------===//
// HCPU profiles and nodes
//===----------------------------------------------------------------------===//

def SDT_HCPURet : SDTypeProfile<0, 1, [SDTCisInt<0>]>;

def HCPURet : SDNode<"HCPUISD::Ret", SDTNone, [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

//===----------------------------------------------------------------------===//
// Cpu0 Instruction Predicate Definitions.
//===----------------------------------------------------------------------===//

def EnableOverflow : Predicate<"Subtarget->enableOverflow()">;
def DisableOverflow : Predicate<"Subtarget->disableOverflow()">;

def HasCmp : Predicate<"Subtarget->hasCmp()">;
def HasSlt : Predicate<"Subtarget->hasSlt()">;

//===----------------------------------------------------------------------===//
// Instruction format superclass
//===----------------------------------------------------------------------===//

include "HCPUInstrFormats.td"

//===----------------------------------------------------------------------===//
// Cpu0 Operand, Complex Patterns and Transformations Definitions.
//===----------------------------------------------------------------------===//

def simm16 : Operand<i32> {
  let DecoderMethod = "DecodeSimm16";
}

def mem : Operand<iPTR> {
  let PrintMethod = "printMemOperand";
  let MIOperandInfo = (ops GPROut, simm16);
  let EncoderMethod = "getMemEncoding";
}

def immSExt16 : PatLeaf<(imm), [{ return isInt<16>(N->getSExtValue()); }]>;

def addr:
  ComplexPattern<iPTR, 2, "SelectAddr", [frameindex], [SDNPWantParent]>;

class AlignedLoad<PatFrag Node> :
  PatFrag<(ops node:$ptr), (Node node:$ptr), [{
  LoadSDNode *LD = cast<LoadSDNode>(N);
  return LD->getMemoryVT().getSizeInBits()/8 <= LD->getAlignment();
}]>;

class AlignedStore<PatFrag Node> :
  PatFrag<(ops node:$val, node:$ptr), (Node node:$val, node:$ptr), [{
  StoreSDNode *SD = cast<StoreSDNode>(N);
  return SD->getMemoryVT().getSizeInBits()/8 <= SD->getAlignment();
}]>;

def load_a : AlignedLoad<load>;
def store_a : AlignedStore<store>;

class ArithLogicI<bits<8> op, string instr_asm, SDNode OpNode,
                  Operand Od, PatLeaf imm_type, RegisterClass RC> : 
  FL<op, (outs GPROut:$ra), (ins RC:$rb, Od:$imm16),
     !strconcat(instr_asm, "\t$ra, $rb, $imm16"),
     [(set GPROut:$ra, (OpNode RC:$rb, imm_type:$imm16))], IIAlu> {
  let isReMaterializable = 1;
}

class FMem<bits<8> op, dag outs, dag ins, string asmstr, list<dag> pattern,
          InstrItinClass itin>: FL<op, outs, ins, asmstr, pattern, itin> {
  bits<20> addr;
  let Inst{19-16} = addr{19-16};
  let Inst{15-0} = addr{15-0};
  let DecoderMethod = "DecodeMem";
}

let canFoldAsLoad = 1 in
class LoadM<bits<8> op, string instr_asm, PatFrag OpNode, RegisterClass RC,
            Operand MemOpnd, bit Pseudo>:
  FMem<op, (outs RC:$ra), (ins MemOpnd:$addr),
      !strconcat(instr_asm, "\t$ra, $addr"),
      [(set RC:$ra, (OpNode addr:$addr))], IILoad> {
  let isPseudo = Pseudo;
}

class StoreM<bits<8> op, string instr_asm, PatFrag OpNode, RegisterClass RC,
            Operand MemOpnd, bit Pseudo>:
  FMem<op, (outs), (ins RC:$ra, MemOpnd:$addr),
      !strconcat(instr_asm, "\t$ra, $addr"),
      [(OpNode RC:$ra, addr:$addr)], IIStore> {
  let isPseudo = Pseudo;
}

class LoadM32<bits<8> op, string instr_asm, PatFrag OpNode, bit Pseudo = 0>
  : LoadM<op, instr_asm, OpNode, GPROut, mem, Pseudo> {
  
}

class StoreM32<bits<8> op, string instr_asm, PatFrag OpNode, bit Pseudo = 0>
  : StoreM<op, instr_asm, OpNode, GPROut, mem, Pseudo> {
  
}

let isBranch = 1, isTerminator = 1, isBarrier = 1, imm16 = 0,
    hasDelaySlot = 1, isIndirectBranch = 1 in
class JumpFR<bits<8> op, string instr_asm, RegisterClass RC> :
  FL<op, (outs), (ins RC:$ra),
    !strconcat(instr_asm, "\t$ra"), [(brind RC:$ra)], IIBranch> {
  let rb = 0;
  let imm16 = 0;
}

class RetBase<RegisterClass RC> : JumpFR<0x3c, "ret", RC> {
  let isReturn = 1;
  let isCodeGenOnly = 1;
  let hasCtrlDep = 1;
  let hasExtraSrcRegAllocReq = 1;
}

// Instruction Definition
def LD : LoadM32<0x01, "ld", load_a>;
def ST : StoreM32<0x02, "st", store_a>;

def ADDiu : ArithLogicI<0x09, "addiu", add, simm16, immSExt16, CPURegs>;

def JR : JumpFR<0x3c, "jr", GPROut>;
def RET : RetBase<GPROut>;

let addr = 0 in
  def NOP : FJ<0, (outs), (ins), "nop", [], IIAlu>; 

def : Pat<(i32 immSExt16: $in),
          (ADDiu ZERO, imm:$in)>;