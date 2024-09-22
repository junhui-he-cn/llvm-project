//===-- HCPUSEISelDAGToDAG.h - A Dag to Dag Inst Selector for HCPUSE -----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Subclass of HCPUDAGToDAGISel specialized for HCPU32.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_HCPU_HCPUSEISELDAGTODAG_H
#define LLVM_LIB_TARGET_HCPU_HCPUSEISELDAGTODAG_H

#include "HCPUISelDAGToDAG.h"
#include "llvm/Support/CodeGen.h"

namespace llvm {

class HCPUSEDAGToDAGISel : public HCPUDAGToDAGISel {

public:
  explicit HCPUSEDAGToDAGISel(HCPUTargetMachine &TM, CodeGenOptLevel OL)
      : HCPUDAGToDAGISel(TM, OL) {}

private:
  bool runOnMachineFunction(MachineFunction &MF) override;

  bool trySelect(SDNode *Node) override;

  void processFunctionAfterISel(MachineFunction &MF) override;

  // Insert instructions to initialize the global base register in the
  // first MBB of the function.
  //  void initGlobalBaseReg(MachineFunction &MF);

  std::pair<SDNode *, SDNode *> selectMULT(SDNode *N, unsigned Opc,
                                           const SDLoc &DL, EVT Ty, bool HasLo,
                                           bool HasHi);

  void selectAddESubE(unsigned MOp, SDValue InFlag, SDValue CmpLHS,
                      const SDLoc &DL, SDNode *Node) const;
};

class HCPUSEDAGToDAGISelLegacy : public HCPUDAGToDAGISelLegacy {
public:
  explicit HCPUSEDAGToDAGISelLegacy(HCPUTargetMachine &TM, CodeGenOptLevel OL);
  void getAnalysisUsage(AnalysisUsage &AU) const override;
};

FunctionPass *createHCPUSEISelDag(HCPUTargetMachine &TM,
                                  CodeGenOptLevel OptLevel);

} // namespace llvm

#endif
