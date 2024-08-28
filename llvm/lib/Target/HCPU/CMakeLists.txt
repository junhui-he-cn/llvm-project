add_llvm_component_group(HCPU)

set(LLVM_TARGET_DEFINITIONS HCPUOther.td)

# Generate HCPUGenRegisterInfo.inc and HCPUGenInstrInfo.inc which included by 
#  your hand code C++ files. 
# HCPUGenRegisterInfo.inc came from HCPURegisterInfo.td, HCPUGenInstrInfo.inc 
#  came from HCPUInstrInfo.td.
tablegen(LLVM HCPUGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM HCPUGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM HCPUGenSubtargetInfo.inc -gen-subtarget)
tablegen(LLVM HCPUGenMCPseudoLowering,inc -gen-pseudo-lowering)

# HCPUCommonTableGen must be defined
add_public_tablegen_target(HCPUCommonTableGen)

add_llvm_target(HCPUCodeGen
  HCPUTargetMachine.cpp

  LINK_COMPONENTS
  Analysis
  AsmPrinter
  CodeGen
  Core
  MC
  HCPUDesc
  HCPUInfo
  SelectionDAG
  Support
  Target
  GlobalIsel

  ADD_TO_COMPONENT
  HCPU
)

add_subdirectory(TargetInfo)
add_subdirectory(MCTargetDesc)