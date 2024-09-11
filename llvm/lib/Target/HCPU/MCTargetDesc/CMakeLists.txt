add_llvm_component_library(LLVMHCPUDesc
  HCPUABIInfo.cpp
  HCPUMCTargetDesc.cpp
  HCPUMCAsmInfo.cpp

  LINK_COMPONENTS
  MC
  HCPUInfo
  HCPUAsmPrinter
  Support

  ADD_TO_COMPONENT
  HCPU
)