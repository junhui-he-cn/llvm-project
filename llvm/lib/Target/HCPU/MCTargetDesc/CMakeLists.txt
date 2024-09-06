add_llvm_component_library(LLVMHCPUDesc
  HCPUABIInfo.cpp
  HCPUMCTargetDesc.cpp

  LINK_COMPONENTS
  MC
  HCPUInfo
  Support

  ADD_TO_COMPONENT
  HCPU
)