add_llvm_component_library(LLVMHCPUInfo
  HCPUTargetInfo.cpp

  LINK_COMPONENTS
  Support

  ADD_TO_COMPONENT
  HCPU
)