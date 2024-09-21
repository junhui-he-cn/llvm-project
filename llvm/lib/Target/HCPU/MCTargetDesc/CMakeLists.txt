add_llvm_component_library(LLVMHCPUDesc
  HCPUABIInfo.cpp
  HCPUMCTargetDesc.cpp
  HCPUMCAsmInfo.cpp
  HCPUAsmBackend.cpp
  HCPUMCCodeEmitter.cpp
  HCPUMCExpr.cpp
  HCPUELFObjectWriter.cpp
  HCPUTargetStreamer.cpp

  LINK_COMPONENTS
  MC
  HCPUInfo
  HCPUAsmPrinter
  Support

  ADD_TO_COMPONENT
  HCPU
)