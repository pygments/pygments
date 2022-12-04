## @file
# EFI/PI MdePkg Package
#
# Copyright (c) 2007 - 2022, Intel Corporation. All rights reserved.<BR>
# Portions copyright (c) 2008 - 2009, Apple Inc. All rights reserved.<BR>
# (C) Copyright 2020 Hewlett Packard Enterprise Development LP<BR>
# Copyright (c) 2022, Loongson Technology Corporation Limited. All rights reserved.<BR>
#
#    SPDX-License-Identifier: BSD-2-Clause-Patent
#
##

[Defines]
  PLATFORM_NAME                  = Mde
  PLATFORM_GUID                  = 082F8BFC-0455-4859-AE3C-ECD64FB81642
  PLATFORM_VERSION               = 1.08
  DSC_SPECIFICATION              = 0x00010005
  OUTPUT_DIRECTORY               = Build/Mde
  SUPPORTED_ARCHITECTURES        = IA32|X64|EBC|ARM|AARCH64|RISCV64|LOONGARCH64
  BUILD_TARGETS                  = DEBUG|RELEASE|NOOPT
  SKUID_IDENTIFIER               = DEFAULT

!include UnitTestFrameworkPkg/UnitTestFrameworkPkgTarget.dsc.inc

!include MdePkg/MdeLibs.dsc.inc

[PcdsFeatureFlag]
  gEfiMdePkgTokenSpaceGuid.PcdUgaConsumeSupport|TRUE

[PcdsFixedAtBuild]
  gEfiMdePkgTokenSpaceGuid.PcdDebugPropertyMask|0x0f
  gEfiMdePkgTokenSpaceGuid.PcdDebugPrintErrorLevel|0x80000000
  gEfiMdePkgTokenSpaceGuid.PcdPciExpressBaseAddress|0xE0000000

[LibraryClasses]
  SafeIntLib|MdePkg/Library/BaseSafeIntLib/BaseSafeIntLib.inf

[Components]
  MdePkg/Library/UefiFileHandleLib/UefiFileHandleLib.inf
  MdePkg/Library/BaseCacheMaintenanceLib/BaseCacheMaintenanceLib.inf
  MdePkg/Library/BaseCacheMaintenanceLibNull/BaseCacheMaintenanceLibNull.inf
  MdePkg/Library/BaseCpuLib/BaseCpuLib.inf
  MdePkg/Library/BaseCpuLibNull/BaseCpuLibNull.inf
  MdePkg/Library/BaseDebugLibNull/BaseDebugLibNull.inf
  MdePkg/Library/BaseDebugLibSerialPort/BaseDebugLibSerialPort.inf
  MdePkg/Library/BaseDebugPrintErrorLevelLib/BaseDebugPrintErrorLevelLib.inf
  MdePkg/Library/BaseLib/BaseLib.inf
  MdePkg/Library/BaseMemoryLib/BaseMemoryLib.inf
  MdePkg/Library/BaseOrderedCollectionRedBlackTreeLib/BaseOrderedCollectionRedBlackTreeLib.inf
  MdePkg/Library/BasePcdLibNull/BasePcdLibNull.inf
  MdePkg/Library/BasePciCf8Lib/BasePciCf8Lib.inf
  MdePkg/Library/BasePciExpressLib/BasePciExpressLib.inf
  MdePkg/Library/SmmCpuRendezvousLibNull/SmmCpuRendezvousLibNull.inf

[Components.IA32, Components.X64, Components.ARM, Components.AARCH64]
  #
  # Add UEFI Target Based Unit Tests
  #
  MdePkg/Test/UnitTest/Library/BaseLib/BaseLibUnitTestsUefi.inf

  #
  # Build PEIM, DXE_DRIVER, SMM_DRIVER, UEFI Shell components that test SafeIntLib
  #
  MdePkg/Test/UnitTest/Library/BaseSafeIntLib/TestBaseSafeIntLibPei.inf
  MdePkg/Test/UnitTest/Library/BaseSafeIntLib/TestBaseSafeIntLibDxe.inf
  MdePkg/Test/UnitTest/Library/BaseSafeIntLib/TestBaseSafeIntLibSmm.inf
  MdePkg/Test/UnitTest/Library/BaseSafeIntLib/TestBaseSafeIntLibUefiShell.inf

[Components.IA32, Components.X64, Components.AARCH64]
  MdePkg/Library/BaseRngLib/BaseRngLib.inf

[Components.IA32, Components.X64]
  MdePkg/Library/BaseIoLibIntrinsic/BaseIoLibIntrinsic.inf
  MdePkg/Library/BaseIoLibIntrinsic/BaseIoLibIntrinsicSev.inf
  MdePkg/Library/BaseMemoryLibMmx/BaseMemoryLibMmx.inf

[Components.EBC]
  MdePkg/Library/BaseIoLibIntrinsic/BaseIoLibIntrinsic.inf
  MdePkg/Library/UefiRuntimeLib/UefiRuntimeLib.inf

[Components.ARM, Components.AARCH64]
  MdePkg/Library/BaseIoLibIntrinsic/BaseIoLibIntrinsicArmVirt.inf
  MdePkg/Library/BaseStackCheckLib/BaseStackCheckLib.inf

[BuildOptions]
