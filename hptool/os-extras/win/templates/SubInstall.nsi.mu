;; WARNING: {{productName}}.nsi is automatically generated from {{productName}}.nsi.mu by
;; the hptool.  Make sure you are editing the template not the generated file.


; {{productName}} Installer

;--------------------------------
;Includes

  !Include "FileFunc.nsh"
  !Include "StrFunc.nsh"
  !Include "LogicLib.nsh"
  !Include "MUI2.nsh"
  !Include "WordFunc.nsh"
  !Include "CommonHP.nsh"

;--------------------------------
;Defines

  !Define GHC_VERSION "{{ghcVersion}}"
  !Define PLATFORM_VERSION "{{hpVersion}}"
  !Define PRODUCT_DIR_REG_KEY "Software\Haskell\Haskell Platform\${PLATFORM_VERSION}"
  !Define HACKAGE_SHORTCUT_TEXT "HackageDB - Haskell Software Repository"
  !Define FILES_SOURCE_PATH "{{targetFiles}}"
  !Define INST_DAT "{{productName}}_inst.dat"
  !Define UNINST_DAT "{{productName}}_uninst.dat"
  !Define PKG_NAME "{{productName}}"

;--------------------------------
;Variables

  Var PROGRAM_FILES

;--------------------------------
;General settings

  ;Name and file
  Name "Haskell Platform ${PLATFORM_VERSION} {{is32or64}}-bit ${PKG_NAME}"
  OutFile "{{productFile}}"

  ;Default install dir
  InstallDir "$PROGRAMFILES\Haskell Platform\${PLATFORM_VERSION}"
  InstallDirRegKey HKLM "${PRODUCT_DIR_REG_KEY}" ""

  ;Icon
  !Define MUI_ICON "icons/installer.ico"
  !Define MUI_UNICON "icons/installer.ico"

  ;Request application privileges for Windows Vista
  RequestExecutionLevel highest

  ;Best available compression
  SetCompressor /SOLID lzma

  ;Install types
  InstType "Standard"
  InstType "Portable (just unpack the files)"

;--------------------------------
;Macros

;--------------------------------
;Callbacks

Function .onInit
  !insertmacro do64Stuff 1
  !insertmacro CheckAdmin "installer"
  SetShellVarContext all
FunctionEnd

Function un.onInit
  !insertmacro do64Stuff 0
  !insertmacro CheckAdmin "uninstaller"
  SetShellVarContext all
FunctionEnd

;--------------------------------
;Interface Settings

  !define MUI_ABORTWARNING

;--------------------------------
;Pages

  !Define MUI_WELCOMEFINISHPAGE_BITMAP "welcome.bmp"
  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_LICENSE "LICENSE"
  !insertmacro MUI_PAGE_DIRECTORY

  !Define MUI_COMPONENTSPAGE_NODESC
  !insertmacro MUI_PAGE_COMPONENTS

  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH

  !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  !insertmacro MUI_UNPAGE_FINISH

;--------------------------------
;Languages

  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "Base components" SecMain

  SectionIn 1 2
  ; Make this section mandatory
  SectionIn RO

  !Include ${INST_DAT}

SectionEnd

Section "Create uninstaller" SecAddRem

  SectionIn 1
  SectionIn RO

  ;Create uninstaller
  WriteUninstaller "$INSTDIR\{{productName}}_Uninstall.exe"

SectionEnd

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  !Include ${UNINST_DAT}

  Delete "$INSTDIR\{{productName}}_Uninstall.exe"

SectionEnd
