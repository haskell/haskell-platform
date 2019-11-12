;; WARNING: MSys.nsi is automatically generated from MSys.nsi.mu by
;; the hptool.  Make sure you are editing the template not the generated file.


; MSys Installer

;--------------------------------
;Includes

  !Include "FileFunc.nsh"
  !Include "StrFunc.nsh"
  !Include "LogicLib.nsh"
  !Include "MUI2.nsh"
  !Include "WordFunc.nsh"
  !Include "x64.nsh"

;--------------------------------
;Defines

  !Define GHC_VERSION "{{ghcVersion}}"
  !Define PLATFORM_VERSION "{{hpVersion}}"
  !Define PRODUCT_DIR_REG_KEY "Software\Haskell\Haskell Platform\${PLATFORM_VERSION}"
  !Define HACKAGE_SHORTCUT_TEXT "HackageDB - Haskell Software Repository"
  !Define FILES_SOURCE_PATH "{{targetFiles}}"
  !Define INST_DAT "MSys_inst.dat"
  !Define UNINST_DAT "MSys_uninst.dat"

;--------------------------------
;Variables

  Var PROGRAM_FILES

;--------------------------------
;General settings

  ;Name and file
  Name "Haskell Platform ${PLATFORM_VERSION} {{is32or64}}-bit"
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

!macro CheckAdmin thing
UserInfo::GetAccountType
pop $0
${If} $0 != "admin" ;Require admin rights on NT4+
    MessageBox MB_YESNO "It is recommended to run this ${thing} as administrator. Do you want to quit and restart the ${thing} manually with elevated privileges?" IDNO CheckAdminDone
    SetErrorLevel 740 ;ERROR_ELEVATION_REQUIRED
    Quit
${EndIf}
CheckAdminDone:
!macroend

  ;--------------------------------
  ;Win 64-bit support

!macro do64Stuff isInstall
  ; The NSIS installer is a 32-bit executable, but it can do a 64-bit install.
  ; Default to 32-bit, change if installing 64-bit on 64-bit.
  ;
  ; The 'isInstall' argument is 1 for the install part of the script (from
  ; .onInit function) and 0 if for the uninstall part (via un.onInit).  The
  ; $INSTDIR must be changed for the installation step to account for the case
  ; of installing the 32-bit installer onto 64-bit Windows; and it must
  ; happen before the user gets to the dialog to change installation location.
  ; On the other hand, $INSTDIR must *not* be changed for the uninstall step
  ; because doing so over-rides what the user did during the install step.
  ;
  ; Also, do not force $INSTDIR to change if this is a silent install.
SetRegView 32
StrCpy $PROGRAM_FILES "$PROGRAMFILES"
${IfNot} ${Silent}
  ${If} ${isInstall} = 1
    StrCpy $INSTDIR "$PROGRAM_FILES\Haskell Platform\${PLATFORM_VERSION}"
  ${EndIf}
${EndIf}
{{#build64bit}}
${If} ${RunningX64}
  ; If this is installing the 64-bit HP on 64-bit Windows, enable FSRedirection.
  ${EnableX64FSRedirection}
  ; enable access to 64-bit portion of registry
  SetRegView 64
  StrCpy $PROGRAM_FILES "$PROGRAMFILES64"
  ${IfNot} ${Silent}
    ${If} ${isInstall} = 1
      StrCpy $INSTDIR "$PROGRAM_FILES\Haskell Platform\${PLATFORM_VERSION}"
    ${EndIf}
  ${EndIf}
${Else}
;     pop up an error message: Cannot install 64-bit HP on 32-bit Windows
    MessageBox MB_OK "You are trying to install the 64-bit version of the Haskell Platform onto a 32-bit version of Windows.  Please use the 32-bit version of the Haskell Platform."
    SetErrorLevel 0x800401FAL ; CO_E_WRONGOSFORAPP
    Quit
${EndIf}
{{/build64bit}}
!macroend

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
  WriteUninstaller "$INSTDIR\MSys_Uninstall.exe"

SectionEnd

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  !Include ${UNINST_DAT}

  Delete "$INSTDIR\MSys_Uninstall.exe"

SectionEnd
