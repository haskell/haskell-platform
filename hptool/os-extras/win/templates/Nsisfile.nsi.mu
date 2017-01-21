;; WARNING: NsisFile.nsi is automatically generated from NsisFile.nsi.mu by
;; the hptool.  Make sure you are editing NsisFile.nsi.mu, not NsisFile.nsi.


; Haskell Platform Installer

;--------------------------------
;Includes

  !Include "EnvVarUpdate.nsh"
  !Include "FileFunc.nsh"
  !Include "StrFunc.nsh"
  !Include "LogicLib.nsh"
  !Include "MUI2.nsh"
  !Include "WordFunc.nsh"
  !Include "CreateInternetShortcut.nsh"
  !Include "x64.nsh"
  !insertmacro GetParameters
  !insertmacro GetOptions

;--------------------------------
;Defines

  !Define GHC_VERSION "{{ghcVersion}}"
  !Define PLATFORM_VERSION "{{hpVersion}}"
  !Define PRODUCT_DIR_REG_KEY "Software\Haskell\Haskell Platform\${PLATFORM_VERSION}"
  !Define HACKAGE_SHORTCUT_TEXT "HackageDB - Haskell Software Repository"
  !Define FILES_SOURCE_PATH "{{targetFiles}}"

;--------------------------------
;Variables

  Var START_MENU_FOLDER
  Var PROGRAM_FILES
  Var STACK_INSTALL_DIR

;--------------------------------
;General settings

  ;Name and file
  Name "Haskell Platform ${PLATFORM_VERSION} {{is32or64}}-bit"
  OutFile "{{productFile}}"

  ;Default install dir
  ; Set as appropriate for 32-bit or 64-bit OS
{{#build64bit}}
  InstallDir "$PROGRAMFILES64\Haskell Platform\${PLATFORM_VERSION}"
{{/build64bit}}

{{^build64bit}}
  InstallDir "$PROGRAMFILES\Haskell Platform\${PLATFORM_VERSION}"
{{/build64bit}}

  InstallDirRegKey HKLM "${PRODUCT_DIR_REG_KEY}" ""

  ;Icon
  !Define MUI_ICON "icons/installer.ico"
  !Define MUI_UNICON "icons/installer.ico"

  ;Request application privileges for Windows Vista
  RequestExecutionLevel highest

  ;Best available compression
  SetCompressor lzma

  ;Install types
  InstType "Standard"
  InstType "Portable (just unpack the files)"

  ;Used on the UI Pages
  BrandingText "Haskell.org"

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
{{#build64bit}}
${If} ${RunningX64}
  ; If this is installing the 64-bit HP on 64-bit Windows, enable FSRedirection.
  ${EnableX64FSRedirection}
  ; enable access to 64-bit portion of registry
  SetRegView 64
  StrCpy $PROGRAM_FILES "$PROGRAMFILES64"
${Else}
;     pop up an error message: Cannot install 64-bit HP on 32-bit Windows
    MessageBox MB_OK "You are trying to install the 64-bit version of the Haskell Platform onto a 32-bit version of Windows.  Please use the 32-bit version of the Haskell Platform."
    SetErrorLevel 0x800401FAL ; CO_E_WRONGOSFORAPP
    Quit
${EndIf}
{{/build64bit}}

!macroend

;--------------------------------
;Warn about other HP installs

!macro CheckForOthers loopN
StrCpy $0 0
loop${loopN}:
  EnumRegKey $1 HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall" $0
  StrCmp $1 "" done${loopN}
  ${StrStr} $2 $1 "HaskellPlatform-"
  IntOp $0 $0 + 1
  StrCmp $2 "" loop${loopN}
  MessageBox MB_YESNO|MB_ICONEXCLAMATION "You have other/older versions of the Haskell Platform installed.$\n\
$\n\
Due to how the PATH environment variable is used (by cabal, etc.) to find the needed GHC toolset, currently only one Haskell Platform can be used at a time.  While it is possible to manually modify the PATH variable to work-around this, it is not recommended.$\n\
$\n\
Unless you uninstall those other version(s), the Haskell Platform ${PLATFORM_VERSION} will likely have problems due to conflicts over which GHC and tools are found on the PATH.$\n\
$\n\
It is recommended that you cancel installing the Haskell Platform ${PLATFORM_VERSION}, uninstall the other/older versions, then try again to install the Haskell Platform ${PLATFORM_VERSION}.$\n\
$\n\
Do you want to quit the installation now?" IDYES quit${loopN} IDNO CheckOtherInstallsDone

quit${loopN}:
  Quit

done${loopN}:
!macroend



!macro CheckOtherInstalls

{{#build64bit}}
SetRegView 64
!insertmacro CheckForOthers 1
SetRegView 32
{{/build64bit}}

!insertmacro CheckForOthers 2

CheckOtherInstallsDone:

{{#build64bit}}
SetRegView 64
{{/build64bit}}

!macroend


;--------------------------------
;Callbacks

Function .onInit
  !insertmacro do64Stuff 1
  !insertmacro CheckAdmin "installer"
  !insertmacro CheckOtherInstalls
  SetShellVarContext all
  SectionSetSize 0 {{mainInstalledSize}} ; 0 is ${SecMain} but the symbol is not defined yet
  SectionSetSize 1 {{stackInstalledSize}} ; 1 is ${SecStack}
  ; /STACK switch to the HP installer for setting the stack install dir
  ${GetParameters} $R0
  ${GetOptions} $R0 "/STACK=" $STACK_INSTALL_DIR
  StrCmp $STACK_INSTALL_DIR "" +2
    StrCpy $STACK_INSTALL_DIR "/D=$STACK_INSTALL_DIR"
FunctionEnd

Function un.onInit
  !insertmacro do64Stuff 0
  !insertmacro CheckAdmin "uninstaller"
  SetShellVarContext all
FunctionEnd

Function .onInstSuccess
  SetOutPath "$INSTDIR\bin"
  ExecWait '"$INSTDIR\bin\ghc-pkg" recache'

;   IfFileExists $SYSDIR\glut32.dll Done
;     MessageBox MB_YESNO "It looks like the GLUT library is not installed on your computer. Do you want to install GLUT?" IDNO Done
;         SetOutPath "$SYSDIR\."
;         File "${FILES_SOURCE_PATH}\etc\glut32.dll"
;   Done:
FunctionEnd

;--------------------------------
Function un.CreateGUID
  System::Call 'ole32::CoCreateGuid(g .s)'
FunctionEnd

;--------------------------------
;Interface Settings

  !define MUI_ABORTWARNING
  !define MUI_FINISHPAGE_NOAUTOCLOSE
  !define MUI_UNFINISHPAGE_NOAUTOCLOSE

;--------------------------------
;Pages

  !Define MUI_WELCOMEFINISHPAGE_BITMAP "welcome.bmp"
  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_LICENSE "LICENSE"
  !insertmacro MUI_PAGE_DIRECTORY

  !Define MUI_COMPONENTSPAGE_NODESC ; this allows a wider text box
  !insertmacro MUI_PAGE_COMPONENTS

  ;Start Menu Folder Page Configuration
  !Define MUI_PAGE_HEADER_SUBTEXT \
      "Choose a Start Menu folder for the Haskell Platform ${PLATFORM_VERSION} shortcuts."
  !Define MUI_STARTMENUPAGE_TEXT_TOP \
      "Select the Start Menu folder in which you would like to create Haskell Platform shortcuts. You can also enter a name to create a new folder."
  !Define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKLM"
  !Define MUI_STARTMENUPAGE_REGISTRY_KEY "${PRODUCT_DIR_REG_KEY}"
  !Define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"
  !Define MUI_STARTMENUPAGE_DEFAULTFOLDER "Haskell Platform ${PLATFORM_VERSION}"
  !insertmacro MUI_PAGE_STARTMENU StartMenuPage $START_MENU_FOLDER
  !insertmacro MUI_PAGE_INSTFILES

  ;Finish page
  ; No amount of shenanigans could get the default string to be referenced
  ; properly, just to add a bit more, so we just need to hardcode it here :(
  !define MUI_FINISHPAGE_TEXT \
      "$(^NameDA) has been installed on your computer.$\r$\n$\r$\nClick Finish to close this wizard.$\r$\n$\r$\n(You may notice a window briefly appear while GHC package.cache is refreshed.)"
  !insertmacro MUI_PAGE_FINISH

  !insertmacro MUI_UNPAGE_WELCOME
  ; !insertmacro MUI_UNPAGE_COMPONENTS ; if we wanted to allow partial uninstall
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

  ;Meta-installer should not try to re-compress the payloads!
  SetCompress off

  ; Put the install of stack here so it is first, since it requires additional
  ; user interaction, and putting it near the end makes the user have to wait.
  ${If} ${SectionIsSelected} 1 ; ${SecStack} but the symbol is not defined yet
    DetailPrint "extracting Stack"
    SetOutPath "$INSTDIR\temp"
    File "{{stackInstallerPath}}"
    DetailPrint "installing Stack"
    SetDetailsPrint listonly
    ${If} ${Silent}
      ExecWait '"$INSTDIR\temp\{{stackInstallerFileName}}" /S $STACK_INSTALL_DIR'
    ${Else}
      ExecWait '"$INSTDIR\temp\{{stackInstallerFileName}}" $STACK_INSTALL_DIR'
    ${EndIf}
    SetDetailsPrint lastused
    DetailPrint "finished Stack"
    Delete "$INSTDIR\temp\{{stackInstallerFileName}}"
    SetOutPath "$INSTDIR" ; RMDIR on a dir will not work if it is the CWD
    RMDir "$INSTDIR\temp"
  ${EndIf}

  ; 1. MSys
  DetailPrint "extracting MSys"
  SetOutPath "$INSTDIR\temp"
  File "..\product\MSys-setup.exe"
  DetailPrint "installing MSys"
  SetDetailsPrint listonly
    ExecWait '"$INSTDIR\temp\MSys-setup.exe" /S /D=$INSTDIR'
  SetDetailsPrint lastused
  DetailPrint "finished MSys"
  Delete "$INSTDIR\temp\MSys-setup.exe"
  SetOutPath "$INSTDIR" ; RMDIR on a dir will not work if it is the CWD
  RMDir "$INSTDIR\temp"

  ; 2. HP-provided packages
  DetailPrint "extracting Extralibs"
  SetOutPath "$INSTDIR\temp"
  File "..\product\Extralibs-setup.exe"
  DetailPrint "installing Extralibs"
  SetDetailsPrint listonly
    ExecWait '"$INSTDIR\temp\Extralibs-setup.exe" /S /D=$INSTDIR'
  SetDetailsPrint lastused
  DetailPrint "finished Extralibs"
  Delete "$INSTDIR\temp\Extralibs-setup.exe"
  SetOutPath "$INSTDIR" ; RMDIR on a dir will not work if it is the CWD
  RMDir "$INSTDIR\temp"

  ; 3. GHC (and anything else not already covered)
  DetailPrint "extracting GHC"
  SetOutPath "$INSTDIR\temp"
  File "..\product\GHC-setup.exe"
  DetailPrint "installing GHC"
  SetDetailsPrint listonly
    ExecWait '"$INSTDIR\temp\GHC-setup.exe" /S /D=$INSTDIR'
  SetDetailsPrint lastused
  DetailPrint "finished GHC"
  Delete "$INSTDIR\temp\GHC-setup.exe"
  SetOutPath "$INSTDIR" ; RMDIR on a dir will not work if it is the CWD
  RMDir "$INSTDIR\temp"

  ; Turn compression back on
  SetCompress auto

SectionEnd


Section "Stack (its included installer will be launched)" SecStack
  SectionIn 1 2

  ; The actual install steps are in the first section

SectionEnd


SectionGroup "Update system settings" SecGr

Section "Associate with .hs/.lhs files" SecAssoc

  SectionIn 1

  ; File associations
  WriteRegStr HKCR ".hs" "" "ghc_haskell"
  WriteRegStr HKCR ".lhs" "" "ghc_haskell"
  WriteRegStr HKCR "ghc_haskell" "" "Haskell Source File"
  WriteRegStr HKCR "ghc_haskell\DefaultIcon" "" "$INSTDIR\icons\hsicon.ico"
  WriteRegStr HKCR "ghc_haskell\shell\open\command" "" '"$INSTDIR\bin\ghci.exe" "%1"'

  ;Remember that we registered associations
  WriteRegDWORD HKLM "${PRODUCT_DIR_REG_KEY}" Assocs 0x1

SectionEnd

Section "Add versioned GHCi to right-click menu" SecGHCiVersioned

  SectionIn 1

  WriteRegStr HKCR "ghc_haskell\shell\Open with GHCi ${GHC_VERSION}" "" ""
  WriteRegStr HKCR "ghc_haskell\shell\Open with GHCi ${GHC_VERSION}\command" "" '"$INSTDIR\bin\ghci.exe" "%1"'

  ;Remember that we added versioned GHCi
  WriteRegDWORD HKLM "${PRODUCT_DIR_REG_KEY}" VersionedGHCi 0x1
SectionEnd

Section "Update the PATH environment variable" SecPath

  SectionIn 1

  ; Update PATH
  ${EnvVarUpdate} $0 "PATH" "P" "HKLM" "$INSTDIR\bin"
  ${EnvVarUpdate} $0 "PATH" "P" "HKLM" "$INSTDIR\lib\extralibs\bin"
  ${EnvVarUpdate} $0 "PATH" "A" "HKLM" "$INSTDIR\mingw\bin"
  ${EnvVarUpdate} $0 "PATH" "P" "HKLM" "$PROGRAM_FILES\Haskell\bin"
  SetShellVarContext current
  ${EnvVarUpdate} $0 "PATH" "P" "HKCU" "$APPDATA\cabal\bin"
  SetShellVarContext all

SectionEnd

Section "Store GHC's location in registry" SecGHCLoc

  SectionIn 1

  ; (copied from the GHC installer).
  WriteRegStr HKCU "Software\Haskell\GHC\ghc-${GHC_VERSION}" "InstallDir" "$INSTDIR"
  WriteRegStr HKCU "Software\Haskell\GHC" "InstallDir" "$INSTDIR"

  WriteRegStr HKCU "Software\Haskell\WinGHCi 1.0.6" "WorkingDir" "$INSTDIR\winghci"

SectionEnd

Section "Create uninstaller" SecAddRem

  SectionIn 1
  SectionIn RO

  ; Add uninstall information to Add/Remove Programs
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\HaskellPlatform-${PLATFORM_VERSION}" \
  "DisplayName" "Haskell Platform ${PLATFORM_VERSION}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\HaskellPlatform-${PLATFORM_VERSION}" \
  "UninstallString" "$\"$INSTDIR\Uninstall.exe$\""
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\HaskellPlatform-${PLATFORM_VERSION}" \
  "DisplayIcon" "$INSTDIR\icons\installer.ico"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\HaskellPlatform-${PLATFORM_VERSION}" \
  "Publisher" "Haskell.org"

  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

  ; This is needed for uninstaller to work
  WriteRegStr HKLM "${PRODUCT_DIR_REG_KEY}" "" "$INSTDIR\Uninstall.exe"
  WriteRegStr HKLM "${PRODUCT_DIR_REG_KEY}" "InstallDir" "$INSTDIR"

SectionEnd

SectionGroupEnd

Section "-StartMenu" StartMenu
  SectionIn 1 2

  ; Add start menu shortcuts

  !insertmacro MUI_STARTMENU_WRITE_BEGIN StartMenuPage

    ;Create shortcuts
    CreateDirectory "$SMPROGRAMS\$START_MENU_FOLDER"
    !insertmacro CreateInternetShortcut \
        "$SMPROGRAMS\$START_MENU_FOLDER\${HACKAGE_SHORTCUT_TEXT}" \
        "http://hackage.haskell.org" \
        "$INSTDIR\icons\hackage.ico" "0"
    CreateShortCut \
        "$SMPROGRAMS\$START_MENU_FOLDER\GHC Documentation.lnk" \
        "$INSTDIR\doc\html\index.html"
    CreateShortCut \
        "$SMPROGRAMS\$START_MENU_FOLDER\GHC Flag Reference.lnk" \
        "$INSTDIR\doc\html\users_guide\flag-reference.html"
    CreateShortCut \
        "$SMPROGRAMS\$START_MENU_FOLDER\Library Documentation.lnk" \
        "$INSTDIR\lib\extralibs\doc\frames.html"
    CreateShortCut "$SMPROGRAMS\$START_MENU_FOLDER\GHCi.lnk" \
        "$INSTDIR\bin\ghci.exe"
    CreateShortCut "$SMPROGRAMS\$START_MENU_FOLDER\WinGHCi.lnk" \
        "$INSTDIR\winghci\winghci.exe"

  !insertmacro MUI_STARTMENU_WRITE_END

SectionEnd

;--------------------------------
;Uninstaller Section

; Section /o "un.Stack" UnSecStack
  ; Actually, we do not want ot let user uninstall Stack this way; the user
  ; should remove Stack using the the normal Add/Remove Programs functionality
  ; in Windows.  If we allow this here, the user might have already removed
  ; Stack using Add/Remove Programs but we would have to go through more
  ; logic to detect that case and handle it.... bleh
; SectionEnd


Section "Uninstall"

  !define MSYS_UNINST "MSys_Uninstall.exe"
  !define EXTRA_UNINST "Extralibs_Uninstall.exe"
  !define GHC_UNINST "GHC_Uninstall.exe"
  Var /GLOBAL tempname

  ; uninstall the sub-components
  ; 1. MSys
  Call un.CreateGUID
  pop $0
  StrCpy $tempname "$TEMP\uninstHP_$0.exe"
  ; copy the uninstaller so it can delete itself
  CopyFiles /SILENT /FILESONLY "$INSTDIR\${MSYS_UNINST}" "$tempname"
  DetailPrint "uninstalling MSys"
  SetDetailsPrint listonly
  ExecWait '"$tempname" /S _?=$INSTDIR'
  Delete "$tempname"
  SetDetailsPrint lastused

  ; 2. HP-provided packages
  Call un.CreateGUID
  pop $0
  StrCpy $tempname "$TEMP\uninstHP_$0.exe"
  ; copy the uninstaller so it can delete itself
  CopyFiles /SILENT /FILESONLY "$INSTDIR\${EXTRA_UNINST}" "$tempname"
  DetailPrint "uninstalling Extralibs"
  SetDetailsPrint listonly
  ExecWait '"$tempname" /S _?=$INSTDIR'
  Delete "$tempname"
  SetDetailsPrint lastused

  ; 3. GHC (and anything else not already covered)
  Call un.CreateGUID
  pop $0
  StrCpy $tempname "$TEMP\uninstHP_$0.exe"
  ; copy the uninstaller so it can delete itself
  CopyFiles /SILENT /FILESONLY "$INSTDIR\${GHC_UNINST}" "$tempname"
  DetailPrint "uninstalling GHC"
  SetDetailsPrint listonly
  ExecWait '"$tempname" /S _?=$INSTDIR'
  Delete "$tempname"
  SetDetailsPrint lastused

  Delete "$INSTDIR\Uninstall.exe"
  RMDir $INSTDIR

  ;Since we install to '$PF\Haskell Platform\$PLATFORM_VERSION', we
  ;should also try to delete '$PF\Haskell Platform' if it is empty.
  ${GetParent} $INSTDIR $R0
  RMDir $R0

  ; Delete start menu shortcuts
  !insertmacro MUI_STARTMENU_GETFOLDER StartMenuPage $START_MENU_FOLDER

  Delete "$SMPROGRAMS\$START_MENU_FOLDER\GHC Documentation.lnk"
  Delete "$SMPROGRAMS\$START_MENU_FOLDER\GHC Flag Reference.lnk"
  Delete "$SMPROGRAMS\$START_MENU_FOLDER\Library Documentation.lnk"
  Delete "$SMPROGRAMS\$START_MENU_FOLDER\GHCi.lnk"
  Delete "$SMPROGRAMS\$START_MENU_FOLDER\WinGHCi.lnk"
  Delete "$SMPROGRAMS\$START_MENU_FOLDER\${HACKAGE_SHORTCUT_TEXT}.url"
  RMDir "$SMPROGRAMS\$START_MENU_FOLDER\"

  ; Delete registry keys
  ReadRegDWORD $0 HKLM "${PRODUCT_DIR_REG_KEY}" VersionedGHCi

  ${If} $0 = 0x1
    DeleteRegKey HKCR "ghc_haskell\shell\Open with GHCi ${GHC_VERSION}\command"
    DeleteRegKey HKCR "ghc_haskell\shell\Open with GHCi ${GHC_VERSION}"
  ${EndIf}

  ReadRegDWORD $0 HKLM "${PRODUCT_DIR_REG_KEY}" Assocs

  ${If} $0 = 0x1
    DeleteRegValue HKCR ".hs" ""
    DeleteRegValue HKCR ".lhs" ""
    DeleteRegValue HKCR "ghc_haskell\shell\open\command" ""
    DeleteRegKey HKCR "ghc_haskell\DefaultIcon"
    DeleteRegKey /IfEmpty HKCR "ghc_haskell\shell\open\command"
    DeleteRegKey /IfEmpty HKCR "ghc_haskell\shell\open"
    DeleteRegKey /IfEmpty HKCR "ghc_haskell\shell"
    DeleteRegKey /IfEmpty HKCR "ghc_haskell"
  ${EndIf}

  DeleteRegKey HKCU "Software\Haskell\GHC\ghc-${GHC_VERSION}"
  DeleteRegKey HKCU "Software\Haskell\GHC"
  ; remove WorkingDir but keep any user customizations for winGHCi
  DeleteRegValue HKCU "Software\Haskell\WinGHCi 1.0.6" "WorkingDir"
  DeleteRegKey HKLM "${PRODUCT_DIR_REG_KEY}"
  DeleteRegKey /IfEmpty HKCU Software\Haskell
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\HaskellPlatform-${PLATFORM_VERSION}"

  ; Update PATH
  ${un.EnvVarUpdate} $0 "PATH" "R" "HKLM" "$INSTDIR\bin"
  ${un.EnvVarUpdate} $0 "PATH" "R" "HKLM" "$INSTDIR\lib\extralibs\bin"
  ${un.EnvVarUpdate} $0 "PATH" "R" "HKLM" "$INSTDIR\mingw\bin"
  ${un.EnvVarUpdate} $0 "PATH" "R" "HKLM" "$PROGRAM_FILES\Haskell\bin"
  SetShellVarContext current
  ${un.EnvVarUpdate} $0 "PATH" "R" "HKCU" "$APPDATA\cabal\bin"
  SetShellVarContext all


SectionEnd

;--------------------------------
; "the last SetCompress command in the file also determines whether or not the
; install info section and uninstall data of the installer is compressed."
; Yes, we want this meta-info to be compressed

SetCompress auto
