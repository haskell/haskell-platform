;; WARNING: Bootstrapper.nsi is automatically generated from
;; Bootstrapper.nsi.mu by the hptool.
;; Make sure you are editing Bootstrapper.nsi.mu, not Bootstrapper.nsi.

; Haskell Platform Bootstrapper-Installer
;
; The purpose of the "bootstrapper" is to be able to launch processes
; as either elevated or not (once elevated, it requires non-trivial code
; for that process to launch an un-elevated process).  So, this installer
; is the wrapper of the entire HP installer.  This bootstrapper must specify
; "RequestExecutionLevel user".  Installers launched from here can specify
; "RequestExecutionLevel highest" or as needed.
;
; This bootstrapper exists for one feature (currently) which must be done as
; the user, which is to write update/create the user's cabal config file.
;
;;;;;;;;;
; NOTE, special handling for $INSTDIR:
;
; $INSTDIR is a variable set initially by the NSIS framework before calling any
; functions in the client's NSI file.  It can be set by the user with the
; command line parameter "D=<directory>", or to some default by NSIS.
;
; Using $INSTDIR in this particular installer is only correct when the user has
; launched this installer with the /S and /D options; otherwise, installing
; anything to $INSTDIR from this installer may not have the correct value since
; the user can subsequently change the install directory in the following
; sub-installer's dialog.  This installer handles post-install tasks, so it
; gets the actual installed directory from the registry for those purposes.
;
; We only support two cases: 1) both /S and /D= or 2) neither /S nor /D=
;
; This installer does modify $INSTDIR for a non-silent install, according to
; the defaults for 32-bit vs 64-bit (using the macro "do64Stuff"), which means
; if the user specifies "/D=" without "/S" (for silent), the user's "/D=" will
; not be used (we cannot readily detect if the user specified "/D=" or not,
; since the NSIS framework strips it out of $CMDLINE; however if /S is present,
; then we do not modify $INSTDIR, so any user-specified /D= will be retained).
;
; To the sub-installer which is launched from here, this installer provides a
; forced "/D=" command line switch, which accounts for whether the user set it
; (but also must have /S), or it was set by the "do64Stuff" logic.  In either
; case, unless /S, the launched sub-installer will ask the user and ignore the
; /D= parameter.


;--------------------------------
;Includes

  !Include "WinMessages.nsh"
  !Include "FileFunc.nsh"
  !Include "StrFunc.nsh"
  !Include "LogicLib.nsh"
  !Include "MUI2.nsh"
  !Include "WordFunc.nsh"
  !Include "x64.nsh"
  !insertmacro GetParameters
  !insertmacro GetOptions
  !Include "CommonHP.nsh"

;--------------------------------
;Defines

  !Define GHC_VERSION "{{ghcVersion}}"
  !Define PLATFORM_VERSION "{{hpVersion}}"
  !Define PRODUCT_DIR_REG_KEY "Software\Haskell\Haskell Platform\${PLATFORM_VERSION}"
  !Define CABAL_CONFIG_MSYS_ADDITIONS \
    '--augment="extra-prog-path: $ACTUAL_INSTDIR\msys\usr\bin" \
    --augment="extra-prog-path: $APPDATA\cabal\bin" \
    --augment="extra-lib-dirs: $ACTUAL_INSTDIR\mingw\lib" \
    --augment="extra-include-dirs: $ACTUAL_INSTDIR\mingw\include"'

;--------------------------------
;Variables

  Var PROGRAM_FILES
  Var INSTALLER_PARAMS
  Var tempDir

  ; ACTUAL_INSTDIR: set from registry, and only valid after the enclosed
  ; installed has finished
  Var ACTUAL_INSTDIR

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

  ;Icon
  !Define MUI_ICON "icons/installer.ico"
  !Define MUI_UNICON "icons/installer.ico"

  ; do this as the user, not as Administrator!
  RequestExecutionLevel user

  ;Best available compression
  SetCompressor lzma

  ;Install types
  InstType "Standard"
  InstType "Portable (just unpack the files)"

  ;Used on the UI Pages
  BrandingText "Haskell.org"

;--------------------------------
;Macros

;--------------------------------
;Callbacks

Function .onInit
  ; Store and pass on all the params given to this bootstrapper
  ; NOTE: GetParameters uses $CMDLINE which has already stripped out any /D=
  ; "If /D= is specified on the command line (to override the install
  ;  directory) it won't show up in $CMDLINE." (nor GetParameters)
  ${GetParameters} $INSTALLER_PARAMS

  ; If the /S (for silent install) switch has been supplied by the user,
  ; then make this truely silent and not show the banner
  ; Without some kind of window to show, this installer gets shoved out of
  ; the foreground, and then the main installer comes up behind windows.
  ClearErrors
  ${GetOptions} "$INSTALLER_PARAMS" "/S" $0
  ; error flag is set if the flag we looked for was not there
  ${If} ${ERRORS}
    Banner::show "Extracting Haskell Platform installer..."
    BringToFront
  ${EndIf}

  !insertmacro do64Stuff 1

  ; Keep this as the current user, so that $APPDATA is expanded for this user!
  SetShellVarContext current
FunctionEnd

Function .onInstSuccess
FunctionEnd

;--------------------------------
Function CreateGUID
  System::Call 'ole32::CoCreateGuid(g .s)'
FunctionEnd

;--------------------------------
;Interface Settings

  !define MUI_ABORTWARNING

; Make this installer completely silent
SilentInstall silent

;--------------------------------
;Pages

;--------------------------------
;Languages

  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "Base components" SecMain

  ; Make this section mandatory
  SectionIn RO

  ;Meta-installer should not try to re-compress the payloads!
  SetCompress off

  ; Create a unique name for a folder in $TEMP
  Call CreateGUID
  pop $0
  StrCpy $tempDir "$TEMP\temp_$0"

  ; 1. HP elevated-privileges wrapper
    SetOutPath "$tempDir"
    ; Yes, we extract the core installer and rename it as the "main" installer.
    ; This way, the user sees the same name as the file that was launched.
    File  "/oname={{osProductFileName}}" "..\product\HP-setup.exe"
    ; Can bring down the banner now
    Banner::destroy
    BringToFront
    !insertmacro ShellExecWait "" '"$tempDir\{{osProductFileName}}"' '$INSTALLER_PARAMS /D=$INSTDIR' "" SW_SHOWNORMAL $0
    ; return value from the ShellExecWait is now in $0
    Delete "$tempDir\{{osProductFileName}}"
    SetOutPath "$TEMP" ; RMDIR on a dir will not work if it is the CWD
    RMDir "$tempDir"

  ; 2. Cabal config update
  ;   Should only do this config update if
  ;     a) user has requested
  ;     b) the main installation above completed successfully
    ${If} $0 == 3 ; magic value means successful install, and user wants update
      ClearErrors
      ReadRegStr $ACTUAL_INSTDIR HKLM "${PRODUCT_DIR_REG_KEY}" "InstallDir"
      IfErrors +2
        nsExec::Exec '"$ACTUAL_INSTDIR\lib\extralibs\bin\cabal" user-config update ${CABAL_CONFIG_MSYS_ADDITIONS}'
    ${EndIf}

  ; Turn compression back on
  SetCompress auto

SectionEnd

;--------------------------------
; "the last SetCompress command in the file also determines whether or not the
; install info section and uninstall data of the installer is compressed."
; Yes, we want this meta-info to be compressed

SetCompress auto
