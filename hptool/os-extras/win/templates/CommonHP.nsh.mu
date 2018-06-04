;; WARNING: CommonHP.nsh is automatically generated from CommonHP.nsh.mu by
;; the hptool.  Make sure you are editing the template not the generated file.

; ---------------------
;       CommonHP.nsh
; ---------------------
;
; A few macros common to our (sub-)installer(s).
; $PROGRAM_FILES and $INSTDIR will be modified by the do64Stuff macro!
; be sure to put the following in any file !Include-ing this file:
;
;     Var PROGRAM_FILES
;

!ifndef ___COMMONHP__NSH__
!define ___COMMONHP__NSH___

;--------------------------------
;Includes

  !Include "StrFunc.nsh"
  !Include "LogicLib.nsh"
  !Include "x64.nsh"


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
; http://nsis.sourceforge.net/ShellExecWait
;--------------------------------

;--------------------------------
;Includes

!include LogicLib.nsh
!include WinMessages.nsh

; ShellExecWait
; 
!macro ShellExecWait verb app param workdir show exitoutvar ;only app and show must be != "", every thing else is optional
#define SEE_MASK_NOCLOSEPROCESS 0x40 
System::Store S
System::Call '*(&i60)i.r0'
System::Call '*$0(i 60,i 0x40,i $hwndparent,t "${verb}",t $\'${app}$\',t $\'${param}$\',t "${workdir}",i ${show})i.r0'
BringToFront
System::Call 'shell32::ShellExecuteEx(ir0)i.r1 ?e'
${If} $1 <> 0
	System::Call '*$0(is,i,i,i,i,i,i,i,i,i,i,i,i,i,i.r1)' ;stack value not really used, just a fancy pop ;)
	System::Call 'kernel32::WaitForSingleObject(ir1,i-1)'
	System::Call 'kernel32::GetExitCodeProcess(ir1,*i.s)'
	System::Call 'kernel32::CloseHandle(ir1)'
${EndIf}
System::Free $0
!if "${exitoutvar}" == ""
	pop $0
!endif
System::Store L
!if "${exitoutvar}" != ""
	pop ${exitoutvar}
!endif
!macroend



!endif # !___COMMONHP__NSH___
