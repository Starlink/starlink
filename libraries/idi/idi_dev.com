$!+
$!  Name:
$!     IDI_DEV.COM
$!  
$!  Purpose:
$!     IDI startup file.
$!
$!  Type of module:
$!     DCL command procedure.
$!
$!  Invocation:
$!     IDI_DEV
$!
$!  Description:
$!     This procedure defines the logical names for the development
$!     of programs utilising IDI.
$!
$!  Authors:
$!     Nick Eaton  ( DUVAD::NE )
$!
$!  History:
$!     15-MAR_1991 (NE):
$!        Original version.
$!-
$!
$!  Define the logical names for the options files
$      DEFINE/NOLOG IDI_LINK       IDI_DIR:IDI_LINK
$      DEFINE/NOLOG IDI_LINK_ADAM  IDI_DIR:IDI_LINK_ADAM
$!
$!  Define the logical names for the include files
$      DEFINE/NOLOG IDI_ERR        IDI_DIR:IDI_ERR
$      DEFINE/NOLOG IDI_PAR        IDI_DIR:IDI_PAR
$      DEFINE/NOLOG IDIINC         IDI_DIR:IDIINC
$!
$!  Exit the procedure
$      EXIT
$!
