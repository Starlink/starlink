$!+
$!  Name:
$!     BUILD_IDI_ADAM.COM
$!  
$!  Purpose:
$!     Build the IDI shareable image from the object libraries.
$!
$!  Type of module:
$!     DCL command procedure.
$!
$!  Invocation:
$!     @BUILD_IDI_ADAM
$!
$!  Description:
$!     This procedure builds the IDI system for the VAX VMS operating.
$!     The starting point is the object libraries of the source code.
$!
$!  Notes:
$!     This procedure will normally have been completed as part of
$!     a release of the IDI software. It is included as part of the
$!     release in case a re-build of the library is required.
$!
$!  Authors:
$!     Nick Eaton  ( DUVAD::NE )
$!
$!  History:
$!     1-OCT-1990 (NE):
$!        Original version.
$!     26-MAR-1991 (NE):
$!        Conforming to SSN/8 naming conventions
$!      2-JAN-1992 (NE):
$!        New shareable libraries.
$!
$!  Bugs:
$!
$!-
$!
$!  Start up ADAM if not already started
$      IF F$TRNLNM( "ADAM_USER" ) .EQS. "" THEN ADAMSTART
$      IF F$TRNLNM( "ADAM_COM" ) .EQS. "" THEN ADAMDEV
$!
$!  Compile the transfer vector macro file
$      MACRO/NOLIST BUILD_IDI_ADAM.MAR
$!
$!  Link the shareable image
$      LINK/NOMAP/SHARE=IDI_IMAGE_ADAM.EXE -
            BUILD_IDI_ADAM.OBJ,-
            IDI/LIBRARY,-
            IDI_X/LIBRARY,-
            BUILD_IDI_ADAM/OPTIONS,-
            NAG_LIB/LIBRARY,-
            GNS_DIR:GNS_LINK_ADAM/OPTIONS
$!
$!  Purge any earlier versions of the shareable image
$      PURGE IDI_IMAGE_ADAM.EXE
$!
$!  Set the protection of this file so all can read it
$      SET PROTECTION=(W:RE) IDI_IMAGE_ADAM.EXE
$!
$!  Delete all intermediate files
$      DELETE BUILD_IDI_ADAM.OBJ.
$!
$!  Exit the procedure
$      EXIT
$!
