$!+
$!  Name:
$!     BUILD_IDI.COM
$!  
$!  Purpose:
$!     Build the IDI shareable image from the object libraries.
$!
$!  Type of module:
$!     DCL command procedure.
$!
$!  Invocation:
$!     @BUILD_IDI
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
$!     26-MAR-1991 (NE):
$!        Original version.
$!      2-JAN-1992 (NE):
$!        New shareable libraries.
$!
$!  Bugs:
$!
$!-
$!
$!  Compile the transfer vector macro file
$      MACRO/NOLIST BUILD_IDI.MAR
$!
$!  Link the shareable image
$      LINK/NOMAP/SHARE=IDI_IMAGE.EXE -
            BUILD_IDI.OBJ,-
            IDI/LIBRARY,-
            IDI_X/LIBRARY,-
            BUILD_IDI/OPTIONS,-
            NAG_LIB/LIBRARY,-
            GNS_DIR:GNS_LINK/OPTIONS
$!
$!  Purge any earlier versions of the shareable image
$      PURGE IDI_IMAGE.EXE
$!
$!  Set the protection of this file so all can read it
$      SET PROTECTION=(W:RE) IDI_IMAGE.EXE
$!
$!  Delete all intermediate files
$      DELETE BUILD_IDI.OBJ.
$!
$!  Exit the procedure
$      EXIT
$!
