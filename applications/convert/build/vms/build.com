$!+
$!  Name:
$!     BUILD.COM
$!
$!  Purpose:
$!     Builds the CONVERT package from the source code files in a VMS release.
$!
$!  Type of Module:
$!     DCL command procedure.
$!
$!  Invocation:
$!     @BUILD
$!
$!  Description:
$!     This procedure builds (or re-builds) the CONVERT package for the VAX
$!     VMS operating system starting from the released source code files
$!     (most of which are held in a text library).  It also runs CONVERT
$!     on a test dataset to determine whether or not the installation was 
$!     successful.
$!
$!  Notes:
$!     For a VMS release, this procedure will ususlly be executed before
$!     releasing the CONVERT package, so that installation will not normally
$!     involve a build operation. However, this procedure should be
$!     retained as part of the release so that it may subsequently be
$!     used for a re-build if necessary.
$!
$!  Authors:
$!     Malcolm J. Currie (STARLINK)
$!     {enter_new_authors_here}
$!
$!  History:
$!     1993 July 28 (MJC):
$!        Original version.
$!     {enter_changes_here}
$!
$!  Bugs:
$!     {note_any_bugs_here}
$!
$!-
$!  Set up symbols and logical names.
$!  =================================
$!
$!  Locally define the package directory as a process logical name and set
$!  up error handling to ensure it gets deassigned if anything goes wrong.
$      FILE_NAME = F$ENVIRONMENT( "PROCEDURE" )
$      DIRECTORY = F$PARSE( FILE_NAME, , , "DEVICE" ) -
                 + F$PARSE( FILE_NAME, , , "DIRECTORY" )
$      DEFINE/NOLOG CONVERT_DIR 'DIRECTORY'
$      ON ERROR THEN $GOTO CLEANUP
$      ON CONTROL_Y THEN $GOTO CLEANUP
$!
$!  Ensure that definitions for CONVERT development are available.  This
$!  includes the commands for development of ADAM applications.
$      @[]CON_DEV NOLOG
$!
$!  Define some temporary logical names for the Figaro include files 
$!  used.
$      DEFINE DTACODES FIGPACK_DISK:[FIGPACK.NFIGARO.INC]DTACODES.INC
$      DEFINE DYNAMIC_MEMORY FIGPACK_DISK:[FIGPACK.NFIGARO.INC]DYNAMIC_MEMORY.INC
$!
$!  Build the CONVERT library.
$!  ==========================
$!
$!  Create the applications object library. 
$      LIBRARY/CREATE/OBJECT CONVERT.OLB
$!
$!  Build the object library from the source code.  Compress it.
$      @[]BUILD_CONVERT_LIBRARY
$      LIBRARY/DATA=REDUCE []CONVERT.OLB/OBJECT
$!
$!  Purge any previous version to save space.
$      PURGE []CONVERT.OLB
$!
$!  Build the CONLIB library.
$!  =========================
$!
$!  Create the subroutine object library. 
$      LIBRARY/CREATE/OBJECT CONLIB.OLB
$!
$!  Build the object library from the source code.  Compress it.
$      @[]BUILD_CONLIB_LIBRARY
$      LIBRARY/DATA=REDUCE []CONLIB.OLB/OBJECT
$!
$!  Purge any previous version to save space.
$      PURGE []CONLIB.OLB
$!
$!  Deassign the temporary logical names for the Figaro include files.
$      DEASSIGN DTACODES
$      DEASSIGN DYNAMIC_MEMORY
$!
$!
$!  Build the CONSPP library.
$!  =========================
$!
$!  Create the SPP object library. 
$      LIBRARY/CREATE/OBJECT CONSPP.OLB
$!
$!  Build the object library from the source code.  Compress it.
$      @[]BUILD_CONSPP_LIBRARY
$      LIBRARY/DATA=REDUCE []CONSPP.OLB/OBJECT
$!
$!  Purge any previous version to save space.
$      PURGE []CONSPP.OLB
$!
$!  Build the CONVERT monolith and A-tasks.
$!  =======================================
$!
$!  Build the A-tasks and compile the interface files.
$      @[]MAKE_CONVERT_ATASKS
$!
$!  Build the monolith.
$      @[]MAKE_CONVERT
$!
$!  Build the monolith interface file.
$      @[]MAKE_CONVERT_IFL
$!
$!  Tidying operations.
$!  ===================
$!
$!  Truncate all the new files and grant world read and execute access 
$!  to them.
$      SET FILE/TRUNCATE *.IFC,*.OLB,*.EXE
$      SET PROTECTION=(W:RE) *.OLB,*.EXE
$!
$!  Perform installation test.
$!  ==========================
$!
$!  Test the installation.  Create a local symbol to run this trace.  The 
$!  symbol is very uinnlikely to replace an existing one.
$      DEV_DST2NDF:= $CONVERT_DIR:DST2NDF
$      DEV_DST2NDF CONVERT_TEST CONVERT_TEST
$!
$!  Closedown operations.
$!  =====================
$!
$!  On error, remove the local definition of the package directory.
$CLEANUP:
$      EXSTAT = $STATUS
$      DEASSIGN CONVERT_DIR
$!
$!  Exit the procedure.
$      EXIT 'EXSTAT'
