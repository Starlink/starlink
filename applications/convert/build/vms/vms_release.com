$ VERIFY = F$VERIFY( 0 )
$!+
$! Name:
$!    VMS_RELEASE
$!
$! Purpose:
$!    Assembles CONVERT for VMS release.
$!
$! Language:
$!    DCL
$!
$! Type of Module:
$!    Command procedure.
$!
$! Invocation:
$!    @VMS_RELEASE [CLASS]
$!
$! Description:
$!    This procedure assembles a VMS release of the CONVERT system.  It
$!    should be executed on the (VMS) development machine in an empty 
$!    directory and will assemble and build all the files needed for the
$!    release into the default directory, fetching the files from the CMS
$!    library.
$!
$! Parameters:
$!    CLASS
$!       The optional name of the CMS class for the release to be assembled.
$!       If this parameter is not supplied, then the most recent version
$!       of the system will be used.
$!
$! Notes:
$!     Execution of this procedure should normally be followed by an
$!     @[]BUILD command on the target VMS machine, which will then build
$!     the CONVERT system from the assembled source-code files.
$!
$! Output:
$!    -  The CONVERT source, help file, build procedures, and IRAF
$!    libraries.
$!
$! Authors:
$!    MJC: Malcolm J. Currie (STARLINK)
$!    {enter_new_authors_here}
$!
$! History:
$!    1993 July 28 (MJC):
$!       Original version.
$!    {enter_further_changes_here}
$!
$! Bugs:
$!    {note_any_bugs_here}
$!
$!-
$!
$!  Check that the current default directory is empty.
$!
$    IF ( F$SEARCH( "*.*;*" ) .NES. "" )
$    THEN
$       WRITE SYS$OUTPUT "The current default directory is not empty!"
$!       EXIT
$    ENDIF
$!
$!  Construct the CMS class qualifier.
$!
$    IF ( P1 .NES. "" )
$    THEN
$       CF = "/GENERATION=" + P1
$       WRITE SYS$OUTPUT "Using class ''P1'"
$    ELSE
$       CF = ""
$    ENDIF
$!
$!  Locally define the package directory as a process logical name and set
$!  up error handling to ensure it gets deassigned if anything goes wrong.
$!
$    FILE_NAME = F$ENVIRONMENT( "PROCEDURE" )
$    DIRECTORY = F$PARSE( FILE_NAME, , , "DEVICE" ) -
               + F$PARSE( FILE_NAME, , , "DIRECTORY" )
$    DEFINE/NOLOG CONVERT_DIR 'DIRECTORY'
$    ON ERROR THEN $GOTO CLEANUP
$    ON CONTROL_Y THEN $GOTO CLEANUP
$!
$!  Assembly procedures.
$!  ====================
$!
$!  Fetch all the procedures used to assemble CONVERT files on VMS.
$!  This file itself can be deleted.
$!
$    CMS FETCH'CF' VMS_ASSEMBLY
$    DELETE VMS_RELEASE.COM;0
$!
$!  Create Builders
$!  ===============
$!
$!  Fetch the files for building CONVERT on VMS.
$!
$    CMS FETCH'CF' VMS_BUILDING
$!
$!  Allow CONVERT development.
$!  ==========================
$!
$!  Set up CONVERT development, here used to define logical names for the
$!  include files.
$!
$    @[]CON_DEV NOLOG
$!
$!  Library maintenance.
$!  ====================
$!
$!  Ensure LIBMAINT switches are correct.
$!
$    SWITCH HELPMODE OFF
$    SWITCH COMPILING OFF
$!
$!  Fetch the source files.
$!  =======================
$!
$    CMS FETCH'CF' CON_C_ROUTINES,CON_FORTRAN_ROUTINES,CON_GENERIC_ROUTINES
$!
$!  Make the CONVERT source library.
$!  ================================
$!
$!  Create the source and library.
$!
$    LIBCRE CONVERT
$!
$!  Create a list of the application source code with the library-insertion
$!  command.
$!
$    CMS SHOW GENERATION'CF' APPLIC -
          /OUTPUT = VMSLIB.TMP /FORMAT = "$ INSERT #E"
$!
$!  Execute the procedure to insert all the modules into the CONVERT
$!  source and object libraries.  The source code and object modules are
$!  deleted as they are inserted in the libraries.
$!
$    @VMSLIB.TMP
$!
$!  Delete the procedures.
$!
$    DELETE VMSLIB.TMP;*
$!
$!  Compress the library to save disc space.
$!
$    LIBREDUCE
$!
$!  Make the CONLIB source library.
$!  ===============================
$!
$!  Create the source and library.
$!
$    LIBCRE CONLIB
$!
$!  Create a list of the application source code with the library-insertion
$!  command.
$!
$    CMS SHOW GENERATION'CF' CON_FORTRAN_ROUTINES,CON_GENERIC_ROUTINES,CON_C_ROUTINES -
          /OUTPUT = VMSLIB.TMP /FORMAT = "$ INSERT #E"
$!
$!  Delete the temporary files.
$!
$    DELETE VMSLIB.TMP;*
$!
$!  Compress the library to save disc space.
$!
$    LIBREDUCE
$!
$!  IRAF SPP code
$!  =============
$!
$!  Create the source and library.
$!
$    LIBCRE CONSPP
$!
$!  Create a list of the SPP Fortran source code with the library-insertion
$!  command.
$!
$    CMS SHOW GENERATION'CF' IRAF_SPP_FOR
          /OUTPUT = VMSLIB.TMP /FORMAT = "$ INSERT #E"
$!
$!  Delete the temporary files.
$!
$    DELETE VMSLIB.TMP;*
$!
$!  Compress the library to save disc space.
$!
$    LIBREDUCE
$!
$!  Fetch the include files.
$!  ========================
$!
$    CMS FETCH'CF' INCLUDE_FILES
$!
$!  Create the portable help library.
$!  =================================
$!
$!  Fetch the top-level source item.
$!
$    CMS FETCH'CF' TOP_HELP.HLP
$!
$!  Create the source file by renaming the top-level source item.
$!  This is level-0 help, and so must appear first in the source file.
$!  The file must have STREAM_LF file organisation.
$!
$    RENAME TOP_HELP.HLP CONVERT.PH
$!
$!  Fetch the main sets of source files: applications help, general
$!  information, and general information, but specific to VMS.  Also
$!  include a description of how to use the portable help system.
$!
$    CMS FETCH'CF' HELP_APPLIC,HELP_GENERAL,HELP_VMS,HELP_USING_PORTABLE
$!
$!  Search through the list of files to append the source to the
$!  compiled help source.
$!
$    APPEND *.HLP CONVERT.PH
$!
$!  Make the portable help library.
$!
$    @HELPDIR:HLIB CONVERT.PH CONVERT.SHL
$!
$!  Delete the unwanted help source.
$!
$    DELETE *.HLP;*,CONVERT.PH;*
$!
$!  Grant world read and execute access to the file.
$!
$    SET PROTECTION=(W:RE) CONVERT.SHL
$!
$!  Create the VMS help library.
$!  ============================
$!
$!  Switch to help library mode.
$!
$    SWITCH HELPMODE ON
$!
$!  Create the help library.
$!
$    LIBCRE CONVERT
$!
$!  Fetch the source files for the applications, general information,
$!  and VMS-specific general information.
$!
$    CMS FETCH'CF' HELP_APPLIC,HELP_GENERAL,HELP_VMS,HELP_USING_VMS
$!
$!  Create a list of the help source code with the library-insertion command.
$!
$    CMS SHOW GENERATION'CF' HELP_APPLIC,HELP_GENERAL,HELP_VMS,HELP_USING_VMS -
          /OUTPUT = VMSLIB.TMP /FORMAT = "$ INSERT #E"
$!
$!  Execute the procedure to insert all the modules into the CONVERT help
$!  library.
$!
$    @VMSLIB.TMP
$!
$!  Delete the procedure.
$!
$    DELETE VMSLIB.TMP;*
$!
$!  Switch off help library mode.
$!
$    SWITCH HELPMODE OFF
$!
$!  Create the interface-file library.
$!  ==================================
$!
$!  Create the interface-file source library.
$!
$    LIBCRE CONVERT_IFL
$!
$!  Fetch the source files.
$!
$    CMS FETCH'CF' IFL
$!
$!  Create a list of the help source code with the library-insertion command.
$!
$    CMS SHOW GENERATION'CF' IFL -
          /OUTPUT = VMSLIB.TMP /FORMAT = "$ INSERT #E"
$!
$!  Execute the procedure to insert all the modules into the CONVERT help
$!  library.
$!
$    @VMSLIB.TMP
$!
$!  Delete the procedure.
$!
$    DELETE VMSLIB.TMP;*
$!
$!  Compress the interface-file source library.
$!
$    LIBREDUCE
$!
$!  IRAF libraries
$!  ==============
$!
$!  Fetch the VMS IRAF libraries.
$!
$    CMS FETCH'CF' IRAF_VMS_LIBRARIES
$!
$!  Installation Test.
$!  ==================
$!
$!  Fetch the dataset to test the installation process.
$!
$    CMS FETCH'CF' TEST_DATA
$!
$!  Definition files.
$!  =================
$!
$!  Fetch the set of files that set up the command definitions.
$!
$    CMS FETCH'CF' DEFINITION_FILES
$!
$!  Perform miscellaneous jobs.
$!  ===========================
$!
$!  Truncate all files.
$!
$    SET FILE/TRUNCATE *.*;*
$!
$!  Grant world read and execute access to all files.
$!
$    SET PROTECTION=(W:RE) *.*;*
$!
$!  Exit the procedure.
$!  ===================
$!
$!  On error, remove the local definition of the package directory.
$!
$ CLEANUP:
$    EXSTAT = $STATUS
$    DEASSIGN CONVERT_DIR
$    IF ( VERIFY ) THEN SET VERIFY
$!
$!  Exit the procedure.
$    EXIT 'EXSTAT'
