$ VERIFY = F$VERIFY( 0 )
$!+
$!  Name:
$!     UNIX_RELEASE.COM
$!
$!  Purpose:
$!     Assembles a UNIX release of the CONVERT package.
$!
$!  Type of Module:
$!     DCL command procedure.
$!
$!  Invovation:
$!     @UNIX_RELEASE [CLASS]
$!
$!  Description:
$!     This procedure assembles a UNIX release of the CONVERT package.  It
$!     should be executed on the (VMS) development machine in an empty
$!     directory and will assemble all the files needed for the release
$!     into the default directory, fetching them from the CMS library.
$!
$!  Parameters:
$!     CLASS
$!        The optional name of the CMS class for the release to be
$!        assembled.  If this parameter is not supplied, then the most
$!        recent version of the system will be used.
$!
$!  Notes:
$!     Execution of this procedure should normally be followed by
$!     transferring the files to a UNIX machine; setting up links for
$!     the include files; and then a UNIX "make" command on the target
$!     machine, which will then build the CONVERT package from the
$!     assembled source-code files.
$!
$!  Output:
$!    -  The CONVERT source, help file, build procedures, and IRAF
$!    libraries.
$!
$!  Authors:
$!     Malcolm J. Currie (STARLINK)
$!     {enter_new_authors_here}
$!
$!  History:
$!     1993 July 29 (MJC):
$!        Original version.
$!     1997 August 1 (MJC):
$!        Fetch IRAF_IX86_LINUX_LIBRARIES, BOURNE-SHELL_SCRIPTS, 
$!        AWK_SCRIPTS, and PUBLIC_TASKS groups.  Used renamed
$!        IRAF_SUN4_SOLARIS_LIBRARIES group.
$!     {enter_changes_here}
$!
$!  Bugs:
$!     {note_any_bugs_here}
$!
$!-
$!
$!  Check that the current default directory is empty.
$!
$    IF ( F$SEARCH( "*.*;*" ) .NES. "" )
$    THEN
$      WRITE SYS$OUTPUT "The current default directory is not empty!"
$!      EXIT
$    ENDIF
$!
$!  Construct the CMS class qualifier.
$!
$    IF ( P1 .NES. "" )
$    THEN
$       CF = "/GENERATION=" + P1
$    ELSE
$       CF = ""
$    ENDIF
$!
$!  Use the CONVERT CMS library.
$!
$    CMS SET LIBRARY DISK$ADAM:[CUR.CONVERT.CMS]
$!
$!  Assembly procedures.
$!  ====================
$!
$!  Fetch all the procedures used to assemble CONVERT files on VMS prior
$!  to moving the CONVERT files to a UNIX platform.  It includes the table
$!  of available types for all the generic routines (CON_GENERIX.LIS).
$!  The file itself can be deleted.
$!
$    CMS FETCH'CF' VMS_UNIX_ASSEMBLY
$    DELETE UNIX_RELEASE.COM;0
$!
$!  Fetch the include files.
$!  ========================
$!
$    CMS FETCH'CF' INCLUDE_FILES
$!
$!  Source-code files.
$!  =================
$!
$!  Fetch all the Fortran routine source-code files.
$!
$    CMS FETCH'CF' CO*_FORTRAN_ROUTINES,AIF_FORTRAN_ROUTINES,APPLIC,IRAF_SPP_FOR
$!
$!  Fetch the source code for all the generic routines.
$!
$    CMS FETCH'CF' CO*_GENERIC_ROUTINES
$!
$!  Fetch the source code for all the C routines.
$!
$    CMS FETCH'CF' CON_C_ROUTINES
$!
$!  Create a list of the generic source code.
$!
$    CMS SHOW GENERATION'CF' CO*_GENERIC_ROUTINES -
        /OUTPUT = GENERIX.TMP /FORMAT = "#E"
$!
$!  Call a procedure to generate the Fortran source from the generic
$!  code just listed and using the CON_GENERIX.LIS lookup table.  The
$!  file comprises one line per generic routine and has the list of
$!  generic types for that routine appended.
$!
$    @GENERIC_EXPAND CON_GENERIX.LIS
$!
$!  Call the procedure which reads the list of generic files and splits
$!  them into one module per file.
$!
$    @SPLIT_GENERIC
$!
$!  Rename the .FOR files to UNIX style .F files.
$!
$    RENAME *.FOR *.F
$!
$!  Delete the temporary files.
$!
$    DELETE GENERIX.TMP;*,SPLIT_GENERIC.COM;*,GENERIC_EXPAND.COM;*
$    DELETE CON_GENERIX.LIS;*
$!
$!  Delete the original generic source code.
$!
$    DELETE *.GEN;*
$!
$!  Interface files.
$!  ================
$!
$!  Fetch the interface files.
$!
$    CMS FETCH'CF' IFL
$!
$!  IRAF libraries
$!  ==============
$!
$!  Fetch the IRAF libraries
$!
$    CMS FETCH'CF' IRAF_SUN4_SOLARIS_LIBRARIES,IRAF_ALPHA_OSF1_LIBRARIES,IRAF_IX86_LINUX_LIBRARIES
$!
$!  Help source files.
$!  ==================
$!
$!  Assembly and compile the portable help library.  Delete the VMS
$!  procedure once's its job is done.
$!
$    @[]HLP_UNIX_RELEASE 'P1'
$    DELETE HLP_UNIX_RELEASE.COM;0
$!
$!  Scripts and procedures
$!  ======================
$!
$!  Fetch the C-shell script and ICL procedure for running tasks or
$!  defining commands.  Also fetch the Bourne-shell scripts for
$!  graphic-file conversions, and the awk scripts.
$!
$    CMS FETCH'CF' C_SHELL_SCRIPTS,CONVERT.ICL,BOURNE-SHELL_SCRIPTS,AWK_SCRIPTS
$!
$!  Build files.
$!  ============
$!
$!  Fetch the files needed for building a UNIX release.  Remove the UNIX
$!  prefixes.
$!
$    CMS FETCH'CF' UNIX_BUILDING
$    RENAME UNIX_MAKEFILE. MAKEFILE.
$!
$!  Public tasks.
$!  =============
$!
$!  Fetch the pre-built UNIX programmes needed in a UNIX release.
$!
$    CMS FETCH'CF' PUBLIC_TASKS
$!
$!  Documentation.
$!  ==============
$!
$!  Fetch the paper documentation and news item.
$!
$    CMS FETCH'CF' SUN55.TEX,CONVERT.NEWS
$!
$!  Test data for installation test.
$!  ================================
$!
$!  Fetch the installation-test data set.
$!
$    CMS FETCH'CF' TEST_DATA
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
$    IF ( VERIFY ) THEN SET VERIFY
$!
$    EXIT
