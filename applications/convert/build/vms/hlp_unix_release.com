$ VERIFY = F$VERIFY( 0 )
$!+
$!  Name:
$!     HLP_UNIX_RELEASE
$!
$!  Purpose:
$!     Assembles and concatenates the source for the CONVERT portable help.
$!
$!  Language:
$!     DCL
$!
$!  Type of Module:
$!     Command procedure.
$!
$!  Invocation:
$!     @HLP_UNIX_RELEASE [CLASS]
$!
$!  Description:
$!     This procedure assembles and concatenates the various help source
$!     files for a UNIX release on a named platform.  It should be
$!     executed on the (VMS) development machine and will fetch the
$!     appropriate files from the CMS library into the default directory,
$!     and append them in the correct order into CONVERT.SHL.  This file
$!     has the STREAM_LF file organization.
$!
$!  Parameters:
$!     CLASS
$!        The optional name of the CMS class for the release to be assembled.
$!        If this parameter is not supplied, then the most recent version
$!        of the system will be used.
$!
$!  Output:
$!     -  The CONVERT portable-help source file, CONVERT.SHL.
$!
$!  Prior Requirements:
$!     It is assumed that you are logged in for CONVERT development with
$!     the current CMS library being the CONVERT library.
$!
$!  Authors:
$!     MJC: Malcolm J. Currie (STARLINK)
$!     {enter_new_authors_here}
$!
$!  History:
$!     1993 July 29 (MJC):
$!        Original version.
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
$!  information, and general information, but specific to a platform.
$!  Also include a description of how to use the portable help system.
$!
$    CMS FETCH'CF' HELP_APPLIC,HELP_GENERAL,HELP_UNIX,HELP_USING_PORTABLE
$!
$!  Search through the list of files to append the source to the
$!  compiled help source.
$!
$    APPEND *.HLP CONVERT.PH
$!
$!  Delete the unwanted help source.
$!
$    DELETE *.HLP;*
$!
$!  Rename the help file.  It could not be called this earlier because
$!  there is already a CONVERT.HLP file.
$!
$    RENAME CONVERT.PH CONVERT.HLP 
$!
$!  Grant world read and execute access to the file.
$!
$    SET PROTECTION=(W:RE) CONVERT.HLP
$!
$!  Exit the procedure.
$!  ===================
$!
$    IF ( VERIFY ) THEN SET VERIFY
$!
$    EXIT
