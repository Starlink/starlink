$ VERIFY = F$VERIFY( 0 )
$!+
$! Name:
$!    MAKE_CONVERT_IFL
$!
$! Purpose:
$!    Builds the CONVERT M-task's interface file in the current directory.
$!
$! Language:
$!    DCL
$!
$! Type of Module:
$!    Command procedure.
$!
$! Invocation:
$!    @MAKE_CONVERT_IFL
$!
$! Description:
$!    Generates the CONVERT monolith's interface file in the current 
$!    directory from its constituent application interface files.
$!    CONVERT.IF% are purged.
$!
$! Output:
$!    -  The CONVERT monolith interface file, both source and in compiled
$!    form called CONVERT.IFL and CONVERT.IFC respectively.
$!
$! Prior Requirements:
$!    It is assumed that you are logged in for ADAM development and are
$!    currently in the CONVERT source directory or one being built. The
$!    following files should be present in the current directory:
$!    CONVERT_IFL.TLB, and CONVERT_IFL_START.LIS, MONOLITH_IFL_END.LIS. 
$!    If any are not, the versions in CONVERT_DIR are used.
$!
$! Authors:
$!    MJC: Malcolm J. Currie (STARLINK)
$!    {enter_new_authors_here}
$!
$! History:
$!    1992 September 7 (MJC):
$!       Original version based on MAKE_KAPPA_IFL.
$!    {enter_changes_here}
$!
$!-
$!-------------------------------------------------------------------------
$!
$!  Make a copy of the start file containing the required monolith
$!  header - do not use wild cards, as the creation date in that case
$!  would be that of the original start file, thus giving no direct
$!  indication of the date of the final product interface file.
$!
$    IF F$SEARCH( "CONVERT_IFL_START.LIS" ) .EQS. ""
$    THEN
$       COPY CONVERT_DIR:CONVERT_IFL_START.LIS CONVERT_IFL_START.LIS
$    ELSE
$       COPY CONVERT_IFL_START.LIS CONVERT_IFL_START.LIS
$    ENDIF
$!
$!  Make a copy of the end file containing the required end monolith
$!  line.
$!
$    IF F$SEARCH( "MONOLITH_IFL_END.LIS" ) .EQS. ""
$    THEN
$       COPY CONVERT_DIR:MONOLITH_IFL_END.LIS MONOLITH_IFL_END.LIS
$    ELSE
$       COPY MONOLITH_IFL_END.LIS MONOLITH_IFL_END.LIS
$    ENDIF
$!
$!  Extract all of the CONVERT A-task IFLs from the text library
$!
$    IF F$SEARCH( "CONVERT_IFL.TLB" ) .EQS. ""
$    THEN
$       LIBRARY/EXTRACT=*/TEXT/OUTPUT=CONVERT_IFL.TXT CONVERT_DIR:CONVERT_IFL
$    ELSE
$       LIBRARY/EXTRACT=*/TEXT/OUTPUT=CONVERT_IFL.TXT CONVERT_IFL
$    ENDIF
$!
$!  Now append all the valid A-task interface files onto the new copy
$!  of the header file, terminated by the end file which contains the
$!  required end of monolith statement.
$!
$    APPEND -
CONVERT_IFL.TXT,-
MONOLITH_IFL_END.LIS    CONVERT_IFL_START.LIS
$!
$    DELETE CONVERT_IFL.TXT;*
$!
$!  Now rename the most recent version of the start file (which now
$!  contains the whole monolith interface) to the required name.
$!
$    RENAME CONVERT_IFL_START.LIS CONVERT.IFL
$!
$!  Compile the newly created interface file.
$!
$    COMPIFL CONVERT
$!
$!  Purge old copies of the monolith interface files.
$!
$    PURGE/NOLOG/NOCONFIRM CONVERT.IF%
$    DELETE MONOLITH_IFL_END.LIS;0
$!
$!  Reset the verification.
$!
$    IF ( VERIFY ) THEN SET VERIFY
$!
$    EXIT
$!
