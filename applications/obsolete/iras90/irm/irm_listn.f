      SUBROUTINE IRM_LISTN( PARAM, IGRP, APPNAM, STATUS )
*+
*  Name:
*     IRM_LISTN

*  Purpose:
*     Create a text file holding a list of NDF names.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_LISTN( PARAM, IGRP, APPNAM, STATUS )

*  Description:
*     This routine can be used to pass on lists of NDFs from one
*     applications to another. It creates a text file holding the NDF
*     names contained within the group identified by IGRP. The text
*     file is created within the users home (or login) directory and is
*     called iras90_ndfs.lis. Any existing version of this file is
*     first deleted. The name of the file is then written out to the
*     parameter specified by PARAM, so long as there is more than one
*     NDF in the group. If this is not the case, then the name of the
*     single NDF is written out to the parameter. If the group contains
*     no names, an error is reported. The parameter should have a type
*     of NDF, a vpath of DYNAMIC and should be associated with a
*     global parameter used to pass NDFs between applications.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        A parameter to which the name of the newly created text file
*        will be written. IRAS90 applications should use parameter
*        NDFLIST.
*     IGRP = INTEGER (Given)
*        The GRP identifier for the group containing the names of the
*        NDFs.
*     APPNAM = CHARACTER * ( * ) (Given)
*        The name of the calling application. This is included in a
*        comment at the start of the created text file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-SEP-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER IGRP
      CHARACTER APPNAM*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns used length of a string.

*  Local Constants:
      CHARACTER FNAME*15         ! The name of the file to be created.
      PARAMETER ( FNAME = 'iras90_ndfs.lis' )

*  Local Variables:
      CHARACTER COMMNT*80        ! A comment to store in the file.
      CHARACTER FILE*(GRP__SZFNM)! The full file specification.
      CHARACTER HOME*(GRP__SZFNM)! The users home directory.
      INTEGER IAT                ! Position of last non-blank character.
      CHARACTER INDC*1           ! Group's indirection control character.
      INTEGER IPAR               ! SUBPAR parameter index.
      INTEGER LHOME              ! Used length of HOME.
      INTEGER NTICKS             ! System time.
      CHARACTER PVAL*(GRP__SZFNM)! The value to assign to the parameter.
      INTEGER SIZE               ! The size of the group.
      CHARACTER TIMDAT*24        ! Time and date string.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the users home directory.
      CALL PSX_GETENV( 'HOME', HOME, STATUS )
      LHOME = CHR_LEN( HOME )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Construct the full specification of the file which will be used to
*  store the names of the output NDFs.
      FILE = HOME( : LHOME )//'/'//FNAME

*  Attempt to delete any existing file with the same name.
      CALL ERR_MARK
      CALL FIO_ERASE( FILE( : LHOME + 16 ), STATUS )

*  If an error occured, annul it and continue.
      IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE

*  Construct a comment for the output text file.
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_CTIME( NTICKS, TIMDAT, STATUS )
      COMMNT = '   NDFs created or accessed by '
      IAT = 31
      CALL CHR_APPND( APPNAM, COMMNT, IAT )
      CALL CHR_APPND( ' at '//TIMDAT, COMMNT, IAT )

*  Create the file containing the list of output NDFs.
      CALL GRP_LISTF( FILE, 0, 0, COMMNT, IGRP, STATUS )

*  Get the size of the group.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )

*  If the group is empty, report an error.
      IF( SIZE .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRM_LISTN_ERR1',
     :            'IRM_LISTN: The supplied group of NDF names is empty',
     :                 STATUS )
         GO TO 999

*  If the size is one, get the name of the single NDF.
      ELSE IF( SIZE .EQ. 1 ) THEN
         CALL GRP_GET( IGRP, 1, 1, PVAL, STATUS )

*  If there is more than one NDF in the group, prfix the full file
*  specification with the group's indirection character to make it a
*  valid group expression.
      ELSE
         CALL GRP_GETCC( IGRP, 'IND', INDC, STATUS )
         PVAL = INDC//FILE

      END IF

*  Store the parameter value.
      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )
      CALL SUBPAR_PUTNAME( IPAR, PVAL( : CHR_LEN( PVAL ) ), STATUS )
      CALL SUBPAR_GETNAME( IPAR, PVAL, STATUS )

*  If an error occured, give a context message.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRM_LISTN_ERR2',
     :                 'IRM_LISTN: Unable to create a file holding '//
     :                 'a list of output NDFs', STATUS )
      END IF

      END
