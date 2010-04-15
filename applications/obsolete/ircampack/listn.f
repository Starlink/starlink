      SUBROUTINE LISTN( PARAM, IGRP, APPNAM, STATUS )
*+
*  Name:
*     LISTN

*  Purpose:
*     Create a text file holding a list of NDF names.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LISTN( PARAM, IGRP, APPNAM, STATUS )

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
*     15-SEP-1993 (DSB):
*        Original version, copied from IRAS90 routine IRM_LISTN
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER IGRP
      CHARACTER APPNAM*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns used length of a string.

*  Local Constants:
      INTEGER LFNAME             ! Length of the file name
      PARAMETER ( LFNAME = 18 )

      CHARACTER FNAME*( LFNAME ) ! The name of the file to be created.
      PARAMETER ( FNAME = 'ircampack_ndfs.lis' )

*  Local Variables:

      CHARACTER
     :        COMMNT*80,         ! A comment to store in the file.
     :        DUM1*1,            ! Dummy argument
     :        DUM2*1,            ! Dummy argument
     :        DUM3*1,            ! Dummy argument
     :        DUM4*1,            ! Dummy argument
     :        FILE*(GRP__SZFNM), ! The full file specification.
     :        HOME*(GRP__SZFNM), ! The users home directory.
     :        INDC*1,            ! The indirection control character.
     :        PVAL*(GRP__SZFNM), ! The value to assign to the parameter.
     :        SYSNAM*30,         ! Name of operating system
     :        TIMDAT*24          ! Time and date string.

      INTEGER
     :        IAT,               ! Position of last non-blank character.
     :        IPAR,              ! SUBPAR parameter index.
     :        LHOME,             ! Used length of HOME.
     :        NTICKS,            ! System time.
     :        SIZE               ! The size of the group.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine which operating system is in use, as this determines the
*  location of the file.
      CALL PSX_UNAME( SYSNAM, DUM1, DUM2, DUM3, DUM4, STATUS )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999
      CALL CHR_UCASE( SYSNAM )

*  If we are on VMS, then the file is put in SYS$LOGIN. Get the full
*  diectory specification.
      IF ( INDEX( SYSNAM, 'VMS' ) .NE. 0 ) THEN
         CALL PSX_GETENV( 'SYS$LOGIN', HOME, STATUS )
         LHOME = CHR_LEN( HOME )

*  Construct the full specification of the file which will be used to
*  store the names of the output NDFs.
         FILE = HOME( : LHOME )//FNAME

*  Attempt to delete any existing file with the same name.
         CALL ERR_MARK
         CALL FIO_ERASE( FILE( : LHOME + LFNAME )//';0', STATUS )

*  If an error occured, annul it and continue.
         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         CALL ERR_RLSE

*  If we are not on VMS, assume posix.
      ELSE

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
         CALL FIO_ERASE( FILE( : LHOME + LFNAME + 1 ), STATUS )

*  If an error occured, annul it and continue.
         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         CALL ERR_RLSE

      END IF

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
         CALL ERR_REP( 'LISTN_ERR1',
     :            'LISTN: The supplied group of NDF names is empty',
     :                 STATUS )
         GO TO 999

*  If the size is one, get the name of the single NDF.
      ELSE IF( SIZE .EQ. 1 ) THEN
         CALL GRP_GET( IGRP, 1, 1, PVAL, STATUS )

*  If there is more than one NDF in the group, prefix the full file
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
         CALL ERR_REP( 'LISTN_ERR2',
     :                 'LISTN: Unable to create a file holding '//
     :                 'a list of output NDFs', STATUS )
      END IF

      END
