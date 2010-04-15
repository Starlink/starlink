      SUBROUTINE SMARA1( PFILE, PEPOCH, SCS, IRA, MAXPNT, X, Y, LON,
     :                   LAT, NPNT, STATUS )
*+
*  Name:
*     SMARA1

*  Purpose:
*     Read sky coordinates from a text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SMARA1( PFILE, PEPOCH, SCS, IRA, MAXPNT, X, Y, LON, LAT,
*                  NPNT, STATUS )

*  Description:
*     This subroutine is used by the application SKYMARK to get a text
*     file from the environment and read the sky coordinates from the
*     file.

*  Arguments:
*     PFILE = CHARACTER (Given)
*        Name of the parameter used to get the input file from the
*        environment.
*     PEPOCH = CHARACTER (Given)
*        Name of the parameter used to get the epoch at which the
*        input coordinates were determined.
*     SCS = CHARACTER (Given)
*        The sky coordinate system in use.
*     IRA = INTEGER (Given)
*        The ID of the IRA system.
*     MAXPNT = INTEGER (Given)
*        Max. number of positions can be specified in a text file.
*     X( MAXPNT ), Y( MAXPNT ) = DOUBLE PRECISION (Returned)
*        Image coordinates coresponding to the sky coordinates read from
*        the input text file.
*     LON( MAXPNT ), LAT( MAXPNT ) = DOUBLE PRECISION (Returned)
*        Sky coordinates reading from the input text file.
*     NPNT = INTEGER (Returned)
*        Actual number of positions specified in the input text file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     26-JAN-1993 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants.
      INCLUDE 'GRP_ERR'          ! GRP_ error constants.
      INCLUDE 'IRA_PAR'          ! IRA_ constants.
      INCLUDE 'IRA_ERR'          ! IRA_ error constants.

*  Arguments Given:
      CHARACTER*( * ) PFILE
      CHARACTER*( * ) PEPOCH
      CHARACTER*( * ) SCS
      INTEGER IRA
      INTEGER MAXPNT

*  Arguments Returned:
      DOUBLE PRECISION X( MAXPNT ), Y( MAXPNT )
      DOUBLE PRECISION LON( MAXPNT ), LAT( MAXPNT )
      INTEGER NPNT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns used length of a string.

*  Local Variables:
      CHARACTER BJ*1             ! Tyoe of epoch.
      CHARACTER COORDS*(IRA__SZSCS)! Input coordinate system.
      CHARACTER INDCC*1          ! Groups indirection control character.
      CHARACTER FILE*(GRP__SZFNM)! File name.
      CHARACTER GRPEXP*(GRP__SZGEX) ! Group expression.
      CHARACTER NAME*(IRA__SZSCS)! Input SCS name.
      CHARACTER TEXT( 2 )*(GRP__SZNAM)! Character coordinate values.

      DOUBLE PRECISION EPOCH     ! Epoch of observations.
      DOUBLE PRECISION EQU       ! SCS equinox epoch.

      INTEGER ADDED              ! No. of names added to group.
      INTEGER I                  ! Do loop index
      INTEGER IGRP               ! GRP group identifier.
      INTEGER IPAR               ! SUBPAR parameter identifier.
      INTEGER ITMP               ! GRP identifier for a temporary group
      INTEGER LFILE              ! Used length of FILE.
      INTEGER NNAMES             ! Number of strings obtained

      LOGICAL FLAG               ! True if group expression was flagged.

      REAL TEST                  ! Test value.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a new empty group.
      CALL GRP_NEW( 'Coordinates', ITMP, STATUS )

*  Get the indirection character for the new group.
      CALL GRP_GETCC( ITMP, 'INDIRECTION', INDCC, STATUS )

*  Get the file name from the environment.
 10   CONTINUE

      CALL SUBPAR_FINDPAR( PFILE, IPAR, STATUS )
      CALL SUBPAR_GETNAME( IPAR, FILE, STATUS )

*  Find the used length.
      LFILE = CHR_LEN( FILE )

*  Create a group expression which will read the contents of the file
*  into a group.
      GRPEXP = INDCC//FILE

*  Attempt to read the contents of the file into the group.
      CALL GRP_GRPEX( GRPEXP, GRP__NOID, ITMP, NNAMES, ADDED, FLAG,
     :                STATUS )

*  If a fortran I/O error has occurred, flush it, set the group size to
*  zero and go round for a new file name.
      IF( STATUS .EQ. GRP__FIOER ) THEN
         CALL ERR_FLUSH( STATUS )
         CALL GRP_SETSZ( ITMP, 0, STATUS )
         CALL SUBPAR_CANCL( IPAR, STATUS )
         GO TO 10
      END IF

*  Create a group from which all blanks have been removed.
      CALL GRP_REMOV( ITMP, ' ', IGRP, STATUS )

*  Delete the original group.
      CALL GRP_DELET( ITMP, STATUS )

*  Get the size of the group without blanks.
      CALL GRP_GRPSZ( IGRP, NNAMES, STATUS )

*  Report an error if the file specifies too many positions.
      IF( NNAMES .GT. MAXPNT*2 + 1 ) THEN
         CALL MSG_SETI( 'N', (NNAMES - 1)/2 )
         CALL MSG_SETI( 'M', MAXPNT )
         CALL ERR_REP( 'SMARA0_ERR1',
     : 'SMARA0: File ^FILE contains ^N positions (maximum allowed is '//
     : '^M).', STATUS )
         GO TO 999
      END IF

*  Get the first element from the group, and convert to upper case.
*  The first element may specify the coordinate system to which the
*  values stored in the file refer (or it may be the first numerical
*  coordinate value).
      CALL GRP_GET( IGRP, 1, 1, COORDS, STATUS )
      CALL CHR_UCASE( COORDS )

*  See if the specified coordinate system is a valid sky coordinate
*  system.
      CALL IRA_GETEQ( COORDS, EQU, BJ, NAME, STATUS )

*  If it is not a valid sky coordinate system, annul the error and
*  re-report a more helpful message.
      IF( STATUS .EQ. IRA__BADSC ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL CHR_CTOR( COORDS, TEST, STATUS )

         IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FILE', FILE )
            CALL ERR_REP( 'SMARA0_ERR2',
     :           'SMARA0: File ^FILE contains image coordinates. Sky '//
     :           'coordinates are required.', STATUS )

         ELSE
            CALL ERR_ANNUL( STATUS )
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FILE', FILE )
            CALL MSG_SETC( 'C', COORDS )
            CALL ERR_REP( 'SMARA0_ERR3',
     :   'SMARA0: File ^FILE specifies unknown sky coordinate system '//
     :   '"^C".', STATUS )
         END IF

         GO TO 999

      END IF

*  If the input coordinates system is different to the requested output
*  coordinate system, get the epoch of the obvservations.
      IF( COORDS .NE. SCS ) THEN
         CALL PAR_DEF0D( PEPOCH, IRA__IRJEP, STATUS )
         CALL PAR_GET0D( PEPOCH, EPOCH, STATUS )
         CALL PAR_CANCL( PEPOCH, STATUS )
      END IF

*  Initialise the number of returned points to zero.
      NPNT = 0

*  Loop round each pair of coordinates.
      DO I = 2, NNAMES - 1, 2

*  Extract the text strings representing the coordinates from the group.
         CALL GRP_GET( IGRP, I, 2, TEXT, STATUS )

*  Increment the number of input positions.
         NPNT = NPNT + 1

*  Convert the coordinates from text to floating point format.
         CALL IRA_CTOD( TEXT( 1 ), TEXT( 2 ), COORDS, LON( NPNT ),
     :                  LAT( NPNT ), STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

      END DO

*  If some sky coordinates were obtained from the file, convert them to
*  the output sky coordinate system.
      IF ( NPNT .GT. 0 ) THEN
         CALL IRA_CONVT( NPNT, LON, LAT, COORDS, SCS, EPOCH, LON, LAT,
     :                   STATUS )

*  Get the image coordinates of these positions.
         CALL IRA_TRANS( NPNT, LON, LAT, .FALSE., SCS, IRA, X, Y,
     :                   STATUS )
      END IF

*  Delete the group.
 999  CONTINUE

      CALL GRP_DELET( IGRP, STATUS )



      END
