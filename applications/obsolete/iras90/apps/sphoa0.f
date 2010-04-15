      SUBROUTINE SPHOA0( PARAM, IGRP, COORDS, SIZE, FILE, LFILE,
     :                   STATUS )
*+
*  Name:
*     SPHOA0

*  Purpose:
*     Read the contents of a text file into a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPHOA0( PARAM, IGRP, COORDS, SIZE, FILE, LFILE, STATUS )

*  Description:
*     The supplied parameter is used to get the name of a text file from
*     the environment. The contents of the file are then read into a GRP
*     group by specifying the file name within an indirection element of
*     a group expression. If an error occurs trying to read the file,
*     the user is re-prompted for a new file. Once a file has been read
*     into a group, the first element of the group is examined. If it
*     is the name of a valid IRA sky coordinate system, then this value
*     is returned in COORDS (in this case the first element is excluded
*     from the returned group). If it is anything else, an error is
*     reported.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter to use.
*     IGRP = INTEGER (Returned)
*        A GRP identifier for a group containing the files contents.
*     COORDS = CHARACTER * ( * ) (Returned)
*        The system in which the coordinates are stored in the text
*        file.
*     SIZE = INTEGER (Returned)
*        The size of the returned group.
*     FILE = CHARACTER * ( * ) (Returned)
*        The name of the text file.
*     LFILE = INTEGER (Returned)
*        Used length of FILE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-JAN-1993 (DSB):
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
      CHARACTER PARAM*(*)

*  Arguments Returned:
      INTEGER IGRP
      CHARACTER COORDS*(*)
      INTEGER SIZE
      CHARACTER FILE*(*)
      INTEGER LFILE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns used length of a string.

*  Local Variables:
      CHARACTER BJ*1             ! Tyoe of epoch.
      CHARACTER INDCC*1          ! Groups indirection control character.
      CHARACTER GRPEXP*(GRP__SZGEX) ! Group expression.
      CHARACTER NAME*(IRA__SZSCS)! SCS name.

      DOUBLE PRECISION EQU       ! SCS equinox epoch.

      INTEGER ADDED              ! No. of names added to group.
      INTEGER IPAR               ! SUBPAR parameter identifier.
      INTEGER ITMP               ! GRP identifier for a temporary group

      LOGICAL FLAG               ! True if group expression was flagged.

      REAL TEST                  ! Test value.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a new empty group.
      CALL GRP_NEW( 'Coordinates', ITMP, STATUS )

*  Get the indirection character.
      CALL GRP_GETCC( ITMP, 'INDIRECTION', INDCC, STATUS )

*  Get the file name from the environment.
 10   CONTINUE

      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )
      CALL SUBPAR_GETNAME( IPAR, FILE, STATUS )

*  Find the used length.
      LFILE = CHR_LEN( FILE )

*  Create a group expression which will read the contents of the file
*  into a group.
      GRPEXP = INDCC//FILE

*  Attempt to read the contents of the file into the group.
      CALL GRP_GRPEX( GRPEXP, GRP__NOID, ITMP, SIZE, ADDED, FLAG,
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

*  Delete the temporary group.
      CALL GRP_DELET( ITMP, STATUS )

*  Get the first element from the new group, and convert to upper case.
      CALL GRP_GET( IGRP, 1, 1, COORDS, STATUS )
      CALL CHR_UCASE( COORDS )

*  See if the specified coordinate system is a valid sky coordinate
*  system.
      CALL IRA_GETEQ( COORDS, EQU, BJ, NAME, STATUS )

*  If it is not a valid sky coordinate system, annul the error and
*  report a more informative error depending on whether or not the
*  first element is a valid numerical value.
      IF( STATUS .EQ. IRA__BADSC ) THEN
         CALL ERR_ANNUL( STATUS )

         CALL CHR_CTOR( COORDS, TEST, STATUS )

         IF( STATUS .EQ. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FILE', FILE )
            CALL ERR_REP( 'SPHOA0_ERR1',
     : 'SPHOA0: File ^FILE specifies image coordinates. SKYPHOT '//
     : 'requires sky coordinates', STATUS )

         ELSE
            CALL ERR_ANNUL( STATUS )
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'C', COORDS )
            CALL MSG_SETC( 'FILE', FILE )
            CALL ERR_REP( 'SPHOA0_ERR2',
     : 'SPHOA0: File ^FILE specifies unknown coordinate system "^C".',
     :                    STATUS )
         END IF

*  If the file specifies a valid sky coordinate system, create a new
*  group excluding the first element.
      ELSE
         CALL GRP_COPY( IGRP, 1, 1, .TRUE., ITMP, STATUS )
         CALL GRP_DELET( IGRP, STATUS )
         IGRP = ITMP

      END IF

*  Return the size of the group.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )

      END
