      SUBROUTINE PREPA1( APARAM, BPARAM, SIZE, SCS, IGRPA, IGRPB,
     :                   STATUS )
*+
*  Name:
*     PREPA1

*  Purpose:
*     Get a group of formatted longitude and latitude values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPA1( APARAM, BPARAM, SIZE, SCS, IGRPA, IGRPB, STATUS )

*  Description:
*     Two groups are created, one holding a list of formatted sky
*     longitude values, and the other holding a group of formatted sky
*     latitude values. The values are obtained from the environment
*     using the supplied parameters, and the user is re-prompted until
*     the group holds exactly the required number of elements (specified
*     by SIZE). The syntax of each string is checked, and the user is
*     re-prompted for a new group if any bad values are supplied.

*  Arguments:
*     APARAM = CHARACTER * ( * ) (Given)
*        The parameter to use to aquire the longitude values.
*     BPARAM = CHARACTER * ( * ) (Given)
*        The parameter to use to aquire the latitude values.
*     SIZE = INTEGER (Given)
*        The number of elements required in each of the returned groups.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system to which the values refer.
*     IGRPA = INTEGER (Returned)
*        A GRP identifier for the group holding the longitude values.
*     IGRPB = INTEGER (Returned)
*        A GRP identifier for the group holding the latitude values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1992 (DSB):
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
      INCLUDE 'IRA_PAR'          ! IRA_ constants.
      INCLUDE 'GRP_PAR'          ! GRP_ constants.

*  Arguments Given:
      CHARACTER APARAM*(*)
      CHARACTER BPARAM*(*)
      INTEGER SIZE
      CHARACTER SCS*(*)

*  Arguments Returned:
      INTEGER IGRPA
      INTEGER IGRPB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ABB*(IRA__SZSCA) ! Abbreviation of long or lat system.
      CHARACTER DESCR*(IRA__SZSCD)! Description of long or lat system.
      CHARACTER TEXT*(GRP__SZNAM)! Text from the group.

      DOUBLE PRECISION VAL       ! Numerical coordinate value.

      INTEGER I                  ! Loop count.
      INTEGER LA                 ! Used length of ABB.
      INTEGER LD                 ! Used length of DESCR.

*.

*  Set both group identifier to GRP__NOID.
      IGRPA = GRP__NOID
      IGRPB = GRP__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the prompt for parameter FIELDLON to indicate what sort of sky
*  coordinates are expected.
      CALL IRA_SCNAM( SCS, 1, DESCR, LD, ABB, LA, STATUS )
      CALL PAR_PROMT( APARAM, DESCR( : LD )//
     :                 ' of a reference position within the field',
     :                 STATUS )

*  Get a group holding the field longitudes for the output NDFs. A
*  value of GRP__NOID is returned for IGRPA if no values are given.
 10   CALL PREPA0( APARAM, SIZE, '  Give more '//DESCR( : LD )//
     :             ' values...', IGRPA, STATUS )

*  Return if an error has occurred, of if no group was given.
      IF( STATUS .NE. SAI__OK .OR. IGRPA .EQ. GRP__NOID ) GO TO 999

*  Check all supplied values are acceptable.
      DO I = 1, SIZE
         CALL GRP_GET( IGRPA, I, 1, TEXT, STATUS )
         CALL IRA_CTOD1( TEXT, SCS, 1, VAL, STATUS )
      END DO

*  If an error occurred, flush it, delete the group and go back for a
*  new group.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PREPA1_ERR1',
     :            'PREPA1: Please give a new group of field positions',
     :                 STATUS )
         CALL ERR_FLUSH( STATUS )
         CALL GRP_DELET( IGRPA, STATUS )
         CALL PAR_CANCL( APARAM, STATUS )
         GO TO 10
      END IF

*  Set the prompt for parameter FIELDLON to indicate what sort of sky
*  coordinates are expected.
      CALL IRA_SCNAM( SCS, 2, DESCR, LD, ABB, LA, STATUS )
      CALL PAR_PROMT( BPARAM, DESCR( : LD )//
     :                 ' of a reference position within the field',
     :                 STATUS )

*  Get a group holding the field latitudes for the output NDFs. A
*  value of GRP__NOID is returned for IGRPB if no values are given.
 20   CALL PREPA0( BPARAM, SIZE, '  Give more '//DESCR( : LD )//
     :             ' values...', IGRPB, STATUS )

*  If an error has occurred, of if no group was given, attempt to
*  delete the longitude group, and return.
      IF( STATUS .NE. SAI__OK .OR. IGRPB .EQ. GRP__NOID ) THEN
         CALL GRP_DELET( IGRPA, STATUS )
         GO TO 999
      END IF

*  Check all supplied values are acceptable.
      DO I = 1, SIZE
         CALL GRP_GET( IGRPB, I, 1, TEXT, STATUS )
         CALL IRA_CTOD1( TEXT, SCS, 2, VAL, STATUS )
      END DO

*  If an error occurred, flush it, delete the group and go back for a
*  new group.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PREPA1_ERR2',
     :            'PREPA1: Please give a new group of field positions',
     :                 STATUS )
         CALL ERR_FLUSH( STATUS )
         CALL GRP_DELET( IGRPB, STATUS )
         CALL PAR_CANCL( BPARAM,  STATUS )
         GO TO 20
      END IF

*  Finish
 999  CONTINUE

      END
