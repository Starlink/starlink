      SUBROUTINE IRC_DINDS( IDC, NDETNO, DETNO, DETIN, NDETIN, STATUS )
*+
*  Name:
*     IRC_DINDS

*  Purpose:
*     Convert a list of detector numbers to detector indices.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_DINDS( IDC, NDETNO, DETNO, DETIN, NDETIN, STATUS )

*  Description:
*     The detector index corresponding to each supplied detector number
*     is found and returned in DETIN. If the CRDD file contains no data
*     for any of the supplied detector numbers, or if any of the
*     detector number are equal to the Starlink "BAD" value
*     (VAL__BADI), then no corresponding value is stored in DETIN. The
*     number of values actually stored in DETIN is returned in NDETIN.
*     These values occupy elements 1 to NDETIN of the DETIN array.

*  Arguments:
*     IDC = INTEGER  (Given)
*        An IRC identifier for the CRDD file.
*     NDETNO = INTEGER (Given)
*        The size of the DETNO and DETIN arrays.
*     DETNO( NDETNO ) = INTEGER (Given)
*        The array of detector numbers.
*     DETIN( NDETNO ) = INTEGER (Returned)
*        The list of detector indices corresponding to the detector
*        numbers in DETNO.
*     NDETIN = INTEGER (Returned)
*        The number of values returned in DETIN. This will be less than
*        DETNO if any "BAD" detector numbers are supplied, or if the
*        CRDD file contains no data for any of the supplied detectors.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-NOV-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC error values.
      INCLUDE 'PRM_PAR'          ! Starlink data constants.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_VALID( IRC__MAX ) = LOGICAL (Read)
*           True if the corresponding IRC identifier is valid.
*        CCM_DETIN(I90__DETS,IRC__MAX) = INTEGER (Read)
*           The detector index corresponding to each detector number.

*  Arguments Given:
      INTEGER IDC
      INTEGER NDETNO
      INTEGER DETNO( NDETNO )

*  Arguments Returned:
      INTEGER DETIN( NDETNO )
      INTEGER NDETIN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DI                 ! Detector index value.
      INTEGER DN                 ! Detector number value.
      INTEGER I                  ! Loop count.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied IRC identifier is valid. If not, report an error
*  and quit.
      IF( IDC .LE. 0 .OR. IDC .GT. IRC__MAX ) THEN
         STATUS = IRC__INVID

      ELSE IF( .NOT. CCM_VALID( IDC ) ) THEN
         STATUS = IRC__INVID

      END IF

      IF( STATUS .EQ. IRC__INVID ) THEN
         CALL ERR_REP( 'IRC_DINDS_ERR1',
     :                 'IRC_DINDS: Invalid IRC identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF

*  Initiialise the number of returned detector indices to zero.
      NDETIN = 0

*  Loop round each supplied detector number.
      DO I = 1, NDETNO
         DN = DETNO( I )

*  If the detector number is "BAD" ignore it.
         IF( DN .NE. VAL__BADI ) THEN

*  Get the corresponding detector index.
            DI = CCM_DETIN( DN, IDC )

*  If the index value is "BAD", then the CRDD file contains no data for
*  this detector. If so, ignore this detector number.
            IF( DI .NE. VAL__BADI ) THEN

*  Increment the number of returned indices and store the detector
*  index in the returned array,
               NDETIN = NDETIN + 1
               DETIN( NDETIN ) = DI

            END IF

         END IF

      END DO

*  If an error occured, give a contextual message.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRC_DINDS_ERR2',
     :       'IRC_DINDS: Unable to convert detector numbers to indices',
     :                 STATUS )
      END IF

      END
