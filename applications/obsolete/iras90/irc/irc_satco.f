      SUBROUTINE IRC_SATCO( IDC, NVAL, SAMPLE, DETIND, PSI, THETA,
     :                      SOLONG, UTCS, STATUS )
*+
*  Name:
*     IRC_SATCO

*  Purpose:
*     Returns satellite coordinates at a set of samples.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_SATCO( IDC, NVAL, SAMPLE, DETIND, PSI, THETA, SOLONG,
*                     UTCS, STATUS )

*  Description:
*     The calling routine specifies a list of samples by giving
*     the sample number and detector index of each sample. For
*     each such sample, various items of information about the boresight
*     position are returned, as listed in the argument list below. If a
*     sample number lies outside the bounds of the first dimension of
*     the NDF, then an extrapolated position is returned if possible.
*     If this is not possible, the STATUS value is set to IRC__BADEX
*     and an error report is generated.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     NVAL = INTEGER (Given)
*        The number of samples in the input and output lists.
*     SAMPLE( NVAL ) = REAL (Given)
*        A list of fractional sample numbers. If any sample number is
*        equal to the Starlink "BAD" value (VAL__BADR) then the
*        corresponding elements of the returned arrays are set to the
*        bad value.
*     DETIND( NVAL ) = INTEGER (Given)
*        A list of detector indices.
*     PSI( NVAL ) = REAL (Returned)
*        An array holding the clock angle of the boresight at the
*        moment each sample specified in the input lists was taken
*        (radians).
*     THETA( NVAL ) = REAL (Returned)
*        An array holding the cone angle of the boresight at the moment
*        each sample specified in the input lists was taken (radians).
*     SOLONG( NVAL ) = REAL (Returned)
*        An array holding the solar longitude at the moment each sample
*        specified in the input lists was taken (radians). These values
*        are refered to the mean equator and equinox of the
*        corresponding time returned in UTCS.
*     UTCS( NVAL ) = DOUBLE PRECISION (Returned)
*        An array holding the UTCS at the moment each sample specified
*        in the input lists was taken (seconds).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The relationships between clock angle, cone angle, solar
*     longitude and UTCS are desribed in the ID/1 appendix, "Satellite
*     Coordinates".
*     - This routine uses PSI, not PHI, as the clock angle.
*     ( PSI=2.PI-PHI ).

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-AUG-1993 (DSB):
*        Orginal version.
*     {original_version_entry}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC errors.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_VALID( IRC__MAX ) = LOGICAL (Read)
*           True if the corresponding IRC identifier is valid.
*        CCM_DLOW( IRC__MAX ) = INTEGER (Read)
*           Lowest detector index in the DATA array.
*        CCM_DHIGH( IRC__MAX ) = INTEGER (Read)
*           Highest detector index in the DATA array.

*  Arguments Given:
      INTEGER IDC
      INTEGER NVAL
      REAL    SAMPLE( NVAL )
      INTEGER DETIND( NVAL )

*  Arguments Returned:
      REAL PSI( NVAL )
      REAL THETA( NVAL )
      REAL SOLONG( NVAL )
      DOUBLE PRECISION UTCS( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER   I                ! Loop count.
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
         CALL ERR_REP( 'IRC_SATCO_ERR1',
     :                 'IRC_SATCO: Invalid IRC identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF

*  Check that at least one sample has been sepecified.
      IF( NVAL .LE. 0 ) THEN
         STATUS = IRC__NVAL
         CALL MSG_SETI( 'N', NVAL )
         CALL ERR_REP( 'IRC_SATCO_ERR2',
     :         'IRC_SATCO: Illegal number of samples specified (^N)',
     :                 STATUS )
         GO TO 999
      END IF

*  Check that all detector indices are within the bounds of the NDF
*  second dimension.
      DO I = 1, NVAL
         IF( DETIND( I ) .LT. CCM_DLOW( IDC ) .OR.
     :       DETIND( I ) .GT. CCM_DHIGH( IDC ) ) THEN
            STATUS = IRC__BADDI
            CALL MSG_SETI( 'D', DETIND( I ) )
            CALL MSG_SETI( 'DL', CCM_DLOW( IDC ) )
            CALL MSG_SETI( 'DH', CCM_DHIGH( IDC ) )
            CALL ERR_REP( 'IRC_SATCO_ERR3',
     :        'IRC_SATCO: Detector index ^D is out of bounds [^DL,^DH]',
     :                  STATUS )
            GO TO 999
         END IF
      END DO

*  Find the boresight positions, in satellite coordinates.
      CALL IRC1_SATCI( IDC, NVAL, SAMPLE, DETIND, PSI, THETA, SOLONG,
     :                 UTCS, STATUS )

*  If an error occured, give a contextual message.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRC_SATCO_ERR4',
     :                'IRC_SATCO: Unable to find satellite coordinates',
     :                STATUS )
      END IF

      END
