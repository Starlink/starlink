      SUBROUTINE IRC_BPOS( IDC, NVAL, SAMPLE, DETIND, RA, DEC,
     :                     ANGLE, SPEED, STATUS )
*+
*  Name:
*     IRC_BPOS

*  Purpose:
*     Returns boresight positions at a set of samples.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_BPOS( IDC, NVAL, SAMPLE, DETIND, RA, DEC, ANGLE,
*                    SPEED, STATUS )

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
*     RA( NVAL ) = DOUBLE PRECISION (Returned)
*        An array holding the Right Ascension (B1950 FK4) of the
*        boresight at the moment each sample specified in the input
*        lists was taken (radians).
*     DEC( NVAL ) = DOUBLE PRECISION (Returned)
*        An array holding the Declination (B1950 FK4) of the
*        boresight at the moment each sample specified in the input
*        lists was taken (radians).
*     ANGLE( NVAL ) = DOUBLE PRECISION (Returned)
*        The scan angle (see ID1 section 3) at the boresight. This is
*        measured from equatorial north to the positive focal plane Y
*        axis (see ID1 Appendix E).  The angle is in radians and is
*        measured positive in the same sense as rotation from north to
*        east.
*     SPEED( NVAL ) = REAL (Returned)
*        The scan speed in radians per second. Positive values imply
*        that sources move across the focal plane in the positive Y
*        direction (i.e in the "with-survey" direction). Negative
*        values imply that sources move in the negative Y direction
*        (i.e. "anti-survey").
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*                (DSB):
*        Orginal version.
*     9-MAY-1991 (DSB):
*        Updated for IRA version 2.
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
      DOUBLE PRECISION RA( NVAL )
      DOUBLE PRECISION DEC( NVAL )
      DOUBLE PRECISION ANGLE( NVAL )
      REAL    SPEED( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER   I                ! Loop count.
      CHARACTER SCS*(IRA__SZSCS)! The Sky Coordinate System used by
                                 ! IRC1_BPOSI
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
         CALL ERR_REP( 'IRC_BPOS_ERR1',
     :                 'IRC_BPOS: Invalid IRC identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF

*  Check that at least one sample has been sepecified.
      IF( NVAL .LE. 0 ) THEN
         STATUS = IRC__NVAL
         CALL MSG_SETI( 'N', NVAL )
         CALL ERR_REP( 'IRC_BPOS_ERR2',
     :         'IRC_BPOS: Illegal number of samples specified (^N)',
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
            CALL ERR_REP( 'IRC_BPOS_ERR3',
     :        'IRC_BPOS: Detector index ^D is out of bounds [^DL,^DH]',
     :                  STATUS )
            GO TO 999
         END IF
      END DO

*  Find the boresight positions, in the primary Sky Coordinate System.
      CALL IRC1_BPOSI( IDC, NVAL, SAMPLE, DETIND, SCS, RA, DEC, ANGLE,
     :                 SPEED, STATUS )

*  Convert the coordinates to EQUATORIAL(B1950).
      CALL IRA_PACON( NVAL, RA, DEC, ANGLE, SCS, 'EQUATORIAL(B1950)',
     :                IRA__IRJEP, RA, DEC, ANGLE, STATUS )

*  If an error occured, give a contextual message.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRC_BPOS_ERR4',
     :                 'IRC_BPOS: Unable to find boresight position ',
     :                 STATUS )
      END IF

      END
