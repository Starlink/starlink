      SUBROUTINE IRC_BCLAP( IDC, DETIND, RA, DEC, CLSAMP, CLZFP,
     :                      STATUS )
*+
*  Name:
*     IRC_BCLAP

*  Purpose:
*     Find the sample number at the closest approach of the boresight
*     to a given sky position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_BCLAP( IDC, DETIND, RA, DEC, CLSAMP, CLZFP, STATUS )

*  Description:
*     The path of a detector centre as it passes over the sky is called
*     the "detector track". The corresponding boresight positions form
*     the "boresight track". The boresight track is the same for all
*     detectors, EXCEPT for a possible shift in sample number. Such a
*     shift will exist if the detector samples in a given column of the
*     NDF DATA array were not obtained simultaneously. This will depend
*     on what type of CRDD file is being processed. CRDD files of type
*     SURVEY_BSIGHT (see ID1 Appendix F) do in fact have simultaneous
*     samples, but other types may not (See routine IRC_SIMUL). For
*     this reason a detector index is needed to completely specify the
*     boresight track.
*
*     This routine finds the fractional sample number from a specified
*     detector which corresponds to the position of closest approach of
*     the boresight track to a given sky position. The focal plane z
*     coordinate (see ID1 Appendix E) of the given position, at the
*     moment at which the closest approach is reached, is also returned.
*     If the position of closest approach lies outside the bounds of the
*     first dimension of the NDF, then the sample number returned
*     represents an extrapolated position. If it is not possible to
*     produce a reliable extrapolated sample number, then STATUS is
*     returned with the value IRC__BADEX, and an error report is made.
*     It is then the responsibility of the calling routine to handle
*     the situation.

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the CRDD file.
*     DETIND = INTEGER (Given)
*        The detector index to which the returned sample number (CLSAMP)
*        should refer.
*     RA = DOUBLE PRECISION (Given)
*        The Right Ascension (B1950, FK4) of the given sky position.
*        If the Starlink "BAD" value (VAL__BADD) is given then CLSAMP
*        and CLZFP are also returned with the bad value.
*     DEC = DOUBLE PRECISION (Given)
*        The Declination (B1950, FK4) of the given sky position.
*        If the Starlink "BAD" value (VAL__BADD) is given then CLSAMP
*        and CLZFP are also returned with the bad value.
*     CLSAMP = REAL (Returned)
*        The fractional sample number at which the point of closest
*        approach of the boresight track to the given sky position is
*        reached. Accurate to about a tenth of a sample.
*     CLZFP = REAL (Returned)
*        The focal plane Z coordinate of the given position at the point
*        of closest approach. In radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-FEB-1991 (DSB):
*        Original version.
*     9-MAY-1991 (DSB):
*        Updated for IRA version 2.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink BAD values.
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC error values.

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
      INTEGER DETIND
      DOUBLE PRECISION RA
      DOUBLE PRECISION DEC

*  Arguments Returned:
      REAL    CLSAMP
      REAL    CLZFP

*  Status:
      INTEGER STATUS             ! Global status
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
         CALL ERR_REP( 'IRC_BCLAP_ERR1',
     :                 'IRC_BCLAP: Invalid IRC identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF

*  Check that the detector index is within the bounds of the NDF second
*  dimension.
      IF( DETIND .LT. CCM_DLOW( IDC ) .OR.
     :    DETIND .GT. CCM_DHIGH( IDC ) ) THEN
         STATUS = IRC__BADDI
         CALL MSG_SETI( 'D', DETIND )
         CALL MSG_SETI( 'DL', CCM_DLOW( IDC ) )
         CALL MSG_SETI( 'DH', CCM_DHIGH( IDC ) )
         CALL ERR_REP( 'IRC_BCLAP_ERR2',
     :        'IRC_BCLAP: Detector index ^D is out of bounds [^DL,^DH]',
     :                  STATUS )
         GO TO 999
      END IF

*  Check that the given RA and DEC values are not bad.
      IF( RA .NE. VAL__BADD .AND. DEC .NE. VAL__BADD ) THEN

*  Call a lower level routine to do the work.
         CALL IRC1_BCLPI( IDC, DETIND, RA, DEC, CLSAMP, CLZFP, STATUS )

*  If either input coordinate is bad, set CLSAMP and CLZFP bad.
      ELSE
         CLSAMP = VAL__BADR
         CLZFP = VAL__BADR

      END IF

*  If an error occured, give a contextual message.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRC_BCLAP_ERR3',
     :'IRC_BCLAP: Unable to find closest approach of the boresight '//
     :'to a given position', STATUS )
      END IF

      END
