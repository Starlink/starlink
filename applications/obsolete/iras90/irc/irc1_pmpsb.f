      SUBROUTINE IRC1_PMPSB( IDC, SLOW, SHIGH, DLOW, DHIGH, MAPS,
     :                       STATUS )
*+
*  Name:
*     IRC1_PMPSB

*  Purpose:
*     Create position and angle maps assuming SURVEY_BSIGHT data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_PMPSB( IDC, SLOW, SHIGH, DLOW, DHIGH, MAPS, STATUS )

*  Description:
*     It is assumed that all detector samples with the same sample
*     number are simultaneous and thus correspond to the same boresight
*     position. The boresight position is found at each sample and then
*     the position of each detector centre is found by offseting from
*     the boresight position along the focal plane Z and Y axes.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier.
*     SLOW = INTEGER (Given)
*        The lower bound on sample numbers.
*     SHIGH = INTEGER (Given)
*        The upper bound on sample numbers.
*     DLOW = INTEGER (Given)
*        The lower bound on detector indices.
*     DHIGH = INTEGER (Given)
*        The upper bound on detector indices.
*     MAPS( SLOW:SHIGH, DLOW:DHIGH, 3 ) = DOUBLE PRECISION (Returned)
*        Maps of RA, DEC and scan angle (planes 1, 2 and 3) at every
*        detector centre. EQUATORIAL(B1950) sky coordinates are
*        returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-FEB-1991 (DSB):
*        Original version.
*     9-MAY-1991 (DSB):
*        Updated for IRA version 2.
*     11-FEB-1992 (DSB):
*        Modified to use CCM_DETOR.
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
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_DETNO( IRC__MXD2S, IRC__MAX ) = INTEGER (Read)
*           The detector number corresponding to each detector index.
*        CCM_DETOR( IRC__MAX ) = INTEGER (Read)
*           The index within CCM_DETNO corresponding to row zero of the
*           NDF.

*  Arguments Given:
      INTEGER IDC
      INTEGER SLOW
      INTEGER SHIGH
      INTEGER DLOW
      INTEGER DHIGH

*  Arguments Returned:
      DOUBLE PRECISION MAPS( SLOW:SHIGH, DLOW:DHIGH, 3 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION A         ! sky longitude at boresight.
      DOUBLE PRECISION ANGLE     ! Scan angle at boresight.
      DOUBLE PRECISION AOUT      ! RA at detector centre.
      DOUBLE PRECISION B         ! sky latitude at boresight.
      DOUBLE PRECISION BOUT      ! DEC at detector centre.
      INTEGER          DETIND    ! Current detector index.
      INTEGER          DETNO     ! Current detector number.
      DOUBLE PRECISION ENDANG    ! Position angle of trajectory at end
                                 ! of shift.
      CHARACTER SCS*(IRA__SZSCS)! Sky Coordinate System used by
                                 ! IRC1_BPSSB.
      INTEGER          SMPNUM    ! Current sample number.
      REAL             SPEED     ! Scan speed at boresight.
      DOUBLE PRECISION TA        ! Intermediate sky longitude value.
      DOUBLE PRECISION TB        ! Intermediate sky latitude value.
      DOUBLE PRECISION YFP       ! Focal plane Y coord. in radians.
      DOUBLE PRECISION ZFP       ! Focal plane Z coord. in radians.
      DOUBLE PRECISION ZPA       ! Position angle of focal plane Z axis
                                 ! as seen from the projection of the
                                 ! detector centre onto the Y axis.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round every sample.
      DO SMPNUM = SLOW, SHIGH

*  Get the boresight position at this sample at the lowest detector
*  index.
         CALL IRC1_BPSSB( IDC, 1, REAL( SMPNUM ), DLOW, SCS, A, B,
     :                    ANGLE, SPEED, STATUS )

*  Convert the position and angle to EQUATORIAL(B1950) coordinates.
         CALL IRA_PACON( 1, A, B, ANGLE, SCS, 'EQUATORIAL(B1950)',
     :                   IRA__IRJEP, A, B, ANGLE, STATUS )

*  Loop round every detector (all detector samples with a given sample
*  number are assumed to be simultanesous, and thus have the same
*  boresight position).
         DO DETIND = DLOW, DHIGH

*  Get the detector number.
            DETNO = CCM_DETNO( DETIND + CCM_DETOR( IDC ), IDC )

*  Get the detector centre Y coordinate in radians.
            YFP = I90__DETY( DETNO )*IRA__DTOR/60.0D0

*  Move from the boresight along the Y axis, a distance equal to the Y
*  coordinate of the detector centre.
            CALL IRA_SHIFT( A, B, ANGLE, YFP, TA, TB, ENDANG, STATUS )

*  Find the position angle of the Z axis at the new position.
            ZPA = ENDANG - IRA__PIBY2

*  Get the detector centre Z coordinate in radians.
            ZFP = I90__DETZ( DETNO )*IRA__DTOR/60.0D0

*  Move from the new position along the Z axis, a distance equal to the
*  Z coordinate of the detector centre.
            CALL IRA_SHIFT( TA, TB, ZPA, ZFP, AOUT, BOUT, ENDANG,
     :                      STATUS)

*  Store the sample position in the output array.
            MAPS(SMPNUM, DETIND, 1 ) = AOUT
            MAPS(SMPNUM, DETIND, 2 ) = BOUT

* Find the position angle of the Y axis at the new position, and store
* in the output array.
            MAPS(SMPNUM, DETIND, 3 ) = ENDANG + IRA__PIBY2

*  Do next detector.
         END DO

*  Do next sample.
      END DO

      END
