      SUBROUTINE IRC1_DPSSB( IDC, NVAL, SAMPLE, DETIND, SCS, A, B,
     :                     ANGLE, SPEED, STATUS )
*+
*  Name:
*     IRC1_DPSSB

*  Purpose:
*     Returns detector centre positions at a set of samples, assuming
*     SURVEY_BSIGHT data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_DPSSB( IDC, NVAL, SAMPLE, DETIND, SCS, A, B, ANGLE,
*                    SPEED, STATUS )

*  Description:
*     Detector positions are found by first finding the boresight
*     positions and then offseting from the boresight to each detector
*     in turn.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     NVAL = INTEGER (Given)
*        The number of samples in the input and output lists.
*     SAMPLE( NVAL ) = REAL (Given)
*        A list of fractional sample numbers. If any sample number has
*        the Starlink "BAD" value (VAL__BADR) then the corresponding
*        elements of the returned arrays are set to the bad value.
*     DETIND( NVAL ) = INTEGER (Given)
*        A list of detector indices.
*     SCS = CHARACTER * ( * ) (Returned)
*        The Sky Coordinate System in which the detector centre
*        positions and angles are returned. See the IRA_ documentation
*        (ID/1) for more information about Sky Coordinate Systems. The
*        variable supplied for argument SCS should have a declared
*        length equal to the symbolic constant IRA__SZSCS.
*     A( NVAL ) = DOUBLE PRECISION (Returned)
*        An array holding the sky longitude values of the
*        detector centres at the moment each sample specified in the
*        input lists was taken (radians). The values are in the Sky
*        Coordinate System returned in argument SCS.
*     B( NVAL ) = DOUBLE PRECISION (Returned)
*        An array holding the sky latitude values of the
*        detector centres at the moment each sample specified in the
*        input lists was taken (radians). The values are in the Sky
*        Coordinate System returned in argument SCS.
*     ANGLE( NVAL ) = DOUBLE PRECISION (Returned)
*        The scan angle at the detector. This is measured from north (in
*        the coordinate system specified by SCS) to a line parallel to
*        the focal plane Y axis passing through the detector centre. The
*        angle is in radians and is measured positive in the same sense
*        as rotation from north to east.
*     SPEED( NVAL ) = REAL (Returned)
*        The scan speed in radians per second. Positive values imply
*        that sources move in the positive focal plane Y direction (i.e
*        in the "with-survey" direction). Negative values imply that
*        sources move in the negative Y direction (i.e. "anti-survey").
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*                (DSB):
*        Original version.
*     9-MAY-1991 (DSB):
*        Updated for IRA version 2.
*     11-FEB-1992 (DSB):
*        Modified to use CCM_DETOR.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'PRM_PAR'          ! Starlink BAD values.
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_DLOW( IRC__MAX ) = INTEGER (Read)
*           Lowest detector index in the DATA array.
*        CCM_DETNO( IRC__MXD2S, IRC__MAX ) = INTEGER (Read)
*           The detector number corresponding to each detector index.
*        CCM_DETOR( IRC__MAX ) = INTEGER (Read)
*           The index within CCM_DETNO corresponding to row zero of the
*           NDF.

*  Arguments Given:
      INTEGER IDC
      INTEGER NVAL
      REAL    SAMPLE( NVAL )
      INTEGER DETIND( NVAL )

*  Arguments Returned:
      CHARACTER SCS*(*)
      DOUBLE PRECISION A( NVAL )
      DOUBLE PRECISION B( NVAL )
      DOUBLE PRECISION ANGLE( NVAL )
      REAL    SPEED( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER          DETNO     ! Current detector number.
      DOUBLE PRECISION ENDANG    ! Position angle of trajectory at end
                                 ! of a shift.
      INTEGER          I         ! Loop count.
      DOUBLE PRECISION TA        ! Intermediate sky longitude value.
      DOUBLE PRECISION TB        ! Intermediate sky latitude value.
      DOUBLE PRECISION YFP       ! Focal plane Y coord. in radians.
      DOUBLE PRECISION ZPA       ! Position angle of focal plane Z axis
                                 ! as seen from the projection of the
                                 ! detector centre onto the Y axis.
      DOUBLE PRECISION ZFP       ! Focal plane Z coord. in radians.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the boresight position for each detector sample.
      CALL IRC1_BPSSB( IDC, NVAL, SAMPLE, DETIND, SCS, A, B, ANGLE,
     :                 SPEED, STATUS )

*  Loop round each detector sample.
      DO I = 1, NVAL

*  If the sample is bad, pass on (IRC1_BPSSB will already have assigned
*  bad values to the output array values).
         IF( SAMPLE( I ) .NE. VAL__BADR ) THEN

*  Get the detector number.
            DETNO = CCM_DETNO( DETIND(I) + CCM_DETOR( IDC ), IDC )

*  Get the detector centre Y coordinate in radians.
            YFP = I90__DETY( DETNO )*IRA__DTOR/60.0D0

*  Move from the boresight along the Y axis, a distance equal to the Y
*  coordinate of the detector centre.
            CALL IRA_SHIFT( A(I), B(I), ANGLE(I), YFP, TA, TB, ENDANG,
     :                      STATUS )

*  Find the position angle of the Z axis at the new position.
            ZPA = ENDANG - IRA__PIBY2

*  Get the detector centre Z coordinate in radians.
            ZFP = I90__DETZ( DETNO )*IRA__DTOR/60.0D0

*  Move from the new position along the Z axis, a distance equal to the
*  Z coordinate of the detector centre.
            CALL IRA_SHIFT( TA, TB, ZPA, ZFP, A(I), B(I), ENDANG,
     :                      STATUS )

*  Find the bearing of the Y axis at the new position.
            ANGLE(I) = ENDANG + IRA__PIBY2

         END IF

      END DO

      END
