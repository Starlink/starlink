      SUBROUTINE IRC1_IMCOI( IDC, SAMP, DETIN, IDA, INIT, C, STATUS )
*+
*  Name:
*     IRC1_IMCOI

*  Purpose:
*     Find the image coordinates of a given sample.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_IMCOI( IDC, SAMP, DETIN, IDA, INIT, C, STATUS )

*  Description:
*     This routine performs the work for IRC_IMCO, without argument
*     verification.  If INIT is true, or if the specified detector
*     index is different to the index specified on the previous call of
*     this routine, then cubic spline fits are made to the image X and
*     Y coordinates and the in-scan displacement of the required
*     detector centre at two degree intervals. These fits are then
*     evaluated for the given sample, together with their derivatives,
*     and these values are used to calculate the returned coefficients.
*     The parameters defining the fits are saved for use in the next
*     call to this routine (using the Fortran SAVE statement).

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the CRDD file. No check is made on the
*        validity of IDC.
*     SAMP = REAL (Given)
*        The fractional sample number. If this has the Starlink "BAD"
*        value (VAL__BADR) then all the returned arguements are set to
*        the bad value.
*     DETIN = INTEGER (Given)
*        The detector index to which SAMP refers. No check is made on
*        the validity of DETIN.
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information which defines
*        the mapping from sky coordinates to image coordinates (see
*        ID2).
*     INIT = LOGICAL (Given)
*        INIT should be set true on the first call to this routine, and
*        false on subsequent calls. If either a new CRDD file, or new
*        astrometry information is supplied, then INIT should be set
*        true again for one call to this routine.
*     C( 6 ) = REAL (Returned)
*        The coefficients of the linear transformation from focal plane
*        offsets (centred on the detector centre), to image coordinates
*        (see description of routine IRC_IMCO).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)

*  History:
*     11-NOV-1991 (DSB):
*        Original version.
*     13-NOV-1991 (DSB):
*        Changed to return transformation coefficients.
*     11-FEB-1992 (DSB):
*        Modified to use CCM_DETOR.
*     14-MAY-1992 (DSB):
*        Modified to use DOUBLE PRECISION NAG routines (single
*        precision is not available on UNIX).
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC errors.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_SLOW( IRC__MAX ) = INTEGER (Read)
*           Lowest sample number in the DATA array.
*        CCM_SHIGH( IRC__MAX ) = INTEGER (Read)
*           Highest sample number in the DATA array.
*        CCM_DETNO( IRC__MXD2S, IRC__MAX ) = INTEGER (Read)
*           The detector number corresponding to each detector index.
*        CCM_DETOR( IRC__MAX ) = INTEGER (Read)
*           The index within CCM_DETNO corresponding to row zero of the
*           NDF.

*  Arguments Given:
      INTEGER IDC
      REAL    SAMP
      INTEGER DETIN
      INTEGER IDA
      LOGICAL INIT

*  Arguments Returned:
      REAL C( 6 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION CDST( 14 )! Coefficients for in-scan offset
                                 ! spline.
      DOUBLE PRECISION CX( 14 )  ! Coefficients for X spline.
      DOUBLE PRECISION CY( 14 )  ! Coefficients for Y spline.
      DOUBLE PRECISION DPW1( 10 )! Double precision work space.
      DOUBLE PRECISION DW1( 76 ) ! Double precision workspace.
      DOUBLE PRECISION FACTOR    ! Multiplicative factor.
      DOUBLE PRECISION FITA( 10 )! Sky longitude at the detector centre
                                 ! for each interpolated sample.
      DOUBLE PRECISION FITB( 10 )! Sky latitude at the detector centre
                                 ! for each interpolated sample.
      DOUBLE PRECISION FITDST( 10 )! Double precision version of FDIST.
      DOUBLE PRECISION FITSMP( 10 )! Interpolated sample numbers.
      DOUBLE PRECISION FITX( 10 )! Image X coordinate at the detector
                                 ! centre for each interpolated sample.
      DOUBLE PRECISION FITY( 10 )! Image Y coordinate at the detector
                                 ! centre for each interpolated sample.
      DOUBLE PRECISION LAMDST( 14 )! Knots for in-scan offset spline.
      DOUBLE PRECISION LAMX( 14 )! Knots for X spline.
      DOUBLE PRECISION LAMY( 14 )! Knots for Y spline.
      DOUBLE PRECISION SDST( 4 ) ! In-scan spline value plus first
                                 ! three derivative values.
      DOUBLE PRECISION SX( 4 )   ! X spline value plus first three
                                 ! derivative values.
      DOUBLE PRECISION SY( 4 )   ! Y spline value plus first three
                                 ! derivative values.


      INTEGER DETNO              ! Detector number corresponding to
                                 ! DETIN.
      INTEGER FITDET( 10 )       ! Detector indices at each interpolated
                                 ! sample (radians).
      INTEGER I                  ! Loop count.
      INTEGER IFAIL              ! NAG error status.
      INTEGER LDETIN             ! Last times value of DETIN.
      INTEGER NCAP7              ! Four more than the number of
                                 ! interpolated samples.
      INTEGER NSTEP              ! No. of interpolated samples.


      REAL    FDIST( 10 )        ! In-scan offset from SAMP0 to each
                                 ! interpolated sample (radians).
      REAL    FSAMP( 10 )        ! Single precision interpolated sample
                                 ! numbers.
      REAL    LENGTH             ! Arc length of data in CRDD file.
      REAL    RW1( 76 )          ! Real workspace.
      REAL    SAMP0              ! First interpolated sample number.
      REAL    SAMPC              ! Sample number corresponding to the
                                 ! centre of the available data.
      REAL    STEP               ! Arc length of the interval between
                                 ! adjacent interpolated samples.
      REAL    TOTLEN             ! Arc length covered by all the
                                 ! interpolated samples.

*  Ensure that the parameters used to define the spline fits are saved
*  for use in future calls of this routine.
      SAVE LAMX, CX, LAMY, CY, LAMDST, CDST, NCAP7, LDETIN
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Perform initialisation if required.
      IF( INIT .OR. DETIN .NE. LDETIN ) THEN

*  Find the total arc-length of the available CRDD.
         CALL IRC1_DISTI( IDC, REAL( CCM_SHIGH( IDC ) ), DETIN,
     :                    REAL( CCM_SLOW( IDC ) ), DETIN, LENGTH,
     :                    STATUS )

*  Find a step length which ensures that between two and eight steps
*  occur within the CRDD file. The nominal step size is about 2 degrees
*  (=0.03 radians).
         STEP = MAX( LENGTH*0.125, MIN( 0.03, LENGTH*0.5 ) )

*  Calculate the total number of steps to be fitted by the cubic
*  splines. There must be between 4 and 10 points, so some will be
*  outside the range of sample numbers for which data values are
*  available in the CRDD file.
         NSTEP = MIN( 10, MAX( 4, INT( 2.0*LENGTH/STEP ) + 1 ) )

*  Find the sample number of the first sample to be fitted by the
*  spline. This is chosen such that the centre of the CRDD file
*  corresponds to the centre of the centre step.
         SAMPC = 0.5*REAL( CCM_SLOW( IDC ) + CCM_SHIGH( IDC ) )
         TOTLEN = STEP*REAL( NSTEP - 1 )
         CALL IRC1_OFFSI( IDC, SAMPC, DETIN, DETIN, 0.5*TOTLEN,
     :                    SAMP0, STATUS )

         FSAMP( 1 ) = SAMP0
         FDIST( 1 ) = 0.0
         FITDET( 1 ) = DETIN

*  Loop through the other steps finding the sample corresponding to
*  each. Store the detector index, and in-scan offset to each step from
*  the first step.
         DO I = 2, NSTEP
            FITDET( I ) = DETIN
            FDIST( I ) = ( I - 1 )*STEP
            CALL IRC1_OFFSI( IDC, SAMP0, DETIN, DETIN, -FDIST( I ),
     :                       FSAMP( I ), STATUS )
         END DO

*  Get the detector number corresponding to DETIN.
         DETNO = CCM_DETNO( DETIN + CCM_DETOR( IDC ), IDC )

*  Get the sky coordinates of the detector centre at each of the
*  interpolated samples. (FITX and FITY are used as workspace.)
         CALL IRC_SKCO2( IDC, NSTEP, FSAMP, FITDET,
     :                   REAL( IRA__DTOR*I90__DETZ( DETNO )/60.0 ),
     :                   REAL( IRA__DTOR*I90__DETY( DETNO )/60.0 ),
     :                   DPW1, FITX, FITY, RW1, FITA, FITB, STATUS )

*  Project the sky coordinates into image coordinates.
         CALL IRA_TRANS( NSTEP, FITA, FITB, .FALSE.,
     :                   'EQUATORIAL(B1950)', IDA, FITX, FITY, STATUS )

*  Store double precision versions of the interpolated sample numbers,
*  and in-scan distance.
         DO I = 1, NSTEP
            FITSMP( I ) = DBLE( FSAMP( I ) )
            FITDST( I ) = DBLE( FDIST( I ) )
         END DO

*  Fit a cubic spline to the X coordinate values.
         IFAIL = 1
*         CALL E01BAF( NSTEP, FITSMP, FITX, LAMX, CX, NSTEP + 4, DW1,
*     :                6*NSTEP + 16, IFAIL )

         STATUS = IRC__NAGER
         CALL ERR_REP('IRC1_IMCOI_ERR0',
     :        'NAG not compiled into this version of IRAS90.',
     :        STATUS)
         GO TO 999

*  Check the IFAIL value.
         IF( IFAIL .NE. 0 ) THEN
            STATUS = IRC__NAGER
            CALL MSG_SETI( 'IF', IFAIL )
            CALL ERR_REP( 'IRC1_IMCOI_ERR1',
     :         'IRC1_IMCOI: E01BAF returned IFAIL = ^IF when fitting '//
     :         'X coordinate values', STATUS )
            GO TO 999
         END IF

*  Fit a cubic spline to the Y coordinate values.
         IFAIL = 1
*         CALL E01BAF( NSTEP, FITSMP, FITY, LAMY, CY, NSTEP + 4, DW1,
*     :                6*NSTEP + 16, IFAIL )

         STATUS = IRC__NAGER
         CALL ERR_REP('IRC1_IMCOI_ERR0',
     :        'NAG not compiled into this version of IRAS90.',
     :        STATUS)
         GO TO 999

*  Check the IFAIL value.
         IF( IFAIL .NE. 0 ) THEN
            STATUS = IRC__NAGER
            CALL MSG_SETI( 'IF', IFAIL )
            CALL ERR_REP( 'IRC1_IMCOI_ERR2',
     :         'IRC1_IMCOI: E01BAF returned IFAIL = ^IF when fitting '//
     :         'Y coordinate values', STATUS )
            GO TO 999
         END IF

*  Fit a cubic spline to the in-scan distance values.
         IFAIL = 1
         CALL E01BAF( NSTEP, FITSMP, FITDST, LAMDST, CDST, NSTEP + 4,
     :                DW1, 6*NSTEP + 16, IFAIL )

*  Check the IFAIL value.
         IF( IFAIL .NE. 0 ) THEN
            STATUS = IRC__NAGER
            CALL MSG_SETI( 'IF', IFAIL )
            CALL ERR_REP( 'IRC1_IMCOI_ERR3',
     :         'IRC1_IMCOI: E01BAF returned IFAIL = ^IF when fitting '//
     :         'in-scan distance', STATUS )
            GO TO 999
         END IF

* Save the value of "NCAP7" required by the spline evaluation routines.
         NCAP7 = NSTEP + 4

      END IF

*  If the sample number is good...
      IF( SAMP .NE. VAL__BADR ) THEN

*  Evaluate the cubic spline giving the image X coordinate at the centre
*  of the sample, together with its first three derivatives.
         IFAIL = 1
*         CALL E02BCF( NCAP7, LAMX, CX, DBLE( SAMP ), 1, SX, IFAIL )

         STATUS = IRC__NAGER
         CALL ERR_REP('IRC1_IMCOI_ERR0',
     :        'NAG not compiled into this version of IRAS90.',
     :        STATUS)
         GO TO 999

*  Check the IFAIL value.
         IF( IFAIL .NE. 0 ) THEN
            STATUS = IRC__NAGER
            CALL MSG_SETI( 'IF', IFAIL )
            CALL ERR_REP( 'IRC1_IMCOI_ERR4',
     :       'IRC1_IMCOI: E02BCF returned IFAIL = ^IF when evaluating'//
     :       'X coordinate spline', STATUS )
            GO TO 999
         END IF

*  Evaluate the cubic spline giving the image Y coordinate at the centre
*  of the sample, together with its first three derivatives.
         IFAIL = 1
         CALL E02BCF( NCAP7, LAMY, CY, DBLE( SAMP ), 1, SY, IFAIL )

*  Check the IFAIL value.
         IF( IFAIL .NE. 0 ) THEN
            STATUS = IRC__NAGER
            CALL MSG_SETI( 'IF', IFAIL )
            CALL ERR_REP( 'IRC1_IMCOI_ERR5',
     :       'IRC1_IMCOI: E02BCF returned IFAIL = ^IF when evaluating'//
     :       'Y coordinate spline', STATUS )
            GO TO 999
         END IF

*  Evaluate the cubic spline giving the in-scan distance at the centre
*  of the sample, together with its first three derivatives.
         IFAIL = 1
         CALL E02BCF( NCAP7, LAMDST, CDST, DBLE( SAMP ), 1, SDST,
     :                IFAIL )

*  Check the IFAIL value.
         IF( IFAIL .NE. 0 ) THEN
            STATUS = IRC__NAGER
            CALL MSG_SETI( 'IF', IFAIL )
            CALL ERR_REP( 'IRC1_IMCOI_ERR6',
     :       'IRC1_IMCOI: E02BCF returned IFAIL = ^IF when evaluating'//
     :       'in-scan offset spline', STATUS )
            GO TO 999
         END IF

*  Evaluate the returned coefficients.
         IF( SDST( 2 ) .NE. 0.0D0 ) THEN
            FACTOR = 1.0D0/SDST( 2 )

            C( 1 ) = REAL( SX( 1 ) )
            C( 2 ) = REAL( -SY( 2 )*FACTOR )
            C( 3 ) = REAL( -SX( 2 )*FACTOR )
            C( 4 ) = REAL( SY( 1 ) )
            C( 5 ) = REAL( SX( 2 )*FACTOR )
            C( 6 ) = REAL( -SY( 2 )*FACTOR )

         ELSE
            STATUS = IRC__ZEROS
            CALL ERR_REP( 'IRC1_IMCOI_ERR7',
     :         'IRC1_IMCOI: No in-scan shift between CRDD samples',
     :         STATUS )

         END IF

*  If the sample number is bad, set the returned values bad.
      ELSE
         C( 1 ) = VAL__BADR
         C( 2 ) = VAL__BADR
         C( 3 ) = VAL__BADR
         C( 4 ) = VAL__BADR
         C( 5 ) = VAL__BADR
         C( 6 ) = VAL__BADR
      END IF

*  Save the detector index used.
      LDETIN = DETIN

 999  CONTINUE

      END
