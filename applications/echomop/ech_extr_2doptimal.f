      SUBROUTINE ECH_EXTR_2DOPTIMAL (
     :           NX,
     :           GOT_AN_ARC,
     :           FULL_SKYFIT,
     :           OBJ_MASK,
     :           IWAVE,
     :           READOUT,
     :           PHOTON,
     :           MAX_SKY_PIXELS,
     :           IMAGE,
     :           IMAGE_ERR,
     :           SKY_MODEL,
     :           SKY_MODEL_ERR,
     :           VARMAT,
     :           IMGDATA,
     :           ERRORS,
     :           VNORM,
     :           ARCPIX,
     :           PROFILE,
     :           SPEC,
     :           SPECSIG,
     :           ARCSPEC,
     :           ARCSIG,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_EXTR_2DOPTIMAL

*  Purpose:
*     Extracts 2-D distortion-corrected order using optimal weighting.

*  Description:
*     This routine calculates the optimal extraction weights based upon
*     the individual variances. These are then applied and an increment
*     weighted sum calculated. The input error array is assumed to contain
*     appropriate values to include the effect of flat fielding

*  Invocation:
*     CALL ECH_EXTR_2DOPTIMAL(
*     :    NX,
*     :    GOT_AN_ARC,
*     :    FULL_SKYFIT,
*     :    OBJ_MASK,
*     :    IWAVE,
*     :    READOUT, PHOTON,
*     :    MAX_SKY_PIXELS,
*     :    IMAGE,
*     :    IMAGE_ERR,
*     :    SKY_MODEL,
*     :    SKY_MODEL_ERR,
*     :    VARMAT,
*     :    IMGDATA,
*     :    ERRORS,
*     :    VNORM,
*     :    ARCPIX,
*     :    PROFILE,
*     :    SPEC,
*     :    SPECSIG,
*     :    ARCSPEC,
*     :    ARCSIG,
*     :    STATUS
*     :   )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NPIX = INTEGER (Given)
*        Number of values in increment.
*     GOT_AN_ARC = LOGICAL (Given)
*        TRUE if arc frame is to be extracted too.
*     FULL_SKYFIT = LOGICAL (Given)
*        TRUE if sky model effect on variance accounted for.
*     OBJ_MASK = INTEGER (Given)
*        Status of each pixel across profile.
*     IWAVE = INTEGER (Given)
*        Index of pixel in wavelength direction.
*     READOUT = REAL (Given)
*        Readout noise level in counts.
*     PHOTON = REAL (Given)
*        Photon-to-ADU conversion factor.
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum extent of sky and therefore dekker.
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and max_sky_pixels rows.
*     IMAGE_ERR = REAL (Given)
*        Input image errors of dimensions nx columns and max_sky_pixels rows.
*     VARMAT = REAL (Given)
*        Covariance matrix for sky model.
*     SKY_MODEL = REAL (Given)
*        Modeled sky intensities at offsets from trace.
*     SKY_MODEL_ERR = REAL (Given)
*        Modeled sky errors at offsets from trace.
*     ARC_FRAME = REAL (Given)
*        Input arc image of dimensions nx columns and max_sky_pixels rows.
*     IMGDATA = REAL (Returned)
*        Calculated object pixel intensities.
*     ERRORS = REAL (Returned)
*        Calculated object pixel errors.
*     VNORM = REAL (Returned)
*        Minimum variance per pixel.
*     PROFILE = REAL (Returned)
*        Profile probabilities for increment.
*     SPEC = REAL (Returned)
*        Object intensity.
*     SPECSIG = REAL (Returned)
*        Object variance.
*     ARCSPEC = REAL (Returned)
*        Arc intensity.
*     ARCSIG = REAL (Returned)
*        Arc variance.
*     ARCPIX = REAL (Given)
*        Rebinned arc order.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Calculate minimum variance floor
*     Determine object pixel offset limits
*     Loop twice to refine variance estiamtes
*        Load ordinary optimal weights when sky fit uncertainty insignificant.
*        If full_skyfit = .TRUE., modify weights to include uncertainty in
*                       sky fit optimised variances
*        Sum object (and arc) increment values using optimal weights
*        Stow optimal spectrum and its standard deviation
*        If another loop to go then
*           Loop thru profile values
*               If an object pixel then
*               Endif
*           End loop
*        Endif
*     End loop

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER MAX_SKY_PIXELS
      INTEGER IWAVE
      REAL READOUT
      REAL PHOTON
      LOGICAL GOT_AN_ARC
      LOGICAL FULL_SKYFIT
      REAL ARCPIX( MAX_SLICE_PIXELS )
      REAL IMAGE( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL IMAGE_ERR( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )

*  Arguments Returned:
      REAL IMGDATA( MAX_SLICE_PIXELS )
      REAL VARMAT( MAX_SKY_PIXELS, MAX_SKY_PIXELS )
      REAL VNORM
      REAL SPEC( NX )
      REAL SPECSIG( NX )
      REAL ARCSPEC( NX )
      REAL ARCSIG( NX )
      REAL PROFILE( MAX_SLICE_PIXELS )
      REAL ERRORS( MAX_SLICE_PIXELS )
      REAL SKY_MODEL( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL SKY_MODEL_ERR( NX, -MAX_SKY_PIXELS / 2 :MAX_SKY_PIXELS / 2 )
      INTEGER OBJ_MASK( -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL WEIGHT( MAX_SLICE_PIXELS )
      REAL VAR
      REAL VAR0
      REAL CMIN
      REAL WSUM
      REAL WVSUM
      REAL SUM
      REAL ARCSUM
      REAL SUMD
      REAL SUMD2
      REAL PESTIMATE
      REAL SKYS
      REAL STAR

      INTEGER I
      INTEGER J
      INTEGER K
      INTEGER IPROFILE
      INTEGER NPIX
      INTEGER IFAIL
      INTEGER YCOORD
      INTEGER OBJ_BELOW
      INTEGER OBJ_ABOVE

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Calculate minimum variance floor.
      VAR0 = READOUT * READOUT
      CMIN = ( V0_ALPHA * V0_BETA / ( V0_BETA - 1.0 ) ) ** 2.0 /
     :       PHOTON - VAR0 * PHOTON
      CMIN = MAX( 0.0, CMIN )

*  Determine object pixel offset limits.
      OBJ_BELOW = 999
      OBJ_ABOVE = -999
      DO I = -MAX_SKY_PIXELS / 2, MAX_SKY_PIXELS / 2
         IF ( OBJ_MASK( I ) .NE. 0 .AND. OBJ_BELOW .GT. I ) OBJ_BELOW =I
         IF ( OBJ_MASK( I ) .NE. 0 .AND. OBJ_ABOVE .LT. I ) OBJ_ABOVE =I
      END DO
      NPIX = OBJ_ABOVE - OBJ_BELOW + 1

*  Loop twice to refine variance estimates.
      DO J = 1, 2

*     Load ordinary optimal weights when sky fit uncertainty insignificant.
*     These will be changed if SKYFIT is true.
         DO I = 1, NPIX
            PESTIMATE = PROFILE( I )
            WEIGHT( I ) = PESTIMATE / ERRORS( I ) ** 2
         END DO

*     If full_skyfit, modify weights to include uncertainty in
*     sky-fit optimised variances.
         IF ( FULL_SKYFIT ) THEN
            CALL ECH_SKYFIT_WEIGHTS(
     :           NPIX,
     :           MAX_SKY_PIXELS,
     :           VNORM,
     :           .TRUE.,
     :           VARMAT,
     :           IMGDATA,
     :           ERRORS,
     :           WEIGHT,
     :           PROFILE,
     :           IFAIL
     :          )
         END IF

*     Sum object (and arc) increment values using optimal weights.
         SUM = 0.0
         ARCSUM = 0.0
         WSUM = 0.0
         SUMD2 = 0.0
         WVSUM = 0.0
         DO I = 1, NPIX
            PESTIMATE = PROFILE( I )
            SUM = SUM + WEIGHT( I ) * IMGDATA( I )
            ARCSUM = ARCSUM + WEIGHT( I ) * ARCPIX( I )
            WSUM = WSUM + WEIGHT( I ) * PESTIMATE
            WVSUM = WVSUM + ( WEIGHT( I ) * ERRORS( I ) ) ** 2
            SUMD = 0.0
            DO K = 1, NPIX
               SUMD = SUMD + WEIGHT( K ) * VARMAT( K, I )
            END DO
            SUMD2 = SUMD2 + WEIGHT( I ) * SUMD
         END DO

*     Stow optimal spectrum and its standard deviation.
         SPEC( IWAVE ) = SUM / WSUM
         SPECSIG( IWAVE ) = ABS( WVSUM + SUMD2 * VNORM ) /
     :         ( WSUM * WSUM )
         IF ( GOT_AN_ARC ) THEN
            ARCSPEC( IWAVE ) = ARCSUM / WSUM
            ARCSIG( IWAVE )  = ARCSUM / WSUM
         END IF

*     If another loop to go then.
         IF ( J .LT. 2 ) THEN
            I = 0

*        Loop thru profile values.
            DO IPROFILE = OBJ_BELOW, OBJ_ABOVE
               I = I + 1
               PESTIMATE = PROFILE( I )
               YCOORD = IPROFILE

*           If an object pixel then.
               IF ( OBJ_MASK( IPROFILE ) .EQ. 1 .AND.
     :              IMAGE( IWAVE, YCOORD ) .NE. ECH__BAD_REAL ) THEN
                  SKYS = SKY_MODEL( IWAVE, IPROFILE )
                  STAR = SPEC( IWAVE ) * PESTIMATE
                  VAR = VAR0 + IMAGE_ERR(IWAVE,YCOORD)  +
     :                  MAX( ABS( SKYS ), ABS( STAR + SKYS ) ) / PHOTON
                  ERRORS( I ) = SQRT( ABS( VAR ) )
               END IF
            END DO
         END IF
      END DO

      END
