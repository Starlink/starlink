      SUBROUTINE ECH_EXTR_2DECOSOPT(
     :           NX,
     :           GOT_QUALITY,
     :           GOT_AN_ARC,
     :           FULL_SKYFIT,
     :           OBJ_MASK,
     :           OBJ_NPOLY,
     :           REJECT_THRESH,
     :           IWAVE,
     :           READOUT,
     :           PHOTON,
     :           MAX_SKY_PIXELS,
     :           IMAGE,
     :           IMAGE_ERR,
     :           QUALITY,
     :           SKY_MODEL,
     :           SKY_MODEL_ERR,
     :           VARMAT,
     :           IMGDATA,
     :           IMGSUM,
     :           ERRORS,
     :           VNORM,
     :           ARCPIX,
     :           PROFILE,
     :           NRAYS,
     :           SPEC,
     :           SPECSIG,
     :           ARCSPEC,
     :           ARCSIG,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_EXTR_2DECOSOPT

*  Purpose:
*     Extract 2-D fitted spectrum and auto-reject cosmic rays.

*  Description:
*     This routine rejects 'bad' pixels from the increment and
*     adjusts the variance weighting scheme accordingly.

*  Invocation:
*     CALL ECH_EXTR_2DECOSOPT(
*     :    NX,
*     :    GOT_QUALITY,
*     :    GOT_AN_ARC,
*     :    FULL_SKYFIT,
*     :    OBJ_MASK,
*     :    OBJ_NPOLY,
*     :    REJECT_THRESH,
*     :    IWAVE,
*     :    READOUT,
*     :    PHOTON,
*     :    MAX_SKY_PIXELS,
*     :    IMAGE,
*     :    IMAGE_ERR,
*     :    QUALITY,
*     :    SKY_MODEL,
*     :    SKY_MODEL_ERR,
*     :    VARMAT,
*     :    IMGDATA,
*     :    IMGSUM,
*     :    ERRORS,
*     :    VNORM,
*     :    ARCPIX,
*     :    PROFILE,
*     :    NRAYS,
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
*     GOT_QUALITY = LOGICAL (Given)
*        TRUE if quality data is available.
*     GOT_AN_ARC = LOGICAL (Given)
*        TRUE if arc frame is to be extracted too.
*     FULL_SKYFIT = LOGICAL (Given)
*        TRUE if sky model effect on variance accounted for.
*     OBJ_MASK = INTEGER (Given)
*        Status of each pixel across profile.
*     OBJ_NPOLY = INTEGER (Given)
*        Degree of polynomial used to model object profiles.
*     IWAVE = INTEGER (Given)
*        Index of pixel in wavelength direction.
*     READOUT = REAL (Given)
*        Readout noise level in counts.
*     PHOTON = REAL (Given)
*        photon to ADU conversion factor.
*     REJECT_THRESH = REAL (Given)
*        Reject threshold for CR detection.
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum extent of sky and therefore dekker.
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and max_sky_pixels rows.
*     IMAGE_ERR = REAL (Given)
*        Input image-errors of dimensions nx columns and max_sky_pixels rows.
*     QUALITY = BYTE (Given)
*        Input image quality of dimensions nx columns and max_sky_pixels rows.
*     VARMAT = REAL (Given)
*        Covariance matrix for sky model.
*     SKY_MODEL = REAL (Given)
*        Modeled sky intensities at offsets from trace.
*     SKY_MODEL_ERR = REAL (Given)
*        Modeled sky errors at offsets from trace.
*     ARC_FRAME = REAL (Given)
*        Input arc frame image of dimensions nx columns and ny rows..
*     IMGDATA = REAL (Returned)
*        Calculated object pixel intensities.
*     IMGSUM = REAL (Returned)
*        Calculated object  intensity at increment.
*     ERRORS = REAL (Returned)
*        Calculated object pixel errors.
*     VNORM = REAL (Returned)
*        Minimum variance per pixel.
*     PROFILE = REAL (Returned)
*        Profile probabilities for increment.
*     NRAYS = INTEGER (Returned)
*        Running bad pixel total.
*     SPEC = REAL (Returned)
*        Object intensity.
*     SPECSIG = REAL (Returned)
*        Object variance.
*     ARCSPEC = REAL (Returned)
*        arc intensity.
*     ARCSIG = REAL (Returned)
*        arc variance.
*     ARCPIX = REAL (Given)
*        Rebinned arc order.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Calculate minimum variance floor
*     Determine object pixel offset limits
*     Initialise control and counter
*     Initialise variables which will be used to correct
*        for the correlation between the spectrum point and data point (01/07/88)
*     Loop thru extracted values
*           If an object pixel and value is positive then
*              Initialise weight to unity and add to variance estimate
*           Endif
*     End loop
*     Sum covariance
*     Loop until all rejects have been treated
*        Update counter and initialise bad-pixel records
*        Loop thru profile
*           Compute revised variance estimate using profile model
*           If pixel ok so far then
*                 If object pixel and positive valued then
*                    If bad quality pixel then
*                       If highest valued bad pixel this increment remember it
*                    Endif
*                    Avoid crashes in zero photon case by pretending that there is one photon.
*                    Evaluate probability of chance correlation
*                 Endif
*           Endif
*        End loop
*        Reject worst outlier (changed to use highest intensity ray already found)
*        Load ordinary optimal weights when sky fit uncertainty insignificant.
*        Sum object (and arc) increment values using optimal weights
*        Stow optimal spectrum and its standard deviation
*     End loop
*     Update running total of bad pixels

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER OBJ_NPOLY
      INTEGER MAX_SKY_PIXELS
      INTEGER IWAVE
      REAL REJECT_THRESH
      REAL READOUT
      REAL PHOTON
      LOGICAL GOT_QUALITY
      LOGICAL GOT_AN_ARC
      LOGICAL FULL_SKYFIT
      REAL ARCPIX( MAX_SLICE_PIXELS )
      REAL IMAGE( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL IMAGE_ERR( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      BYTE QUALITY( NX,  -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )

*  Arguments Returned:
      INTEGER NRAYS
      REAL IMGDATA( MAX_SLICE_PIXELS )
      REAL VARMAT( MAX_SKY_PIXELS, MAX_SKY_PIXELS )
      REAL IMGSUM
      REAL VNORM
      REAL SPEC( NX )
      REAL SPECSIG( NX )
      REAL ARCSPEC( NX )
      REAL ARCSIG( NX )
      REAL PROFILE( MAX_SLICE_PIXELS )
      REAL ERRORS( MAX_SLICE_PIXELS )
      REAL SKY_MODEL( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL SKY_MODEL_ERR( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      INTEGER OBJ_MASK( -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )

*  Status:
      INTEGER STATUS

*  Local Constants:
      REAL EPS
      PARAMETER ( EPS = 0.02 )

*  Local Variables:
      REAL WEIGHT( MAX_SLICE_PIXELS )
      REAL VAR
      REAL VAR0
      REAL CMIN
      REAL VOLD
      REAL RATIO
      REAL WSUM
      REAL WVSUM
      REAL RMIN
      REAL RMAX
      REAL SUM
      REAL ARCSUM
      REAL SUMD
      REAL SUMD2
      REAL RATLO
      REAL RATHI
      REAL PESTIMATE
      REAL SKYS
      REAL STAR
      REAL OLD_STAR
      REAL VT
      REAL WORST_RAY

      INTEGER WORST_AT
      INTEGER I
      INTEGER J
      INTEGER IPROFILE
      INTEGER YCOORD
      INTEGER NPIX
      INTEGER IFAIL
      INTEGER OBJ_BELOW
      INTEGER OBJ_ABOVE
      INTEGER NREJT
      INTEGER ICYCLE

      LOGICAL PIXEL_OK

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      REAL CHANCE
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

*  Initialise control and counter.
      RATLO = - REJECT_THRESH
      RATHI = REJECT_THRESH
      NREJT = 0
      ICYCLE = 0
      WVSUM = 0.0

*  Initialise variables which will be used to correct
*  for the correlation between the spectrum point and data point (01/07/88).
*  Loop thru extracted values.
      DO I = 1, NPIX
         PESTIMATE = PROFILE( I )
         IPROFILE = OBJ_BELOW + I - 1
         YCOORD = IPROFILE

*     If an object pixel and not BAD then.
         IF ( OBJ_MASK( IPROFILE ) .EQ. 1 .AND.
     :        IMAGE( IWAVE, YCOORD ) .NE. ECH__BAD_REAL ) THEN

*        Initialise weight to unity and add to variance estimate.
            WEIGHT( I ) = 1.0
            SKYS = SKY_MODEL( IWAVE, IPROFILE )
            STAR = SPEC( IWAVE ) * PESTIMATE
            WVSUM = WVSUM + ( VAR0 + IMAGE_ERR( IWAVE, YCOORD ) ) +
     :              ABS( STAR + SKYS ) / PHOTON
         END IF
      END DO

*  Sum covariance.
      SUMD2 = 0.0
      DO J = 1, NPIX
         DO I = 1, NPIX
            SUMD2 = SUMD2 + VARMAT( I, J )
         END DO
      END DO
      WSUM = 1.0

*  Loop until all rejects have been treated.
      WORST_AT = 0
      DO WHILE( ICYCLE .LE. 2 .OR. WORST_AT .GT. 0 )

*     Update counter and initialise bad-pixel records.
         ICYCLE = ICYCLE + 1
         I = 0
         WORST_RAY = 0.0
         WORST_AT = 0
         RMIN = 2.0
         RMAX = 10.0

*     Loop through profile.
         DO IPROFILE = OBJ_BELOW, OBJ_ABOVE
            I = I + 1

*        Compute revised variance estimate using profile model.
*        Skip rejected pixels, find worst pixel.  At least one
*        extra run is forced to take advantage of the improved
*        estimate of the spectrum, even if no pixel is rejected.
            PESTIMATE = PROFILE( I )

*        If pixel ok so far then.
            IF ( ERRORS( I ) .GT. 0.0 ) THEN
               YCOORD = IPROFILE

*           If object pixel and not BAD then.
               IF ( OBJ_MASK( IPROFILE ) .EQ. 1 .AND.
     :              IMAGE( IWAVE, YCOORD ) .NE. ECH__BAD_REAL ) THEN

*              If bad quality pixel then.
                  PIXEL_OK = .TRUE.
                  IF ( GOT_QUALITY ) THEN
                     IF ( QUALITY( IWAVE, YCOORD ) .GT. 0 )
     :                  PIXEL_OK = .FALSE.
                  END IF

                  IF ( .NOT. PIXEL_OK ) THEN

*                 If highest valued bad pixel this increment remember it.
                     IF ( IMAGE( IWAVE, YCOORD ) .GT. WORST_RAY ) THEN
                        WORST_RAY = IMAGE( IWAVE, YCOORD )
                        WORST_AT = I
                     END IF
                  END IF

*              Avoid crashes in zero-photon case by pretending that
*              there is one photon.  This will be a bad approximation
*              if there are very many such instances since then the
*              chance of only one photon is small. However, this
*              should be rare.
                  SKYS = SKY_MODEL( IWAVE, IPROFILE )
                  STAR = SPEC( IWAVE ) * PESTIMATE
                  OLD_STAR = STAR
                  STAR = MAX( PESTIMATE / PHOTON, STAR )
                  VOLD = ERRORS( I ) ** 2
                  VAR = VAR0 + IMAGE_ERR( IWAVE, YCOORD ) +
     :                  MAX( ABS( SKYS ), ABS( STAR + SKYS ) ) / PHOTON
                  ERRORS( I ) = SQRT( ABS( VAR ) )

*              A correction is made before calling CHANCE to account for
*              correlation between the estimated spectrum and the
*              data value. This increases the chance of rejection on
*              data points with high weights which was too low before.
*              Approximate decrease in var by (1-f*f/sum of f*f) where f
*              is the profile factor. (01/07/88 TRM).
                  VAR = VOLD
                  IF ( WSUM .GT. 0.0 )
     :               VAR = VAR * ( 1.0 - 2.0 *
     :                     WEIGHT ( I ) * PESTIMATE / WSUM ) +
     :                     ( WVSUM + SUMD2 * VNORM ) *
     :                     ( PESTIMATE / WSUM ) ** 2
                  VAR = SQRT( ABS( VAR ) )
                  VT = MAX( VAR, EPS * OLD_STAR )

*              Evaluate probability of chance correlation.
                  RATIO = CHANCE( IMGDATA( I ), OLD_STAR, VT,
     :                            RATLO, RATHI )
                  IF ( RATIO .GT. -0.5 ) THEN
                     IF ( RATIO .GT. RMAX) THEN
                        RMAX = RATIO
                        RMIN = -1.0

                     ELSE IF ( RATIO .LT. RMIN ) THEN
                        RMIN = RATIO
                     END IF
                  END IF
               END IF
            END IF
         END DO

*     Reject worst outlier (changed to use highest-intensity ray already found).
         IF ( WORST_AT .GT. 0 ) THEN
            ERRORS( WORST_AT ) = -ABS( ERRORS( WORST_AT ) )
            NREJT = NREJT + 1
            WORST_RAY = 0.0
            WORST_AT = 0
         END IF

*     Load ordinary optimal weights when sky fit uncertainty insignificant.
*     These will be changed if full skyfit is true.
         DO I = 1, NPIX
            IF ( ERRORS( I ) .GT. 0.0 ) THEN
               WEIGHT( I ) = PROFILE( I ) / ERRORS( I ) ** 2
            END IF
         END DO

*     If full_skyfit, modify weights to include uncertainty in
*     sky fit optimised variances.
         IF ( FULL_SKYFIT ) THEN
            CALL ECH_SKYFIT_WEIGHTS(
     :           NPIX,
     :           MAX_SKY_PIXELS,
     :           VNORM,
     :           .FALSE.,
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
            IF ( ERRORS( I ) .GT. 0.0 ) THEN
               SUM = SUM  + WEIGHT( I ) * IMGDATA( I )
               ARCSUM = ARCSUM  + WEIGHT( I ) * ARCPIX( I )
               WSUM = WSUM + WEIGHT( I ) * PESTIMATE
               WVSUM = WVSUM + ( WEIGHT( I ) * ERRORS( I ) ) ** 2
               SUMD = 0.0
               DO J = 1, NPIX
                  IF ( ERRORS( J ) .GT. 0. ) THEN
                     SUMD = SUMD + WEIGHT( J ) * VARMAT( J, I )
                  END IF
               END DO
               SUMD2 = SUMD2 + WEIGHT( I ) * SUMD
            END IF
         END DO

*     Stow optimal spectrum and its standard deviation.
         SPEC( IWAVE ) = SUM / WSUM
         SPECSIG( IWAVE ) = ABS( WVSUM + SUMD2 * VNORM ) /
     :         ( WSUM * WSUM )
         IF ( GOT_AN_ARC ) THEN
            ARCSPEC( IWAVE ) = ARCSUM / WSUM
            ARCSIG( IWAVE )  = ARCSUM / WSUM
         END IF
      END DO

*  Update running total of bad pixels.
      NRAYS = NRAYS + NREJT

      END
