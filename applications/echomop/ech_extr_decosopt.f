      SUBROUTINE ECH_EXTR_DECOSOPT(
     :           NX,
     :           NY,
     :           GOT_QUALITY,
     :           GOT_AN_ARC,
     :           GOT_BALANCE,
     :           NO_ERRORS,
     :           FULL_SKYFIT,
     :           OBJ_MASK,
     :           OBJ_BELOW,
     :           OBJ_ABOVE,
     :           OBJ_NPOLY,
     :           REJECT_THRESH,
     :           IWAVE,
     :           Y_CENTER,
     :           VAR0,
     :           PHOTON,
     :           CMIN,
     :           MAX_SKY_PIXELS,
     :           IMAGE,
     :           IMAGE_ERR,
     :           QUALITY,
     :           SKY_MODEL_ERR,
     :           FLAT_MODEL_ERR,
     :           VARMAT,
     :           NPIX,
     :           IMGDATA,
     :           IMGSUM,
     :           ERRORS,
     :           VNORM,
     :           BALANCES,
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
*     ECHOMOP - ECH_EXTR_DECOSOPT

*  Purpose:
*     Extracts optimally and also rejects bad pixels.

*  Description:
*     This routine rejects 'bad' pixels from the increment and
*     adjusts the variance weighting scheme accordingly.

*  Invocation:
*     CALL ECH_EXTR_DECOSOPT(
*     :    NX,
*     :    NY,
*     :    GOT_QUALITY,
*     :    GOT_AN_ARC,
*     :    GOT_BALANCE,
*     :    NO_ERRORS,
*     :    FULL_SKYFIT,
*     :    OBJ_MASK,
*     :    OBJ_NPOLY,
*     :    REJECT_THRESH,
*     :    IWAVE,
*     :    Y_CENTER,
*     :    VAR0,
*     :    PHOTON,
*     :    CMIN,
*     :    MAX_SKY_PIXELS,
*     :    IMAGE,
*     :    IMAGE_ERR,
*     :    QUALITY,
*     :    SKY_MODEL_ERR,
*     :    FLAT_MODEL_ERR,
*     :    VARMAT,
*     :    NPIX,
*     :    IMGDATA,
*     :    IMGSUM,
*     :    ERRORS,
*     :    VNORM,
*     :    BALANCES,
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
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     NPIX = INTEGER (Given)
*        Number of values in increment.
*     GOT_QUALITY = LOGICAL (Given)
*        TRUE if quality data is available.
*     GOT_AN_ARC = LOGICAL (Given)
*        TRUE if arc frame is to be extracted too.
*     GOT_BALANCE = LOGICAL (Given)
*        TRUE if balance factors are available.
*     NO_ERRORS = LOGICAL (Given)
*        TRUE if no error array available.
*     FULL_SKYFIT = LOGICAL (Given)
*        TRUE if sky model effect on variance accounted for.
*     OBJ_MASK = INTEGER (Given)
*        Status of each pixel across profile.
*     OBJ_NPOLY = INTEGER (Given)
*        Degree of polynomial used to model object profiles.
*     IWAVE = INTEGER (Given)
*        Index of pixel in wavelength direction.
*     Y_CENTER = REAL (Given)
*        Y coordinate of order trace center.
*     VAR0 = REAL (Given)
*        Readout noise level in counts squared.
*     PHOTON = REAL (Given)
*        Photon-to-ADU conversion factor.
*     CMIN = REAL (Given)
*        Variance floor.
*     REJECT_THRESH = REAL (Given)
*        Reject threshold for CR detection.
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum extent of sky and therefore dekker.
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     IMAGE_ERR = REAL (Given)
*        Input errors frame image of dimensions nx columns and ny rows.
*     QUALITY = BYTE (Given)
*        Input quality frame image of dimensions nx columns and ny rows.
*     VARMAT = REAL (Given)
*        Covariance matrix for sky model.
*     SKY_MODEL_ERR = REAL (Given)
*        Modeled sky errors at offsets from trace.
*     FLAT_MODEL_ERR = REAL (Given)
*        Balance factor errors.
*     ARC_FRAME = REAL (Given)
*        Input arc frame image of dimensions nx columns and ny rows.
*     IMGDATA = REAL (Returned)
*        Calculated object pixel intensities.
*     IMGSUM = REAL (Returned)
*        Calculated object  intensity at increment.
*     ERRORS = REAL (Returned)
*        Calculated object pixel errors.
*     VNORM = REAL (Returned)
*        Minimum variance per pixel.
*     BALANCES = REAL (Returned)
*        Balance factors for increment.
*     PROFILE = REAL (Returned)
*        Profile probabilities for increment.
*     NRAYS = INTEGER (Returned)
*        Running bad pixel total.
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
*      Initialise control and counter
*      Initialise variables which will be used to correct
*         for the correlation between the spectrum point and data point (01/07/88)
*      Loop thru extracted values
*         If pixel in within frame boundaries then
*            If an object pixel and value is positive then
*               Initialise weight to unity and add to variance estimate
*            Endif
*         Endif
*      End loop
*      Sum covariance
*      Loop until all rejects have been treated
*         Update counter and initialise bad-pixel records
*         Loop thru profile
*            Compute revised variance estimate using profile model
*            If pixel ok so far then
*               If inside frame boundaries then
*                  If object pixel and positive valued then
*                     If bad quality pixel then
*                        If highest valued bad pixel this increment remember it
*                     Endif
*                     Avoid crashes in zero photon case by pretending that there is one photon.
*                     Evaluate probability of chance correlation
*                  Endif
*               Endif
*            Endif
*         End loop
*         Reject worst outlier (changed to use highest intensity ray already found)
*         Load ordinary optimal weights when sky fit uncertainty insignificant.
*         Sum object (and arc) increment values using optimal weights
*         Stow optimal spectrum and its standard deviation
*      End loop
*      Update running total of bad pixels

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     04-JUL-1996 (MJC):
*       Fix for the 'jump' due to incorrect boundary handling.
*     10-SEP-1996 (MJC):
*       Removed code invariant along an order to ECH_EXT_OPT for speed.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      REAL IMAGE( NX, NY )
      INTEGER MAX_SKY_PIXELS
      INTEGER OBJ_MASK( -MAX_SKY_PIXELS/2 : MAX_SKY_PIXELS/2 )
      INTEGER OBJ_BELOW
      INTEGER OBJ_ABOVE
      INTEGER OBJ_NPOLY
      REAL Y_CENTER
      INTEGER IWAVE
      REAL REJECT_THRESH
      REAL VAR0
      REAL PHOTON
      REAL CMIN
      LOGICAL GOT_QUALITY
      LOGICAL GOT_AN_ARC
      LOGICAL GOT_BALANCE
      LOGICAL NO_ERRORS
      LOGICAL FULL_SKYFIT
      REAL ARCPIX( MAX_SLICE_PIXELS )
      REAL IMAGE_ERR( NX, NY )
      BYTE QUALITY( NX, NY )
      REAL FLAT_MODEL_ERR( NX, -MAX_SKY_PIXELS/2 : MAX_SKY_PIXELS/2 )

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
      REAL BALANCES( MAX_SLICE_PIXELS )
      REAL SKY_MODEL_ERR( NX, -MAX_SKY_PIXELS/2 : MAX_SKY_PIXELS/2 )

*  Status:
      INTEGER STATUS

*  Local Constants:
      REAL EPS
      PARAMETER ( EPS = 0.02 )

*  Local variables:
      REAL WEIGHT( MAX_SLICE_PIXELS )
      REAL VAR
      REAL VOLD
      REAL RATIO
      REAL WSUM
      REAL INPUT_VAR
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
      REAL BALANCE
      REAL VT
      REAL WORST_RAY
      REAL FRACTION
      REAL YFRAC

      INTEGER I
      INTEGER J
      INTEGER IOFF
      INTEGER IOFFST
      INTEGER IOFFEN
      INTEGER YCOORD
      INTEGER NPIX
      INTEGER IFAIL
      INTEGER WORST_AT
      INTEGER IREJ
      INTEGER MAXREJ
      INTEGER NREJT
      INTEGER ICYCLE

      LOGICAL PIXEL_OK

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      REAL CHANCE
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Initialise control and counter.
      RATLO = - REJECT_THRESH
      RATHI = REJECT_THRESH
      IREJ = 1
      MAXREJ = 6
      NREJT = 0
      ICYCLE = 0
      WVSUM = 0.0

*  Find fraction of a pixel part of trace.
      YFRAC = Y_CENTER - FLOAT( INT( Y_CENTER + 0.5 ) )

*  Initialise variables which will be used to correct for the
*  correlation between the spectrum point and data point (01/07/88).
      I = 0
      YCOORD = INT( Y_CENTER + 0.5 ) + OBJ_BELOW - 1
      IF ( YFRAC .GT. 0.0 ) THEN
         IOFFEN = OBJ_ABOVE
         IOFFST = OBJ_BELOW - 1

      ELSE
         IOFFST = OBJ_BELOW
         IOFFEN = OBJ_ABOVE + 1
         YCOORD = YCOORD + 1
      END IF
      IF ( YCOORD .LT. 1 ) THEN
         IOFFST = IOFFST - YCOORD + 1
         YCOORD = 1
      END IF
      IF ( YCOORD + IOFFEN - IOFFST .GT. NY ) THEN
         IOFFEN = IOFFST + NY - YCOORD
      END IF
      DO IOFF = IOFFST, IOFFEN
         FRACTION = 1.0
         IF ( YFRAC .GT. 0.0 ) THEN
            IF ( IOFF .EQ. OBJ_BELOW - 1 ) THEN
               FRACTION = YFRAC

            ELSE IF ( IOFF .EQ. OBJ_ABOVE ) THEN
               FRACTION = 1.0 - YFRAC
            END IF

         ELSE
            IF ( IOFF .EQ. OBJ_ABOVE + 1 ) THEN
               FRACTION = -YFRAC

            ELSE IF ( IOFF .EQ. OBJ_BELOW ) THEN
               FRACTION = 1.0 + YFRAC
            END IF
         END IF

         I = I + 1
         PESTIMATE = PROFILE( I )

*     If an object pixel and value is not BAD then.
         IF ( ( OBJ_MASK( IOFF ) .EQ. 1 .OR.
     :        IOFF .EQ. OBJ_BELOW - 1 .OR.
     :        IOFF .EQ. OBJ_ABOVE + 1 ) .AND.
     :        ( GOT_QUALITY .AND. ( QUALITY( IWAVE, YCOORD ) .EQ. 0 )
     :        .OR. .NOT. GOT_QUALITY ) .AND.
     :        IMAGE( IWAVE, YCOORD ) .NE. ECH__BAD_REAL ) THEN

*        Initialise weight to unity and add to variance estimate.
            WEIGHT( I ) = 1.0
            SKYS = SKY_MODEL_ERR( IWAVE, IOFF )
            BALANCE = BALANCES( I )
            STAR = SPEC( IWAVE ) / BALANCE * PESTIMATE
            STAR = STAR * FRACTION
            IF ( NO_ERRORS ) THEN
               INPUT_VAR = 0.0

            ELSE
               INPUT_VAR = IMAGE_ERR( IWAVE, YCOORD ) ** 2.0
            END IF
            WVSUM = WVSUM + BALANCE *
     :              ( BALANCE * ( VAR0 + INPUT_VAR ) +
     :              ( ABS( STAR ) + SKYS ) / PHOTON )
         END IF
         YCOORD = YCOORD + 1
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
         IREJ = 0
         WORST_RAY = 0.0
         WORST_AT = 0
         RMIN = 2.0
         RMAX = 10.0

*     Loop through profile.
         I = 0
         YCOORD = INT( Y_CENTER + 0.5 ) + OBJ_BELOW - 2
         IF ( YFRAC .GT. 0.0 ) THEN
            IOFFEN = OBJ_ABOVE
            IOFFST = OBJ_BELOW - 1

         ELSE
            IOFFST = OBJ_BELOW
            IOFFEN = OBJ_ABOVE + 1
            YCOORD = YCOORD + 1
         END IF
         IF ( YCOORD .LT. 1 ) THEN
            IOFFST = IOFFST - YCOORD + 1
            YCOORD = 1
         END IF
         IF ( YCOORD + IOFFEN - IOFFST .GT. NY ) THEN
            IOFFEN = IOFFST + NY - YCOORD
         END IF
         DO IOFF = IOFFST, IOFFEN
            I = I + 1

*        Compute revised variance estimate using profile model
*        Skip rejected pixels, find worst pixel. At least one
*        extra run is forced to take advantage of the improved
*        estimate of the spectrum, even if no pixel is rejected.
            PESTIMATE = PROFILE( I )

*        If pixel ok so far then.
            IF ( ERRORS( I ) .GT. 0.0 ) THEN
               FRACTION = 1.0
               IF ( YFRAC .GT. 0.0 ) THEN
                  IF ( IOFF .EQ. OBJ_BELOW - 1 ) THEN
                     FRACTION = YFRAC

                  ELSE IF ( IOFF .EQ. OBJ_ABOVE ) THEN
                     FRACTION = 1.0 - YFRAC
                  END IF

               ELSE
                  IF ( IOFF .EQ. OBJ_ABOVE + 1 ) THEN
                     FRACTION = -YFRAC

                  ELSE IF ( IOFF .EQ. OBJ_BELOW ) THEN
                     FRACTION = 1.0 + YFRAC
                  END IF
               END IF


*           If object pixel and not BAD then.
               IF ( ( OBJ_MASK( IOFF ) .EQ. 1 .OR.
     :              IOFF .EQ. OBJ_BELOW - 1 .OR.
     :              IOFF .EQ. OBJ_ABOVE + 1 ) .AND.
     :              IMAGE( IWAVE, YCOORD ) .NE. ECH__BAD_REAL ) THEN

*              If bad quality pixel then.
                  PIXEL_OK = .TRUE.
                  IF ( GOT_QUALITY ) THEN
                     IF ( QUALITY( IWAVE, YCOORD ) .GT. 0 )
     :                  PIXEL_OK = .FALSE.
                  ENDIF
                  IF ( .NOT. PIXEL_OK ) THEN

*                 If highest-valued bad pixel this increment
*                 remember it.
                     IF ( IMAGE( IWAVE, YCOORD ) .GT.
     :                    WORST_RAY ) THEN
                        WORST_RAY = IMAGE( IWAVE, YCOORD )
                        WORST_AT = I
                     END IF
                  END IF

*              Avoid crashes in zero-photon case by pretending that
*              there is one photon.  This will be a bad approximation
*              if there are very many such instances since then the
*              chance of only one photon is small.  However, this
*              should be rare.
                  BALANCE = BALANCES( I )
                  SKYS = SKY_MODEL_ERR( IWAVE, IOFF )
                  STAR = SPEC( IWAVE ) / BALANCE * PESTIMATE
                  STAR = STAR * FRACTION
                  OLD_STAR = STAR
                  STAR = MAX( PESTIMATE / PHOTON, STAR )
                  VOLD = ERRORS( I ) ** 2
                  IF ( NO_ERRORS ) THEN
                     INPUT_VAR = 0.0

                  ELSE
                     INPUT_VAR = IMAGE_ERR( IWAVE, YCOORD ) ** 2.0
                  END IF
                  VAR = BALANCE * ( BALANCE * (
     :                  VAR0 + INPUT_VAR ) +
     :                  MAX( CMIN + ABS( STAR ),
     :                  ABS( STAR ) + SKYS ) / PHOTON )

                  IF ( GOT_BALANCE ) THEN
                     IF ( FLAT_MODEL_ERR( IWAVE, IOFF )
     :                    .GT. 0.0 ) THEN
                        VAR = VAR + (
     :                        FLAT_MODEL_ERR( IWAVE, IOFF ) *
     :                        IMAGE( IWAVE, YCOORD ) *
     :                        FLAT_MODEL_ERR( IWAVE, IOFF ) *
     :                        IMAGE( IWAVE, YCOORD ) )
                     END IF
                  END IF
                  ERRORS( I ) = MAX( 1.0, SQRT( ABS( VAR ) ) )

*              A correction is made before calling CHANCE to account
*              for correlation between the estimated spectrum and the
*              data value.  This increases the chance of rejection on
*              data points with high weights which was too low before.
*              Approximate decrease in var by (1-f*f/sum of f*f)
*              where f is the profile factor.  (01/07/88 TRM).
                  VAR = VOLD
                  IF ( WSUM .GT. 0.0 )
     :               VAR = VAR * ( 1.0 - 2.0 *
     :                     WEIGHT( I ) * PESTIMATE / WSUM ) +
     :                     ( WVSUM + SUMD2 * VNORM ) *
     :                     ( PESTIMATE / WSUM ) ** 2
                  VAR = SQRT( ABS( VAR ) )
                  VT = MAX( VAR, EPS * OLD_STAR )

*              Evaluate probability of chance correlation.
                  RATIO = CHANCE( IMGDATA( I ), OLD_STAR, VT,
     :                            RATLO, RATHI )
                  IF ( RATIO .GT. -0.5 ) THEN
                     IF ( RATIO .GT. RMAX ) THEN
                        RMAX = RATIO
                        RMIN = -1.0
                         IREJ = I

                     ELSE IF ( RATIO .LT. RMIN ) THEN
                        RMIN = RATIO
                        IREJ = I
                     END IF
                  END IF
               END IF
            END IF
            YCOORD = YCOORD + 1
         END DO

*     Reject worst outlier (changed to use highest-intensity ray
*     already found).
         IF ( WORST_AT .GT. 0 ) THEN
            ERRORS( WORST_AT ) = -ABS( ERRORS( WORST_AT ) )
            NREJT = NREJT + 1
            WORST_RAY = 0.0
            WORST_AT = 0
         END IF

*     Load ordinary optimal weights when sky-fit uncertainty insignificant.
*     These will be changed if full skyfit is true.
         DO I = 1, NPIX
            IF ( ERRORS( I ) .GT. 0. ) THEN
               WEIGHT( I ) = PROFILE( I ) / ERRORS( I ) ** 2
            END IF
         END DO

*     If full_skyfit is needed, modify weights to include uncertainty in
*     sky fit optimised variances.
         IF ( FULL_SKYFIT ) THEN
            CALL ECH_SKYFIT_WEIGHTS( NPIX, MAX_SKY_PIXELS, VNORM,
     :           .FALSE., VARMAT, IMGDATA, ERRORS, WEIGHT, PROFILE,
     :           IFAIL )
         END IF

*     Sum object (and arc) increment values using optimal weights.
         SUM = 0.0
         ARCSUM = 0.0
         WSUM = 0.0
         SUMD2 = 0.0
         WVSUM = 0.0
         DO I = 1, NPIX
            PESTIMATE = PROFILE( I )
            IF ( ERRORS( I ) .GT. 0. ) THEN
               SUM = SUM + WEIGHT( I ) * IMGDATA( I )
               ARCSUM = ARCSUM + WEIGHT( I ) * ARCPIX( I )
               WSUM = WSUM + WEIGHT( I ) * PESTIMATE
               WVSUM = WVSUM + ( WEIGHT( I ) * ERRORS ( I ) ) ** 2
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
         IF ( WSUM .GT. 0.0 ) THEN
            SPEC( IWAVE ) = SUM / WSUM
            SPECSIG( IWAVE ) = ABS( WVSUM + SUMD2 * VNORM ) /
     :            ( WSUM * WSUM )
            IF ( GOT_AN_ARC ) THEN
               ARCSPEC( IWAVE ) = ARCSUM / WSUM
               ARCSIG( IWAVE )  = ARCSUM / WSUM
            END IF
         END IF
      END DO

*  Update running total of bad pixels.
      NRAYS = NRAYS + NREJT

      END
