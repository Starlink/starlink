      SUBROUTINE ECH_EXTR_INCPROF(
     :           NX,
     :           NY,
     :           GOT_QUALITY,
     :           GOT_AN_ARC,
     :           GOT_BALANCE,
     :           NO_ERRORS,
     :           OBJ_MASK,
     :           OBJ_BELOW,
     :           OBJ_ABOVE,
     :           OBJ_NPOLY,
     :           MAXIMUM_POLY,
     :           IWAVE,
     :           Y_CENTER,
     :           VAR0,
     :           PHOTON,
     :           CMIN,
     :           MAX_SKY_PIXELS,
     :           PFL_SUBSAMPLES,
     :           IMAGE,
     :           IMAGE_ERR,
     :           QUALITY,
     :           SKY_MODEL,
     :           SKY_MODEL_ERR,
     :           FLAT_MODEL,
     :           FLAT_MODEL_ERR,
     :           ARC_FRAME,
     :           SUBSTEPS,
     :           MODELED_PROFILE,
     :           POLY_PROFILES,
     :           IPIX,
     :           IMGDATA,
     :           IMGSUM,
     :           ERRORS,
     :           VNORM,
     :           BALANCES,
     :           PROFILE,
     :           ARCPIX,
     :           ARCSUM,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_EXTR_INCPROF

*  Purpose:
*     Extracts an order using profile weighting.

*  Description:
*     This routine extracts the profile for a single increment in wavelength
*     along with raw intensities, errors, balances factors and arc intensity.

*  Invocation:
*     CALL ECH_EXTR_INCPROF(
*    :     NX,
*    :     NY,
*    :     GOT_QUALITY,
*    :     GOT_AN_ARC,
*    :     GOT_BALANCE,
*    :     NO_ERRORS,
*    :     OBJ_MASK,
*    :     OBJ_BELOW,
*    :     OBJ_ABOVE,
*    :     OBJ_NPOLY,
*    :     MAXIMUM_POLY,
*    :     IWAVE,
*    :     Y_CENTER,
*    :     VAR0,
*    :     PHOTON,
*    :     CMIN,
*    :     MAX_SKY_PIXELS,
*    :     PFL_SUBSAMPLES,
*    :     IMAGE,
*    :     IMAGE_ERR,
*    :     QUALITY,
*    :     SKY_MODEL,
*    :     SKY_MODEL_ERR,
*    :     FLAT_MODEL,
*    :     FLAT_MODEL_ERR,
*    :     ARC_FRAME,
*    :     SUBSTEPS,
*    :     MODELED_PROFILE,
*    :     POLY_PROFILES,
*    :     IPIX,
*    :     IMGDATA,
*    :     IMGSUM,
*    :     ERRORS,
*    :     VNORM,
*    :     BALANCES,
*    :     PROFILE,
*    :     ARCPIX,
*    :     ARCSUM,
*    :     STATUS
*    :    )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     GOT_QUALITY = LOGICAL (Given)
*        TRUE if quality data is available.
*     GOT_AN_ARC = LOGICAL (Given)
*        TRUE if arc frame is to be extracted too.
*     GOT_BALANCE = LOGICAL (Given)
*        TRUE if balance factors are available.
*     NO_ERRORS = LOGICAL (Given)
*        TRUE if no error array available.
*     OBJ_MASK = INTEGER (Given)
*        Status of each pixel across profile.
*     OBJ_BELOW = INTEGER (Given)
*        Offset of bottom edge of object from trace.
*     OBJ_ABOVE = INTEGER (Given)
*        Offset of top edge of object from trace.
*     OBJ_NPOLY = INTEGER (Given)
*        Degree of polynomial used to model object profiles.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum Degree of polynomial used.
*     IWAVE = INTEGER (Given)
*        Index of pixel in wavelength direction.
*     Y_CENTER = REAL (Given)
*        Y coordinate of order trace center.
*     VAR0 = REAL (Given)
*        Variance floor in counts squared.
*     PHOTON = REAL (Given)
*        Photon to ADU conversion factor.
*     CMIN = REAL (Given)
*        Minimum Variance floor.
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum extent of sky and therefore dekker.
*     PFL_SUBSAMPLES = INTEGER (Given)
*        Number of subsamples across dekker.
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     IMAGE_ERR = REAL (Given)
*        Input errors frame image of dimensions nx columns and ny rows.
*     QUALITY = BYTE (Given)
*        Input quality frame image of dimensions nx columns and ny rows.
*     SKY_MODEL = REAL (Given)
*        Modeled sky intensities at offsets from trace.
*     SKY_MODEL_ERR = REAL (Given)
*        Modeled sky errors at offsets from trace.
*     FLAT_MODEL = REAL (Given)
*        Balance factors at offsets from trace.
*     FLAT_MODEL_ERR = REAL (Given)
*        Balance factor errors.
*     ARC_FRAME = REAL (Given)
*        Input ARC frame image of dimensions nx columns and ny rows.
*     SUBSTEPS = INTEGER (Given)
*        Number of substeps per pixel for subsampling.
*     MODELED_PROFILE = REAL (Returned)
*        Model profile formed using global averaging.
*     POLY_PROFILE = REAL (Returned)
*        Model profile formed using polynomials.
*     IMGDATA = REAL (Returned)
*        Calculated object pixel intensities.
*     IMGSUM = REAL (Returned)
*        Calculated object intensity at increment.
*     ERRORS = REAL (Returned)
*        Calculated object pixel errors.
*     VNORM = REAL (Returned)
*        Minimum variance per pixel.
*     BALANCES = REAL (Returned)
*        Balance factors for increment.
*     PROFILE = REAL (Returned)
*        Profile probabilities for increment.
*     ARCPIX = REAL (Returned)
*        Calculated arc intensities at increment.
*     ARCSUM = REAL (Returned)
*        Calculated arc intensity at increment.
*     POLY_PROFILES = DOUBLE (Given and Returned)
*        Polynomial coefficients for spatially subsampled x-varying profiles.
*     IPIX = INTEGER (Given and Returned)
*        Number of active  pixels in increment.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Loop through object pixels
*         Calculate y coordinate of nearest pixel in image frame
*         If using a polynomial model of the profile then
*            Evaluate profile polynomial at this x-coordinate and offset
*         Else
*            Calculate index into subsampled profile model array
*         Endif
*         Ensure profile probability positive, and save it
*         If y-coordinate is inside frame boundary then
*            Check pixel status (quality)
*           If pixel OK and flagged as 'object' then
*              Ensure good balance factor is available
*              Calculate sky, object and variance for pixel
*              Modify variance estimate if actual balance factor available
*              Save intensities and variance (possibly for arc too)
*              Set variance negative to indicate unusable
*           Endif
*           Set variance negative to indicate unusable
*         Endif
*     End loop
*     Renormalize profile to unit sum and sum intensities across profile
*     Calculate average sky intensity and variance on estimate

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
      LOGICAL GOT_QUALITY
      LOGICAL GOT_AN_ARC
      LOGICAL GOT_BALANCE
      INTEGER MAX_SKY_PIXELS
      INTEGER OBJ_MASK( -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      INTEGER OBJ_BELOW
      INTEGER OBJ_ABOVE
      INTEGER OBJ_NPOLY
      INTEGER MAXIMUM_POLY
      REAL Y_CENTER
      INTEGER PFL_SUBSAMPLES
      INTEGER IWAVE
      REAL ARC_FRAME( NX, NY )
      REAL VAR0
      REAL PHOTON
      REAL CMIN
      LOGICAL NO_ERRORS
      REAL ARCPIX( MAX_SLICE_PIXELS )
      REAL IMAGE_ERR( NX, NY )
      BYTE QUALITY( NX, NY )
      REAL FLAT_MODEL( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL FLAT_MODEL_ERR( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS/2 )

*  Arguments Returned:
      INTEGER SUBSTEPS
      DOUBLE PRECISION POLY_PROFILES( MAXIMUM_POLY, -PFL_SUBSAMPLES / 2:
     :      PFL_SUBSAMPLES / 2  )
      REAL MODELED_PROFILE ( -PFL_SUBSAMPLES / 2 : PFL_SUBSAMPLES / 2 )
      REAL IMGDATA( MAX_SLICE_PIXELS )
      REAL ARCSUM
      REAL IMGSUM
      REAL VNORM
      REAL PROFILE( MAX_SLICE_PIXELS )
      REAL ERRORS( MAX_SLICE_PIXELS )
      REAL BALANCES( MAX_SLICE_PIXELS )
      REAL SKY_MODEL( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL SKY_MODEL_ERR( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL BALANCE
      REAL BALANCE_ERR
      REAL STAR
      REAL SKY_ESTIMATE
      REAL RCOORD
      REAL PSUM
      REAL PFL
      REAL SKY_AVERAGE
      REAL VAR
      REAL FRACTION
      REAL YFRAC
      REAL INPUT_VAR

      INTEGER I
      INTEGER SKY_COUNT
      INTEGER IOFF
      INTEGER IOFFST
      INTEGER IOFFEN
      INTEGER PINDEX
      INTEGER IPIX
      INTEGER YCOORD

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Calculate Y-coordinate of nearest pixel in image frame.
      YCOORD = INT( Y_CENTER + 0.5 ) + OBJ_BELOW - 1
      YFRAC = Y_CENTER - FLOAT( INT( Y_CENTER + 0.5 ) )
      IF ( YFRAC .GT. 0.0 ) THEN
         IOFFEN = OBJ_ABOVE
         IOFFST = OBJ_BELOW - 1

      ELSE
         IOFFEN = OBJ_ABOVE + 1
         IOFFST = OBJ_BELOW
         YCOORD = YCOORD + 1
      END IF
      IF ( YCOORD .LT. 1 ) THEN
         IOFFST = IOFFST - YCOORD + 1
         YCOORD = 1
      END IF
      IF ( YCOORD + IOFFEN - IOFFST .GT. NY ) THEN
         IOFFEN = IOFFST + NY - YCOORD
      END IF

*  Evaluate profile model and force profile to be positive.
      PSUM = 0.0
      SKY_COUNT = 0
      SKY_AVERAGE = 0.0
      IPIX = 0

*  Loop through object pixels.
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

         IPIX = IPIX + 1
         RCOORD = FLOAT( YCOORD ) - Y_CENTER
         IF ( RCOORD .GE. 0.0 ) THEN
            PINDEX = INT( RCOORD * FLOAT( SUBSTEPS ) )

         ELSE
            PINDEX = - INT( - RCOORD * FLOAT( SUBSTEPS )  )
         END IF

*     If using a polynomial model of the profile then.
         IF ( OBJ_NPOLY .GT. 0 ) THEN

*        Evaluate profile polynomial at this x-coordinate and offset.
            CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :           POLY_PROFILES( 1, PINDEX ), 1, FLOAT( IWAVE ), PFL,
     :           STATUS )

*     Calculate index into subsampled profile model array.
         ELSE
            PFL = MODELED_PROFILE( PINDEX )
         END IF

*     Ensure profile probability positive, and save it.
         PFL = MAX( 0.0, PFL ) * FRACTION
         PSUM = PSUM + PFL
         PROFILE( IPIX ) = PFL

*     If pixel flagged as 'object' process it.
         IF ( OBJ_MASK( IOFF ) .EQ. 1 .OR.
     :        IOFF .EQ. OBJ_BELOW - 1 .OR.
     :        IOFF .EQ. OBJ_ABOVE + 1 ) THEN

*        Check pixel quality.
            IF ( GOT_QUALITY ) THEN
               IF ( QUALITY( IWAVE, YCOORD ) .GT. 0 ) THEN
                  GO TO 500
               END IF
            END IF

*        Check for BAD value pixel.
            IF ( IMAGE( IWAVE, YCOORD ) .EQ. ECH__BAD_REAL )
     :         GO TO 500

*        Stow a spatial slice of the spectrum data.
*        Ensure good balance factor is available.
            IF ( GOT_BALANCE ) THEN
               BALANCE = FLAT_MODEL( IWAVE, IOFF )
               BALANCE_ERR = FLAT_MODEL_ERR( IWAVE, IOFF )

            ELSE
               BALANCE = 1.0
               BALANCE_ERR = 0.0
            END IF

*        Calculate sky, object and variance for pixel.
            SKY_COUNT = SKY_COUNT + 1
            SKY_ESTIMATE = SKY_MODEL( IWAVE, IOFF )
            SKY_AVERAGE = SKY_AVERAGE + SKY_ESTIMATE
            STAR = BALANCE * IMAGE( IWAVE, YCOORD ) - SKY_ESTIMATE
            IF ( NO_ERRORS ) THEN
               INPUT_VAR = 0.0

            ELSE
               INPUT_VAR = IMAGE_ERR( IWAVE, YCOORD ) ** 2.0
            END IF
            VAR = BALANCE * ( BALANCE * ( VAR0 + INPUT_VAR ) +
     :            MAX( CMIN + ABS( STAR ),
     :            ABS( STAR ) + SKY_MODEL_ERR( IWAVE,IOFF ) )
     :            / PHOTON )

*        Modify variance estimate if actual balance factor available.
            IF ( GOT_BALANCE .AND. BALANCE_ERR .GT. 0.0 ) THEN
               VAR = VAR + BALANCE_ERR * IMAGE( IWAVE, YCOORD ) *
     :               BALANCE_ERR * IMAGE( IWAVE, YCOORD )
            END IF

*        Save intensities and variance (possibly for arc too).
            IMGDATA( IPIX ) = STAR * FRACTION
            IF ( GOT_AN_ARC ) THEN
               IF ( ARC_FRAME( IWAVE, YCOORD ) .NE.
     :              ECH__BAD_REAL ) THEN
                  ARCPIX( IPIX ) = BALANCE *
     :                  ARC_FRAME( IWAVE, YCOORD ) * FRACTION

               ELSE
                  ARCPIX( IPIX ) = 0.0
               END IF
            END IF
            ERRORS( IPIX ) = MAX( 1.0, SQRT(
     :                       ABS( VAR * FRACTION ) ) )
            BALANCES( IPIX ) = BALANCE

*     Set variance negative to indicate unusable.
         ELSE
  500       ERRORS( IPIX ) = -1.0
            PROFILE( IPIX ) = 0.0
            IMGDATA( IPIX ) = 0.0
         END IF
         YCOORD = YCOORD + 1
      END DO

*  Renormalize profile to unit sum and sum intensities across profile.
      IMGSUM = 0.0
      IF ( GOT_AN_ARC ) ARCSUM = 0.0
      DO I = IPIX, 1, -1
         IF ( PSUM .GT. 0.0 ) PROFILE( I ) = PROFILE( I ) / PSUM
         IF ( IMGDATA( I ) .GT. 0.0 ) IMGSUM = IMGSUM + IMGDATA( I )
         IF ( GOT_AN_ARC ) ARCSUM = ARCSUM + ARCPIX( I )
      END DO

*  Calculate average sky intensity and variance on estimate.
      IF ( SKY_COUNT .GT. 0 )
     :   SKY_AVERAGE = SKY_AVERAGE / FLOAT( SKY_COUNT )
      VNORM = VAR0 + ABS( SKY_AVERAGE )  / PHOTON

      END
