      SUBROUTINE ECH_EXTR_2DINCPROF(
     :           NX,
     :           GOT_QUALITY,
     :           GOT_AN_ARC,
     :           OBJ_MASK,
     :           OBJ_NPOLY,
     :           MAXIMUM_POLY,
     :           IWAVE,
     :           READOUT,
     :           PHOTON,
     :           MAX_SKY_PIXELS,
     :           PFL_SUBSAMPLES,
     :           IMAGE,
     :           IMAGE_ERR,
     :           QUALITY,
     :           SKY_MODEL,
     :           SKY_MODEL_ERR,
     :           ARC_FRAME,
     :           SUBSTEPS,
     :           MODELED_PROFILE,
     :           POLY_PROFILES,
     :           IMGDATA,
     :           IMGSUM,
     :           ERRORS,
     :           VNORM,
     :           PROFILE,
     :           ARCPIX,
     :           ARCSUM,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_EXTR_2DINCPROF

*  Purpose:
*     Calculate profile weights at x increment.

*  Description:
*     This routine extracts the profile for a single increment in wavelength
*     along with raw intensities, errors, arc intensity.
*     This routine assumes that the spectrum has an associated error array
*     which already contains appropriate errors corresponding to the
*     balance factors involved.  This routine will usually be used by
*     to extract a pre-2d-scrunched array

*  Invocation:
*     CALL ECH_EXTR_2DINCPROF(
*     :    NX,
*     :    GOT_QUALITY,
*     :    GOT_AN_ARC,
*     :    OBJ_MASK,
*     :    OBJ_NPOLY,
*     :    MAXIMUM_POLY,
*     :    IWAVE,
*     :    READOUT,
*     :    PHOTON,
*     :    MAX_SKY_PIXELS,
*     :    PFL_SUBSAMPLES,
*     :    IMAGE,
*     :    IMAGE_ERR,
*     :    QUALITY,
*     :    SKY_MODEL,
*     :    SKY_MODEL_ERR,
*     :    ARC_FRAME,
*     :    SUBSTEPS,
*     :    MODELED_PROFILE,
*     :    POLY_PROFILES,
*     :    IMGDATA,
*     :    IMGSUM,
*     :    ERRORS,
*     :    VNORM,
*     :    PROFILE,
*     :    ARCPIX,
*     :    ARCSUM,
*     :    STATUS
*     :   )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     GOT_QUALITY = LOGICAL (Given)
*        TRUE if quality data is available.
*     GOT_AN_ARC = LOGICAL (Given)
*        TRUE if arc frame is to be extracted too.
*     OBJ_MASK = INTEGER (Given)
*        Status of each pixel across profile.
*     OBJ_NPOLY = INTEGER (Given)
*        Degree of polynomial used to model object profiles.
*     IWAVE = INTEGER (Given)
*        Index of pixel in wavelength direction.
*     READOUT = REAL (Given)
*        Readout noise level in counts.
*     PHOTON = REAL (Given)
*        Photon to ADU conversion factor.
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum extent of sky and therefore dekker.
*     PFL_SUBSAMPLES = INTEGER (Given)
*        Number of subsamples across dekker.
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and max_sky_pixels rows.
*     IMAGE_ERR = REAL (Given)
*        Input image errors of dimensions nx columns and max_sky_pixels rows.
*     QUALITY = BYTE (Given)
*        Input image quality of dimensions nx columns and max_sky_pixels rows.
*     SKY_MODEL = REAL (Given)
*        Modeled sky intensities at offsets from trace.
*     SKY_MODEL_ERR = REAL (Given)
*        Modeled sky errors at offsets from trace.
*     ARC_FRAME = REAL (Given)
*        Input ARC image of dimensions nx columns and max_sky_pixels rows.
*     SUBSTEPS = INTEGER (Given)
*        Number of substeps per pixel for subsampling.
*     MODELED_PROFILE = REAL (Returned)
*        Model profile formed using global averaging.
*     POLY_PROFILE = REAL (Returned)
*        Model profile formed using polynomials.
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
*     ARCPIX = REAL (Returned)
*        Calculated arc intensities at increment.
*     ARCSUM = REAL (Returned)
*        Calculated arc intensity at increment.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of fit coefficients allowed.
*     POLY_PROFILES = DOUBLE (Given and Returned)
*        Polynomial coefficients for spatially subsampled x-varying profiles.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Calculate minimum variance floor
*     Determine object pixel offset limits
*     Loop thru object pixels
*         Calculate y coordinate of nearest pixel in image frame
*         If using a polynomial model of the profile then
*            Evaluate profile polynomial at this x-coordinate and offset
*         Else
*            Calculate index into subsampled profile model array
*         Endif
*         Ensure profile probability positive, and save it
*         Check pixel status (quality)
*           If pixel OK and flagged as 'object' then
*              Calculate sky, object and variance for pixel
*              Save intensities and variance (possibly for arc too)
*        Set variance negative to indicate unusable
*        Endif
*     End loop
*     Renormalize profile to unit sum and sum intensities across profile
*     Calculate average sky intensity and variance on estimate

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
      INTEGER OBJ_NPOLY
      INTEGER MAX_SKY_PIXELS
      INTEGER MAXIMUM_POLY
      INTEGER PFL_SUBSAMPLES
      INTEGER IWAVE
      REAL READOUT
      REAL PHOTON
      LOGICAL GOT_QUALITY
      LOGICAL GOT_AN_ARC
      REAL ARCPIX( MAX_SLICE_PIXELS )
      REAL IMAGE( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL ARC_FRAME( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL IMAGE_ERR( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      BYTE QUALITY( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )

*  Arguments Returned:
      INTEGER SUBSTEPS
      DOUBLE PRECISION POLY_PROFILES( MAXIMUM_POLY, -PFL_SUBSAMPLES / 2:
     :      PFL_SUBSAMPLES / 2 )
      REAL MODELED_PROFILE( -PFL_SUBSAMPLES / 2 : PFL_SUBSAMPLES / 2 )
      REAL IMGDATA( MAX_SLICE_PIXELS )
      REAL ARCSUM
      REAL IMGSUM
      REAL VNORM
      REAL PROFILE( MAX_SLICE_PIXELS )
      REAL ERRORS( MAX_SLICE_PIXELS )
      REAL SKY_MODEL( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL SKY_MODEL_ERR( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      INTEGER OBJ_MASK( -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL DAT
      REAL STAR
      REAL SKY_ESTIMATE
      REAL RCOORD
      REAL PSUM
      REAL PFL
      REAL SKY_AVERAGE
      REAL VAR
      REAL VAR0
      REAL CMIN

      INTEGER I
      INTEGER SKY_COUNT
      INTEGER IPROFILE
      INTEGER PINDEX
      INTEGER IPIX
      INTEGER YCOORD
      INTEGER OBJ_BELOW
      INTEGER OBJ_ABOVE

      LOGICAL PIXEL_OK

*    Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled
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

*  Evaluate profile model and force profile to be positive.
      PSUM = 0.0
      SKY_COUNT = 0
      SKY_AVERAGE = 0.0

*  Loop through object pixels.
      DO IPROFILE = OBJ_BELOW, OBJ_ABOVE

*     Calculate y coordinate of nearest pixel in image frame.
         YCOORD = IPROFILE
         IPIX = IPROFILE - OBJ_BELOW + 1

         RCOORD = FLOAT( YCOORD )
         IF ( RCOORD .GE. 0 ) THEN
            PINDEX = INT( RCOORD * FLOAT( SUBSTEPS )  )

         ELSE
            PINDEX = - INT( ABS( RCOORD ) * FLOAT( SUBSTEPS ) )
         END IF

*     If using a polynomial model of the profile then
         IF ( OBJ_NPOLY .GT. 0 ) THEN

*        Evaluate profile polynomial at this x-coordinate and offset
            CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :           POLY_PROFILES( 1, PINDEX ),
     :           1, FLOAT( IWAVE ), PFL, STATUS )

*     Else Calculate index into subsampled profile model array.
         ELSE
            PFL = MODELED_PROFILE( PINDEX )
         END IF

*     Ensure profile probability positive, and save it.
         PFL = MAX( 0.0, PFL )
         PSUM = PSUM + PFL
         PROFILE( IPIX ) = PFL

*     Check pixel quality.
         PIXEL_OK = .TRUE.
         IF ( GOT_QUALITY ) THEN
            IF ( QUALITY( IWAVE, YCOORD ) .GT. 0 ) THEN
               PIXEL_OK = .FALSE.
            END IF
         END IF

*     Check for BAD value pixel.
         IF ( IMAGE( IWAVE, YCOORD ) .EQ. ECH__BAD_REAL )
     :      PIXEL_OK = .FALSE.

*     If pixel OK and flagged as 'object' then.
         IF ( OBJ_MASK( IPROFILE ) .EQ. 1 .AND. PIXEL_OK ) THEN

*        Calculate sky, object and variance for pixel.
            SKY_COUNT = SKY_COUNT + 1
            SKY_ESTIMATE = SKY_MODEL( IWAVE, IPROFILE )
            SKY_AVERAGE = SKY_AVERAGE + SKY_ESTIMATE
            DAT = IMAGE( IWAVE, YCOORD )
            STAR = DAT - SKY_ESTIMATE
            VAR = VAR0 + IMAGE_ERR( IWAVE, YCOORD ) +
     :            MAX( CMIN, ABS( DAT ), ABS( SKY_ESTIMATE ) ) /
     :            PHOTON

*        Save intensities and variance (possibly for arc too).
            IMGDATA( IPIX ) = STAR
            IF ( GOT_AN_ARC ) THEN
               IF ( ARC_FRAME( IWAVE, YCOORD ) .NE.
     :              ECH__BAD_REAL ) THEN
                  ARCPIX( IPIX ) = ARC_FRAME( IWAVE, YCOORD )

               ELSE
                  ARCPIX( IPIX ) = 0.0
               END IF
            END IF
            ERRORS( IPIX ) = SQRT( ABS( VAR ) )

*     Set variance negative to indicate unusable.
         ELSE
            ERRORS( IPIX ) = -1
         END IF
      END DO

*  Renormalize profile to unit sum and sum intensities across profile.
      IMGSUM = 0.0
      IF ( GOT_AN_ARC )  ARCSUM = 0.0
      DO I = 1, IPIX
         PROFILE( I ) = PROFILE( I ) / PSUM
         IMGSUM = IMGSUM + IMGDATA( I )
         IF ( GOT_AN_ARC ) ARCSUM = ARCSUM + ARCPIX( I )
      END DO

*  Calculate average sky intensity and variance on estimate.
      IF ( SKY_COUNT .GT. 0 )
     :   SKY_AVERAGE = SKY_AVERAGE / FLOAT( SKY_COUNT )

      VNORM = VAR0 + ABS( SKY_AVERAGE )  / PHOTON

      END
