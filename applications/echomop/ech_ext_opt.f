      SUBROUTINE ECH_EXT_OPT(
     :           IMAGE,
     :           QUALITY,
     :           IMAGE_ERR,
     :           NO_ARC,
     :           NO_FLAT,
     :           NO_ERRORS,
     :           ARC_FRAME,
     :           NX,
     :           NY,
     :           N_ORDERS,
     :           EXTRACTION_MODE,
     :           FULL_SKYFIT,
     :           ZAPRATS,
     :           MAX_SKY_PIXELS,
     :           SKY_MASK,
     :           OBJ_MASK,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           SKY_NPOLY,
     :           SKY_NREJ,
     :           SKY_RTHR,
     :           READOUT,
     :           PHOTON,
     :           OBJ_NPOLY,
     :           MAXIMUM_POLY,
     :           TRACE_POLYNOMIAL,
     :           PFL_SUBSAMPLES,
     :           MODELED_PROFILE,
     :           POLY_PROFILES,
     :           SPEC,
     :           SPECSIG,
     :           ARCSPEC,
     :           ARCSIG,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           SKY_MODEL,
     :           SKY_MODEL_ERR,
     :           VARMAT,
     :           FLAT_MODEL,
     :           FLAT_MODEL_ERR,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_EXT_OPT

*  Purpose:
*     Control progress of various types of extraction (1-D).

*  Description:
*     o This routine has been modified from EXT_OPT
*       written by T.R. Marsh, June 1988, which itself was modified for
*       FIGARO from Keith Horne's original EXTOPT in the program PAMELA.
*
*     Extracts a 1-D stellar spectrum from a 2-D
*     Different weights are chosen for each pixel in order to
*     optimize the S/N ratio in the 1-D spectrum.
*
*     The optimal extraction algorithm for CCD data is described
*     in Horne(1986) PASP 98, 609.  This version of EXTOPT includes
*     many changes for low count data with Poisson statistics. This
*     is dened to improve the rejection for low nal data.
*
*     The original profile estimation by polynomial fitting parallel to the
*     order has been replaced by a global subsampled averaging process.
*     The original algorithm is still available by setting obj_npoly >0
*     BUT should not be used unless the order is v. nearly parallel to
*     the pixel rows. The program has also been severly de-constructed
*     into modules and the sky modeling and flat-fielding totally
*     separated. In addition the calculation of variances has been altered
*     to account for any user-supplied image-error frame data on both
*     object and flat-field balance factor frames.
*
*     The "seeing" profile is assumed to vary smoothly with wavelength.
*     This program should therefore not be used to extract spectra
*     of extended objects, or to extract ccd spectra in which
*     charge transfer problems in the direction along the slit distort
*     the seeing profile.
*     The program constructs a smooth representation of the seeing profile,
*     normalized to unit integral over spatial dimensions, by fitting
*     a polynomial in wavelength to each column of normalized data.
*     This procedure takes into account the smooth wavelength dependence
*     of the seeing profile (due e.g. to focus, seeing, differential refraction,
*     etc.), as well as moderate distortions (tilt, curvature, s-distortion)
*     of the object spectrum relative to a row of pixels.
*
*     The noise is modelled by Poisson statistics plus an independent
*     background noise, assumed to be uniform over the picture.
*
*     The optimal 1-D spectrum estimate is obtained by a weighted
*     least squares fit of the seeing profile model to picture data
*     at each wavelength. As of July 1988 it is also possible to account
*     for uncertainty in the sky, assuming that this has been computed
*     from polynomial fits. This produces a small improvement when there
*     are few sky pixels compared to object pixels, and/or a high order
*     poly had to be used. Using this option (FULL_SKYFIT=.TRUE.) requires
*     a matrix inversion at every column, and can therefore be very slow.
*     Should only be used when desperate.
*
*     Balance corrections are applied to the data on a pixel-by-pixel basis.
*     Data should not be balanced before running this routine as then the
*     estimates of variance may be incorrect.
*
*     Cosmic ray hits and other detector defects near the spectrum
*     are eliminated by a ma-clipping algorithm,
*     or by approximate poisson probability in case of low count rate.

*  Invocation:
*     CALL ECH_EXT_OPT(
*    :     IMAGE,
*    :     QUALITY,
*    :     IMAGE_ERR,
*    :     NO_ARC,
*    :     NO_FLAT,
*    :     NO_ERRORS,
*    :     ARC_FRAME,
*    :     NX,
*    :     NY,
*    :     N_ORDERS,
*    :     EXTRACTION_MODE,
*    :     FULL_SKYFIT,
*    :     ZAPRATS,
*    :     MAX_SKY_PIXELS,
*    :     SKY_MASK,
*    :     OBJ_MASK,
*    :     DEK_BELOW,
*    :     DEK_ABOVE,
*    :     SKY_NPOLY,
*    :     SKY_NREJ,
*    :     SKY_RTHR,
*    :     READOUT,
*    :     PHOTON,
*    :     OBJ_NPOLY,
*    :     MAXIMUM_POLY,
*    :     TRACE_POLYNOMIAL,
*    :     PFL_SUBSAMPLES,
*    :     MODELED_PROFILE,
*    :     POLY_PROFILES,
*    :     SPEC,
*    :     SPECSIG,
*    :     ARCSPEC,
*    :     ARCSIG,
*    :     X_TRACE_COORD,
*    :     Y_TRACE_COORD,
*    :     SKY_MODEL,
*    :     SKY_MODEL_ERR,
*    :     VARMAT,
*    :     FLAT_MODEL,
*    :     FLAT_MODEL_ERR,
*    :     STATUS
*    :    )

*  Arguments:
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     QUALITY = LOGICAL (Given)
*        Input data frame quality flags array.
*     IMAGE_ERR = REAL (Given)
*        Input data frame variance array.
*     NO_ARC = LOGICAL (Given)
*        Set TRUE to indicate that no arc frame is available.
*     NO_FLAT = LOGICAL (Given)
*        Set TRUE if no flat field frame available.
*     NO_ERRORS = LOGICAL (Given)
*        TRUE if no error array available.
*     ARC_FRAME = REAL (Given)
*        Arc lamp data frame.
*     EXTRACTION_MODE = CHAR (Given)
*        Type of extraction algorithm to use (Simple,Profile,Optimal).
*     FULL_SKYFIT = LOGICAL (Given)
*        Set TRUE when monte-carlo sky simulation is required.
*     ZAPRATS = LOGICAL (Given)
*        Set TRUE when Horne cosmic ray location is enabled.
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum number of pixels in dekker (max extractable).
*     SKY_MASK = INTEGER (Given and Returned)
*        Mask flagging which pixels contribute to the sky.
*     OBJ_MASK = INTEGER (Given and Returned)
*        Mask flagging which pixels are extracted as part of the object.
*     DEK_BELOW = INTEGER (Given)
*        Extent of dekker below order trace.
*     DEK_ABOVE = INTEGER (Given)
*        Extent of dekker above order trace.
*     SKY_NPOLY = INTEGER (Given)
*        Maximum number of coeffs for sky spatial fit.
*     SKY_NREJ = INTEGER (Given)
*        Number of reject cycles to use during sky modelling.
*     SKY_RTHR = FLOAT (Given)
*        Reject threshold (sigma) to use during sky modelling.
*     READOUT = FLOAT (Given)
*        Readout noise (sigma counts).
*     PHOTON = FLOAT (Given)
*        Photon to ADUs conversion factor.
*     OBJ_NPOLY = INTEGER (Given)
*        Maximum number of coefficients to use in object profile x fits.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of fit coefficients allowed.
*     TRACE_POLYNOMIAL = DOUBLE (Given)
*        Coefficients of fit describing order path across frame.
*     PFL_SUBSAMPLES = INTEGER (Given)
*        Maximum number of profile subsamples (spatial) allowed.
*     MODELED_PROFILE = READ (Given and Returned)
*        Modelled profile (subsampled spatially).
*     POLY_PROFILES = DOUBLE (Given and Returned)
*        Polynomial coefficients for spatially subsampled x-varying profiles.
*     SPEC = REAL (Given and Returned)
*        Extracted spectrum.
*     SPECSIG = REAL (Given and Returned)
*        Extracted spectrum variances.
*     ARCSPEC = REAL (Given and Returned)
*        Extracted arc.
*     ARCSIG = REAL (Given and Returned)
*        Extracted arc variances.
*     X_TRACE_COORD = DOUBLE (Temporary Workspace)
*        X coords of order trace path.
*     Y_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Y coords of order trace path.
*     SKY_MODEL = REAL (Given and Returned)
*        Modelled sky intensities.
*     SKY_MODEL_ERR = REAL (Given and Returned)
*        Variances on modelled sky intensities.
*     VARMAT = REAL (Given and Returned)
*        Sky covariance matrix.
*     FLAT_MODEL = REAL (Given)
*        Balance factors at offsets from trace.
*     FLAT_MODEL_ERR = REAL (Given)
*        Balance factor errors.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Exit right away if the order is disabled
*     Intialise spectrum
*     Check balance model for naff values
*     Report characteristics of extraction selected
*     Estimate the spatial extent in pixels
*     Set appropriate subsampling rate
*     Calculate object limits above and below order trace
*     Abort if no pixels marked as object
*     If only a single object pixel and not 'simple' extraction, default to it
*     Check for profile weighted extraction selected
*     Warn user if X-profile modeling is active
*     Calculate coordinates of order trace across frame
*     If simple extraction required then do it
*     Else
*        Calculate sky co-variance matrix
*        Loop thru wavelength increments
*           Extract data at this increment and estimate variance
*           Initial estimate of the spectrum is a normal sum across the profile.
*           If profile weighted extraction then do it
*           Else
*              If Reject cosmic rays then do it
*              Else If not cosmic ray zapping, just give normal weighted estimate,
*                   with an extra loop to refine the variances.
*              Endif
*           Endif
*           Apply ADU to photon scaling to get counts in photons
*        End loop
*     Endif
*     Report on any bad pixels excluded from the extraction
*

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     12-JUN-1996 (MJC):
*       Tidier output messages.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER N_ORDERS
      LOGICAL ZAPRATS
      LOGICAL GOT_QUALITY
      LOGICAL GOT_AN_ARC
      LOGICAL GOT_BALANCE
      LOGICAL NO_ARC
      LOGICAL NO_FLAT
      LOGICAL NO_ERRORS
      INTEGER PFL_SUBSAMPLES
      INTEGER MAX_SKY_PIXELS
      INTEGER DEK_ABOVE
      INTEGER DEK_BELOW
      INTEGER SKY_NPOLY
      INTEGER OBJ_NPOLY
      INTEGER SKY_NREJ
      REAL PHOTON
      REAL SKY_RTHR
      REAL ARC_FRAME( NX,NY )
      REAL IMAGE( NX, NY )
      INTEGER MAXIMUM_POLY
      REAL IMAGE_ERR( NX, NY )
      BYTE QUALITY( NX, NY )
      REAL FLAT_MODEL( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL FLAT_MODEL_ERR( NX, -MAX_SKY_PIXELS/2 : MAX_SKY_PIXELS/2 )
      REAL READOUT
      DOUBLE PRECISION TRACE_POLYNOMIAL( MAXIMUM_POLY )
      LOGICAL FULL_SKYFIT
      CHARACTER*1 EXTRACTION_MODE

*  Arguments Returned:
      REAL MODELED_PROFILE( -PFL_SUBSAMPLES / 2 : PFL_SUBSAMPLES / 2 )
      REAL ARCSPEC( NX )
      REAL ARCSIG( NX )
      REAL SPEC( NX )
      REAL SPECSIG( NX )
      REAL SKY_MODEL( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL SKY_MODEL_ERR( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      DOUBLE PRECISION POLY_PROFILES( MAXIMUM_POLY, -PFL_SUBSAMPLES / 2:
     :                      PFL_SUBSAMPLES / 2 )
      INTEGER SKY_MASK( -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      INTEGER OBJ_MASK( -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL VARMAT( MAX_COVAR_SIZE )

*  Workspace:
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION SPACE( MAX_COVAR_SIZE )

      REAL IMGSUM
      REAL VNORM
      REAL REJECT_THRESH
      REAL SUM
      REAL ARCSUM
      REAL CPHOTON     ! Range-limited photon value.
      REAL VAR0        ! Variance floor.
      REAL CMIN

      INTEGER I
      INTEGER XSTART
      INTEGER XEND
      INTEGER ORDER_SIZE
      INTEGER NBAD
      INTEGER NPIX
      INTEGER IX
      INTEGER IW
      INTEGER IWAVE
      INTEGER IP
      INTEGER OBJ_ABOVE
      INTEGER OBJ_BELOW
      INTEGER NCHAR

      LOGICAL USE_QUALITY

      CHARACTER*32 REF_STR

*  Plotting arrays:
      REAL IMGDATA( MAX_SLICE_PIXELS )
      REAL PROFILE( MAX_SLICE_PIXELS )
      REAL ARCPIX( MAX_SLICE_PIXELS )
      REAL ERRORS( MAX_SLICE_PIXELS )
      REAL BALANCES( MAX_SLICE_PIXELS )

*  Polynomial fitting arrays used by LSQUAR:
      REAL Y_CENTER
      INTEGER OBJ_COUNT
      REAL ESTIMATE
      INTEGER SUBSTEPS
      LOGICAL SIMPLE
      LOGICAL JUST_PROFILE

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Exit right away if the order is disabled.
      IF ( TRACE_POLYNOMIAL( 1 ) .EQ. ECH__BAD_DOUBLE ) THEN
         CALL ECH_REPORT( 0, ' Order disabled: no extraction.' )
         GO TO 999
      END IF

*  Intialise spectrum to zero.
      CALL ECH_ZERO_REAL( NX, SPEC( 1 ) )
      CALL ECH_ZERO_REAL( NX, SPECSIG( 1 ) )

*  Although the following flags are hardcoded to TRUE, they are provided to
*  assist portability. If the environment is unable to supply quality
*  info, or if the balance model is not required etc. then simply setting
*  the requisite flag and providing dummy variables for the relevant
*  arrays  (eg quality() ) will allow the routines called by this
*  module to operate correctly.
      GOT_QUALITY = .TRUE.
      GOT_AN_ARC = .NOT. NO_ARC
      GOT_BALANCE = .NOT. NO_FLAT
      NBAD = 0

*  Check balance model for unuseable values.
      IF ( GOT_BALANCE ) THEN
         DO I = DEK_BELOW, DEK_ABOVE
            DO IX = NX, 1, -1
               IF ( FLAT_MODEL( IX, I ) .LE. 0.0 ) THEN
                  FLAT_MODEL( IX, I ) = 1.0
                  FLAT_MODEL_ERR( IX, I ) = 0.0
               END IF
            END DO
         END DO
      END IF

*  Report characteristics of extraction selected.
      IF ( PHOTON .LE. 0.0 ) THEN
         CALL ECH_REPORT( 0,
     :   ' Defaulting to ADU conversion factor of 1.')
         PHOTON = 1.0
      END IF
      SIMPLE = .FALSE.
      IF ( EXTRACTION_MODE .EQ. 'O' ) THEN
         CALL ECH_REPORT( 0, ' Using optimal extraction algorithm.' )

      ELSE IF ( EXTRACTION_MODE .EQ. 'P' ) THEN
         CALL ECH_REPORT( 0, ' Using profile-weighting algorithm.' )

      ELSE IF ( EXTRACTION_MODE .EQ. 'S' ) THEN
         CALL ECH_REPORT( 0, ' Using simple summed extraction.' )
         SIMPLE = .TRUE.

      ELSE
         CALL ECH_REPORT( 0,
     :        ' Unknown extraction mode: using simple sum.' )
         EXTRACTION_MODE = 'S'
         SIMPLE = .TRUE.
      END IF
      IF ( GOT_AN_ARC ) THEN
         CALL ECH_REPORT( 0,
     :   ' The Arc spectra will be extracted using the same method.' )
      END IF

*  Estimate the spatial extent in pixels.
*  Set appropriate subsampling rate.
      CALL ECH_CALC_PROFSAMP( NY, N_ORDERS, PFL_SUBSAMPLES, DEK_BELOW,
     :     DEK_ABOVE, ORDER_SIZE, SUBSTEPS, STATUS )

*  Calculate object limits above and below order trace.
      OBJ_BELOW = 999
      OBJ_ABOVE = -999
      OBJ_COUNT = 0
      DO I = DEK_BELOW + 1, DEK_ABOVE - 1
         IF ( OBJ_MASK( I ) .NE. 0 .AND. OBJ_BELOW .GT.I ) OBJ_BELOW = I
         IF ( OBJ_MASK( I ) .NE. 0 .AND. OBJ_ABOVE .LT.I ) OBJ_ABOVE = I
         IF ( OBJ_MASK( I ) .NE. 0 ) OBJ_COUNT = OBJ_COUNT + 1
      END DO

*  Abort if there are no pixels marked as object.
      IF ( OBJ_COUNT .EQ. 0 ) THEN
         CALL ECH_REPORT( 0,
     :        ' No pixels marked as Object in profile: no extraction.' )
         GO TO 999

*  If only a single object pixel and not 'simple' extraction, default to it.
      ELSE IF ( OBJ_COUNT .EQ. 1 .AND. EXTRACTION_MODE .NE. 'S' ) THEN
         CALL ECH_REPORT( 0,
     :        ' Single-pixel object: using simple extraction.' )
         EXTRACTION_MODE = 'S'
         SIMPLE = .TRUE.
      END IF
      CALL CHR_ITOC( OBJ_COUNT, REF_STR, NCHAR )
      REPORT_STRING = ' Extracting object over ' //
     :      REF_STR( :NCHAR ) // '-pixel spatial extent.'
      CALL ECH_REPORT( 0, REPORT_STRING )

*  Check for profile-weighted extraction.
      IF ( EXTRACTION_MODE .EQ. 'P' ) THEN
         JUST_PROFILE = .TRUE.

      ELSE
         JUST_PROFILE = .FALSE.
      END IF

*  Warn if X-profile modelling is active.
      IF ( OBJ_NPOLY .GT. 0 .AND. .NOT. SIMPLE ) THEN
         CALL ECH_REPORT( 0,
     :        ' Object profile X-dependency fits are being used.' )
      END IF

*  Calculate coordinates of order trace across frame.
      XSTART = 1
      XEND = NX
      CALL ECH_CALC_TRACE( NX, MAXIMUM_POLY, TRACE_POLYNOMIAL,
     :     X_TRACE_COORD, Y_TRACE_COORD, STATUS )

*  If simple extraction required then do it.
      IF ( EXTRACTION_MODE .EQ. 'S' ) THEN
         CALL ECH_EXTR_SIMPLE(
     :        NX,
     :        NY,
     :        GOT_QUALITY,
     :        GOT_AN_ARC,
     :        GOT_BALANCE,
     :        NO_ERRORS,
     :        OBJ_MASK,
     :        OBJ_BELOW,
     :        OBJ_ABOVE,
     :        READOUT,
     :        PHOTON,
     :        MAX_SKY_PIXELS,
     :        IMAGE,
     :        IMAGE_ERR,
     :        QUALITY,
     :        SKY_MODEL,
     :        SKY_MODEL_ERR,
     :        FLAT_MODEL,
     :        FLAT_MODEL_ERR,
     :        ARC_FRAME,
     :        NBAD,
     :        SPEC,
     :        SPECSIG,
     :        ARCSPEC,
     :        ARCSIG,
     :        Y_TRACE_COORD,
     :        STATUS
     :       )

      ELSE

*    For accurate computation of the uncertainties, the variance
*    in the sky estimate must be included.
*    This contributes a term Sum over i and j of w(i)w(j)V(i,j) where
*    V(i,j) is the co-variance between the sky estimates on
*    pixel i and j and w(i) is the weight used on the ith pixel.  The
*    variance matrix has to be estimated from the sky pixels used and
*    the polynomial order.  We make the approximation of constant
*    variance on each sky pixel for a given row.
        IF ( MAX_SKY_PIXELS .LT. 41 .AND. SKY_NPOLY .GT. 1 ) THEN
           CALL ECH_EXTR_COVARIANCE( MAX_SKY_PIXELS, SKY_NPOLY, SPACE,
     :          DEK_BELOW, DEK_ABOVE, SKY_MASK, VARMAT, STATUS )
        END IF

*    If quality is available and we are doing a profile-weighted
*    (but NOT optimal) extraction then we use the info right away
*    when estimating the profile.  Optimal extraction ignores the
*    quality until is is time to refine the variance estimates.
         USE_QUALITY = .TRUE.

*     Calculate minimum variance floor.
         VAR0 = READOUT * READOUT
         CPHOTON = MAX( 1.0, PHOTON )
         CMIN = ( V0_ALPHA * V0_BETA / ( V0_BETA - 1.0 ) ) ** 2.0 /
     :          CPHOTON - VAR0 * CPHOTON
         CMIN = MAX( 0., CMIN )

*    Loop through wavelength increments.
         DO IWAVE = XSTART, XEND

*        Extract data at this increment and estimate variance.
            Y_CENTER = Y_TRACE_COORD( IWAVE )
            CALL ECH_EXTR_INCPROF(
     :           NX,
     :           NY,
     :           USE_QUALITY,
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
     :           CPHOTON,
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
     :           NPIX,
     :           IMGDATA,
     :           SUM,
     :           ERRORS,
     :           VNORM,
     :           BALANCES,
     :           PROFILE,
     :           ARCPIX,
     :           ARCSUM,
     :           STATUS
     :          )

*        Initial estimate of the spectrum is a normal sum across the profile.
            IW = IWAVE - XSTART + 1
            SPEC( IW ) = REAL( SUM )
            SPECSIG( IW ) = ABS( SPEC( IW ) )

            IF ( GOT_AN_ARC ) THEN
               ARCSPEC( IW ) = REAL( ARCSUM )
               ARCSIG( IW ) = ABS( ARCSPEC( IW ) )
            END IF

            ESTIMATE = REAL( SUM )

*        If profile weighted extraction then do it.
            IF ( JUST_PROFILE ) THEN
               CALL ECH_EXTR_PROFILE(
     :              NX,
     :              NPIX,
     :              IW,
     :              GOT_AN_ARC,
     :              ESTIMATE,
     :              IMGDATA,
     :              ERRORS,
     :              ARCPIX,
     :              PROFILE,
     :              NBAD,
     :              SPEC,
     :              SPECSIG,
     :              ARCSPEC,
     :              ARCSIG,
     :              STATUS
     :             )
               DO IP = NPIX, 1, -1
                  IF ( ERRORS( IP ) .LT. 0.0 ) NBAD = NBAD + 1
               END DO

*        If Reject cosmic rays then do it.
            ELSE
               IF ( ZAPRATS ) THEN
                  REJECT_THRESH = 6.0
                  CALL ECH_EXTR_DECOSOPT(
     :                 NX,
     :                 NY,
     :                 GOT_QUALITY,
     :                 GOT_AN_ARC,
     :                 GOT_BALANCE,
     :                 NO_ERRORS,
     :                 FULL_SKYFIT,
     :                 OBJ_MASK,
     :                 OBJ_BELOW,
     :                 OBJ_ABOVE,
     :                 OBJ_NPOLY,
     :                 REJECT_THRESH,
     :                 IWAVE,
     :                 Y_CENTER,
     :                 VAR0,
     :                 CPHOTON,
     :                 CMIN,
     :                 MAX_SKY_PIXELS,
     :                 IMAGE,
     :                 IMAGE_ERR,
     :                 QUALITY,
     :                 SKY_MODEL_ERR,
     :                 FLAT_MODEL_ERR,
     :                 VARMAT,
     :                 NPIX,
     :                 IMGDATA,
     :                 IMGSUM,
     :                 ERRORS,
     :                 VNORM,
     :                 BALANCES,
     :                 ARCPIX,
     :                 PROFILE,
     :                 NBAD,
     :                 SPEC,
     :                 SPECSIG,
     :                 ARCSPEC,
     :                 ARCSIG,
     :                 STATUS
     :                )


*          Else If not cosmic ray zapping, just give normal weighted estimate,
*          with an extra loop to refine the variances.
               ELSE
                  CALL ECH_EXTR_OPTIMAL(
     :                 NX,
     :                 NY,
     :                 GOT_AN_ARC,
     :                 GOT_BALANCE,
     :                 NO_ERRORS,
     :                 FULL_SKYFIT,
     :                 OBJ_MASK,
     :                 OBJ_BELOW,
     :                 OBJ_ABOVE,
     :                 IWAVE,
     :                 Y_CENTER,
     :                 VAR0,
     :                 CPHOTON,
     :                 CMIN,
     :                 MAX_SKY_PIXELS,
     :                 IMAGE,
     :                 IMAGE_ERR,
     :                 SKY_MODEL_ERR,
     :                 FLAT_MODEL_ERR,
     :                 VARMAT,
     :                 NPIX,
     :                 IMGDATA,
     :                 ERRORS,
     :                 VNORM,
     :                 BALANCES,
     :                 ARCPIX,
     :                 PROFILE,
     :                 SPEC,
     :                 SPECSIG,
     :                 ARCSPEC,
     :                 ARCSIG,
     :                 STATUS
     :                )
                  DO IP = NPIX, 1, -1
                     IF ( ERRORS( IP ) .LT. 0.0 ) NBAD = NBAD + 1
                  END DO
               END IF
            END IF

*        Apply ADU-to-photon scaling (if not unity) to get counts in photons.
            IF ( PHOTON .NE. 1.0 ) THEN
               SPEC( IW ) = SPEC( IW ) * PHOTON
               SPECSIG( IW ) = SPECSIG( IW ) * PHOTON * CPHOTON
               IF ( GOT_AN_ARC ) THEN
                  ARCSPEC( IW ) = ARCSPEC( IW ) * PHOTON
                  ARCSIG( IW ) = ARCSIG( IW ) * PHOTON * CPHOTON
               END IF
            END IF
         END DO
      END IF

*  Report on any bad pixels excluded from the extraction.
      CALL CHR_ITOC( NBAD, REF_STR, NCHAR )
      REPORT_STRING = ' Total pixels rejected: ' //
     :      REF_STR( :NCHAR ) // '.'
      CALL ECH_REPORT( 0, REPORT_STRING )

  999 CONTINUE

      END


      REAL FUNCTION CHANCE( OBS, PRED, MA, THRLO, THRHI )
*+
*  Name:
*     REAL FUNCTION CHANCE

*  Notes:
*     Written by T.R. Marsh
*
*     Modified May 1989 to give -LOG(RATIO) for very small values
*     (approx RATIO < 1.E-34)
*
*     Given a variable with predicted value PRED, observed OBS
*     and uncertainty MA this routine determines whether
*     OBS lies further than THRLO below or THRHI above PRED
*     and if it does, it comes back with the ratio of the
*     probability of observing OBS/divided by the probability
*     of observing the relavant threshold (that is thrlo if
*     OBS .lt. PRED, or else THRHI). If not rejected it does not
*     compute the ratio (for speed) and it returns with -1. If the
*     probability is too small for REAL (i.e. = 0), the program
*     will return with -LOG(RATIO), a positive number greater
*     than 1
*
*     If MA .LT.0. it returns with -2.

*-
      REAL OBS, PRED, MA, THRLO, THRHI, PROBLO, PROBHI
      REAL OLDTHRLO, OLDTHRHI, POISS_PARAM, PLIM

      DOUBLE PRECISION ROOT2
      PARAMETER ( ROOT2 = 1.414213562 )

      DOUBLE PRECISION DEVIATE
      REAL ECH_CCNDBR
*
      DATA PLIM / 5.0 /

* Automatic rejection if ma is < 0.
      OLDTHRLO=1.
      OLDTHRHI=-1.
      PROBLO=0.
      PROBHI=0.

      IF(MA.LE.0.) THEN
        CHANCE = -2.
        GO TO 999
      END IF

* Compute rejection probability every time THRLO or THRHI are altered.
      IF(THRLO.NE.OLDTHRLO) THEN
        PROBLO = MAX( 1e-10, ECH_CCNDBR( THRLO ) )
        OLDTHRLO = THRLO
        DLOLIM = SQRT(MAX(0.,-2.*(-74.+LOG(max(1e-10,PROBLO)))))
        DLOLIM = SQRT(MAX(0.,-2.*(-76.4+LOG(max(1e-10,DLOLIM*PROBLO)))))
      END IF
      IF(THRHI.NE.OLDTHRHI) THEN
        PROBHI = ECH_CCNDBR( THRHI )
        OLDTHRHI = THRHI
        DHILIM = SQRT(MAX(0.,-2.*(-74.+LOG(max(1e-10,PROBHI)))))
        DHILIM = SQRT(MAX(0.,-2.*(-76.4+
     :               LOG(max(1e-10,DHILIM*PROBHI)))))
      END IF
*
* Gaussian rejection section
*
* If gaussian does not reject the point, then Poisson
* won't, so jump out of program if its OK for Gaussian.
*
      IF(OBS-PRED.GT.THRLO*MA .AND. OBS-PRED.LT.THRHI*MA) THEN
        CHANCE = -1.
        GO TO 999
      END IF
      POISS_PARAM = MA*MA
*
* Assume Gaussian if parameter of Poisson
* distribution is greater than PLIM. Since the
* variable has been passed by previous test, it
* must be rejected if it passes the test below.
*
      IF(POISS_PARAM.GT.PLIM) THEN
        DEVIATE = (OBS-PRED)/MA
        IF(DEVIATE.GT.-DLOLIM .AND. DEVIATE.LT.DHILIM) THEN
           CHANCE = ECH_CCNDBR( SNGL( DEVIATE ) )
          IF(DEVIATE.LT.0.D0) THEN
            CHANCE = CHANCE/PROBLO
          ELSE
            CHANCE = CHANCE/PROBHI
          END IF
        ELSE
*
* For large deviation X, asymptotic expansion gives
* CHANCE = exp(-X**2/2)/sqrt(2*pi)/X
*
* We return with LOG(PROB/CHANCE)
*
          CHANCE = REAL(DEVIATE**2/2. + 0.919 +
     :                     LOG(max(1d-10,DEVIATE)))
          IF(DEVIATE.LT.0.D0) THEN
            CHANCE = CHANCE + LOG(max(1e-10,PROBLO))
          ELSE
            CHANCE = CHANCE + LOG(max(1e-10,PROBHI))
          END IF
        END IF
        GO TO 999
      END IF
*
* End of Gaussian rejection section
*
* Poisson section
*
* Scale to give a Poisson variate with
* Poisson parameter = MA*MA
*
      NDATA = MAX(0,NINT(OBS - PRED + POISS_PARAM))
      CHANCE = -1.

* Case 1: Observed is less than Poisson parameter.
      IF(REAL(NDATA).LT.POISS_PARAM) THEN
        Q = EXP(-POISS_PARAM)
        IF(NDATA.EQ.0 .AND. Q.LT.PROBLO) THEN
          CHANCE = Q/PROBLO
          GO TO 999
        END IF
        P = Q
        I = 1
        DO WHILE(P.LE.PROBLO .AND. I.LE.NDATA)
          Q = Q*POISS_PARAM/REAL(I)
          P = P + Q
          I = I + 1
        END DO
        IF(P.LE.PROBLO) CHANCE = P/PROBLO

* Case 2: More than Poisson parameter observed.
      ELSE
        P = -POISS_PARAM+NDATA*
     :            LOG(max(1e-10,POISS_PARAM))-QFACTOR(NDATA)
        CHANCE = EXP(P)/PROBHI
        I = NDATA + 1
        Q = 1.
        SUM = 1.
        DO WHILE(SUM*CHANCE.LE.1. .AND. I.LE.NDATA+NINT(PLIM))
          Q = Q*POISS_PARAM/REAL(I)
          SUM = SUM + Q
          I = I + 1
        END DO
        IF(SUM*CHANCE.LE.1.) CHANCE = SUM*CHANCE
        IF(CHANCE.LT.1.E-20) CHANCE = LOG(max(1e-10,PROBHI/SUM))-P
      END IF
  999 CONTINUE

      END


      REAL FUNCTION QFACTOR( NDATA )
*+
*  Name:
*     REAL FUNCTION QFACTOR

*  Purpose:
*     Evaluates LOG(NDATAfactorial).  For large data
*     values it calls a Gamma function evaluator.

*-
      IMPLICIT NONE

      INTEGER MAXSTORE
      PARAMETER ( MAXSTORE = 30 )

      REAL A(MAXSTORE)
      INTEGER NDATA
      INTEGER NTOP
      INTEGER J

*  Functions called:
      REAL GAMMLN
*.
      NTOP = 0
      A( 1 ) = 0.0

      IF( NDATA .LE. NTOP ) THEN
         QFACTOR = A( NDATA + 1 )

      ELSE IF( NDATA + 1 .LE. MAXSTORE ) THEN
         DO J = NTOP + 1, NDATA
            A( J + 1 ) = A( J ) + LOG( MAX( 1E-10, REAL( J ) ) )
         END DO

      ELSE
         QFACTOR = GAMMLN( REAL( NDATA + 1 ) )
      END IF

      END


      REAL FUNCTION GAMMLN( XX )
*+
*  Name:
*     REAL FUNCTION GAMMLN

*  Purpose:
*     Returns LOG(Gamma(XX)) for XX > 0.

*-
      IMPLICIT NONE

*  Arguments Given:
      REAL XX

*  Local Variables:
      DOUBLE PRECISION COF( 6 )
      DOUBLE PRECISION STP
      DOUBLE PRECISION HALF
      DOUBLE PRECISION ONE
      DOUBLE PRECISION FPF
      DOUBLE PRECISION X
      DOUBLE PRECISION TMP
      DOUBLE PRECISION SER

      INTEGER I
*
      DATA COF, STP / 76.18009173D0, -86.50532033D0, 24.01409822D0,
     :               -1.231739516D0,  .120858003D-2,   -.536382D-5,
     :                2.50662827465D0 /

      DATA HALF, ONE, FPF / 0.5D0, 1.0D0, 5.5D0 /
*
      X = DBLE( XX ) - ONE
      TMP = X + FPF
      TMP = ( X + HALF ) * LOG( MAX( 1D-10, TMP ) ) - TMP
      SER = ONE
      DO I = 1, 6
         X = X + ONE
         SER = SER + COF( I ) / X
      END DO

      GAMMLN = REAL( TMP + LOG( MAX( 1D-10, STP * SER ) ) )

      END

      REAL FUNCTION ECH_CCNDBR( X )
*+
*  Name:
*     ECHOMOP - ECH_CCNDBR

*  Purpose:
*     Computes the complement of the cumulative normal distribution
*     function.

*  Authors:
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     03-SEP-1996 (MJC):
*       Initial version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      IMPLICIT NONE

      REAL X

      DOUBLE PRECISION XA
      DOUBLE PRECISION ROOT2
      PARAMETER ( ROOT2 = 1.414213562 )

      INTEGER IFAIL

*  Functions Called:
*  NAG version.
*      DOUBLE PRECISION S15ACF

*  PDA version.
      DOUBLE PRECISION PDA_DERFC
*.

*  NAG Version.
*      IFAIL = 0
*      ECH_CCNDBR = REAL( S15ACF( DBLE( X ), IFAIL ) )

*  PDA version.
      XA = DBLE( ABS( X ) ) / ROOT2
      IF ( XA .GT. 26.5 ) THEN
         ECH_CCNDBR = 0.0

      ELSE
         IFAIL = 0
         ECH_CCNDBR = 0.5 * REAL( PDA_DERFC( XA, IFAIL ) )
      END IF

      END
