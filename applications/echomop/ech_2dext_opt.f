      SUBROUTINE ECH_2DEXT_OPT(
     :           IMAGE,
     :           IMAGE_ERR,
     :           QUALITY,
     :           NO_ARC,
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
     :           SKY_MODEL,
     :           SKY_MODEL_ERR,
     :           VARMAT,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_2DEXT_OPT

*  Purpose:
*     Extraction from 2-D distortion corrected orders.

*  Description:
*     o This routine has been modified from EXT_OPT, written by
*       T.R. Marsh, June 1988, which itself was modified for FIGARO
*       from Keith Horne's original EXTOPT in the program PAMELA.
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
*     separated.
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
*
*     09/06/88 TRM @RGO -- Modified for FIGARO
*     17/06/88 TRM @RGO -- Modified rejection scheme to do it one at a time
*                          with no restoration, slow but careful. CHANCE instead
*                          of PREJECT
*     30/06/88 TRM @RGO -- Improved estimation of uncertainties by accounting
*                          for uncertainty in the sky fit. Have not modified
*                          extraction weights to account for the same effect
*                          although in principle this should be done.
*
*     01/07/88 TRM @RGO -- Included correlation between sum over profile and
*                          individual points during estimation of variances
*                          for profile fitting.
*
*     01/07/88 TRM @RGO -- Included correlation between individual data points
*                          and spectrum estimate during cosmic ray rejection
*
*     24/08/88 TRM @RGO -- Fixed bug which prevented INTERACT = .FALSE. working.
*                          Stopped irritating PAR_RDUSER input with no <CR>
*
*     31/05/89 TRM @STSCI -- Modified to cope with new version of CHANCE
*
*     05/09/90 DJM @UCL -- New version decomposed into separate modules

*  Invocation:
*     CALL ECH_2DEXT_OPT(
*     :    IMAGE,
*     :    IMAGE_ERR,
*     :    QUALITY,
*     :    NO_ARC,
*     :    ARC_FRAME,
*     :    NX,
*     :    NY,
*     :    N_ORDERS,
*     :    EXTRACTION_MODE,
*     :    FULL_SKYFIT,
*     :    ZAPRATS,
*     :    MAX_SKY_PIXELS,
*     :    SKY_MASK,
*     :    OBJ_MASK,
*     :    DEK_BELOW,
*     :    DEK_ABOVE,
*     :    SKY_NPOLY,
*     :    READOUT,
*     :    PHOTON,
*     :    OBJ_NPOLY,
*     :    MAXIMUM_POLY,
*     :    TRACE_POLYNOMIAL,
*     :    PFL_SUBSAMPLES,
*     :    MODELED_PROFILE,
*     :    POLY_PROFILES,
*     :    SPEC, SPECSIG,
*     :    ARCSPEC,
*     :    ARCSIG,
*     :    SKY_MODEL,
*     :    SKY_MODEL_ERR,
*     :    VARMAT,
*     :    STATUS
*     :   )

*  Arguments:
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and max_sky_pixels rows.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     IMAGE_ERR = REAL (Given)
*        Input data frame variance array.
*     QUALITY = LOGICAL (Given)
*        Input data frame quality flags array.
*     NO_ARC = LOGICAL (Given)
*        Set TRUE to indicate that no arc frame is available.
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
*     SKY_MODEL = REAL (Given and Returned)
*        Modelled sky intensities.
*     SKY_MODEL_ERR = REAL (Given and Returned)
*        Variances on modelled sky intensities.
*     VARMAT = REAL (Given and Returned)
*        Sky covariance matrix.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Exit right away if the order is disabled
*     Intialise spectrum
*     Report characteristics of extraction selected
*     Estimate the spatial extent in pixels
*     Set appropriate subsampling rate
*     Calculate object limits above and below order trace
*     Abort if no pixels marked as object
*     If only a single object pixel and not 'simple' extraction, default to it
*     Check for profile weighted extraction selected
*     Warn user of constraints if polynomial profile modeling is active
*     Calculate coordinates of order trace across frame
*     If simple extraction required then do it
*     Else
*        If poly fit is being used then calculate polynomials at each offset
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

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     12-JUN-1996 (MJC):
*       Tidier messages.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'ECH_QUALITIES.INC'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER N_ORDERS
      LOGICAL ZAPRATS
      LOGICAL NO_ARC
      INTEGER PFL_SUBSAMPLES
      INTEGER MAX_SKY_PIXELS
      INTEGER DEK_ABOVE
      INTEGER DEK_BELOW
      INTEGER SKY_NPOLY
      INTEGER OBJ_NPOLY
      REAL PHOTON
      REAL ARC_FRAME( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL IMAGE( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL IMAGE_ERR( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      BYTE QUALITY( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL READOUT
      INTEGER MAXIMUM_POLY
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
      INTEGER SKY_MASK( -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      INTEGER OBJ_MASK( -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL VARMAT( MAX_SKY_PIXELS, MAX_SKY_PIXELS )
      DOUBLE PRECISION POLY_PROFILES( MAXIMUM_POLY, -PFL_SUBSAMPLES / 2:
     :      PFL_SUBSAMPLES / 2 )

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION SPACE( MAX_COVAR_SIZE )
      DOUBLE PRECISION SUM
      DOUBLE PRECISION ARCSUM

      REAL IMGSUM
      REAL VNORM
      REAL REJECT_THRESH
      REAL IMGDATA( MAX_SLICE_PIXELS )
      REAL PROFILE( MAX_SLICE_PIXELS )
      REAL ARCPIX( MAX_SLICE_PIXELS )
      REAL ERRORS( MAX_SLICE_PIXELS )
      REAL ESTIMATE

      INTEGER I
      INTEGER XSTART
      INTEGER XEND
      INTEGER ORDER_SIZE
      INTEGER NBAD
      INTEGER NPIX
      INTEGER IW
      INTEGER OBJ_COUNT
      INTEGER IWAVE
      INTEGER OBJ_ABOVE
      INTEGER OBJ_BELOW
      INTEGER SUBSTEPS
      INTEGER NCHAR

      LOGICAL SIMPLE
      LOGICAL JUST_PROFILE
      LOGICAL USE_QUALITY
      LOGICAL GOT_QUALITY
      LOGICAL GOT_AN_ARC

      CHARACTER*32 REF_STR

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) )  RETURN

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

*  Report characteristics of extraction selected.
      CALL ECH_REPORT( 0,
     :     ' Extracting from 2-D distortion corrected data.' )
      IF ( PHOTON .LE. 0.0 ) THEN
         CALL ECH_REPORT( 0,
     :        ' Defaulting to ADU conversion factor of 1.' )
         PHOTON = 1.0
      END IF
      SIMPLE = .FALSE.
      IF ( EXTRACTION_MODE .EQ. 'O' ) THEN
         CALL ECH_REPORT( 0, ' Using Optimal extraction algorithm.' )

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

*  Estimate the spatial extent in pixels, set appropriate subsampling rate.
      CALL ECH_CALC_PROFSAMP( NY, N_ORDERS, PFL_SUBSAMPLES, DEK_BELOW,
     :     DEK_ABOVE, ORDER_SIZE, SUBSTEPS, STATUS )

*  Calculate object limits above and below order trace.
      OBJ_BELOW = 999
      OBJ_ABOVE = -999
      OBJ_COUNT = 0
      DO I = DEK_BELOW + 1, DEK_ABOVE - 1
         IF ( OBJ_MASK( I ) .NE. 0 .AND. OBJ_BELOW .GT. I ) OBJ_BELOW =I
         IF ( OBJ_MASK( I ) .NE. 0 .AND. OBJ_ABOVE .LT. I ) OBJ_ABOVE =I
         IF ( OBJ_MASK( I ) .NE. 0 ) OBJ_COUNT = OBJ_COUNT + 1
      END DO

*  Abort if no pixels marked as object.
      IF ( OBJ_COUNT .EQ. 0 ) THEN
         CALL ECH_REPORT( 0,
     :        ' No pixels marked as Object in profile: no extraction.' )
         GO TO 999

*  If only a single object pixel and not 'simple' extraction, default to it.
      ELSE IF ( OBJ_COUNT .EQ. 1 .AND.
     :          EXTRACTION_MODE .NE. 'S' ) THEN
         CALL ECH_REPORT( 0,
     :        ' Single-pixel object: using simple extraction.' )
         EXTRACTION_MODE = 'S'
         SIMPLE = .TRUE.
      END IF
      CALL CHR_ITOC( OBJ_COUNT, REF_STR, NCHAR )
      REPORT_STRING = ' Extracting object over ' //
     :      REF_STR( :NCHAR ) // '-pixel spatial extent.'
      CALL ECH_REPORT( 0, REPORT_STRING )

*  Check for profile-weighted extraction selected.
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

      XSTART = 1
      XEND = NX

*  If simple extraction required then do it.
      IF ( EXTRACTION_MODE .EQ. 'S' ) THEN
         CALL ECH_EXTR_2DSIMPLE(
     :        NX,
     :        GOT_QUALITY,
     :        GOT_AN_ARC,
     :        OBJ_MASK,
     :        ZAPRATS,
     :        READOUT,
     :        PHOTON,
     :        MAX_SKY_PIXELS,
     :        IMAGE,
     :        IMAGE_ERR,
     :        QUALITY,
     :        SKY_MODEL,
     :        SKY_MODEL_ERR,
     :        ARC_FRAME,
     :        NBAD,
     :        SPEC,
     :        SPECSIG,
     :        ARCSPEC,
     :        ARCSIG,
     :        STATUS
     :       )

      ELSE

*     If poly fit is being used then calculate polynomials at each offset.
*
*     For accurate computation of the uncertainties the variance
*     in the sky estimate must be included.
*     This contributes a term Sum over i and j of w(i)w(j)V(i,j) where
*     V(i,j) is the co-variance between the sky estimates on
*     pixel i and j and w(i) is the weight used on the ith pixel. The
*     variance matrix has to be estimated from the sky pixels used and
*     the polynomial order. We make the approximation of constant variance
*     on each sky pixel for a given row.
*     Calculate sky co-variance matrix
         IF ( MAX_SKY_PIXELS .LT. 41 )
     :      CALL ECH_EXTR_COVARIANCE(
     :           MAX_SKY_PIXELS,
     :           MAX( 1, SKY_NPOLY ),
     :           SPACE,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           SKY_MASK,
     :           VARMAT,
     :           STATUS
     :          )

*     If quality is available and we are doing a profile-weighted
*     but NOT optimal) extraction then we use the info right away
*     when estimating the profile.  Optimal extraction ignores the
*     quality until is is time to refine the variance estimates.
         USE_QUALITY = .FALSE.
         IF ( GOT_QUALITY .AND. JUST_PROFILE ) USE_QUALITY = .TRUE.

*     Loop thru wavelength increments.
         DO IWAVE = XSTART, XEND

*        Extract data at this increment and estimate variance.
            CALL ECH_EXTR_2DINCPROF(
     :           NX,
     :           USE_QUALITY,
     :           GOT_AN_ARC,
     :           OBJ_MASK,
     :           OBJ_NPOLY,
     :           MAXIMUM_POLY,
     :           IWAVE,
     :           READOUT,
     :           MAX( 1.0, PHOTON ),
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
     :           SUM,
     :           ERRORS,
     :           VNORM,
     :           PROFILE,
     :           ARCPIX,
     :           ARCSUM,
     :           STATUS
     :          )
            NPIX = OBJ_ABOVE - OBJ_BELOW + 1

*        Initial estimate of the spectrum is a normal sum across the profile..
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

            ELSE

*           If Reject cosmic rays then do it.
               IF ( ZAPRATS ) THEN
                  REJECT_THRESH = 6.0
                  CALL ECH_EXTR_2DECOSOPT(
     :                 NX,
     :                 GOT_QUALITY,
     :                 GOT_AN_ARC,
     :                 FULL_SKYFIT,
     :                 OBJ_MASK,
     :                 OBJ_NPOLY,
     :                 REJECT_THRESH,
     :                 IWAVE,
     :                 READOUT,
     :                 MAX( 1.0, PHOTON ),
     :                 MAX_SKY_PIXELS,
     :                 IMAGE,
     :                 IMAGE_ERR,
     :                 QUALITY,
     :                 SKY_MODEL,
     :                 SKY_MODEL_ERR,
     :                 VARMAT,
     :                 IMGDATA,
     :                 IMGSUM,
     :                 ERRORS,
     :                 VNORM,
     :                 ARCPIX,
     :                 PROFILE,
     :                 NBAD,
     :                 SPEC,
     :                 SPECSIG,
     :                 ARCSPEC,
     :                 ARCSIG,
     :                 STATUS
     :                )


*           Else If not cosmic ray zapping, just give normal weighted estimate,
*           with an extra loop to refine the variances.
               ELSE
                  CALL ECH_EXTR_2DOPTIMAL(
     :                 NX,
     :                 GOT_AN_ARC,
     :                 FULL_SKYFIT,
     :                 OBJ_MASK,
     :                 IWAVE,
     :                 READOUT,
     :                 MAX( 1.0, PHOTON ),
     :                 MAX_SKY_PIXELS,
     :                 IMAGE,
     :                 IMAGE_ERR,
     :                 SKY_MODEL,
     :                 SKY_MODEL_ERR,
     :                 VARMAT,
     :                 IMGDATA,
     :                 ERRORS,
     :                 VNORM,
     :                 ARCPIX,
     :                 PROFILE,
     :                 SPEC,
     :                 SPECSIG,
     :                 ARCSPEC,
     :                 ARCSIG,
     :                 STATUS
     :                )
               END IF
            END IF

*        Apply ADU to photon scaling to get counts in photons.
            IF ( PHOTON .NE. 1.0 ) THEN
               SPEC( IW ) = SPEC( IW ) * PHOTON
               SPECSIG( IW ) = SPECSIG( IW ) * PHOTON *
     :               MAX( 1.0, PHOTON )
               ARCSPEC( IW ) = ARCSPEC( IW ) * PHOTON
               ARCSIG( IW ) = ARCSIG( IW ) * PHOTON * MAX( 1.0, PHOTON )
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
