      SUBROUTINE ECH_EXTR_OPTIMAL(
     :           NX,
     :           NY,
     :           GOT_AN_ARC,
     :           GOT_BALANCE,
     :           NO_ERRORS,
     :           FULL_SKYFIT,
     :           OBJ_MASK,
     :           OBJ_BELOW,
     :           OBJ_ABOVE,
     :           IWAVE,
     :           Y_CENTER,
     :           VAR0,
     :           PHOTON,
     :           CMIN,
     :           MAX_SKY_PIXELS,
     :           IMAGE,
     :           IMAGE_ERR,
     :           SKY_MODEL_ERR,
     :           FLAT_MODEL_ERR,
     :           VARMAT,
     :           NPIX,
     :           IMGDATA,
     :           ERRORS,
     :           VNORM,
     :           BALANCES,
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
*     ECHOMOP - ECH_EXTR_OPTIMAL

*  Purpose:
*     Extracts an order using optimal weighting.

*  Description:
*     This routine calculates the optimal extraction weights based upon
*     the individual variances. These are then applied and an increment
*     weighted sum calculated.

*  Invocation:
*     CALL ECH_EXTR_OPTIMAL(
*    :     NX,
*    :     NY,
*    :     GOT_AN_ARC,
*    :     GOT_BALANCE,
*    :     NO_ERRORS,
*    :     FULL_SKYFIT,
*    :     OBJ_MASK,
*    :     OBJ_BELOW,
*    :     OBJ_ABOVE,
*    :     IWAVE,
*    :     Y_CENTER,
*    :     VAR0,
*    :     PHOTON,
*    :     CMIN,
*    :     MAX_SKY_PIXELS,
*    :     IMAGE,
*    :     IMAGE_ERR,
*    :     SKY_MODEL_ERR,
*    :     FLAT_MODEL_ERR,
*    :     VARMAT,
*    :     NPIX,
*    :     IMGDATA,
*    :     ERRORS,
*    :     VNORM,
*    :     BALANCES,
*    :     ARCPIX,
*    :     PROFILE,
*    :     SPEC,
*    :     SPECSIG,
*    :     ARCSPEC,
*    :     ARCSIG,
*    :     STATUS
*    :    )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     NPIX = INTEGER (Given)
*        Number of values in increment.
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
*     OBJ_BELOW = INTEGER (Given)
*        Offset of bottom edge of object from trace.
*     OBJ_ABOVE = INTEGER (Given)
*        Offset of top edge of object from trace.
*     IWAVE = INTEGER (Given)
*        Index of pixel in wavelength direction.
*     Y_CENTER = REAL (Given)
*        Y coordinate of order trace center.
*     VAR0 = REAL (Given)
*        Readout noise level in counts quared.
*     PHOTON = REAL (Given)
*        Photon to ADU conversion factor.
*     CMIN = REAL (Given)
*        Variance floor.
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum extent of sky and therefore dekker.
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     IMAGE_ERR = REAL (Given)
*        Input errors frame image of dimensions nx columns and ny rows.
*     VARMAT = REAL (Given)
*        Covariance matrix for sky model.
*     SKY_MODEL_ERR = REAL (Given)
*        Modeled sky errors at offsets from trace.
*     FLAT_MODEL_ERR = REAL (Given)
*        Balance factor errors.
*     IMGDATA = REAL (Returned)
*        Calculated object pixel intensities.
*     ERRORS = REAL (Returned)
*        Calculated object pixel errors.
*     VNORM = REAL (Returned)
*        Minimum variance per pixel.
*     BALANCES = REAL (Returned)
*        Balance factors for increment.
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
*     STATUS = INTEGER (Given and Retrned)
*        Input/Output status conditions.

*  Method:
*     Calculate minimum variance floor
*      Determine pioffset limits
*     Loop twice to refine variance estiamtes
*        Load ordinary optimal weights when sky fit uncertainty insignificant.
*        If full_skyfit = .TRUE., modify weights to include uncertainty in
*                       sky fit optimised variances
*        Sum object (and arc) increment values using optimal weights
*        Stow optimal spectrum and its standard deviation
*        If another loop to go then
*           Loop thru profile values
*               If pixel within frame boundaries then
*                  If an object pixel then
*                  Endif
*               Endif
*           End loop
*        Endif
*     End loop

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
      INTEGER OBJ_MASK( -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      INTEGER OBJ_BELOW
      INTEGER OBJ_ABOVE
      REAL Y_CENTER
      INTEGER IWAVE
      REAL VAR0
      REAL PHOTON
      LOGICAL GOT_AN_ARC
      LOGICAL GOT_BALANCE
      LOGICAL NO_ERRORS
      LOGICAL FULL_SKYFIT
      REAL ARCPIX( MAX_SLICE_PIXELS )
      REAL IMAGE_ERR( NX, NY )
      REAL FLAT_MODEL_ERR( NX, -MAX_SKY_PIXELS/2 : MAX_SKY_PIXELS / 2 )
      INTEGER NPIX

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
      REAL BALANCES( MAX_SLICE_PIXELS )
      REAL SKY_MODEL_ERR( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL WEIGHT( MAX_SLICE_PIXELS )
      REAL VAR
      REAL CMIN
      REAL WSUM
      REAL WVSUM
      REAL FRACTION
      REAL YFRAC
      REAL SUM
      REAL ARCSUM
      REAL SUMD
      REAL SUMD2
      REAL SKYS
      REAL INPUT_VAR
      REAL STAR
      REAL BALANCE

      INTEGER I
      INTEGER J
      INTEGER K
      INTEGER IOFF
      INTEGER IOFFST
      INTEGER IOFFEN
      INTEGER YCOORD
      INTEGER IFAIL

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Get fractional pixel part of trace coordinate.
      YFRAC = Y_CENTER - FLOAT( INT( Y_CENTER + 0.5 ) )

*  Loop twice to refine variance estiamtes.
      DO J = 2, 1, -1

*     Load ordinary optimal weights when sky fit uncertainty insignificant.
*     These will be changed if SKYFIT is true.
         DO I = NPIX, 1, -1
            WEIGHT( I ) = PROFILE( I ) / ERRORS( I ) ** 2
         END DO

*    If full_skyfit is true, modify weights to include uncertainty in
*    sky fit optimised variances.
         IF ( FULL_SKYFIT ) THEN
            CALL ECH_SKYFIT_WEIGHTS( NPIX, MAX_SKY_PIXELS, VNORM,
     :           .TRUE., VARMAT, IMGDATA, ERRORS, WEIGHT, PROFILE,
     :           IFAIL )
         END IF

*     Sum object and arc increment values using optimal weights.
         WSUM = 0.00
         DO I = NPIX, 1, -1
            WSUM = WSUM + WEIGHT( I )
         END DO
         IF ( WSUM .NE. 0.0 ) THEN
            DO I = NPIX, 1, -1
               WEIGHT( I ) = WEIGHT( I ) / WSUM
            END DO
         END IF

         SUM = 0.0
         ARCSUM = 0.0
         WSUM = 0.0
         SUMD2 = 0.0
         WVSUM = 0.0
         DO I = NPIX, 1, -1
            SUM = SUM + WEIGHT( I ) * IMGDATA( I )
            ARCSUM = ARCSUM  + WEIGHT( I ) * MAX( 0.0, ARCPIX( I ) )
            WSUM = WSUM + WEIGHT( I ) * PROFILE( I )
            IF ( ERRORS( I ) .GT. 0.0 )
     :         WVSUM = WVSUM + ( WEIGHT( I ) * ERRORS( I ) ) ** 2
            SUMD = 0.0
            DO K = NPIX, 1, -1
               SUMD = SUMD + WEIGHT( K ) * VARMAT( K, I )
            END DO
            SUMD2 = SUMD2 + WEIGHT( I ) * SUMD
         END DO

*     Stow optimal spectrum and its standard deviation.
         IF ( WSUM .NE. 0.0 ) THEN
            SPEC( IWAVE ) = SUM / WSUM
            SPECSIG( IWAVE ) = ABS( WVSUM + SUMD2 * VNORM ) /
     :            ( WSUM * WSUM )
            IF ( GOT_AN_ARC ) THEN
               ARCSPEC( IWAVE ) = ARCSUM / WSUM
               ARCSIG( IWAVE )  = ARCSUM / WSUM
            END IF
         END IF

*     If another loop to go then.
         IF ( J .EQ. 2 ) THEN
            I = 0

*        Loop through profile values.
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

*           If an object pixel then.
               IF ( ( OBJ_MASK( IOFF ) .EQ. 1 .OR.
     :              IOFF .EQ. OBJ_BELOW - 1 .OR.
     :              IOFF .EQ. OBJ_ABOVE + 1 )  .AND.
     :              ERRORS( I ) .GT. 0.0 .AND.
     :              IMAGE( IWAVE, YCOORD ) .GT. ECH__BAD_REAL ) THEN
                  SKYS = SKY_MODEL_ERR( IWAVE, IOFF )
*                  BALANCE = BALANCES( I )
                  BALANCE = 1.0
                  STAR = SPEC( IWAVE ) / BALANCE * PROFILE( I )
                  IF ( NO_ERRORS ) THEN
                     INPUT_VAR = 0.0

                  ELSE
                     INPUT_VAR = IMAGE_ERR( IWAVE, YCOORD ) ** 2.0
                  END IF
                  VAR = BALANCE *
     :                 ( BALANCE * ( VAR0 + INPUT_VAR ) +
     :                 MAX( CMIN + ABS( STAR ),
     :                  SKYS + ABS( STAR ) ) / PHOTON )
                  STAR = STAR * FRACTION
                  IF ( GOT_BALANCE ) THEN
                     IF ( FLAT_MODEL_ERR( IWAVE, IOFF ) .GT.
     :                    0.0 ) THEN
                        VAR = VAR +
     :                        ( FLAT_MODEL_ERR( IWAVE, IOFF ) *
     :                        IMAGE( IWAVE, YCOORD ) ) ** 2.0
                     END IF
                  END IF
                  VAR = VAR * FRACTION
                  ERRORS( I ) = MAX( 1.0, SQRT( ABS( VAR ) ) )
               END IF
               YCOORD = YCOORD + 1
            END DO
         END IF
      END DO

      END
