      SUBROUTINE ECH_EXTR_PROFILE(
     :           NX,
     :           NPIX,
     :           IWAVE,
     :           GOT_AN_ARC,
     :           ESTIMATE,
     :           IMGDATA,
     :           ERRORS,
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
*     ECHOMOP - ECH_EXTR_PROFILE

*  Purpose:
*     Extract an order using the profile weights.

*  Description:
*     This routine extracts the profile for a single increment in wavelength
*     along with raw intensities, errors, balances factors and arc intensity.
*     The pixels in the increment are weighted according to the modeled
*     profile at that offset from the order trace.

*  Method:
*     A profile-weighted summation of all 'good' pixels in the x-increment
*     is used.

*  Invocation:
*     CALL ECH_EXTR_PROFILE(
*     :    NX,
*     :    NPIX,
*     :    IWAVE,
*     :    GOT_AN_ARC,
*     :    ESTIMATE,
*     :    IMGDATA,
*     :    ERRORS,
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
*        Number of values in increment arrays.
*     IWAVE = INTEGER (Given)
*        Increment index into output arrays.
*     GOT_AN_ARC = LOGICAL (Given)
*        TRUE if arc frame is to be extracted too.
*     ESTIMATE = REAL (Given)
*        Estimate of object sum.
*     IMGDATA = REAL (Given)
*        Extracted increment.
*     ERRORS = REAL (Given)
*        Extracted variances.
*     PROFILE = REAL (Given)
*        Normalised profile.
*     ARCPIX = REAL (Given)
*        Extracted arc increment.
*     NRAYS = INTEGER (Returned)
*        Running Count of bad pixels.
*     SPEC = REAL (Returned)
*        Calculated object pixel intensities.
*     SPECSIG = REAL (Returned)
*        Calculated object pixel errors.
*     ARCSPEC = REAL (Returned)
*        Calculated arc intensities at increment.
*     ARCSIG = REAL (Returned)
*        Calculated arc errors.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

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

*  Arguments Given:
      INTEGER NX
      INTEGER NPIX
      REAL ESTIMATE
      INTEGER IWAVE
      LOGICAL GOT_AN_ARC
      REAL ARCPIX( MAX_SLICE_PIXELS )

*  Arguments Returned:
      INTEGER NRAYS
      REAL ARCSPEC( NX )
      REAL ARCSIG( NX )
      REAL SPEC( NX )
      REAL SPECSIG( NX )
      REAL PROFILE( MAX_SLICE_PIXELS )
      REAL IMGDATA( MAX_SLICE_PIXELS )
      REAL ERRORS( MAX_SLICE_PIXELS )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL SUM
      REAL SIG
      REAL ARCSUM
      REAL NORM

      INTEGER I

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Initilise totals.
      SUM = 0.0
      ARCSUM = 0.0
      NORM = 0.0
      SPEC( IWAVE ) = 0.0
      SPECSIG( IWAVE ) = 0.0
      IF ( GOT_AN_ARC ) THEN
         ARCSPEC( IWAVE ) = 0.0
         ARCSIG( IWAVE) = 0.0
      END IF

*  Loop through profile increments.
      DO I = 1, NPIX

*     If a usable value then.
         IF ( ERRORS( I ) .GT. 0.0 ) THEN

*        Add values to sums for object and arc (if required).
            SUM = SUM + IMGDATA( I ) * PROFILE( I )
            NORM = NORM + PROFILE( I ) * PROFILE( I )
            IF ( ESTIMATE .GT. 0.0 ) THEN
               SIG = SQRT( ABS( IMGDATA( I ) /
     :               ESTIMATE - PROFILE( I ) * ESTIMATE ) ) *
     :               PROFILE( I )
            END IF
            SPECSIG( IWAVE ) = SPECSIG( IWAVE ) +
     :            MAX( SIG * SIG + PROFILE( I ) *
     :            ERRORS( I ) * ERRORS( I ),
     :            IMGDATA( I ) * PROFILE( I ) )

            IF ( GOT_AN_ARC ) THEN
               ARCSUM = ARCSUM + ARCPIX( I ) * PROFILE( I )
            END IF

*     Else increment dead-pixel counter and bump up variance.
         ELSE
            IF ( ERRORS( I ) .LT. 0. ) NRAYS = NRAYS + 1
            SPECSIG( IWAVE ) = SPECSIG( IWAVE ) +
     :            ABS( PROFILE( I ) * ESTIMATE )
         END IF
      END DO

*  Copy sums into final arrays.
      IF ( NORM .GT. 0.0 ) THEN
         SPEC( IWAVE ) = SUM / NORM
         SPECSIG( IWAVE ) = ABS( SPECSIG( IWAVE ) ) / NORM
      END IF
      IF ( GOT_AN_ARC ) THEN
         ARCSPEC( IWAVE ) = ARCSUM
         ARCSIG( IWAVE ) = SQRT( ABS( ARCSUM ) )
      END IF

      END
