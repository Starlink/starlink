      SUBROUTINE ECH_EXTR_2DSIMPLE(
     :           NX,
     :           GOT_QUALITY,
     :           GOT_AN_ARC,
     :           OBJ_MASK,
     :           CR_CLEAN,
     :           READOUT,
     :           PHOTON,
     :           MAX_SKY_PIXELS,
     :           IMAGE,
     :           IMAGE_ERR,
     :           QUALITY,
     :           SKY_MODEL,
     :           SKY_MODEL_ERR,
     :           ARC_FRAME,
     :           NBAD,
     :           SPEC,
     :           SPECSIG,
     :           ARCSPEC,
     :           ARCSIG,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_EXTR_2DSIMPLE

*  Purpose:
*     Extracts an order using a simple sum.

*  Description:
*     This routine extracts the spectrum for all increments in wavelength
*     along with raw intensities, errors, arc intensity.  The input data
*     array is assumed to have an associated variance array taking into
*     account balance factors etc.

*  Method:
*     A simple summation of all 'good' pixels in the x-increment is used.
*     Pixels marked bad in the quality array may be excluded from the sum
*     using the 'cr_clean' flag, in which case the sums wil be scaled
*     to re-normalise for any absent pixels.

*  Invocation:
*     CALL ECH_EXTR_2DSIMPLE(
*    :     NX,
*    :     GOT_QUALITY,
*    :     GOT_AN_ARC,
*    :     OBJ_MASK,
*    :     CR_CLEAN,
*    :     READOUT,
*    :     PHOTON,
*    :     MAX_SKY_PIXELS,
*    :     IMAGE,
*    :     IMAGE_ERR,
*    :     QUALITY,
*    :     SKY_MODEL,
*    :     SKY_MODEL_ERR,
*    :     ARC_FRAME,
*    :     NBAD,
*    :     SPEC,
*    :     SPECSIG,
*    :     ARCSPEC,
*    :     ARCSIG,
*    :     STATUS
*    :    )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     GOT_QUALITY = LOGICAL (Given)
*        TRUE if quality data is available.
*     GOT_AN_ARC = LOGICAL (Given)
*        TRUE if arc frame is to be extracted too.
*     OBJ_MASK = INTEGER (Given)
*        Status of each pixel across profile.
*     CR_CLEAN = LOGICAL (Given)
*        TRUE if bad pixels are to be excluded.
*     READOUT = REAL (Given)
*        Readout noise level in counts.
*     PHOTON = REAL (Given)
*        Photon to ADU conversion factor.
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum extent of sky and therefore dekker.
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and max_sky_pixels rows.
*     IMAGE_ERR = REAL (Given)
*        Input errors frame image of dims nx columns and max_sky_pixels rows.
*     QUALITY = BYTE (Given)
*        Input quality frame image of dims nx columns and max_sky_pixels rows.
*     SKY_MODEL = REAL (Given)
*        Modeled sky intensities at offsets from trace.
*     SKY_MODEL_ERR = REAL (Given)
*        Modeled sky errors at offsets from trace.
*     ARC_FRAME = REAL (Given)
*        Input ARC frame image of dims nx columns and max_sky_pixels rows.
*     NBAD = INTEGER (Returned)
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

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     19-MAY-1997 (MJC):
*       Added prologue, some small improvements.
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
      LOGICAL CR_CLEAN
      INTEGER MAX_SKY_PIXELS
      REAL IMAGE( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL ARC_FRAME( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL READOUT
      REAL PHOTON
      LOGICAL GOT_QUALITY
      LOGICAL GOT_AN_ARC
      REAL IMAGE_ERR( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      BYTE QUALITY( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )

*  Arguments Returned:
      INTEGER NBAD
      REAL ARCSPEC( NX )
      REAL ARCSIG( NX )
      REAL SPEC( NX )
      REAL SPECSIG( NX )
      REAL SKY_MODEL( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL SKY_MODEL_ERR( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      INTEGER OBJ_MASK( -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL SKY_AVERAGE
      REAL VAR
      REAL VAR0
      REAL CMIN
      REAL DAT
      REAL STAR
      REAL SKY_ESTIMATE

      INTEGER I
      INTEGER SKY_COUNT
      INTEGER OBJ_COUNT
      INTEGER IPROFILE
      INTEGER YCOORD
      INTEGER OBJ_BELOW
      INTEGER IWAVE
      INTEGER OBJ_ABOVE

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :     CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Calculate minimum variance floor.
      VAR0 = READOUT * READOUT
      CMIN = ( V0_ALPHA * V0_BETA / ( V0_BETA - 1.0 ) ) ** 2.0 /
     :       PHOTON - VAR0 * PHOTON
      CMIN = MAX( 0.0, CMIN )

*  Determine object pixel offset limits.
      OBJ_BELOW = 999
      OBJ_ABOVE = -999
      DO I = -MAX_SKY_PIXELS / 2, MAX_SKY_PIXELS / 2
         IF ( OBJ_MASK( I ) .NE. 0 .AND. OBJ_BELOW .GT. I )
     :      OBJ_BELOW = I
         IF ( OBJ_MASK( I ) .NE. 0 .AND. OBJ_ABOVE .LT. I )
     :      OBJ_ABOVE = I
      END DO

*  Loop through increments along the order.
      DO IWAVE = NX, 1, -1

*     Clear totals.
         SKY_COUNT = 0
         OBJ_COUNT = 0
         SKY_AVERAGE = 0.0
         SPEC( IWAVE ) = 0.0
         SPECSIG( IWAVE ) = 0.0
         IF ( GOT_AN_ARC ) THEN
            ARCSPEC( IWAVE ) = 0.0
            ARCSIG( IWAVE ) = 0.0
         END IF

*     Loop through all contributing pixels in increment.
         DO IPROFILE = OBJ_BELOW, OBJ_ABOVE
            IF ( OBJ_MASK( IPROFILE ) .EQ. 1 ) THEN
               OBJ_COUNT = OBJ_COUNT + 1
               YCOORD = IPROFILE

*           Check pixel quality.
               IF ( GOT_QUALITY .AND. CR_CLEAN ) THEN
                  IF ( QUALITY( IWAVE, YCOORD ) .GT. 0 ) THEN
                     GO TO 100
                  END IF
               END IF

*           Check for BAD value pixel.
               IF ( IMAGE( IWAVE, YCOORD ) .EQ. ECH__BAD_REAL )
     :            GO TO 100

*           Calculate sky, object and variance for pixel.
               SKY_COUNT = SKY_COUNT + 1
               SKY_ESTIMATE = SKY_MODEL( IWAVE, IPROFILE )
               SKY_AVERAGE = SKY_AVERAGE + SKY_ESTIMATE
               DAT = IMAGE( IWAVE, YCOORD )
               STAR = DAT - SKY_ESTIMATE
               VAR = ( VAR0 + IMAGE_ERR( IWAVE, YCOORD ) +
     :               MAX( CMIN, ABS( DAT ), ABS( SKY_ESTIMATE ) ) /
     :               MAX( 1.0, PHOTON ) )

*           Add to running totals for object and arc.
               SPEC( IWAVE ) = SPEC( IWAVE ) + STAR
               SPECSIG( IWAVE ) = SPECSIG( IWAVE ) + VAR
               IF ( GOT_AN_ARC ) THEN
                  IF ( ARC_FRAME( IWAVE, YCOORD ) .NE. ECH__BAD_REAL )
     :                 THEN
                     ARCSPEC( IWAVE ) = ARCSPEC( IWAVE ) +
     :                     ARC_FRAME( IWAVE, YCOORD )
                  END IF
                  ARCSIG( IWAVE ) = SQRT( ABS( ARCSPEC( IWAVE ) ) )
               END IF
               GO TO 200

*           Increment bad pixel count.
  100          NBAD = NBAD + 1
  200          CONTINUE
            END IF
         END DO

*     Re-Normalise estimate to compensate for dead pixels.
         IF ( SKY_COUNT .GT. 0 ) THEN
            SPEC( IWAVE ) = SPEC( IWAVE ) / FLOAT( SKY_COUNT ) *
     :            FLOAT( OBJ_COUNT ) * PHOTON
            SPECSIG( IWAVE ) = ABS( SPECSIG( IWAVE ) /
     :            FLOAT( SKY_COUNT ) * FLOAT( OBJ_COUNT ) ) *
     :            PHOTON * PHOTON
            IF ( GOT_AN_ARC ) THEN
               ARCSPEC( IWAVE ) = ARCSPEC( IWAVE ) /
     :               FLOAT( SKY_COUNT ) * FLOAT( OBJ_COUNT ) * PHOTON
               ARCSIG( IWAVE ) = ARCSIG( IWAVE ) /
     :               FLOAT( SKY_COUNT ) * FLOAT ( OBJ_COUNT ) *
     :               PHOTON * PHOTON
            END IF

*     Else set variance negative.
         ELSE
            SPECSIG( IWAVE ) = -1.0
         END IF
      END DO

      END
