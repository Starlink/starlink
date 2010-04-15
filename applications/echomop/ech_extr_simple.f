      SUBROUTINE ECH_EXTR_SIMPLE(
     :           NX,
     :           NY,
     :           GOT_QUALITY,
     :           GOT_AN_ARC,
     :           GOT_BALANCE,
     :           NO_ERRORS,
     :           OBJ_MASK,
     :           OBJ_BELOW,
     :           OBJ_ABOVE,
     :           READOUT,
     :           PHOTON,
     :           MAX_SKY_PIXELS,
     :           IMAGE,
     :           IMAGE_ERR,
     :           QUALITY,
     :           SKY_MODEL,
     :           SKY_MODEL_ERR,
     :           FLAT_MODEL,
     :           FLAT_MODEL_ERR,
     :           ARC_FRAME,
     :           NBAD,
     :           SPEC,
     :           SPECSIG,
     :           ARCSPEC,
     :           ARCSIG,
     :           Y_TRACE_COORD,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_EXTR_SIMPLE

*  Purpose:
*     Simple summed order extraction.

*  Description:
*     This routine extracts a the profile for a single increment in wavelength
*     along with raw intensities, errors, balances factors and arc intensity.

*  Method:
*     A simple summation of all 'good' pixels in the x-increment is used.

*  Invocation:
*     CALL ECH_EXTR_SIMPLE(
*    :     NX,
*    :     NY,
*    :     GOT_QUALITY,
*    :     GOT_AN_ARC,
*    :     GOT_BALANCE,
*    :     NO_ERRORS,
*    :     OBJ_MASK,
*    :     OBJ_BELOW,
*    :     OBJ_ABOVE,
*    :     READOUT,
*    :     PHOTON,
*    :     MAX_SKY_PIXELS,
*    :     IMAGE,
*    :     IMAGE_ERR,
*    :     QUALITY,
*    :     SKY_MODEL,
*    :     SKY_MODEL_ERR,
*    :     FLAT_MODEL,
*    :     FLAT_MODEL_ERR,
*    :     ARC_FRAME,
*    :     NBAD,
*    :     SPEC,
*    :     SPECSIG,
*    :     ARCSPEC,
*    :     ARCSIG,
*    :     Y_TRACE_COORD,
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
*     READOUT = REAL (Given)
*        Readout noise level in counts.
*     PHOTON = REAL (Given)
*        Photon to ADU conversion factor.
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum extent of sky and therefore dekker.
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
*     Y_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Y coordinates of order trace.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Calculate minimum variance floor
*     Determine object pixel offset limits
*      Loop through increments along the order
*         Clear totals
*         Loop through all contributing pixels in increment
*            If pixel is within frame boundaries and part of object then
*                  If ok to extract this pixel then
*                     Calculate sky, object and variance for pixel
*                     Modify variance estimate if actual balance factor available
*                     Add to running totals for object and arc
*                  Else increment 'hit' counter
*                  Endif
*            Endif
*         End loop
*         Re-Normalise estimate to compensate for dead pixels
*         Else set variance negative
*         Endif
*      End loop

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     04-JUL-1996 (MJC):
*       Fix for the 'jump' due to incorrect boundary handling.
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
      REAL ARC_FRAME( NX, NY )
      REAL READOUT
      REAL PHOTON
      LOGICAL GOT_QUALITY
      LOGICAL GOT_AN_ARC
      LOGICAL GOT_BALANCE
      LOGICAL NO_ERRORS
      REAL IMAGE_ERR( NX, NY )
      BYTE QUALITY( NX, NY )
      REAL FLAT_MODEL( NX, -MAX_SKY_PIXELS/2 : MAX_SKY_PIXELS/2 )
      REAL FLAT_MODEL_ERR( NX, -MAX_SKY_PIXELS/2 : MAX_SKY_PIXELS/2 )

*  Arguments Returned:
      INTEGER NBAD
      REAL ARCSPEC( NX )
      REAL ARCSIG( NX )
      REAL SPEC( NX )
      REAL SPECSIG( NX )
      REAL SKY_MODEL( NX, -MAX_SKY_PIXELS/2 : MAX_SKY_PIXELS/2 )
      REAL SKY_MODEL_ERR( NX, -MAX_SKY_PIXELS/2 : MAX_SKY_PIXELS/2 )

*  Workspace:
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL BALANCE
      REAL BALANCE_ERR
      REAL VAR
      REAL FRACTION
      REAL YFRAC
      REAL VAR0
      REAL INPUT_VAR
      REAL CMIN
      REAL STAR
      REAL MPHOTON

      INTEGER SKY_COUNT
      INTEGER OBJ_COUNT
      INTEGER IOFF
      INTEGER IOFFST
      INTEGER IOFFEN
      INTEGER YCOORD
      INTEGER IWAVE

*  Functions called:
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
      MPHOTON = MAX( 1.0, PHOTON )

*  Loop through increments along the order.
      DO IWAVE = NX, 1, -1

*     Clear totals.
         SKY_COUNT = 0
         OBJ_COUNT = 0
         SPEC( IWAVE ) = 0.0
         SPECSIG( IWAVE ) = 0.0
         IF ( GOT_AN_ARC ) THEN
            ARCSPEC( IWAVE ) = 0.0
            ARCSIG( IWAVE ) = 0.0
         END IF

*     Get fractional part of pixel for this trace point.
         YFRAC = Y_TRACE_COORD( IWAVE ) - FLOAT( INT(
     :           Y_TRACE_COORD( IWAVE ) + 0.5 ) )

*     Loop through all contributing pixels in increment.
         YCOORD = INT( Y_TRACE_COORD( IWAVE ) + 0.5 ) + OBJ_BELOW - 1
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
            IF ( OBJ_MASK( IOFF ) .EQ. 1 ) OBJ_COUNT = OBJ_COUNT + 1
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

*        If pixel is part of object process it.
            IF ( OBJ_MASK( IOFF ) .EQ. 1 .OR.
     :           IOFF .EQ. OBJ_BELOW - 1 .OR.
     :           IOFF .EQ. OBJ_ABOVE + 1 ) THEN

*           Check pixel quality.
               IF ( GOT_QUALITY ) THEN
                  IF ( QUALITY( IWAVE, YCOORD ) .GT. 0 ) THEN
                     GO TO 100
                  END IF
               END IF

*           Check for BAD value pixel.
               IF ( IMAGE( IWAVE, YCOORD ) .EQ. ECH__BAD_REAL )
     :            GO TO 100

               IF ( GOT_BALANCE ) THEN
                  BALANCE = FLAT_MODEL( IWAVE, IOFF )
                  BALANCE_ERR = FLAT_MODEL_ERR( IWAVE, IOFF )

               ELSE
                  BALANCE = 1.0
                  BALANCE_ERR = 0.0
               END IF

*           Calculate sky, object and variance for pixel.
               IF ( OBJ_MASK( IOFF ) .EQ. 1  ) THEN
                  SKY_COUNT = SKY_COUNT + 1
               END IF
               STAR = BALANCE * IMAGE( IWAVE, YCOORD ) -
     :                SKY_MODEL( IWAVE, IOFF )
               IF ( NO_ERRORS ) THEN
                  INPUT_VAR = 0.0

               ELSE
                  INPUT_VAR = IMAGE_ERR( IWAVE, YCOORD ) ** 2.0
               END IF
               VAR = BALANCE * ( BALANCE * ( VAR0 + INPUT_VAR )
     :               + MAX( ABS( STAR ) + CMIN, ABS( STAR ) +
     :               SKY_MODEL_ERR( IWAVE, IOFF ) ) / MPHOTON )

*           Modify variance estimate if balance factor is available.
               IF ( GOT_BALANCE .AND. BALANCE_ERR .GT. 0.0 )
     :              THEN
                  VAR = VAR +
     :                  ( BALANCE_ERR * IMAGE( IWAVE, YCOORD ) ) ** 2.0
               END IF

*           Add to running totals for object and arc.
               SPEC( IWAVE ) = SPEC( IWAVE ) + STAR * FRACTION
               SPECSIG( IWAVE ) = SPECSIG( IWAVE ) + VAR * FRACTION
               IF ( GOT_AN_ARC ) THEN
                  IF ( ARC_FRAME( IWAVE, YCOORD ) .NE.
     :                 ECH__BAD_REAL ) THEN
                     ARCSPEC( IWAVE ) = ARCSPEC( IWAVE ) +
     :                     BALANCE * ARC_FRAME( IWAVE, YCOORD )
                  END IF
                  ARCSIG( IWAVE ) = SQRT( ABS( ARCSPEC( IWAVE ) ) )
               END IF
               GO TO 200

*           Increment bad pixel count.
  100          NBAD = NBAD + 1
  200          CONTINUE
            END IF
            YCOORD = YCOORD + 1
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
     :                FLOAT( SKY_COUNT ) * FLOAT( OBJ_COUNT ) * PHOTON
                ARCSIG( IWAVE ) = ARCSIG( IWAVE ) /
     :                FLOAT( SKY_COUNT ) * FLOAT( OBJ_COUNT ) *
     :                PHOTON * PHOTON
            END IF

*     Else set variance negative.
         ELSE
            SPECSIG( IWAVE ) = -1.0
         END IF
      END DO

      END
