      SUBROUTINE ECH_GEN_FLAT(
     :           NX,
     :           NY,
     :           IMAGE,
     :           ERRORS,
     :           MAX_SKY_PIXELS,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           MAXIMUM_POLY,
     :           POLYNOMIALS,
     :           FLAT_MODEL,
     :           FLAT_MODEL_ERR,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_GEN_FLAT

*  Purpose:
*     Output "apflatten"-style flat-field image for use as prebalanced
*     flat field.

*  Description:
*     This routine copies flat-field balance factors from an ECHOMOP
*     reduction database file to an output image.  The resulting image
*     should then be useable as the ECHOMOP FFIELD with TUNE_PREBAL=TRUE.
*     In practice, however, this function is available to provide an
*     easy way to output balance factors for inspection.
*
*     It should be noted that the values of DEKABV and DEKBLW have a major
*     influence over the way this routine works.  If the dekker settings
*     are so wide that the part of an image selected as for one order
*     overlaps with the next order then, obviously, only one balance factor
*     can be written.

*  Invocation:
*     CALL ECH_GEN_FLAT(
*     :    NX,
*     :    NY,
*     :    IMAGE,
*     :    N_ORDERS,
*     :    MAX_SKY_PIXELS,
*     :    DEK_BELOW,
*     :    DEK_ABOVE,
*     :    MAXIMUM_POLY,
*     :    POLYNOMIALS,
*     :    FLAT_MODEL,
*     :    FLAT_MODEL_ERR,
*     :    X_TRACE_COORD,
*     :    Y_TRACE_COORD,
*     :    STATUS
*     :   )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     IMAGE = REAL( NX, NY ) (Returned)
*        Input frame image of dimensions nx columns and ny rows.
*     DEK_BELOW = INTEGER( N_ORDERS ) (Given)
*        Lower dekker offset in pixels.
*     DEK_ABOVE = INTEGER( N_ORDERS ) (Given)
*        Upper dekker offset in pixels.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of poly coeffs allowed.
*     POLYNOMIALS = DOUBLE( MAXIMUM_POLY ) (Given)
*        Polynomial coeffs describing order trace.
*     FLAT_MODEL = REAL (Given)
*        Modelled balance factors.
*     FLAT_MODEL_ERR = REAL (Given)
*        Variances on  Modelled balance factors.
*     X_TRACE_COORD = DOUBLE PRECISION (Temporary Workspace)
*        X coordinates of order trace.
*     Y_TRACE_COORD = DOUBLE PRECISION (Temporary Workspace)
*        Y coordinates of order trace.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     MJC: Martin Clayton (Starlink, UCL)

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER MAX_SKY_PIXELS
      REAL IMAGE( NX, NY )
      REAL ERRORS( NX, NY )
      INTEGER DEK_BELOW       ! ( N_ORDERS )
      INTEGER DEK_ABOVE       ! ( N_ORDERS )
      INTEGER MAXIMUM_POLY
      DOUBLE PRECISION POLYNOMIALS( MAXIMUM_POLY ) ! , N_ORDERS )
      REAL FLAT_MODEL( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL FLAT_MODEL_ERR( NX, -MAX_SKY_PIXELS / 2: MAX_SKY_PIXELS / 2 )

*  Workspace:
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I
      INTEGER IOFF
      INTEGER IWAVE
      INTEGER YCOORD

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Work through orders.
      DO I = 1, 1

*     Do nothing if order is disabled.
         IF ( POLYNOMIALS( 1 ) .EQ. ECH__BAD_DOUBLE ) THEN
            GO TO 100
         END IF

*     Calculate order trace path across frame.
         CALL ECH_CALC_TRACE( NX, MAXIMUM_POLY, POLYNOMIALS,
     :      X_TRACE_COORD, Y_TRACE_COORD, STATUS )

*     Copy balance factors into output frame - don't overwrite
*     a non-unity value.
         DO IWAVE = 1, NX
            DO IOFF = DEK_BELOW, DEK_ABOVE
               YCOORD = INT( Y_TRACE_COORD( IWAVE ) + 0.5 ) + IOFF
               IF ( YCOORD .GE. 1 .AND. YCOORD .LE. NY ) THEN
                  IMAGE( IWAVE, YCOORD ) = FLAT_MODEL( IWAVE, IOFF )
*                  ERRORS( IWAVE, YCOORD ) =
*     :                  FLAT_MODEL_ERR( IWAVE, IOFF )
               END IF
            END DO
         END DO
  100    CONTINUE
      END DO

      END


      SUBROUTINE ECH_UNITY_FLAT(
     :           NX,
     :           NY,
     :           IMAGE,
     :           ERRORS,
     :           STATUS
     :          )
*+

*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     IMAGE = REAL( NX, NY ) (Returned)
*        Input frame image of dimensions nx columns and ny rows.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*-
      IMPLICIT NONE

      INTEGER NX
      INTEGER NY
      REAL IMAGE( NX, NY )
      REAL ERRORS( NX, NY )
      INTEGER STATUS

      INTEGER IX
      INTEGER IY

*  Initialise flat-field balance model to unity everywhere.
      DO IX = 1, NX
         DO IY = 1, NY
            IMAGE( IX, IY ) = 1.0
*            ERRORS( IX, IY ) = 0.0
         END DO
      END DO

      END
