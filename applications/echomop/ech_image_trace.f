      SUBROUTINE ECH_IMAGE_TRACE( NX, NY, N_ORDERS, IMAGE, MAXIMUM_POLY,
     :           POLYNOMIALS, IMAGE2, X_TRACE_COORD, Y_TRACE_COORD,
     :           STATUS )
*+
*  Name:
*     ECHOMOP - ECH_IMAGE_TRACE

*  Purpose:
*     Make a copy of object frame with order traces flagged.

*  Description:
*     This routine copies the input frame to the output frame and adds in
*     the paths of any traced orders in contrasting pixel values.

*  Invocation:
*     CALL ECH_IMAGE_TRACE( NX, NY, N_ORDERS, IMAGE, MAXIMUM_POLY,
*    :     POLYNOMIALS, IMAGE2, X_TRACE_COORD, Y_TRACE_COORD,
*    :     STATUS )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum degree of trace polynomials.
*     POLYNOMIALS = DOUBLE (Given)
*        Polynomials for trace fits.
*     IMAGE2 = REAL (Returned)
*        Output frame image of dimensions nx columns and ny rows.
*     X_TRACE_COORD = DOUBLE (Temporary Workspace)
*        X coordinates along trace.
*     Y_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Y coordinates along trace.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Copy original image into new one
*     Loop thru traced orders
*       Calculate coordinates of path of trace on image
*       If trace is OK then
*          Loop thru x direction along trace
*               If pixel value is low, replace with 1000.
*               If pixel value is high, replace with 0.
*          End loop
*       Endif
*     End loop

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1: Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER N_ORDERS
      REAL IMAGE( NX, NY )
      INTEGER MAXIMUM_POLY
      DOUBLE PRECISION POLYNOMIALS( MAXIMUM_POLY, N_ORDERS ) ! Trace polynomials.
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Arguments Returned:
      REAL IMAGE2( NX, NY )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I
      INTEGER IX
      INTEGER IY
      INTEGER IORD

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  Check inherited Global status.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( report_mode, rpm_full + rpm_calls ) .GT. 0 )
     :   CALL ECH_REPORT( report_mode, ECH__MOD_ENTRY )

*  Copy original image into new one.
      DO IY = 1, NY
         DO IX = 1, NX
            IMAGE2( IX, IY ) = IMAGE( IX, IY )
         END DO
      END DO

*  Loop through traced orders.
      DO IORD = 1, N_ORDERS
         IF ( POLYNOMIALS( 1, IORD ) .NE. ECH__BAD_DOUBLE ) THEN

*        Calculate coordinates of path of trace on image.
            CALL ECH_CALC_TRACE( NX, MAXIMUM_POLY,
     :           POLYNOMIALS( 1, IORD ), X_TRACE_COORD, Y_TRACE_COORD,
     :           STATUS )
            IF ( STATUS .EQ. 0 ) THEN

*        "Hilight" the trace path in the image.
               DO I = 1, NX
                  IY = INT( Y_TRACE_COORD( I ) + 0.5 )
                  IF ( IY .GT. 0 .AND. IY .LE. NY ) THEN
                     IF ( IMAGE( I, IY ) .LT. 10.0 ) THEN
                        IMAGE2( I, IY ) = 1000.0

                     ELSE
                        IMAGE2( I, IY ) = 0.0
                     END IF
                  END IF
               END DO
            END IF
         END IF
      END DO

      END
