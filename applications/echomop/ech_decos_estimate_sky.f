      SUBROUTINE ECH_DECOS_ESTIMATE_SKY(
     :           NX,
     :           NY,
     :           IMAGE,
     :           QUALITY,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           PROCESS,
     :           COUNT_PER_X,
     :           SKY_SPECTRUM,
     :           Y_TRACE_COORD,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_DECOS_ESTIMATE_SKY

*  Purpose:
*     Estimate sky intensity used by automatic cosmic-ray locator.

*  Description:
*     This routine quickly estmates the sky background intensity for use by
*     the automatic cosmic ray location function. It calculates the mean
*     background in all sky pixels in at each X-increment in an order.

*  Invocation:
*     CALL ECH_DECOS_ESTIMATE_SKY(
*     :    NX,
*     :    NY,
*     :    IMAGE,
*     :    QUALITY,
*     :    DEK_BELOW,
*     :    DEK_ABOVE,
*     :    PROCESS,
*     :    COUNT_PER_X,
*     :    SKY_SPECTRUM,
*     :    Y_TRACE_COORD,
*     :    STATUS
*     :   )

*  Arguments:
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     DEK_BELOW = INTEGER (Given)
*        Dekker distance below order traces.
*     DEK_ABOVE = INTEGER (Given)
*        Dekker distance above order traces.
*     SKY_SPECTRUM = REAL (Returned)
*        Sky spectrum estimates.
*     Y_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Y-coordinates of trace.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.
*     QUALITY = LOGICAL (Given)
*        Input data frame quality flags array.
*     PROCESS = LOGICAL (Given and Returned)
*        Set TRUE when increment is being processed.
*     COUNT_PER_X = INTEGER (Given and Returned)
*        Pixels used per increment.

*  Method:
*     Initialise sky spectrum
*     Loop thru increments from below to above order
*        If increment being processed then
*           Loop along trace in x coord
*              If pixel is on image AND still original data then
*                  Add to sky spectrum array
*              Endif
*           End loop
*        Endif
*     End loop
*     Loop thru sky spectrum array
*        If any contributing pixels to this bin, normalise it by number
*         Else flag bad value
*         Endif
*     End loop

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     17-JUL-1996 (MJC):
*       Added handling for possible BAD values in input image.
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
      BYTE QUALITY( NX, NY )
      INTEGER DEK_BELOW
      INTEGER DEK_ABOVE

*  Arguments Returned:
      INTEGER COUNT_PER_X( NX )
      LOGICAL PROCESS( -MAX_SLICE_PIXELS / 2 : MAX_SLICE_PIXELS / 2 )
*          ! TRUE if increment is being processed.
      REAL SKY_SPECTRUM( NX )

*  Workspace:
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I
      INTEGER IY
      INTEGER IY_DELTA

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Initialise sky spectrum.
      DO I = 1, NX
         SKY_SPECTRUM( I ) = 0.0
         COUNT_PER_X( I ) = 0
      END DO

*  Loop thru increments from below to above order.
      DO IY_DELTA = DEK_BELOW, DEK_ABOVE

*     If increment is being processed.
         IF ( PROCESS( IY_DELTA ) ) THEN

*        Loop along trace in x coord.
            DO I = 1, NX

*           If pixel is on image AND still original data then
               IY = INT( Y_TRACE_COORD( I ) + 0.5 ) + IY_DELTA

*           Add to sky spectrum array.
               IF ( IY .GT. 0 .AND. IY .LE. NY )  THEN
                  IF ( QUALITY( I, IY ) .EQ. 0 .AND.
     :                 IMAGE( I, IY ) .NE. ECH__BAD_REAL ) THEN
                     SKY_SPECTRUM( I ) = SKY_SPECTRUM( I ) +
     :                     IMAGE( I, IY )
                     COUNT_PER_X( I ) = COUNT_PER_X( I ) + 1
                  END IF
               END IF
            END DO
         END IF
      END DO

*  Loop through sky spectrum array.
      DO I = 1, NX

*     If any contributing pixels to this bin, normalise it by number.
         IF ( COUNT_PER_X( I ) .GT. 0 ) THEN
            SKY_SPECTRUM( I ) = SKY_SPECTRUM( I ) /
     :            FLOAT( COUNT_PER_X( I ) )

         ELSE
            SKY_SPECTRUM( I ) = ECH__BAD_REAL
         END IF
      END DO

      END
