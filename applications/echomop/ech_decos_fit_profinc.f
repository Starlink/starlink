      SUBROUTINE ECH_DECOS_FIT_PROFINC(
     :           NX,
     :           NY,
     :           IMAGE,
     :           QUALITY,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           PROCESS,
     :           COUNT,
     :           ENERGY,
     :           MAXIMUM_POLY,
     :           FIT_POLY,
     :           FIT_ORDER,
     :           DATA,
     :           FRACTION,
     :           XAXIS,
     :           YAXIS,
     :           W_TO_FIT,
     :           Y_TRACE_COORD,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_DECOS_FIT_PROFINC

*  Purpose:
*     Fit spatial profile for automatic cosmic-ray locator.

*  Description:
*     This routine estimates the spatial profile used by the automatic cosmic
*     ray location algorithm. The estimate is based on a subsampled average
*     of the profile in each order and is re-evaluated aftwer each iterative
*     clip.

*  Invocation:
*     CALL ECH_DECOS_FIT_PROFINC(
*     :    NX,
*     :    NY,
*     :    IMAGE,
*     :    QUALITY,
*     :    DEK_BELOW,
*     :    DEK_ABOVE,
*     :    PROCESS,
*     :    COUNT,
*     :    ENERGY,
*     :    MAXIMUM_POLY,
*     :    FIT_POLY,
*     :    FIT_ORDER,
*     :    DATA,
*     :    FRACTION,
*     :    XAXIS,
*     :    YAXIS,
*     :    W_TO_FIT,
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
*     FIT_ORDER = INTEGER (Given)
*        Degree of polynomial to fit to fractions.
*     DATA = REAL (Temporary Workspace)
*        Ratios of observed/predicted energy.
*     YAXIS = REAL (Temporary Workspace)
*        Theoretical CDF values.
*     XAXIS = REAL (Temporary Workspace)
*        Theoretical data points.
*     Y_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Y-coordinates of trace.
*     QUALITY = LOGICAL (Given)
*        Input data frame quality flags array.
*     PROCESS = LOGICAL (Given and Returned)
*        Set TRUE when increment is being processed.
*     COUNT = INTEGER (Given)
*        Number of bytes to copy.
*     ENERGY = REAL (Given and Returned)
*        Total counts in an increment.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of fit coefficients allowed.
*     FIT_POLY = DOUBLE (Given and Returned)
*        Polynomial fits to object profile (workspace).
*     FRACTION = REAL (Given and Returned)
*        Fraction of energy used.
*     W_TO_FIT = REAL (Given and Returned)
*        Fit workspace.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Loop from below to above order by pixel increments
*        If this increment is being processed then
*           Loop along order in x dimension
*              If pixel is on image and still 'original' data then
*                 Add the pixels fractional energy at this x-slice of the order
*                 Sample pixels along the increment
*              Endif
*           End loop
*           If enough good pixels to attempt a fit then
*              Fit polynomial to pixel intensities along increment
*           Endif
*           If fractions to plot then
*              Plot graph of fractional pixel energy vs x position
*           Endif
*        Endif
*     End do

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     17-JUL-1996 (MJC):
*       Added handling for possible BAD values in IMAGE.
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
      REAL IMAGE( NX, NY )
      BYTE QUALITY( NX, NY )
      INTEGER DEK_BELOW
      INTEGER DEK_ABOVE
      INTEGER MAXIMUM_POLY
      INTEGER FIT_ORDER

*  Arguments Returned:
      REAL ENERGY( NX )
      REAL FRACTION( NX )
      REAL W_TO_FIT( NX )

*  Workspace:
      REAL DATA( NX * MAX_SLICE_PIXELS )  ! Ratios of obs./predicted energy.
      REAL YAXIS( NX * MAX_SLICE_PIXELS ) ! Theoretical CDF values.
      REAL XAXIS( NX * MAX_SLICE_PIXELS ) ! Theoretical data points.
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION FIT_POLY( MAXIMUM_POLY,
     :      -MAX_SLICE_PIXELS / 2 : MAX_SLICE_PIXELS / 2 )
*           ! Fitted polynomials modeling increments.
      INTEGER COUNT( -MAX_SLICE_PIXELS / 2 : MAX_SLICE_PIXELS / 2 )
*           ! Count of good pixels per order increment.
      INTEGER I
      INTEGER IY
      INTEGER IY_DELTA
      INTEGER PCOUNT

      LOGICAL PROCESS( -MAX_SLICE_PIXELS / 2 : MAX_SLICE_PIXELS / 2 )
*          ! TRUE if increment is being processed.

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Loop from below to above order by pixel increments.
      DO IY_DELTA = DEK_BELOW, DEK_ABOVE

*     If this increment is being processed then.
         IF ( PROCESS( IY_DELTA ) ) THEN
            COUNT( IY_DELTA ) = 0
            PCOUNT = 0

*        Loop along order in x dimension.
            DO I = 1, NX

*           If pixel is on image and still 'original' data then.
               IY = INT( Y_TRACE_COORD( I ) + 0.5 ) + IY_DELTA

*           Add the pixels fractional energy at this x-slice.
               IF ( IY .GT. 0 .AND. IY .LE. NY  ) THEN
                  IF ( QUALITY( I, IY ) .EQ. 0 .AND.
     :                 IMAGE( I , IY ) .NE. ECH__BAD_REAL ) THEN
                     COUNT( IY_DELTA ) = COUNT( IY_DELTA ) + 1
                     DATA( COUNT( IY_DELTA ) ) = FLOAT( I )
                     FRACTION( COUNT( IY_DELTA ) ) =
     :                     IMAGE( I, IY ) / MAX( 1., ENERGY( I ) )

*                 Sample pixels along the increment.
                     PCOUNT = PCOUNT + 1
                     XAXIS( PCOUNT ) = FLOAT( I )
                     YAXIS( PCOUNT ) = IMAGE( I, IY )
                     W_TO_FIT( PCOUNT ) = 1.0
                  END IF
               END IF
            END DO

*        If enough good pixels to attempt a fit then fit polynomial to
*        pixel intensities along increment.
            IF ( PCOUNT .GT. FIT_ORDER ) THEN
               CALL ECH_FITTER( 'REAL-POLY', FIT_ORDER,
     :              FIT_POLY( 1, IY_DELTA ), PCOUNT, XAXIS, YAXIS,
     :              W_TO_FIT, 0, 1000., STATUS )
               CALL ECH_FITTER( 'REAL-POLY', FIT_ORDER,
     :              FIT_POLY( 1, IY_DELTA ), COUNT( IY_DELTA ),
     :              DATA, FRACTION, W_TO_FIT, 0, 1000., STATUS )
            END IF
         END IF
      END DO

      END
