      SUBROUTINE KPG1_HSTLO( NHIST, HIST, HMIN, HMAX, NULL, XLOG,
     :                       YLOG, YLOW, NPOINT, XLOC, YLOC, STATUS )
*+
*  Name:
*     KPG1_HSTLO

*  Purpose:
*     Computes the locus of an histogram.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL KPG1_HSTLO( HIST, NHIST, HMIN, HMAX, NULL, XLOG, YLOG,
*                       NPOINT, XLOC, YLOC, STATUS )

*  Description:
*     This routine computes the locus of an histogram for later
*     plotting.  Either or both of the ordinate or abscissa may be on
*     a logarithmic scale.  Where a position becomes undefined, the
*     graphics-package's null value is substituted, e.g. with a
*     logarithmic ordinate an empty bin takes this value.  If the
*     abscissa is logarthmic and the lower limit is negative, rather
*     than using the data values, the locus is described in terms of
*     histogram bin numbers.

*  Arguments:
*     NHIST = INTEGER (Given)
*        The number of bins in the histogram.
*     HIST( NHIST ) = INTEGER (Given)
*        The histogram whose locus is to be found.
*     HMIN = REAL (Given)
*        The minimum data value that could be included within the
*        histogram.
*     HMAX = REAL (Given)
*        The maximum data value that could be included within the
*        histogram.
*     NULL = REAL (Given)
*        The null value used by graphics package that suppresses
*        plotting to a position.
*     XLOG = LOGICAL (Given)
*        If .TRUE., the x axis is logarithmic.
*     YLOG = LOGICAL (Given)
*        If .TRUE., the y axis is logarithmic.
*     YLOW = REAL (Given)
*        The minimum y for a log plot, i.e. it is only used when %YLOG
*        is true.  It should take a value in the range 0.5 to 1.0.
*     NPOINT = INTEGER (Given)
*         The number of elements of each work array which must be at
*         least 2*(NHIST+1)+3*(NHIST-2).
*     XLOC( NPOINT ) = REAL( WRITE )
*        Work array for the x locus of the histogram.
*     YLOC( NPOINT ) = REAL( WRITE )
*        Work array for the y locus of the histogram.
*     STATUS = INTEGER (Given and Returned)
*        This is the status value on entry to this subroutine.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     If abscissa is logarithmic then
*        If minimum value is zero or negative
*           Compute x locus of histogram outline in bin numbers
*        Else
*           Compute x locus of histogram outline in log data values
*        Endif
*        Define locus of the vertical lines separating the histogram
*          boxes from each other
*     Else
*        Compute x locus of histogram outline in data values
*        Define x locus of the vertical lines separating the histogram
*          boxes from each other
*     Endif
*     If ordinate is logarithmic then
*        Set height for empty bins --- y null value
*        Compute y locus of histogram outline in log histogram using
*          the y null value for empty bins
*        Define y locus of the vertical lines separating the histogram
*          boxes from each other using the null value for the base of
*          the plot and for points that would duplicate a vertical line
*     Else
*        Compute y locus of histogram outline in data values
*        Define y locus of the vertical lines separating the histogram
*          boxes from each other using zero for the base of the plot
*          and for points that would duplicate a vertical line
*     Endif
*     End

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1994 December 5 (MJC):
*        Original version based on HSTLO.
*     {enter_any_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions

*  Arguments Given:
      INTEGER NHIST
      INTEGER NPOINT
      INTEGER HIST( NHIST )

      LOGICAL XLOG
      LOGICAL YLOG

      REAL HMIN
      REAL HMAX
      REAL NULL
      REAL YLOW

*  Arguments Returned:
      REAL XLOC( NPOINT )
      REAL YLOC( NPOINT )

*  Status:
      INTEGER STATUS             ! Global status


*  Local Variables:
      REAL BINSZ                 ! Size of the bins
      INTEGER I                  ! General variable
      INTEGER NLOCS              ! Running total of the number of points
                                 ! in histogram locus
      REAL YNULL                 ! Bin height for an empty bin in a
                                 ! histogram with a log ordinate

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Deal with a logarithmic x-axis.
      IF ( XLOG ) THEN

*  First the locus of the histogram outline is computed.
         IF ( HMIN .LE. 0.0 ) THEN

*  Use bin numbers instead.
            XLOC( 1 ) = 0.5
            DO  I = 2, 2 * NHIST, 2
               XLOC( I ) = XLOC( I-1 )
               XLOC( I+1 ) = REAL( I/2 ) + 0.5
            END DO
         ELSE

*  Use the data values.  Start at the minimum value.  Finds the bin
*  size.
            XLOC( 1 ) = LOG10( HMIN )
            BINSZ = ( HMAX - HMIN ) / REAL( NHIST )

*  Define the x locus for each bin.  Two points per bin are needed.
            DO I = 2, 2 * NHIST, 2
               XLOC( I ) = XLOC( I - 1 )
               XLOC( I + 1 ) = HMIN + BINSZ * REAL( I / 2 )
            END DO
         END IF

*  Set the number of points defining the locus thus far.  Set the last
*  point to be the data maximum.
         NLOCS = 2 * ( NHIST + 1 )
         XLOC( NLOCS ) = HMAX

*  Define the locus of the vertical lines separating the boxes from
*  each other.
         DO  I = 3, 2 * NHIST - 1, 2
            XLOC( NLOCS + 1 ) = NULL
            XLOC( NLOCS + 2 ) = XLOC( I )
            XLOC( NLOCS + 3 ) = XLOC( I )
            NLOCS = NLOCS + 3
         END DO
      ELSE

*  Deal with a normal x axis.

*  First the locus of the histogram outline is computed.  Define the
*  bin size.
         XLOC( 1 ) = HMIN
         BINSZ = ( HMAX - HMIN ) / REAL( NHIST )

*  Define the x locus for the bins, one for each side of the bin.
         DO  I = 2, 2 * NHIST, 2
            XLOC( I ) = XLOC( I - 1 )
            XLOC( I + 1 ) = HMIN + BINSZ * REAL( I / 2 )
         END DO

*  Set the number of points defining the locus thus far.  Set the last
*  point to be the data maximum.
         NLOCS = 2 * ( NHIST + 1 )
         XLOC( NLOCS ) = HMAX

*  Define the locus of the vertical lines separating the boxes from
*  each other.
         DO  I = 3, 2 * NHIST - 1, 2
            XLOC( NLOCS + 1 ) = NULL
            XLOC( NLOCS + 2 ) = XLOC( I )
            XLOC( NLOCS + 3 ) = XLOC( I )
            NLOCS = NLOCS + 3
         END DO
      END IF

*  Now deal with logarithmic y positions.
      IF ( YLOG ) THEN

*  Set the height for empty bins.
         IF ( YLOW .LT. 0.5 .OR. YLOW .GT. 1.0 ) THEN
            YNULL = 0.8
         ELSE
            YNULL = YLOW
         END IF

*  First the locus of the histogram outline is computed.  Note the null
*  value must be used for empty bins, and the initial value.
         YLOC( 1 ) = NULL
         DO  I = 2, 2 * NHIST, 2
            IF ( HIST( I / 2 ) .EQ. 0 ) THEN
               YLOC( I ) = YNULL
            ELSE
               YLOC( I ) = REAL( HIST( I / 2 ) )
            END IF
            YLOC( I + 1 ) = YLOC( I )
         END DO

*  Set the number of points defining the locus thus far.  Set the last
*  point to be the null value.
         NLOCS = 2 * ( NHIST + 1 )
         YLOC( NLOCS ) = NULL

*  Define the locus of the vertical lines separating the boxes from
*  each other.
         DO  I = 3, 2 * NHIST - 1, 2
            YLOC( NLOCS + 1 ) = NULL
            YLOC( NLOCS + 2 ) = YNULL
            YLOC( NLOCS + 3 ) = MIN( YLOC( I ), YLOC( I + 1 ) )
            NLOCS = NLOCS + 3
         END DO
      ELSE

*  First the locus of the histogram outline is computed.
         YLOC( 1 ) = 0.0
         DO  I = 2, 2 * NHIST, 2
            YLOC( I ) = REAL( HIST( I / 2 ) )
            YLOC( I + 1 ) = YLOC( I )
         END DO

*  Set the number of points defining the locus thus far.  Set the last
*  point to be at the origin.
         NLOCS = 2 * ( NHIST + 1 )
         YLOC( NLOCS ) = 0.0

*  Define the locus of the vertical lines separating the boxes from
*  each other.
         DO I = 3, 2 * NHIST - 1, 2
            YLOC( NLOCS + 1 ) = NULL
            YLOC( NLOCS + 2 ) = 0.0
            YLOC( NLOCS + 3 ) = MIN( YLOC( I ), YLOC( I + 1 ) )
            NLOCS = NLOCS + 3
         END DO
      END IF

      END
