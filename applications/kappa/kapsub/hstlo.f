      SUBROUTINE HSTLO( HIST, NHIST, HMIN, HMAX, NULL, XLOG, YLOG,
     :                  YLOW, NPOINT, XLOC, YLOC, STATUS )
*+
*  Name:
*     HSTLO

*  Purpose:
*     Computes an histogram locus.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     SUBROUTINE

*  Invocation:
*     CALL HSTLO( HIST, NHIST, HMIN, HMAX, NULL, XLOG, YLOG, NPOINT,
*    :            XLOC, YLOC, STATUS )

*  Description:
*     This routine computes the locus of an histogram for later
*     plotting.  Either or both of the ordinate or abscissa may be on
*     a logarithmic scale.  Where a position becomes undefined, the
*     graphics-package's null value is substituted, e.g. with a
*     logarithmic ordinate an empty bin takes this value.  If the
*     abscissa is logarthmic and the lower limt is negative, rather than
*     using the data values, the locus is decribed in terms of histogram
*     bin numbers.

*  Arguments:
*     HIST( NHIST ) = INTEGER( READ )
*         This array contains the histogram.
*     NHIST = INTEGER( READ )
*         This is the number of bins in the histogram.
*     HMIN = REAL( READ )
*         This is the minimum value of the data used to create the
*           histogram.
*     HMAX = REAL( READ )
*         This is the maximum value of the data used to create the
*           histogram.
*     NULL = REAL( READ )
*         Null value used by graphics package
*     XLOG = LOGICAL( READ )
*         If true the x axis is logarithmic
*     YLOG = LOGICAL( READ )
*         If true the y axis is logarithmic
*     YLOW = REAL( READ )
*         The minimum y for a log plot, i.e. it is only used when %YLOG
*           is true.  It should take a value in the range 0.5 to 1.0.
*     NPOINT = INTEGER( READ )
*         The number of elements of each work array which must be at
*           least 2*(NHIST+1)+3*(NHIST-2).
*     XLOC( NPOINT ) = REAL( WRITE )
*         Work array for the x locus of the histogram
*     YLOC( NPOINT ) = REAL( WRITE )
*         Work array for the y locus of the histogram
*     STATUS = INTEGER( READ, WRITE )
*         This is the status value on entry to this subroutine.

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

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     Malcolm J. Currie  STARLINK (RAL::CUR)
*     {enter_new_authors_here}

*  History:
*     1989 Apr 23: Original (RAL::CUR).
*     {enter_further_changes_here}

*-

*  Type Definitions:

      IMPLICIT NONE            ! no default typing allowed

*  Global Constants:

      INCLUDE 'SAE_PAR'        ! SSE global definitions

*  Arguments Given:

      INTEGER
     :  NHIST,
     :  NPOINT,
     :  HIST( NHIST )

      LOGICAL
     :  XLOG, YLOG

      REAL
     :  HMIN, HMAX,
     :  NULL,
     :  YLOW

*  Arguments Returned:

      REAL
     :  XLOC( NPOINT ),
     :  YLOC( NPOINT )

*  Status:

      INTEGER STATUS

*  Local Variables:

      INTEGER
     :  I,                     ! general variable
     :  NLOCS                  ! running total of the number of points
                               ! in histogram locus

      REAL
     :  BINSZ,                 ! size of the bins
     :  YNULL                  ! bin height for an empty bin in a
                               ! histogram with a log ordinate
*.

*    If the status is bad, then return immediately

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( XLOG ) THEN

*       First the locus of the histogram outline is computed

         IF ( HMIN .LE. 0.0 ) THEN

*          Use bin numbers instead

            XLOC( 1 ) = 0.5
            DO  I = 2, 2*NHIST, 2
               XLOC( I ) = XLOC( I-1 )
               XLOC( I+1 ) = REAL( I/2 ) + 0.5
            END DO
         ELSE

*          Use data values

            XLOC( 1 ) = LOG10( HMIN )
            BINSZ = ( HMAX - HMIN ) / REAL( NHIST )
            DO  I = 2, 2*NHIST, 2
               XLOC( I ) = XLOC( I-1 )
               XLOC( I+1 ) = HMIN + BINSZ * REAL( I/2 )
            END DO
         END IF

*       Number of points defining the locus thus far

         NLOCS = 2 * ( NHIST + 1 )
         XLOC( NLOCS ) = HMAX

*       Define the locus of the vertical lines separating the boxes
*       from each other

         DO  I = 3, 2*NHIST - 1, 2
            XLOC( NLOCS+1 ) = NULL
            XLOC( NLOCS+2 ) = XLOC( I )
            XLOC( NLOCS+3 ) = XLOC( I )
            NLOCS = NLOCS + 3
         END DO
      ELSE

*       First the locus of the histogram outline is computed

         XLOC( 1 ) = HMIN
         BINSZ = ( HMAX - HMIN ) / REAL( NHIST )
         DO  I = 2, 2*NHIST, 2
            XLOC( I ) = XLOC( I-1 )
            XLOC( I+1 ) = HMIN + BINSZ * REAL( I/2 )
         END DO

*       Number of points defining the locus thus far

         NLOCS = 2 * ( NHIST + 1 )
         XLOC( NLOCS ) = HMAX

*       Define the locus of the vertical lines separating the boxes
*       from each other

         DO  I = 3, 2*NHIST - 1, 2
            XLOC( NLOCS+1 ) = NULL
            XLOC( NLOCS+2 ) = XLOC( I )
            XLOC( NLOCS+3 ) = XLOC( I )
            NLOCS = NLOCS + 3
         END DO
      END IF

*    Now for the y positions

      IF ( YLOG ) THEN

*       Set height for empty bins

         IF ( YLOW .LT. 0.5 .OR. YLOW .GT. 1.0 ) THEN
            YNULL = 0.8
         ELSE
            YNULL = YLOW
         END IF

*       First the locus of the histogram outline is computed
*       Note the null value must be used for empty bins, and the
*       initial value.

         YLOC( 1 ) = NULL
         DO  I = 2, 2*NHIST, 2
            IF ( HIST( I/2 ) .EQ. 0 ) THEN
               YLOC( I ) = YNULL
            ELSE
               YLOC( I ) = REAL( HIST( I/2 ) )
            END IF
            YLOC( I+1 ) = YLOC( I )
         END DO

*       Number of points defining the locus thus far

         NLOCS = 2 * ( NHIST + 1 )
         YLOC( NLOCS ) = NULL

*       Define the locus of the vertical lines separating the boxes
*       from each other

         DO  I = 3, 2*NHIST - 1, 2
            YLOC( NLOCS+1 ) = NULL
            YLOC( NLOCS+2 ) = YNULL
            YLOC( NLOCS+3 ) = MIN( YLOC( I ), YLOC( I+1 ) )
            NLOCS = NLOCS + 3
         END DO
      ELSE

*       First the locus of the histogram outline is computed

         YLOC( 1 ) = 0.0
         DO  I = 2, 2*NHIST, 2
            YLOC( I ) = REAL( HIST( I/2 ) )
            YLOC( I+1 ) = YLOC( I )
         END DO

*       Number of points defining the locus thus far

         NLOCS = 2 * ( NHIST + 1 )
         YLOC( NLOCS ) = 0.0

*       Define the locus of the vertical lines separating the boxes
*       from each other

         DO  I = 3, 2*NHIST - 1, 2
            YLOC( NLOCS+1 ) = NULL
            YLOC( NLOCS+2 ) = 0.0
            YLOC( NLOCS+3 ) = MIN( YLOC( I ), YLOC( I+1 ) )
            NLOCS = NLOCS + 3
         END DO
      END IF

      END
