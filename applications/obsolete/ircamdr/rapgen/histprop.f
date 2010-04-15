
*+ HISTPROP - calculates statistics of image from its histogram

      SUBROUTINE HISTPROP( HIST, NUMBIN, VALMAX, VALMIN,
     :                     SUM, MEAN, MEDIAN, MODE, STATUS )

*    Description :
*
*       This routine calculates certain statistical parameters for
*     an image, given its intensity histogram, which should have
*     been previously calculated using GENHISSUB.
*
*    Parameters :
*
*     HIST( NUMBIN ) = INTEGER( READ )
*           Array containing the intensity histogram
*     NUMBIN = INTEGER( READ )
*           Number of bins used in the histogram
*     VALMAX = REAL( READ, WRITE )
*           Maximum intensity included in histogram
*     VALMIN = REAL( READ )
*           Minimum intensity included in histogram
*     SUM = REAL( WRITE )
*           Sum of all values in the image
*     MEAN = REAL( WRITE )
*           Mean intensity value in image
*     MEDIAN = REAL( WRITE )
*           Median intensity value in image
*     MODE = REAL( WRITE )
*           Mode of image
*
*    Method :
*
*    Bugs :
*
*     None are known at this time.
*
*    Authors :
*
*     Mark McCaughrean UOE (REVA::MJM)
*
*    History :
*
*     31-07-1985 : First SSE/ADAM implementation, with algorithm
*                : taken from Dennis Kelly's HISTPROP from Starlink.
*                : (REVA::MJM)
*
*    Type Definitions :

      IMPLICIT  NONE

*    Global constants :

      INCLUDE  'SAE_PAR'

*    Import :

      INTEGER
     :    NUMBIN,              ! number of bins used in histogram
     :    HIST( NUMBIN )       ! intensity histogram array

      REAL
     :    VALMAX,              ! maximum value in image
     :    VALMIN               ! minimum value in image

*    Export :

      REAL
     :    SUM,                 ! sum of all values in image
     :    MEAN,                ! mean of all values in image
     :    MEDIAN,              ! mean value in image
     :    MODE                 ! mode of image

*    Status :

      INTEGER  STATUS

*    Local variables :

      REAL
     :    NUMBER,              ! counter for number of points in image
     :    HALFNUM,             ! counter for middle point in image
     :    I, J                 ! general array counters

*-
*    Error check on entry - return if not ok

      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    firstly find sum and mean of data, being explicit with mixed
*    mode arithmetic conversions. If on a Vax, the Vax Fortran compiler
*    will insert all the required REALs etc.., but other machines
*    may not.

      NUMBER  =  0.0
      SUM     =  0.0

      DO  I  =  1, NUMBIN
         SUM  =  SUM + ( REAL( I ) * REAL( HIST( I ) ) )
         NUMBER  =  NUMBER + REAL( HIST( I ) )
      END DO

      SUM  =  ( NUMBER * VALMIN ) +
     :        ( SUM * ( VALMAX - VALMIN ) / NUMBIN )

      MEAN  =  SUM / NUMBER

*    Now find median and estimate mode. It is assumed that the
*    histogram bins are narrow enough that the error in calculating
*    the sum of the pixel values directly from the histogram bins
*    is negligible.
*    Median is found by looking for the halfway numbered pixel, and
*    calculating its intensity - i.e. if you have 101 pixels in the
*    histogram, the median value is the value assigned to the bin
*    in which the 51st ranked pixel sits (to the accuracy of one bin
*    width), and the mode will be calculated from the approximation :
*         mode = 3*median - 2*mean


      HALFNUM  =  0.0
      J  =  1

      DO WHILE( ( HALFNUM .LT. ( NUMBER / 2.0 ) ) .AND.
     :          ( J .LT. NUMBIN ) )
         HALFNUM  =  HALFNUM + HIST( J )
         J  =  J + 1
      END DO

      MEDIAN  =  VALMIN + ( J * ( VALMAX - VALMIN ) / NUMBIN )
      MODE    =  ( 3.0 * MEDIAN ) - ( 2.0 * MEAN )


*    end and return

      END
