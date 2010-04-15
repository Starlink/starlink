
      SUBROUTINE PERIOD_STRING(XDATA, YDATA, NDATA, STRING, IFAIL)

C===========================================================================
C This routine calculates the string-length of XDATA(NDATA), YDATA(NDATA)
C following the method described by Dworetsky (1983, MNRAS, 203, 917) and
C Friend et al. (1990, MNRAS, 246, 637).
C
C Written by Vikram Singh Dhillon @Sussex 7-July-1992.
C
C Removed unused variables XMAX and XMIN - GJP June 1995
C
C Converted to Double Precision (KPD), August 2001
C===========================================================================

      IMPLICIT NONE

      INCLUDE "mnmxvl.h"

C-----------------------------------------------------------------------------
C PERIOD_STRING declarations.
C-----------------------------------------------------------------------------

      INTEGER NDATA, IFAIL, I
      DOUBLE PRECISION XDATA(NDATA), YDATA(NDATA)
      DOUBLE PRECISION STRING
      DOUBLE PRECISION YMAX, YMIN
      DOUBLE PRECISION XDIFF, YDIFF, LENGTH

C-----------------------------------------------------------------------------
C Find the maximum and minimum data points.
C-----------------------------------------------------------------------------

      YMAX = DNMX30
      YMIN = DPMX30
      DO 100 I = 1, NDATA
         IF ( YDATA(I).GT.YMAX ) YMAX = YDATA(I)
         IF ( YDATA(I).LT.YMIN ) YMIN = YDATA(I)
 100  CONTINUE

C-----------------------------------------------------------------------------
C If YMAX = YMIN (ie. if windowed data), report an error and abort.
C-----------------------------------------------------------------------------

      IF ( YMIN.EQ.YMAX ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR: YMIN .EQ. YMAX in PERIOD_STRING.'
         IF ( YMIN.EQ.1.0D0 ) WRITE (*, *)
     :                         '** ERROR: Cannot process windowed data.'
         IFAIL = 1
         GO TO 400
      END IF

C-----------------------------------------------------------------------------
C Scale all of the y-axis data points so that when determining the string
C length they will have equal weight to the phases.
C-----------------------------------------------------------------------------

      DO 200 I = 1, NDATA
         YDATA(I) = (YDATA(I)-YMIN)/(2.0D0*(YMAX-YMIN)) - 0.25D0
 200  CONTINUE

C-----------------------------------------------------------------------------
C Calculate the string-length.
C-----------------------------------------------------------------------------

      STRING = 0.0D0
      DO 300 I = 1, NDATA - 1

         YDIFF = YDATA(I+1) - YDATA(I)
         XDIFF = XDATA(I+1) - XDATA(I)
         LENGTH = DSQRT((XDIFF*XDIFF)+(YDIFF*YDIFF))
         STRING = STRING + LENGTH

 300  CONTINUE

C-----------------------------------------------------------------------------
C Complete the string length calculation by adding on a piece of string for
C the first and last observations.
C-----------------------------------------------------------------------------

      YDIFF = YDATA(1) - YDATA(NDATA)
      XDIFF = XDATA(1) + 1.0D0 - XDATA(NDATA)
      LENGTH = DSQRT((XDIFF*XDIFF)+(YDIFF*YDIFF))
      STRING = STRING + LENGTH

C-----------------------------------------------------------------------------
C And return.
C-----------------------------------------------------------------------------

      IFAIL = 0
 400  CONTINUE
      RETURN
      END
