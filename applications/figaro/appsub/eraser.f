C
      SUBROUTINE ERASER(N,X,Y,XVST,XVEN,HIGH,LOW,HIGHR,LOWR,TYP)
C
C     E R A S E R
C
C     Plots an array of points in PEN 0. The effect is to erase
C     a previous plot. The type of plot is given by TYP: TYP=1 -
C     a histo plot; TYP=2 a line plot; TYP=3 - a point plot.
C
C     Parameters - (">" input, "<" output )
C
C     (>) N     (Integer) Number of elements in X and Y
C     (>) X     (Real array) X values - abscissae
C     (>) Y     (Real array) Y values to be plotted
C     (>) XVST  (Real) The x-start value of the plot
C     (>) XVEN  (Real) The x-end value of the plot
C     (>) LOW   (Real) The minimum value for the lower plot
C     (>) HIGH  (Real) The maximum value for the lower plot
C     (>) LOWR  (Real) The minimum value for the upper plot
C     (>) HIGHR (Real) The maximum value for the upper plot
C     (>) TYP (Integer) Type of plot ( 1 - 3 )
C
C     Subroutines called:
C       PGPLOT routines
C
C                                          JRW / AAO February 1987
C     Modified:
C       Original
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER N,TYP
      REAL X(N),Y(N),XVST,XVEN,LOW,HIGH,LOWR,HIGHR
C
C     Local variables
C
      INTEGER THICK,THIN
      PARAMETER (THICK=2,THIN=1)

      CALL PGSCI(0)

      IF (TYP.EQ.1) THEN
C
C  Plot histo in pen 0 on lower plot
C
        CALL PGVPORT(0.08,0.97,0.09,0.74)
        CALL PGWINDOW(XVST,XVEN,LOW,HIGH)
        CALL PGSLW(THIN)
        CALL PGBIN(N,X,Y,.TRUE.)
      END IF

      IF (TYP.EQ.2) THEN
C
C  Plot line in pen 0 on lower plot
C
        CALL PGVPORT(0.08,0.97,0.09,0.74)
        CALL PGWINDOW(XVST,XVEN,LOW,HIGH)
        CALL PGSLW(THIN)
        CALL PGLINE(N,X,Y)
      END IF

      IF (TYP.EQ.3) THEN
        CALL PGVPORT(0.08,0.97,0.75,0.92)
        CALL PGWINDOW(XVST,XVEN,LOWR,HIGHR)
        CALL PGSLW(THIN)
C
C  Plot crosses in pen 0 on upper residuals plot ( no error bars )
C
        CALL PGPOINT(N,X,Y,5)
      END IF

      END
