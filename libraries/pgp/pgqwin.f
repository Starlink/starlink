C*PGQWIN -- inquire window boundary coordinates
C%void cpgqwin(float *x1, float *x2, float *y1, float *y2);
C+
      SUBROUTINE PGQWIN (X1, X2, Y1, Y2)
      REAL X1, X2, Y1, Y2
C
C Inquiry routine to determine the current window setting.
C The values returned are world coordinates.
C
C Arguments:
C  X1     (output) : the x-coordinate of the bottom left corner
C                    of the window.
C  X2     (output) : the x-coordinate of the top right corner
C                    of the window.
C  Y1     (output) : the y-coordinate of the bottom left corner
C                    of the window.
C  Y2     (output) : the y-coordinate of the top right corner
C                    of the window.
C--
C 26-Sep-1985 - new routine (TJP).
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
      X1 = PGXBLC(PGID)
      X2 = PGXTRC(PGID)
      Y1 = PGYBLC(PGID)
      Y2 = PGYTRC(PGID)
      END
