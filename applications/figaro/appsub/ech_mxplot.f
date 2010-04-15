C+
      SUBROUTINE ECH_MXPLOT(XVAL,MVAL,NTOT,NIDS,NX,M1,M2)
C
C     E C H _ M X P L O T
C
C     ECHARC utility.  Displays on the graphics device an overview of
C     the locations of all arc lines found.  The purpose is to check
C     whether or not ECH_ARFIND commonly identified bad columns and/or
C     saturated columns as arc lines.  ECH_MXPLOT uses PGPLOT routines,
C     and it assumes PGPBEGIN has already been called.  It calls PGEND.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) XVAL      (Real array XVAL(NTOT)) The x-locations of the
C                   lines, assumed to be channel numbers.
C     (>) ZVAL      (Real array MVAL(NTOT)) The y-locations of the
C                   lines, assumed to be order numbers.
C     (>) NTOT      (Integer) The number of lines possible.
C     (>) NIDS      (Integer) The number of lines actually identified.
C     (>) NX        (Integer) The maximum x-value.
C     (>) M1        (Integer) The minimum y-value.
C     (>) M2        (Integer) The maximum y-value.
C
C     Subroutines / functions used -
C
C     PGENV         (PGPCKG) Sets up the plot environment.
C     PGLABEL       (  "   ) Labels plot.
C     PGLINE        (  "   ) Draws lines between given endpoints.
C
C                                           -  JKM / ESO 25. Nov. 1987
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NTOT,NIDS,NX,M1,M2
      INTEGER MVAL(NTOT)
      REAL    XVAL(NTOT)
C
C     Local variables
C
      INTEGER I,M,DM
      REAL XPLOT(2),YPLOT(2)
C
C     Lets start with the plot environment
C
      IF (M1.LT.M2) THEN
         DM=1
      ELSE
         DM=-1
      END IF
      CALL PGENV(1.0,FLOAT(NX),
     :           FLOAT(M1)-0.5*DM,FLOAT(M2)+0.5*DM,0,0)
      CALL PGLABEL('X Pixel #','Order Number',
     :           'ECHARC Detected Line Locations')
C
C     Draw horizontal lines to define the orders
C
      DO M=M1+DM,M2,DM
C
         XPLOT(1)=1.0
         XPLOT(2)=FLOAT(NX)
         YPLOT(1)=FLOAT(M)-0.5*DM
         YPLOT(2)=YPLOT(1)
         CALL PGLINE(2,XPLOT,YPLOT)
C
      END DO
C
C     Now draw the arc lines in
C
      DO M=M1,M2,DM
C
         YPLOT(1)=FLOAT(M)-0.5*DM
         YPLOT(2)=FLOAT(M)+0.5*DM
C
         DO I=1,NIDS,1
            IF (MVAL(I).EQ.M) THEN
               XPLOT(1)=XVAL(I)
               XPLOT(2)=XPLOT(1)
               CALL PGLINE(2,XPLOT,YPLOT)
            END IF
         END DO
C
      END DO
C
C     Now close down the plot device as promised ...
C
      CALL PGEND
C
C     ... and return to the calling program ...
C
      RETURN
      END
