C+
      SUBROUTINE ARC_ARCENTR(XVALS,ZVALS,NX,IX,CENT,XCENT)
C
C     A R C E N T R
C
C     Arc utility.  Determines the center of a line using a
C     center of gravity algorithm, with the user indicating the
C     limits to be used on an expanded plot.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) XVALS     (Real array XVALS(NX)) The x-values for the plot.
C     (>) ZVALS     (Real array ZVALS(NX)) The data values for the plot.
C     (>) NX        (Integer) The number of data points in the arc
C     (>) IX        (Integer) The element number about which the plot
C                   is to be expanded.
C     (<) CENT      (Real) The center of the plot - in pixel numbers.
C     (<) XCENT     (Real) the center of the plot - in x-values.
C
C     Subroutines used -
C
C     GEN_BSEARCH   (GEN_ package) Search for value in table
C     PGADVANCE     (PGPLOT) Clear plotting device
C     PGBOX         (  "   ) Draw plot axes
C     PGWINDOW      (  "   ) Set plotting window
C     PGBIN         (  "   ) Plot data as a histogram
C     PGCURSE       (  "   ) Read cursor position
C     PGPOINT       (  "   ) Plot a point
C     PAR_WRUSER    (PAR_ package) Output string to user.
C
C                                               KS / CIT 13th June 1983
C     Modified:
C
C     26th Jul 1993  HME / UoE, Starlink.  Disuse GKD_* and PAR_Q*.
C                    Use PAR_ABORT.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER NX,IX
      REAL XVALS(NX),ZVALS(NX),CENT,XCENT
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER GEN_BSEARCH
C
C     Local variables
C
      LOGICAL OK
      INTEGER I,IXEN,IXST,KX,KX1,KX2,STATUS
      REAL HIGH,LOW,SUMXY,SUMY,TEMP,X1,X2,Y1,Y2,YV
      CHARACTER CH
C
C     Clear screen and draw the expanded plot
C
      CALL PGADVANCE
      IXST=MAX(1,IX-15)
      IXEN=MIN(NX,IX+15)
      CALL GEN_RANGEF(ZVALS,IXST,IXEN,HIGH,LOW)
      CALL PGWINDOW(XVALS(IXST),XVALS(IXEN),LOW,HIGH*1.1)
      CALL PGBOX('ABCNST',0.,0,'ABCNST',0.,0)
      CALL PGBIN(IXEN-IXST+1,XVALS(IXST),ZVALS(IXST),.TRUE.)
C
C     Repeat until user is happy
C
      OK=.FALSE.
      DO WHILE (.NOT.OK)
C
C        Get cursor position of line limits
C
         CALL PAR_WRUSER(
     :            'Line center is the center of gravity of the line',
     :                                                        STATUS)
         CALL PAR_WRUSER(
     :            'over a range in X and above a certain level in Y',
     :                                                        STATUS)
         CALL PAR_WRUSER(
     :            'Select 2 points with the cursor giving the X range',
     :                                                        STATUS)
         CALL PAR_WRUSER('The lowest point gives the Y level',STATUS)
         X1=XVALS(IX)
         Y1=(HIGH+LOW)*.5
         CALL PGCURSE(X1,Y1,CH)
         CALL PGPOINT(1,X1,Y1,ICHAR('1'))
         Y2=Y1
         X2=X1
         CALL PGCURSE(X2,Y2,CH)
         CALL PGPOINT(1,X2,Y2,ICHAR('2'))
C
C        Sort in X and get lowest Y
C
         IF (X2.LT.X1) THEN
            TEMP=X1
            X1=X2
            X2=TEMP
         END IF
         Y1=MIN(Y1,Y2)
C
C        Get X's in element numbers.
C
         KX1=GEN_BSEARCH(XVALS(IXST),IXEN-IXST+1,X1)+IXST
         KX2=GEN_BSEARCH(XVALS(IXST),IXEN-IXST+1,X2)+IXST-1
C
C        Calculate center of gravity.
C
         SUMXY=0.
         SUMY=0.
         DO I=KX1,KX2
            IF (ZVALS(I).GT.Y1) THEN
               SUMXY=SUMXY+FLOAT(I-KX1)*(ZVALS(I)-Y1)
               SUMY=SUMY+ZVALS(I)-Y1
            END IF
         END DO
         IF (SUMY.LE.0.) THEN
            CALL PAR_WRUSER('No data in line',STATUS)
         ELSE
            CENT=FLOAT(KX1)+SUMXY/SUMY
            KX=CENT
            XCENT=XVALS(KX)+((XVALS(KX+1)-XVALS(KX))*(CENT-FLOAT(KX)))
            YV=MAX(ZVALS(KX),ZVALS(KX-1),ZVALS(KX+1))*1.02
            CALL PGPOINT(1,XCENT,YV,ICHAR('X'))
            CALL PAR_CNPAR('LINEOK')
            CALL PAR_RDKEY('LINEOK',.TRUE.,OK)
            IF (PAR_ABORT()) RETURN
         END IF
      END DO
C
      RETURN
      END
