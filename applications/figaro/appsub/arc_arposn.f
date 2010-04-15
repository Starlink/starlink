C+
      SUBROUTINE ARC_ARPOSN(XVALS,ZVALS,NX,IX,CENT,XCENT)
C
C     A R P O S N
C
C     Arc utility.  Lets the user indicate the center of a line
C     using the cursor, on an expanded plot.
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
C     PAR_WRUSER    (PAR_ package) Output string to user.
C
C                                               KS / CIT 12th Jan 1983
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
      INTEGER GEN_BSEARCH
C
C     Local variables
C
      INTEGER IXEN,IXST,KX,STATUS
      REAL LOW,HIGH,Y
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
C     Get cursor position of line center
C
      CALL PAR_WRUSER('Indicate line center with cursor',STATUS)
      XCENT=XVALS(IX)
      Y=(HIGH+LOW)*.5
      CALL PGCURSE(XCENT,Y,CH)
C
C     Get center in element numbers.
C
      KX=GEN_BSEARCH(XVALS(IXST),IXEN-IXST+1,XCENT)+IXST-1
      CENT=FLOAT(KX)+(XCENT-XVALS(KX))/(XVALS(KX+1)-XVALS(KX))
C
      RETURN
      END
