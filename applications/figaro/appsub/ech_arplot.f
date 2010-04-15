C+
      SUBROUTINE ECH_ARPLOT(XVALS,ZVALS,NX,IXST,IXEN,XLAB,ZLAB,
     :                          TLAB,NC,COEFFS,NCOEFF,XS,CHANS,
     :                            WAVES,CLASS,NP,NMAX,HIGH,LOW)
C
C     E C H _ A R P L O T
C
C     ECHARC utility.  Displays on the graphics device a section of an
C     arc spectrum, autoscaled, with space at the top for interactive
C     line finding. Also displays any points already identified in this
C     range.  Uses PGPLOT routines, and assumes that PGBEGIN has
C     already been called.  Does not call PGEND.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) XVALS     (Real array XVALS(NX)) The x-values for the data.
C     (>) ZVALS     (Real array ZVALS(NX)) The data values.
C     (>) NX        (Integer) The dimension of the arrays.
C     (>) IXST      (Integer) The first element to be displayed.
C     (>) IXEN      (Integer) The last element to be displayed.
C     (>) XLAB      (Character) The x-label for the plot.
C     (>) ZLAB      (Character) The z-label for the plot.
C     (>) TLAB      (Character) The title for the plot.
C     (>) NC        (Integer)  The dimension of COEFFS array.
C     (>) COEFFS    (Double precision array COEFFS(NC)) The
C                   current wavelength coefficients.
C     (>) NCOEFF    (Integer) The number of coefficients used.
C     (>) XS        (Logical) True if identified lines are to be
C                   indicated only by an X instead of by wavelength.
C     (>) CHANS     (Real array CHANS(NMAX)) The channels of the
C                   identified lines so far.
C     (>) WAVES     (Real array WAVES(NMAX)) The wavelengths of the
C                   identified lines so far.
C     (>) CLASS     (Integer array CLASS(NMAX)) The class codes for
C                   the identified lines.
C     (>) NP        (Integer) The number of lines identified so far.
C     (>) NMAX      (Integer) The dimension of CHANS,WAVES,CLASS.
C     (<) HIGH      (Real) The high value used for the plot.
C     (<) LOW       (Real) The low value used for the plot.
C
C     Subroutines / functions used -
C
C     GEN_RANGEF    (GEN_ package) Find range of data in array.
C     ICH_ENCODE    (ICH_   "    ) Encode a number into a char string.
C     PGADVANCE     (PGPLOT) Erase screen for new plot.
C     PGASK         (  "   ) Set prompt state of PGPLOT.
C     PGWINDOW      (  "   ) Set the world-coordinate window.
C     PGVPORT       (  "   ) Set the view port.
C     PGPTEXT       (  "   ) Plot a text string.
C     PGPOINT       (  "   ) Mark a single point.
C     PGBOX         (  "   ) Draw axes for plot.
C     PGBIN         (  "   ) Plot data as a histogram.
C     PGLABEL       (  "   ) Label plot.
C
C     Originally ARPLOT:               KS / CIT 14th June 1983
C
C        Modified:
C
C        21st Nov 1984.  KS/AAO. Pathological test modified for case
C                        where HIGH=LOW<>0.
C        5th Sept 1985.  KS / AAO. CLASS parameter added.
C
C     Stolen & Modified --> ECH_ARPLOT:   JKM / CIT 11th Dec 1986
C
C+
      IMPLICIT NONE
C
C     Parameters
C     Note - CHANS etc dimensioned 1 rather than NP, since
C            NP can be 0.
C
      LOGICAL XS
      INTEGER NC,NMAX
      INTEGER NX,IXST,IXEN,NP,NCOEFF,CLASS(NMAX)
      REAL XVALS(NX),ZVALS(NX),CHANS(NMAX),WAVES(NMAX)
      DOUBLE PRECISION COEFFS(NC)
      CHARACTER*(*) XLAB,ZLAB,TLAB
C
C     Functions
C
      INTEGER I,ICH_ENCODE
C
C     Local variables
C
      LOGICAL WAVED
      INTEGER INVOKE,IX,NEXT
      REAL HIGH,LOW,XV,XVMAX,XVMIN,YOFF,YP
      CHARACTER CHARS*16
C
C     Autoscale data, allowing 10% extra at the top. (And allow for
C     the pathological case where there is no data at all!)
C     ECHARC ... ASSUMES LOW=0.0, ALLOWS 55% SINCE LABELS VERTICAL
C
      CALL GEN_RANGEF(ZVALS,IXST,IXEN,HIGH,LOW)
      LOW=0.0
      IF (HIGH.EQ.LOW) THEN
         HIGH=10.+LOW
         LOW=-10.+LOW
      END IF
      HIGH=HIGH+0.55*(HIGH-LOW)
      XVMIN=XVALS(IXST)
      XVMAX=XVALS(IXEN)
      YOFF=(HIGH-LOW)*0.05
C
C     Setup plot environment - note, not done using PGENV
C     because the wiewport is non-standard.
C
      CALL PGASK(.FALSE.)
      CALL PGADVANCE
      CALL PGVPORT(.1,.95,.1,.85)
      CALL PGWINDOW(XVMIN,XVMAX,LOW,HIGH)
      CALL PGBOX('ABCNST',0.,0,'ABCNST',0.,0)
      CALL PGLABEL(XLAB,ZLAB,TLAB)
C
C     Plot data
C
      CALL PGBIN(IXEN-IXST+1,XVALS(IXST),ZVALS(IXST),.TRUE.)
C
C     See if the XVALS represent pixels or wavelengths
C
      WAVED=XVALS(1).NE.1.
C
C     Indicate any lines identified in this range.
C
      IF (NP.GT.0) THEN
         DO I=1,NP
            IX=CHANS(I)
            IF (WAVED) THEN
               XV=XVALS(IX)+(CHANS(I)-FLOAT(IX))*
     :                                  (XVALS(IX+1)-XVALS(IX))
            ELSE
               XV=CHANS(I)
            END IF
            IF ((XV.GE.XVMIN).AND.(XV.LE.XVMAX)) THEN
               YP=MAX(ZVALS(IX-1),ZVALS(IX),ZVALS(IX+1))+YOFF
               IF (XS) THEN
                  IF (CLASS(I).EQ.0) THEN
                     CALL PGPOINT(1,XV,YP,ICHAR('X'))
                  ELSE
                     CALL PGPOINT(1,XV,YP,ICHAR('+'))
                  END IF
               ELSE
                  IF (CLASS(I).NE.0) THEN
                     CHARS='___ ('
                     INVOKE=ICH_ENCODE(CHARS,WAVES(I),6,6,NEXT)
                     CHARS(NEXT:NEXT)=')'
                  ELSE
                     CHARS='___ '
                     INVOKE=ICH_ENCODE(CHARS,WAVES(I),5,6,NEXT)
                     NEXT=NEXT-1
                  END IF
                  CALL PGPTEXT(XV,YP,90.0,0.0,CHARS(:NEXT))
               END IF
            END IF
         END DO
      END IF
C
      RETURN
      END
