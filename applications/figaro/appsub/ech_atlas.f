C+
      SUBROUTINE ECH_ATLAS(X_ARRAY,Z_ARRAY,XLABEL,YLABEL,NX,IORDR,
     :                  NLMAX,ORDER,CHANS,WAVES,WEIGHTS,CLASS,NLID)
C
C     E C H _ A T L A S
C
C     ECHARC utility.  Displays on the graphics device an exhaustive
C     set of ECH_ARPLOTs, one set for each order.
C
C     Parameters  -  (">" input, "<" output)
C
C     (>) X_ARRAY   (Real Array X_ARRAY(NX))  The X axis co-ordinates.
C     (>) Z_ARRAY   (Real Array Z_ARRAY(NX))  The Y axis co-ordinates.
C     (>) XLABEL    (Character) The X axis label for the plots.
C     (>) YLABEL    (Character) The Y axis label for the plots.
C     (>) NX        (Integer) The number of data points in the order.
C     (>) IORDR     (Integer) The order number of the order.
C     (>) NLMAX     (Integer) The maximum possible number of arc lines.
C     (>) ORDER     (Integer Array ORDER(NLMAX))  The order number of
C                   each of the identified arc lines.
C     (>) CHANS     (Real Array CHANS(NLMAX))  The x-centroids of each
C                   of the identified lines.
C     (>) WAVES     (Real Array WAVES(NLMAX))  The arc wavelengths
C                   assigned to each of the identified lines.
C     (>) WEIGHTS   (Real Array WEIGHTS(NLMAX))  The weights of each
C                   line to be fit.
C     (>) CLASS     (Integer Array CLASS(NLMAX)) The class codes of
C                   each of the identified arc lines.
C     (>) NLID      (Integer)  The number of identified arc lines.
C
C     Subroutines/functions used:
C
C       ICH_ENCODE  --  ICH_ pckg -- encodes a number into a character
C                       string in a convenient manner.
C       ECH_ARPLOT  --  (Figaro)  -- does all the hard stuff ...
C
C                                               -  JKM / ESO 25. Nov. 1987
C+
      IMPLICIT NONE
C
C     Functions
C
      INTEGER ICH_ENCODE
C
C     Parameters
C
      INTEGER NX,IORDR,NLMAX,NLID
      INTEGER ORDER(NLMAX),CLASS(NLMAX)
      REAL X_ARRAY(NX),Z_ARRAY(NX)
      REAL CHANS(NLMAX),WAVES(NLMAX),WEIGHTS(NLMAX)
      CHARACTER*(*) XLABEL,YLABEL
C
C     Local variables for the lines of a single order
C
      INTEGER NLOMX,NEWNL
      PARAMETER (NLOMX=100)
      INTEGER NEWCL(NLOMX)
      REAL NEWCH(NLOMX),NEWWV(NLOMX),NEWWT(NLOMX)
C
C     Local variables
C
      LOGICAL COMPLETE,XS
      INTEGER IXST,IXEN,NCHAN,I,INVOKE,NEXT,NCOEFF,NC
      PARAMETER (NCHAN=200,NC=4,NCOEFF=4)
      REAL HIGH,LOW
      DOUBLE PRECISION COEFFS(NC)
      CHARACTER*27 TLABEL
C
C     Initialize the Coeffs to 0.0D00 (not used by ECH_ARPLOT anyway
C
      DO I=1,NC,1
         COEFFS(I)=0.0D00
      END DO
C
C     Fill the NEWxx arrays with arc lines of the current order
C
      NEWNL=0
      IF (NLID.GT.0) THEN
         DO I=1,NLID,1
            IF (ORDER(I).EQ.IORDR) THEN
               NEWNL=NEWNL+1
               NEWCH(NEWNL)=CHANS(I)
               NEWWV(NEWNL)=WAVES(I)
               NEWWT(NEWNL)=WEIGHTS(I)
               NEWCL(NEWNL)=CLASS(I)
            END IF
         END DO
      END IF
C
C     Now plot these lines with the data, NCHAN at a time
C
      TLABEL=CHAR(92)//'(2248)'//CHAR(92)//'bechelle order '
      INVOKE=ICH_ENCODE(TLABEL,FLOAT(IORDR),24,0,NEXT)
      TLABEL(NEXT:27)=' '
      COMPLETE=.FALSE.
      XS=.FALSE.
      IXST=1
      DO WHILE(.NOT.COMPLETE)
         IXEN=IXST+NCHAN-1
         IF (IXEN.GT.NX) THEN
            IXEN=NX
            IXST=NX-NCHAN+1
         END IF
         CALL ECH_ARPLOT(X_ARRAY,Z_ARRAY,NX,IXST,IXEN,
     :                  XLABEL,YLABEL,TLABEL,NC,COEFFS,
     :                  NCOEFF,XS,NEWCH,NEWWV,NEWCL,NEWNL,
     :                  NLOMX,HIGH,LOW)
         IXST=IXST+NCHAN
         COMPLETE=IXST.GE.NX-20
      END DO
C
C     That's all there is to it ...
C
      RETURN
      END
