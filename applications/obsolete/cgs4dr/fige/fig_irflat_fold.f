      SUBROUTINE FIG_IRFLAT_FOLD(DATA,QUALITY,NX,IXST,IXEN,DETS,SC,USES,
     :     NS,ND,SUM,NUM)
C
C     Fold data for all scan positions contained entirely within the
C     range IXST to IXEN into the array SUM(ND) where ND is the number
C     of detectors
C
C    (>) DATA      (Real array DATA(NX)) The input data array
C    (>) QUALITY   (Byte array QUALITY(NX)) The input quality array
C    (>) NX        (Integer) The number of data points
C    (>) IXST      (Integer) The first data value to use
C    (>) IXEN      (Integer) The last data value to use
C    (>) DETS      (Integer array DETS(NX)) The array of detector numbers
C    (>) SC        (Integer array SC(NX)) The array of scan position numbers
C    (>) USES      (Logical array USES(NS)) Workspace array of positions to use
C    (>) NS        (Integer) Number of scan positions
C    (>) ND        (Integer) Number of detector positions
C    (<) SUM       (Real array SUM(ND)) Array of folded data
C    (<) NUM       (Integer array NUM(ND)) Number of points in each element of
C                            SUM.
C
      IMPLICIT NONE
      INTEGER NX,IXST,IXEN,NS,ND
      REAL DATA(NX),SUM(ND)
      INTEGER DETS(NX),NUM(ND),SC(NX)
      LOGICAL USES(NS)
      BYTE QUALITY(NX)
C
      INTEGER IS,IX,ID
C
C     Set USES array to TRUE
C
      DO IS=1,NS
          USES(IS) = .TRUE.
      ENDDO
C
C     Set USES array to FALSE for all positions partially outside range
C
      DO IX=1,IXST
          USES(SC(IX))=.FALSE.
      ENDDO
      DO IX=IXEN,NX
          USES(SC(IX))=.FALSE.
      ENDDO
C
C     Fold remaining data
C
      DO IX=IXST,IXEN
          IF (USES(SC(IX)) .AND. (QUALITY(IX) .EQ. 0)) THEN
              ID = DETS(IX)
              SUM(ID) = SUM(ID)+DATA(IX)
              NUM(ID) = NUM(ID)+1
          ENDIF
      ENDDO
      END
