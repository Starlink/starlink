C-----------------------------------------------------------------------

      SUBROUTINE INITNPNEW (NPTSNEW)

C   Routine to initialize the array NPTSNEW with the # of data-points
C   in each quadrant of the current spectrum

      INCLUDE   'STACKCOMM'
      INTEGER    NPTSNEW(8)

      DO NQ=1,NQUAD
        NPTSNEW(NQ)=NPTS(NQ)
      END DO

      RETURN
      END


