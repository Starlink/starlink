C-----------------------------------------------------------------------

      SUBROUTINE COMPRESSQ (NPTSNEW)

C   Routine to compress reduced quadrants to format in which all
C   channels are still valid

      INCLUDE   'STACKCOMM'
      INTEGER    NPTSNEW(8)

      NSTNEW=0
      DO NQ=1,NQUAD
        NST=NTOT(NQ-1)
        DO I=1,NPTSNEW(NQ)
          DATA(NSTNEW+I)=DATA(NST+I)
        END DO
        NSTNEW=NSTNEW+NPTSNEW(NQ)
      END DO

      DO NQ=1,NQUAD
       NPTS(NQ)=NPTSNEW(NQ)
      END DO

      RETURN
      END


