*  History:
*     17 Dec 1993 (hme):
*        In order to adapt to the new STACKCOMM, do not use TSYS in
*        EQUIVALENCE, since it is no longer at the beginning of the
*        common block.
C-----------------------------------------------------------------------

      SUBROUTINE STOREX (IREG)

C   Stores contents of X-register in storage register

      INCLUDE 'STACKCOMM'
      INCLUDE 'SPECX_PARS'
      INCLUDE 'STAKPAR'

      REAL*4      STACK(1)
      EQUIVALENCE (STACK(1),SCAN_HEADER(1))

      IF(IREG.GT.5.OR.IREG.LT.1) THEN
        TYPE *,'*** Illegal register: 1 .LE. IREG .LE. 5 ***'
        RETURN
      END IF

C  Store the header and data

      IWORDS=LHEAD+NTOT(NQUAD)
      DO I=1,IWORDS
        STORE(I,IREG)=STACK(I)
      END DO

C  Zero the remaining words

      IF(IWORDS.EQ.LSTK)   RETURN
      DO I=IWORDS+1,LSTK
        STORE(I,IREG)=0.0
      END DO

      RETURN
      END


