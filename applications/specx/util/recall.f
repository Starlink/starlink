*  History:
*     17 Dec 1993 (hme):
*        In order to adapt to the new STACKCOMM, do not use TSYS in
*        EQUIVALENCE, since it is no longer at the beginning of the
*        common block.
C-----------------------------------------------------------------------

      SUBROUTINE RECALL (IREG)

C   Retrieves contents of storage register(IREG) and places in stack.

      INCLUDE 'STACKCOMM'
      INCLUDE 'SPECX_PARS'
      INCLUDE 'STAKPAR'

      REAL*4      STACK(1)
      EQUIVALENCE (STACK(1),SCAN_HEADER(1))

      IF(IREG.GT.5.OR.IREG.LT.1) THEN
        TYPE *,'*** Illegal register: 1 .LE. IREG .LE. 5 ***'
        RETURN
      END IF

      CALL PUSH
      IF (JTOP.EQ.0) JTOP=1

C  Recall the header

      DO I=1,LHEAD
        STACK(I)=STORE(I,IREG)
      END DO

C  Recall the data up to the maximum allowed stack length

      IWORDS=NTOT(NQUAD)+LHEAD
      IF(IWORDS.GT.LSTK)   IWORDS=LSTK
      DO I=LHEAD+1,IWORDS
        STACK(I)=STORE(I,IREG)
      END DO

      XCLEAR = .FALSE.

      RETURN
      END


