*  History:
*     17 Dec 1993 (hme):
*        In order to adapt to the new STACKCOMM, do not use TSYS in
*        EQUIVALENCE, since it is no longer at the beginning of the
*        common block.
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
C-----------------------------------------------------------------------

      SUBROUTINE PUSH

      REAL*4   STACK(1)

      INCLUDE 'STAKPAR'
      INCLUDE 'STACKCOMM'

      EQUIVALENCE (STACK(1),SCAN_HEADER(1))

*     Print *,'-- Push --'
*     Print *,'  XCLEAR, JTOP: ',XCLEAR,JTOP

      IF (JTOP.EQ.0) RETURN

      N = MIN (JTOP,JSTK-1)
      DO I=1,N
        NS=N+1-I
        DO J=1,LSTK
          STACK(NS*LSTK+J)=STACK((NS-1)*LSTK+J)
        END DO
      END DO

      JTOP = N+1

      RETURN
      END


