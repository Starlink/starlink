*  History:
*     17 Dec 1993 (hme):
*        The EQUIVALENCE with TSYS was unnecessary since the stack is
*        passed to another subroutine only anyway. Also no longer use
*        TSYS to locate the stack, since it is no longer at its beginning.
C-----------------------------------------------------------------------

      SUBROUTINE XY

C   Routine to swap data and header arrays in bottom two stack positions

      INCLUDE 'STACKCOMM'
      INCLUDE 'STAKPAR'

      LSCANX = LSCAN
      CALL EXCHNGE (1, 2, SCAN_HEADER, 4*LSTK)

      IF (JTOP.EQ.1) THEN
        JTOP = 2
      ELSE IF (JTOP.EQ.2 .AND. LSCANX.LT.0) THEN
        JTOP = 1
      END IF

      RETURN
      END


