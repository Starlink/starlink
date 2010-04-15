      SUBROUTINE GK2DLT(LTYPE)

      INCLUDE '../../include/check.inc'
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkio.par'
*
*         Selects appropriate linestyle
*
      INTEGER LINES(5),LTYPE,LIND,NLEFT
      CHARACTER*5 BUFFER
      DATA LINES /1,2,4,3,5/
*
      IF (LTYPE.GE.1 .AND. LTYPE.LE.5) THEN
         LIND=LINES(LTYPE)
      ELSE
         LIND=1
      ENDIF
*
      WRITE(BUFFER,100)LIND
  100 FORMAT('W(P',I1.1,')')
*
      CALL GKIOCO(KIOPB,BUFFER,NLEFT)
*
      RETURN
      END

