*  History:
*     17 Dec 1993 (hme):
*        In order to adapt to the new STACKCOMM, use offest 110 (not
*        108) for scan number. There is also no need for a locally
*        declared and equivalenced ISTACK, when SCAN_HEADER is declared
*        in STACKCOMM. Also do not use TSYS in
*        EQUIVALENCE, since it is no longer at the beginning of the
*        common block.
C-----------------------------------------------------------------------

      SUBROUTINE ROLL (BUF)

C   Routine to rotate spectra around stack positions.
C   Note that this implies that JTOP will be set to JSTK afterwards
C   (unless the X-register was a "blank" spectrum)
C   as the T-register will now contain the old X-register.
C   User may need to clear and/or reset JTOP explicitly for non-standard
C   use of this subroutine.

      REAL*4  BUF(1)

      INCLUDE 'STACKCOMM'
      INCLUDE 'STAKPAR'

      REAL*4      STACK(1)
      EQUIVALENCE (STACK(1),SCAN_HEADER(1))

      DO I=1,LSTK
        BUF(I)=STACK(I)
      END DO

      CALL POP

      DO I=1,LSTK
        STACK((JSTK-1)*LSTK+I)=BUF(I)
      END DO

      JTOP = JSTK
      DO I = 1,JSTK
        KSCAN = SCAN_HEADER((JSTK-I)*LSTK+110)
        IF (KSCAN.LT.0) THEN
          JTOP  = JTOP - 1
        ELSE
          GO TO 99
        END IF
      END DO

   99 CONTINUE

      RETURN
      END


