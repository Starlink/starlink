*  History:
*     17 Dec 1993 (hme):
*        In order to adapt to the new STACKCOMM, use offest 110 (not
*        108) for scan number. There is also no need for a locally
*        declared and equivalenced ISTACK, when SCAN_HEADER is declared
*        in STACKCOMM. Also do not use TSYS in EQUIVALENCE, since it is
*        no longer at the beginning of the common block.
C-----------------------------------------------------------------------

      SUBROUTINE POP

      INCLUDE 'STACKCOMM'
      INCLUDE 'STAKPAR'
      REAL*4      STACK(1)
      EQUIVALENCE (STACK(1),SCAN_HEADER(1))

*     Type *,'-- Pop --'
*     Type *,'   XCLEAR, JTOP: ',XCLEAR,JTOP

C  Copy the spectra from higher positions to lower...

      DO I=1,JSTK-1
        DO J=1,LSTK
          STACK((I-1)*LSTK+J)=STACK(I*LSTK+J)
        END DO
      END DO

C  Make sure that flags indicate current state of stack

      JTOP = MAX (JTOP-1,0)
      IF (JTOP.EQ.0 .OR. LSCAN.LT.0) THEN
        XCLEAR = .TRUE.
      ELSE
        XCLEAR = .FALSE.
      END IF

C  Set all scan numbers negative for unfilled positions

      DO I=JTOP+1,JSTK
        SCAN_HEADER((I-1)*LSTK+110) = -1
      END DO

*     type *,'   on exit... ',XCLEAR, JTOP

      RETURN
      END


