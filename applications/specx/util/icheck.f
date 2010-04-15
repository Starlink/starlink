*  History:
*     17 Dec 1993 (hme):
*        In order to adapt to the new STACKCOMM, use offest 110 (not
*        108) for scan number. There is also no need for a locally
*        declared and equivalenced ISTACK, when SCAN_HEADER is declared
*        in STACKCOMM.
C-----------------------------------------------------------------------

      INTEGER FUNCTION ICHECK (NSTACK, IERR)

C   Routine to check that there are at least NSTACK spectra in the
C   stack, so that program does not attempt operations on undefined
C   variables, which may lead to crashes
C   Returns a value 1 if OK, 0 otherwise

      INCLUDE 'STACKCOMM'
      INCLUDE 'STAKPAR'

      ICHECK = 1
      IERR   = 0

      IF (NSTACK.EQ.0) RETURN

      IF (JTOP.EQ.0) THEN
        ICHECK = 0
        IERR   = 8
      END IF

C     Also check that the scan number is not negative (LSCAN).
C     (I have forgotten why this is included, but it must be
C     so that a stack rolled when XCLEAR is 0 does not give
C     a spurious indication of enough spectra).

      DO J = 1,NSTACK
        IF (SCAN_HEADER((J-1)*LSTK+110) .LT. 0)   THEN
          ICHECK = 0
          IERR   = 8
        ENDIF
      END DO

      RETURN
      END
