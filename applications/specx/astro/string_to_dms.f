C-----------------------------------------------------------------------

      SUBROUTINE STRING_TO_DMS (ASTRING, IDMS, IFAIL)

C  Routine to take input string and produce angle or time array
C  including sign (if negative) assuming that sign is given with first
C  non-zero array element.

      IMPLICIT NONE

C  Formal parameters
      CHARACTER ASTRING*(*)
      INTEGER*4 IDMS(4)
      INTEGER*4 IFAIL

C  Local variables
      LOGICAL*4 NEGATIVE
      INTEGER*4 FIRST
      INTEGER*4 I
      INTEGER*4 IERR
      INTEGER*4 IMINUS, IPLUS
      INTEGER*4 LSTR
      REAL*4    DMS(3)
      CHARACTER STRING*32

C  Functions
      INTEGER*4 GEN_ILEN

      IFAIL = 0

C  What string was it anyway?

      LSTR = GEN_ILEN (ASTRING)
*     TYPE *, 'Received string (length', LSTR, ') => ', ASTRING

C  Look for a sign

      IMINUS = INDEX (ASTRING, '-')
      IPLUS  = INDEX (ASTRING, '+')

      NEGATIVE = (IMINUS .NE. 0)

      FIRST  = MAX (IMINUS, IPLUS) + 1
      STRING = ASTRING(FIRST:LSTR)//' 0.0, 0.0, 0.0'
      READ (STRING, *, IOSTAT=IERR) DMS
      IF (IERR.NE.0) THEN
        IFAIL = 39
        RETURN
      END IF

      IDMS(1) = NINT (DMS(1))
      IDMS(2) = NINT (DMS(2))
      IDMS(3) = INT  (DMS(3))
      IDMS(4) = MOD  (NINT(100*DMS(3)), 100)

      IF (NEGATIVE) THEN
        I = 1
        DO WHILE (IDMS(I).EQ.0 .AND. I.LE.4)
          I = I+1
        END DO
        IDMS(I) = - IDMS(I)
      END IF

      RETURN
      END

C-----------------------------------------------------------------------

