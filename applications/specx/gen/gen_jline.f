
*-----------------------------------------------------------------------

      SUBROUTINE GEN_JLINE (STRING)

*  Routine to write next line of input from terminal or wherever to journal
*  file.

      IMPLICIT  NONE

*  Formal parameters:

      CHARACTER STRING*(*)

*  Common blocks

      LOGICAL*4 JON
      INTEGER*4 JLUN
      COMMON /GEN_JOURNAL/ JON, JLUN

*  Functions

      INTEGER*4 GEN_ILEN

*  Local variables

      INTEGER*4 ILS

      IF (.NOT.JON) RETURN

      ILS = GEN_ILEN (STRING)
      IF (ILS.EQ.0) THEN
        STRING='/'
        ILS = 1
      END IF

      WRITE (JLUN,'(A)') STRING(1:ILS)

      RETURN
      END
