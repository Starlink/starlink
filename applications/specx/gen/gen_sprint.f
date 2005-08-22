*  History:
*      1 Aug 2000 (ajc):
*        Test LUN_OUT_SET as INTEGER not LOGICAL
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
*-----------------------------------------------------------------------

      SUBROUTINE GEN_SPRINT (STRING, IERR)

*  Routine to print the value of a symbol. Wraparound routine for the
*  routine that actually prints it onto the character variable supplied.

      IMPLICIT  NONE

*     Formal parameter(s):

      INTEGER*4 IERR                ! Error return
      CHARACTER STRING*(*)          ! List of items to print

*     GENLIB unit numbers

      INCLUDE 'LOGICAL_UNITS.INC'

*     Local variable(s):

      INTEGER   LUN
      INTEGER   LOUT
      CHARACTER OUT_LINE*132

*  Ok, go...
      IERR = 0

*     Prepare the output string
      LOUT = 132
      CALL GEN_ENCODE (STRING, OUT_LINE, LOUT, IERR)
      IF (IERR.ne.0) RETURN

*     Print it to the output unit
      LUN = 6
      IF (LUN_OUT_SET.EQ.1) LUN = LUN_OUT
      WRITE (LUN, *) OUT_LINE(:LOUT)

CD    PRINT *, 'Output written to unit # ',LUN

      RETURN
      END
