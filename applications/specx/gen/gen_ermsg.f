*  History:
*     16 Nov 1993 (hme):
*        Disuse VMS RTL stuff. Always write about unidentified internal
*        error.
*     02 Jan 1993 (rp):
*        Make it report error (so useful status is returned!)
*-----------------------------------------------------------------------

      SUBROUTINE GEN_ERMSG (IERR)

C  Routine to produce message after error transfer in I/O statements without
C  causing the program to crash. IERR is a dummy argument for compatibility
C  with NORD fortran as implemented at MRAO Cambridge, UK.

      IMPLICIT   NONE

*     Formal parameter:

      INTEGER   IERR

*     Local variables:

      INTEGER   MLEN
      CHARACTER MESSAGE*256

*     Functions:

      INTEGER   GEN_ILEN

*     Global variables:

      INTEGER   LUN_IN, LUN_OUT, LUN_ERROR
      INTEGER   LUN_IN_SET, LUN_OUT_SET, LUN_ERROR_SET
      COMMON /FORLUNS/ LUN_IN,LUN_IN_SET,LUN_OUT,LUN_OUT_SET,
     &                 LUN_ERROR,LUN_ERROR_SET

*  Ok, go...

      IF(LUN_ERROR_SET.EQ.0)   LUN_ERROR=6

      WRITE (MESSAGE, '('' --- FORTRAN i/o run-time error # '','  //
     &                'I4.1,'' ---'')') IERR
      MLEN = GEN_ILEN (MESSAGE)

C  Now put out message to terminal

      WRITE(LUN_ERROR, *) MESSAGE(:MLEN)

      RETURN
      END

*-----------------------------------------------------------------------
