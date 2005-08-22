*  History:
*     16 Nov 1993 (hme):
*        Replace LIB${GET|FREE}_LUN with FIO_{G|P}UNIT.
*     09 Jan 1994 (rp):
*        Replace FIO_ routines with UGET/UFREE
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused GEN_ILEN
*-----------------------------------------------------------------------

      SUBROUTINE GEN_UNWIND(ICLOSE)

C   Routine to unwind the stack when an @file is exhausted

      INTEGER*4 GEN_ICHTOT

      INTEGER STATUS

      INCLUDE 'CLI_STACK.INC'

      IF(ISP.EQ.0) RETURN

      LUN = ICLI(3,ISP)

      IF (ICLOSE.GE.1)   THEN
        CLOSE             (LUN)
        STATUS = 0
        CALL UFREELUN (LUN, STATUS)
        ICLI (2,ISP) = 0
        ICLI (3,ISP) = 0
        CLILINE (GEN_ICHTOT(ISP-1)+1:) = ' '
      END IF

      ISP = ISP - 1
      LUN = ICLI(3,ISP)
      CALL SET_LUN_IN (LUN)

CD    print *,'---------------------'
CD    print *,'Stack unwound, new stack pointer ',isp
CD    print *,' new LUN_IN ', icli(3,isp)
CD    print *,'...new length of CLI ',ICLI(2,ISP)
CD    print *,'---------------------'

      RETURN
      END
