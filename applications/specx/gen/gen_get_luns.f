
C--------------------------------------------------------------------------

      SUBROUTINE GEN_GET_LUNS

C   Routine to enable the logical unit numbers assumed by the FORLIB
C   routines to be determined

      INCLUDE 'LOGICAL_UNITS.INC'

      ENTRY GET_LUN_IN(L1)
C     --------------------

      L1 = 0
      IF (LUN_IN_SET.EQ.1) L1 = LUN_IN
      RETURN

      ENTRY GET_LUN_OUT(L2)
C     --------------------

      L2 = 0
      IF (LUN_OUT_SET.EQ.1) L2 = LUN_OUT
      RETURN

      ENTRY GET_LUN_ERROR(L3)
C     --------------------

      L3 = 0
      IF (LUN_ERROR_SET.EQ.1) L3 = LUN_ERROR
      RETURN

      END
