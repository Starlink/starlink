
*-----------------------------------------------------------------------
C--------------------------------------------------------------------------

      SUBROUTINE GEN_SET_LUNS

C   Routine to enable the logical unit numbers assumed by the FORLIB
C   routines to be altered from their default values (in=5,out=error=6)

      INCLUDE 'LOGICAL_UNITS.INC'

      ENTRY SET_LUN_IN(L1)
C     --------------------
      LUN_IN     = L1
      LUN_IN_SET = 1
      RETURN

      ENTRY SET_LUN_OUT(L2)
C     --------------------
      LUN_OUT     = L2
      LUN_OUT_SET = 1
      RETURN

      ENTRY SET_LUN_ERROR(L3)
C     --------------------
      LUN_ERROR     = L3
      LUN_ERROR_SET = 1
      RETURN

      END
