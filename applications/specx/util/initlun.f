*-----------------------------------------------------------------------

      BLOCK DATA INITLUN

C     Define the logical unit number table and initialize

      INCLUDE   'LUNTAB.INC'

      DATA      UNIT_NUMBER     /MAXENT*0/
      DATA      LUN_TIDY        /MAXENT*.FALSE./
      DATA      CALLING_ROUTINE /MAXENT*' '/

      END

*-----------------------------------------------------------------------
