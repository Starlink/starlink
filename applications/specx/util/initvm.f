*-----------------------------------------------------------------------

      BLOCK DATA INITVM

C     Define the virtual memory table and initialize

      INCLUDE       'VMTAB.INC'

      DATA VM_BYTES /MAXENT*0/
      DATA VM_PTR   /MAXENT*0/
      DATA VM_TIDY  /MAXENT*.FALSE./
      DATA VM_NAME  /MAXENT*' '/

      END

*-----------------------------------------------------------------------
