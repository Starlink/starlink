C-----------------------------------------------------------------------
C     Define the virtual memory table
C     -- include file vmtab.inc
C-----------------------------------------------------------------------

      INTEGER    MAXENT
      PARAMETER (MAXENT=32)

      INTEGER    VM_BYTES
      INTEGER    VM_PTR
      LOGICAL    VM_TIDY
      CHARACTER  VM_NAME*24
      COMMON /VM_TABLE/ VM_BYTES(MAXENT), VM_PTR(MAXENT),
     &                  VM_TIDY(MAXENT),  VM_NAME(MAXENT)

C-----------------------------------------------------------------------
