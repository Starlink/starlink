*-----------------------------------------------------------------------
*     luntab.inc
*     --- defines logical unit number table for LUN_HNDL routines
*
*-----------------------------------------------------------------------

      INTEGER    MAXENT
      PARAMETER (MAXENT=20)

      INTEGER   UNIT_NUMBER
      LOGICAL   LUN_TIDY
      CHARACTER CALLING_ROUTINE*24

      COMMON /LUN_TABLE/ UNIT_NUMBER(MAXENT),
     &                   LUN_TIDY(MAXENT),
     &                   CALLING_ROUTINE(MAXENT)

*-----------------------------------------------------------------------
