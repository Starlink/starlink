*-----------------------------------------------------------------------
*   scl_do.inc
*   Include file to define the DO loop limits and increments.
*   Name deliberately not underscored to avoid confusion with routine SCL_DO
*-----------------------------------------------------------------------

      INTEGER*4 ADDRESS    (10)   ! Address of variable to increment on
      INTEGER*4 DOVALS     (3,10) ! The DO parameters - first, last, increment
      INTEGER*4 CALL_LEVEL (10)   ! Stack pointer at call
      INTEGER*4 CALL_LINE  (10)   ! Calling line number
      INTEGER*4 DO_DEPTH          ! Depth of DO nesting. 0 when not DOing
      INTEGER*4 DO_FILE           ! Logical unit number of DO file being edited

      COMMON /SCLDO/ ADDRESS, DOVALS,
     &               CALL_LEVEL, CALL_LINE, DO_DEPTH, DO_FILE

      SAVE   /SCLDO/
