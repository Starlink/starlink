*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      LOGICAL*4 FUNCTION PUSH_IFSTACK (ISP)

      IMPLICIT  NONE

*     Formal parameters

      INTEGER*4 ISP      ! Stack pointer

*     IF-stack

      INTEGER*4 MAX_IF
      PARAMETER (MAX_IF=10)

      INTEGER*4 IF_COUNT
      INTEGER*4 IF_STACK(MAX_IF)
      COMMON /SPECX_IF/ IF_COUNT, IF_STACK

* Ok, go..

CD    Print *,'-- push_ifstack --'
CD    Print *,'   initial IF_COUNT = ', IF_COUNT

      IF_COUNT = IF_COUNT + 1

      IF (IF_COUNT.GT.MAX_IF) THEN
        PUSH_IFSTACK = .FALSE.
        IF_COUNT     = IF_COUNT - 1
        RETURN
      END IF

      IF_STACK(IF_COUNT) = ISP

      PUSH_IFSTACK = .TRUE.

CD    Print *,'     final IF_COUNT = ', IF_COUNT, ' IF_LEVEL = ', ISP

      RETURN
      END

*-----------------------------------------------------------------------

      LOGICAL*4 FUNCTION PULL_IFSTACK (ISP)

      IMPLICIT  NONE

*     Formal parameters

      INTEGER*4 ISP      ! Stack pointer

*     IF-stack

      INTEGER*4 MAX_IF
      PARAMETER (MAX_IF=10)

      INTEGER*4 IF_COUNT
      INTEGER*4 IF_STACK(MAX_IF)
      COMMON /SPECX_IF/ IF_COUNT, IF_STACK

*  Ok, go..

CD    Print *,'-- pull_ifstack --'
CD    Print *,'   initial IF_COUNT = ', IF_COUNT

      IF_COUNT = IF_COUNT - 1

      IF (IF_COUNT.LT.0) THEN
        PULL_IFSTACK = .FALSE.
        IF_COUNT     = 0
        RETURN
      END IF

      IF (IF_COUNT.EQ.0) THEN
        ISP  = 0
      ELSE
        ISP  = IF_STACK(IF_COUNT)
      END IF

      PULL_IFSTACK = .TRUE.

CD    Print *,'     final IF_COUNT = ', IF_COUNT, ' IF_LEVEL = ', ISP

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE INIT_IFSTACK

      IMPLICIT  NONE

*     IF-stack

      INTEGER*4 MAX_IF
      PARAMETER (MAX_IF=10)

      INTEGER*4 IF_COUNT
      INTEGER*4 IF_STACK(MAX_IF)
      COMMON /SPECX_IF/ IF_COUNT, IF_STACK

      INTEGER*4 IF_LEVEL
      INTEGER*4 IF_SKIP
      LOGICAL*4 DO_TO_ELSEIF
      LOGICAL*4 WAIT_FOR_ENDIF
      COMMON /IF/ IF_LEVEL, IF_SKIP, DO_TO_ELSEIF, WAIT_FOR_ENDIF

      IF_COUNT = 0

CD    Print *,'IF stack initialized: IF level = ', IF_COUNT

      IF_LEVEL = 0
      IF_SKIP  = 0
      DO_TO_ELSEIF   = .TRUE.
      WAIT_FOR_ENDIF = .FALSE.

      RETURN
      END

*-----------------------------------------------------------------------
