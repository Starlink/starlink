
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------

      SUBROUTINE GEN_MAKESYMB (SYMBOL, INTYPE, LENGTH, ADDRESS, IERR)

      IMPLICIT NONE

*     Formal parameters

      CHARACTER SYMBOL*(*)                 ! Symbol name
      CHARACTER INTYPE*(*)                 ! Symbol type
      INTEGER*4 LENGTH                     ! Array length
      INTEGER*4 ADDRESS                    ! Variable location
      INTEGER*4 IERR                       ! Error if non-zero

      INTEGER*4 TABLE_ADDRESS
      INTEGER*4 LENGTH_ADDRESS
      COMMON /GEN_SYMBOLS/ TABLE_ADDRESS, LENGTH_ADDRESS

      CALL GEN_MAKESYM1 (%VAL(TABLE_ADDRESS), %VAL(LENGTH_ADDRESS),
     &                   SYMBOL, INTYPE, LENGTH, ADDRESS, IERR)

      RETURN
      END
