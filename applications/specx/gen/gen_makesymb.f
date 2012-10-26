
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------

      SUBROUTINE GEN_MAKESYMB (SYMBOL, INTYPE, LENGTH, ADDRESS, IERR)

      IMPLICIT NONE
      INCLUDE 'CNF_PAR'

*     Formal parameters

      CHARACTER SYMBOL*(*)                 ! Symbol name
      CHARACTER INTYPE*(*)                 ! Symbol type
      INTEGER*4 LENGTH                     ! Array length
      INTEGER*4 ADDRESS                    ! Variable location
      INTEGER*4 IERR                       ! Error if non-zero

      INTEGER*4 TABLE_ADDRESS
      INTEGER*4 LENGTH_ADDRESS
      COMMON /GEN_SYMBOLS/ TABLE_ADDRESS, LENGTH_ADDRESS

      IF (LENGTH_ADDRESS .EQ. 0) THEN
         PRINT *, 'Error registering length of symbol ', SYMBOL
         IERR = 1
         RETURN
      END IF

      IF (TABLE_ADDRESS .EQ. 0) THEN
         PRINT *, 'Error with TABLE_ADDRESS pointer for symbol ', SYMBOL
         IERR = 1
         RETURN
      END IF

      CALL GEN_MAKESYM1 (%VAL(CNF_PVAL(TABLE_ADDRESS)),
     :                   %VAL(CNF_PVAL(LENGTH_ADDRESS)),
     &                   SYMBOL, INTYPE, LENGTH, ADDRESS, IERR)

      RETURN
      END
