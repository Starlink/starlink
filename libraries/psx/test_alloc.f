      PROGRAM TEST_ALLOC

* Test subroutines PSX_MALLOC, PSX_REALLOC, PSX_CALLOC and PSX_FREE.

      IMPLICIT NONE
      INCLUDE 'CNF_PAR'
      INCLUDE 'SAE_PAR'
      INTEGER STATUS

* Local Variables:
      INTEGER PNTR, PNTR2, PNTR3

* Initialize STATUS
      STATUS = SAI__OK
      
* Test PSX_MALLOC
      PRINT *,' '
      PRINT *,'--  Program TEST_ALLOC, function PSX_MALLOC  --'
      PRINT *,' '

      CALL PSX_MALLOC( 40, PNTR, STATUS )
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SUBI( %VAL(CNF_PVAL(PNTR)), 10 )
         CALL PRNTI( %VAL(CNF_PVAL(PNTR)), 10 )
      ELSE
         PRINT *,'Error calling PSX_MALLOC'
      END IF

* Test PSX_REALLOC
      PRINT *,' '
      PRINT *,'--  Program TEST_ALLOC, function PSX_REALLOC  --'
      PRINT *,' '

      IF (STATUS .EQ. SAI__OK) THEN
         PRINT *,'Testing PSX_REALLOC'
         CALL PSX_REALLOC( 80, PNTR, STATUS )
         IF (STATUS .EQ. SAI__OK) THEN
            CALL SUBI( %VAL(CNF_PVAL(PNTR)), 20 )
            CALL PRNTI( %VAL(CNF_PVAL(PNTR)), 20 )
         ELSE
            PRINT *,'Error calling PSX_REALLOC'
         END IF
      END IF

* Test PSX_CALLOC - INTEGER
      PRINT *,' '
      PRINT *,'--  Program TEST_ALLOC, function PSX_CALLOC, INTEGER  --'
      PRINT *,' '

      STATUS = SAI__OK
      PRINT *,'Testing PSX_CALLOC'
      CALL PSX_CALLOC( 10, '_INTEGER', PNTR2, STATUS )
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SUBI( %VAL(CNF_PVAL(PNTR2)), 10 )
         CALL PRNTI( %VAL(CNF_PVAL(PNTR2)), 10 )
      ELSE
         PRINT *,'Error calling PSX_CALLOC'
      END IF

* Test PSX_CALLOC - WORD
      PRINT *,' '
      PRINT *,'--  Program TEST_ALLOC, function PSX_CALLOC, WORD  --'
      PRINT *,' '

      STATUS = SAI__OK
      PRINT *,'Testing PSX_CALLOC'
      CALL PSX_CALLOC( 10, '_WORD', PNTR3, STATUS )
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SUBW( %VAL(CNF_PVAL(PNTR3)), 10 )
         CALL PRNTW( %VAL(CNF_PVAL(PNTR3)), 10 )
      ELSE
         PRINT *,'Error calling PSX_CALLOC'
      END IF

* Test PSX_FREE
      PRINT *,' '
      PRINT *,'--  Program TEST_ALLOC, function PSX_FREE  --'
      PRINT *,' '

      STATUS = SAI__OK
      PRINT *,'Testing PSX_FREE'
      CALL PSX_FREE( PNTR, STATUS )
      CALL PSX_FREE( PNTR2, STATUS )
      CALL PSX_FREE( PNTR3, STATUS )
      IF (STATUS .EQ. SAI__OK) THEN
         CONTINUE
      ELSE
         PRINT *,'Error calling PSX_FREE'
      END IF

      END

      SUBROUTINE SUBI( ARRAY, N )
      INTEGER N, ARRAY( N )
      INTEGER I

      DO I = 1, N
         ARRAY( I ) = I
      END DO

      END

      SUBROUTINE PRNTI( ARRAY, N )
      INTEGER N, ARRAY( N )
      INTEGER I

      DO I = 1, N
         PRINT *,ARRAY( I )
      END DO

      END

      SUBROUTINE SUBW( ARRAY, N )
      INTEGER N
      INTEGER*2 ARRAY( N )
      INTEGER I

      DO I = 1, N
         ARRAY( I ) = I
      END DO

      END

      SUBROUTINE PRNTW( ARRAY, N )
      INTEGER N
      INTEGER*2 ARRAY( N )
      INTEGER I

      DO I = 1, N
         PRINT *,ARRAY( I )
      END DO

      END
