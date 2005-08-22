*-----------------------------------------------------------------------

      SUBROUTINE SCL_MATCHCOMM (COMMAND, ICOM, IERR)

*  Routine to find a match for the SCL command string. On the assumption
*  that the complete command is given, hashes the string and searches the
*  hash table. If this fails then probably the command is a minimum string,
*  so call the GEN_COMACH to do the minimum matching. If that fails there must
*  be an error!

*  History:
*     6-JUN-2000 (AJC):
*       Replace 'Type *' with 'PRINT *'

      IMPLICIT  NONE

*     Formal parameters:

      CHARACTER COMMAND*(*)
      INTEGER*4 ICOM
      INTEGER*4 IERR

*     Include files:

      INCLUDE  'COMMAND_TABLE'

*     Local variables:

*     Functions:

      INTEGER*4 HASH
      INTEGER*4 SCL_HASHSEARCH

*  Ok, go..

      HASH = SCL_HASHSEARCH (COMMAND, M_CTAB, TABLE, COMMS, ICOM)
      IF (HASH.LT.0) THEN
        IERR = 81
      ELSE
CD      PRINT *,'Command located in hash table @ ', ICOM
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SCL_MAKETABLE (IERR)

*  Routine to build the initial hash table from the SCL "built-in" commands.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER*4 IERR

*     Include files:

      INCLUDE  'COMMAND_TABLE'

*     Local variables:

      INTEGER*4 J
      INTEGER*4 HASH

*     Functions:

      INTEGER*4 SCL_HASHINSERT

*  Ok, go..

      DO J = 1, NFUNC
        HASH = SCL_HASHINSERT (COMMS(J), J, M_CTAB, TABLE)
        IF (HASH.LT.0) THEN
          PRINT *, '-- scl_maketable --'
          PRINT *, '   Error inserting ', COMMS(J),' in hash table'
        END IF
      END DO

      RETURN
      END

*-----------------------------------------------------------------------

      INTEGER*4 FUNCTION SCL_HASHINSERT (SYMBOL, VALUE, M, TABLE)

      IMPLICIT  NONE

*     Formal parameters

      CHARACTER       SYMBOL*(*)
      INTEGER*4       VALUE
      INTEGER*4       M
      INTEGER*4       TABLE(0:M-1)

*     Local variables

      LOGICAL*4 DONE
      INTEGER*4 COUNTER
      INTEGER*4 EMPTY     /-1/      ! placeholder for deleted entries
      INTEGER*4 ISYM

*     Functions

      INTEGER*4 GEN_HASH

*     OK? Go..

      COUNTER        = 1
      SCL_HASHINSERT = GEN_HASH (SYMBOL, M)

CD    PRINT *,'Symbol ', symbol,' hashed to ', scl_hashinsert

      DONE = .FALSE.

      DO WHILE ( COUNTER.LT.M .AND. .NOT.DONE )
        ISYM = TABLE(SCL_HASHINSERT)
        IF (ISYM.NE.0 .AND. ISYM.NE.EMPTY) THEN
          SCL_HASHINSERT = MOD (SCL_HASHINSERT+1, M)
          COUNTER = COUNTER + 1
        ELSE
          DONE = .TRUE.
        END IF
      END DO

      IF (COUNTER.NE.M) THEN
        TABLE(SCL_HASHINSERT) = VALUE
      ELSE
        SCL_HASHINSERT = -1
        PRINT *,'-- SCL_hashinsert --'
        PRINT *,'Hash table full!'
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      INTEGER*4 FUNCTION SCL_HASHSEARCH (SYMBOL, M, TABLE, COMMS, ICOM)

      IMPLICIT  NONE

*     Formal parameters

      CHARACTER       SYMBOL*(*)
      INTEGER*4       M
      INTEGER*4       TABLE(0:M-1)
      CHARACTER       COMMS(*)*(*)
      INTEGER*4       ICOM

*     Local variables

      LOGICAL*4       DONE
      LOGICAL*4       FOUND
      INTEGER*4       COUNT
      INTEGER*4       ISYM
      INTEGER*4       EMPTY /-1/

*     Functions

      INTEGER*4       GEN_HASH

*     OK? Go..

      COUNT          = 1
      SCL_HASHSEARCH = GEN_HASH (SYMBOL, M)

CD    PRINT *, 'Command ', SYMBOL, ' hashed to', SCL_HASHSEARCH

      DONE  = .FALSE.
      FOUND = .FALSE.

      DO WHILE (.NOT.DONE .AND. COUNT.LE.M)
        ISYM = TABLE(SCL_HASHSEARCH)

CD      PRINT *,'  -- table entry = ', ISYM
CD      PRINT *,'  -- corresponding command is ', COMMS(ISYM)

        IF (ISYM.EQ.0) THEN                             ! Empty position
          DONE = .TRUE.
        ELSE IF (ISYM.EQ.EMPTY) THEN                    ! Deleted entry
          SCL_HASHSEARCH = MOD (SCL_HASHSEARCH+1, M)
          COUNT          = COUNT + 1
        ELSE IF (COMMS(ISYM).EQ.SYMBOL) THEN            ! Got it!
          ICOM  =  ISYM
          FOUND = .TRUE.
          DONE  = .TRUE.
        ELSE                                            ! Collision
          SCL_HASHSEARCH = MOD (SCL_HASHSEARCH+1, M)
          COUNT          = COUNT + 1
        END IF
      END DO

      IF (.NOT. FOUND) THEN
        SCL_HASHSEARCH = -1
CD      PRINT *,'-- SCL_hashsearch --'
CD      PRINT *,'Symbol not defined'
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      LOGICAL*4 FUNCTION SCL_HASHDELETE (SYMBOL, M, TABLE, COMMS, HASH)

      IMPLICIT  NONE

*     Formal parameters

      CHARACTER       SYMBOL*(*)
      INTEGER*4       M
      INTEGER*4       TABLE(0:M-1)
      CHARACTER       COMMS(*)*(*)
      INTEGER*4       HASH

*     Local variables

      INTEGER*4       EMPTY   /-1/
      INTEGER*4       VALUE

*     Functions

      INTEGER*4       SCL_HASHSEARCH

*     OK? Go..

      HASH = SCL_HASHSEARCH (SYMBOL, M, TABLE, COMMS, VALUE)

      IF (HASH.NE.-1) THEN
        TABLE(HASH)    = EMPTY
        SCL_HASHDELETE = .TRUE.
      ELSE
        SCL_HASHDELETE = .FALSE.
        PRINT *,'-- SCL_hashdelete --'
        PRINT *,'Symbol not defined'
      END IF

      RETURN
      END

*-----------------------------------------------------------------------
