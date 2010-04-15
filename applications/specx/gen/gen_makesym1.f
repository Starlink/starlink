*  History:
*     16 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*      7 Jun 2000 (ajc):
*        Use C structure for port to Linux
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      SUBROUTINE GEN_MAKESYM1 (TABLE, NO_ENTRIES,
     &                         SYMBOL, INTYPE, LENGTH, ADDRESS, IERR)

      IMPLICIT NONE

      INTEGER*4 MAX_TABLE
      PARAMETER (MAX_TABLE=512)

*     Formal parameters

      INTEGER TABLE                        ! Dummy
      INTEGER*4 NO_ENTRIES                 ! Updated on return
      CHARACTER SYMBOL*(*)                 ! Symbol name
      CHARACTER INTYPE*(*)                 ! Symbol type
      INTEGER*4 LENGTH                     ! Array length
      INTEGER*4 ADDRESS                    ! Variable location
      INTEGER*4 IERR                       ! Error if non-zero

*     Functions

      LOGICAL   GEN_DFORMAT
      LOGICAL   GEN_EFORMAT

*     Hash table

      INTEGER*4           ENTRY
      COMMON /HASH_TABLE/ ENTRY

*     Local variables

      CHARACTER NAME*16
      CHARACTER TYPE*4

      LOGICAL*4 READONLY
      INTEGER*4 SYM_INDEX
      CHARACTER SYM_TYPE*4
      INTEGER*4 SYM_SIZE
      INTEGER*4 SYM_ADDR

      INTEGER*4 N
      INTEGER*4 IDIGITS, FDIGITS, EDIGITS

*     Functions

      INTEGER*4 GEN_HASHINS

*  OK? Go..

      IERR = 0
      IF (NO_ENTRIES .EQ. MAX_TABLE) THEN
        IERR = 1
        PRINT *, 'Variable table full'
        RETURN
      END IF

      NAME = SYMBOL
      CALL UUCASE (NAME)
      TYPE = INTYPE
      CALL UUCASE (TYPE)

*     Check that symbol is not a number!

      IF (GEN_EFORMAT(SYMBOL, IDIGITS, FDIGITS, EDIGITS)) THEN
        PRINT *,'Symbol name is a valid numerical value!'
        IERR = 2
        RETURN
      END IF

      IF (GEN_DFORMAT (SYMBOL, IDIGITS, FDIGITS, EDIGITS)) THEN
        PRINT *,'Symbol name is a valid numerical value!'
        IERR = 2
        RETURN
      END IF

*     Check that symbol does not already exist

      CALL GEN_INQSYMB1 (TABLE, NO_ENTRIES, NAME, SYM_INDEX,
     &                   SYM_TYPE, SYM_SIZE, SYM_ADDR, READONLY, IERR)

      IF (SYM_INDEX.NE.0) THEN
*       PRINT *,'Symbol already defined!'
        IERR = 3
        RETURN
      END IF

      CALL GEN_MAKESYM2( NO_ENTRIES+1, NAME, TYPE, LENGTH, ADDRESS,
     :  IERR )

      NO_ENTRIES = NO_ENTRIES + 1

*     Enter it in the hash table

      N = GEN_HASHINS (NAME(INDEX(NAME,'*')+1:), 503,
     &                 ENTRY, NO_ENTRIES)
      IF (N .EQ. -1) THEN
        PRINT *,'-- gen_makesym1 --'
        PRINT *,'   hash table full'
        IERR = 4
      ELSE
CD      PRINT *,'New entry at position ', N, ' in hash table'
      END IF

      RETURN
      END
