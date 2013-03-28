*  History:
*     16 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*      3 Aug 2000 (ajc):
*        Use format I3 to read type size
*        and construct format to read array size
*        Unused LENGTH, SYM_INDEX, SYM_TYPE, SYM_LENGTH, SYM_ADDR
*-----------------------------------------------------------------------

      SUBROUTINE GEN_MAKE_VAR (STRING, INTYPE, IERR)

      IMPLICIT NONE

      INCLUDE 'CNF_PAR'

*     Formal parameters

      CHARACTER STRING*(*)                 ! Symbol to be defined
      CHARACTER INTYPE*(*)                 ! Its type -- L4/I4/R4/R8/Cnnn
      INTEGER*4 IERR                       ! Error if non-zero

*     User symbol table

      INTEGER*4 UMEMORY_PTR
      INTEGER*4 UMEMORY_SIZE
      INTEGER*4 UMEMORY_LENGTH
      COMMON /UMEMORY/ UMEMORY_PTR, UMEMORY_SIZE,
     &                 UMEMORY_LENGTH

*     Local variables

      INTEGER*4 ERROR
      INTEGER*4 ILS
      INTEGER*4 ILT
      INTEGER*4 I
      INTEGER*4 NBYTES
      INTEGER*4 LBRACKET
      INTEGER*4 RBRACKET
      LOGICAL   ISNEW
      CHARACTER TYPE*4

      CHARACTER NAME*16
      CHARACTER FORMAT*8
      INTEGER*4 ARRAY_LENGTH

*     Functions

      INTEGER*4 GEN_ILEN

*  Ok, go...

      IERR = 0

*     Determine a type and element-length

      TYPE = INTYPE
      CALL UUCASE (TYPE)
      ILT  = GEN_ILEN (TYPE)

      IF (TYPE(1:2).EQ.'R8') THEN
        NBYTES = 8
      ELSE IF (TYPE(1:2).EQ.'R4') THEN
        NBYTES = 4
      ELSE IF (TYPE(1:2).EQ.'I4') THEN
        NBYTES = 4
      ELSE IF (TYPE(1:2).EQ.'L4') THEN
        NBYTES = 4
      ELSE IF (TYPE(1:1).EQ.'C') THEN
        READ (TYPE(2:ILT), '(I3)', IOSTAT=ERROR) NBYTES
        IF (ERROR.NE.0) THEN
          IERR = 5
          GO TO 99
        END IF
      ELSE
        IERR = 5
        GO TO 99
      END IF

*     Remove any leading blanks (index to start of non-blank string)

      I = 1
      DO WHILE (STRING(I:I).EQ.' ')
        I = I+1
      END DO

      ILS = MIN (LEN(STRING)-I,LEN(NAME))
      NAME = STRING(I:ILS)
      CALL UUCASE (NAME)

*     Check if it is an array and if so how long

      ARRAY_LENGTH = 1
      LBRACKET = INDEX (NAME, '(')
      IF (LBRACKET.NE.0) THEN
        RBRACKET = INDEX (NAME, ')')
        IF (RBRACKET.EQ.0) RBRACKET = LEN(NAME)
        WRITE( FORMAT, '(''(I'', I3, '')'')' ) RBRACKET - LBRACKET -1
        READ (NAME(LBRACKET+1:RBRACKET-1), FORMAT) ARRAY_LENGTH
      ELSE
        LBRACKET = LEN(NAME) + 1
      END IF

      CALL GEN_MAKESYMB (NAME(:LBRACKET-1), TYPE, ARRAY_LENGTH,
     &     CNF_PREG( CNF_PVAL(UMEMORY_PTR) + UMEMORY_LENGTH, ISNEW ),
     &     IERR )

*     No errors? allocate user memory

      IF (IERR.NE.0) GO TO 99

      IF (UMEMORY_LENGTH + NBYTES*ARRAY_LENGTH
     &    .GT.  UMEMORY_SIZE) THEN
        IERR = 90
        GO TO 99
      ELSE
        UMEMORY_LENGTH = UMEMORY_LENGTH + NBYTES*ARRAY_LENGTH
      END IF

      RETURN

*     --------------

   99 CONTINUE

      IF (IERR.EQ.1) THEN
        IERR = 104                ! variable table full
      ELSE IF (IERR.EQ.2) THEN
        IERR = 105                ! valid numerical value
      ELSE IF (IERR.EQ.3) THEN
        IERR = 101                ! Symbol already exists
      ELSE IF (IERR.EQ.4) THEN
        IERR = 106                ! Hash table full
      ELSE IF (IERR.EQ.5) THEN
        IERR = 21                 ! Type not available
      END IF

      RETURN
      END
