*  History:
*     16 Nov 1993 (hme):
*        Disable specx_show_table1 and the STRUCTURE therein.
*        Replace STR$UPCASE with CHR_UCASE.
*        Set common variable PRINT_OUTPUT false in a statement rather
*        than in the declaration.
*     23 Nov 1993 (hme):
*        Reinstate specx_show_table1 and the STRUCTURE. The problem was
*        the implied DO in the WRITE, rather the extra () in it.
*     17 Dec 1993 (hme):
*        Re-order IODATA common block to avoid alignment problems.
*     31 Dec 1993 (rp):
*        Insert wildcard matching stuff from V6.3
*        Remove initialization of PRINT_OUTPUT; done in SCL_MAIN anyway
*     15 Jan 1994 (rp):
*        Change CHR_UCASE to UUCASE
*      7 Jun 2000 (ajc):
*        Use C structure in specx_show_table1 for port to Linux
*        Change TYPE * to PRINT *
*        Unused in SPECX_MAKE_VAR: LENGTH, SYM_INDEX/TYPE/LENGTH/ADDR
*               in SPECX_SET_VALUE: ISTAT
*-----------------------------------------------------------------------

      SUBROUTINE SPECX_MAKE_VAR (STRING, INTYPE, IERR)

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

*     Other parameters

      INTEGER*4 MAXSTRLEN
      PARAMETER (MAXSTRLEN=128)

*     Local variables

      INTEGER*4 ERROR
      INTEGER*4 ILS
      INTEGER*4 I
      INTEGER*4 NBYTES
      INTEGER*4 NBTOT
      INTEGER*4 LBRACKET
      INTEGER*4 RBRACKET
      LOGICAL   ISNEW
      CHARACTER TYPE*4

      CHARACTER NAME*16
      INTEGER*4 ARRAY_LENGTH

      IERR = 0

*     Determine a type and element-length

      TYPE = INTYPE
      CALL UUCASE (TYPE)

      IF (TYPE(1:2).EQ.'R8') THEN
        NBYTES = 8
      ELSE IF (TYPE(1:2).EQ.'R4') THEN
        NBYTES = 4
      ELSE IF (TYPE(1:2).EQ.'I4') THEN
        NBYTES = 4
      ELSE IF (TYPE(1:2).EQ.'L4') THEN
        NBYTES = 4
      ELSE IF (TYPE(1:1).EQ.'C') THEN
        READ (TYPE(2:), *, IOSTAT=ERROR) NBYTES
        IF (ERROR.NE.0) THEN
          IERR = 5
          GO TO 99
        ELSE IF (NBYTES.GT.MAXSTRLEN) THEN
          IERR = 6
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

      ILS = MIN (LEN(STRING),LEN(NAME)+I-1)
      NAME = STRING(I:ILS)
      CALL UUCASE (NAME)

*     Check if it is an array and if so how long

      ARRAY_LENGTH = 1
      LBRACKET = INDEX (NAME, '(')
      IF (LBRACKET.NE.0) THEN
        RBRACKET = INDEX (NAME, ')')
        IF (RBRACKET.EQ.0) RBRACKET = LEN(NAME)
        READ (NAME(LBRACKET+1:RBRACKET-1), *, IOSTAT=ERROR)
     &        ARRAY_LENGTH
        IF (ERROR.NE.0) THEN
          IERR = 85
          RETURN
        END IF
      ELSE
        LBRACKET = LEN(NAME) + 1
      END IF

      CALL GEN_MAKESYMB (NAME(:LBRACKET-1), TYPE, ARRAY_LENGTH,
     &     CNF_PREG( CNF_PVAL(UMEMORY_PTR) + UMEMORY_LENGTH, ISNEW ),
     &     IERR )

*     No errors? allocate user memory (note: allocate enough to end
*     on 8-byte word boundary)

      IF (IERR.NE.0) GO TO 99

      NBTOT = 8*((NBYTES*ARRAY_LENGTH-1)/8)+8
      IF (UMEMORY_LENGTH + NBTOT .GT.  UMEMORY_SIZE) THEN
        IERR = 90
        GO TO 99
      ELSE
        UMEMORY_LENGTH = UMEMORY_LENGTH + NBTOT
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
      ELSE IF (IERR.EQ.6) THEN
        IERR = 99                 ! String too long
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SPECX_SHOW_TABLE (SYMBOL)

      IMPLICIT NONE

*     Formal parameter(s):

      CHARACTER SYMBOL*(*)

      INTEGER*4 L_SYMTAB, SYMTAB_ADDRESS
      COMMON /SYMTABS/ L_SYMTAB, SYMTAB_ADDRESS

*   I/O control

      INCLUDE  'IODATA'
      INCLUDE  'CNF_PAR'

*     Functions:

      INTEGER   GEN_ILEN

*     Local variables:

      INTEGER   ILS
      INTEGER   SYM_INDEX
      CHARACTER SYM_TYPE*4
      INTEGER   SYM_LEN
      INTEGER   SYM_ADDR
      LOGICAL   READONLY
      INTEGER   IERR

*  Ok, go...

*     Search for wildcards:
      IF (INDEX (SYMBOL, '*') .ne. 0) THEN
        CALL SPECX_SHOW_TABLE1 (%VAL(CNF_PVAL(SYMTAB_ADDRESS)), 
     :                          L_SYMTAB,
     &                          SYMBOL, ILOUT2)

*     No wildcards, look for exact match

      ELSE IF (SYMBOL.NE.' ') THEN
        ILS = GEN_ILEN (SYMBOL)
        CALL GEN_INQSYMB (SYMBOL(:ILS), SYM_INDEX, SYM_TYPE,
     &                    SYM_LEN, SYM_ADDR, READONLY, IERR)
        IF (SYM_INDEX.NE.0) THEN
          WRITE (ILOUT2, '(5X,A16,1X,A4,1X,I4.1,6X)', IOSTAT=IERR)
     &                   SYMBOL, SYM_TYPE, SYM_LEN
        ELSE
          PRINT *, 'Symbol name "', SYMBOL(:ILS), '" not found.'
        END IF
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SPECX_SHOW_TABLE1 (TABLE, NO_ENTRIES, TSYMBOL, ILOUT)

      IMPLICIT NONE

*     Formal parameters

      INTEGER*4 NO_ENTRIES         ! Number of entries made in table to date
      INTEGER TABLE    !Dummy

      CHARACTER       TSYMBOL*(*)
      INTEGER*4       ILOUT

      INTEGER*4 I
      INTEGER*4 IERR
      LOGICAL*4 MATCH_OK

      CHARACTER*16 NAME
      CHARACTER*4  TYPE
      INTEGER*4    LENGTH
      INTEGER*4    ADDRESS

*  ok, go..

      IF (NO_ENTRIES.LE.0) THEN
        WRITE (ILOUT, *) 'No entries in symbol table yet!'
        RETURN
      END IF

      CALL UUCASE (TSYMBOL)
      WRITE (ILOUT,*) '  (Symbol name     Type Array_length)'

      DO I = 1, NO_ENTRIES

        CALL GEN_INQSYMENT( I, NAME, TYPE, LENGTH, ADDRESS, IERR )
        CALL SCL_MATCH_WILD (NAME, TSYMBOL, MATCH_OK)
        IF (MATCH_OK)
     &    WRITE (ILOUT, '(5X,A16,1X,A4,1X,I4.1,6X)', IOSTAT=IERR)
     &           NAME,
     &           TYPE,
     &           LENGTH

      END DO

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SPECX_SET_VALUE (SYMBOL, STRING, IFAIL)

      IMPLICIT  NONE
      INCLUDE 'CNF_PAR'

*     Formal parameters

      CHARACTER SYMBOL*(*)
      CHARACTER STRING*(*)
      INTEGER*4 IFAIL

*     Symbol table

      INTEGER*4 L_SYMTAB, SYMTAB_PTR
      COMMON /SYMTABS/ L_SYMTAB, SYMTAB_PTR

*     Local variables

      LOGICAL*4 LVALUE
      INTEGER*4 IVALUE
      REAL*4    R4VALUE
      REAL*8    R8VALUE
      CHARACTER CVALUE*128
      EQUIVALENCE (LVALUE, IVALUE, R4VALUE, R8VALUE, CVALUE)

      LOGICAL*4 READONLY
      CHARACTER TYPE*1
      CHARACTER SYM_TYPE*4
      INTEGER*4 SYM_INDEX
      INTEGER*4 SYM_LEN
      INTEGER*4 SYM_ADDR
      INTEGER*4 NBYTES
      INTEGER*4 IERR
      INTEGER*4 ILS
      INTEGER*4 ICH1, ICH2
      INTEGER*4 IOFF

*     Other functions

      INTEGER*4 GEN_ILEN

      IFAIL = 0

*     PRINT *,'-- specx_set_value --'
*     PRINT *,'  Symbol and string lengths:', LEN(SYMBOL), LEN(STRING)
*     PRINT *,'  Symbol name:   ', SYMBOL
*     PRINT *,'  String value:  ', STRING

      ILS = INDEX (SYMBOL,'(') - 1
      IF (ILS.EQ.-1) ILS = LEN(SYMBOL)

      CALL GEN_INQSYMB (SYMBOL(:ILS), SYM_INDEX, SYM_TYPE,
     &                  SYM_LEN, SYM_ADDR, READONLY, IERR)

      IF (SYM_INDEX.EQ.0) THEN
        PRINT *, 'Variable "', SYMBOL(:ILS), '" not defined'
        IFAIL = 100
        RETURN
      END IF

      IF (READONLY) THEN
        PRINT *, 'Variable "', SYMBOL(:ILS), '" is readonly'
        IFAIL = 102
        RETURN
      END IF

      TYPE = SYM_TYPE(1:1)
      READ (SYM_TYPE(2:), *) NBYTES
*     PRINT *,'  Symbol type:   ', TYPE
*     PRINT *,'  # of bytes:    ', NBYTES

      IF (TYPE.EQ.'C') THEN
        CVALUE = ' '
      END IF

      CALL GEN_EVAL_AE (STRING, SYM_TYPE, LVALUE, IERR)
      IF (IERR.NE.0) THEN
!        IF (IERR.EQ.1) THEN
!          IFAIL = 85
!        ELSE IF (IERR.EQ.2) THEN
!          IFAIL = 95
!        ELSE IF (IERR.EQ.3) THEN
!          IFAIL = 103
!        ELSE IF (IERR.EQ.4) THEN
!          IFAIL = 108
!        ELSE IF (IERR.EQ.5) THEN
!          IFAIL = 100
!        ELSE IF (IERR.EQ.6) THEN
!          IFAIL = 100
!        END IF
        IFAIL = 103
        RETURN
      END IF

*     Find if it is an array and evaluate the array index

      CALL GET_SUBEXPR (SYMBOL, ICH1, ICH2, IERR)
      IF (IERR.NE.0) RETURN

      IF (ICH1.GT.0 .AND. ICH2.GE.ICH1) THEN

        CALL GEN_EVAL_AE (SYMBOL(ICH1:ICH2), 'I4', IOFF, IERR)

        IF (IERR.NE.0) THEN
          IF (IERR.EQ.1) THEN
            IFAIL = 85
          ELSE IF (IERR.EQ.2) THEN
            IFAIL = 95
          ELSE IF (IERR.EQ.3) THEN
            IFAIL = 107
          ELSE IF (IERR.EQ.4) THEN
            IFAIL = 108
          ELSE IF (IERR.EQ.5) THEN
            IFAIL = 100
          ELSE IF (IERR.EQ.6) THEN
            IFAIL = 100
          END IF
          RETURN
        END IF

      ELSE
        IOFF = 1
        ICH1 = GEN_ILEN(SYMBOL) + 2
        ICH2 = ICH1
      END IF

*     IF (TYPE.NE.'C') THEN

        CALL GEN_SETSYM1 (%VAL(CNF_PVAL(SYMTAB_PTR)), L_SYMTAB,
     &                    SYM_INDEX, IOFF, LVALUE, IERR)
*     ELSE
*       CALL GEN_SETSYM1 (%VAL(CNF_PVAL(SYMTAB_PTR)), L_SYMTAB,
*    &                    SYM_INDEX, IOFF, %REF(STRING), IERR)
*     END IF

      RETURN
      END

*-----------------------------------------------------------------------
