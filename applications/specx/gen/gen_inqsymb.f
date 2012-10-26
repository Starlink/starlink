*  History:
*     31 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Use Fortran standard format for reading numbers from strings
*-----------------------------------------------------------------------

      SUBROUTINE GEN_INQSYMB (INSYMBOL, SYM_INDEX, TYPE,
     &                        LENGTH, ADDRESS, READONLY, IERR)

      IMPLICIT NONE

*     Formal parameters

      CHARACTER INSYMBOL*(*)
      INTEGER*4 SYM_INDEX
      CHARACTER TYPE*(*)
      INTEGER*4 LENGTH
      INTEGER*4 ADDRESS
      LOGICAL*4 READONLY
      INTEGER*4 IERR          ! = 1, symbol not found; 6, table not installed

*     Symbol table

      INCLUDE  'GEN_SYMBOLS.INC'
      INCLUDE  'CNF_PAR'

*     User variables

      INTEGER*4 C1, C2
      INTEGER*4 ELEMENT
      INTEGER*4 ILS
      INTEGER*4 NCH
      INTEGER*4 ERROR
      INTEGER*4 IDIGITS, FDIGITS, EDIGITS
      CHARACTER*10 FORMAT

*     Functions

      INTEGER*4 GEN_ILEN
      LOGICAL   GEN_INTEGER
      LOGICAL   GEN_DFORMAT
      LOGICAL   GEN_EFORMAT

*  OK? Go..

      IF (.NOT. SYMTAB_INSTALLED) THEN
        IERR = 6
        RETURN
      END IF

      ILS = GEN_ILEN (INSYMBOL)
      C1 = INDEX (INSYMBOL, '(') - 1
      IF (C1.LT.0) C1 = ILS

      CALL GEN_INQSYMB1 (%VAL(CNF_PVAL(TABLE_ADDRESS)), 
     :                   %VAL(CNF_PVAL(LENGTH_ADDRESS)),
     &                   INSYMBOL(:C1),
     &                   SYM_INDEX, TYPE, LENGTH, ADDRESS,
     &                   READONLY, IERR)

      IF (IERR.NE.0) THEN
        IERR = 99
        RETURN
      ELSE IF (SYM_INDEX.EQ.0) THEN
        IERR = 100
        RETURN
      END IF

CD    PRINT *,'-- gen_inqsymb --'

      IF (C1.NE.ILS) THEN
        CALL GET_SUBEXPR  (INSYMBOL, C1, C2, IERR)
        IF (IERR.NE.0) RETURN

        IF (GEN_INTEGER (INSYMBOL(C1:C2))) THEN
* Construct FORMAT for reading symbol element number
          WRITE( FORMAT, '(''(I'', I3, '')'')' ) C2-C1+1
          READ (INSYMBOL(C1:C2), FORMAT, IOSTAT=ERROR) ELEMENT
          IF (ERROR.NE.0) THEN
            PRINT *,'-- gen_inqsymb --'
            PRINT *,'   error reading index from string ''',
     &                 INSYMBOL(C1:C2), ''''
            IERR = -1
            RETURN
          END IF

        ELSE IF (GEN_EFORMAT (INSYMBOL(C1:C2),
     &                        IDIGITS, FDIGITS, EDIGITS)) THEN
          PRINT *,'-- gen_inqsymb --'
          PRINT *, '   constant array index must be integer'
          IERR = -1
          RETURN

        ELSE IF (GEN_DFORMAT (INSYMBOL(C1:C2),
     &                        IDIGITS, FDIGITS, EDIGITS)) THEN
          PRINT *,'-- gen_inqsymb --'
          PRINT *, '   constant array index must be integer'
          IERR = -1
          RETURN

        ELSE
CD        PRINT *, '   evaluating array index: ', INSYMBOL(C1:C2)
          CALL GEN_EVAL_AE (INSYMBOL(C1:C2), 'I4', ELEMENT, IERR)
          IF (IERR.NE.0) RETURN
CD        PRINT *, '   array index result = ', ELEMENT

        END IF

      ELSE
        ELEMENT = 0
      END IF

* Construct FORMAT for reading type size
      WRITE( FORMAT, '(''(I'', I3, '')'')' ) GEN_ILEN(TYPE)-1
      READ (TYPE(2:), FORMAT, IOSTAT=ERROR) NCH
      IF (ERROR.NE.0) THEN
        PRINT *, '-- gen_inqsymb --'
        PRINT *, '   error reading # bytes from type string ',TYPE
        IERR = 99
        RETURN
      END IF

CD    PRINT *, '   symbol length and element number: ', NCH, ELEMENT

      IF (ELEMENT.NE.0) THEN
        ADDRESS = ADDRESS + NCH*(ELEMENT-1)
        LENGTH  = 1
      END IF

      RETURN
      END
