*  History:
*     16 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*-----------------------------------------------------------------------

      SUBROUTINE GEN_PUT_SYMB (INSYMBOL, INTYPE, VALUE, IERR)

*  Routine intended to provide easy access to symbol table with
*  straightforward type conversion.

      IMPLICIT   NONE

*     Formal parameters

      CHARACTER INSYMBOL*(*)
      CHARACTER INTYPE*(*)
      REAL*4    VALUE
      INTEGER*4 IERR

      INTEGER*4 TABLE_ADDRESS
      INTEGER*4 LENGTH_ADDRESS
      COMMON /GEN_SYMBOLS/ TABLE_ADDRESS, LENGTH_ADDRESS

      CHARACTER SYMBOL*16
      INTEGER*4 SYM_INDEX
      CHARACTER SYM_TYPE*4
      INTEGER*4 SYM_SIZE
      INTEGER*4 SYM_LEN
      LOGICAL*4 READONLY
      INTEGER*4 SYM_ADDR

      INTEGER*4 NBYTES
      INTEGER*4 ILT
      INTEGER*4 IOFF
      INTEGER*4 ICH1, ICH2
 
      LOGICAL*4 L4IN, L4OUT
      INTEGER*4 I4IN, I4OUT
      REAL*4    R4IN, R4OUT
      REAL*8    R8IN, R8OUT
      EQUIVALENCE (L4IN,  I4IN,  R4IN,  R8IN)
      EQUIVALENCE (L4OUT, I4OUT, R4OUT, R8OUT)
      
      INTEGER*4 GEN_ILEN

*     OK, Go...

      SYMBOL = INSYMBOL
      CALL UUCASE (SYMBOL)

*     Check if it is an array and find array index

      CALL GET_SUBEXPR (SYMBOL, ICH1, ICH2, IERR)
      IF (IERR.EQ.0) THEN
        IF (ICH1.GT.0 .AND. ICH2.GE.ICH1) THEN
          CALL GEN_EVAL_AE (SYMBOL(ICH1:ICH2), 'I4', IOFF, IERR)
          IF (IERR.NE.0) THEN
            TYPE *,'-- gen_putsymb --'
            TYPE *,'  array subscript not evaluable!'
            RETURN
          END IF
        ELSE
          IOFF = 1
          ICH1 = GEN_ILEN (SYMBOL) + 2
          ICH2 = ICH1
        END IF
      ELSE
        RETURN
      END IF

      CALL GEN_INQSYMB1 (%VAL(TABLE_ADDRESS), %VAL(LENGTH_ADDRESS),
     &                   SYMBOL(:ICH1-2),
     &                   SYM_INDEX, SYM_TYPE, SYM_LEN, SYM_ADDR,
     &                   READONLY, IERR)

      IF (SYM_INDEX.EQ.0) THEN
        TYPE *, SYMBOL, ' not defined'
        IERR = 1
        RETURN
      ELSE IF (READONLY) THEN
        TYPE *, SYMBOL, ' readonly'
        IERR = 1
        RETURN
      END IF

      ILT = GEN_ILEN (SYM_TYPE)
      READ (SYM_TYPE(2:ILT), '(I)', IOSTAT=IERR) NBYTES
      IF (IERR.NE.0) THEN
        CALL GEN_ERMSG (IERR)
        IERR = 18
        RETURN
      END IF

*     Same variable type? Direct write to table.

      IF (SYM_TYPE.EQ.INTYPE) THEN
        CALL GEN_SETSYM1 (%VAL(TABLE_ADDRESS), %VAL(LENGTH_ADDRESS),
     &                    SYM_INDEX, IOFF, VALUE, IERR)

      ELSE IF (SYM_TYPE(:1).EQ.'C' .OR. INTYPE(:1).EQ.'C') THEN
        TYPE *,'--gen_putsymb--'
        TYPE *,'  No type conversion to/from strings!'
        IERR = -1
        RETURN

      ELSE
        CALL XCOPY (NBYTES, VALUE, L4IN)
*       Type conversion is required
        IF (INTYPE.EQ.'L4') THEN
          IF (SYM_TYPE.EQ.'I4') L4OUT = I4IN
          IF (SYM_TYPE.EQ.'R4') L4OUT = R4IN
          IF (SYM_TYPE.EQ.'R8') L4OUT = R8IN
        ELSE IF (INTYPE.EQ.'I4') THEN
          IF (SYM_TYPE.EQ.'L4') I4OUT = L4IN
          IF (SYM_TYPE.EQ.'R4') I4OUT = R4IN
          IF (SYM_TYPE.EQ.'R8') I4OUT = R8IN
        ELSE IF (INTYPE.EQ.'R4') THEN
          IF (SYM_TYPE.EQ.'L4') R4OUT = L4IN
          IF (SYM_TYPE.EQ.'I4') R4OUT = I4IN
          IF (SYM_TYPE.EQ.'R8') R4OUT = R8IN
        ELSE IF (INTYPE.EQ.'R8') THEN
          IF (SYM_TYPE.EQ.'L4') R8OUT = L4IN
          IF (SYM_TYPE.EQ.'I4') R8OUT = I4IN
          IF (SYM_TYPE.EQ.'R4') R8OUT = R4IN
        END IF

        CALL GEN_SETSYM1 (%VAL(TABLE_ADDRESS), %VAL(LENGTH_ADDRESS),
     &                    SYM_INDEX, IOFF, L4OUT, IERR)

      END IF

      RETURN
      END
