*+  STR_OBVAL - returns value of HDS object formatted into a string
      SUBROUTINE STR_OBVAL(LOC,STR,STATUS)
*    Description :
*     returns the value of the object pointed to by LOC in
*     a character string - returns blank if locator invalid
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC) LOC
*    Export :
      CHARACTER*(*) STR
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*80 STRING         ! string buffer
      CHARACTER*(DAT__SZTYP) TYPE ! data type
      INTEGER IVAL                ! integer,byte or word value
      REAL RVAL                   ! real value
      DOUBLE PRECISION DVAL       ! double precision value
      LOGICAL VALID		  ! whether locator valid
      LOGICAL LVAL                ! logical value
      LOGICAL INTYP               ! whether of type byte,word or integer
      LOGICAL SET                 ! whether value set
*    Local Constants :
      CHARACTER*1 BLANK
      PARAMETER (BLANK=' ')
      CHARACTER*80 FMT1,FMT2,FMT3,FMT4
      PARAMETER (FMT1='(I12)',
     &           FMT2='(1PG15.7)',
     &           FMT3='(1PG20.12)',
     &           FMT4='(L8)')
*    Local data :
*-

*    Check status
      IF ( STATUS.NE. SAI__OK ) RETURN

*    blank out string to start with
      STRING=BLANK

*  check if locator valid
      CALL DAT_VALID(LOC,VALID,STATUS)
      IF (VALID) THEN
*  check if object has a value
        CALL DAT_STATE(LOC,SET,STATUS)

*  if it has a value then according to type convert to a
*  string and position it at a set column
        IF (SET) THEN

          CALL DAT_TYPE(LOC,TYPE,STATUS)
*  combined integer types
          INTYP=(TYPE.EQ.'_INTEGER'.OR.TYPE.EQ.'_BYTE'
     &         .OR.TYPE.EQ.'_UBYTE'.OR.TYPE.EQ.'_WORD'
     &         .OR.TYPE.EQ.'_UWORD')
          IF (INTYP) THEN

            CALL DAT_GET0I(LOC,IVAL,STATUS)
            WRITE(STRING,FMT1) IVAL

*  reals
          ELSEIF (TYPE.EQ.'_REAL') THEN
            CALL DAT_GET0R(LOC,RVAL,STATUS)
            WRITE(STRING,FMT2) RVAL

*  double precision
          ELSEIF (TYPE.EQ.'_DOUBLE') THEN
            CALL DAT_GET0D(LOC,DVAL,STATUS)
            WRITE(STRING,FMT3) DVAL

*  logicals
          ELSEIF (TYPE.EQ.'_LOGICAL') THEN
            CALL DAT_GET0L(LOC,LVAL,STATUS)
            WRITE(STRING,FMT4) LVAL

*  character types
          ELSEIF (TYPE(:5).EQ.'_CHAR') THEN
            CALL DAT_GET0C(LOC,STRING,STATUS)
          ENDIF

*  if no value set
          ELSE
            STRING='   Not set'
          ENDIF
        ENDIF
        STR=STRING
        END
