*-----------------------------------------------------------------------

      INTEGER*4 FUNCTION GEN_READNUM (STRING, TYPE, FORM, VALUE)

      IMPLICIT  NONE

*     Formal parameters

      CHARACTER STRING*(*)
      CHARACTER TYPE*(*)
      CHARACTER FORM*(*)
      INTEGER*4 VALUE

*     Local variables

      INTEGER*4 NBYTES
      INTEGER*4 ILF
      INTEGER*4 ILS
      INTEGER*4 IERR
      CHARACTER FORM2*32

      LOGICAL*4 LOGICAL
      INTEGER*4 INTEGER
      REAL*4    REAL
      REAL*8    DOUBLE

      EQUIVALENCE (DOUBLE, LOGICAL, INTEGER, REAL)

*     Functions:

      INTEGER*4 GEN_ILEN

*  Ok, go..

      GEN_READNUM = 0

D     Type *, ' --- gen_readnum ---'
D     Type *, '     input format = ', form

      ILF   = GEN_ILEN (FORM)
      ILS   = GEN_ILEN (STRING)
      FORM2 = '(' // FORM(1:ILF) // ')' // ' '

      IF (TYPE.EQ.'L4') THEN
        READ (STRING(1:ILS), FMT=FORM2(1:ILF+2), IOSTAT=IERR) LOGICAL
D       TYPE *, ' Just read logical*4 = ', LOGICAL
      ELSE IF (TYPE.EQ.'I4') THEN
        READ (STRING(1:ILS), FMT=FORM2(1:ILF+2), IOSTAT=IERR) INTEGER
D       TYPE *, ' Just read integer*4 = ', INTEGER
      ELSE IF (TYPE.EQ.'R4') THEN
        READ (STRING(1:ILS), FMT=FORM2(1:ILF+2), IOSTAT=IERR) REAL
D       TYPE *, ' Just read real*4 = ', REAL
      ELSE IF (TYPE.EQ.'R8') THEN
        READ (STRING(1:ILS), FMT=FORM2(1:ILF+2), IOSTAT=IERR) DOUBLE
D       TYPE *, ' Just read real*8 = ', DOUBLE
      END IF

      IF (IERR.NE.0) GO TO 99

      READ (TYPE(2:GEN_ILEN(TYPE)), '(I)', IOSTAT=IERR) NBYTES
      IF (IERR.NE.0) GO TO 99
      CALL XCOPY (NBYTES, LOGICAL, VALUE)
D     TYPE *, ' Just copied ', NBYTES, ' bytes to VALUE'

      RETURN

*     Error return

   99 CONTINUE
      IF (IERR.NE.0) THEN
        TYPE *,'-- gen_readnum --'
        CALL GEN_ERMSG(IERR)
        GEN_READNUM = 10
      END IF

      RETURN
      END
