*  History:
*     28 July 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      BLOCK DATA INIGEN_UNITS

*     Logical unit pool
*     Entry is .TRUE. if the unit is allocated with UGETLUN.

      INTEGER     NUNITS
      PARAMETER  (NUNITS = 40)

      LOGICAL     UNIT_POOL
      INTEGER     UOFFSET
      COMMON /GEN_UNITS/ UNIT_POOL(NUNITS), UOFFSET
      SAVE   /GEN_UNITS/

      DATA  UOFFSET   /20/
      DATA  UNIT_POOL /NUNITS * .FALSE./

      END

*-----------------------------------------------------------------------

      SUBROUTINE UGETLUN (LUN, STATUS)

*  Routine to use system services to get a logical unit

      IMPLICIT    NONE

*     Formal parameters:

      INTEGER     LUN
      INTEGER     STATUS

*     Logical unit pool
*     Entry is .TRUE. if the unit is allocated with UGETLUN.

      INTEGER     NUNITS
      PARAMETER  (NUNITS = 40)

      LOGICAL     UNIT_POOL
      INTEGER     UOFFSET
      COMMON /GEN_UNITS/ UNIT_POOL(NUNITS), UOFFSET
      SAVE   /GEN_UNITS/

      EXTERNAL    INIGEN_UNITS

*     Local variables:

      LOGICAL     UOPENED

*  Ok, go...

      STATUS = 0

*     Find a unit which is not in use:

      LUN     =  NUNITS + UOFFSET
      UOPENED = .TRUE.

      DO WHILE (LUN.GE.UOFFSET .AND. UOPENED)
        IF (.NOT. UNIT_POOL(LUN-UOFFSET)) THEN
          INQUIRE (LUN, OPENED=UOPENED)
        END IF
        IF (UOPENED) THEN
          LUN = LUN - 1
        ELSE
          UNIT_POOL(LUN-UOFFSET) = .TRUE.
        END IF
      END DO

*     If we get to here and UOPENED is still true, then
*     no logical unit is available...

      IF (UOPENED) THEN
        STATUS = 1
        PRINT *, ' -- ugetlun --'
        PRINT *, '    bad status in UGETLUN = ', STATUS,
     &              'No unit available'
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE UFREELUN (LUN, STATUS)

*  Routine to use system services to return logical unit to the pool

      IMPLICIT    NONE

*     Formal parameters:

      INTEGER     LUN
      INTEGER     STATUS

*     Logical unit pool
*     Entry is .TRUE. if the unit is allocated with UGETLUN.

      INTEGER     NUNITS
      PARAMETER  (NUNITS = 40)

      LOGICAL     UNIT_POOL
      INTEGER     UOFFSET
      COMMON /GEN_UNITS/ UNIT_POOL(NUNITS), UOFFSET
      SAVE   /GEN_UNITS/

*     Local variables

      LOGICAL     UOPENED

*  Ok, go...

      INQUIRE (LUN, OPENED=UOPENED)
      IF (UOPENED) THEN
        STATUS = 1
      ELSE IF (.NOT. UNIT_POOL(LUN-UOFFSET)) THEN
        STATUS = 2
      ELSE
        UNIT_POOL(LUN-UOFFSET) = .FALSE.
        STATUS = 0
      END IF

      IF (STATUS.ne.0) THEN
        PRINT *, ' -- ufreelun --'
        PRINT *, '    bad status in UFREELUN = ', STATUS
        IF (STATUS.eq.1) THEN
          PRINT *, '    File still open on unit'
        ELSE IF (STATUS.eq.2) THEN
          PRINT *, '    Unit not allocated by UGETLUN'
        END IF
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE UUCASE (STRING)

*  Subroutine to convert entire string to upper case using available
*  system services.

      IMPLICIT    NONE

*     Formal parameters:

      CHARACTER*(*)  STRING

*     Local variables:

      INTEGER     I
      INTEGER     ICH

*  Ok, go...

C     PRINT *, ' --- uucase ---'
C     PRINT *, '     input string  = ', string(:GEN_ILEN(STRING))

      DO I = 1, LEN (STRING)
        ICH = ICHAR (STRING(I:I))
        IF (ICH.ge.097 .AND. ICH.le.122) STRING(I:I) = CHAR (ICH-32)
      END DO

C     PRINT *, '     output string = ', string(:GEN_ILEN(STRING))


      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE ULCASE (STRING)

*  Subroutine to convert entire string to lower case using available
*  system services.

      IMPLICIT    NONE

*     Formal parameters:

      CHARACTER*(*)  STRING

*     Local variables:

      INTEGER     I
      INTEGER     ICH

*  Ok, go...

C     PRINT *, ' --- ulcase ---'
C     PRINT *, '     input string  = ', string(:GEN_ILEN(STRING))

      DO I = 1, LEN (STRING)
        ICH = ICHAR (STRING(I:I))
        IF (ICH.ge.065 .AND. ICH.le.90) STRING(I:I) = CHAR (ICH+32)
      END DO

C     PRINT *, '     output string = ', string(:GEN_ILEN(STRING))


      RETURN
      END

*-----------------------------------------------------------------------
