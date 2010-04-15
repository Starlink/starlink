*
*  18-Mar-2003 (rpt)
*               Modified UTRNLOG slightly. Added UPUTLOG
*
*-----------------------------------------------------------------------

      SUBROUTINE UGETVM (NBYTES, IPTR, STATUS)

*  Routine to grab some virtual memory, using system services

      IMPLICIT    NONE
      INCLUDE 'SAE_PAR'

*     Formal parameters:

      INTEGER     NBYTES
      INTEGER     IPTR
      INTEGER     STATUS

*  Ok, go...
      STATUS = SAI__OK

      CALL PSX_MALLOC (NBYTES, IPTR, STATUS)

      IF (STATUS.NE.SAI__OK) THEN
        PRINT *, ' -- ugetvm --'
        PRINT *, '    bad status in PSX_MALLOC = ', STATUS
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE UFREEVM (IPTR, STATUS)

*  Routine to free virtual memory, using system services

      IMPLICIT    NONE
      INCLUDE 'SAE_PAR'

*     Formal parameters:

      INTEGER     IPTR
      INTEGER     STATUS

*  Ok, go...

      STATUS = SAI__OK

      CALL PSX_FREE (IPTR, STATUS)

      IF (STATUS.NE.SAI__OK) THEN
        PRINT *, ' -- ufreevm --'
        PRINT *, '    bad status in PSX_FREE = ', STATUS
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE UTRNLOG (LOGNAME, VALUE, STATUS)

*  Uses system services to find a translation for a logical name.

      IMPLICIT    NONE
      INCLUDE 'PSX_ERR'
      INCLUDE 'SAE_PAR'

*     Formal parameters:

      CHARACTER   LOGNAME*(*)
      CHARACTER   VALUE*(*)
      INTEGER     STATUS

*  External Functions:
      INTEGER GEN_ILEN

*  Ok, go...

      STATUS = SAI__OK

      CALL PSX_GETENV (LOGNAME, VALUE, STATUS)

      IF (STATUS.NE.SAI__OK .AND. STATUS.NE.PSX__NOENV) THEN
        PRINT *, ' -- utrnlog --'
        PRINT *, '    bad status in PSX_GETENV = ', STATUS
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE UPUTLOG (LOGNAME, VALUE, STATUS)

*  Uses system services to find set a logical name. It checks the result
*  by reading the variable back.

      IMPLICIT    NONE
      INCLUDE 'PSX_ERR'
      INCLUDE 'SAE_PAR'

*     Formal parameters:

      CHARACTER     LOGNAME*(*)
      CHARACTER     VALUE*(*)
      INTEGER       STATUS

      CHARACTER*132 VALUE2

*  External Functions:
      INTEGER GEN_ILEN

*  Ok, go...
      CALL PSX_PUTENV( LOGNAME, VALUE, STATUS )

*     Now check that it worked
      IF (STATUS .EQ. SAI__OK) THEN

*        We stored something
         CALL UTRNLOG( LOGNAME, VALUE2, STATUS )

         IF (STATUS .EQ. SAI__OK) THEN
            PRINT *, '   Logical ',
     &           LOGNAME(:GEN_ILEN(LOGNAME)),
     &           ' now set to: "',VALUE2(:GEN_ILEN(VALUE2)),'"'
         ELSE
            PRINT *, ' -- uputlog --'
            PRINT *,
     &      '    Error reading back environment variable ',
     &      LOGNAME(:GEN_ILEN(LOGNAME))
         END IF
      ELSE
        PRINT *, ' -- uputlog --'
        PRINT *, '    Error setting ',
     &           LOGNAME(:GEN_ILEN(LOGNAME)),
     &           ' environment variable'
        PRINT *, '    bad status in PSX_PUTENV = ', STATUS

      ENDIF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE UGETTIME (TIME, STATUS)

*  Subroutine to return character time in form hh:mm:ss.s
*  using available system services

      IMPLICIT    NONE
      INCLUDE 'SAE_PAR'

*     Formal parameters:

      CHARACTER   TIME*(*)
      INTEGER     STATUS

*     Local variables (if any):

      INTEGER     NTICKS
      CHARACTER   CTIME*32

*  Ok, go...

      STATUS = SAI__OK

      CALL PSX_TIME  (NTICKS, STATUS)
      IF (STATUS.NE.SAI__OK) THEN
        PRINT *, ' -- ugettim --'
        PRINT *, '    bad status in PSX_TIME = ', STATUS
        RETURN
      END IF

      CALL PSX_CTIME (NTICKS, CTIME, STATUS)

      IF (STATUS.NE.SAI__OK) THEN
        PRINT *, ' -- ugettim --'
        PRINT *, '    bad status in PSX_CTIME = ', STATUS
        RETURN
      END IF

      TIME = CTIME(12:19)//'.0'

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE UGETDATE (DATE, STATUS)

*  Subroutine to return character date in form dd-mon-yy
*  using available system services

      IMPLICIT    NONE
      INCLUDE 'SAE_PAR'

*     Formal parameters:

      CHARACTER   DATE*(*)
      INTEGER     STATUS

*     Local variables (if any):

      INTEGER     NTICKS
      CHARACTER   CTIME*32

*  Ok, go...
      STATUS = SAI__OK

      CALL PSX_TIME  (NTICKS, STATUS)

      IF (STATUS.NE.SAI__OK) THEN
        PRINT *, ' -- ugetdat --'
        PRINT *, '    bad status in PSX_TIME = ', STATUS
        RETURN
      END IF

      CALL PSX_CTIME (NTICKS, CTIME, STATUS)

      IF (STATUS.NE.SAI__OK) THEN
        PRINT *, ' -- ugetdat --'
        PRINT *, '    bad status in PSX_CTIME = ', STATUS
        RETURN
      END IF

      DATE = CTIME(9:10) // '-' // CTIME(5:7) // '-' // CTIME(23:24)
      CALL CHR_UCASE (DATE)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE UGETDATTIM (DATTIM, STATUS)

*  Subroutine to return character date/time in form 'Day Mon dd hh:mm:ss yyyy'
*  using available system services

      IMPLICIT    NONE
      INCLUDE 'SAE_PAR'

*     Formal parameters:

      CHARACTER   DATTIM*(*)
      INTEGER     STATUS

*     Local variables (if any):

      INTEGER     NTICKS
      CHARACTER   CTIME*32

*  Ok, go...

      STATUS = SAI__OK

      CALL PSX_TIME  (NTICKS, STATUS)

      IF (STATUS.NE.SAI__OK) THEN
        PRINT *, ' -- ugetdattim --'
        PRINT *, '    bad status in PSX_TIME = ', STATUS
        RETURN
      END IF

      CALL PSX_CTIME (NTICKS, CTIME, STATUS)

      IF (STATUS.NE.SAI__OK) THEN
        PRINT *, ' -- ugetdattim --'
        PRINT *, '    bad status in PSX_CTIME = ', STATUS
        RETURN
      END IF

      DATTIM = CTIME(1:24)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE ULDBLK (STRING)

*  Subroutine to remove leading blanks from a string using available
*  system services.

      IMPLICIT    NONE

*     Formal parameters:

      CHARACTER*(*)  STRING

*  Ok, go...

      CALL CHR_LDBLK (STRING)

      RETURN
      END

*-----------------------------------------------------------------------
