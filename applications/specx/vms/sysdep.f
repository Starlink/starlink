*-----------------------------------------------------------------------

      SUBROUTINE UGETVM (NBYTES, IPTR, STATUS)

*  Routine to grab some virtual memory, using system services

      IMPLICIT    NONE

*     Formal parameters:

      INTEGER     NBYTES
      INTEGER     IPTR
      INTEGER     STATUS

*  Ok, go...

      CALL PSX_MALLOC (NBYTES, IPTR, STATUS)

      IF (STATUS.ne.0) THEN
        TYPE *, ' -- ugetvm --'
        TYPE *, '    bad status in PSX_MALLOC = ', STATUS
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE UFREEVM (IPTR, STATUS)

*  Routine to free virtual memory, using system services

      IMPLICIT    NONE

*     Formal parameters:

      INTEGER     IPTR
      INTEGER     STATUS

*  Ok, go...

      CALL PSX_FREE (IPTR, STATUS)

      IF (STATUS.ne.0) THEN
        TYPE *, ' -- ufreevm --'
        TYPE *, '    bad status in PSX_FREE = ', STATUS
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE UTRNLOG (LOGNAME, FILENAME, STATUS)

*  Uses system services to find a translation for a logical name.

      IMPLICIT    NONE

*     Formal parameters:

      CHARACTER   LOGNAME*(*)
      CHARACTER   FILENAME*(*)
      INTEGER     STATUS

*  Ok, go...

      CALL PSX_GETENV (LOGNAME, FILENAME, STATUS)

      IF (STATUS.ne.0) THEN
        TYPE *, ' -- utrnlog --'
        TYPE *, '    bad status in PSX_GETENV = ', STATUS
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE UGETTIME (TIME, STATUS)

*  Subroutine to return character time in form hh:mm:ss.s
*  using available system services

      IMPLICIT    NONE

*     Formal parameters:

      CHARACTER   TIME*(*)
      INTEGER     STATUS

*     Local variables (if any):

      INTEGER     NTICKS
      CHARACTER   CTIME*32

*  Ok, go...

      CALL PSX_TIME  (NTICKS, STATUS)
      IF (STATUS.ne.0) THEN
        TYPE *, ' -- ugettim --'
        TYPE *, '    bad status in PSX_TIME = ', STATUS
        RETURN
      END IF

      CALL PSX_CTIME (NTICKS, CTIME, STATUS)

      IF (STATUS.ne.0) THEN
        TYPE *, ' -- ugettim --'
        TYPE *, '    bad status in PSX_CTIME = ', STATUS
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

*     Formal parameters:

      CHARACTER   DATE*(*)
      INTEGER     STATUS

*     Local variables (if any):

      INTEGER     NTICKS
      CHARACTER   CTIME*32

*  Ok, go...

      CALL PSX_TIME  (NTICKS, STATUS)

      IF (STATUS.ne.0) THEN
        TYPE *, ' -- ugetdat --'
        TYPE *, '    bad status in PSX_TIME = ', STATUS
        RETURN
      END IF

      CALL PSX_CTIME (NTICKS, CTIME, STATUS)

      IF (STATUS.ne.0) THEN
        TYPE *, ' -- ugetdat --'
        TYPE *, '    bad status in PSX_CTIME = ', STATUS
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

*     Formal parameters:

      CHARACTER   DATTIM*(*)
      INTEGER     STATUS

*     Local variables (if any):

      INTEGER     NTICKS
      CHARACTER   CTIME*32

*  Ok, go...

      CALL PSX_TIME  (NTICKS, STATUS)

      IF (STATUS.ne.0) THEN
        TYPE *, ' -- ugetdattim --'
        TYPE *, '    bad status in PSX_TIME = ', STATUS
        RETURN
      END IF

      CALL PSX_CTIME (NTICKS, CTIME, STATUS)

      IF (STATUS.ne.0) THEN
        TYPE *, ' -- ugetdattim --'
        TYPE *, '    bad status in PSX_CTIME = ', STATUS
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
