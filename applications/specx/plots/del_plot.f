*  History:
*     19 Nov 1993 (hme):
*        Replace LIB${GET|FREE}_LUN with FIO_{G|P}UNIT.
*        Use PSX_GETENV to translate SPECXDIR.
*     30 Nov 1993 (hme):
*        Disuse SPECXDIR, use current working directory instead.
*     09 Jan 1994 (rp):
*        Replace FIO_{G|P}UNIT with I{GET|FREE}LUN
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Change DISP= to STATUS= in CLOSE
C-----------------------------------------------------------------------

      SUBROUTINE DEL_PLOT (FILNAM)

      CHARACTER FILNAM*(*)
      LOGICAL   IEXIST,IOPEN

      INTEGER*4 PLOT_UNIT
      COMMON /PLTDEV/ PLOT_UNIT

      INTEGER STATUS

      INTEGER  IGETLUN
      INTEGER  IFREELUN
*

      STATUS = 0

      INQUIRE (FILE=FILNAM, EXIST=IEXIST,
     &         OPENED=IOPEN)
      IF (.NOT.IEXIST)   RETURN

      IF (.NOT.IOPEN) THEN
*       CALL FIO_GUNIT (PLOT_UNIT, STATUS)
        STATUS = IGETLUN (PLOT_UNIT, 'del_plot', .FALSE.)
        OPEN (PLOT_UNIT, FILE=FILNAM, STATUS='OLD',
     &        IOSTAT=STATUS, ERR=100)
      END IF

      CLOSE (PLOT_UNIT, STATUS='DELETE')
*     CALL FIO_PUNIT (PLOT_UNIT, STATUS)
      STATUS = IFREELUN (PLOT_UNIT)

  100 IF(STATUS.NE.0) THEN
        PRINT *,'DEL_PLOT Error:'
        CALL GEN_ERMSG (STATUS)
      END IF

      RETURN
      END


