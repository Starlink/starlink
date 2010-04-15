*  History:
*     19 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
C-----------------------------------------------------------------------------
C
      SUBROUTINE ASK_PLOT_DEVICE (IOLD, INEW, TERM)
C
C subroutine to prompt user for a new plot device.
C
C Arguments:
C
C      IOLD  (integer, given)      old plot device no:
C                                     0-5 current device
C                                      -1 assume no device selected
C      INEW  (integer, returned)   new plot device no.
C
C      TERM  (logical, returned) true if device is a terminal

C R. Prestage, JACH, 15th February 1989
C R. Padman, MRAO, 25th January, 1990
C
C based entirely on code in SET-PLOT-DEVICE / SEE-PLOT,
C but implemented to facilitate graphics independence.
C
C "native" MONGO version.


      IMPLICIT  NONE

      LOGICAL   TERM
      INTEGER   IOLD, INEW, JDEF
      CHARACTER IANS*1

      INCLUDE  'FLAGCOMM'

      IF (IOLD.EQ.0) THEN
        IANS = 'N'
      ELSE IF (IOLD.LE.19) THEN
        IANS = 'T'
      ELSE IF (IOLD.GE.20) THEN
        IANS = 'H'
      END IF

      IF (IOLD.GE.0 .AND. IOLD.LE.23) THEN

C prompt for new device, given old device

        CALL GEN_GETSTR('Terminal / Hardcopy / Null (T/H/N)',
     &                   IANS, 'A1', IANS, JDEF)

      ELSE

C ask for new device with no prompt

        CALL GEN_GETSTR('Terminal / Hardcopy / Null (T/H/N)',
     &                  ' ', ' ', IANS, JDEF)

      END IF

C generate new plot device from answer

      CALL UUCASE  (IANS)
      CALL GETDEV  (TERMDEV, PRINTDEV, IANS, INEW)

      TERM = IANS.EQ.'T'

*     print *
*     print *, '-- ask_plot_device --'
*     print *, '    default device number: ', iold
*     print *, '    final device number:   ', inew
*     if (term) print *, '    device is a terminal.'
*     print *

      RETURN
      END
