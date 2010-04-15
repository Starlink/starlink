*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      SUBROUTINE REFRESH_CLI(STRING)

C   Routine to fill an empty command line with the contents of STRING

      INTEGER*4   GEN_ILEN, GEN_ICHTOT
      CHARACTER   STRING*(*)

      INCLUDE 'CLI_STACK.INC'

      ISTR             = GEN_ILEN   (STRING)
      ICLIST           = GEN_ICHTOT (ISP-1)+1
      CLILINE(ICLIST:) = ' '

CD    print *,'Refresh_CLI...New starting position is ',ICLIST

      IF (ISTR.NE.0) THEN
        CLILINE(ICLIST:) = STRING(:ISTR)//' '
      END IF

      STRING       = ' '
      ICLI (1,ISP) = 1         ! Set pointer to first character in string
      ICLI (2,ISP) = ISTR      ! Length of string for this level

      RETURN
      END
