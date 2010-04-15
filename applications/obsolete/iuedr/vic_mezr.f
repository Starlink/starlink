      SUBROUTINE VIC_MEZR(RECLEN, DATA, STATUS)

*+
*
*   Name:
*      SUBROUTINE VIC_MEZR
*
*   Description:
*      Translate the Record Zero contents.
*
*   History:
*      Jack Giddings      02-DEC-81     AT4 version
*      Paul Rees          07-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*      The first data line containing "Record Zero" is stored in the
*      common blocks CMUEZ1 and CMUEZ2. Only the new ground station
*      software contains CMUEZ2 data. The data are accessed from memory.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER RECLEN             ! record size in short words

      INTEGER*2 DATA(RECLEN)     ! data array

*   Export:
      INTEGER STATUS             ! status return

*   Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMSPC'

*   Translate CMUEZ1 contents
      CALL VIC_MEZ1(RECLEN, DATA, STATUS)
      IF (STATUS.NE.0) RETURN

*   Read newly added parts of Record Zero
      IF (SWVER.EQ.2) THEN
         CALL VIC_MEZ2(RECLEN, DATA, STATUS)
         IF (STATUS.NE.0) RETURN

*   Default missing things
      ELSE
         YEAR = 0
         MONTH = 0
         DAY = 0
         THDA = 0.0
      END IF
      END
