*+  CGS3DR_GENRAWNAME - Generate a filename for a raw CGS3 file
      SUBROUTINE CGS3DR_GENRAWNAME (NUMBER, FILENAME, STATUS)
*    Description:
*     Given a run number this generates a file name for a raw CGS3 data file.
*    Invocation:
*     CALL CGS3DR_GENRAWNAME (NUMBER, FILENAME, STATUS)
*    Parameters:
*     NUMBER = INTEGER (READ)
*           Run Number of raw data
*     FILENAME = CHARACTER*(*) (WRITE)
*           Generated filename of raw data
*     STATUS = INTEGER (UPDATE)
*           ADAM Status return
*    Method:
*     If status bad then return
*     Using the date and directories obtained in CGS3DR_INIT generate what
*     the raw data file name must be.
*    Deficiencies:
*     None Known
*    Bugs:
*     None Known
*    Authors:
*     Alan Bridger (JAC::AB)
*    History:
*     14-Dec-92: Original (JAC::AB)
*     15-Dec-92: Directories already known from INIT (JAC::AB)
*    Type Definitions:
      IMPLICIT NONE
*    Global constants:
      INCLUDE 'SAE_PAR'
*    Import:
      INTEGER NUMBER
*    Import-Export:
*     None
*    Export:
      CHARACTER*(*) FILENAME
*    Status:
      INTEGER STATUS
*    External references:
      INTEGER CHR_LEN                 ! Length of string
*    Global variables:
      INCLUDE 'CGS3DR_CMN'
*    Local Constants:
*     None
*    Local variables:
      CHARACTER*40    FILE            ! String containing filename only
      INTEGER         LENGTH          ! Length of string
*    Internal References:
*     None
*    Local data:
*     None
*-

*    If status bad then return
      IF (STATUS .NE. SAI__OK) RETURN

*    raw data directory
      LENGTH = CHR_LEN(DATADIR)

*    Create the name
      FILE(1:5) = UTDAYMONTH
      WRITE (FILE(6:9), '(I4.4)') NUMBER

*    If the directory ends with a "]" then it is already a valid directory
*    just append the filename to it
      IF (DATADIR(LENGTH:LENGTH) .EQ. ']') THEN
         FILENAME = DATADIR(:LENGTH)//FILE(:9)

      ELSE
*       if it ends with a "/" then leave it - assume its a logical,
*       if not then append a "/" - still assuming its a logical!
         IF (DATADIR(LENGTH:LENGTH) .EQ. '/') THEN
            FILENAME = DATADIR(:LENGTH)//FILE(:9)
         ELSE
            FILENAME = DATADIR(:LENGTH)//'/'//FILE(:9)
         END IF
      END IF
      CALL CHR_LCASE( FILENAME )

      END
