*+  CGS3DR_GENGRPNAME - Generate a filename for a reduced CGS3 group
      SUBROUTINE CGS3DR_GENGRPNAME (NUMBER, FILENAME, STATUS)
*    Description:
*     Given a run number this generates a file name for a reduced
*     CGS3 group data file. This will have the form RODIR:GRPnnnn.
*    Invocation:
*     CALL CGS3DR_GENGRPNAME (NUMBER, FILENAME, STATUS)
*    Parameters:
*     NUMBER = INTEGER (READ)
*           Run Number of data
*     FILENAME = CHARACTER*(*) (WRITE)
*           Generated filename of reduced data
*     STATUS = INTEGER (UPDATE)
*           ADAM Status return
*    Method:
*     If status bad then return
*     Generate the name from the RODIR directory and the number
*    Deficiencies:
*     None Known
*    Bugs:
*     None Known
*    Authors:
*     Alan Bridger (JAC::AB)
*    History:
*     14-Dec-92: Original (JAC::AB)
*     20-Dec-95: change : to /, GRP to grp (KK)
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

*    reduced data directory
      LENGTH = CHR_LEN(RODIR)

*    Create the name
      FILE(1:3) = 'grp'
      WRITE (FILE(4:7), '(I4.4)') NUMBER

*    If the directory ends with a "]" then it is already a valid directory
*    just append the filename to it
      IF (RODIR(LENGTH:LENGTH) .EQ. ']') THEN
         FILENAME = RODIR(:LENGTH)//FILE(:7)

      ELSE
*       if it ends with a "/" then leave it - assume its a logical,
*       if not then append a "/" - still assuming its a logical!
         IF (RODIR(LENGTH:LENGTH) .EQ. '/') THEN
            FILENAME = RODIR(:LENGTH)//FILE(:7)
         ELSE
            FILENAME = RODIR(:LENGTH)//'/'//FILE(:7)
         END IF
      END IF


      END
