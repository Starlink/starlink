      SUBROUTINE CRSPEC( DSNEW, STATUS )

*+
*
*   Name:
*      SUBROUTINE CRSPEC
*
*   Description:
*      Create a new data file and write contents. This for case with
*      spectrum, but no image.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          22-SEP-88     IUEDR Vn. 2.0
*      Martin Clayton     15-AUG-94     IUEDR Vn. 3.1-2
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Import:
      BYTE DSNEW(81)     ! file name

*   Export:
      INTEGER STATUS     ! status return

*   Global variables:
      INCLUDE 'CMFILE'
      INCLUDE 'CMHEAD'

      NOHEAD = .FALSE.
      CACHAN = .TRUE.
      SPCHAN = .TRUE.

*   Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL WRIMG( DSNEW, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL STR_MOVE( DSNEW, 81, DSNAME )
         NODSN = .FALSE.
      END IF
      END
