      SUBROUTINE TAPE_FIND( NEXFIL, FILE, STATUS )
*+
*   Name:
*      SUBROUTINE TAPE_FIND
*
*   Description:
*      Find next or specific tape file.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      13-OCT-81
*         AT4 version.
*      Paul Rees          26-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          01-JUN-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMTAPE'

*   Import:
      LOGICAL NEXFIL     ! whether next file found is wanted

*   Import-Export:
      INTEGER FILE       ! IPCS file wanted/found

*   Export:
      INTEGER STATUS     ! status return

*   Local Variables:
      INTEGER BLOCK
      INTEGER CFILE

      LOGICAL START
      LOGICAL MOVED
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get tape position.
      CALL MAG_POS( TCHAN, CFILE, START, BLOCK, MOVED, STATUS )

      IF ( NEXFIL ) THEN
         FILE = CFILE
      END IF

*   Move to start of this file.
      CALL MAG_MOVE( TCHAN, FILE, .TRUE., 1, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error moving to tape file\\', STATUS )
      END IF

 999  CONTINUE

      END
