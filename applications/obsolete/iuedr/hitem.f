      SUBROUTINE HITEM( IAPER, STATUS )
*+
*
*   Name:
*      SUBROUTINE HITEM
*
*   Description:
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          03-OCT-88     IUEDR Vn. 2.0
*      Martin Clayton     08-JUL-94     IUEDR Vn. 3.1-1
*
*   Method:
*      A slow coordinate transform is used.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Import:
      INTEGER IAPER      ! aperture index

*   Export:
      INTEGER STATUS     ! status return

*   Global variables:
      INCLUDE 'CMFACE'
      INCLUDE 'CMHEAD'

*   Local variables:

*   Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Move template around
      CALL MVDISP( IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT('Error: moving dispersion constants\\', STATUS)
         GO TO 999
      END IF

*   Define ROTRAN
      CALL SETUV( 90.0d0 - ANGLE, DBLE( CENTRE( 1 ) ),
     :            DBLE( CENTRE( 2 ) ) )

 999  CONTINUE

      END
