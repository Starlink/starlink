      SUBROUTINE grf_CLZONE( STATUS )
*+
*
*   Name:
*      SUBROUTINE grf_CLZONE
*
*   Description:
*      Read RS pearameter and reset zone parameters if true.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      05-MAY-82
*         AT4 version.
*      Paul Rees          12-JAN-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*         Conversion to GKS 7.2 graphics.
*      Paul Rees          08-MAY-89     IUEDR Vn. 2.1
*         Some restructuring and final conversion to SGP/16 style.
*      Martin Clayton     23-OCT-94     IUEDR Vn. 3.2
*
*   Method:
*      The RS parameter is read, and if it is TRUE, then the display
*      characteristics are reset.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Global variables:
      INCLUDE 'CMGRAF'

*   Export:
      INTEGER STATUS   ! status return

*   Local variables:
      INTEGER ACTVAL   ! parameter value count

*   Read RESET flag
      CALL RDPARL('RS\\', .FALSE., 1, RESET, ACTVAL, STATUS)
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'RS\\', STATUS )

      ELSE IF ( RESET ) THEN

*      Reset display parameters
         CALL grf_RSGRAF( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: resetting display parameters\\',
     :                   STATUS )
         END IF
      END IF

      END
