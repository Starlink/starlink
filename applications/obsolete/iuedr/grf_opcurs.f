      SUBROUTINE grf_OPCURS( STATUS )

*+
*
*   Name:
*      SUBROUTINE grf_OPCURS
*
*   Description:
*      Open display for access by cursor.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      05-MAY-82
*         AT4 version.
*      Paul Rees          14-JAN-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*         Conversion to GKS 7.2 graphics.
*      Paul Rees          09-MAY-89     IUEDR Vn. 2.1
*         Some restructuring and final conversion to SGP/16 style.
*      Martin Clayton     24-OCT-94     IUEDR Vn. 3.2
*
*   Method:
*      Check that there is a current graph for cursor operation mode.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Export:
      INTEGER STATUS   ! status return

*   Global variables:
      INCLUDE 'CMGRAF'

*   Local variables:
      LOGICAL CURAV    ! cursor available

*   Determine if current workstation has cursor facility
      CALL sgs_ICUAV( CURAV )

*   No graphics cursor available
      IF ( .NOT. CURAV ) THEN
         CALL ERROUT('Error: device has no cursor\\', STATUS)

*   No plot currently drawn
      ELSE IF ( .NOT.DRAWN .AND. .NOT.IDRAWN ) THEN
         CALL ERROUT('Error: no graph drawn\\', STATUS)
      END IF

      END
