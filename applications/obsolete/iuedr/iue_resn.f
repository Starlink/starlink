      SUBROUTINE IUE_RESN( RES, IRES, STATUS )

*+
*
*   Name:
*      SUBROUTINE IUE_RESN
*
*   Description:
*      Provide resolution number from name.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          04-OCT-88     IUEDR Vn. 2.0
*      Martin Clayton     18-SEP-94     IUEDR Vn. 3.1-8
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Import:
      BYTE RES(16)       ! res res

*   Export:
      INTEGER IRES       ! res index
      INTEGER STATUS     ! status return

*   External references:
      LOGICAL STR_SIMLR  ! caseless string equality

*   Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( STR_SIMLR( 'LORES\\', RES ) ) THEN
         IRES = 1

      ELSE IF ( STR_SIMLR( 'HIRES\\', RES ) ) THEN
         IRES = 2

      ELSE
         STATUS = -3
      END IF
      END
