      SUBROUTINE IUE_APRN( APER, IAPER, STATUS )

*+
*
*   Name:
*      SUBROUTINE IUE_APRN
*
*   Description:
*      Provide aperture number from name.
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
      BYTE APER( 16 )    ! aperture name

*   Export:
      INTEGER IAPER      ! aper index
      INTEGER STATUS     ! status return

*   External references:
      LOGICAL STR_SIMLR  ! caseless string equality

*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( STR_SIMLR( 'SAP\\', APER ) ) THEN
         IAPER = 1

      ELSE IF ( STR_SIMLR( 'LAP\\', APER ) ) THEN
         IAPER = 2

      ELSE IF ( STR_SIMLR( 'BAP\\', APER ) ) THEN
         IAPER = 3

      ELSE
         STATUS = -3
      END IF
      END
