      SUBROUTINE SZTYPE(TYPE, BYTES, STATUS)

*+
*
*   Name:
*      SUBROUTINE SZTYPE
*
*   Description:
*      Give size in bytes for TYPE.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      31-DEC-81
*         AT4 version.
*      Paul Rees          20-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          11-MAY-89     IUEDR Vn. 2.1
*         Some restructuring and final conversion to SGP/16 style.
*      Martin Clayton     06-JUL-94     IUEDR Vn. 3.1-1
*         Added SAE_PAR
*
*   Method:
*      TYPE must be one of the primitive alphameric types.
*
*-

*   Implicit:
      IMPLICIT NONE

      INCLUDE 'SAE_PAR'

*   Global constants:
      INTEGER ERR           ! error status
      INTEGER MAXTYPE       ! maximum length of data type string

      PARAMETER (ERR=-3, MAXTYPE=16)

*   Import:
      BYTE TYPE(MAXTYPE)    ! data type (Fortran only)

*   Export:
      INTEGER BYTES         ! bytes per primitive element
      INTEGER STATUS        ! status return

*   External references:
      LOGICAL str_SIMLR     ! caseless string equality

*   Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF (str_SIMLR('char\\', TYPE)) THEN
         BYTES = 1
      ELSE IF (str_SIMLR('ubyte\\', TYPE)) THEN
         BYTES = 1
      ELSE IF (str_SIMLR('byte\\', TYPE)) THEN
         BYTES = 1
      ELSE IF (str_SIMLR('short\\', TYPE)) THEN
         BYTES = 2
      ELSE IF (str_SIMLR('int\\', TYPE)) THEN
         BYTES = 4
      ELSE IF (str_SIMLR('long\\', TYPE)) THEN
         BYTES = 4
      ELSE IF (str_SIMLR('float\\', TYPE)) THEN
         BYTES = 8
      ELSE IF (str_SIMLR('double\\', TYPE)) THEN
         BYTES = 8
      ELSE IF (str_SIMLR('bit\\', TYPE)) THEN
         BYTES = 1
      ELSE
         STATUS = ERR
         BYTES = 0
      END IF

      END
