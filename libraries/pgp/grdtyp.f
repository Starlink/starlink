      INTEGER FUNCTION GRDTYP(DEV)
*+
*     - - - - - - - -
*       G R D T Y P     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Returns the device type of the specified device
*
*   Given:
*        DEV      c(*)        Device name
*
*   Returned:
*        Function value    i     Device type
*
*   D.L.Terrett  Starlink  Jul 1988
*+
      IMPLICIT NONE

      CHARACTER*(*) DEV

      INCLUDE 'SAE_PAR'


      INTEGER IERR, IWKTYP, ICON

      CALL ERR_MARK
      IERR = SAI__OK
      CALL gns_TNG(DEV,IWKTYP,ICON,IERR)
      IF (IERR.NE.SAI__OK) THEN
         GRDTYP = 0
      ELSE
         GRDTYP = IWKTYP
      END IF
      CALL ERR_ANNUL(IERR)
      CALL ERR_RLSE
      END
