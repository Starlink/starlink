      SUBROUTINE CONST(A,D, STATUS )

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)

*+
*   Sets up Plate Constants for use by Routine
*   PROJ.
*
*   Gets
*   ----
*      A - Plate Centre R.A. in Radians
*      D -   "     "    DEC.  "    "
*
*   Returns
*   -------
*      Derived Plate Constants are put in the Common
*      Block 'PCONST'

*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*   Note - Based on Routine in Program A06E written by W.Nicholson

*  History:
*     Sometime (UNK):
*        Original version.
*     2-MAR-1993 (AJJB):
*        STATUS argument added.
*-

*  Global constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

      COMMON/PCONST/SA,CA,SD,CD,CASD,SASD,CACD,SACD
      COMMON/CONVF/TWOPI,HALFPI,RDSA,RDST,RDDG

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      SA   = SIN(A)
      CA   = COS(A)
      SD   = SIN(D)
      CD   = COS(D)
      CASD = CA * SD
      SASD = SA * SD
      CACD = CA * CD
      SACD = SA * CD
      END
