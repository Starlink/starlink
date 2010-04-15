      SUBROUTINE CON_FACTOR( STATUS )

*+
*  Name:
*     CON_FACTOR

*  Purpose:
*     Define conversion factors used throughout the CHART code

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_FACTOR

*  Description:
*     Define conversion factors used throughout the CHART code
*     constants are defined by executable statements, but could equally
*     well be defined by DATA statements.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council
*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1991 (PMA):
*        Original version.
*     2-MAR-1993 (AJJB):
*        STATUS argument added.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'CONVF'            ! Conversion factors

*        TWOPI = DOUBLE PRECISION (Write)
*           Two times pi
*        HALFPI = DOUBLE PRECISION (Write)
*           Half of pi
*        RDSA = DOUBLE PRECISION (Write)
*           Radians per second of arc
*        RDST = DOUBLE PRECISION (Write)
*           Radians per second of time
*        RDDG = DOUBLE PRECISION (Write)
*           Radians per degree

*  Status:
      INTEGER STATUS             ! Global status

*.
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      HALFPI = 0.1570796326794897E+1
      TWOPI  = 0.6283185307179586E+1
      RDSA   = 0.4848136811095360E-5
      RDST   = 0.7272205216643039E-4
      RDDG   = 0.1745329251994329E-1

      END
