      SUBROUTINE IRH1_GSUPP( INDEX, SIZE, LEVELS, FILES, MODGPS, MODINS,
     :                       LEVEL, FILE, MODGP, MODIN, STATUS )
*+
*  Name:
*     IRH1_GSUPP

*  Purpose:
*     Retrieve group supplementary information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_GSUPP( INDEX, SIZE, LEVELS, FILES, MODGPS, MODINS,
*                      LEVEL, FILE, MODGP, MODIN, STATUS )

*  Description:
*     This routine retrieves the supplementary information relating to
*     a member of a group, from the corresponding GROUP structure
*     arrays.

*  Arguments:
*     INDEX = INTEGER (Given)
*        The index within the group from which the information is to be
*        retrieved. 
*     SIZE = INTEGER (Given)
*        The size of the GROUP structure arrays.
*     LEVELS(SIZE) = INTEGER (Given)
*        The LEVEL array.
*     FILES(SIZE) = CHARACTER (Given)
*        The FILE  array.
*     MODGPS(SIZE) = INTEGER (Given)
*        The MOD_GROUP  array.
*     MODINS(SIZE) = INTEGER (Given)
*        The MOD_INDEX  array.
*     LEVEL = INTEGER (Returned)
*        The indirection depth at which the name was specified. Zero
*        is returned if the name was given directly, instead of by
*        an indirection element.
*     FILE = CHARACTER (Returned)
*        The name of the indirection file in which the name was
*        specified. A blank is returned if the name was given directly,
*        instead of by an indirection element.
*     MODGP = INTEGER (Returned)
*        The IRH identifier of the group used as a basis for the name
*        if it was created as a result of a modification element. A
*        value of zero is returned if the name was not created as a
*        result of a modification element.
*     MODIN = INTEGER (Returned)
*        The index within the group specified by MODGP, of the name
*        used as a basis for the name returned by argument NAME.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1991 (DSB):
*        Original version.
*     26-FEB-1992 (PDRAPER):
*        Removed I90_PAR reference.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER INDEX
      INTEGER SIZE
      INTEGER LEVELS(SIZE)
      CHARACTER FILES(SIZE)*(*)
      INTEGER MODGPS(SIZE)
      INTEGER MODINS(SIZE)

*  Arguments Returned:
      INTEGER LEVEL
      CHARACTER FILE*(*)
      INTEGER MODGP
      INTEGER MODIN

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the index is out of bounds, return null information.
      IF( INDEX .LE. 0 .OR. INDEX .GT. SIZE ) THEN
         LEVEL = 0
         FILE = ' '
         MODGP = 0
         MODIN = 0

*  Otherwise, retrieve the information from the GROUP structure arrays.
      ELSE
         LEVEL = LEVELS( INDEX )
         FILE = FILES( INDEX )
         MODGP = MODGPS( INDEX )
         MODIN = MODINS( INDEX )

      END IF

      END
* $Id$
