      SUBROUTINE IRH1_IPUT( INDEX, SIZE,
     :                      NAMES, LEVELS, FILES, MODGPS, MODINS,
     :                      NAME, LEVEL, FILE, MODGP, MODIN, STATUS )
*+
*  Name:
*     IRH1_IPUT

*  Purpose:
*     Store group information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_IPUT( INDEX, NAME, 
*                     NAMES, LEVELS, FILES, MODGPS, MODINS,
*                     LEVEL, FILE, MODGP, MODIN, SIZE, STATUS )

*  Description:
*     This routine stores the given information relating to a member of
*     a group, in the corresponding GROUP structure arrays. Character
*     strings are converted to upper case before being stored.

*  Arguments:
*     INDEX = INTEGER (Given)
*        The index within the group at which the information is to be
*        stored. 
*     SIZE = INTEGER (Given)
*        The size of the GROUP structure arrays.
*     NAMES(SIZE) = CHARACTER (Given and Returned)
*        The NAMES array.
*     LEVELS(SIZE) = INTEGER (Given and Returned)
*        The LEVEL array.
*     FILES(SIZE) = CHARACTER (Given and Returned)
*        The FILE  array.
*     MODGPS(SIZE) = INTEGER (Given and Returned)
*        The MOD_GROUP  array.
*     MODINS(SIZE) = INTEGER (Given and Returned)
*        The MOD_INDEX  array.
*     NAME = CHARACTER (Given)
*        The text to be stored in the NAMES array.
*     LEVEL = INTEGER (Given)
*        The indirection depth at which the name was specified. Zero
*        should be given if the name was given directly, instead of by
*        an indirection element.
*     FILE = CHARACTER (Given)
*        The name of the indirection file in which the name was
*        specified. A blank should be given if the name was given
*        directly, instead of by an indirection element.
*     MODGP = INTEGER (Given)
*        The IRH identifier of the group used as a basis for the name
*        if it was created as a result of a modification element. A
*        value of zero should be given if the name was not created as a
*        result of a modification element.
*     MODIN = INTEGER (Given)
*        The index within the group specified by MODGP, of the name
*        used as a basis for the name given by argument NAME.  If MODGP
*        is given as zero, then MODIN is ignored.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1991 (DSB):
*        Original version.
*     7-NOV-1991 (DSB):
*        Character conversion to upper case included.
*     26-FEB-1992 (PDRAPER):
*        Removed I90_PAR reference.
*     27-FEB-1992 (PDRAPER):
*        Changed argument order to put %VALed character arrays first.
*     28-FEB-1992 (PDRAPER):
*        Added case sensitivity flags.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRH_ERR'          ! IRH error values.
      INCLUDE 'IRH_PAR'          ! IRH constants

*  Arguments Given:
      INTEGER INDEX
      CHARACTER NAME*(*)
      INTEGER LEVEL
      CHARACTER FILE*(*)
      INTEGER MODGP
      INTEGER MODIN
      INTEGER SIZE

*  Arguments Given and Returned:
      CHARACTER NAMES(SIZE)*(*)
      INTEGER LEVELS(SIZE)
      CHARACTER FILES(SIZE)*(*)
      INTEGER MODGPS(SIZE)
      INTEGER MODINS(SIZE)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the index is out of bounds, report an error.
      IF( INDEX .LE. 0 .OR. INDEX .GT. SIZE ) THEN
         STATUS = IRH__OUTBN
         CALL ERR_REP( 'IRH1_IPUT_ERR1',
     :                 'IRH1_IPUT: Array index out of bounds',
     :                 STATUS )

*  Otherwise, store the information.
      ELSE
         NAMES( INDEX ) = NAME
         LEVELS( INDEX ) = LEVEL
         FILES( INDEX ) = FILE
         MODGPS( INDEX ) = MODGP
         MODINS( INDEX ) = MODIN

*  Convert strings to upper case if enabled.
         IF ( IRH__UCASE ) THEN
            CALL CHR_UCASE( NAMES( INDEX ) )
            CALL CHR_UCASE( FILES( INDEX ) )
         END IF

      END IF

      END
* $Id$
