      SUBROUTINE DSA1_MSNEW( MSLOT, STATUS )
*+
*  Name:
*     DSA1_MSNEW

*  Purpose:
*     Return the number of a free map slot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA1_MSNEW( MSLOT, STATUS )

*  Description:
*     Return the number of a free map slot.

*  Arguments:
*     MSLOT = INTEGER (Returned)
*        The unused map slot.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     26 Nov 1995 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Returned:
      INTEGER MSLOT

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check each slot in turn, first unused bails out of search.
*  If the loop finishes, there was not slot left free.
      DO 1 MSLOT = 1, DSA__MAXMAP
         IF ( .NOT. DSA__MAPUSD(MSLOT) ) GO TO 2
 1    CONTINUE
         MSLOT = 1
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FDA_E005', 'DSA1_MSNEW: ' //
     :      'No free map slot left, all are in use.', STATUS )
 2    CONTINUE

*  Return.
      END
