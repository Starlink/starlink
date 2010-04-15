      SUBROUTINE DSA_SEEK_VARIANCE( DSAREF, EXIST, STATUS )
*+
*  Name:
*     DSA_SEEK_VARIANCE

*  Purpose:
*     Determine whether or not a variance array exists.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_SEEK_VARIANCE( DSAREF, EXIST, STATUS )

*  Description:
*     This routine looks to see if an NDF contains a variance component.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     EXIST = LOGICAL (Returned)
*        True/false if the variance component exists/does not exist.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     12 Dec 1989 (ks):
*        Original version.
*     12 Mar 1990 (ks):
*        Modified to work with different data formats.  (Only changes
*        required, in fact, were to the comments).
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     18 Dec 1995 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
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

*  Arguments Given:
      CHARACTER * ( * ) DSAREF

*  Arguments Returned:
      LOGICAL EXIST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! The reference slot

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up reference slot and check state of NDF component.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      CALL NDF_STATE( DSA__REFID1(SLOT), 'VARIANCE', EXIST, STATUS )

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END
