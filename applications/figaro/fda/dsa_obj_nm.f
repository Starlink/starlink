      SUBROUTINE DSA_OBJECT_NAME( DSAREF, OBJECT, STATUS )
*+
*  Name:
*     DSA_OBJECT_NAME

*  Purpose:
*     Return the title of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_OBJECT_NAME( DSAREF, OBJECT, STATUS )

*  Description:
*     Some Figaro data structures will have an object name associated
*     with them. This routine returns that name, if it exists, and
*     returns a blank string if it does not. The NDF component in
*     question is in fact the NDF title.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     OBJECT = CHARACTER * ( * ) (Returned)
*        The NDF title, a blank string if there is no title.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     29 Jul 1987 (ks):
*        Original version.
*     16 Jan 1990 (ks):
*        Now uses DSA__ routines to handle the structure details.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     02 Feb 1996 (hme):
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
      CHARACTER * ( * ) OBJECT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL THERE              ! Whether an HDS component exists
      INTEGER SLOT               ! The reference slot

*.

*  Default value.
      OBJECT = ' '

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up reference slot.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )

*  Get the title string.
      CALL NDF_STATE( DSA__REFID1(SLOT), 'TITLE', THERE, STATUS )
      IF ( THERE )CALL NDF_CGET( DSA__REFID1(SLOT),
     :      'TITLE', OBJECT, STATUS )

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
