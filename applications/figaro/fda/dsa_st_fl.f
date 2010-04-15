      SUBROUTINE DSA_SET_FLAGGED_VALUES( DSAREF, EXIST, STATUS )
*+
*  Name:
*     DSA_SET_FLAGGED_VALUES

*  Purpose:
*     Indicate whether or not a data array may contain bad values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_SET_FLAGGED_VALUES( DSAREF, EXIST, STATUS )

*  Description:
*     If an application needs to explicitly indicate either that a newly
*     created data array contains flagged data values, or needs to
*     indicate to the system that a data array that did contain flagged
*     values no longer does so, it can call this routine to set the
*     appropriate flags in the NDF.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     EXIST = LOGICAL (Given)
*        True if the data may contain bad values. Give false only if it
*        is certain that there are no bad values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     14 Jul 1988 (ks):
*        Original version.
*     13 Dec 1989 (ks):
*        Now uses DSA__SET_FLAGGED.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     25 Nov 1995 (hme):
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

*  Look up reference slot and set the flag.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      CALL NDF_SBAD( EXIST, DSA__REFID1(SLOT), 'DATA', STATUS )

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
