      LOGICAL FUNCTION DSA_SAME_DATA( DSARE1, DSARE2, STATUS )
*+
*  Name:
*     DSA_SAME_DATA

*  Purpose:
*     Indicates whether two DSA references refer to the same base NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = DSA_SAME_DATA( DSARE1, DSARE2, STATUS )

*  Description:
*     Sometimes, a program needs to know if the main data arrays
*     associated with two reference names are in fact the same data if
*     it is to organise its workspace properly. This routine can be used
*     to find this out.

*  Arguments:
*     DSARE1 = CHARACTER * ( * ) (Given)
*        The reference name associated with the first NDF.
*     DSARE2 = CHARACTER * ( * ) (Given)
*        The reference name associated with the second NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     DSA_SAME_DATA = LOGICAL
*        True if the two data structures are the same.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     03 Aug 1987 (ks):
*        Original version.
*     27 Feb 1990 (ks):
*        Now uses DSA__ routines for structure details instead of
*        assuming the original Figaro data format.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     23 Aug 1992 (ks):
*        Ensure function value is defined before allowing a RETURN (the
*        DecStation compiler spotted this one).
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
      CHARACTER * ( * ) DSARE1
      CHARACTER * ( * ) DSARE2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL IGNORE             ! Ignored
      INTEGER SLO1               ! The first reference slot
      INTEGER SLO2               ! The second reference slot

*.

*  Safe value.
      DSA_SAME_DATA = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up both reference slots.
      CALL DSA1_RFND( DSARE1, SLO1, STATUS )
      CALL DSA1_RFND( DSARE2, SLO2, STATUS )

*  Ask NDF library.
      CALL NDF_SAME( DSA__REFID1(SLO1), DSA__REFID1(SLO2),
     :   DSA_SAME_DATA, IGNORE, STATUS )

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
