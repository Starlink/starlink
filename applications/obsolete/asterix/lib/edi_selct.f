      SUBROUTINE EDI_SELCT( PAR, NLIST, MINN, MAXN, ISEL,
     :                      NSEL, STATUS )
*+
*  Name:
*     EDI_SELCT

*  Purpose:
*     Select lists from those available in dataset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_SELCT( PAR, NLIST, MINN, MAXN, ISEL, NSEL, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PAR = CHARACTER*(*) (given)
*        Parameter to use to get list
*     NLIST = INTEGER (given)
*        Number of lists in dataset
*     MINN = INTEGER (given)
*        Minimum number allowed in selection
*     MAXN = INTEGER (given)
*        Maximum number allowed in selection
*     ISEL[] = INTEGER (returned)
*        Selected lists, must be declared as big as MAXN
*     NSEL = INTEGER (returned)
*        Number of lists selected
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     EDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/edi.html

*  Keywords:
*     package:edi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     18 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER*(*)		PAR
      INTEGER			NLIST, MINN, MAXN

*  Arguments Returned:
      INTEGER			ISEL(*), NSEL

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      INTEGER			IDXPTR			! Selection array
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check enough lists!
      IF ( NLIST .LT. MINN ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETI( 'N', MINN )
        CALL ERR_REP( ' ', 'Too few lists in dataset, need ^N', STATUS )

      ELSE

*    Create workspace
        CALL DYN_MAPI( 1, NLIST, IDXPTR, STATUS )

*    Get lists from user
        CALL PRS_GETLIST( PAR, NLIST, %VAL(IDXPTR), NSEL, STATUS )

*    User managed to do typing ok?
        IF ( STATUS .EQ. SAI__OK ) THEN

*      Check whether number of lists supplied was ok
          IF ( (MINN.EQ.MAXN) .AND. (NSEL.NE.MINN) ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'N', NSEL )
            CALL MSG_SETI( 'NR', MINN )
            CALL ERR_REP( 'EDI_SELCT_3', '^N lists were selected, '/
     :                                 /'^NR are required', STATUS )


          ELSE IF ( NSEL .LT. MINN ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'N', NSEL )
            CALL MSG_SETI( 'NR', MINN )
            CALL ERR_REP( 'EDI_SELCT_2', 'Only ^N lists selected, '/
     :                        /'at least ^NR are required', STATUS )

          ELSE IF ( NSEL .GT. MAXN ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'N', NSEL )
            CALL MSG_SETI( 'NR', MAXN )
            CALL ERR_REP( 'EDI_SELCT_3', '^N lists were selected, '/
     :                        /'at most ^NR are required', STATUS )

          END IF

*      If still ok, copy the lists to the users output array
          IF ( STATUS .EQ. SAI__OK ) THEN
            CALL ARR_COP1I( NSEL, %VAL(IDXPTR), ISEL, STATUS )
          END IF

        END IF

*    Release workspace
        CALL ERR_BEGIN( STATUS )
        CALL DYN_UNMAP( IDXPTR, STATUS )
        CALL ERR_END( STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_SELCT', STATUS )

      END
