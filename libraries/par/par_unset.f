      SUBROUTINE PAR_UNSET( PARAM, WHICH, STATUS )
*+
*  Name:
*     PAR_UNSET

*  Purpose:
*     Cancels various parameter control values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PAR_UNSET( PARAM, WHICH, STATUS )

*  Description:
*     This routine cancels one or more control values of a parameter.
*     These are currently the parameter's dynamic default value (set by
*     PAR_DEFnx or DAT_DEF), its minimum value (set by PAR_MINx), or its
*     maximum value (set by PAR_MAXx).

*     The routine will operate regardless of the given STATUS value and
*     will not report or set STATUS if the specified values have not
*     been set or are already cancelled.

*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        The name of the parameter.
*     WHICH = CHARACTER*(*) (Given)
*        A comma-separated list of the control values to be cancelled,
*        selected from the following options:
*           'DEFAULT' to cancel the dynamic default,
*           'MAXIMUM' to cancel the maximum value, and
*           'MINIMUM' to cancel the minimum value.
*        Unambiguous abbreviations are permitted.
*     STATUS = INTEGER (Unused)
*        The global status.  The routine is executed regardless of the
*        import value of STATUS.  If the import value is not SAI__OK,
*        then it is left unchanged, even if the routine fails to
*        complete.  If the import value is SAI__OK on entry and the
*        routine fails to complete, STATUS will be set to an
*        appropriate value.
*
*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-MAR-1993 (AJC):
*        Original version.
*     1993 May 7 (MJC):
*        Minor rearrangement of the documentation for consistency.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) WHICH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variable:
      INTEGER ISTAT              ! Local STATUS
      INTEGER NAMCOD             ! The parameter index
*.

*  Copy the inherited status rather than the usual status check, as this
*  is a tidying routine that should work even if there has been an
*  earlier error.
      ISTAT = STATUS
      STATUS = SAI__OK

*  Find the parameter-system pointer to the internal parameter space
*  associated with the parameter.
      CALL SUBPAR_FINDPAR( PARAM, NAMCOD, STATUS )

*  Unset the chosen value or values for the parameter.
      CALL SUBPAR_UNSET( NAMCOD, WHICH, STATUS )

*  Restore the original error status.
      IF ( ISTAT .NE. SAI__OK ) STATUS = ISTAT

      END
