      SUBROUTINE IRM_SPAGE( ON )
*+
*  Name:
*     IRM_SPAGE

*  Purpose:
*     See if paged screen output is currently enabled.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_SPAGE( ON )

*  Description:
*     Routine IRM_PTOUT produces paged output on the screen. Such
*     output may be suppressed by the user by supplying a non-blank
*     string when asked to "Press RETURN to continue". This routine
*     returns a flag indicating if output is currently enabled or
*     suppressed.

*  Arguments:
*     ON = LOGICAL (Returned)
*        True if text passed to IRM_PTOUT will actually appear on the
*        screen. False if the user has suppressed such output.

*  Authors:
*     (DSB) David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-DEC-1992 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no default typing allowed

*  Global Variables:
      INCLUDE 'IRM_COM'        ! IRM common blocks.
*        MCM_TXTON = LOGICAL (Read)
*           If true, text output is enabled.

*  Arguments Returned:
      LOGICAL ON

*.

*  Return the value of MCM_TXTON.
      ON = MCM_TXTON

      END
