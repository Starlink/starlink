      SUBROUTINE CON_RESCM
*+
*  Name:
*     CON_RESCM

*  Purpose:
*     Resets the INTERIM common blocks.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_RESCM

*  Description:
*     This routine clears the user INTERIM common blocks so that INTERIM
*     will start constructing tables etc. from scratch.

*  [optional_subroutine_items]...
*  Authors:
*     AJC: Alan Chipperfield (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1988 December 8 (AJC):
*        Original version.
*     1992 September 4 (MJC):
*        Given an SSE prologue.  Renamed from STL_RESCOM for CONVERT.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'INTERIM(PCTCOM)'
      INCLUDE 'INTERIM(DFSCOM)'
      INCLUDE 'INTERIM(CLICOM)'

*.

      PCT_NENT = 0
      DFS_NENT = 0
      CLI_COMBUF = 'NOT ACCESSED'

      END
