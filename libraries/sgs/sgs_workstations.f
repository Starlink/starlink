      PROGRAM WKSTN
*+
*  Name:
*     WKSTN

*  Invocation:
*     sgs_workstations

*  Language:
*     Starlink Fortran 77

*  Description:
*     Lists the names and descriptions of all the available SGS
*     workstations on FORTRAN unit 6 (usually the terminal).

*  Authors:
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     22-NOV-1988 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      IMPLICIT NONE

      CALL SGS_WLIST(6)
      
      END
