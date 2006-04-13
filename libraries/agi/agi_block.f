************************************************************************
      BLOCK DATA AGI_BLOCK
*+
*  Name:
*     AGI_BLOCK

*  Purpose:
*     Block data for intialising counters.

*  Language:
*     VAX Fortran

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL AGI_BLOCK( [p]... )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     {routine_description}

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'

*  Common Blocks:
      INCLUDE 'agi_cref'
      INCLUDE 'agi_nest'
      INCLUDE 'agi_pfree'

*  Initialise Common:
      DATA CREF / 0 /
      DATA CNEST / 1 /
      DATA CURPID / 0 /
*.

      END


