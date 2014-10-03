      subroutine cmp_deact(status)
*+
*  Name:
*     CMP_DEACT

*  Purpose:
*     deactivate CMP library after SCL application.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_DEACT( [p]... )

*  Description:
*     Terminate the parameter part of the CMP-system

*  Algorithm:
*     Release all active locators etc.

*  Authors:
*     Sid Wright  (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     17-Apr-1983:  Original.  (UCL::SLW)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CMP_CONST'
*    Status return :
      integer status			! status return

*  Global Variables:
      INCLUDE 'CMP_CCT'

*.


*   zero component table
      Cmpcnt = 0

*    Set CMP asleep
*     Cmpslp = .true.

      end
