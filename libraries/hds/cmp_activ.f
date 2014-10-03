      subroutine cmp_activ(status)
*+
*  Name:
*     CMP_ACTIV

*  Purpose:
*     initialise CMP library for SCL application.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_ACTIV( [p]... )

*  Description:
*     Intialise the Parameter part of the CMP-system.

*  Algorithm:
*     Initialise common blocks etc.

*  Authors:
*     Sid Wright  (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     17-Apr-1983:  Starlink Version. (UCL::SLW)
*     04.03.1985:   ADAM version (REVAD::BDK)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CMP_CONST'
*    Status return :
      integer status			! Status

*  Global Variables:
      INCLUDE 'CMP_CCT'

*.


*    Execution allowed ?
      if (status .ne. SAI__OK) then
         return
      endif

*    Initialise component table
      Cmpcnt = 0

*    Declare CMP awake
      Cmpslp = .false.

      end
