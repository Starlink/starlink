*+ CMP_DEACT - deactivate CMP library after SCL application
      subroutine cmp_deact(status)
*    Description :
*     Terminate the parameter part of the CMP-system
*    Method :
*     Release all active locators etc.
*    Authors :
*     Sid Wright  (UCL::SLW)
*    History :
*     17-Apr-1983:  Original.  (UCL::SLW)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CMP_CONST'
*    Status return :
      integer status			! status return
*    Global variables :
      INCLUDE 'CMP_CCT'
*-

*   zero component table
      Cmpcnt = 0

*    Set CMP asleep
*     Cmpslp = .true.

      end
