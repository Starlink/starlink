*+ CMP_ACTIV - initialise CMP library for SCL application
      subroutine cmp_activ(status)
*    Description :
*     Intialise the Parameter part of the CMP-system.
*    Method :
*     Initialise common blocks etc.
*    Authors :
*     Sid Wright  (UCL::SLW)
*    History :
*     17-Apr-1983:  Starlink Version. (UCL::SLW)
*     04.03.1985:   ADAM version (REVAD::BDK)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CMP_CONST'
*    Status return :
      integer status			! Status
*    Global variables :
      INCLUDE 'CMP_CCT'
*-

*    Execution allowed ?
      if (status .ne. SAI__OK) then
         return
      endif

*    Initialise component table
      Cmpcnt = 0

*    Declare CMP awake
      Cmpslp = .false.

      end
