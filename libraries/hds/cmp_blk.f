*+ CMP_BLK - CMP Block Data Initialisation
      block data cmp_blk
*    Description :
*     Initialise the CMPGO Common Block so that implicit activation
*     of CMP can be done.
*    Authors :
*     Jack Giddings (UCL::JRG)
*    History :
*     3-JAN-1983:  Original.  (UCL::JRG)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CMP_CONST'
*    Global variables :
      INCLUDE 'CMP_CCT'
*    Global data :
      data Cmpslp /.true./
*-

      end
