*+  PSS_IN_SLICE - Is an image position in the user supplied slice
      LOGICAL FUNCTION PSS_IN_SLICE( POS )
*
*    Description :
*
*     Test whether the specified image position lies within the bounds
*     selected by the user using the SLICE parameter.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      6 Aug 90 : Original (DJA)
*     10 Jul 93 : No longer uses inline functions. Arguments vectorised (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      REAL                     POS(2)        ! Position of interest
*
*    Function definitions :
*
      INTEGER                  PIX
*
*    Local variables :
*
      INTEGER      XED, YED                  ! Distance from left/bottom edges
*-

      XED = PIX(1,POS(1))-BDS_SPOF(1,1)
      YED = PIX(2,POS(2))-BDS_SPOF(1,2)
      PSS_IN_SLICE = ( ( XED .GE. 0 ) .AND. ( YED .GE. 0 ) .AND.
     :                 ( XED .LE. (BDS_SPOF(2,1)-BDS_SPOF(1,1))) .AND.
     :                 ( YED .LE. (BDS_SPOF(2,2)-BDS_SPOF(1,2))) )

      END
