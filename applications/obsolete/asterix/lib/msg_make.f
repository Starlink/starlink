*+  MSG_MAKE - Put Adam message into a text string
      SUBROUTINE MSG_MAKE( TEXT, MSGTXT, MSGLEN )
*    Description :
*     Wrap-up of Adam service call MSG_LOAD to permit user to get output
*     as if it were from MSG_OUT, but into a string instead.
*    History :
*     05 Dec 88 : David J. Allan (BHVAD::DJA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*)     TEXT
*    Export :
      CHARACTER*(*)     MSGTXT
      INTEGER           MSGLEN
*    Local variables :
      INTEGER           ISTAT
*-

      ISTAT = SAI__OK

      CALL MSG_LOAD( ' ', TEXT, MSGTXT, MSGLEN, ISTAT )

      END
