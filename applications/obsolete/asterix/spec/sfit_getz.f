*+  SFIT_GETZ - Get redshift from user
      SUBROUTINE SFIT_GETZ( Z, STATUS )
*
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (ROSAT,BHVAD::DJA)
*
*    History :
*
*     22 Sep 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Export :
*
      REAL                   Z 			! User selected redshift
*
*    Status :
*
      INTEGER STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get redshift from user
      CALL USI_GET0R( 'Z', Z, STATUS )

      IF ( (Z .GT. 0.0) .AND. (STATUS.EQ.SAI__OK) ) THEN
        CALL MSG_BLNK()
        CALL MSG_SETR('Z',Z)
        CALL MSG_PRNT('** Redshift ^Z applied **')
        CALL MSG_BLNK()
      END IF

      END
