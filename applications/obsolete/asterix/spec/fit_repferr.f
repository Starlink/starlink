*+  FIT_REPFERR - Describe a fitting error to the user
      SUBROUTINE FIT_REPFERR( FITERR, STATUS )
*
*    Description :
*
*     Describe a fitting error to the user
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Nov 92 : Original (BHVAD::DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER      FITERR                    ! Fitting error reported
*
*    Local variables :
*
      LOGICAL      BIGMESS                   ! Print the big message?
*-

*    Fatal fitting error
      CALL MSG_SETI( 'ERRNO', FITERR )
      CALL MSG_PRNT( 'Error number ^ERRNO in fitting' )
      BIGMESS = .FALSE.

*    Trap each one
      IF ( FITERR .EQ. 1 ) THEN
        CALL MSG_PRNT('- bad parameter bounds')

      ELSE IF ( FITERR .EQ. 2 ) THEN
	CALL MSG_PRNT('- all parameters frozen')

      ELSE IF( FITERR .EQ. 3 ) THEN
	CALL MSG_PRNT('- no unpegged parameters')

      ELSE IF ( FITERR .EQ. 4 ) THEN
        CALL MSG_PRNT('- matrix of 2nd derivs is singular')
        BIGMESS = .TRUE.

      ELSE IF ( (FITERR .EQ. 5) .OR. (FITERR .EQ. 6) ) THEN
	CALL MSG_PRNT('- matrix of 2nd derivs ill-conditioned')
        BIGMESS = .TRUE.

      ELSE
	CALL MSG_PRNT( 'Unknown error number' )

      END IF

*    Write the big message
      IF ( BIGMESS ) THEN
        CALL MSG_PRNT( ' This means that the predicted data have'/
     :                 /' become insensitive to changes in' )
        CALL MSG_PRNT( ' one or more parameters. The problem is '/
     :                 /'most likely to be solved by starting' )
        CALL MSG_PRNT( ' from a point in parameter space closer '/
     :                 /'to an acceptable fit.' )
        CALL MSG_PRNT( ' With multiple component models it is of'/
     :                 /'ten helpful to fit a reduced number of' )
        CALL MSG_PRNT( ' components first, to establish sensible'/
     :                 /' starting values for their parameters.' )
      END IF

      END
