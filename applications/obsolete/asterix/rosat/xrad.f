*+  XRAD - calculates the PSF radius for a PSPC source.
        SUBROUTINE XRAD(STATUS)
*    Description :
*     <description of what the application does - for user info>
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           <description of parameter>
*    Method :
*     <description of how the application works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Richard Saxton (LTVAD::RDS)
*    History :
*     6-OCT-1992  -  original
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*     <any INCLUDE files containing global constant definitions>
*    Global variables :
*     <global variables held in named COMMON>
*    Structure definitions :
*     <specification of FORTRAN structures>
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
*    Local variables :
        REAL PSF                      ! Radius including 90% of counts (degs)
        LOGICAL LDISP                 ! Display the result ?
        REAL THETA                    ! Off-axis angle (arcminutes)
        REAL ENERGY                   ! Photon energy
        REAL PFRAC                    ! Fraction to use
*
* Code :
* Get off-axis angle
        CALL PAR_GET0R('OFFAX', THETA, STATUS)
*
        CALL PAR_GET0R('ENERGY', ENERGY, STATUS)
*
        CALL PAR_GET0R('PFRAC', PFRAC, STATUS)
*
        IF (STATUS .NE. SAI__OK) GOTO 999
*
* Can only calculate upto 95% radii
        IF (PFRAC .GT. 0.95) THEN
           CALL MSG_PRNT('** PFRAC too large: using the 0.95 radius **')
        ENDIF
*
* Calculate the radius
        CALL XRT_GETPSF(PFRAC, ENERGY, THETA, PSF, STATUS)
*
        IF (STATUS .NE. SAI__OK) GOTO 999
*
* Convert the radius back into degrees
        PSF = PSF / 60.0
*
* Display the result ?
        CALL PAR_GET0L('DISP', LDISP, STATUS)
*
        IF (LDISP) THEN
           CALL MSG_SETR('RAD', PSF)
           CALL MSG_SETR('RADAM', PSF*60.)
           CALL MSG_PRNT(' PSF radius = ^RAD degrees (^RADAM arcmins)')
        ENDIF
*
* Write result to output parameter
        CALL PAR_PUT0R('PSFRAD', PSF, STATUS)
*
999     CONTINUE
*
        IF (STATUS .NE. SAI__OK) THEN
           CALL ERR_REP(' ','from XRAD',STATUS)
        ENDIF
*
	END
