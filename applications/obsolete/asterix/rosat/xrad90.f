*+  XRAD90 - calculates the 90% PSF radius for a PSPC source.
        SUBROUTINE XRAD90(STATUS)
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
*     author (institution::username)
*    History :
*      21 Feb 94 :  remove maximum theta restriction
*                   if > max then calculate as for max (RJV)
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
*
* Code :
* Get off-axis angle
        CALL PAR_GET0R('OFFAX', THETA, STATUS)
*
        IF (STATUS .NE. SAI__OK) GOTO 999
*
* Calculate the 90% radius
        CALL XRAD90_INT(THETA, PSF, STATUS)
*
        IF (STATUS .NE. SAI__OK) GOTO 999
*
* Display the result ?
        CALL PAR_GET0L('DISP', LDISP, STATUS)
*
        IF (LDISP) THEN
           CALL MSG_SETR('RAD', PSF)
           CALL MSG_SETR('RADAM', PSF*60.)
           CALL MSG_PRNT(' 90% radius = ^RAD degrees (^RADAM arcmins)')
        ENDIF
*
* Write result to output parameter
        CALL PAR_PUT0R('PSFRAD', PSF, STATUS)
*
999     CONTINUE
*
	END
*
*+XRAD90_INT - calculates the 90% PSF radius for a PSPC source.
        SUBROUTINE XRAD90_INT(THETA, PSF, STATUS)
*    Description :
*     Calculates the approximate radius containing 90% of source
*     counts as a function of off-axis angle. It uses the 1.0 keV
*     curve in the AO-2 document.
*    Environment parameters :
*    Method :
*     <description of how the application works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     author (institution::username)
*    History :
*     date:  changes (institution::username)
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
*    Import :
      REAL THETA          ! Off-axis angle (arcmins)
*    Export :
      REAL PSF            ! Radius including 90% of counts (degs)
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
        INTEGER NBIN
	PARAMETER(NBIN=13)
        REAL THETAMAX
	PARAMETER (THETAMAX=65.0)
*    Local variables :
        REAL FAC(NBIN+1)              ! PSF radii (arcsecs)
        INTEGER IBIN
        INTEGER I
        REAL RBIN,DIFF
*    Local data :
	DATA (FAC(I),I=1,NBIN+1)/14.,14.,17.,25.,42.,55.,65.,85.,107.,133.,
     &				158.,158.,158.,158./
*
* if theta > max then calculate as for max
        IF (THETA .GE. THETAMAX) THEN
          IBIN=NBIN
          DIFF=0.0
        ELSE
          RBIN = NBIN * THETA / THETAMAX + 1
          IBIN = INT(RBIN)
          DIFF = RBIN - REAL(IBIN)
        ENDIF
*
* Calculate the 90% radius in degrees
	PSF = 1.825 * (FAC(IBIN) + (FAC(IBIN+1) - FAC(IBIN)) * DIFF)
        PSF = PSF / 3600.0
*
999     CONTINUE
*
	END
