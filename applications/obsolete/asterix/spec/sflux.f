*+  SFLUX - Integrates energy and photon flux under a spectrum
      SUBROUTINE SFLUX( STATUS )
*
*    Description :
*
*     Takes a fit_model data object and calculates the flux (ergs/cm**2/sec)
*     and number of photons (photons/sec) between two entered energy limits.
*     The number of energy channels between these limits used for integration
*     is 1000 by default, but can be made up to 5000.
*
*    Parameters :
*
*     MODEL=UNIV(R)
*            data object containing model spec
*     Z=REAL(R)
*            redshift to be applied to source spectrum
*     NCH=INTEGER(R)
*            number of channels for integral evaluation
*     LEN=REAL(R)
*            start energy for integral
*     UEN=REAL(R)
*            upper energy for integral
*     SPLIT=LOGICAL(R)
*            find fluxes in each model component
*
*    Method :
*
*     Integration channels are spaced logarithmically in energy.
*     If a non-zero redshift is specified, it is incorporated by scaling
*     all the model space bounds by 1+z, to shift them into the source frame.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Martin Watt (BHVAD:: MPW)
*     Trevor Ponman (BHVAD::TJP)
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     23 Oct 85 : Original (MPW)
*      3 Jul 87 : V0.6-1 New fitting system (TJP)
*      5 Nov 87 : V0.6-2 New FIT_MODGET interface (TJP)
*     14 Dec 88 : V0.6-3 Further updates to subroutine calls (TJP)
*     23 Feb 89 : FIT_MCALC interface slightly changed. V no. not changed (TJP)
*     20 Jun 89 : V1.0-1 ASTERIX88 version, redshift included (TJP)
*     14 Aug 90 : V1.0-2 Write output to parameters (RJV)
*      5 Aug 91 : V1.5-1 INIT flag added (TJP)
*     12 Jan 93 : V1.7-0 Missing AST_INIT added. Now finds fluxes in each
*                        additive model component. (DJA)
*      8 Sep 93 : V1.7-1 Added SPEC_INIT call and removed reference to
*                        SPEC_CMN_RZ (DJA)
*     24 Jan 94 : V1.7-2 SPLIT mode can now calculates unabsorbed fluxes in
*                        additive model components. (DJA)
*      8 Feb 94 : V1.7-3 More changes to FIT_MCALC interface (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
*
*    Structure declarations:
*
      INCLUDE 'FIT_STRUC'
*
*    Status :
*
      INTEGER STATUS
*
*    Local constants :
*
      INTEGER MAXEN
	PARAMETER (MAXEN=5000)		! max number of energy channels
      INTEGER MAXTERM			! Max no. additive terms
        PARAMETER (MAXTERM=MAXCOMP)
      INTEGER DEFAULT
	PARAMETER (DEFAULT=1000)	! default number of energy channels
*
*    Local variables :
*
      RECORD /MODEL_SPEC/ MODEL      	! model specification
      RECORD /MODEL_SPEC/ TMODEL      	! Term model specification

      CHARACTER*(DAT__SZLOC) MLOC    	! locator to fit_model data object
      CHARACTER*79           TXT        ! Output text

      REAL ELBOUND(MAXEN)            	! lower bounds of energy channels
      REAL EUBOUND(MAXEN)            	! upper bounds of energy channels
      REAL FLUX(MAXEN)               	! photons per second in each channel
      REAL LB(NPAMAX)                	! their lower bounds
      REAL LE(NPAMAX)		       	! lower error estimate
      REAL LENERGY                   	! lower energy boundary for integration
      REAL LOWER		       	! log10 of lower boundary
      REAL PARAM(NPAMAX)             	! all the component model parameters
      REAL PHOTONS		       	! total no of photons in all channels
      REAL STEP		       		! increment in log10 of energy loop
      REAL TOTFLX		       	! total integrated flux
      REAL TPHOTONS		       	! total no of photons in all channels
      REAL TTOTFLX		       	! total integrated flux
      REAL UB(NPAMAX)                	! their upper bounds
      REAL UE(NPAMAX)		       	! upper error estimate
      REAL UENERGY		       	! upper energy boundary for integration
      REAL UPPER		       	! log10 of upper boundary
      REAL Z                         	! redshift [ Eobs/Esource=1/(1+z) ]

      INTEGER J                      	! loop counter
      INTEGER NEN                    	! no of input (energy) channels
      INTEGER NPAR                   	! total no of model parameters
      INTEGER NTERM			! No. additive terms
      INTEGER SIGNS(MAXTERM)		! Additive terms
      INTEGER STACKPTR               	! pointer to model stack
      INTEGER TERMS(MAXCOMP,MAXTERM)	! Additive terms

      LOGICAL FROZEN(NPAMAX)         	! FROZEN facility
      LOGICAL INPRIM                 	! primitive input data?
      LOGICAL SPLIT                     ! Split model into components?
*
*    Version :
*
      CHARACTER*30 VERSION
	PARAMETER		( VERSION='SFLUX Version 1.8-0' )
*-

*    Version
      CALL MSG_PRNT( VERSION )

*    Initialise
      CALL AST_INIT()
      CALL SPEC_INIT( STATUS )

*    Model genus
      MODEL.GENUS='SPEC'

*    Read in the model to be fitted
      CALL USI_ASSOCI('MODEL','READ',MLOC,INPRIM,STATUS)
      CALL FIT_MODGET(MLOC,MODEL,NPAR,PARAM,LB,UB,LE,UE,FROZEN,STATUS)
      IF ( STATUS.NE.SAI__OK ) GOTO 9000

*    Look for redshift
      CALL SFIT_GETZ( Z, STATUS )

*    Enter no of energy channels
      CALL USI_GET0I('NCH',NEN,STATUS)
      IF(STATUS.NE.SAI__OK) GOTO 9000
      IF(NEN.GT.MAXEN) THEN
	CALL MSG_SETI('MAXEN',MAXEN)
	CALL MSG_PRNT( 'Using maximum of ^MAXEN channels' )
	NEN=MAXEN
      END IF

*    Enter energy range to be integrated
      CALL MSG_PRNT( 'Enter energy limits in keV' )
      CALL USI_GET0R('LEN',LENERGY,STATUS)
      CALL USI_GET0R('UEN',UENERGY,STATUS)
      IF(STATUS.NE.SAI__OK) GOTO 9000

*    Apply redshift to required energy range
      IF ( Z .GT. 0.0 ) THEN
        CALL SFIT_APPRED( Z, 1, LENERGY, UENERGY, STATUS )
      END IF

*    Calculate energy boundaries for integration
      LOWER=ALOG10(LENERGY)
      UPPER=ALOG10(UENERGY)
      STEP =(UPPER-LOWER)/REAL(NEN)
      DO J=1,NEN
        ELBOUND(J)=10.0**(LOWER+REAL(J-1)*STEP)
      END DO
      DO J=1,NEN-1
        EUBOUND(J)=ELBOUND(J+1)
      ENDDO
      EUBOUND(NEN)=10.0**(LOWER+REAL(NEN)*STEP)

*    Calculate number of photons in each energy channel
      CALL DYN_MAPR( 1, NEN*MAXSTACK, STACKPTR, STATUS )
      CALL FIT_MCALC( MODEL, PARAM, 1, 1, NEN, NEN, NEN+1, ELBOUND,
     :                EUBOUND, %VAL(STACKPTR), FLUX, STATUS )

*    Split model up
      IF ( MODEL.NCOMP .GT. 1 ) THEN
        CALL USI_GET0L( 'SPLIT', SPLIT, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 9000
      ELSE
        SPLIT = .FALSE.
      END IF

*    Calculate energy and photon fluxes between the two energy values
      CALL SFLUX_FLX( NEN, FLUX, ELBOUND, EUBOUND, PHOTONS, TOTFLX,
     :                                                     STATUS )

*    Split up the model?
      IF ( SPLIT ) THEN

*      Identify the additive terms in the model and their associated
*      multiplicative components.
        CALL FIT_MSPECFLAT( MODEL, MAXTERM, NTERM, TERMS, SIGNS,
     :                      STATUS )

*      Heading
        CALL MSG_PRNT( '  Model Term              Photons/cm**2/sec  '/
     :                 /'Erg/cm**2/sec' )

*      Loop over additive terms
        DO J = 1, NTERM

*        Evaluate this term
          CALL FIT_MCALCTERM( MODEL, NTERM, TERMS, SIGNS, J,
     :                        PARAM, 1, 1, NEN, NEN, NEN+1,
     :                        ELBOUND, EUBOUND, %VAL(STACKPTR),
     :                        TMODEL, FLUX, STATUS )

*        Get energy and photon flux for this spectrum
          CALL SFLUX_FLX( NEN, FLUX, ELBOUND, EUBOUND, TPHOTONS,
     :                    TTOTFLX, STATUS )

*        Output text
          WRITE( TXT, '(2X,A23,2(2X,1PG14.7))' )
     :                           TMODEL.SPEC, TPHOTONS, TTOTFLX
          CALL MSG_PRNT( TXT )

        END DO

*      Write unattenuated flux if more than one multiplicative component
        CALL MSG_BLNK()

      END IF

*    Unmap model stack
      CALL DYN_UNMAP( STACKPTR, STATUS )

*    Print out results
      CALL MSG_SETR('TOTFLX',TOTFLX)
      CALL MSG_PRNT( 'Energy flux = ^TOTFLX ergs/cm**2/sec' )
      CALL MSG_SETR('PHOTONS',PHOTONS)
      CALL MSG_PRNT( 'Photon flux = ^PHOTONS photons/cm**2/sec' )

*    Write output to environment
      CALL USI_PUT0R( 'EFLUX', TOTFLX, STATUS )
      CALL USI_PUT0R( 'PFLUX', PHOTONS, STATUS )

*    Tidy up and exit
      CALL USI_ANNUL(MLOC,STATUS)
 9000 CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+   SFLUX_FLX - Find fluxes for model components
      SUBROUTINE SFLUX_FLX( NEN, STK, ELBOUND, EUBOUND, TPHOT,
     :                                          TFLX, STATUS )
*
*    Description :
*
*     Finds total photon and energy flux for one or more predicted data
*     datasets.
*
*    History :
*
*     12 Jan 93 : Original (DJA)
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
      INTEGER NEN                       ! Number of energy channels
      REAL STK(NEN)                  	! Predicted model data
      REAL ELBOUND(NEN)            	! lower bounds of energy channels (keV)
      REAL EUBOUND(NEN)            	! upper bounds of energy channels (keV)
*
*    Export :
*
      REAL TPHOT	                ! Predicted photon flux phot/cm**2/sec
      REAL TFLX				! Predicted photon flux erg/cm**2/sec
*
*    Local constants :
*
      REAL KEV_TO_ERG                   ! keV to erg conversion factor
        PARAMETER (KEV_TO_ERG = 1.60219E-9)
*
*    Local variables :
*
      INTEGER J
*-

      TPHOT = 0.0
      TFLX = 0.0
      DO J = 1, NEN
	TPHOT = TPHOT + STK(J)
	TFLX = TFLX+STK(J)*(ELBOUND(J)+EUBOUND(J))/2.0*KEV_TO_ERG
      END DO

      END
