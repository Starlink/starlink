      SUBROUTINE SFLUX( STATUS )
*+
*  Name:
*     SFLUX

*  Purpose:
*     Integrates energy and photon flux under a spectrum

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL SFLUX( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Takes a fit_model data object and calculates the flux (ergs/cm**2/sec)
*     and number of photons (photons/sec) between two entered energy limits.
*     The number of energy channels between these limits used for integration
*     is 1000 by default, but can be made up to 5000.

*  Usage:
*     sflux {parameter_usage}

*  Environment Parameters:
*     MODEL = CHAR (read)
*        Data object containing model spec
*     Z = REAL (read)
*        Redshift to be applied to source spectrum
*     NCH = INTEGER (read)
*        Number of channels for integral evaluation
*     LEN = REAL (read)
*        Start energy for integral
*     UEN = REAL (read)
*        Upper energy for integral
*     SPLIT = LOGICAL (read)
*        Find fluxes in each model component

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     Integration channels are spaced logarithmically in energy.
*     If a non-zero redshift is specified, it is incorporated by scaling
*     all the model space bounds by 1+z, to shift them into the source frame.

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     sflux, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     MPW: Martin Watt (Spacelab 2, University of Birmingham)
*     TJP: Trevor Ponman (University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Oct 1985 V0.6-0 (MPW):
*        Original version.
*      3 Jul 1987 V0.6-1 (TJP):
*        New fitting system
*      5 Nov 1987 V0.6-2 (TJP):
*        New FIT_MODGET interface
*     14 Dec 1988 V0.6-3 (TJP):
*        Further updates to subroutine calls
*     23 Feb 1989 V0.6-4 (TJP):
*        FIT_MCALC interface slightly changed
*     20 Jun 1989 V1.0-1 (TJP):
*        ASTERIX88 version, redshift included
*     14 Aug 1990 V1.0-2 (RJV):
*        Write output to parameters
*      5 Aug 1991 V1.5-1 (TJP):
*        INIT flag added
*     12 Jan 1993 V1.7-0 (DJA):
*        Missing AST_INIT added. Now finds fluxes in each
*        additive model component
*      8 Sep 1993 V1.7-1 (DJA):
*        Added SPEC_INIT call and removed reference to SPEC_CMN_RZ
*     24 Jan 1994 V1.7-2 (DJA):
*        SPLIT mode can now calculates unabsorbed fluxes in
*        additive model components.
*      8 Feb 1994 V1.7-3 (DJA):
*        More changes to FIT_MCALC interface
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*      1 Dec 1995 V2.0-0 (DJA):
*        ADI port
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIT_PAR'

*  Structure Definitions:
      INCLUDE 'FIT_STRUC'

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      INTEGER 			MAXEN			! max number of energy channels
	PARAMETER 		( MAXEN = 5000 )

      INTEGER 			MAXTERM			! Max no. additive terms
        PARAMETER 		( MAXTERM = MAXCOMP )

      INTEGER 			DEFAULT
	PARAMETER 		( DEFAULT = 1000 )	! default number of energy channels

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'SFLUX Version 2.1-0' )

*  Local Variables:
      RECORD /MODEL_SPEC/ MODEL      	! model specification
      RECORD /MODEL_SPEC/ TMODEL      	! Term model specification

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
      INTEGER			MFID			! Model spec dataset id
      INTEGER NEN                    	! no of input (energy) channels
      INTEGER NPAR                   	! total no of model parameters
      INTEGER NTERM			! No. additive terms
      INTEGER SIGNS(MAXTERM)		! Additive terms
      INTEGER STACKPTR               	! pointer to model stack
      INTEGER TERMS(MAXCOMP,MAXTERM)	! Additive terms

      LOGICAL FROZEN(NPAMAX)         	! FROZEN facility
      LOGICAL SPLIT                     ! Split model into components?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()
      CALL SPEC_INIT( STATUS )

*  Model genus
      MODEL.GENUS = 'SPEC'

*  Read in the model to be fitted
      CALL USI_ASSOC( 'MODEL', '*', 'READ', MFID, STATUS )
      CALL FIT_MODGET( MFID, MODEL, NPAR, PARAM, LB, UB, LE, UE,
     :                 FROZEN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Look for redshift
      CALL SFIT_GETZ( Z, STATUS )

*  Enter no of energy channels
      CALL USI_GET0I( 'NCH', NEN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( NEN .GT. MAXEN ) THEN
	CALL MSG_SETI( 'MAXEN', MAXEN )
	CALL MSG_PRNT( 'Using maximum of ^MAXEN channels' )
	NEN = MAXEN
      END IF

*  Enter energy range to be integrated
      CALL MSG_PRNT( 'Enter energy limits in keV' )
      CALL USI_GET0R( 'LEN', LENERGY, STATUS )
      CALL USI_GET0R( 'UEN', UENERGY, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Apply redshift to required energy range
      IF ( Z .GT. 0.0 ) THEN
        CALL SFIT_APPRED( Z, 1, LENERGY, UENERGY, STATUS )
      END IF

*  Calculate energy boundaries for integration
      LOWER = ALOG10(LENERGY)
      UPPER = ALOG10(UENERGY)
      STEP = (UPPER-LOWER)/REAL(NEN)
      DO J = 1, NEN
        ELBOUND(J) = 10.0**(LOWER+REAL(J-1)*STEP)
      END DO
      DO J = 1, NEN-1
        EUBOUND(J) = ELBOUND(J+1)
      END DO
      EUBOUND(NEN) = 10.0**(LOWER+REAL(NEN)*STEP)

*  Calculate number of photons in each energy channel
      CALL DYN_MAPR( 1, NEN*MAXSTACK, STACKPTR, STATUS )
      CALL FIT_MCALC( MODEL, PARAM, 1, 1, NEN, NEN, NEN+1, ELBOUND,
     :                EUBOUND, %VAL(STACKPTR), FLUX, STATUS )

*  Split model up
      IF ( MODEL.NCOMP .GT. 1 ) THEN
        CALL USI_GET0L( 'SPLIT', SPLIT, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
      ELSE
        SPLIT = .FALSE.
      END IF

*  Calculate energy and photon fluxes between the two energy values
      CALL SFLUX_FLX( NEN, FLUX, ELBOUND, EUBOUND, PHOTONS, TOTFLX,
     :                                                     STATUS )

*  Split up the model?
      IF ( SPLIT ) THEN

*    Identify the additive terms in the model and their associated
*    multiplicative components.
        CALL FIT_MSPECFLAT( MODEL, MAXTERM, NTERM, TERMS, SIGNS,
     :                      STATUS )

*    Heading
        CALL MSG_PRNT( '  Model Term              Photons/cm**2/sec  '/
     :                 /'Erg/cm**2/sec' )

*    Loop over additive terms
        DO J = 1, NTERM

*      Evaluate this term
          CALL FIT_MCALCTERM( MODEL, NTERM, TERMS, SIGNS, J,
     :                        PARAM, 1, 1, NEN, NEN, NEN+1,
     :                        ELBOUND, EUBOUND, %VAL(STACKPTR),
     :                        TMODEL, FLUX, STATUS )

*      Get energy and photon flux for this spectrum
          CALL SFLUX_FLX( NEN, FLUX, ELBOUND, EUBOUND, TPHOTONS,
     :                    TTOTFLX, STATUS )

*      Output text
          WRITE( TXT, '(2X,A23,2(2X,1PG14.7))' )
     :                           TMODEL.SPEC, TPHOTONS, TTOTFLX
          CALL MSG_PRNT( TXT )

        END DO

*    Write unattenuated flux if more than one multiplicative component
        CALL MSG_BLNK()

      END IF

*  Unmap model stack
      CALL DYN_UNMAP( STACKPTR, STATUS )

*  Print out results
      CALL MSG_SETR('TOTFLX',TOTFLX)
      CALL MSG_PRNT( 'Energy flux = ^TOTFLX ergs/cm**2/sec' )
      CALL MSG_SETR('PHOTONS',PHOTONS)
      CALL MSG_PRNT( 'Photon flux = ^PHOTONS photons/cm**2/sec' )

*  Write output to environment
      CALL USI_PUT0R( 'EFLUX', TOTFLX, STATUS )
      CALL USI_PUT0R( 'PFLUX', PHOTONS, STATUS )

*  Tidy up and exit
      CALL USI_ANNUL( 'MODEL', STATUS )

 99   CALL AST_CLOSE()
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
