*+  SSIM - Generates simulated spectral dataset
      SUBROUTINE SSIM( STATUS )
*
*    Description :
*
*     A composite spectral model is folded through an instrument response
*     (for a given observation duration) and a background spectrum and Poisson
*     noise are optionally added, to give a simulated spectral observation.
*     Since results are given in count/chan/sec the observation time need
*     only be specified where Poisson errors are to be included.
*
*    Environment parameters :
*
*     INP=UNIV(R)
*            dataset containing instrument response
*     MODEL=UNIV(R)
*            data object containing model specification
*     BG=UNIV(R)
*            background spectral dataset
*     POISS=LOGICAL(R)
*            Poisson noise to be added?
*     OBLEN=REAL(R)
*            observation length in seconds
*     Z=REAL(R)
*            redshift
*     OUT=UNIV(W)
*            output spectrum
*
*    Method :
*
*     The program will now work from any object containing a suitable INSTR_RESP
*     component. Axis data are generated from the information in the response
*     matrix itself. The OBDAT structure is only used to pass the number of
*     channels and the object locator into subroutines.
*     The MORE box is copied from the input dataset (if present) and is
*     slightly amended (e.g. TARGET name is changed and EXPOSURE_TIME
*     overwritten), but many of the components will no longer be applicable.
*     Note that if any background value to be added is negative then it is
*     taken as zero.
*     If a non-zero redshift is set then this is applied to the source
*     spectrum (by shifting all energy bounds by a factor 1+z).
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman (BHVAD::TJP)
*    History :
*
*     21 May 85 : Original
*      1 Oct 86 : V0.5-1 ADAM version
*     18 Mar 87 : V0.6-1 RESGET changed to pick up ENERGY_BOUNDS  (TJP)
*      6 Jul 87 : V0.6-2 Major change to new fitting system (TJP)
*     21 Oct 88 : V0.6-3 New structures + no need for input SPECTRUM (TJP)
*      5 Jul 89 : V0.6-4 Faster RZ routines for some instruments (MPW)
*     15 Mar 91 : V1.5-1 ASTERIX88 version (TJP)
*      6 Aug 91 : V1.5-2 Incorporate redshifting & use of INIT (TJP)
*     19 Nov 92 : V1.6-1 FIT_PREDDAT interface arg list changed (TJP)
*     24 Feb 93 : V1.6-2 Use new subroutines for Z etc. (DJA)
*      8 Sep 93 : V1.7-0 Added SPEC_INIT call and removed reference to
*                        SPEC_CMN_RZ (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     21 Apr 95 : V1.8-1 Updated data interface (DJA)
*     14 Jun 95 : V1.8-2 Ensure effective exposure is updated in the
*                        Poisson error case (DJA)
*     25 Aug 95 : V1.8-3 Silly big with new vignetting code (DJA)
*      9 Nov 95 : V2.0-0 Data interface converted to ADI (DJA)
*
*    Type definitions :
	IMPLICIT NONE
*    Global constants :
	INCLUDE 'SAE_PAR'
	INCLUDE 'ADI_PAR'
	INCLUDE 'DAT_PAR'
	INCLUDE 'PAR_ERR'
	INCLUDE 'FIT_PAR'
*    Structure definitions :
	INCLUDE 'FIT_STRUC'
*    Status :
	INTEGER STATUS
*    Local variables :
	RECORD/DATASET/ OBDAT		! Observed dataset (dummy structure)
	RECORD/PREDICTION/ PREDDAT	! Data predicted by model
	RECORD/INSTR_RESP/ INSTR	! Instrument response
	RECORD /MODEL_SPEC/ MODEL	! Model specification

	CHARACTER*100 BGOBJ		! B/g dataset name
	CHARACTER*100 FILE		! File name
	CHARACTER*80 TELES		! Telescope name
	CHARACTER*80 INSTRUM		! Instrument name
	CHARACTER*80 LEGEND(3)		! Legend for graphics

	INTEGER DPTR			! Pointer to data array
	INTEGER VPTR			! Pointer to variance array
	INTEGER BPTR			! Pointer to b/g array
	INTEGER CHPTR			! Pointer to i/p channel spec array
	INTEGER I			!
	INTEGER NPAR			! No of parameters
	INTEGER NDIM			! Dimensionality
	INTEGER DIMS(ADI__MXDIM)	! Dimensions

	REAL TOBS			! Observation time (secs)
	REAL TOTAL			! Total count/sec
	REAL PARAM(NPAMAX)		! Model parameters
	REAL LB(NPAMAX)			! Parameter lower bounds
	REAL UB(NPAMAX)			! Parameter upper bounds
	REAL LE(NPAMAX)			! Lower parameter errors
	REAL UE(NPAMAX)			! Upper parameter errors
	REAL Z				! Redshift [ Eobs/Esource=1/(1+z) ]

      INTEGER			BGID			! Bgnd file
      INTEGER			DETID			! Detector config info
      INTEGER			IFID			! I/p containing resp
      INTEGER			MFID			! Model spec id
      INTEGER			NCH			! # chars used in string
      INTEGER 			NCHAN			! Channels in spectrum
      INTEGER			OFID			! Output dataset id
      INTEGER			SPID			! Spectral object
      INTEGER			TIMID			! Timing info

      LOGICAL 			BG			! B/g spectrum to be added?
      LOGICAL 			FOUND			! Instrument response found?
      LOGICAL 			FROZEN(NPAMAX)		! Frozen parameter flag
      LOGICAL 			OK			! Data OK?
      LOGICAL 			POISS			! Poisson errors required?

*  Version :
      CHARACTER*30 		VERSION
	PARAMETER		( VERSION = 'SSIM VERSION 1.8-3' )
*-

*  Version
      CALL MSG_PRNT( VERSION )

*  Initialise
      CALL AST_INIT()
      CALL SPEC_INIT( STATUS )

*  Model GENUS
      MODEL.GENUS = 'SPEC'

*  Get instrument response
      FOUND = .FALSE.

*  Read input file
      CALL USI_ASSOC( 'INP', '*', 'READ', IFID, STATUS )
      CALL FIT_GETINS( IFID, 0, 1, FOUND, INSTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF(.NOT.FOUND)THEN
	CALL MSG_PRNT( 'No instrument response component found' )
	GOTO 99
      END IF

* Get model specification
      CALL USI_ASSOC( 'MODEL', '*', 'READ', MFID, STATUS )
      CALL FIT_MODGET( MFID, MODEL, NPAR, PARAM, LB, UB, LE, UE,
     :                 FROZEN, STATUS )
      IF(STATUS.NE.SAI__OK) GOTO 99

* Background spectrum
      CALL USI_ASSOC( 'BG', 'BinDS', 'READ', BGID, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
	BG = .TRUE.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
	CALL ERR_ANNUL(STATUS)
	BG = .FALSE.
      ELSE
	GOTO 99
      END IF

* Poisson deviate?
      CALL USI_GET0L( 'POISS', POISS, STATUS )
      IF ( POISS ) THEN
	CALL USI_GET0R( 'OBLEN', TOBS, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get red-shift
      CALL SFIT_GETZ( Z, STATUS )

*  Output dataset
      CALL BDI_NEW( 'Spectrum', 1, NCHAN, 'REAL', SPID, STATUS )
      CALL USI_CREAT( 'OUT', SPID, OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Set up the only OBDAT component needed (no. of channels, taken from response)
      CALL ADI_CGET0I( INSTR.R_ID, 'NCHAN', NCHAN, STATUS )
      OBDAT.NDAT = NCHAN
      OBDAT.V_ID = ADI__NULLID

*  Set up PREDDAT (no workspace required)
      PREDDAT.CONVOLVE = .TRUE.
      CALL FIT_PREDSET( IFID, 1, .FALSE., OBDAT, PREDDAT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Apply redshift to model space energy bounds
      CALL SFIT_APPRED( Z, PREDDAT.NMBOUND, %VAL(PREDDAT.MLBNDPTR),
     :                  %VAL(PREDDAT.MUBNDPTR), STATUS )

*  Copy ancillary stuff
      CALL UDI_COPANC( IFID, 'grf', OFID, STATUS )

*  Read timing info from input, adjust and write to output
      IF ( POISS ) THEN
        CALL TCI_GETID( IFID, TIMID, STATUS )
        CALL ADI_CPUT0R( TIMID, 'ObsLength', TOBS, STATUS )
        CALL ADI_CPUT0R( TIMID, 'Exposure', TOBS, STATUS )
        CALL ADI_CPUT0R( TIMID, 'EffExposure', TOBS, STATUS )
        CALL TCI_PUTID( OFID, TIMID, STATUS )
      END IF

*  Change target
      CALL DCI_GETID( IFID, DETID, STATUS )
      CALL ADI_CPUT0C( DETID, 'Target', 'Simulated data', STATUS )
      CALL DCI_PUTID( OFID, DETID, STATUS )

*  Copy ChannelSpec from response matrix to output axis
      CALL ADI_CMAPR( INSTR.R_ID, 'ChannelSpec', 'READ', CHPTR, STATUS )
      CALL BDI_AXPUT1R( OFID, 1, 'Data', NCHAN, %VAL(CHPTR), STATUS )
      CALL ADI_CUNMAP( INSTR.R_ID, 'ChannelSpec', CHPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH(STATUS)

*  Map data etc and write labels
      CALL BDI_MAPR( OFID, 'Data', 'WRITE', DPTR, STATUS )
      IF ( POISS ) THEN
	CALL BDI_MAPR( OFID, 'Variance', 'WRITE', VPTR, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      CALL BDI_PUT0C( OFID, 'Label', 'Intensity', STATUS )
      CALL BDI_PUT0C( OFID, 'Units', 'count/sec', STATUS )
      CALL BDI_AXPUT0C( OFID, 1, 'Label', 'Channel', STATUS )
      IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH( STATUS )

*  Copy spectral model into ASTERIX.SPEC_MODEL auxilliary data
      CALL AUI_PUTID( OFID, 'ASTERIX.SPEC_MODEL', MFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Workspace for model stack
      CALL SFIT_MAPMODSTK( 1, PREDDAT, MODEL.STACKPTR, STATUS )

*  Calculate predicted data
      CALL FIT_PREDDAT( 1, 1, OBDAT, INSTR, PREDDAT, MODEL, PARAM,
     :                  1, %VAL(DPTR), STATUS )

*  Map background data and ...
      IF ( BG ) THEN
        CALL BDI_CHK( BGID, 'Data', OK, STATUS )
        CALL BDI_GETSHP( BGID, ADI__MXDIM, DIMS, NDIM, STATUS )
	IF ( .NOT. OK ) THEN
	  CALL MSG_PRNT( 'No background spectrum available' )
	  BG = .FALSE.
	  GOTO 10
	END IF
	IF ( DIMS(1) .NE. NCHAN ) THEN
	  CALL MSG_PRNT( 'Background spectrum has wrong number of'/
     :        /' channels - not used' )
	  BG = .FALSE.
	  GOTO 10
	END IF
	CALL BDI_MAPR( BGID, 'Data', 'READ', BPTR, STATUS )

* ...write b/g dataset name to output file
	CALL ADI_FTRACE( BGID, I, BGOBJ, FILE, STATUS )
        CALL AUI_PUT0C( OFID, 'ASTERIX.BGFILE', BGOBJ, STATUS )
	IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH( STATUS )

      END IF
 10   CONTINUE

*  If necessary pass to subroutine to add b/g and calculate variances
      IF ( POISS .OR. BG ) THEN
	CALL SSIM_BGERR( NCHAN, POISS, TOBS, BG, %VAL(BPTR), %VAL(DPTR),
     :                   %VAL(VPTR), STATUS )
      END IF

*  Report total count rate
      CALL ARR_SUM1R( NCHAN, %VAL(DPTR), TOTAL, STATUS )
      CALL MSG_BLNK
      CALL MSG_SETR('TOT',TOTAL)
      CALL MSG_PRNT('Total count rate = ^TOT')

*  Construct graph legend strings
      CALL ADI_CGET0C( DETID, 'Mission', TELES, STATUS )
      CALL ADI_CGET0C( DETID, 'Instrument', INSTRUM, STATUS )
      IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)
      CALL MSG_SETC( 'TEL', TELES )
      CALL MSG_SETC( 'INS', INSTRUM )
      CALL MSG_MAKE( '^TEL / ^INS : Simulated data', LEGEND(1), NCH )
      IF ( POISS ) THEN
        CALL MSG_SETR( 'TOBS', TOBS )
        CALL MSG_MAKE( 'Observation length : ^TOBS sec', LEGEND(2),
     :                 NCH )
      ELSE
	LEGEND(2) = 'No statistical noise'
      END IF
      CALL MSG_SETC( 'SPEC', MODEL.SPEC )
      CALL MSG_MAKE( 'Spectral model : ^SPEC', LEGEND(3), NCH )

*  Attach GCB and write attributes
      CALL GCB_LCONNECT( STATUS )
      DO I = 1, 3
        CALL GCB_SET1C( 'TITLE_TEXT', I, 1, LEGEND(I), STATUS )
      END DO
      CALL GCB_SETI( 'TITLE_N', 3, STATUS )
      CALL GCB_FSAVE( OFID, STATUS )
      CALL GCB_DETACH( STATUS )
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH(STATUS)

*  History
      CALL HSI_ADD( OFID, VERSION, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  SSIM_BGERR - Adds b/g and calculates poisson errors
	SUBROUTINE SSIM_BGERR(NDAT,POISS,TOBS,BG,BGVAL,DVAL,VVAL,STATUS)
*    Description :
*     Adds b/g to data array if BG is .TRUE.
*     Calculates poisson variances if POISS=.TRUE.
*     NOTE: If background count in a channel is -ve then it is NOT added,
*     i.e. it is assumed to be zero.
*    History :
*      6 Jul 87: Original (BHVAD::TJP)
*     14 Mar 91: Variances replace errors (TJP)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
	INCLUDE 'SAE_PAR'
*    Status :
	INTEGER STATUS
*    Import :
	INTEGER NDAT		! No of data values
	LOGICAL POISS		! Poisson errors required?
	REAL TOBS		! Observation time in secs
	LOGICAL BG		! B/g to be added?
	REAL BGVAL(*)		! B/g data
*    Import-Export :
	REAL DVAL(*)		! Data array
*    Export :
	REAL VVAL(*)		! Variance array
*    Local Constants :
*    Local variables :
	INTEGER I
	INTEGER MATH_POISS
*-

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

	DO I=1,NDAT
	  IF(BG)THEN

* Add b/g (if +ve)
	    IF(BGVAL(I).GT.0.0)THEN
	      DVAL(I)=DVAL(I)+BGVAL(I)
	    ENDIF
	  ENDIF
	  IF(POISS)THEN

* Errors and noise
	    VVAL(I)=DVAL(I)/TOBS
	    DVAL(I)=REAL(MATH_POISS(TOBS*DVAL(I)))/TOBS
	  ENDIF
	ENDDO

	END
