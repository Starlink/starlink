*+  SSIM - Generates simulated spectral dataset
      SUBROUTINE SSIM( STATUS )
*    Description :
*     A composite spectral model is folded through an instrument response
*     (for a given observation duration) and a background spectrum and Poisson
*     noise are optionally added, to give a simulated spectral observation.
*     Since results are given in count/chan/sec the observation time need
*     only be specified where Poisson errors are to be included.
*    Environment parameters :
*     INP=UNIV(R)
*            dataset containing instrument response
*     FIT_MOD=UNIV(R)
*            data object containing model specification
*     BG_DATA=UNIV(R)
*            background spectral dataset
*     POISS_ERRS=LOGICAL(R)
*            Poisson noise to be added?
*     OBS_LENGTH=REAL(R)
*            observation length in seconds
*     REDSHIFT=REAL(R)
*            redshift
*     OUT=UNIV(W)
*            output spectrum
*    Method :
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
*
*    Type definitions :
	IMPLICIT NONE
*    Global constants :
	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'
	INCLUDE 'PAR_ERR'
	INCLUDE 'FIT_PAR'
*    Structure definitions :
	INCLUDE 'FIT_STRUC'
*    Status :
	INTEGER STATUS
*    Function declarations :
	INTEGER CHR_LEN
*    Local variables :
	RECORD/DATASET/ OBDAT		! Observed dataset (dummy structure)
	RECORD/PREDICTION/ PREDDAT	! Data predicted by model
	RECORD/INSTR_RESP/ INSTR	! Instrument response
	RECORD /MODEL_SPEC/ MODEL	! Model specification
	CHARACTER*(DAT__SZLOC) ILOC	! Locator to input data (or ref object)
	CHARACTER*(DAT__SZLOC) OLOC	! Output spectrum dataset
	CHARACTER*(DAT__SZLOC) BGLOC	! Background spectrum
	CHARACTER*(DAT__SZLOC) MLOC	! Locator to model_spec object
	CHARACTER*(DAT__SZLOC) CHLOC	! Locator to CHANNEL compt of response
	CHARACTER*(DAT__SZLOC) ASTLOC	! Locator to ASTERIX box
	CHARACTER*(DAT__SZLOC) HLOC	! Locator to HEADER
	CHARACTER*(DAT__SZLOC) LOC	! Locator
	CHARACTER*100 BGOBJ		! B/g dataset name
	CHARACTER*100 FILE		! File name
	CHARACTER*80 TELES		! Telescope name
	CHARACTER*80 INSTRUM		! Instrument name
	CHARACTER*80 CTOBS		! Character conversion of TOBS
	CHARACTER*80 MODSPEC		! Spectral model specification
	CHARACTER*80 LEGEND(3)		! Legend for graphics
	LOGICAL FROZEN(NPAMAX)		! Frozen parameter flag
	LOGICAL POISS			! Poisson errors required?
	LOGICAL BG			! B/g spectrum to be added?
	LOGICAL THERE			! Component present?
	LOGICAL FOUND			! Instrument response found?
	LOGICAL OK			! Data OK?
	INTEGER DPTR			! Pointer to data array
	INTEGER VPTR			! Pointer to variance array
	INTEGER BPTR			! Pointer to b/g array
	INTEGER CHPTR			! Pointer to i/p channel spec array
	INTEGER AXPTR			! Pointer to o/p axis array
	INTEGER I			!
	INTEGER NPAR			! No of parameters
	INTEGER NCHAN			! No of channels in spectrum
	INTEGER NCHAR			! No of characters in string
	INTEGER NDIM			! Dimensionality
	INTEGER DIMS(DAT__MXDIM)	! Dimensions
	REAL TOBS			! Observation time (secs)
	REAL TOTAL			! Total count/sec
	REAL PARAM(NPAMAX)		! Model parameters
	REAL LB(NPAMAX)			! Parameter lower bounds
	REAL UB(NPAMAX)			! Parameter upper bounds
	REAL LE(NPAMAX)			! Lower parameter errors
	REAL UE(NPAMAX)			! Upper parameter errors
	REAL Z				! Redshift [ Eobs/Esource=1/(1+z) ]
*    Local data :
*    Version :
	CHARACTER*30 VERSION
	PARAMETER		(VERSION='SSIM VERSION 1.8-0')
*-

* Version
	CALL MSG_PRNT( VERSION )

* Initialise
	CALL AST_INIT
        CALL SPEC_INIT( STATUS )

* Model GENUS
	MODEL.GENUS='SPEC'

* Get instrument response
	FOUND=.FALSE.
	CALL USI_DASSOC('INP','READ',ILOC,STATUS)
	CALL FIT_INSGET(ILOC,0,FOUND,INSTR,STATUS)
	IF(STATUS.NE.SAI__OK) GO TO 9000
	IF(.NOT.FOUND)THEN
	  CALL MSG_PRNT( 'No instrument response component found' )
	  GO TO 9000
	ENDIF

* Get model specification
	CALL USI_DASSOC('FIT_MOD','READ',MLOC,STATUS)
	CALL FIT_MODGET(MLOC,MODEL,NPAR,PARAM,LB,UB,LE,UE,FROZEN,STATUS)
	IF(STATUS.NE.SAI__OK) GO TO 9000

* User input
	CALL USI_DASSOC('BG_DATA','READ',BGLOC,STATUS)
	IF(STATUS.EQ.SAI__OK)THEN
	  BG=.TRUE.
	ELSE IF(STATUS.EQ.PAR__NULL)THEN
	  CALL ERR_ANNUL(STATUS)
	  BG=.FALSE.
	ELSE
	  GO TO 9000
	ENDIF
	CALL USI_GET0L('POISS_ERRS',POISS,STATUS)
	IF(POISS)THEN
	  CALL USI_GET0R('OBS_LENGTH',TOBS,STATUS)
	ENDIF
        CALL SFIT_GETZ( Z, STATUS )

* Output dataset
	CALL USI_DCREAT('OUT','SPECTRUM',0,0,STATUS)
	CALL USI_DASSOC('OUT','WRITE',OLOC,STATUS)
	IF(STATUS.NE.SAI__OK) GO TO 9000

* Set up the only OBDAT component needed (no. of channels, taken from response)
	CALL DAT_FIND(INSTR.ELOC,'CHANNEL',CHLOC,STATUS)
	CALL CMP_SIZE(CHLOC,'CHANNEL_SPEC',NCHAN,STATUS)
	OBDAT.NDAT=NCHAN

* Set up PREDDAT (no workspace required)
	PREDDAT.CONVOLVE=.TRUE.
	CALL FIT_PREDSET(ILOC,1,.FALSE.,OBDAT,PREDDAT,STATUS)
	IF(STATUS.NE.SAI__OK) GO TO 9000

* Apply redshift to model space energy bounds
        CALL SFIT_APPRED( Z, PREDDAT.NMBOUND,
     :                    %VAL(PREDDAT.MLBNDPTR),
     :                    %VAL(PREDDAT.MUBNDPTR), STATUS )

* Copy MORE box to output file, amend HEADER and write exposure time
	CALL BDA_COPMORE(ILOC,OLOC,STATUS)
	CALL BDA_LOCHEAD(OLOC,HLOC,STATUS)
	CALL CMP_PUT0C(HLOC,'TARGET','Simulated data',STATUS)
	CALL CMP_PUT0R(HLOC,'OBS_LENGTH',TOBS,STATUS)
	CALL CMP_PUT0R(HLOC,'EXPOSURE_TIME',TOBS,STATUS)
	IF(STATUS.NE.SAI__OK) CALL ERR_ANNUL(STATUS)

* Set up output components
	CALL BDA_CREDATA(OLOC,1,NCHAN,STATUS)
	CALL BDA_CREAXES(OLOC,1,STATUS)
	CALL BDA_CREAXVAL(OLOC,1,.FALSE.,NCHAN,STATUS)
	IF(POISS)THEN
	  CALL BDA_CREVAR(OLOC,1,NCHAN,STATUS)
	ENDIF
	IF(STATUS.NE.SAI__OK) GO TO 9000

	CALL DAT_FIND(CHLOC,'CHANNEL_SPEC',LOC,STATUS)
	CALL DAT_MAPV(LOC,'_REAL','READ',CHPTR,NCHAN,STATUS)
	CALL BDA_MAPAXVAL(OLOC,'WRITE',1,AXPTR,STATUS)
	CALL ARR_COP1R(NCHAN,%VAL(CHPTR),%VAL(AXPTR),STATUS)
	CALL BDA_UNMAPAXVAL(OLOC,1,STATUS)
	CALL DAT_ANNUL(CHLOC,STATUS)
	CALL DAT_ANNUL(LOC,STATUS)
	IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)

* Map DATA_ARRAY etc, write labels
	CALL BDA_MAPDATA(OLOC,'WRITE',DPTR,STATUS)
	IF(POISS)THEN
	  CALL BDA_MAPVAR(OLOC,'WRITE',VPTR,STATUS)
	ENDIF
	IF(STATUS.NE.SAI__OK) GO TO 9000
	CALL BDA_PUTLABEL(OLOC,'Intensity',STATUS)
	CALL BDA_PUTUNITS(OLOC,'count/sec',STATUS)
	CALL BDA_PUTAXLABEL(OLOC,1,'Channel',STATUS)
	IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)

* Copy spectral model into MORE.ASTERIX.SPEC_MODEL
	CALL BDA_LOCAST(OLOC,ASTLOC,STATUS)
	CALL DAT_THERE(ASTLOC,'SPEC_MODEL',OK,STATUS)
	IF(OK)THEN
	  CALL DAT_ERASE(ASTLOC,'SPEC_MODEL',STATUS)
	ENDIF
	CALL DAT_COPY(MLOC,ASTLOC,'SPEC_MODEL',STATUS)
	IF(STATUS.NE.SAI__OK) GO TO 9000

* Workspace for model stack
	CALL DYN_MAPR(1,PREDDAT.NMDAT*MAXSTACK,MODEL.STACKPTR,
     :  STATUS)

* Calculate predicted data
	CALL FIT_PREDDAT(1,1,OBDAT,INSTR,PREDDAT,MODEL,PARAM,1,%VAL(DPTR),
     :  STATUS)

* Map background data and ...
	IF(BG)THEN
	  CALL BDA_CHKDATA(BGLOC,OK,NDIM,DIMS,STATUS)
	  IF(.NOT.OK)THEN
	    CALL MSG_PRNT( 'No background spectrum available' )
	    BG=.FALSE.
	    GO TO 100
	  ENDIF
	  IF(DIMS(1).NE.NCHAN)THEN
	    CALL MSG_PRNT( 'Background spectrum has wrong number of'/
     :        /' channels - not used' )
	    BG=.FALSE.
	    GO TO 100
	  ENDIF
	  CALL BDA_MAPDATA(BGLOC,'READ',BPTR,STATUS)

* ...write b/g dataset name to output file (ASTERIX box)
	  CALL HDS_TRACE(BGLOC,I,BGOBJ,FILE,STATUS)
	  CALL DAT_THERE(BGLOC,'BGFILE',OK,STATUS)
	  IF(.NOT.THERE)THEN
	    CALL DAT_NEWC(ASTLOC,'BGFILE',80,0,0,STATUS)
	  ENDIF
	  CALL CMP_PUT0C(ASTLOC,'BGFILE',BGOBJ,STATUS)
	  IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)
	ENDIF
 100	CONTINUE

* If necessary pass to subroutine to add b/g and calculate variances
	IF(POISS.OR.BG)THEN
	  CALL SSIM_BGERR(NCHAN,POISS,TOBS,BG,%VAL(BPTR),%VAL(DPTR),
     :    %VAL(VPTR))
	ENDIF

* Report total count rate
	CALL ARR_SUMR(1,NCHAN,%VAL(DPTR),'_REAL',TOTAL,STATUS)
	CALL MSG_BLNK
	CALL MSG_SETR('TOT',TOTAL)
	CALL MSG_PRNT('Total count rate = ^TOT')

* Graph legend
	CALL CMP_GET0C(HLOC,'OBSERVATORY',TELES,STATUS)
	CALL CMP_GET0C(HLOC,'INSTRUMENT',INSTRUM,STATUS)
	IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)
	NCHAR=0
	IF(CHR_LEN(TELES).GT.0)THEN
	  NCHAR=CHR_LEN(TELES)
	  LEGEND(1)=TELES(1:NCHAR)//' : '
	  NCHAR=NCHAR+3
	ENDIF
	IF(CHR_LEN(INSTRUM).GT.0)THEN
	  LEGEND(1)=LEGEND(1)(1:NCHAR)//
     :    INSTRUM(1:CHR_LEN(INSTRUM))//' : '
	  NCHAR=NCHAR+CHR_LEN(INSTRUM)+3
	ENDIF
	LEGEND(1)=LEGEND(1)(1:NCHAR)//'Simulated data'
	IF(POISS)THEN
	  CALL CHR_RTOC(TOBS,CTOBS,I)
	  LEGEND(2)='Observation length : '//CTOBS(1:CHR_LEN(CTOBS))/
     :    /' sec'
	ELSE
	  LEGEND(2)='No statistical noise'
	ENDIF
	CALL CMP_GET0C(MLOC,'SPEC',MODSPEC,STATUS)
	LEGEND(3)='Spectral model : '//MODSPEC(1:CHR_LEN(MODSPEC))

        CALL GCB_ATTACH('LOCAL',STATUS)
	DO I=1,3
          CALL GCB_SET1C('TITLE_TEXT',I,1,LEGEND(I),STATUS)
	ENDDO
        CALL GCB_SETI('TITLE_N',3,STATUS)
        CALL GCB_SAVE(OLOC,STATUS)
        CALL GCB_DETACH(STATUS)

	IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)

* History
	CALL HIST_ADD(OLOC,VERSION,STATUS)

* Tidy up
 9000	CALL AST_CLOSE
        CALL AST_ERR( STATUS )

	END


*+  SSIM_BGERR - Adds b/g and calculates poisson errors
	SUBROUTINE SSIM_BGERR(NDAT,POISS,TOBS,BG,BGVAL,DVAL,VVAL)
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
*--------------------------------------------------------------

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
