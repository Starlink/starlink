*+
*  BSUB_SMOOTH - Create background subtracted image from raw image
*
	SUBROUTINE BSUB(STATUS)

	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	INTEGER STATUS
	INTEGER BSUB_INT
	INTEGER ISTATUS
	INTEGER   ICPU
	REAL   CPU
*-

*
	ISTATUS = BSUB_INT( STATUS )

	IF(.NOT.ISTATUS)THEN
	  CALL MSG_PRNT('ERROR: Failure in BSUB top level (see
     &    PSF Access routines)')
	ENDIF

	END
*
*+
*
      INTEGER FUNCTION BSUB_INT(STATUS)
*
*    Description :
*
*    Environment parameters :
*
*     INP = UNIV(R)
*           Input dataset
*     OUT = UNIV(W)
*           Output dataset
*
*    Method :
*
*    Authors :
*
*     J.P.D.Mittaz and S.R.Rosen ( UCL.MSSL.C::JPDM,SRR )
*
*    History :
*
*     03 Mar 90 : V1.2-0  Original (DJA)
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
      INCLUDE 'PAR_ERR'
      INCLUDE 'MATH_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Global variables :
*
      INCLUDE 'BSUB_CMN'
*
*    Status :
*
      INTEGER STATUS
      INTEGER OLDSTATUS
*
*    Local constants :
*
      INTEGER			MAXLINES   ! Maximum amount of hist text
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)    BLOC             ! Background model
      CHARACTER*(DAT__SZLOC)	ILOC             ! Input dataset
      CHARACTER*(DAT__SZLOC)    OLOC             ! Output dataset

	REAL			SIZE		 ! pixel size in arc secs
	REAL			BASE, SCALE	 ! axis attributes

	REAL   			FRAC(4),RADII(4) ! % values of PSF

	INTEGER 		DIM		 ! axis attribute

	REAL			MEAN, SDEV       ! Mean and std. deviation
	INTEGER                 DIMS(DAT__MXDIM) ! Input dimensions
	INTEGER                 TDIMS(DAT__MXDIM)!
	INTEGER                 PSFDIMS(DAT__MXDIM)!
	INTEGER                 PSF_FDIMS(DAT__MXDIM)!
	INTEGER                 PSF_WDIMS(DAT__MXDIM)!

	INTEGER			NDIM             ! Input dimensionality
	INTEGER                 NELM             ! Total number of data items
	INTEGER                 NREC             ! Amount of TEXT used
        INTEGER                 DATA_PTR         ! Output data
	INTEGER                 VAR_PTR
	INTEGER                 BDATA_PTR
	INTEGER                 QUAL_PTR
	INTEGER                 TNDIM
	INTEGER                 ODATA_PTR
	INTEGER                 PSF_FUNC         ! Pointer to PSF func
	INTEGER                 PSF_STORE        ! Pointer to PSF func
	INTEGER                 PSF_SHIFT        ! Pointer to PSF func
	INTEGER                 PSF_WORK         ! Pointer to PSF func

        LOGICAL                 CREBACK          ! Create background model
        LOGICAL                 CREBSUB          ! Create background sub'd img
	LOGICAL			IN_PRIM          ! Input primitive?
	LOGICAL                 DATA_OK          ! Input data ok?
	LOGICAL                 QUAL_OK, VAR_OK
	LOGICAL                 ANYBAD
*
*    Version :
*
	CHARACTER*30   		VERSION
	PARAMETER               (VERSION = 'BSUB Version 1.8-0')

	CHARACTER*3		  SC_UN		!comparison unit type
	CHARACTER*80		  UNITS		!units of axis

	INTEGER   		  X_S_PTR	!locator to X source pos
	INTEGER   		  Y_S_PTR	!   "       Y
	INTEGER   		  R_S_PTR	!   "       source radius
	INTEGER  		  N_REMOVED	!No. of removed sources
	INTEGER   		  SDIM		!dim of above arrays
	INTEGER   		  BSUB_SUBTRACT_BACKGROUND
	INTEGER   		  ISTATUS
	INTEGER   		  BSUB_ERROR_HANDLER
	INTEGER   		  NEXPAND	!for UIS_TEXT
	INTEGER  		  I		!counter

	CHARACTER*26 		  CRASH_ID(30)		!crash identifier array
	CHARACTER*132		  TEXT			!text string
	CHARACTER*132		  OUT_HISTORY(100)	!history string
	CHARACTER*132		  EXPAND_TEXT(5)	!for UIS_TEXT

	REAL                      SCF		!conv factor to arc secs

	REAL   			  AXIS1(3)	!axis variables
	REAL   			  AXIS2(3)	!      "
	REAL  			  R_95		!95% radius

	INTEGER   SRCMAP_PTR			!

	INTEGER   COMPX_PTR, COMPY_PTR

	DATA CRASH_ID/
     &  'BSUB_SUBTRACT_BACKGROUND  ','BSUB_GET_SMOOTHED_IMAGE   ',
     &  'BSUB_COMPUTE_FACTORIALS   ','BSUB_GET_BOX_LIMITS       ',
     &  'BSUB_GET_HISTOGRAM        ','BSUB_WRITE_BACK           ',
     &  'BSUB_FAST_SMOOTH          ','BSUB_UPDATE               ',
     &  'BSUB_BINOMIAL_ERROR       ','BSUB_FILL_FUNC            ',
     &  'BSUB_FIT_HISTOGRAM        ','BSUB_GAUSS_ELIM           ',
     &  'BSUB_GET_HEADER_INFO      ','BSUB_GET_PSF_GRID         ',
     &  'BSUB_MARQ_COEF            ','BSUB_MIN_FUNC             ',
     &  'BSUB_POISSON              ','BSUB_SORT_COVAR           ',
     &  'BSUB_FIT_PSF_TEMPLATE     ','BSUB_UPDATE_SOURCE_MAP    ',
     &  'BSUB_UNRELIABLE           ','BSUB_IMAGE_LIMITS         ',
     &  '                          ','                          ',
     &  '                          ','                          ',
     &  '                          ','                          ',
     &  '                          ','                          '/
*-

*
*  set up top level error handler

	BSUB_INT=.TRUE.

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	ISTATUS=.TRUE.

	MAX_HIST=100

*    Version announcement
      CALL MSG_PRNT( VERSION )

*    Get Output write parameter
      CALL USI_GET0L('OUT_MESSAGES',LOUD,STATUS)
      IF(STATUS.NE.SAI__OK)RETURN

*    Initialise
      CALL AST_INIT( STATUS )

      CALL PSF_INIT( STATUS )

*    Associate the input and output objects (files)
      CALL USI_ASSOCI( 'INP', 'READ', ILOC, IN_PRIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      CALL USI_ASSOCO( 'OUT', 'DATASET', OLOC, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
        CREBSUB = .FALSE.
        CALL ERR_ANNUL( STATUS )
      ELSE
        CREBSUB = .TRUE.
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Background model
      CALL USI_ASSOCO( 'BGND', 'BACKGROUND', BLOC, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
        CREBACK = .FALSE.
        CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
        CREBACK = .TRUE.
      ELSE
        GOTO 99
      END IF

*    Must have one or othjer
      IF ( .NOT. (CREBSUB .OR. CREBACK) ) THEN
        CALL MSG_PRNT( '! Must either background subtracted image'/
     :                                               /' or model' )
        GOTO 99
      END IF

*    Copy file info from one to other and get the PSF handle
      IF ( CREBSUB ) THEN
        CALL HDX_COPY( ILOC, OLOC, STATUS )
        CALL DAT_ERASE( OLOC, 'DATA_ARRAY', STATUS )
	CALL HIST_ADD( OLOC, VERSION, STATUS )
      END IF
      IF ( CREBACK ) THEN
        CALL HDX_COPY( ILOC, BLOC, STATUS )
        CALL DAT_ERASE( BLOC, 'DATA_ARRAY', STATUS )
	CALL HIST_ADD( BLOC, VERSION, STATUS )
      END IF

*    Associate psf
      CALL PSF_ASSOCI( ILOC, PSFHAN, STATUS )

*    Map input image
      CALL BDA_MAPDATA( ILOC, 'READ', DATA_PTR, STATUS )

*    Check data
      CALL BDA_CHKDATA( ILOC, DATA_OK, NDIM, DIMS, STATUS )

      IF ( DATA_OK ) THEN

*       Create output data in case input was not floating point
         IF ( CREBSUB ) THEN

           CALL BDA_CREDATA( OLOC, NDIM, DIMS, STATUS )
	   CALL BDA_MAPDATA(OLOC, 'WRITE', ODATA_PTR, STATUS)

*         Get variance if present
	   CALL BDA_CHKVAR( OLOC, VAR_OK, TNDIM, TDIMS, STATUS )
C	   IF ( VAR_OK ) THEN
C	     CALL BDA_MAPVAR( OLOC, 'UPDATE', VAR_PTR, STATUS )
C	   ELSE
C	     CALL BDA_CREVAR( OLOC, NDIM, DIMS, STATUS )
C	     CALL BDA_MAPVAR( OLOC, 'WRITE', VAR_PTR, STATUS )
C	   END IF
           IF ( STATUS .NE. SAI__OK ) GOTO 99
         END IF

*       Create background model
         IF ( CREBACK ) THEN
           CALL BDA_CREDATA( BLOC, NDIM, DIMS, STATUS )
	   CALL BDA_MAPDATA( BLOC, 'WRITE', BDATA_PTR, STATUS)
           IF ( .NOT. CREBSUB ) THEN
             ODATA_PTR = BDATA_PTR
             OLOC = BLOC
           END IF
         END IF
         CALL DYN_MAPR( NDIM, DIMS, VAR_PTR, STATUS )
	 CALL ARR_INIT1R( 0.0, NELM, %VAL(VAR_PTR), STATUS )

*       Make copy of input data in output (bsub'd unless bmodel only being
*       created)
	  CALL ARR_COP1R( DIMS(1)*DIMS(2), %VAL(DATA_PTR),
     :                    %VAL(ODATA_PTR), STATUS )

	  IF(STATUS.NE.SAI__OK)THEN

	    CALL ERR_REP(' ','Error in BDA_MAPDATA',STATUS)
	    GOTO 99

	  END IF

	ELSE

	  CALL ERR_REP( ' ', 'Invalid data', STATUS )

	END IF
	IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Find total number of elements in dataset
        CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*      Map quality from input
	CALL BDA_CHKQUAL( ILOC, QUAL_OK, TNDIM, TDIMS, STATUS )
	IF ( QUAL_OK ) THEN
	  CALL BDA_MAPLQUAL( ILOC, 'READ', ANYBAD, QUAL_PTR, STATUS )
	  IF ( .NOT. ANYBAD ) THEN
	    QUAL_OK = .FALSE.
	    CALL BDA_UNMAPLQUAL( ILOC, STATUS )
	  END IF
	END IF

	NEXPAND=5
	EXPAND_TEXT(1)='Input dataset {INP}'
	CALL USI_TEXT(1,EXPAND_TEXT,NEXPAND,STATUS)

	IF(.NOT.LOUD)THEN
	  CALL MSG_PRNT(' ')
	  DO I=1,NEXPAND
	    CALL MSG_PRNT(EXPAND_TEXT(I))
	  END DO
	ENDIF

        NHISTORY=0
	DO I=1,NEXPAND
	  NHISTORY=NHISTORY+1
	  HISTORY(I) = EXPAND_TEXT(I)
	END DO
	NHISTMAX=NEXPAND

*  Find out if image scale is in degrees or arc mins and then get axis scale
*  in arc secs
	CALL BDA_GETAXUNITS(ILOC, 1, UNITS, STATUS)
        CALL CONV_UNIT2R( UNITS, SCF, STATUS )

        IF ( STATUS .NE. SAI__OK ) THEN
           CALL MSG_PRNT( 'Unrecognised axis units' )
           GOTO 99
        ELSE

d           SCF = SCF * MATH__RTOD * 3600.0
d	    SCF=2.908882E-4		!temporary fix arcmins to radians

        END IF

*    Get axis 1 stuff
	CALL BDA_GETAXVAL( ILOC, 1, BASE, SCALE, DIM, STATUS )

*    Get min and max values of the axes (for spatial filter)
	AXIS1(1)=BASE
	AXIS1(2)=BASE+(DIMS(1)-1)*SCALE
	AXIS1(3)=SCALE

*    Convert to arcsec
	IF(STATUS.EQ.SAI__OK)THEN
	  SIZE = ABS(SCALE * SCF)
	ELSE
	  CALL ERR_REP(' ',' Error reading pixel scale ')
	  GOTO 99
	END IF

*    Get axis 2 stuff
	CALL BDA_GETAXVAL( ILOC, 2, BASE, SCALE, DIM, STATUS )

*    Get min and max values of the axes (for spatial filter)
	AXIS2(1)=BASE
	AXIS2(2)=BASE+(DIMS(2)-1)*SCALE
	AXIS2(3)=SCALE
*
*  grab a locator to the MORE box
	CALL DAT_FIND(ILOC, 'MORE', LOCMORE, STATUS)

*    Get 68,95,99% limits on PSF
*    Get pixel size in radians

       SCALE=SIZE		!*4.8481368E-06

*    Get radii
      FRAC(1)=0.68
      FRAC(2)=0.80
      FRAC(3)=0.90
      FRAC(4)=0.95

      CALL PSF_ENERGY_PFL(PSFHAN,4,FRAC,0.0,0.0,RADII,STATUS)
      IF(STATUS.NE.SAI__OK)GOTO 99

      R_PSF=RADII(1)/SCALE
      R_80=RADII(2)/SCALE
      R_90=RADII(3)/SCALE
      R_95=RADII(4)/SCALE

*    Get number pixels at 68%

      NPIX_R_PSF=NINT(MATH__PI*RADII(1)*RADII(1))

*  Tell user radii of PSF

	IF(LOUD)THEN
          CALL MSG_PRNT(' ')
          CALL MSG_FMTR('RAD','F6.1',R_PSF)
          CALL MSG_PRNT('68% radius of PSF is ^RAD pixels')
          CALL MSG_FMTR('RAD','F6.1',R_80)
          CALL MSG_PRNT('80% radius of PSF is ^RAD pixels')
          CALL MSG_FMTR('RAD','F6.1',R_90)
          CALL MSG_PRNT('90% radius of PSF is ^RAD pixels')
          CALL MSG_FMTR('RAD','F6.1',R_95)
          CALL MSG_PRNT('95% radius of PSF is ^RAD pixels')
	ENDIF

*    Make sure its an odd number

	PSFDIMS(1)=INT(R_95)*2+1 +2  !(+2 for border of 1 for PSF_RESAMPLE)
	PSFDIMS(2)=INT(R_95)*2+1 +2  !                   "
	PSF_FDIMS(1)=PSFDIMS(1)-2	!for border free function
	PSF_FDIMS(2)=PSFDIMS(2)-2	!         "

	PSF_WDIMS(1)=PSFDIMS(1)
	PSF_WDIMS(2)=PSFDIMS(2)
	PSF_WDIMS(3)=2

*    Get array for PSF_GRID = contains 95% of PSF
	CALL DYN_MAPR(2,PSF_FDIMS,PSF_FUNC,STATUS)
	CALL DYN_MAPR(2,PSFDIMS,PSF_STORE,STATUS)
	CALL DYN_MAPR(2,PSFDIMS,PSF_SHIFT,STATUS)
	CALL DYN_MAPR(3,PSF_WDIMS,PSF_WORK,STATUS)

*  Image size
	IF(LOUD)THEN
	  CALL MSG_PRNT(' ')
	ENDIF
	NHISTORY=NHISTORY+1
	HISTORY(NHISTORY)=' '
	IF(LOUD)THEN
	  CALL MSG_SETI('XDIM',DIMS(1))
	  CALL MSG_SETI('YDIM',DIMS(2))
	  CALL MSG_PRNT('Image is ^XDIM x ^YDIM')
	ENDIF

	NHISTORY=NHISTORY+1
	WRITE(TEXT,'(''Image is '',I3,'' x '',I3)')DIMS(1),DIMS(2)
	HISTORY(NHISTORY)=TEXT
*
*
*  map source detection arrays
*
	SDIM=10000
	CALL DYN_MAPI(1,SDIM,X_S_PTR,STATUS)
	CALL DYN_MAPI(1,SDIM,Y_S_PTR,STATUS)
	CALL DYN_MAPI(1,SDIM,R_S_PTR,STATUS)

	CALL DYN_MAPI(1,DIMS(1),COMPX_PTR,STATUS)
	CALL DYN_MAPI(1,DIMS(2),COMPY_PTR,STATUS)

	CALL DYN_MAPR(2,DIMS,SRCMAP_PTR,STATUS)
*
* create quality array
*
	CALL BDA_CREQUAL(OLOC,2,DIMS,STATUS)
	CALL BDA_PUTMASK(OLOC,QUAL__MASK,STATUS)
	CALL BDA_MAPQUAL(OLOC,'WRITE',QUAL_PTR,STATUS)
*
* calculate the background
*
        ISTATUS=BSUB_SUBTRACT_BACKGROUND(DIMS(1),DIMS(2),
     &  %VAL(COMPX_PTR),%VAL(COMPY_PTR),SIZE,AXIS1,AXIS2,
     &  PSF_FDIMS(1),PSF_FDIMS(2),%VAL(PSF_FUNC),PSF_WDIMS(1),
     &  PSF_WDIMS(2),PSF_WDIMS(3),%VAL(PSF_WORK),PSFDIMS(1),
     &  PSFDIMS(2),%VAL(PSF_STORE),%VAL(PSF_SHIFT),CREBSUB,CREBACK,
     &  %VAL(ODATA_PTR),%VAL(BDATA_PTR),
     &  %VAL(VAR_PTR),SDIM,%VAL(X_S_PTR),%VAL(Y_S_PTR),
     &	%VAL(R_S_PTR),%VAL(SRCMAP_PTR),N_REMOVED,OLOC,%VAL(QUAL_PTR),
     &  STATUS)

	IF(.NOT.ISTATUS)THEN

	   CALL MSG_SETC('ROUTINE',CRASH_ID(ROUTINE_ID))
	   CALL MSG_PRNT(' Fortran error in lower subroutine ^ROUTINE ')

	ELSE

*         Create background model
           IF ( CREBACK ) THEN

*           Create model data
             CALL ARR_SUMS( '-', NELM, %VAL(DATA_PTR), 0, 0,
     :                           NELM, %VAL(ODATA_PTR), 0 , 0,
     :                           NELM, %VAL(BDATA_PTR), 0 , 0 )

           END IF

*
*  unmap source detection arrays
*
	  CALL DYN_UNMAP(COMPX_PTR,STATUS)
	  CALL DYN_UNMAP(COMPY_PTR,STATUS)
	  CALL DYN_UNMAP(X_S_PTR,STATUS)
	  CALL DYN_UNMAP(Y_S_PTR,STATUS)
	  CALL DYN_UNMAP(R_S_PTR,STATUS)
	  CALL DYN_UNMAP(PSF_FUNC,STATUS)
	  CALL DYN_UNMAP(PSF_STORE,STATUS)
	  CALL DYN_UNMAP(PSF_SHIFT,STATUS)
	  CALL DYN_UNMAP(PSF_WORK,STATUS)
	  CALL DYN_UNMAP(SRCMAP_PTR,STATUS)

*    Inform user

	  NHISTORY=NHISTORY+1
	  HISTORY(NHISTORY)=' '

	  NEXPAND=5
	  EXPAND_TEXT(1)='Output dataset {OUT}'
	  CALL USI_TEXT(1,EXPAND_TEXT,NEXPAND,STATUS)

	  DO I=1,NEXPAND
	    NHISTORY=NHISTORY+1
	    HISTORY(NHISTORY) = EXPAND_TEXT(I)
	  END DO

	  DO I=1,NHISTORY
	    OUT_HISTORY(I)=HISTORY(I)
	  END DO

*    Write this into history structure
          IF ( CREBSUB ) THEN
	    CALL HIST_PTXT( OLOC, NHISTORY, OUT_HISTORY, STATUS )
          END IF
          IF ( CREBACK ) THEN
	    CALL HIST_PTXT( BLOC, NHISTORY, OUT_HISTORY, STATUS )
          END IF

*    Release datasets
          CALL PSF_RELEASE( PSFHAN, STATUS )
	  CALL BDA_RELEASE( ILOC, STATUS )
          CALL DAT_ANNUL(LOCMORE,STATUS)

          IF ( CREBSUB ) THEN

*           Set BACKGROUND SUBTRACT flag to true
             CALL PRO_SET( OLOC, 'BGND_SUBTRACTED', .TRUE., STATUS )

	     CALL BDA_RELEASE( OLOC, STATUS )

          END IF
          IF ( CREBACK ) THEN
	     CALL BDA_RELEASE( BLOC, STATUS )
          END IF

	END IF		!if status OK after call to BSUB_SUBTRACT_BACKGROUND

*    Tidy up

 99	CONTINUE

	IF(STATUS.NE.SAI__OK)THEN

	  OLDSTATUS=STATUS
          CALL ERR_FLUSH(STATUS)

	  IF(OLDSTATUS.EQ.-100.OR.OLDSTATUS.EQ.-200.OR.
     &OLDSTATUS.EQ.-300)THEN
	    STATUS=SAI__OK
	  ELSE
	    STATUS=OLDSTATUS
	  ENDIF

	ENDIF

*      Shutdown ASTERIX
	CALL AST_CLOSE
	CALL AST_ERR( STATUS )

	END
*
*+
*  Program subtract_BACKGROUND computes the background surface of a
*  ROSAT WFC survey image.
*
*  The routine first removes bright sources from the image and then
*  searches for any horizontal or vertical sharp edge features in the
*  image (This feature may be removed as such edges are not expected).
*  The image is then appropriately divided into boxes. The mean
*  in each box, is computed assuming Poisson statistics. Sources that
*  are significantly above this mean are removed and the mean recomputed
*  until all contaminating sources are removed. When the whole image
*  has been processed in this way, the means for each box are recomputed
*  a final time and uncertainty estimates on these means are derived.
*  These values are then passed to a Spline algorithm which computes the
*  final surface.
*
**************************************
*				     *
*  Authors J. Mittaz and S. Rosen    *
*				     *
*	       MSSL		     *
*				     *
*            27.2.90		     *
*				     *
* modified during early 1992 for S3  *
* Now uses smoothing rather than     *
* splines                            *
*				     *
**************************************
*
	INTEGER   FUNCTION BSUB_SUBTRACT_BACKGROUND(IMAGE_X,
     &  IMAGE_Y,COMP_X,COMP_Y,SIZE,AXIS1,AXIS2,N_AZIM,N_ELEV,
     &  PSF_FUNC,N1,N2,N3,WORK,N_AZIMP2,N_ELEVP2,PSF_STORE,PSF_SHIFT,
     &  CREBSUB,CREBACK,
     &  IMAGE,BMODEL,VARRAY,SDIM,X_SOURCE,Y_SOURCE,R_SOURCE,SRCMAP,
     &  N_REMOVED,OLOC,QUALITY,STATUS)

	INCLUDE 'SAE_PAR'
	INCLUDE 'QUAL_PAR'
	INCLUDE 'DAT_PAR'
*
	INTEGER   IMAGE_X, IMAGE_Y	!dimensions of the full image

	INTEGER   COMP_X(IMAGE_X)	!compression arrays

	INTEGER   COMP_Y(IMAGE_Y)	!       "

	REAL   SIZE			!pixel size in arc secs

	REAL   AXIS1(3)			!image axis numbers

	REAL   AXIS2(3)			!

	INTEGER   N_AZIM,N_ELEV		!no of X and Y PSF pixels

	REAL   PSF_FUNC(N_AZIM,N_ELEV)	!PSF function grid

	INTEGER   N1, N2, N3		!dimensions of work array

	REAL   WORK(N1,N2,N3)		!work array for PSF_RESAMPLE

	INTEGER   N_AZIMP2,N_ELEVP2	!as above but for sampling grid

	REAL   PSF_STORE(N_AZIMP2,N_ELEVP2)	!stored grid

	REAL   PSF_SHIFT(N_AZIMP2,N_ELEVP2)	!PSF shift grid

        LOGICAL CREBSUB                 ! Create bgnd subtracted o/p
        LOGICAL CREBACK                 ! Create bgnd model o/p

	REAL   IMAGE(IMAGE_X,IMAGE_Y)	!image array. Input on entry and bsub'd
                                        ! on exit if CREBSUB is true. If false
                                        ! then same address as BMODEL
	REAL   BMODEL(IMAGE_X,IMAGE_Y)	!image array. Input on entry and bsub'd
                                        ! on exit if CREBSUB is true

	REAL   VARRAY(IMAGE_X,IMAGE_Y)	!variance array

	INTEGER   SDIM			!dim of source param arrays

	INTEGER   X_SOURCE(SDIM)	!X source coord

	INTEGER   Y_SOURCE(SDIM)	!Y source coords

	INTEGER   R_SOURCE(SDIM)	!source radius

	REAL   SRCMAP(IMAGE_X,IMAGE_Y)	!image array

	INTEGER   N_REMOVED		!No. of sources removed

	CHARACTER*(DAT__SZLOC) OLOC	!output locator

	BYTE QUALITY(IMAGE_X,IMAGE_Y)	!quality array

	INTEGER   STATUS          	!status of subroutine execution
*
	INTEGER   BOXSIZE, MIN_BOXSIZE	!box size / minimum allowed size

	INTEGER   I, J			!loop variables

	INTEGER   IFAIL			!NAG routine fail check

	INTEGER   IMAX, IMAXP10		!max count in image, imax+10

	INTEGER   IMIN

	INTEGER   XS, YS		!LH lower corner of image box region

	INTEGER   XE, YE		!RH upper corner of image box region

	INTEGER   NSOURCES		!number of detected sources

	REAL   IMAGE_MEAN 		!mean count level of image

	INTEGER   IR_80			!radius of PSF containg 95% of source

	INTEGER   MIN_XPOS, MAX_XPOS	!X limits of data present

	INTEGER   MIN_YPOS, MAX_YPOS	!Y          "

	INTEGER   XPOS, YPOS, SIG	!for DIAG_EDGE

	INTEGER   NSQ			!Dimension of compression array

	INTEGER   HIST_PTR		!pointer to histogram array

	INTEGER   NHIST			!dimension of histogram array

	INTEGER   NBOXES		!maps for GET_MEANS

	INTEGER   WEIGHT_PTR		!     "

	INTEGER   MINBOXSIZE		!minimum box size

	CHARACTER*132	TEXT		!text for HISTORY

	LOGICAL POINTED			!pointed phase image?

	REAL   OCHISQ

	INTEGER   INT_DIMS(2), INT_PTR  !dims + pointer for intermediate image

	INTEGER   SMOOTHED_PTR		!pointer to smoothed image

	INTEGER   GET_SMOOTHED_IMAGE	!function name

	INTEGER   MAXCT_PTR		!pointer to max ct array

	INTEGER   P_PTR, PQ_PTR		!pointers to P and PQ probability arrays

	INTEGER   M_PTR			!pointer to image for NIPS

	INTEGER   NDET, K		!other local counters

	REAL   XPOS_S(100), YPOS_S(100)		!source parameterisation

	REAL   COUNTS_S(100), PSF_COEFS(4)	!arrays

	REAL   LOG_FACTORIALS(0:100)		!log(N!) array

	REAL   OFF_X, OFF_Y, CEN_X, CEN_Y	!PSF grid info

	INTEGER   IRADIUS, IR_4SIG		!source radius limits

	INTEGER   MAX_RADIUS			!max radius for any source

	INTEGER   RDIMS(2), RBIN_PTR		!setup rbin

	INTEGER   NP_PTR			!ptr to NP array

	INTEGER   IMETH				!temporary option

	LOGICAL FOUND_MINX, FOUND_MINY		!flags for non-zero image limits
        LOGICAL  EDGES                   ! Find sloping edges?
	LOGICAL ALL_ZEROS			!All pixels zero

	INTEGER   BSUB_ERROR_HANDLER

	INTEGER MAX_DIMS(2)

	integer edges_ptr

	COMMON/FACTORIALS/LOG_FACTORIALS

*
*    Global variables :
*
      INCLUDE 'BSUB_CMN'
*
	BSUB_SUBTRACT_BACKGROUND=.TRUE.
*-

*
	ROUTINE_ID=1
*
	NSQ=MAX(IMAGE_X,IMAGE_Y)	!max dimension of image
*
*  find out if its a SURVEY image etc.
*
	CALL BSUB_GET_HEADER_INFO(OLOC,IMAGE_X,IMAGE_Y,IMAGE,VARRAY,
     &  SIZE,POINTED,MIN_XPOS,MAX_XPOS,MIN_YPOS,MAX_YPOS,STATUS)
*
*  The minimum box size is subsequently defined to be N*IR_80 where
*  (currently) N=2
*
	IR_80=NINT(R_80)

	MIN_BOXSIZE=1.5*IR_80		!get minimum box size
*
*  the detection threshold significance for other, weaker sources, can
*  now be entered as a user input
*
	CALL MSG_PRNT(' ')
	CALL USI_GET0R('SOURCE_THRESH',DETECTION_THRESH,STATUS)
	IF(STATUS.NE.SAI__OK)THEN
	  RETURN
	ENDIF

	NHISTORY=NHISTORY+1
	WRITE(TEXT,'(''Threshold for source detection is '',
     &1PG8.1)')DETECTION_THRESH
	HISTORY(NHISTORY)=TEXT
*
*  The image will be divided into boxes. The user specifies an initial trial
*  box size
*
	BOXSIZE=0

	CALL MSG_PRNT(' ')
	CALL USI_GET0I('BOX_DIM',BOXSIZE,STATUS)
	IF(STATUS.NE.SAI__OK)THEN
	  RETURN
	ENDIF

	NHISTORY=NHISTORY+1
	WRITE(TEXT,'(''Boxsize used is '',I5)')BOXSIZE
	HISTORY(NHISTORY)=TEXT
*
*  get maximum size of histogram array needed and compress image in each
*  direction to determine limits of non zero image. Also determine max of
*  image and zero source map array
*
	CALL BSUB_IMAGE_LIMITS(IMAGE_X,IMAGE_Y,IMAGE,COMP_X,COMP_Y,
     &  SRCMAP,IMAX,IMIN,MIN_XPOS,MAX_XPOS,MIN_YPOS,MAX_YPOS,STATUS)

	ALL_ZEROS=.FALSE.
	IF(IMIN.EQ.0.AND.IMAX.EQ.0)ALL_ZEROS=.TRUE.
*
*  determine if there are any edges in the data
*
	MAX_DIMS(1)=IMAGE_X
	IF(IMAGE_Y.GT.IMAGE_X)MAX_DIMS(1)=IMAGE_Y
	MAX_DIMS(2)=2

	CALL DYN_MAPI(2,MAX_DIMS,EDGES_PTR,STATUS)

*
*  Do edge finding?
*
        CALL USI_GET0L( 'SLOPING_EDGES', EDGES, STATUS )
        IF ( STATUS .NE. SAI__OK ) RETURN

        IF ( EDGES .AND. .NOT. ALL_ZEROS ) THEN
          CALL FIND_SLOPING_EDGES(IMAGE_X,IMAGE_Y,IMAGE,
     &      MIN_XPOS,MAX_XPOS,MIN_YPOS,MAX_YPOS,QUALITY,MAX_DIMS(1),
     &      MAX_DIMS(2),%VAL(EDGES_PTR),STATUS)
        ELSE IF(.NOT.ALL_ZEROS) THEN
          CALL ARR_INIT1B(QUAL__GOOD,IMAGE_X*IMAGE_Y,QUALITY,STATUS)
        END IF

*
*  store factorials into array for later use
*
	CALL BSUB_COMPUTE_FACTORIALS(IMAX,STATUS)

*  Number of boxes needed (over estimate)

	MINBOXSIZE=INT(R_80*2.)
	MINBOXSIZE=MIN(MINBOXSIZE,BOXSIZE)

	NBOXES=(FLOAT(IMAGE_X)/FLOAT(MINBOXSIZE))*
     &		(FLOAT(IMAGE_X)/FLOAT(MINBOXSIZE))*2

*  Map arrays

	IMAXP10=IMAX+10
	CALL DYN_MAPI(1,NBOXES,MAXCT_PTR,STATUS)
	CALL DYN_MAPI(1,IMAXP10,P_PTR,STATUS)
	CALL DYN_MAPI(1,IMAXP10,PQ_PTR,STATUS)

	CALL DYN_MAPR(1,NBOXES,WEIGHT_PTR,STATUS)

	INT_DIMS(1)=IMAGE_X
	INT_DIMS(2)=IMAGE_Y
*
*  if image is non zero, use CGP's NIPS code to search for brightish sources
*
	IF(.NOT.ALL_ZEROS)THEN

	  CALL DYN_MAPR(2, INT_DIMS, M_PTR, STATUS)

	  CALL NIPSUB(IMAGE_X,IMAGE_Y,IMAGE,%VAL(M_PTR),DETECTION_THRESH,
     &    NDET,XPOS_S,YPOS_S,COUNTS_S,STATUS)
          IF ( STATUS.NE.0) THEN
            STATUS=SAI__OK
            CALL MSG_PRNT( 'WARNING : NIPS error fitting source' )
          ENDIF
	  CALL DYN_UNMAP(M_PTR,STATUS)

	  CALL MSG_PRNT(' ')
	  CALL MSG_FMTI('NSOUR','i4',NDET)
	  CALL MSG_PRNT(' Detected ^NSOUR sources ')

	  DO K=1,NDET

            CALL MSG_FMTR('XP','F8.2',XPOS_S(K))
            CALL MSG_FMTR('YP','F8.2',YPOS_S(K))
            CALL MSG_FMTR('TCTS','F10.1',COUNTS_S(K))
            CALL MSG_PRNT(' source at ^XP ^YP : Approx cts = ^TCTS ')

	  END DO
*
*  now, for each source, read in the PSF grid centred on the source and
*  then fit the PSF template plus a background function to each source.
*  The store the scaled PSF into a source map. Note that points within
*  the central 68% radius are set to -100000.0 so that those pixels are
*  marked for exclusion from the smoothing process later.
*
	  N_REMOVED=NDET
	  IMETH=2

	  MAX_RADIUS=0

	  DO K=1,NDET

	    XPOS_S(K)=XPOS_S(K)-0.5	!source coords. Note -0.5 for
	    YPOS_S(K)=YPOS_S(K)-0.5	!consistency with NIPS definition
	    CEN_X=0.
	    CEN_Y=0.

	    OFF_X=( INT(XPOS_S(K))+0.5 - XPOS_S(K) )   !get source centroid off-
	    OFF_Y=( INT(YPOS_S(K))+0.5 - YPOS_S(K) )   !set from pixel centre

	    IF(IMETH.EQ.1)THEN	!direct extraction

	      OFF_X=-OFF_X*SIZE			!direction consistency
	      OFF_Y=-OFF_Y*SIZE			!with DJA's definition

	      CALL BSUB_GET_PSF_GRID(SIZE,CEN_X,CEN_Y,OFF_X,OFF_Y,
     &        N_AZIM,N_ELEV,PSF_FUNC,STATUS)	!extract PSF

	    ELSE		!resample
*
* resample the PSF to the centroid of the source. NOTE that this
* routine uses an offset in the OPPOSITE sense to PSF_2D_DATA AND
* the offset is in pixels, NOT radians (see DJA's note)
*
	      CEN_X=0.
	      CEN_Y=0.
	      OFF_X=0.
	      OFF_Y=0.

	      CALL BSUB_GET_PSF_GRID(SIZE,CEN_X,CEN_Y,OFF_X,OFF_Y,
     &        N_AZIMP2,N_ELEVP2,PSF_STORE,STATUS)

	      IF(STATUS.NE.SAI__OK)THEN
	        CALL ERR_REP(' ','Error in BSUB_GET_PSF_GRID - returning
     & to top level',STATUS)
	        RETURN
	      ENDIF

	      CALL PSF_RESAMPLE(N_AZIMP2,N_ELEVP2,PSF_STORE,OFF_X,OFF_Y,
     &        WORK,PSF_SHIFT,STATUS)
*
*  transfer shifted PSF grid with border to PSF_FUNC (without border)
*
	      PSF_MAX=0.
	      DO I=2,N_AZIMP2-1
		DO J=2,N_ELEVP2-1

		  PSF_FUNC(I-1,J-1)=PSF_SHIFT(I,J)
		  PSF_MAX=MAX(PSF_MAX,PSF_FUNC(I-1,J-1) )	!get peak value

		END DO
	      END DO
*
*  and normalise (don't generally need to do this as the fit will return
*  the appropriate scale factor)
*
	      DO I=1,N_AZIM
		DO J=1,N_ELEV
		  PSF_FUNC(I,J)=PSF_FUNC(I,J)/PSF_MAX
		END DO
	      END DO

	    END IF

	    IF(STATUS.NE.SAI__OK)THEN
	      CALL ERR_REP(' ','Error in BSUB_GET_PSF_GRID - returning to top
     & level',STATUS)
	      RETURN
	    ENDIF
*
*  now fit this template plus a sloping background
*  (ie c(x,y)=a+b*x+c*y+d*t(x,y) where t(x,y) is the PSF template
*
	    RDIMS(1)=2
	    RDIMS(2)=SQRT( (N_AZIM/2.)**2 + (N_ELEV/2.)**2 ) + 1

	    CALL DYN_MAPR(2, RDIMS, RBIN_PTR, STATUS)
	    CALL DYN_MAPI(1, RDIMS(2), NP_PTR, STATUS)
*
	    CALL BSUB_FIT_PSF_TEMPLATE(IMAGE_X,IMAGE_Y,IMAGE,
     &      MIN_XPOS,MAX_XPOS,MIN_YPOS,MAX_YPOS,XPOS_S(K),YPOS_S(K),
     &      N_AZIM,N_ELEV,PSF_FUNC,PSF_COEFS,IRADIUS,IR_4SIG,RDIMS(1),
     &      RDIMS(2),%VAL(RBIN_PTR),%VAL(NP_PTR),STATUS)

	    CALL DYN_UNMAP(RBIN_PTR,STATUS)
	    CALL DYN_UNMAP(NP_PTR,STATUS)

*
*  and store PSF into source map
*
	    CALL BSUB_UPDATE_SOURCE_MAP(IMAGE_X,IMAGE_Y,SRCMAP,
     &      N_AZIM,N_ELEV,PSF_FUNC,MIN_XPOS,MAX_XPOS,MIN_YPOS,MAX_YPOS,
     &      XPOS_S(K),YPOS_S(K),PSF_COEFS,IRADIUS,IR_4SIG,STATUS)

	    MAX_RADIUS=MAX(IRADIUS,MAX_RADIUS)

	  END DO

	END IF			!if image is non zero
*
*  and smooth resulting image
*
	CALL DYN_MAPR(2,INT_DIMS,INT_PTR,STATUS)
	CALL DYN_MAPR(2,INT_DIMS,SMOOTHED_PTR,STATUS)

	CALL BSUB_GET_SMOOTHED_IMAGE(BOXSIZE,IMAGE_X,IMAGE_Y,
     &  CREBSUB,CREBACK,IMAGE,BMODEL,SRCMAP,IMAX,IMIN,MIN_XPOS,MAX_XPOS,
     &  MIN_YPOS,MAX_YPOS,IMAGE_MEAN,VARRAY,SDIM,X_SOURCE,Y_SOURCE,
     &  R_SOURCE,N_REMOVED,MAX_RADIUS,N_AZIM,N_ELEV,PSF_FUNC,NBOXES,
     &  %VAL(WEIGHT_PTR),%VAL(INT_PTR),%VAL(SMOOTHED_PTR),
     &  %VAL(MAXCT_PTR),IMAXP10,%VAL(P_PTR),%VAL(PQ_PTR),QUALITY,STATUS)

	IF(STATUS.NE.SAI__OK.AND.STATUS.NE.-200)THEN
	    CALL ERR_REP(' ','Error in GET_MEANS - returning to top level',
     &STATUS)
	    RETURN
	ENDIF

	IF(STATUS.EQ.-200)THEN
	  STATUS=SAI__OK
	ENDIF

	CALL DYN_UNMAP(HIST_PTR,STATUS)
	CALL DYN_UNMAP(P_PTR,STATUS)
	CALL DYN_UNMAP(PQ_PTR,STATUS)
	CALL DYN_UNMAP(INT_PTR,STATUS)
	CALL DYN_UNMAP(SMOOTHED_PTR,STATUS)
	CALL DYN_UNMAP(MAXCT_PTR,STATUS)
	CALL DYN_UNMAP(WEIGHT_PTR,STATUS)

*
	END

*+
********************************
*
*  subroutine to get the smoothed image
*
	SUBROUTINE BSUB_GET_SMOOTHED_IMAGE(BOXSIZE,NX,NY,
     &  CREBSUB,CREBACK,IMAGE,BMODEL,SRCMAP,IMAX,IMIN,MIN_XPOS,MAX_XPOS,
     &  MIN_YPOS,MAX_YPOS,IMAGE_MEAN,VARRAY,SDIM,X_SOURCE,Y_SOURCE,
     &  R_SOURCE,N_REMOVED,MAX_RADIUS,N_AZIM,N_ELEV,PSF_FUNC,NBOX_SIZE,
     &  WEIGHTS,INT_IMAGE,SMOOTHED_IMAGE,MAXCT,IMAXP10,SUM_P,SUM_PQ,
     &  QUALITY,STATUS)
*
	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'
	INCLUDE 'QUAL_PAR'
*
	INTEGER   BOXSIZE		!box size

	INTEGER   NX, NY		!image dimensions

        LOGICAL CREBSUB                 ! Create bgnd subtracted o/p
        LOGICAL CREBACK                 ! Create bgnd model o/p

	REAL   IMAGE(NX,NY)		!image array. Input on entry and bsub'd
                                        ! on exit if CREBSUB is true. If false
                                        ! then same address as BMODEL
	REAL   BMODEL(NX,NY)		!image array. Input on entry and bsub'd
                                        ! on exit if CREBSUB is true

	REAL   SRCMAP(NX,NY)		!image array

	INTEGER   IMAX, IMAXP10		!brightest pixel in image, imax+10

	INTEGER   IMIN			!min level

	INTEGER   MIN_XPOS, MAX_XPOS	!X image limits

	INTEGER   MIN_YPOS, MAX_YPOS	!Y image limits

	REAL   INT_IMAGE(NX,NY)		!intermediate image

	REAL   SMOOTHED_IMAGE(NX,NY)	!smoothed image

	INTEGER   NBOX_SIZE		!dimension of box ID array

	REAL   WEIGHTS(NBOX_SIZE)	!weight (not used)

	INTEGER   NPIXELS		!no of pixels

	INTEGER   DIM, DIMX, DIMY	!image dimesions

	INTEGER   IFAIL			!fail test

	INTEGER   CTS			!counts

	INTEGER   IBIN			!bin counter

	INTEGER   NPIX_USED		!no of pixels used

	INTEGER   ICPU1, ICPU2, ICPU3	!cpu counters

	INTEGER   K, KL			!counters

	INTEGER   SM_SIZE		!smoothing box size

	INTEGER   HSIZE			!half smoothing box size

	INTEGER   XSTART, XEND		!start and end values in X

	INTEGER   YSTART, YEND		!              "         Y

	INTEGER   IS, JS		!counters

	INTEGER   LISTA(3)		!parameter list

	INTEGER   NCA, MA 		!array dim and no of parameters

	INTEGER   MFIT, NDATA		!no of fitted pars and no of data pts

	INTEGER   BOX_SIZE		!binning box size

	INTEGER   BOX_LIMS(4,1000)	!X,Y box limits

	INTEGER   NXBOX, NYBOX		!no of boxes in X and Y directions

	INTEGER   NX_LEFT, NY_LEFT	!no of unboxed pixels left over

	INTEGER   NHIST_LAST		!no of pts in last histogram

	INTEGER   IWORST		!box id for worst fit

	INTEGER   IROW, ICOL		!row and column counters

	INTEGER   IOS, IDD		!error id and device ID

	INTEGER   NCOEFS		!no of coefs in fit of PSF

	INTEGER   MAXCT(NBOX_SIZE)	!max ct in each box

	INTEGER   I, J, II, JJ		!counters

	INTEGER   M, L			!more counters

	REAL   X(1001), Y(1001), E(1001)	!data arrays for fitting

	REAL   COVAR(2,2), ALPHA(2,2)		!internal arrays

	REAL   ALAMDA, CHISQ			!lambda and chisqr values

	REAL   A(2)				!parameter value array

	REAL   BOX_MEANS(1000)			!array of mean in each box

	REAL   SMOOTH_MEAN, IMAGE_MEAN		!means for smooth and input image

	REAL   DIFF				!difference value

	REAL   MEAN_VALUE			!mean value

	REAL   CT_SUM				!sum of cts

	REAL   MAX_CTS, MIN_CTS, MAX_DIFF, MIN_DIFF	!limits of differences

	REAL   MEAN_BOX_DATA			!mean of data in box

	REAL   MEAN_BOX_SMOOTH			!mean of smooth values in box

	REAL   RMS_BOX				!RMS in box

	REAL   RMS_IMAGE			!RMS of whole image

	REAL   MEAN_BOX_DIFF			!mean diff of data-smooth

	REAL   WORST_BOX_DIFF 			!worst case diff of data-smooth

	REAL   MEAN_IMAGE_DIFF			!mean diff (data-smooth) over image

	REAL   LOG_FACTORIALS(0:100)		!array of log(N!)

	LOGICAL OUTPUT				!output info (for testing)

	INTEGER   N_AZIM, N_ELEV		!dimensions of PSF grid

	REAL   PSF_FUNC(N_AZIM,N_ELEV)		!PSF grid

	REAL   VARRAY(NX,NY)			!image array

	INTEGER   NHIST				!dimension of histogram array

	INTEGER   HISTOGRAM(0:100)		!histogram array

	INTEGER   STATUS			!status return

	INTEGER   SDIM				!Dimension of source arrays

	INTEGER   X_SOURCE(SDIM), Y_SOURCE(SDIM)	!source posn arrays

	INTEGER   R_SOURCE(SDIM)		!source radius array

	INTEGER   N_REMOVED			!No of removed sources

	INTEGER   MAX_RADIUS			!max radius of any source

	INTEGER   NUMBER_BOXES      		!number of boxes

	INTEGER   NBOXES			!number of boxes - 2

	LOGICAL ALL_INT				!are all pixels integer?

	LOGICAL	ENOUGH_PIXELS			!enough pixels for fit?

	INTEGER   IERR, itf			!type of error and mean to use

	REAL   SUM_PQ(IMAXP10), SUM_P(IMAXP10)	!P and P*Q sums for each ct

	REAL   P_PROB, Q_PROB			!P and Q probabilities

	REAL   PRED_PIX, ERR_PIX		!pred no of pixels and error

	REAL   CHISQR_HIST, CHISQR_IMAGE	!chisqr for histogram and image

	REAL   DCHISQR				!chisqr difference

	REAL   CHI2				!local chisqr

	REAL   SUM_MIN, SUM_MOUT 		!means of real and fitted means

	REAL   PC_DIFF				!% difference

	INTEGER   NPIX_POSS			!no of possible pixels in box

	INTEGER   NPIX_IMAGE_USED 		!no of used pixels in image

	INTEGER   NBOX_USED			!no of boxes used

	REAL   AV_BOX_DIFF, RMS_BOX_DIFF	!average and RMS diffs over boxes

	INTEGER   N_EXTEND			!no of zero bins added to hist

	INTEGER   OLD_ROUTINE			!old routine ID store

	LOGICAL SM_BORDER			!use a border on smoothing box?

	LOGICAL ALL_ZEROS			!zero image

	CHARACTER FILT*1

	REAL   FWHM

	INTEGER   WORK1_PTR, WORK2_PTR		!work array pointers

	INTEGER   WORK3_PTR, DIMS(2)

	BYTE QUALITY(NX,NY)

	COMMON/FACTORIALS/LOG_FACTORIALS
	INCLUDE 'BSUB_CMN'
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=2

	DIM=MIN(NX,NY)
	DIMX=NX
	DIMY=NY

	BOX_SIZE=BOXSIZE
*
*  compute box limits
*
	CALL BSUB_GET_BOX_LIMITS(NX,NY,BOX_SIZE,MIN_XPOS,MAX_XPOS,
     &  MIN_YPOS,MAX_YPOS,NXBOX,NYBOX,
     &  BOX_LIMS,NBOXES,ROUTINE_ID,STATUS)
*
*---------------------
*  if image is non zero,
*  go through each box in the image and get the histogram. Then fit
*  this histogram with a Poisson function and write-back the fit value
*  into each pixel in the image
*
	ALL_ZEROS=.FALSE.
	IF(IMIN.EQ.0.AND.IMAX.EQ.0)ALL_ZEROS=.TRUE.

	IF(.NOT.ALL_ZEROS)THEN

	  NHIST_LAST=0
	  MAX_CTS=0.
	  MIN_CTS=1.E8
	  SUM_MIN=0.
	  SUM_MOUT=0.

	  IERR=2
*
*  Gaussian or Top Hat smoothing function
*
	  FILT=' '
	  DO WHILE(FILT.NE.'H'.AND.FILT.NE.'G')
	    CALL MSG_PRNT(' ')
	    CALL USI_GET0C('SM_FILT',FILT,STATUS)
	    IF(STATUS.NE.SAI__OK)THEN
	      RETURN
	    ENDIF
	    CALL CHR_UCASE(FILT)
	  END DO
*
*  if using a top hat function, input smoothing box size (must be less
*  than half the image size AND be of an odd dimension). If the smoothing
*  box size is less than the maximum excluded region for any source, make
*  the smoothing box big enough to cover across such an excluded region

	  IF(FILT.EQ.'H')THEN

	    CALL MSG_PRNT(' ')
	    CALL USI_GET0I('SMOOTH_SIZE',SM_SIZE,STATUS)
	    IF(STATUS.NE.SAI__OK)THEN
	      RETURN
	    ENDIF

	    IF( SM_SIZE .LT. 2*MAX_RADIUS+5 )THEN
	      SM_SIZE=2*MAX_RADIUS+5
	      CALL MSG_FMTI('ADJ','I4',SM_SIZE)
	      CALL MSG_PRNT(' Adjusting smoothing box size to ^ADJ ')
	    END IF

	    IF(MOD(SM_SIZE,2).NE.1)SM_SIZE=SM_SIZE+1	!make odd size
	    HSIZE=INT(SM_SIZE/2.)

	    CALL USI_GET0L('SMOOTH_BORDER',SM_BORDER,STATUS)
	    IF(STATUS.NE.SAI__OK)THEN
	      RETURN
	    ENDIF

	  ELSEIF(FILT.EQ.'G')THEN

	    CALL USI_GET0R('SM_FWHM',FWHM,STATUS)
	    IF(STATUS.NE.SAI__OK)THEN
	      RETURN
	    ENDIF

	    IF( 3.*FWHM/2.354 .LT. MAX_RADIUS+3 )THEN
	      FWHM=(MAX_RADIUS+3)*2.354/3.
	      CALL MSG_FMTI('ADJ','I4',NINT(FWHM))
	      CALL MSG_PRNT(' Adjusting smoothing FWHM to ^ADJ ')
	    END IF

	  END IF

	  N_EXTEND=3

	  DO I=1,NBOXES
*
*  bins histogram
*
	    CALL GET_HISTOGRAM(DIMX,DIMY,IMAGE,SRCMAP,I,BOX_LIMS,
     +      HISTOGRAM,NHIST,NHIST_LAST,CT_SUM,NPIX_USED,
     +      MEAN_VALUE,ALL_INT,ROUTINE_ID,QUALITY,STATUS)

	    MAXCT(I)=0
	    IF(NHIST.NE.100)MAXCT(I)=NHIST

	    NHIST_LAST=NHIST	!store max size of last histogram
*
*  if no. of pixels used is too small for reliable estimate of mean, then
*  set all SRCMAP pixels in that box to -100000 (set at < 1/3 of pixels in
*  box)
*
	    NPIX_POSS=(BOX_LIMS(2,I)-BOX_LIMS(1,I)+1)*
     +      (BOX_LIMS(4,I)-BOX_LIMS(3,I)+1)

	    ENOUGH_PIXELS=.TRUE.
	    IF(NPIX_USED.LT.INT(0.333*NPIX_POSS))THEN
	      CALL BSUB_UNRELIABLE(DIMX,DIMY,SRCMAP,I,BOX_LIMS,STATUS)
	      ENOUGH_PIXELS=.FALSE.
	    END IF
*
*  if enough pixels, then compute mean by Poisson fitting or simple mean.
*
	    IF(ENOUGH_PIXELS)THEN

	      IF(ALL_INT)THEN
*
*  Poisson fitting: store this data into fitting arrays (this can be
*  speeded up by direct storage later) and get the binomial errors on the bins
*
		DO M=0,NHIST+N_EXTEND

		  X(M+1)=1.D0*M
		  Y(M+1)=1.D0*HISTOGRAM(M)
d		  E(M+1)=1.D0

		  CALL BINOMIAL_ERROR(NPIX_USED,MEAN_VALUE,M,E(M+1),P_PROB,
     +		  Q_PROB,STATUS)

		END DO
*
*  call Poisson fitting routines to fit the histogram
*
		NDATA=NHIST+N_EXTEND		!2
		A(1)=NPIX_USED
		A(2)=MEAN_VALUE

		NCA=2
		MA=2

		MFIT=1		! i.e, A(1) is fixed !
		LISTA(1)=2
		KL=0

		ALAMDA=-0.001

		IF(NDATA-N_EXTEND.EQ.0)THEN	!if all pts are 0 - mean=0

		  A(1)=NPIX_USED
		  A(2)=0.
		  STATUS=0
		  MFIT=0

		ELSEIF(NDATA-N_EXTEND.GE.1)THEN		!else fit

		  CALL BSUB_FIT_HISTOGRAM(X,Y,E,NDATA,A,MA,MFIT,LISTA,
     &  CHISQ,KL,STATUS)

		END IF
*
	      ELSE		!if not all integer pixels (e.g. source subtracted)

		A(1)=NPIX_USED
		A(2)=MEAN_VALUE

	      END IF

	    ELSE			!if not enough pixels, set to zero.

	      A(2)=0.0		!The weighting is also set to zero later

	    END IF
*
*  now write back the mean value to the image array (or rather, the
*  intermediate array), storing the same mean value to all pixels in the box
*  This routine also gets the weighting factor in each box
*
	    CALL WRITE_BACK(DIMX,DIMY,I,BOX_LIMS,A(2),INT_IMAGE,
     +      NPIX_USED,CT_SUM,VARRAY,ENOUGH_PIXELS,ROUTINE_ID,STATUS)

	    MAX_CTS=MAX(MAX_CTS,A(2))
	    MIN_CTS=MIN(MIN_CTS,A(2))

	  END DO	!over all boxes in image
*
*-------------
*
*  now do the fast or Gaussian smooth (for the intermediate image)
*
	  IF(FILT.EQ.'H')THEN

	    CALL BSUB_FAST_SMOOTH(DIMX,DIMY,INT_IMAGE,VARRAY,
     &      MIN_XPOS,MAX_XPOS,MIN_YPOS,MAX_YPOS,HSIZE,
     &      SMOOTHED_IMAGE,MIN_CTS,MAX_CTS,SM_BORDER,ROUTINE_ID,STATUS)

	  ELSEIF(FILT.EQ.'G')THEN

	    DIMS(1)=DIMX
	    DIMS(2)=DIMY

	    CALL DYN_MAPR(2,DIMS,WORK1_PTR,STATUS)
	    CALL DYN_MAPR(2,DIMS,WORK2_PTR,STATUS)
	    CALL DYN_MAPR(2,DIMS,WORK3_PTR,STATUS)

	    CALL BSUB_GAUSS_SMOOTH(DIMX,DIMY,INT_IMAGE,VARRAY,
     &      MIN_XPOS,MAX_XPOS,MIN_YPOS,MAX_YPOS,FWHM,
     &      SMOOTHED_IMAGE,MIN_CTS,MAX_CTS,SM_BORDER,
     &      %VAL(WORK1_PTR),%VAL(WORK2_PTR),%VAL(WORK3_PTR),
     &      ROUTINE_ID,STATUS)

	    CALL DYN_UNMAP(WORK1_PTR,STATUS)
	    CALL DYN_UNMAP(WORK2_PTR,STATUS)
	    CALL DYN_UNMAP(WORK3_PTR,STATUS)

	  END IF

	ELSE					!if zero image

	  DO I=1,DIMX
	    DO J=1,DIMY
	      VARRAY(I,J)=0.
	      IF ( CREBSUB ) IMAGE(J,I)=0.
	      IF ( CREBACK ) BMODEL(J,I)=0.
	    END DO
	  END DO

	END IF
*
*------------------------
*  Again, if non zero image,
*  Program now goes through the boxes and computes various statistics to
*  measure the quality of the smoothed image to the data. Several values
*  are computed. Firstly, for each box, the program gets the cts histogram
*  and then generates the expected histogram given the smoothed surface.
*  This expected histogram uses the probability of getting J cts for the
*  mean in each pixel and sums. The error is the sqrt(sum of all
*  p*q products). The chisqr for the histograms is then evaluated to test for
*  the accuracy with which the predicted histogram matches the real data.
*  the program also gets the total cts observed and predicted and compares
*  these. At the same time, all these values are stored for a global image
*  value. The RMS values are also computed and the deviation of the box means
*  from the predictions are obtained.
*
	IF(.NOT.ALL_ZEROS)THEN
*
*  initialise variables
*

	  MIN_CTS=1.E8		!limits of output background subtracted image
	  MAX_CTS=-1.E8		!                 "
	  MIN_DIFF=1.E8
	  MAX_DIFF=-1.E8
*
	  WORST_BOX_DIFF=0.0    !worst diff between cts and smooth surface in box
*
	  RMS_IMAGE=0.		!RMS over whole image
	  IMAGE_MEAN=0.		!mean of total image cts
	  SMOOTH_MEAN=0.	  	!mean of smooth surface over image
	  NPIX_IMAGE_USED=0	!no of pixels used for whole image
	  CHISQR_IMAGE=0.		!chisqr of whole image histogram
	  MEAN_IMAGE_DIFF=0.
	  AV_BOX_DIFF=0.
	  RMS_BOX_DIFF=0.
	  NBOX_USED=0

	  NHIST_LAST=100		!for first box, initialise all elements

	  DO K=1,NBOXES
*
*  zero the elements that were used for the last box. Since all elements are
*  zero'd first time around, none should be filled if we re-zero those used
*  for the last box
*
	      DO M=0,NHIST_LAST
		HISTOGRAM(M)=0
	      END DO
*
*  initialise box counters and zero probability accumulator arrays
*
	      MEAN_BOX_DATA=0.
	      MEAN_BOX_SMOOTH=0.
	      RMS_BOX=0.
	      NPIX_USED=0

	      DO M=0,MAXCT(K)		!zero probability accumulator arrays
		SUM_P(M+1)=0.
		SUM_PQ(M+1)=0.
	      END DO
*
*  loop around all pixels in the box
*
	      DO I=BOX_LIMS(3,K),BOX_LIMS(4,K)		!y dirn
		DO J=BOX_LIMS(1,K),BOX_LIMS(2,K)		!x dirn

		  DIFF=IMAGE(J,I)-SMOOTHED_IMAGE(J,I)	!bkgnd subtracted image

		  IF(SRCMAP(J,I).EQ.0.0.AND.
     :               QUALITY(J,I).EQ.QUAL__GOOD)THEN	!Only do for non-source pixels

		    CTS=IMAGE(J,I)

		    IF(IMAGE(J,I)-CTS.EQ.0.)THEN		!compile histogram if
		      HISTOGRAM(CTS)=HISTOGRAM(CTS)+1	!integer pixel
		    END IF
*
*  For boxes, sum up cts, smooth values, and pixels used in getting them.
*  Also, get difference for output image and RMS summation
*
		    MEAN_BOX_DATA=MEAN_BOX_DATA+IMAGE(J,I)
		    MEAN_BOX_SMOOTH=MEAN_BOX_SMOOTH+SMOOTHED_IMAGE(J,I)
		    RMS_BOX=RMS_BOX+DIFF*DIFF
		    NPIX_USED=NPIX_USED+1
*
		    MAX_CTS=MAX(MAX_CTS,IMAGE(J,I))
		    MIN_CTS=MIN(MIN_CTS,IMAGE(J,I))
*
*  Compute probability of getting m=0->maxct(k) cts in each pixel for a mean
*  equal to that of the smoothed surface at that pixel. Sum the P and PQ
*  products for histogram comparison (may not need this later - comparison
*  of total counts may be sufficient)
*
		    DO M=0,MAXCT(K)

			CALL BINOMIAL_ERROR(1,SMOOTHED_IMAGE(J,I),M,
     +		 	E(M+1),P_PROB,Q_PROB,STATUS)

			SUM_P(M+1)=SUM_P(M+1) + P_PROB
			SUM_PQ(M+1)=SUM_PQ(M+1) + P_PROB*Q_PROB

		    END DO

		  END IF		!if srcmap eq 0
*
*  now update output image
*
		  IF ( CREBSUB ) THEN
		     IMAGE(J,I)=DIFF
		     IF(QUALITY(J,I).NE.QUAL__GOOD)IMAGE(J,I)=0.
		  END IF

		  IF ( CREBACK )THEN
		    BMODEL(J,I)=SMOOTHED_IMAGE(J,I)
		    IF(QUALITY(J,I).NE.QUAL__GOOD)BMODEL(J,I)=0.
		  END IF

		  MIN_DIFF=MIN(MIN_DIFF,DIFF)
		  MAX_DIFF=MAX(MAX_DIFF,DIFF)

	        END DO
	      END DO

	      NHIST_LAST=MAX_CTS+1   !no of hist bins to re-zero for next box
*
*  Update whole image info. Get mean of image pixels, smooth surface and
*  the RMS. Also, update the average fractional difference between cts and
*  smooth over boxes and the RMS scatter of these fractional differences and
*  also the worst deviation
*
	      IMAGE_MEAN=IMAGE_MEAN+MEAN_BOX_DATA
	      SMOOTH_MEAN=SMOOTH_MEAN+MEAN_BOX_SMOOTH
	      RMS_IMAGE=RMS_IMAGE+RMS_BOX
	      NPIX_IMAGE_USED=NPIX_IMAGE_USED+NPIX_USED

	      IF(MEAN_BOX_DATA.GT.0.0.AND.NPIX_USED.GT.0)THEN

		DIFF=(MEAN_BOX_DATA-MEAN_BOX_SMOOTH)

		AV_BOX_DIFF=AV_BOX_DIFF + (DIFF/MEAN_BOX_DATA)
		RMS_BOX_DIFF=RMS_BOX_DIFF + (DIFF/MEAN_BOX_DATA)**2
		NBOX_USED=NBOX_USED+1

		IF(ABS(DIFF/MEAN_BOX_DATA).GT.ABS(WORST_BOX_DIFF))THEN
		  WORST_BOX_DIFF=DIFF/MEAN_BOX_DATA
		  IWORST=K
	        END IF

	        CHI2=0.
	        CHI2=(DIFF/SQRT(MEAN_BOX_DATA))**2
	        PC_DIFF=100.*(DIFF/MEAN_BOX_DATA)

	      END IF
*
*  Compute the total predicted no of pixels in each count bin and the
*  error on that number (according to the binomial distribution).
*
	      CHISQR_HIST=0.
	      DO M=0,MAXCT(K)

		IF(SUM_PQ(M+1).GT.0.0)THEN

		  ERR_PIX=SQRT(SUM_PQ(M+1))
		  PRED_PIX=SUM_P(M+1)
		  DIFF=HISTOGRAM(M)-PRED_PIX
		  DCHISQR=(DIFF/ERR_PIX)**2
		  CHISQR_HIST=CHISQR_HIST+DCHISQR

		END IF

	      END DO

	      CHISQR_IMAGE=CHISQR_IMAGE+CHISQR_HIST	!total chi2 for image

	      IF(NPIX_USED.GT.0)THEN
		MEAN_BOX_DATA=MEAN_BOX_DATA/NPIX_USED
		MEAN_BOX_SMOOTH=MEAN_BOX_SMOOTH/NPIX_USED
	      END IF

	  END DO
*
	  IMAGE_MEAN=IMAGE_MEAN/NPIX_IMAGE_USED
	  SMOOTH_MEAN=SMOOTH_MEAN/NPIX_IMAGE_USED

	  RMS_IMAGE=SQRT(RMS_IMAGE/NPIX_IMAGE_USED)
	  MEAN_IMAGE_DIFF=(IMAGE_MEAN-SMOOTH_MEAN)

	  CALL MSG_PRNT(' ')
	  CALL MSG_FMTR('MIN','F8.2',MIN_DIFF)
	  CALL MSG_FMTR('MAX','F8.2',MAX_DIFF)
	  CALL MSG_PRNT(' For background subtracted image, minimum= ^MIN :
     + maximum= ^MAX cts ')

	  CALL MSG_FMTR('DM','F9.3',IMAGE_MEAN)
	  CALL MSG_FMTR('SM','F9.3',SMOOTH_MEAN)
	  CALL MSG_PRNT(' Mean of image= ^DM  Mean of smoothed image = ^SM ')

	  CALL MSG_FMTR('MDIFF','F8.4',MEAN_IMAGE_DIFF)
	  CALL MSG_FMTR('RMS','F8.4',RMS_IMAGE)
	  CALL MSG_PRNT(' Mean difference over image= ^MDIFF : RMS= ^RMS ')

	  WORST_BOX_DIFF=WORST_BOX_DIFF*100.
	  CALL MSG_FMTR('WD','F7.1',WORST_BOX_DIFF)
	  CALL MSG_FMTI('WB','I3',IWORST)
	  CALL MSG_PRNT(' Worst % deviation (data-smooth) in a box
     + is ^WD % in box ^WB ')

	  AV_BOX_DIFf=AV_BOX_DIFF*100./NBOX_USED
	  RMS_BOX_DIFF=100.*SQRT(RMS_BOX_DIFF/NBOX_USED)
	  CALL MSG_FMTR('AVDEV','F7.2',AV_BOX_DIFF)
	  CALL MSG_FMTR('RMS','F6.1',RMS_BOX_DIFF)
	  CALL MSG_PRNT(' Average of box (data-smooth) variations = ^AVDEV %
     + RMS = ^RMS % ')
*
	ELSE

	  CALL MSG_PRNT(' ')
	  CALL MSG_PRNT(' Input image was all zero. Output image is therefore
     + also zero. ')

	END IF

	ROUTINE_ID=OLD_ROUTINE

	END
*
*+
*
*  subroutine to evaluate factorials ans store in array
*
	SUBROUTINE BSUB_COMPUTE_FACTORIALS(IMAX,STATUS)
*
	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	REAL   LOG_FACTORIALS(0:100)		!factorial array

	REAL   RJ				!real factorial variable

	REAL   FACTORIAL, LN_FAC		!N! and log(N!) variables

	REAL   PI				!pi

	INTEGER   J				!integer factorial variable

	INTEGER   IMAX 				!max factorial to evaluate
*
	INTEGER   OLD_ROUTINE			!old routine ID store

	INTEGER   STATUS

	INCLUDE 'BSUB_CMN'

	COMMON/FACTORIALS/LOG_FACTORIALS
*
*-
*
*  will compute ln(J!) for J=0 -> 50. For higher values of J, ln(J!) will be
*  computed within the routines requiring it.
*  Here, for J<30, use simple J!=1*1*2*... *J. For J>=30, use Stirlings approx
*
	IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=3

	FACTORIAL=1.0			!(factorial of zero)

	DO J=0,50			!do for 1st 50 numbers 0->50

	  RJ=J

	  IF(J.LT.30)THEN		!for cases J<30

	    IF(J.GT.0)FACTORIAL=FACTORIAL*J	!if j>0, then get j!

	    IF(FACTORIAL.LE.0)THEN
	      CALL ERR_REP(' ',' Error in Poisson factorial ',STATUS)
	      STATUS=-100
	      RETURN
	    ELSE

	      LN_FAC=LOG(FACTORIAL)	!convert to nat. log

	    END IF

	  ELSEIF(J.GE.30)THEN	!if J > 30  => Stirlings approx

	    IF(RJ.LE.0)THEN
	      CALL ERR_REP(' ',' Error in Poisson: RJ < 0.0 ',STATUS)
	      STATUS=-100
	      RETURN
	    ELSE

	      PI=4.0*ATAN(1.0)
	      LN_FAC=(RJ+0.50)*LOG(RJ) - RJ + 0.50*LOG(2.0*PI)

	    END IF

	  END IF

	  LOG_FACTORIALS(j)=LN_FAC

	END DO
*
	ROUTINE_ID=OLD_ROUTINE

	RETURN
	END

*
*+
*
*  subroutine to determine the box limits
*
	SUBROUTINE BSUB_GET_BOX_LIMITS(DIMX,DIMY,BOX_SIZE,
     &  MIN_XPOS,MAX_XPOS,MIN_YPOS,MAX_YPOS,NXBOX,NYBOX,
     &  BOX_LIMS,NBOXES,ROUTINE_ID,STATUS)
*
	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	INTEGER   DIMX, DIMY		!image dimensions

	INTEGER   BOX_SIZE		!box size

	INTEGER   MIN_XPOS, MAX_XPOS

	INTEGER   MIN_YPOS, MAX_YPOS

	INTEGER   NXBOX, NYBOX		!no of boxes in x and y

	INTEGER   NX_LEFT, NY_LOW	!no of pixels left over

	INTEGER   BOX_LIMS(4,1000)	!box limits in X and Y

	INTEGER   I, IROW, ICOL		!local variables

	INTEGER   IX1, IX2, IY1, IY2	!   "

	INTEGER   NBOXES

	INTEGER   ROUTINE_ID, OLD_ROUTINE	!routine ID monitors

	INTEGER   STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=4
*
*  determine how many boxes are required to fill image
*
	NXBOX=(MAX_XPOS-MIN_XPOS+0.1)/BOX_SIZE
	NXBOX=NXBOX-1
	IF(NXBOX.LT.0)NXBOX=0
	NX_LEFT=(MAX_XPOS-MIN_XPOS) - NXBOX*BOX_SIZE
	IX1=MIN_XPOS + NX_LEFT/2.
	IX2=IX1+NXBOX*BOX_SIZE+1
*
	NYBOX=(MAX_YPOS-MIN_YPOS+0.1)/BOX_SIZE
	NYBOX=NYBOX-1
	IF(NYBOX.LT.0)NYBOX=0
	NY_LOW=(MAX_YPOS-MIN_YPOS) - NYBOX*BOX_SIZE
	IY1=MIN_YPOS + NY_LOW/2.
	IY2=IY1+NYBOX*BOX_SIZE+1
*
*  total no of boxes is central region plus two outside strips (one strip
*  is missing two boxes which are INCLUDED in the other two strips
*  note that because nxbox has 2 (smaller boxes) either side of it the total
*  no of boxes is NX*NY + (NX+2)*2 + (NY*2)
*
	NBOXES=NXBOX*NYBOX+2*(NXBOX+2)+2*NYBOX

	DO I=1,NBOXES

	  IROW=(I-0.1)/(NXBOX+2)+1
	  ICOL=I-(IROW-1)*(NXBOX+2)

* for x direction

	  IF(ICOL.EQ.1)THEN
	     BOX_LIMS(1,I)=MIN_XPOS
	     BOX_LIMS(2,I)=IX1
	  ELSEIF(ICOL.EQ.NXBOX+2)THEN
	     BOX_LIMS(1,I)=IX2
	     BOX_LIMS(2,I)=MAX_XPOS
	  ELSE
	     BOX_LIMS(1,I)=IX1+(ICOL-2)*BOX_SIZE+1
	     BOX_LIMS(2,I)=BOX_LIMS(1,I)+BOX_SIZE-1
	  END IF

* for y direction

	  IF(IROW.EQ.1)THEN
	     BOX_LIMS(3,I)=MIN_YPOS
	     BOX_LIMS(4,I)=IY1
	  ELSEIF(IROW.EQ.NYBOX+2)THEN
	     BOX_LIMS(3,I)=IY2
	     BOX_LIMS(4,I)=MAX_YPOS
	  ELSE
	     BOX_LIMS(3,I)=IY1+(IROW-2)*BOX_SIZE+1
	     BOX_LIMS(4,I)=BOX_LIMS(3,I)+BOX_SIZE-1
	  END IF

	END DO

	ROUTINE_ID=OLD_ROUTINE

	RETURN
	END
*
*+
*
*  subroutine to bin up histogram
*
	SUBROUTINE GET_HISTOGRAM(DIMX,DIMY,IMAGE,SRCMAP,IBOX,BOX_LIMS,
     +  HISTOGRAM,NHIST,NHL,CT_SUM,NPIX_USED,MEAN,ALL_INT,ROUTINE_ID,
     +  QUALITY,STATUS)

	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'
	INCLUDE 'QUAL_PAR'

	INTEGER   DIMX, DIMY		!image dimensions

	REAL   IMAGE(DIMX,DIMY)		!image

	REAL   MEAN, CT_SUM		!mean and counts sum

	REAL   SRCMAP(DIMX,DIMY)	!source map image

	INTEGER   IBOX, BOX_LIMS(4,1000)  !box variable and box limits array

	INTEGER   HISTOGRAM(0:100), NHIST	!histogram array and limit

	INTEGER   I, J, NHL, NMAX		!local variables

	INTEGER   CTS				!cts in pixel

	INTEGER   NPIX_USED			!no of pixels used

	INTEGER   ROUTINE_ID, OLD_ROUTINE

	LOGICAL ALL_INT				!all integer pixels monitor

	BYTE QUALITY(DIMX,DIMY)

	INTEGER   STATUS			!status
*
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  initialise parameters
	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=5

	ALL_INT=.TRUE.

	NMAX=NHL
	IF(IBOX.EQ.1)NMAX=100

	DO I=0,NMAX
	  HISTOGRAM(I)=0
	END DO
*
*  now go through pixels in this box (note the i loop is over x which is the
*  first element of the image matrix)
*
	NPIX_USED=0
	CT_SUM=0.
	NMAX=0

	DO I=BOX_LIMS(1,IBOX),BOX_LIMS(2,IBOX)

	  DO J=BOX_LIMS(3,IBOX),BOX_LIMS(4,IBOX)

	     CTS=IMAGE(I,J)		!note order of elements

	     IF(SRCMAP(I,J).EQ.0.0.AND.QUALITY(I,J).EQ.QUAL__GOOD)THEN	!if integers only (no source)

	        HISTOGRAM(CTS)=HISTOGRAM(CTS)+1

	        NMAX=MAX(NMAX,CTS)

	        CT_SUM=CT_SUM + CTS
	        NPIX_USED=NPIX_USED + 1

	     ELSEIF(SRCMAP(I,J).NE.-100000.0)THEN  !if non integer (i.e. source
*                                                  !wings subtracted)
	        ALL_INT=.FALSE.
		NMAX=100

	     END IF

	  END DO

	END DO

	IF(NPIX_USED.NE.0)MEAN=CT_SUM/NPIX_USED

	NHIST=NMAX

	ROUTINE_ID=OLD_ROUTINE

	RETURN
	END
*
*+
*
*  subroutine to write back fitted mean value to the appropriate pixels
*  in an intermediate image and to store a weighting factor, prop to
*  the number of pixels used (the more pixels, the higher the weighting)
*


	SUBROUTINE WRITE_BACK(DIMX,DIMY,IBOX,BOX_LIMS,MEAN,
     +  INT_IMAGE,NPIX_USED,CT_SUM,WEIGHT,ENOUGH_PIXELS,ROUTINE_ID,
     +  STATUS)
*
	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	INTEGER   DIMX, DIMY		!image dimensions

	REAL   INT_IMAGE(DIMX,DIMY)	!image

	REAL   WEIGHT(DIMX,DIMY)	!weighting image

	REAL   MEAN			!mean value in box

	REAL   CT_SUM			!sum of cts

	INTEGER   NPIX_USED		!no of pixels used

	INTEGER   IBOX, BOX_LIMS(4,1000)	!box ID and box limits arrays

	INTEGER   I, J			!local variables

	LOGICAL ENOUGH_PIXELS		!enough pixels control

	INTEGER   ROUTINE_ID, OLD_ROUTINE

	INTEGER   STATUS
*
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=6
*
*  not same element order as for get_histogram
*
	DO I=BOX_LIMS(1,IBOX),BOX_LIMS(2,IBOX)		!X loop

	  DO J=BOX_LIMS(3,IBOX),BOX_LIMS(4,IBOX)	!Y loop

	     INT_IMAGE(I,J)=MEAN			!correct way round

	     WEIGHT(I,J)=0.				!initialise

	     IF(ENOUGH_PIXELS.AND.NPIX_USED.GT.0.AND.CT_SUM.GT.0.0)THEN

		WEIGHT(I,J)=1./( (SQRT(CT_SUM)/NPIX_USED)**2 )	!1/SIGMA**2

	     END IF

	  END DO

	END DO

	ROUTINE_ID=OLD_ROUTINE

	RETURN
	END
*
*+
*
*  subroutine to perform a fast smooth of the data in a supplied image
*
	SUBROUTINE BSUB_FAST_SMOOTH(DIMX,DIMY,IMAGE,WEIGHT,
     &  MIN_XPOS,MAX_XPOS,MIN_YPOS,MAX_YPOS,HSIZE,SMOOTHED_IMAGE,
     &  MIN_CTS,MAX_CTS,SM_BORDER,ROUTINE_ID,STATUS)
*
	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	INTEGER   DIMX, DIMY			!image dimensions

	REAL   IMAGE(DIMX,DIMY)			!image

	REAL   WEIGHT(DIMX,DIMY)		!weighting array

	REAL   SMOOTHED_IMAGE(DIMX,DIMY)	!smoothed output image

	REAL   MIN_CTS, MAX_CTS			!min/max cts in smoothed image

	REAL   FRONT_STRIP(2), BACK_STRIP(2)	!add/subtract strips

	REAL   TOP_STRIP(2), BOTTOM_STRIP(2)	!add/subtract strips

	REAL   WT_SUM, WT_CT_SUM		!wt and ct*wt summation vars

	REAL   LAST_WT_CT_SUM, LAST_WT_SUM	!old values

	REAL   LAST_ROW_WT_SUM, LAST_ROW_WT_CT_SUM	!for last row

	REAL   BORDER(2), WFX, WFY		!border cts and weight factor

	INTEGER   MIN_XPOS, MAX_XPOS		!non-zero image limits in X

	INTEGER   MIN_YPOS, MAX_YPOS		!       "                 Y

	INTEGER   I, J, IS, JS			!loop variables

	INTEGER   XSTART, XEND			!X box limits on image

	INTEGER   YSTART, YEND			!Y box limits on image

	INTEGER   HSIZE				!half smoothing box size

	LOGICAL SM_BORDER			!use weighted border

	INTEGER   IEDGE				!border width

	INTEGER   ROUTINE_ID, OLD_ROUTINE

	INTEGER   STATUS
*
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=7

	MAX_CTS=0.
	MIN_CTS=1.E8
*
*  loop over all pixels
*
	IEDGE=0
	IF(SM_BORDER)IEDGE=2

	DO I=MIN_YPOS,MAX_YPOS

	  DO J=MIN_XPOS,MAX_XPOS		!in x dirn

	     XSTART=J-HSIZE+IEDGE	!define limits of smoothing box
	     XEND=J+HSIZE-IEDGE 	!as before

	     YSTART=I-HSIZE+IEDGE	!
	     YEND=I+HSIZE-IEDGE		!
*
*  if dealing with 1st pixel in 1st row, sum up all counts as for normal
*  smoothing
*
	     IF(J.EQ.MIN_XPOS.AND.I.EQ.MIN_YPOS)THEN

		WT_SUM=0.
		WT_CT_SUM=0.

		CALL BSUB_UPDATE(DIMX,DIMY,IMAGE,WEIGHT,
     +		YSTART,YEND,XSTART,XEND,MIN_XPOS,MAX_XPOS,MIN_YPOS,
     +          MAX_YPOS,WT_CT_SUM,WT_SUM,ROUTINE_ID,STATUS)

		LAST_ROW_WT_SUM=WT_SUM		!store for j.ne.1 rows
		LAST_ROW_WT_CT_SUM=WT_CT_SUM	!          "
*
*  if dealing with first pixel in any row, take the stored sum for the 1st
*  pixel in the row before and add on the new top edge and subtract off the
*  old bottom edge. This saves time of having to sum all cts in box at
*  start of new row
*
	     ELSEIF(J.EQ.MIN_XPOS)THEN

		TOP_STRIP(1)=0.
		TOP_STRIP(2)=0.
		BOTTOM_STRIP(1)=0.
		BOTTOM_STRIP(2)=0.
*
*  first, for row at bottom edge (1 below smoothing box limit ! )
*  losing row behind xstart

		CALL BSUB_UPDATE(DIMX,DIMY,IMAGE,WEIGHT,
     +		YSTART-1,YSTART-1,XSTART,XEND,MIN_XPOS,MAX_XPOS,
     +          MIN_YPOS,MAX_YPOS,BOTTOM_STRIP(1),
     +          BOTTOM_STRIP(2),ROUTINE_ID,STATUS)
*
*  and for top edge
*
		CALL BSUB_UPDATE(DIMX,DIMY,IMAGE,WEIGHT,
     +		YEND,YEND,XSTART,XEND,MIN_XPOS,MAX_XPOS,MIN_YPOS,
     +          MAX_YPOS,TOP_STRIP(1),TOP_STRIP(2),ROUTINE_ID,
     +          STATUS)

* update sums for start of new row.

		WT_CT_SUM=LAST_ROW_WT_CT_SUM+TOP_STRIP(1)-
     +		BOTTOM_STRIP(1)
		WT_SUM=LAST_ROW_WT_SUM+TOP_STRIP(2)-BOTTOM_STRIP(2)

		LAST_ROW_WT_CT_SUM=WT_CT_SUM	!restore ready for next row
		LAST_ROW_WT_SUM=WT_SUM		!            "

	     ELSE	! if j ne 1
*
*  otherwise, if not at very first pixel, or start of new row, evaluate the
*  extra counts added at the front and removed from the back and adjust
*  the sums accordingly
*
		FRONT_STRIP(1)=0.
		FRONT_STRIP(2)=0.
		BACK_STRIP(1)=0.
		BACK_STRIP(2)=0.
*
*  first, for row at back edge (1 behind smoothing box limit ! )
*
		CALL BSUB_UPDATE(DIMX,DIMY,IMAGE,WEIGHT,
     +		YSTART,YEND,XSTART-1,XSTART-1,MIN_XPOS,MAX_XPOS,
     +          MIN_YPOS,MAX_YPOS,BACK_STRIP(1),BACK_STRIP(2),
     +		ROUTINE_ID,STATUS)
*
*  and for front edge
*
		CALL BSUB_UPDATE(DIMX,DIMY,IMAGE,WEIGHT,
     +		YSTART,YEND,XEND,XEND,MIN_XPOS,MAX_XPOS,MIN_YPOS,
     +          MAX_YPOS,FRONT_STRIP(1),FRONT_STRIP(2),
     +		ROUTINE_ID,STATUS)
*
*  now adjust last summation values by adding new data from front strip
*  and subtracting lost data from back strip
*
		WT_CT_SUM=LAST_WT_CT_SUM+FRONT_STRIP(1)-BACK_STRIP(1)
		WT_SUM=LAST_WT_SUM+FRONT_STRIP(2)-BACK_STRIP(2)

	     END IF
*
*  now add on border region (bottom and top first). For the top and bottom
*  strips, the corners are defined as well.
*
	     BORDER(1)=0.
	     BORDER(2)=0.

	     IF(SM_BORDER)THEN

	       DO IS=I-HSIZE,I-HSIZE+1		!y dirn
		 WFY=0.25
		 IF(IS.EQ.I-HSIZE+1)WFY=0.75	!x dirn

		 DO JS=J-HSIZE,J+HSIZE
		    WFX=1.0
		    IF(JS.EQ.J-HSIZE)WFX=0.25
		    IF(JS.EQ.J-HSIZE+1)WFX=0.75
		    IF(JS.EQ.J+HSIZE-1)WFX=0.75
		    IF(JS.EQ.J+HSIZE)WFX=0.25

		    IF((JS.GE.MIN_XPOS.AND.JS.LE.MAX_XPOS).AND.
     +		    (IS.GE.MIN_YPOS.AND.IS.LE.MAX_YPOS))THEN

		      BORDER(1)=BORDER(1)+IMAGE(JS,IS)*WEIGHT(JS,IS)*
     +                WFY*WFX
		      BORDER(2)=BORDER(2)+WEIGHT(JS,IS)*WFY*WFX

		    END IF

		 END DO
	       END DO

	       DO IS=I+HSIZE-1,I+HSIZE
		 WFY=0.25
		 IF(IS.EQ.I+HSIZE-1)WFY=0.75

		 DO JS=J-HSIZE,J+HSIZE
		    WFX=1.0
		    IF(JS.EQ.J-HSIZE)WFX=0.25
		    IF(JS.EQ.J-HSIZE+1)WFX=0.75
		    IF(JS.EQ.J+HSIZE-1)WFX=0.75
		    IF(JS.EQ.J+HSIZE)WFX=0.25

		    IF((JS.GE.MIN_XPOS.AND.JS.LE.MAX_XPOS).AND.
     +		    (IS.GE.MIN_YPOS.AND.IS.LE.MAX_YPOS))THEN
		      BORDER(1)=BORDER(1)+IMAGE(JS,IS)*WEIGHT(JS,IS)*
     +		      WFY*WFX
		      BORDER(2)=BORDER(2)+WEIGHT(JS,IS)*WFY*WFX
		    END IF

		 END DO
	       END DO
*
*  for side strips
*
	       DO JS=J-HSIZE,J-HSIZE+1

		 WFX=0.25
		 IF(JS.EQ.J-HSIZE+1)WFX=0.75

		 DO IS=YSTART,YEND
		    WFY=1.0

		    IF((JS.GE.MIN_XPOS.AND.JS.LE.MAX_XPOS).AND.
     +		    (IS.GE.MIN_YPOS.AND.IS.LE.MAX_YPOS))THEN

		      BORDER(1)=BORDER(1)+IMAGE(JS,IS)*WEIGHT(JS,IS)*
     +		      WFY*WFX
		      BORDER(2)=BORDER(2)+WEIGHT(JS,IS)*WFY*WFX

		    END IF

		 END DO
	       END DO

	       DO JS=J+HSIZE-1,J+HSIZE
		 WFX=0.25
		 IF(JS.EQ.J+HSIZE-1)WFX=0.75

		 DO IS=YSTART,YEND
		    WFY=1.0

		    IF((JS.GE.MIN_XPOS.AND.JS.LE.MAX_XPOS).AND.
     +		    (IS.GE.MIN_YPOS.AND.IS.LE.MAX_YPOS))THEN
		      BORDER(1)=BORDER(1)+IMAGE(JS,IS)*WEIGHT(JS,IS)*
     +		      WFY*WFX
		      BORDER(2)=BORDER(2)+WEIGHT(JS,IS)*WFY*WFX
		    END IF

		 END DO
	       END DO

	     END IF
*
*  and get smoothed (average) value
*
	     IF(WT_SUM.EQ.0)THEN
	       SMOOTHED_IMAGE(J,I)=0.
	     ELSEIF(WT_SUM.GT.0.0)THEN
	       SMOOTHED_IMAGE(J,I)=(WT_CT_SUM+border(1))/
     +	       (WT_SUM+border(2))
	     END IF

	     MIN_CTS=MIN(MIN_CTS,SMOOTHED_IMAGE(J,I))
	     MAX_CTS=MAX(MAX_CTS,SMOOTHED_IMAGE(J,I))
*
*  store last values for count sum and the no of pixels used in getting sum
*
	     LAST_WT_CT_SUM=WT_CT_SUM
	     LAST_WT_SUM=WT_SUM

	  END DO	!over j

	END DO		!over i

	ROUTINE_ID=OLD_ROUTINE

	RETURN
	END
*
*+
*
*******************************
*
*  ROUTINE TO ADD ON APPROPRIATE SECTIONS OF IMAGE TO SMOOTH
*
	SUBROUTINE BSUB_UPDATE(DIMX,DIMY,IMAGE,WEIGHT,
     +	YS,YE,XS,XE,MIN_XPOS,MAX_XPOS,MIN_YPOS,MAX_YPOS,
     +  STRIP1,STRIP2,ROUTINE_ID,STATUS)
*
	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	INTEGER   DIMX, DIMY			!image dimensions

	REAL   IMAGE(DIMX,DIMY)			!image

	REAL   WEIGHT(DIMX,DIMY)		!weights

	INTEGER   XS, XE, YS, YE

	INTEGER   MIN_XPOS, MAX_XPOS

	INTEGER   MIN_YPOS, MAX_YPOS

	REAL   STRIP1, STRIP2

	INTEGER   IS, JS

	INTEGER   ROUTINE_ID, OLD_ROUTINE

	INTEGER   STATUS
*
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=8
*
	DO IS=YS,YE

	  DO JS=XS,XE	!loop in y direction

	     IF((JS.GE.MIN_XPOS.AND.JS.LE.MAX_XPOS).AND.(IS.GE.
     +       MIN_YPOS.AND.IS.LE.MAX_YPOS))THEN

		STRIP1=STRIP1+IMAGE(JS,IS)*WEIGHT(JS,IS)
		STRIP2=STRIP2+WEIGHT(JS,IS)

	     END IF

	  END DO
	END DO
*
	ROUTINE_ID=OLD_ROUTINE

	RETURN
	END
*
*+
*
*  routine to evaluate binomial error value
*
	SUBROUTINE BINOMIAL_ERROR(DIM,MEAN,IBIN,E,P_PROB,Q_PROB,STATUS)
*
	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	INTEGER   DIM			!no of pixels used in this calc

	REAL   MEAN			!supplied mean

	INTEGER   IBIN			!which histogram bin

	REAL   E			!returned error value

	REAL   P_PROB, Q_PROB		!probabilities p and q

	REAL   YV			!expected Poisson value

	INTEGER   I, J			!local variables

	REAL   FACTORIAL		!factorial value

	REAL   PI			!pi

	REAL   LN_FAC, LN_YV		!ln(factorial),   ln(yv)

	REAL   RJ			!real variable = x

	REAL   LOG_FACTORIALS(0:100)	!log (N!) array

	INTEGER   OLD_ROUTINE

	INTEGER   STATUS
*
	COMMON/FACTORIALS/LOG_FACTORIALS
	INCLUDE 'BSUB_CMN'
*
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=9
*
*  calclate J factorial (uses simple calculation if J < 30 and uses
*  Stirlings approximation if J > 30).
*
	IF(MEAN.GT.0.0)THEN

	  RJ=0.d0
	  J=IBIN				!NINT(XI)
	  RJ=J
*
	  IF(J.LE.50)THEN			!if J < 30  =>simple computation

	      LN_FAC=LOG_FACTORIALS(J)		!read from array

	  ELSEIF(J.GT.50)THEN	!if J > 50  => compute (using Stirlings approx)

	    IF(RJ.LE.0)THEN
	      CALL ERR_REP(' ',' Error in Poisson: RJ < 0.0 ',STATUS)
	      STATUS=-100
	      RETURN
	    ELSE

	      PI=4.0*ATAN(1.0)
	      LN_FAC=(RJ+0.50)*LOG(RJ) - RJ + 0.50*LOG(2.0*PI)

	    END IF

	  END IF
*
*  Poisson probability of getting J cts when the mean is X is
*
*                j  -X
*               X  e
*    Pr= A(1) --------
*                j!
*
	  LN_YV= RJ*LOG(MEAN) - MEAN - LN_FAC

	  YV=EXP(LN_YV)		!Pr
*
	  P_PROB=YV
	  Q_PROB=1.-P_PROB	!1-Pr =Qr
*
* NOTE dim*dim => npix_used(my new def of dim)
*
	  E=(1.*DIM*P_PROB*Q_PROB)	! need sqrt( NPIX * p * q)

	  IF(E.LE.0.0)THEN
	    E=1.0
	    WRITE(6,'(/ ,'' PROBLEM WITH ERROR VALUE'')')
	  ELSE
	    E=SQRT(E)		!TAKE SQRT if NOT -ve.
	  END IF

	ELSEIF(MEAN.EQ.0.0)THEN		!if mean =0.

	  E=1.0

	END IF
*
	ROUTINE_ID=OLD_ROUTINE

	END
*
*
*****************************************************************
*
*  Subroutine BSUB_FILL_FUNC transfers the PSF data in ARR_2D to PSF_FUNC
*
	SUBROUTINE BSUB_FILL_FUNC(N_AZIM,N_ELEV,PSF_FUNC,ARR_2D,STATUS)

	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	INTEGER   N_ELEV, N_AZIM		!dimenions of PSF grid

	REAL   PSF_FUNC(N_AZIM,N_ELEV)		!PSF grid

	REAL   ARR_2D(N_AZIM,N_ELEV)		!internal PSF grid

	INTEGER   I, J				!local variables

	INTEGER   OLD_ROUTINE			!routine ID store

	INTEGER   STATUS

	INCLUDE 'BSUB_CMN'
*
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=10
*
	DO I=1,N_ELEV
	  DO J=1,N_AZIM
	     PSF_FUNC(J,I)=ARR_2D(J,I)
	  END DO
	END DO

	ROUTINE_ID=OLD_ROUTINE

	END
*
*+  BSUB_FIT_HISTOGRAM - Fits the histogram to a poission distrn
*
	SUBROUTINE BSUB_FIT_HISTOGRAM(X,Y,E,N,A,MA,MFIT,LISTA,
     &  CHISQ,K,STATUS)

*	Description :
*	  Uses the Levenberg-Maruardt method to fit a poisson
*	  distribution to the histogram of the data
*	Method :
*	  Uses the Levenberg-Maruardt method to fit a poissionian to the
*	  data - taken from NUMERICAL RECIPES.  The Levenberg-Marquardt
*	  method attempting to reduce the value of Chi^2
*	  of a fit between a set of NDATA  points X(i),Y(i) with individual
*	  standard deviations EY(i) and a non-linear function dependent on
*	  MA coefficients. The program returns the best fit values for the
*	  MA fit parameters A and the chi^2, CHISQ. The arrays COVAR(NCA,NCA),
*	  ALPHA(NCA,NCA) with dimensions NCA are used as working space during
*	  most iterations. Supply a subroutine FUNCS(X,A,DYDA,MA) that
*	  evaluates the fitting function YFIT and its derivatives DYDA with
* 	  respect to the fitting parameters A at X. On the first call provide
*	  an initial guess for the parameters A and set ALAMDA<0 for
*	  initialisation (which then sets ALAMDA=0.001). If the step suceeds,
*	  CHISQ becomes smaller and ALAMDA decreases by a factor 10.0. If a
*	  step fails, ALAMDA grows by a factor 10.0. User must cal this
*	  routine repatedly until convergence is acheived. Then make one
*	  final call with ALAMDA=0.0 so that COVAR (I,J) returns the
*	  covariance matrix and ALPHA the curavture matrix
*	Author :
*	  J.P.D.Mittaz  Mullard Space Science Lab.  UCL.MSSL.C::JPDM
*	History :
*	  Written 23-JAN-1990
*	Type definitions :

	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	INTEGER N			!array dimensions

	REAL X(N),Y(N),E(N)		!histogram array values (J,NPIX,ERROR)

	INTEGER MA			!parameter counter

	REAL A(MA)			!fit parameters

	INTEGER STATUS, STATUSE		!status values

	INTEGER   IDIR, NSTEP		!error direction and no of steps

	INTEGER   LISTA(2)		!parameter IDs used in fitting

	INTEGER   MFIT, MFIT_E		!no of fitted pars for fit + error calcs

	LOGICAL CONVERGED		!overall convergence test

	LOGICAL PAR_CONVERGE		!parameter convergence test

	LOGICAL CHI_CONV		!chisqr convergence test

	LOGICAL REPEAT, REPEATED	!repeat fit tests

	REAL ALAMBDA			!lambda

	REAL OLD_A(2)			!store of last iteration fit parameters

	REAL DCHI, DPAR			!change in chisqr and par values

	REAL TOL			!fit tolerance

	REAL OCHISQ, OLD_ALAMBDA	!last iteration chisqr and lambda vals

	REAL ALPHA(2,2), COVAR(2,2)	!fitting matrices

	REAL CHISQ			!chisqr

	REAL DEL_CHI, CHI_REQ		!chisqr components for error analysis

	REAL   DIR(2)			!error direction

	REAL   A_BEST(2)		!best fit pars

	REAL   DA2, CHI_DIFF		!step size in A(2) and chisqr change

	REAL DELTA_A2			!actual val of delta(a(2)) (inc dirn)

	REAL   YV, DYDA(2)		!func value and derivatives

	INTEGER I, J, K			!local variables

	INTEGER OLD_ROUTINE

	INCLUDE 'BSUB_CMN'
*
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=11
*
	TOL=0.0001

*	set up old arrays

	DO I=1,MA
	  OLD_A(I)=A(I)
	END DO
*
	I=0
	K=0
	ALAMBDA=-1
	REPEAT=.FALSE.
	REPEATED=.FALSE.
	CONVERGED=.FALSE.

	DO WHILE(.NOT.CONVERGED.AND.I.LE.100)	!I.le.100 added in 4-1-91
	  IF(REPEAT.AND..NOT.REPEATED)THEN
	    A(2)=A(2)*0.97
	    ALAMBDA=-1
	    I=0
	    K=0
	    REPEAT=.FALSE.
	    REPEATED=.TRUE.
	  END IF

	  CALL BSUB_MIN_FUNC(X,Y,E,N,2,COVAR,ALPHA,A,MA,LISTA,MFIT,
     &	  CHISQ,ALAMBDA,STATUS)
	  IF(STATUS.NE.SAI__OK.AND.STATUS.NE.-300)THEN
	    CALL ERR_REP(' ','Error in BSUB_MIN_FUNC - returning to
     : top level', STATUS)
	    RETURN
	  ELSE IF(STATUS.EQ.-300)THEN
	    RETURN
	  END IF

	  I=I+1
	  K=K+1
	  IF(I.GT.1)THEN 		! more than just the one
	    IF(ALAMBDA.LT.OLD_ALAMBDA.AND.ALAMBDA.LT.1E8)THEN ! moved towards a better fit
              CHI_CONV=.FALSE.
	      DCHI=ABS(CHISQ-OCHISQ)/OCHISQ
	      IF(DCHI.LT.TOL)THEN
                CHI_CONV=.TRUE.
	        PAR_CONVERGE=.TRUE.
	        DO J=1,MA
	          DPAR=ABS ( (A(J)-OLD_A(J))/OLD_A(J) )    !added abs() 8-1-91
	          IF(DPAR.GT.TOL)THEN
	            PAR_CONVERGE=.FALSE.
	          END IF
	        END DO
	        IF(PAR_CONVERGE)THEN
	          CONVERGED=.TRUE.
	        END IF
	      END IF

	      IF(ABS(ALAMBDA).LT.1.E-35)CONVERGED=.TRUE.    !exit if alambda=0
							    !added 8-1-91
	      OLD_ALAMBDA=ALAMBDA
	      OCHISQ=CHISQ
	      DO J=1,MA
	        OLD_A(J)=A(J)
              END DO
	    ELSE IF(ALAMBDA.GE.1E8.AND..NOT.REPEATED)THEN
	      REPEAT=.TRUE.
	    ELSE IF(ALAMBDA.GE.1E8.AND.REPEATED)THEN
	      CONVERGED=.TRUE.
	    END IF
	  ELSE
	    OCHISQ=CHISQ
	    OLD_ALAMBDA=ALAMBDA
	    DO J=1,MA
	      OLD_A(J)=A(J)
	    END DO
	  END IF
	END DO

	ALAMBDA=0.0
	CALL BSUB_MIN_FUNC(X,Y,E,N,2,COVAR,ALPHA,A,MA,LISTA,MFIT,CHISQ,
     &  ALAMBDA,STATUS)
	IF(STATUS.NE.SAI__OK)THEN
	  CALL ERR_REP(' ','Error in BSUB_MIN_FUNC - returning to '/
     :                                        /'top level', STATUS )
	  RETURN
	END IF
*
	A_BEST(1)=A(1)
	A_BEST(2)=A(2)

	ROUTINE_ID=OLD_ROUTINE

	RETURN
	END
*
*+  subroutine to do gaussian elimination
*
	INTEGER FUNCTION BSUB_GAUSS_ELIM(A,N,NP,B,M,MP,STATUS)

	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	INTEGER   NMAX			!max array size

	PARAMETER (NMAX=50)

	INTEGER   N			!no of fitted pars

	INTEGER   M, NP, MP		!array dimensions

	INTEGER   I, J, K, L, LL	!local variables

	INTEGER   ICOL,IROW		!local variables

	REAL   PIVINV, DUM		!    "

	REAL   BIG			!   "

	REAL   A(NP,NP), B(NP,MP)	!local arrays

	REAL   IPIV(NMAX), INDXR(NMAX), INDXC(NMAX)	!local arrays

	INTEGER   STATUS

	INTEGER   OLD_ROUTINE

	INCLUDE 'BSUB_CMN'

	BSUB_GAUSS_ELIM=.TRUE.
*
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=12
*
	DO J=1,N
	  IPIV(J)=0
	END DO

	DO  I=1,N
	  BIG=0.

	  DO J=1,N
	    IF(IPIV(J).NE.1)THEN

	      DO K=1,N
		IF (IPIV(K).EQ.0) THEN

		  IF (ABS(A(J,K)).GE.BIG)THEN
		    BIG=ABS(A(J,K))
		    IROW=J
		    ICOL=K
		  END IF

		ELSE IF (IPIV(K).GT.1) THEN

		  CALL ERR_REP(' ','Singular matrix in fit',STATUS)
		  STATUS=-100
		  RETURN

		END IF

	      END DO

	    END IF
	  END DO

	  IPIV(ICOL)=IPIV(ICOL)+1

	  IF (IROW.NE.ICOL) THEN

	    DO L=1,N
	      DUM=A(IROW,L)
	      A(IROW,L)=A(ICOL,L)
	      A(ICOL,L)=DUM
	    END DO

	    DO L=1,M
	      DUM=B(IROW,L)
	      B(IROW,L)=B(ICOL,L)
	      B(ICOL,L)=DUM
	    END DO

	  END IF

	  INDXR(I)=IROW
	  INDXC(I)=ICOL

	  IF (A(ICOL,ICOL).EQ.0.)THEN
	    CALL ERR_REP(' ','Singular matrix in fit',STATUS)
	    STATUS=-100
	    RETURN
	  END IF

	  PIVINV=1./A(ICOL,ICOL)
	  A(ICOL,ICOL)=1.

	  DO L=1,N
	    A(ICOL,L)=A(ICOL,L)*PIVINV
	  END DO

	  DO L=1,M
	    B(ICOL,L)=B(ICOL,L)*PIVINV
	  END DO

	  DO LL=1,N

	    IF(LL.NE.ICOL)THEN
	      DUM=A(LL,ICOL)
	      A(LL,ICOL)=0.
	      DO L=1,N
		A(LL,L)=A(LL,L)-A(ICOL,L)*DUM
	      END DO
	      DO L=1,M
		B(LL,L)=B(LL,L)-B(ICOL,L)*DUM
	      END DO

	    END IF

	  END DO

	END DO

	DO L=N,1,-1
	  IF(INDXR(L).NE.INDXC(L))THEN
	    DO K=1,N
		DUM=A(K,INDXR(L))
		A(K,INDXR(L))=A(K,INDXC(L))
		A(K,INDXC(L))=DUM
	    END DO
	  END IF
	END DO

	ROUTINE_ID=OLD_ROUTINE

	RETURN
	END

*
*+  BSUB_GET_HEADER_INFO - Gets useful info + circular mask
*
	SUBROUTINE BSUB_GET_HEADER_INFO(OLOC,NX,NY,IMAGE,VARRAY,SIZE,
     &POINTED,MIN_X,MAX_X,MIN_Y,MAX_Y,STATUS)

*  A routine to get useful info from header and act upon it

	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	INCLUDE 'BSUB_CMN'

	CHARACTER*(DAT__SZLOC)	OLOC		!output image locator

	INTEGER NX,NY				!image dimensions

	REAL   IMAGE(NX,NY),VARRAY(NX,NY)	!image, variance array

	REAL   SIZE				!pixel size in arcsec

	LOGICAL POINTED				!pointed phase

	INTEGER MIN_X,MAX_X			!X limits of non-zero image

	INTEGER MIN_Y,MAX_Y			!Y           "

	INTEGER STATUS

	CHARACTER*(DAT__SZLOC)	LOCHEAD		! HEADER object

	CHARACTER*132 UNITS			!units string
	CHARACTER*132 STRING			!SURVEY string
	CHARACTER*132 INSTRUMENT 		!which instrument

	INTEGER I,J				!local counters

	INTEGER EXPOS,SURVEY			!index positions

	INTEGER SURVEY1,SURVEY2			!index positions

	INTEGER INSTR				!   "

	REAL   EXPOSURE_TIME			!exposure time

	REAL   RADIUS, OK_RADIUS		!radii

	REAL   PIX_X,PIX_Y			!posn from center

	DOUBLE PRECISION AXIS_RA,AXIS_DEC	!positions

	DOUBLE PRECISION FIELD_RA,FIELD_DEC	!

	DOUBLE PRECISION PI			! pi!

	DOUBLE PRECISION CEL(2),ROLL		!for routines

	DOUBLE PRECISION AZ_SUBIM,EL_SUBIM	!positions of center SUBIM

	DOUBLE PRECISION CTOS(3,3)

	INTEGER OLD_ROUTINE

	LOGICAL FLAG				!removed any?
	LOGICAL HEAD_THERE
	LOGICAL TARGET_THERE
	LOGICAL RA_THERE
	LOGICAL DEC_THERE
	LOGICAL FRA_THERE
	LOGICAL FDEC_THERE
	LOGICAL EXPOS_THERE
	LOGICAL INST_THERE
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=13
*
* set up constants
*
	PI=4.0D0*ATAN(1.0D0)

*      Default is they don't exist
        HEAD_THERE = .FALSE.
	TARGET_THERE=.FALSE.
        RA_THERE=.FALSE.
	DEC_THERE=.FALSE.
	FRA_THERE=.FALSE.
	FDEC_THERE=.FALSE.
	EXPOS_THERE=.FALSE.
	INST_THERE=.FALSE.

*      Does HEADER exist?
        CALL HDX_FIND( LOCMORE, 'ASTERIX.HEADER', LOCHEAD, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
	  HEAD_THERE=.TRUE.

*        Do components exist?
          CALL DAT_THERE( LOCHEAD, 'TARGET', TARGET_THERE, STATUS )
          CALL DAT_THERE( LOCHEAD, 'AXIS_RA', RA_THERE, STATUS )
          CALL DAT_THERE( LOCHEAD, 'AXIS_DEC', DEC_THERE, STATUS )
          CALL DAT_THERE( LOCHEAD, 'FIELD_RA', FRA_THERE, STATUS )
          CALL DAT_THERE( LOCHEAD, 'FIELD_DEC', FDEC_THERE, STATUS )
          CALL DAT_THERE( LOCHEAD, 'EXPOSURE_TIME', EXPOS_THERE,
     :                                                  STATUS )
          CALL DAT_THERE( LOCHEAD, 'INSTRUMENT', INST_THERE, STATUS )

        END IF

*      Annul status looking for components
        IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL(STATUS)

*
*  get units - if /C in string then exposure corrected
*
        CALL CMP_GET0C(OLOC,'UNITS',UNITS,STATUS)
	IF(STATUS.NE.SAI__OK)THEN
	  CALL MSG_PRNT('ERROR: No UNIT information in header')
          CALL ERR_ANNUL( STATUS )

	ELSE
	  CALL CHR_UCASE( UNITS )
	  EXPOS=INDEX(UNITS,'/C')

	  IF(EXPOS.NE.0)THEN

*          Exposure correct
            CALL CMP_GET0R( LOCHEAD, 'EXPOSURE_TIME', EXPOSURE_TIME,
     :                                                      STATUS )

	    CALL MSG_PRNT('Image was exposure corrected - for correct
     &background use UN-corrected image')
	    CALL MSG_SETR('EXP',EXPOSURE_TIME)
	    CALL MSG_PRNT('Exposure time was ^EXP seconds')

	    DO I=1,NY
	      DO J=1,NX
	        IMAGE(J,I)=FLOAT(NINT(IMAGE(J,I)*EXPOSURE_TIME))
	      END DO
	    END DO

	  ENDIF
	ENDIF

*      Get target instrument
        IF ( INST_THERE ) THEN
	  CALL CMP_GET0C( LOCHEAD, 'INSTRUMENT', INSTRUMENT, STATUS )
	  CALL CHR_UCASE( INSTRUMENT )
        ELSE
	  CALL MSG_PRNT( 'No instrument information, defaulting to WFC')
          INSTRUMENT = 'WFC'
        END IF
	INSTR = INDEX(INSTRUMENT,'WFC')

	IF(INSTR.GT.0)THEN

*  now find out if survey or pointed mode

	IF ( TARGET_THERE ) THEN
	  CALL CMP_GET0C(LOCHEAD,'TARGET',STRING,STATUS)
	ELSE
	  CALL MSG_PRNT('ERROR: No target information - assuming
     &WFC SURVEY' )
	  STRING='SURVEY'
	ENDIF

	CALL CHR_UCASE( STRING )
	SURVEY1=0
	SURVEY2=0
	SURVEY1=INDEX(STRING,'SURVEY')	!will be non-zero if SURVEY image
	SURVEY2=MAX(INDEX(STRING,'S2 DATA BASE'),
     :              INDEX(STRING,'S3 DATA BASE'))

	SURVEY=MAX(SURVEY1,SURVEY2)

	IF(SURVEY.NE.0)THEN
	  CALL MSG_PRNT(' ')
	  CALL MSG_PRNT('SURVEY image')
	  POINTED=.FALSE.
	ELSE
	  POINTED=.TRUE.
	  CALL MSG_PRNT(' ')
	  CALL MSG_SETC('TARG',STRING)
	  CALL MSG_PRNT('POINTED Phase image, Target ^TARG')

	  IF(RA_THERE)THEN
	    CALL CMP_GET0D(LOCHEAD,'AXIS_RA',AXIS_RA,STATUS)
	  ELSE
	    CALL MSG_PRNT('ERROR: no AXIS_RA information')
	    CALL MSG_PRNT('       No circular mask applied')
	  ENDIF
	  IF(DEC_THERE)THEN
	    CALL CMP_GET0D(LOCHEAD,'AXIS_DEC',AXIS_DEC,STATUS)
	  ELSE
	    CALL MSG_PRNT('ERROR: no AXIS_DEC information')
	    CALL MSG_PRNT('       No circular mask applied')
	  ENDIF
	  IF(FRA_THERE)THEN
	    CALL CMP_GET0D(LOCHEAD,'FIELD_RA',FIELD_RA,STATUS)
	  ELSE
	    CALL MSG_PRNT('ERROR: no FIELD_RA information')
	    CALL MSG_PRNT('       No circular mask applied')
	  ENDIF
	  IF(FDEC_THERE)THEN
	    CALL CMP_GET0D(LOCHEAD,'FIELD_DEC',FIELD_DEC,STATUS)
	  ELSE
	    CALL MSG_PRNT('ERROR: no FIELD_DEC information')
	    CALL MSG_PRNT('       No circular mask applied')
	  ENDIF

	  IF(STATUS.NE.SAI__OK)THEN
	    CALL MSG_PRNT('ERROR: some error in BSUB_GET_HEADER_INFO')
            CALL ERR_ANNUL( STATUS )
            GOTO 99
	  ENDIF

*	 Convert to radians
	  AXIS_RA=AXIS_RA*PI/180.D0
	  AXIS_DEC=AXIS_DEC*PI/180.D0
	  FIELD_RA=FIELD_RA*PI/180.D0
	  FIELD_DEC=FIELD_DEC*PI/180.D0

	  CEL(1)=AXIS_RA
	  CEL(2)=AXIS_DEC
	  ROLL=0.D0

*        get difference in radians
	  CALL CONV_GENDMAT(CEL(1),CEL(2),ROLL,CTOS )
	  CALL CONV_DSPCVRT(FIELD_RA,FIELD_DEC,CTOS,AZ_SUBIM,EL_SUBIM)

	  IF(AZ_SUBIM.GT.PI)AZ_SUBIM=AZ_SUBIM-2.D0*PI

*        get shift in pixels
	  AZ_SUBIM=AZ_SUBIM*(180.D0/PI)*3600.D0/DBLE(SIZE)
	  EL_SUBIM=EL_SUBIM*(180.D0/PI)*3600.D0/DBLE(SIZE)

*  now have shift in terms of pixels of this image from image center
*
*  now put on circular mask - 2.45 degrees

*  get number of pixels for radius

	  OK_RADIUS=2.45*3600./SIZE

	  FLAG=.FALSE.
	  DO I=MIN_Y,MAX_Y
	    DO J=MIN_X,MAX_X

*  get radius from real center
*  put mask on if necessary

	      PIX_X=J*1.-(NX*1.0)/2.
	      PIX_Y=I*1.-(NY*1.0)/2.
	      RADIUS=SQRT((PIX_X+SNGL(AZ_SUBIM))**2+
     &			(PIX_Y+SNGL(EL_SUBIM))**2)
	      IF(RADIUS.GT.OK_RADIUS)THEN
	        FLAG=.TRUE.
	        VARRAY(J,I)=-100
	        IF(IMAGE(J,I).EQ.0)THEN
	          IMAGE(J,I)=-100000
	        ELSE
		  IMAGE(J,I)=-IMAGE(J,I)
	        ENDIF
	      ENDIF

	    END DO
	  END DO

	  IF(FLAG)THEN
	    CALL MSG_PRNT('Circlular mask placed on image')
	  ENDIF

	ENDIF

        ELSE              ! Not WFC
          POINTED = .FALSE.
          CALL MSG_SETC( 'INS', INSTRUMENT )
          CALL MSG_PRNT( 'Instrument is ^INS' )
	ENDIF

	ROUTINE_ID=OLD_ROUTINE

*      Annul locators even if bad status by declaring new error context
 99     CALL ERR_BEGIN( STATUS )

        IF ( HEAD_THERE ) CALL DAT_ANNUL( LOCHEAD, STATUS )

        CALL ERR_END( STATUS )

	END


*+
*
*  Subroutine BSUB_GET_PSF_GRID reads in the PSF data.
*
	SUBROUTINE BSUB_GET_PSF_GRID(SIZE,XCEN,YCEN,xoff,yoff,
     *  N_AZIM,N_ELEV,PSF_FUNC,STATUS)
*
	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	real*4 XCEN, YCEN		!image coords at which to get PSF

	REAL   XOFF, YOFF		!offsets from pixel centre

	INTEGER   IMCP			!detector

	INTEGER   N_AZIM, N_ELEV 	!No of grid pts in Az and Elev

	INTEGER   MAX

	INTEGER   I, J, K		!local counters

	INTEGER   ARR_PTR, ARR_DIM	!pointers to arrays

	REAL   PSF_FUNC(N_AZIM,N_ELEV)  !PSF function

	REAL   SIZE			!pixel size in arc secs

	REAL   SCF			! factor to normalise peak to 1.0

	DOUBLE PRECISION MJD		!modified Julian data

	REAL   ENERGY			!energy to evaluate PSf at

	REAL   AZIM, D_AZIM		!central Azimuth and step size

	REAL   ELEV, D_ELEV		!central Eleveation and step size

	REAL   OFF_AZIM, OFF_ELEV 	!psf centre offset in Az and elev

	REAL   PIX_SIZ			!pixel size in required units

	REAL   SUM			!integrated PSF value

	CHARACTER*(DAT__SZLOC) PLOC	!locator

	INTEGER   DIMS(2), PTR		!dims and pointer to internal PSF array

	INTEGER NDIMS			!no of dims

	LOGICAL DATA_OK			!data ok flag
*
	INTEGER   OLD_ROUTINE

	INTEGER   STATUS

	INCLUDE 'BSUB_CMN'
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=14
*
*  set up parameters for PSF evaluation
*
	DIMS(1)=N_AZIM
	DIMS(2)=N_ELEV

	CALL DYN_MAPR(2,DIMS,PTR,STATUS)

	PIX_SIZ=SIZE

	AZIM=XCEN		!azimuth of PSF cf image centre
	D_AZIM=PIX_SIZ		!step size in Azimuth

	ELEV=YCEN		!elevation of PSF cf image centre
	D_ELEV=PIX_SIZ		!step size in elevation

	OFF_AZIM=XOFF
	OFF_ELEV=YOFF
*
*  access master calibration file to derive PSF.
*
	CALL PSF_2D_DATA( PSFHAN, AZIM, ELEV, OFF_AZIM, OFF_ELEV,
     &    D_AZIM, D_ELEV, .TRUE.,N_AZIM,N_ELEV,%VAL(PTR),STATUS)

	IF(STATUS.NE.SAI__OK)THEN
	  CALL ERR_REP(' ','Error in PSF system - returning to top
     & level',STATUS)
	  RETURN
	END IF

	CALL BSUB_FILL_FUNC(N_AZIM,N_ELEV,PSF_FUNC,%VAL(PTR),STATUS)

	CALL DYN_UNMAP(PTR,STATUS)
*
*  get max of PSF from a grid
*
	PSF_MAX=0
	PSF_INTEGRAL=0.
	DO I=1,N_ELEV
	   DO J=1,N_AZIM
	      PSF_INTEGRAL=PSF_INTEGRAL+PSF_FUNC(J,I)
	      PSF_MAX=MAX(PSF_MAX,PSF_FUNC(J,I))
	   END DO
	END DO
*
* store dimensions
*
	PSF_X=N_AZIM
	PSF_Y=N_ELEV
*
*  normalise psf to make maximum value =1
*
	PSF_INTEGRAL=0.
	DO I=1,PSF_Y
	  DO J=1,PSF_X
	     PSF_FUNC(J,I)=PSF_FUNC(J,I)/PSF_MAX
	     PSF_INTEGRAL=PSF_INTEGRAL+PSF_FUNC(J,I)
	  END DO
	END DO
*
	ROUTINE_ID=OLD_ROUTINE

	END
*
*+  BSUB_MARQ_COEF
*
	SUBROUTINE BSUB_MARQ_COEF(X,Y,EY,NDATA,A,MA,LISTA,MFIT,ALPHA,
     &  BETA,NALP,CHISQ,STATUS)

*	Description :
*	  Used by BSUB_MARQ_COEF to evaluate the linearized fitting matrix ALPHA
*	  and the vector beta as in NUMERICAL RECIPES
*	Method :
*	  See NUMERICAL RECIPES
*	Author :
*	  From NUMERICAL RECIPES
*	History :
*	  Removed LISTA array from original code 23-JAN-1990
*	    	J.P.D.Mittax Mullard Space Science Lab. UCL.MSSL.C::JPDM
*	Type definitions :

	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	INTEGER MMAX				!max dim of arrays
	PARAMETER (MMAX=2)

	INTEGER NDATA				! number of data points

	INTEGER MA				! number of parameters

	INTEGER NALP				! array dimensions

	INTEGER   STATUS

	INTEGER   OLD_ROUTINE

	INTEGER I, J, K				!local counters

	REAL   X(NDATA), Y(NDATA), EY(NDATA)	!data arrays to be fitted

	REAL   A(MA)				!parameter array

	INTEGER MFIT				!no of pars to fit

	INTEGER LISTA(MA)			!parameter ID array

	REAL   ALPHA(NALP,NALP)			!internal array

	REAL   BETA(MA)				!      "

	REAL   CHISQ				!chisqr

	REAL   DYDA(MMAX)			!derivative array

	REAL   YMOD				!value of function

	REAL   SIG2I				!weight

	REAL   DY, WT				!internal variables

	INCLUDE 'BSUB_CMN'
*
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=15

*	zero everything

	DO J=1,MFIT
	  DO K=1,J
	    ALPHA(J,K)=0.0
	  END DO
	  BETA(J)=0.0
	END DO

	CHISQ=0.0

*	get values and derivatives

	DO I=1,NDATA

*  	work out poisson distribution

	  CALL BSUB_POISSON(X(I),A,YMOD,DYDA,MA,STATUS)
	  IF(STATUS.EQ.-100)THEN
	    CALL ERR_REP(' ','Error in BSUB_POISSON - returning '/
     :                                    /'to top level', STATUS)
	    RETURN
	  ELSE IF(STATUS.EQ.-200)THEN
	    RETURN
	  END IF
	  SIG2I=1.0/(EY(I)*EY(I))
	  DY=Y(I)-YMOD
	  DO J=1,MFIT
	    WT=DYDA(LISTA(J))*SIG2I
	    DO K=1,J
	      ALPHA(J,K)=ALPHA(J,K)+WT*DYDA(LISTA(K))
	    END DO
	    BETA(J)=BETA(J)+DY*WT
	  END DO
	  CHISQ=CHISQ+DY*DY*SIG2I
	END DO

	DO J=2,MFIT
	  DO K=1,J-1
	    ALPHA(K,J)=ALPHA(J,K)
	  END DO
	END DO

	ROUTINE_ID=OLD_ROUTINE

	END
*
*+  BSUB_MIN_FUNC  		This Routines is taken from Numerical Recipes
*
	SUBROUTINE BSUB_MIN_FUNC(X,Y,EY,NDATA,NCA,COVAR,ALPHA,
     &  A,MA,LISTA,MFIT,CHISQ,ALAMDA,STATUS)

*	Description :
*	  Uses the Levenberg-Maruardt method to fit a poissionian to the
*	  data - taken from NUMERICAL RECIPES.  The Levenberg-Marquardt
*	  method attempting to reduce the value of Chi^2
*	  of a fit between a set of NDATA  points X(i),Y(i) with individual
*	  standard deviations EY(i) and a non-linear function dependent on
*	  MA coefficients. The program returns the best fit values for the
*	  MA fit parameters A and the chi^2, CHISQ. The arrays COVAR(NCA,NCA),
*	  ALPHA(NCA,NCA) with dimensions NCA are used as working space during
*	  most iterations. Supply a subroutine FUNCS(X,A,DYDA,MA) that
*	  evaluates the fitting function YFIT and its derivatives DYDA with
* 	  respect to the fitting parameters A at X. On the first call provide
*	  an initial guess for the parameters A and set ALAMDA<0 for
*	  initialisation (which then sets ALAMDA=0.001). If the step suceeds,
*	  CHISQ becomes smaller and ALAMDA decreases by a factor 10.0. If a
*	  step fails, ALAMDA grows by a factor 10.0. User must cal this
*	  routine repatedly until convergence is acheived. Then make one
*	  final call with ALAMDA=0.0 so that COVAR (I,J) returns the
*	  covariance matrix and ALPHA the curavture matrix
*	Method :
*	  See NUMERICAL RECIPES
*	Author :
*	  From NUMERICAL RECIPES
*	History :
*	  Removed LISTA array from original code 23-JAN-1990
*	    	J.P.D.Mittax Mullard Space Science Lab. UCL.MSSL.C::JPDM
*	Type definitions :

	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	INTEGER MMAX				!max array dimension
	PARAMETER(MMAX=2)

	INTEGER   NDATA				! number of data points

	INTEGER   MA				! number of variables(2)

	INTEGER   STATUS			! status flag

	INTEGER   NCA

	REAL   X(NDATA),Y(NDATA),EY(NDATA)	! input data

	REAL   COVAR(NCA,NCA)			! covariance matrix/work

	REAL   ALPHA(NCA,NCA)			! work array

	REAL   A(MA)				! parameter array

	INTEGER MFIT				! no of fitted params

	INTEGER LISTA(MA)			!parameter ID array

	REAL   ALAMDA				!lambda

	REAL   CHISQ				! chi-squared

	REAL   ATRY(MMAX)			! trial A values

	REAL   BETA(MMAX)			!internal array

	REAL   DA(MMAX)				!     "

	REAL   OCHISQ				! old chi-squared

	INTEGER J, K, IHIT, KK			!internal counters

	INTEGER ISTATUS_G

	INTEGER BSUB_GAUSS_ELIM

	INTEGER OLD_ROUTINE

	INCLUDE 'BSUB_CMN'
*
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=16

*  	to set everything up use alamda lt 0

	IF(ALAMDA.LT.0)THEN

          KK=MFIT+1
	  DO J=1,MA

            IHIT=0
	    DO K=1,MFIT
              IF(LISTA(K).EQ.J)IHIT=IHIT+1
	    END DO

            IF (IHIT.EQ.0) THEN
              LISTA(KK)=J
              KK=KK+1
            ELSE IF (IHIT.GT.1) THEN
              CALL ERR_REP(' ','Improper permutation in LISTA',
     &STATUS)
	      STATUS=-200
	      RETURN
            END IF

	  END DO

          IF (KK.NE.(MA+1))THEN
	    CALL ERR_REP(' ','Improper permutation in LISTA',STATUS)
	    STATUS=-200
	    RETURN
	  END IF

          ALAMDA=0.001

*	get old chisq and alpha/beta etc

	  CALL BSUB_MARQ_COEF(X,Y,EY,NDATA,A,MA,LISTA,MFIT,ALPHA,BETA,NCA,
     &CHISQ,STATUS)
	  IF(STATUS.NE.SAI__OK.AND.STATUS.NE.-300)THEN
	    CALL ERR_REP(' ','Error in BSUB_MARQ_COEF - returning '/
     :                                      /'to top level', STATUS)
	    RETURN
	  ELSE IF(STATUS.EQ.-300)THEN
	    RETURN
	  END IF
	  OCHISQ=CHISQ

*	fill atry with A

	  DO J=1,MA
	    ATRY(J)=A(J)
	  END DO

	END IF

	DO J=1,MFIT

	  DO K=1,MFIT
	    COVAR(J,K)=ALPHA(J,K)
	  END DO
	  COVAR(J,J)=ALPHA(J,J)*(1.0+ALAMDA)
	  DA(J)=BETA(J)

	END DO

*	do gauss-jordan elimination

	ISTATUS_G=BSUB_GAUSS_ELIM(COVAR,MFIT,NCA,DA,1,1,STATUS)
	IF(STATUS.EQ.100)THEN			! singular matrix
	  RETURN
	END IF
	IF(.NOT.ISTATUS_G)THEN
	  STATUS=-100
	ENDIF

*	if final try - get covariance matrix

	IF(ALAMDA.EQ.0)THEN
	  CALL BSUB_SORT_COVAR(COVAR,NCA,MA,LISTA,MFIT,STATUS)
	  RETURN
	END IF

*	change value of ATRY to new value

	DO J=1,MFIT
	  ATRY(LISTA(J))=A(LISTA(J))+DA(J)
	end do

*	get new values with new ATRY

	CALL BSUB_MARQ_COEF(X,Y,EY,NDATA,ATRY,MA,LISTA,MFIT,COVAR,DA,NCA,
     &CHISQ,STATUS)
	IF(STATUS.NE.SAI__OK.AND.STATUS.NE.-300)THEN
	  CALL ERR_REP(' ','Error in BSUB_MARQ_COEF - returning '/
     :                                    /'to top level', STATUS)
	  RETURN
	ELSE IF(STATUS.EQ.-300)THEN
	  RETURN
	END IF

*	check an improvement in chisq

	IF(CHISQ.LT.OCHISQ)THEN

	  ALAMDA=0.1*ALAMDA
	  OCHISQ=CHISQ

	  DO J=1,MFIT

	    DO K=1,MFIT
	      ALPHA(J,K)=COVAR(J,K)
	    END DO

	    BETA(J)=DA(J)
	    A(LISTA(J))=ATRY(LISTA(J))

	  END DO

	ELSE

	  ALAMDA=10.0*ALAMDA
	  CHISQ=OCHISQ

	END IF

	ROUTINE_ID=OLD_ROUTINE

	END
*
*+
*
*  Subroutine POISSON evaluates the function value and derivatives
*  of a Poisson distribution at XI
*
	SUBROUTINE BSUB_POISSON(XI,A,YV,DYDA,NA,STATUS)
*
	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	REAL   XI		!input X value

	REAL   A(2)		!parameter array

	REAL   YV		!function value

	REAL   DYDA(2)		!derivatives

	INTEGER   NA		!dimension of parameter array

	INTEGER   STATUS	!status

	INTEGER   J		!integer x value

	INTEGER   I		!counter

	REAL   FACTORIAL	!factorial value

	REAL   PI		!pi

	REAL   LN_FAC, LN_YV	!ln(factorial),   ln(yv)

	REAL   RJ		!real variable = x

	REAL   LOG_FACTORIALS(0:100)	!log(N!) array

	INTEGER   OLD_ROUTINE

	COMMON/FACTORIALS/LOG_FACTORIALS
*
	INCLUDE 'BSUB_CMN'
*
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=17
*
*  If J is <= 50, read log(J!) from array, else, compute directly
*  using Stirlings approx. Note the setup computation for the array
*  only goes up to 50, using simple J! and Stirlings approx where
*  appropriate. See BSUB_COMPUTE_FACTORIALS
*
	RJ=0.D0
	J=NINT(XI)
	RJ=J
*
	IF(J.LE.50)THEN			!J < 50  get log(N!) from array

	  LN_FAC=LOG_FACTORIALS(J) 	!read from array

	ELSEIF(J.GT.50)THEN		!if J > 50  => Stirlings approx

	  IF(RJ.LE.0)THEN
	    CALL ERR_REP(' ',' Error in Poisson: RJ < 0.0 ',STATUS)
	    STATUS=-100
	    RETURN
	  ELSE

	    PI=4.0*ATAN(1.0)
	    LN_FAC=(RJ+0.50)*LOG(RJ) - RJ + 0.50*LOG(2.0*PI)

	  END IF

	END IF
*
*  Poisson function and derivatives for background are
*
*                j  -X
*               X  e       dP    P     dP     |  J      |
*    Pr= A(1) --------     -- = ---,   -- = P*| ---  -1 |
*                j!       a(1)  a(1)  a(2)    | a(2)    |
*
	IF(A(1).LE.0)THEN

	  CALL MSG_PRNT(' Error in fitting histogram A1 < 0 ')

	  STATUS=-100
	  RETURN

	ELSE IF(A(2).LE.0)THEN

*	  CALL MSG_PRNT(' Error in fitting histogram A2 < 0 ')

	  STATUS=-300
	  RETURN

	ELSE

	  LN_YV=LOG(A(1)) + RJ*LOG(A(2)) - A(2) - LN_FAC

	END IF

	YV=EXP(LN_YV)
*
	DYDA(1)=YV/A(1)
	DYDA(2)=YV * ((RJ/A(2)) -1.D0)
*
	ROUTINE_ID=OLD_ROUTINE

	END
*
*+
*
*  routine to sort the covariance matrix such that it corresponds
*  to the list of input parameters
*
	SUBROUTINE BSUB_SORT_COVAR(COVAR,NCVM,MA,LISTA,MFIT,STATUS)

	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	INTEGER MMAX			!max array dimension
	PARAMETER (MMAX=2)

	INTEGER NCVM, MA		!array size, no of parameters

	INTEGER MFIT			!no of fitted parameters

	INTEGER LISTA(MA)		!parameter ID array

	REAL   COVAR(NCVM,NCVM)		!internal covariance matrix

	REAL   SWAP			!swap over variable

	INTEGER I, J			!internal counters

	INTEGER   STATUS

	INTEGER OLD_ROUTINE

	INCLUDE 'BSUB_CMN'
*
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=18

	DO J=1,MA-1
	  DO I=J+1,MA
	    COVAR(I,J)=0.0
	  END DO
	END DO

	DO I=1,MFIT-1
	  DO J=I+1,MFIT
	    IF(LISTA(J).GT.LISTA(I))THEN
	      COVAR(LISTA(J),LISTA(I))=COVAR(I,J)
	    ELSE
	      COVAR(LISTA(I),LISTA(J))=COVAR(I,J)
	    END IF
	  END DO
	END DO

	SWAP=COVAR(1,1)
	DO J=1,MA
	  COVAR(1,J)=COVAR(J,J)
	  COVAR(J,J)=0.0
	END DO

	COVAR(LISTA(1),LISTA(1))=SWAP
	DO J=2,MFIT
	  COVAR(LISTA(J),LISTA(J))=COVAR(1,J)
	END DO

	DO J=2,MA
	  DO I=1,J-1
	    COVAR(I,J)=COVAR(J,I)
	  END DO
	END DO

	ROUTINE_ID=OLD_ROUTINE

	END
*
**************************
*+
*  routine to perform a linear least squares fit of the PSF template
*  + a background to the data around a source
*
	SUBROUTINE BSUB_FIT_PSF_TEMPLATE(IMAGE_X,IMAGE_Y,IMAGE,
     &  MIN_XPOS,MAX_XPOS,MIN_YPOS,MAX_YPOS,XPOS,YPOS,
     &  N_AZIM,N_ELEV,PSF_FUNC,PSF_COEFS,RMAX,R_4SIG,RDIMS1,RDIMS2,
     &  RBIN,NP,STATUS)
*
	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'
D	IMPLICIT NONE

	INTEGER   IMAGE_X, IMAGE_Y		!image dimensions

	REAL   IMAGE(IMAGE_X,IMAGE_Y)		!image

	INTEGER   MIN_XPOS, MAX_XPOS		!X limits of useable image

	INTEGER   MIN_YPOS, MAX_YPOS		!Y limits of useable image

	REAL   XPOS, YPOS			!source centroid

	INTEGER   N_AZIM, N_ELEV		!dimensions of PSF array

	REAL   PSF_FUNC(N_AZIM,N_ELEV)		!PSF array

	REAL   PSF_COEFS(4)			!fitted funcction coefficients

	INTEGER   RMAX				!source radius limit

	INTEGER   RDIMS1, RDIMS2		!dims for RBIN

	REAL   RBIN(RDIMS1,RDIMS2)		!bkgnd+src flux in radial bin

	INTEGER   NP(RDIMS2)			!pixels in annular bin

	INTEGER   STATUS			!status

	DOUBLE PRECISION BCOEF(4)

	INTEGER   IP				!number of coefficients

	INTEGER   IX_ST, IX_END, IY_ST, IY_END	!PSF grid lims on image

	INTEGER   II, JJ, KK			!loop variables

	INTEGER   IBIN				!radial bin identifier

	REAL   RPIX				!pixel radius from src centre

	REAL   RSTEP				!annulus width

	INTEGER   R_4SIG			!rad where src is 4sig over bkgd

	REAL   FUNC, ERR			!fitted func value + err on data

	REAL   SMX, SMY, SMZ, SMT		!summation variables
	REAL   SMXY, SMXZ, SMYZ, SMZT		!        "
	REAL   SMX2, SMY2, SMT2, SMXT, SMYT	!        "

	DOUBLE PRECISION E, F, G, H, I, J, K, L	!computation variables
	DOUBLE PRECISION M, N, P, Q		!          "
	DOUBLE PRECISION R, S, T, U, V, W	!          "
	DOUBLE PRECISION XX, YY			!          "

	INTEGER   NUSED 			!pixels used in computations

	LOGICAL FOUND_LIMIT			!radial limit logical

	LOGICAL FOUND_4SIG			!4 sigma radius logical

	INTEGER   OLD_ROUTINE

	INCLUDE 'BSUB_CMN'
*
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=19
*
*  Routine will fit a function of the form f(x,y)=A + B*x + C*y + D*t(x,y)
*  where t(x,y) if the normalised PSF template
*  First, must store the appropriate summation values for computing fit
*  coefficients
*
*  define limits of PSF grid on the image
*
	IX_ST=INT(XPOS+1)-INT(N_AZIM/2.)	!PSF X lower lim (image coords)
	IX_END=INT(XPOS+1)+INT(N_AZIM/2.)	!      upper
	IY_ST=INT(YPOS+1)-INT(N_ELEV/2.)	!    Y lower
	IY_END=INT(YPOS+1)+INT(N_ELEV/2.)	!      upper
*
* initialise summation variables
*
	SMX=0.
	SMY=0.
	SMT=0.
	SMZ=0.
	SMXY=0.
	SMXZ=0.
	SMYZ=0.
	SMXT=0.
	SMYT=0.
	SMZT=0.
	SMX2=0.
	SMY2=0.
	SMT2=0.
*
*  loop over all pixels within PSF grid area and get summation values
*
	KK=0
	DO II=IX_ST,IX_END
	  DO JJ=IY_ST, IY_END

	    IF((II.GE.MIN_XPOS.AND.II.LE.MAX_XPOS).AND.
     +      (JJ.GE.MIN_YPOS.AND.JJ.LE.MAX_YPOS))THEN	!within image region

	       KK=KK+1				!ARRAY COUNTER

	       SMX=SMX+( II-IX_ST+1 )
	       SMY=SMY+( JJ-IY_ST+1 )
	       SMZ=SMZ+IMAGE(II,JJ)
	       SMT=SMT+PSF_FUNC(II-IX_ST+1,JJ-IY_ST+1)

	       SMX2=SMX2+( II-IX_ST+1 )**2
	       SMY2=SMY2+( JJ-IY_ST+1 )**2
	       SMT2=SMT2+( PSF_FUNC(II-IX_ST+1,JJ-IY_ST+1) )**2

	       SMXZ=SMXZ+( II-IX_ST+1 )*IMAGE(II,JJ)
	       SMYZ=SMYZ+( JJ-IY_ST+1 )*IMAGE(II,JJ)
	       SMXT=SMXT+( II-IX_ST+1 )*PSF_FUNC(II-IX_ST+1,JJ-IY_ST+1)
	       SMYT=SMYT+( JJ-IY_ST+1 )*PSF_FUNC(II-IX_ST+1,JJ-IY_ST+1)
	       SMZT=SMZT+PSF_FUNC(II-IX_ST+1,JJ-IY_ST+1)*IMAGE(II,JJ)

	    END IF		!within image region?

	  END DO
	END DO
*
*  compute case where we include only A+D*t(x,y)
*

	BCOEF(1)=(SMT2*SMZ-SMT*SMZT)/(KK*SMT2-SMT*SMT)
	BCOEF(2)=(KK*SMZT-SMT*SMZ)/(KK*SMT2-SMT*SMT)
*
* compute the least squares coefficients of the full fit directly
* using equations derived for a fit to a function of the form A+Bx+Cy+dT(x,y)
* First compute some quantities for this
*
	E=KK*SMXZ-SMX*SMZ
	F=KK*SMX2-SMX*SMX
	G=KK*SMXY-SMX*SMY
	H=KK*SMXT-SMX*SMT

	I=KK*SMYZ-SMY*SMZ
	J=KK*SMXY-SMX*SMY
	K=KK*SMY2-SMY*SMY
	L=KK*SMYT-SMY*SMT

	M=KK*SMZT-SMZ*SMT
	N=KK*SMXT-SMX*SMT
	P=KK*SMYT-SMY*SMT
	Q=KK*SMT2-SMT*SMT

	R=J*E-I*F
	S=J*G-K*F
	T=J*H-L*F

	U=N*E-M*F
	V=N*G-P*F
	W=N*H-Q*F

	XX=R*V-U*S
	YY=V*T-W*S
*
* and get the coefficients
*
	IF(YY.NE.0.D0)THEN

	  BCOEF(4)=XX/YY

	  IF(S.NE.0.D0)THEN
	    BCOEF(3)=(R-BCOEF(4)*T)/S
	  ELSEIF(V.NE.0.D0)THEN
	    BCOEF(3)=(U-BCOEF(4)*W)/V
	  END IF

	  IF(N.NE.0.D0)THEN
	    BCOEF(2)=(M-BCOEF(3)*P-BCOEF(4)*Q)/N
	  ELSEIF(J.NE.0.D0)THEN
	    BCOEF(2)=(I-BCOEF(3)*K-BCOEF(4)*L)/J
	  ELSEIF(F.NE.0.D0)THEN
	    BCOEF(2)=(E-BCOEF(3)*G-BCOEF(4)*H)/F
	  END IF

	  IF(KK.NE.0)THEN
	    BCOEF(1)=(SMZ-BCOEF(2)*SMX-BCOEF(3)*SMY-BCOEF(4)*SMT)/KK
	  END IF

	END IF

C	write(6,'(/ ,'' Coefficients= '',f6.3,2x,f10.5,2x,f10.5,f10.3)')
C     &  (bcoef(ii),ii=1,4)

	IP=4
	DO II=1,IP			!store coefs for output
	  PSF_COEFS(II)=BCOEF(II)
	END DO
*
*  now compute the limiting radius of the source by testing to see at what
*  radius the source flux in an annulus (from fitted PSF function) drops below
*  the 1 sigma detection threshold above the predicted backround in that
*  annulus (could also make it say below 5% of background level)
*
	RSTEP=1.		!annulus width

	DO IBIN=1,RDIMS2	!Clear annulus summation arrays
	  RBIN(1,IBIN)=0.	!background
	  RBIN(2,IBIN)=0.	!source
	  NP(IBIN)=0
	END DO

	RMAX=0			!initialise
*
*  loop over PSF grid area and assign each pixel to a radial bin.
*  Then sum up PSF flux and background (using constant term only)
*
	DO II=IX_ST,IX_END
	  DO JJ=IY_ST,IY_END

	    IF((II.GE.MIN_XPOS.AND.II.LE.MAX_XPOS).AND.
     +      (JJ.GE.MIN_YPOS.AND.JJ.LE.MAX_YPOS))THEN	!within image area?

	      RPIX=SQRT( (II-0.5-XPOS)**2 + (JJ-0.5-YPOS)**2 )	!rad from centre

	      IBIN=RPIX/RSTEP+1				!radial bin
	      RMAX=MAX(RMAX,IBIN)			!largest radial bin

	      RBIN(1,IBIN)=RBIN(1,IBIN)+PSF_COEFS(1)	!sum up background

	      RBIN(2,IBIN)=RBIN(2,IBIN)+PSF_COEFS(4)*
     +        PSF_FUNC(II-IX_ST+1,JJ-IY_ST+1)		!and fitted source flux

	      NP(IBIN)=NP(IBIN)+1			!no of pixels in annulus
*
*  get chisqr and data-fit sums of the fit a) within the 90% radius and
*  b) (for the difference only) outside that radius
*
	      ERR=1.0
	      IF(IMAGE(II,JJ).NE.0.0)ERR=SQRT(IMAGE(II,JJ))

	      FUNC=PSF_COEFS(1)+(II-IX_ST+1)*PSF_COEFS(2)+
     +	      (JJ-IY_ST+1)*PSF_COEFS(3)+PSF_COEFS(4)*PSF_FUNC(II-IX_ST+1,
     +	      JJ-IY_ST+1)

	    END IF

	  END DO
	END DO

*
*  determine smallest radius (i.e., make sure radius isn't larger than PSF grid
*
	IF(INT(N_AZIM/2.).LT.RMAX)RMAX=INT(N_AZIM/2.)
	IF(INT(N_ELEV/2.).LT.RMAX)RMAX=INT(N_ELEV/2.)
*
*  and test each annulus to find out where source flux becomes consistent with
*  background level at 1 sigma level. Note that we are assuming Poisson stats
*  on a non Poissonian set of data but the results should be sufficiently
*  useful for this test.
*
	FOUND_LIMIT=.FALSE.			!initialise
	FOUND_4SIG=.FALSE.			!     "

	II=0
	DO WHILE(.NOT.FOUND_LIMIT.AND.II.LE.RMAX)  !until limit reached

	  II=II+1					!next loop

	  IF(RBIN(1,II).GT.0.0)THEN		!if background is non zero

	    IF(.NOT.FOUND_4SIG.AND.RBIN(2,II).LT.4*SQRT(RBIN(1,II)) )THEN
	      R_4SIG=II
	      FOUND_4SIG=.TRUE.
	    END IF

	    IF( RBIN(2,II).LT.0.05*RBIN(1,II) )THEN	!if source flux
	      FOUND_LIMIT=.TRUE.			!consistent with bkgnd
	    END IF

	  END IF

	END DO
*
	RMAX=II				!maximum radius of source
	IF(.NOT.FOUND_4SIG)R_4SIG=RMAX	!set to outer limit if not reached

	ROUTINE_ID=OLD_ROUTINE

	RETURN
	END
*
*+
*  subroutine to subtract off the scaled PSF template
*
	SUBROUTINE BSUB_UPDATE_SOURCE_MAP(IMAGE_X,IMAGE_Y,SRCMAP,
     +  N_AZIM,N_ELEV,PSF_FUNC,MIN_XPOS,MAX_XPOS,MIN_YPOS,MAX_YPOS,
     +  XPOS,YPOS,PSF_COEFS,RMAX,R_4SIG,STATUS)

	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	INTEGER   IMAGE_X, IMAGE_Y		!image dimensions

	REAL   SRCMAP(IMAGE_X, IMAGE_Y)		!source map to update

	INTEGER   N_AZIM, N_ELEV		!PSF dimensions

	REAL   PSF_FUNC(N_AZIM, N_ELEV)		!PSF array

	INTEGER   MIN_XPOS, MAX_XPOS    	!X limits of useable image

	INTEGER   MIN_YPOS, MAX_YPOS		!Y limits of useable image

	REAL   PSF_COEFS(4)			!fitted funtion coefficients

	INTEGER   RMAX				!max radius of source

	INTEGER   R_4SIG			!4 sigma radius

	INTEGER   STATUS			!status

	REAL   XPOS, YPOS			!source centroid

	REAL   SCALED_PSF			!scaled PSF function

	REAL   RPIX				!pixel radius from src centroid

	INTEGER   II, JJ			!loop variables

	INTEGER   IX_ST, IY_ST, IX_END, IY_END	!grid limits on image

	INTEGER   OLD_ROUTINE

	INCLUDE 'BSUB_CMN'
*
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=20
*
*  set up limits of PSF grid on image
*
	IX_ST=INT(XPOS+1)-INT(N_AZIM/2.)
	IX_END=INT(XPOS+1)+INT(N_AZIM/2.)
	IY_ST=INT(YPOS+1)-INT(N_ELEV/2.)
	IY_END=INT(YPOS+1)+INT(N_ELEV/2.)
*
*  go through PSF grid area and store scaled PSF values. Note that if the
*  pixel is within the radius where the source flux is more than 4 sigma
*  above the background in an annulus, then set the value to -100000.0
*  so that these pixels are completely ignored during the smoothing. This is
*  because close double sources or un-PSF-like sources may leave a poor
*  residual when the PSF is removed and contaminate the background level
*
	IX_ST=INT(XPOS+1)-RMAX-1		!reset to make circular
	IX_END=INT(XPOS+1)+RMAX+1
	IY_ST=INT(YPOS+1)-RMAX-1
	IY_END=INT(YPOS+1)+RMAX+1

	DO II=IX_ST,IX_END
	  DO JJ=IY_ST, IY_END

	    IF((II.GE.MIN_XPOS.AND.II.LE.MAX_XPOS).AND.
     +      (JJ.GE.MIN_YPOS.AND.JJ.LE.MAX_YPOS))THEN	!within image area?

	      RPIX=SQRT((II-0.5-XPOS)**2+(JJ-0.5-YPOS)**2)  !rad from src centre

	      IF(RPIX.LT.RMAX)THEN			!within PSF area?

dd	        SCALED_PSF=PSF_COEFS(4)*PSF_FUNC(II-IX_ST+1,JJ-IY_ST+1) !func

		SCALED_PSF=-100000.0			!excl whole source
*
*  now add current PSF function to source map. Must add because of possible
*  overlapping sources. Make sure that if the value has already been set
*  to -100000.0, it is not added to. Otherwise, the test for this value will
*  fail later in the program
*
		IF(SCALED_PSF.EQ.-100000.0)THEN
		  SRCMAP(II,JJ)=SCALED_PSF
		ELSEIF(SRCMAP(II,JJ).NE.-100000.0)THEN
		  SRCMAP(II,JJ)=SRCMAP(II,JJ)+SCALED_PSF
		END IF

	      END IF	!within PSF limit

	    END IF	!within image area

	  END DO	!over y dim
	END DO		!over x dim
*
	ROUTINE_ID=OLD_ROUTINE

	RETURN
	END
*
*+
*
*  routine to reset pixels in the SRCMAP to -100000 if there are insufficient
*  pixels in the image to get a reliable estimate of the background mean
*
	SUBROUTINE BSUB_UNRELIABLE(IMAGE_X,IMAGE_Y,SRCMAP,IBOX,
     +  BOX_LIMS,STATUS)

	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	INTEGER   IMAGE_X, IMAGE_Y		!image dimensions

	REAL   SRCMAP(IMAGE_X, IMAGE_Y)		!source map to update

	INTEGER   STATUS			!status

	INTEGER   II, JJ			!loop variables

	INTEGER   IBOX, BOX_LIMS(4,1000)	!box id and box limits array

	INTEGER   OLD_ROUTINE

	INCLUDE 'BSUB_CMN'
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=21
*
*  go through box and set all pixels in it to -1000000
*
	DO II=BOX_LIMS(1,IBOX),BOX_LIMS(2,IBOX)
	  DO JJ=BOX_LIMS(3,IBOX),BOX_LIMS(4,IBOX)

	    IF((II.GE.1.AND.II.LE.IMAGE_X).AND.
     +      (JJ.GE.1.AND.JJ.LE.IMAGE_Y))THEN	!within image area?

	       SRCMAP(II,JJ)=-100000.0		!excl whole BOX

	    END IF	!within image area

	  END DO	!over y dim
	END DO		!over x dim
*
	ROUTINE_ID=OLD_ROUTINE

	RETURN
	END

*
*+
*
*  routine to determine limits of non-zero input image
*
	SUBROUTINE BSUB_IMAGE_LIMITS(IMAGE_X,IMAGE_Y,IMAGE,COMP_X,COMP_Y,
     &  SRCMAP,IMAX,IMIN,MIN_XPOS,MAX_XPOS,MIN_YPOS,MAX_YPOS,STATUS)
*
	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	INTEGER   IMAGE_X, IMAGE_Y		!image dimensions

	REAL   IMAGE(IMAGE_X, IMAGE_Y)		!image

	INTEGER   COMP_X(IMAGE_X), COMP_Y(IMAGE_Y) !compression arrays

	REAL   SRCMAP(IMAGE_X, IMAGE_Y)		!source map

	INTEGER   IMAX				!max counts in image

	INTEGER   IMIN				!min cts in image

	INTEGER   MIN_XPOS, MAX_XPOS		!min and max X image limits

	INTEGER   MIN_YPOS, MAX_YPOS		!min and max Y image limits

	INTEGER   STATUS			!status

	LOGICAL FOUND_MINX, FOUND_MINY		!min position logicals

	INTEGER   I, J				!loop variables

	INTEGER   OLD_ROUTINE

	INCLUDE 'BSUB_CMN'
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=22
*
*  initialise arrays and imax
*
	IMAX=-100000000
	IMIN= 100000000

	DO I=1,IMAGE_X
	   COMP_X(I)=0
	END DO
	DO I=1,IMAGE_Y
	   COMP_Y(I)=0
	END DO
*
*  loop over image and compress

	DO I=1,IMAGE_Y
	  DO J=1,IMAGE_X

	    COMP_X(J)=COMP_X(J)+INT(IMAGE(J,I))
	    COMP_Y(I)=COMP_Y(I)+INT(IMAGE(J,I))

	    IMAX=MAX(IMAX,INT(IMAGE(J,I)) )
	    IMIN=MIN(IMIN,INT(IMAGE(J,I)) )
	    SRCMAP(J,I)=0.

	  END DO
	END DO
*
*  search to find first and last non-zero cells in compressed arrays. This
*  determines the limits of the non-zero image
*
	MIN_XPOS=1
	MAX_XPOS=IMAGE_X
	MIN_YPOS=1
	MAX_YPOS=IMAGE_Y
	FOUND_MINX=.FALSE.
	FOUND_MINY=.FALSE.

	DO I=1,IMAGE_X
	  IF(.NOT.FOUND_MINX.AND.COMP_X(I).NE.0)THEN
	    FOUND_MINX=.TRUE.
	    MIN_XPOS=I
	  ELSEIF(COMP_X(I).NE.0)THEN
	    MAX_XPOS=I
	  END IF
	END DO
	DO I=1,IMAGE_Y
	  IF(.NOT.FOUND_MINY.AND.COMP_Y(I).NE.0)THEN
	    FOUND_MINY=.TRUE.
	    MIN_YPOS=I
	  ELSEIF(COMP_Y(I).NE.0)THEN
	    MAX_YPOS=I
	  END IF
	END DO

	CALL MSG_PRNT(' ')
	CALL MSG_FMTI('XLOW','I4',MIN_XPOS)
	CALL MSG_FMTI('XHI','I4',MAX_XPOS)
	CALL MSG_FMTI('YLOW','I4',MIN_YPOS)
	CALL MSG_FMTI('YHI','I4',MAX_YPOS)
	CALL MSG_PRNT(' NON-ZERO image limits are ^XLOW : ^XHI
     + (in X) and ^YLOW : ^YHI (in Y) ')
*
	ROUTINE_ID=OLD_ROUTINE

	RETURN

	END


*+
*
*  subroutine to perform a fast smooth of the data in a supplied image
*
	SUBROUTINE BSUB_GAUSS_SMOOTH(DIMX,DIMY,IMAGE,WEIGHT,
     &  MIN_XPOS,MAX_XPOS,MIN_YPOS,MAX_YPOS,FWHM,SMOOTHED_IMAGE,
     &  MIN_CTS,MAX_CTS,SM_BORDER,WORK1,WORK2,WORK3,ROUTINE_ID,STATUS)
*
	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'

	INTEGER   DIMX, DIMY			!image dimensions

	REAL   IMAGE(DIMX,DIMY)			!image

	REAL   WEIGHT(DIMX,DIMY)		!weighting array

	REAL   SMOOTHED_IMAGE(DIMX,DIMY)	!smoothed output image

	REAL   MIN_CTS, MAX_CTS			!min/max cts in smoothed image

	REAL   FRONT_STRIP(2), BACK_STRIP(2)	!add/subtract strips

	REAL   TOP_STRIP(2), BOTTOM_STRIP(2)	!add/subtract strips

	REAL   WT_SUM, WT_CT_SUM		!wt and ct*wt summation vars

	REAL   LAST_WT_CT_SUM, LAST_WT_SUM	!old values

	REAL   LAST_ROW_WT_SUM, LAST_ROW_WT_CT_SUM	!for last row

	REAL   BORDER(2), WFX, WFY		!border cts and weight factor

	INTEGER   MIN_XPOS, MAX_XPOS		!non-zero image limits in X

	INTEGER   MIN_YPOS, MAX_YPOS		!       "                 Y

	INTEGER   I, J, IS, JS			!loop variables

	INTEGER   XSTART, XEND			!X box limits on image

	INTEGER   YSTART, YEND			!Y box limits on image

	INTEGER   HSIZE				!half smoothing box size

	LOGICAL SM_BORDER			!use weighted border

	INTEGER   IEDGE				!border width

	REAL   WORK1(DIMX,DIMY)

	REAL   WORK2(DIMX,DIMY)

	REAL   WORK3(DIMX,DIMY)

	INTEGER   GLIM

	REAL   FWHM

	REAL   SIG

	REAL   GFUNC(-500:500)

	INTEGER   K

	INTEGER   ROUTINE_ID, OLD_ROUTINE

	INTEGER   STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

	OLD_ROUTINE=ROUTINE_ID
	ROUTINE_ID=23

	MAX_CTS=0.
	MIN_CTS=1.E8
*
*  define the gaussian profile limit and evaluate the function
*
	SIG=FWHM/2.354
	GLIM=NINT(3*SIG)
	DO I=-GLIM,GLIM
	  GFUNC(I)=1./SQRT(2.*3.141592654*SIG*SIG)
	  GFUNC(I)=GFUNC(I)*EXP( - I*I/(2.*SIG*SIG) )
	END DO
*
*  loop over all pixels, smoothing in X direction
*
	DO I=MIN_YPOS,MAX_YPOS

	  DO J=MIN_XPOS,MAX_XPOS		!in x dirn

	     WORK1(J,I)=0.
	     WORK2(J,I)=0.
*
*  define limits of smoothing function on image
*
	     XSTART=J-GLIM	!define limits of smoothing box
	     XEND=J+GLIM	!as before
*
* now convolve image data in this strip with linear smoothing function
*
	     DO K=XSTART,XEND

		IF(K.GE.MIN_XPOS.AND.K.LE.MAX_XPOS)THEN

		  WORK1(J,I)=WORK1(J,I)+IMAGE(K,I)*WEIGHT(K,I)*GFUNC(K-J)
		  WORK2(J,I)=WORK2(J,I)+WEIGHT(K,I)*GFUNC(K-J)


		END IF


	     END DO

	  END DO

	END DO
*
*  repeat now in Y direction
*
	DO J=MIN_XPOS,MAX_XPOS

	  DO I=MIN_YPOS,MAX_YPOS		!in x dirn
*
*  define limits of smoothing function on image
*
	     SMOOTHED_IMAGE(J,I)=0.
	     WORK3(J,I)=0.

	     YSTART=I-GLIM	!define limits of smoothing box
	     YEND=I+GLIM	!as before
*
* now in Y dirn, convolve IMAGE SMOOTHED IN 1-D WITH smoothing function
*
	     DO K=YSTART,YEND

		IF(K.GE.MIN_YPOS.AND.K.LE.MAX_YPOS)THEN

		  SMOOTHED_IMAGE(J,I)=SMOOTHED_IMAGE(J,I) +
     &		  WORK1(J,K)*GFUNC(K-I)
		  WORK3(J,I)=WORK3(J,I)+ WORK2(J,K)*GFUNC(K-I)

		END IF

	     END DO

	     IF(WORK3(J,I).GT.0.0)SMOOTHED_IMAGE(J,I)=
     &       SMOOTHED_IMAGE(J,I)/WORK3(J,I)

	     MIN_CTS=MIN(MIN_CTS,SMOOTHED_IMAGE(J,I))
	     MAX_CTS=MAX(MAX_CTS,SMOOTHED_IMAGE(J,I))

	  END DO

	END DO

	ROUTINE_ID=OLD_ROUTINE

	RETURN
	END
*
*+
* routine to find sloping edges in the non zero image
*
	SUBROUTINE FIND_SLOPING_EDGES(IMAGE_X,IMAGE_Y,IMAGE,
     &  MIN_XPOS,MAX_XPOS,MIN_YPOS,MAX_YPOS,QUALITY,MAX_DIM1,
     &  MAX_DIM2,EDGES,STATUS)
*
	IMPLICIT NONE

	INCLUDE 'QUAL_PAR'

	INTEGER IMAGE_X, IMAGE_Y

	REAL IMAGE(IMAGE_X,IMAGE_Y)

	INTEGER MIN_XPOS, MAX_XPOS

	INTEGER MIN_YPOS, MAX_YPOS

	BYTE QUALITY(IMAGE_X,IMAGE_Y)

	INTEGER MAX_DIM1, MAX_DIM2

	INTEGER EDGES(MAX_DIM1,MAX_DIM2)

	INTEGER STATUS

	INTEGER I, J

	INTEGER NN0
*
	REAL SIGMA(2)

	REAL A(2), B(2)

	INTEGER START, END

	logical OK(2)
*
*-
*
*  first fill everywhere outside the main non-zero image with bad quality (0)
*  and set area within the non-zero image to good quality (1)
*
	DO J=1,IMAGE_Y
	  DO I=1,IMAGE_X

	    QUALITY(I,J)=QUAL__IGNORE 	!everything bad

	    IF((I.GE.MIN_XPOS.AND.I.LE.MAX_XPOS).AND.
     &      (J.GE.MIN_YPOS.AND.J.LE.MAX_YPOS))QUALITY(I,J)=QUAL__GOOD	!inside good

	  END DO
	END DO
*
*  Now in X, step along each pixel row to find first and last non zero pixel
*
	DO J=MIN_YPOS,MAX_YPOS

	  NN0=0

	  DO I=MIN_XPOS,MAX_XPOS

	     IF(IMAGE(I,J).NE.0)THEN

		NN0=NN0+1
		IF(NN0.EQ.1)THEN
		  EDGES(J,1)=I
		END IF
		EDGES(J,2)=I
	      END IF

	  END DO
	END DO
*
*  determine the best linear fit through these data for near xside
*
	CALL GET_SLOPE(MAX_DIM1,MAX_DIM2,EDGES,MIN_YPOS,MAX_YPOS,
     &  A,B,SIGMA,1,OK(1),STATUS)
*
*  and for far xside
*
	CALL GET_SLOPE(MAX_DIM1,MAX_DIM2,EDGES,MIN_YPOS,MAX_YPOS,
     &  A,B,SIGMA,2,OK(2),STATUS)
*
*  now fill quality array such that points outside the limits of the sloping
*  lines (-/+ one sigma) are set bad
*
	DO J=MIN_YPOS,MAX_YPOS

	  END=MIN_XPOS
	  IF(OK(1))THEN
	    END=B(1)+A(1)*J-2*SIGMA(1)-1
	    IF(END.LE.MIN_XPOS)END=MIN_XPOS
	  END IF

	  START=MAX_XPOS
	  IF(OK(2))THEN
	    START=B(2)+A(2)*J+2*SIGMA(2)+1
	    IF(START.GE.MAX_XPOS)START=MAX_XPOS
	  END IF

	  DO I=MIN_XPOS,END
	     IF(I.GT.MIN_XPOS)QUALITY(I,J)=QUAL__IGNORE
	  END DO
	  DO I=START,MAX_XPOS
	     IF(I.GT.START)QUALITY(I,J)=QUAL__IGNORE
	  END DO

	END DO
*
	RETURN
	END
*
*+
*  routine to compute slope parameters for fit to edge of image
*
	SUBROUTINE GET_SLOPE(MAX_DIM1,MAX_DIM2,EDGES,L1,L2,A,B,SIGMA,
     &  IEDGE,OK,STATUS)

	IMPLICIT NONE

	INTEGER MAX_DIM1, MAX_DIM2

	INTEGER EDGES(MAX_DIM1,MAX_DIM2)

	INTEGER L1, L2

	REAL A(2), B(2)

	REAL SIGMA(2)

	INTEGER IEDGE

	LOGICAL OK

	INTEGER STATUS
*
	INTEGER N

	REAL SUMY, SUMY2, YBAR2

	REAL SUMX, SUMX2

	REAL SUMXY

	INTEGER I
*
*-
*
*  initialise summation variables
*
	OK=.FALSE.

	YBAR2=0.
	SUMX=0.
	SUMX2=0.
	SUMY=0.
	SUMY2=0.
	SUMXY=0.
	N=0
*
* perform necessary summations
*
	DO I=L1,L2

	  N=N+1

	  SUMX=SUMX + I
	  SUMX2=SUMX2 + I*I

	  SUMY=SUMY + EDGES(I,IEDGE)
	  SUMY2=SUMY2 + EDGES(I,IEDGE)*EDGES(I,IEDGE)

	  SUMXY=SUMXY + I*EDGES(I,IEDGE)

	END DO
*
*  and compute resulting slope parameters
*
	A(IEDGE)=0.0
	B(IEDGE)=0.0
	SIGMA(IEDGE)=0.0

	IF(N*SUMX2-SUMX*SUMX.NE.0.0)THEN

	  OK=.TRUE.

	  A(IEDGE)=(N*SUMXY-SUMX*SUMY)/(N*SUMX2-SUMX*SUMX)
	  B(IEDGE)=(SUMX2*SUMY-SUMX*SUMXY)/(N*SUMX2-SUMX*SUMX)

	  YBAR2=(SUMY/N)*(SUMY/N)
	  SIGMA(IEDGE)=SQRT((SUMY2/N)-YBAR2)

	END IF

	RETURN
	END
