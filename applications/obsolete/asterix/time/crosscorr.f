*+  CROSSCORR - Computes cross-correlation of two equal length series
      SUBROUTINE CROSSCORR(STATUS)
*    Description :
*
*     Calls subroutine TIM_XCOR to calculate the cross-correlation of two series
*     Y and Z which may both come from the same file or from different files.
*     If the series are not of equal length then the longer is truncated.
*
*    Environment parameters :
*
*     INP1=UNIV(R)
*            first input object
*     INP2=UNIV(R)
*            second input object
*     LAG=INTEGER(R)
*            maximum lag to be computed
*     WEIGHTED=LOGICAL(R)
*            cross-correlation to be weighted?
*     NOISE=LOGICAL(R)
*            remove expected noise bias?
*     OUT=UNIV(W)
*            output object
*
*    Method :
*
*     The autocorrelation calculated is the biased version, which has lower
*     variance than the unbiased estimator. It reduces towards zero as the
*     lag increases.
*     Data weights may be taken into account, and the noise contribution
*     can be removed from the denominator of the autocorrelation - see e.g.
*     Weisskopf et al, Ap.J.199, L147.
*
*    Deficiencies :
*     The FFT is not used; this involves a time penalty for large data sets.
*
*    Bugs :
*    Authors :
*
*     Trevor Ponman  (BHVAD::TJP)
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      4 Apr 84 : Original
*     29 Aug 86 : graphics removed (JCMP)
*     11 Jun 87 : restructured, COMMENT removed (pla@uk.ac.bham.sr.star)
*     24 Sep 88 : V1.0-1 ASTERIX88 conversion (TJP)
*     13 Dec 88 : V1.0-2 Rationalised to use standard subroutines etc. (TJP)
*
*     13 Jun 90 : V1.2-0 UTIL_NBAD removed (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*    Structure definitions :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) YLOC            ! First data object locator
      CHARACTER*(DAT__SZLOC) ZLOC            ! Second data object locator
      CHARACTER*(DAT__SZLOC) ILOC            ! Locator of 1st non-prim input
      CHARACTER*(DAT__SZLOC) XCLOC           ! Locator to output object
      CHARACTER*(DAT__SZLOC) HRLOC           ! Locator to o/p history record
      CHARACTER*(DAT__SZTYP) YOBTYPE         ! Type of first data object
      CHARACTER*(DAT__SZTYP) ZOBTYPE         ! Type of second data object
      CHARACTER*80           LUNITS          ! Lag units
      CHARACTER*80           YFILE,ZFILE     ! Input files
      CHARACTER*80           YPATH,ZPATH     ! Input paths (within file)
      CHARACTER*80           STRING          ! Text string
      CHARACTER*80           TEXT(10)        ! History text

      LOGICAL                ANYBAD          ! Any bad quality points at all?
      LOGICAL                DENOISE         ! Noise variance removed?
      LOGICAL                LOG
      LOGICAL                OK              ! Present & correct?
      LOGICAL                VARS            ! Variances available?
      LOGICAL                WEIGHT          ! Weighted cross-correlation?
      LOGICAL                YPRIM           ! Primitive input array 1?
      LOGICAL                YREG            ! Y spacing known to be regular?
      LOGICAL                ZPRIM           ! Primitive input array 2?
      LOGICAL                ZREG            ! Z spacing known to be regular?

      INTEGER                I
      INTEGER                YDPTR           ! Pointer to Y data array
      INTEGER                ZDPTR           ! Pointer to Z data array
      INTEGER                YVPTR           ! Pointer to Y variance array
      INTEGER                YQPTR           ! Pointer to Y quality array
      INTEGER                ZVPTR           ! Pointer to Z variance array
      INTEGER                ZQPTR           ! Pointer to Z quality array
      INTEGER                XCPTR           ! Pointer to cross-correln values
      INTEGER                LAGPTR          ! Pointer to array of lags
      INTEGER                NY              ! No. of points in Y array
      INTEGER                NZ              ! No. of points in Z array
      INTEGER                ND              ! No. of data points used
      INTEGER                LMAX            ! Maximum lag to be computed
      INTEGER                NL              ! Total number of lag values
      INTEGER                NDIM            ! Dimensionality of data
      INTEGER                NBAD            ! No.of bad quality data
      INTEGER                NVAL            ! No.of values
      INTEGER                NMAP            ! No.of values mapped
      INTEGER                BASEFILE        ! File # from which MORE/HIST taken
      INTEGER                QNDIM           ! Dimensionality of quality
      INTEGER                QDIM(DAT__MXDIM)! Quality dimensions
      INTEGER                NLINE           ! Line no. of HISTORY text
      INTEGER                TEXTLEN         ! Length of text string
      INTEGER                IDIM(DAT__MXDIM)! Size of each dimension

      REAL                   BASE            ! Axis base value
      REAL                   SCALE           ! Axis scale value
      REAL                   YSCALE          ! Axis scale value for Y
      REAL                   ZSCALE          ! Axis scale value for Z
      REAL                   VMIN,VMAX       ! Min and max variance values
*    Local data :
*    Version :
      CHARACTER*30 VERSION
         PARAMETER (VERSION = 'CROSSCORR Version 1.2-0')
*-

*    Announce version
      CALL MSG_PRNT(VERSION)

*    Initialise ASTERIX packages
      CALL AST_INIT

*    Obtain data objects
      CALL USI_ASSOCI( 'INP1', 'READ' , YLOC , YPRIM , STATUS )
      CALL USI_ASSOCI( 'INP2', 'READ' , ZLOC , ZPRIM , STATUS )
      IF(STATUS.NE.SAI__OK) GO TO 9000

*    Find types and inform user
      CALL DAT_TYPE( YLOC, YOBTYPE, STATUS )
      IF(STATUS.EQ.SAI__OK)THEN
        CALL MSG_SETC( 'OTYPE1', YOBTYPE )
        CALL DAT_TYPE( ZLOC, ZOBTYPE, STATUS )

        IF ( YOBTYPE .EQ. ZOBTYPE ) THEN
          CALL MSG_PRNT( 'Both objects are of type ^OTYPE1')
        ELSE
          CALL MSG_PRNT( '$OBJ1 is of type ^OTYPE1')
          CALL MSG_SETC( 'OTYPE2', ZOBTYPE )
          CALL MSG_PRNT( '$OBJ2 is of type ^OTYPE2')
        ENDIF
      ELSE
        CALL ERR_FLUSH(STATUS)
      ENDIF

*    Check data, map if present and 1-dimensional
      CALL BDA_CHKDATA(YLOC,OK,NDIM,IDIM,STATUS)
D     print *,'y;ok,ndim,idim,status:',ok,ndim,idim,status
      IF(.NOT.OK)THEN
        CALL ERR_REP('BADDAT','Invalid data in dataset 1',STATUS)
        STATUS=SAI__ERROR
      ELSE
        IF(NDIM.NE.1)THEN
          CALL ERR_REP( 'NOT_1DY', '$OBJ1 data are not one-dimensional',
     :    STATUS )
          STATUS=SAI__ERROR
        ELSE
          CALL BDA_MAPDATA(YLOC,'READ',YDPTR,STATUS)
          NY=IDIM(1)
        ENDIF
      ENDIF
      IF(STATUS.NE.SAI__OK) GO TO 9000

      CALL BDA_CHKDATA(ZLOC,OK,NDIM,IDIM,STATUS)
D     print *,'z;ok,ndim,idim,status:',ok,ndim,idim,status
      IF(.NOT.OK)THEN
        CALL ERR_REP('BADDAT','Invalid data in dataset 2',STATUS)
        STATUS=SAI__ERROR
      ELSE
        IF(NDIM.NE.1)THEN
          CALL ERR_REP( 'NOT_1DZ', '$OBJ2 data are not one-dimensional',
     :    STATUS )
          STATUS=SAI__ERROR
        ELSE
          CALL BDA_MAPDATA(ZLOC,'READ',ZDPTR,STATUS)
          NZ=IDIM(1)
        ENDIF
      ENDIF
      IF(STATUS.NE.SAI__OK) GO TO 9000

*    Check for regular axis spacing
      YREG=.FALSE.
      IF(.NOT.YPRIM)THEN
        CALL BDA_CHKAXVAL(YLOC,1,OK,YREG,NVAL,STATUS)
      ENDIF
      IF (.NOT.YREG) THEN
        CALL MSG_PRNT( 'Assuming $OBJ1 regularly spaced')
      END IF
      IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)

      ZREG=.FALSE.
      IF(.NOT.ZPRIM)THEN
        CALL BDA_CHKAXVAL(ZLOC,1,OK,ZREG,NVAL,STATUS)
      ENDIF
      IF(.NOT.ZREG) THEN
        CALL MSG_PRNT( 'Assuming $OBJ2 regularly spaced')
      END IF
      IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)

*    Get axis spacing if available
      IF(YREG)THEN
        CALL BDA_GETAXVAL(YLOC,1,BASE,YSCALE,I,STATUS)
      ENDIF
      IF(ZREG)THEN
        CALL BDA_GETAXVAL(ZLOC,1,BASE,ZSCALE,I,STATUS)
      ENDIF
      IF(YREG.AND.ZREG)THEN
        IF(ABS(YSCALE-ZSCALE)/YSCALE.LT.0.0001)THEN
          SCALE=YSCALE
        ELSE
          CALL ERR_REP('BADSCALES','The two datasets have different'//
     :    ' axis spacing',STATUS)
          STATUS=SAI__ERROR
        ENDIF
      ELSE IF(YREG)THEN
        SCALE=YSCALE
      ELSE IF(ZREG)THEN
        SCALE=ZSCALE
      ELSE
        SCALE=1.0
        CALL MSG_PRNT( 'Adopting unit axis spacing')
      ENDIF

*    If one array is larger than the other then truncate it
      IF (NY.GT.NZ) THEN
        CALL MSG_PRNT('$OBJ1 array truncated for use')
        ND = NZ
      ELSE IF (NZ.GT.NY) THEN
        CALL MSG_PRNT('$OBJ2 array truncated for use')
        ND=NY
      ELSE
        ND=NY
      END IF
      IF(ND.GT.1)THEN
        CALL MSG_SETI( 'NDAT', ND )
        CALL MSG_PRNT( 'Using ^NDAT data points')
      ELSE
        CALL ERR_REP('TOO_SHORT','Insufficient data to cross-correlate',
     :  STATUS)
        STATUS=SAI__ERROR
        GO TO 9000
      ENDIF

*    User input
      CALL USI_DEF0I( 'LAG',ND-1,STATUS)
      CALL USI_GET0I( 'LAG', LMAX, STATUS )
      IF(STATUS.NE.SAI__OK) GO TO 9000
      IF(LMAX.GT.ND-1)THEN
        LMAX=ND-1
        CALL MSG_SETI('LMAX',LMAX)
        CALL MSG_PRNT('Maximum lag possible is ^LMAX')
      ENDIF

*    Check data variances - only allow weighting if all variances are available
      IF((.NOT.YPRIM).AND.(.NOT.ZPRIM))THEN
        CALL BDA_CHKVAR(YLOC,VARS,NDIM,IDIM,STATUS)
        IF(VARS)THEN
          IF(NDIM.NE.1.OR.IDIM(1).NE.NY)THEN
            CALL MSG_PRNT('WARNING: $OBJ1 variance array is'
     :      //' wrong size. Will proceed without weighting.')
            VARS=.FALSE.
          ELSE
            CALL BDA_CHKVAR(ZLOC,OK,NDIM,IDIM,STATUS)
            IF(OK)THEN
              IF(NDIM.NE.1.OR.IDIM(1).NE.NZ)THEN
                CALL MSG_PRNT('WARNING: $OBJ2 variance array'
     :          //' is wrong size. Will proceed without weighting.')
                VARS=.FALSE.
              ENDIF
            ENDIF
          ENDIF
        ENDIF

*      Weighting &/or noise correction required?
        IF(VARS)THEN
          CALL USI_GET0L( 'WEIGHTED', WEIGHT, STATUS )
          CALL USI_GET0L( 'NOISE', DENOISE, STATUS )
        ENDIF
        IF(STATUS.NE.SAI__OK) GO TO 9000

*      Map variances
        IF(WEIGHT.OR.DENOISE)THEN
          CALL BDA_MAPVAR(YLOC,'READ',YVPTR,STATUS)
          CALL BDA_MAPVAR(ZLOC,'READ',ZVPTR,STATUS)
        ENDIF
        IF(STATUS.NE.SAI__OK)THEN
          CALL ERR_REP('BAD_VAR','Failed to map variances - no '//
     :    'weighting or noise correction performed',STATUS)
          CALL ERR_FLUSH(STATUS)
          WEIGHT=.FALSE.
          DENOISE=.FALSE.
        ENDIF

*      Check data quality
        CALL BDA_CHKQUAL( YLOC, OK, QNDIM, QDIM, STATUS )
        IF ( OK ) THEN
           CALL BDA_MAPLQUAL( YLOC, 'READ', ANYBAD, YQPTR, STATUS )
           IF ( ANYBAD ) THEN
              CALL ARR_NBAD( ND, %VAL(YQPTR), NBAD, STATUS )
              CALL MSG_SETI( 'NBAD', NBAD )
              CALL MSG_PRNT('WARNING: ^NBAD bad data points present'/
     :                                                  /' in $OBJ1')
           END IF
           CALL BDA_UNMAPLQUAL( YLOC, STATUS )
        END IF
        CALL BDA_CHKQUAL( ZLOC, OK, QNDIM, QDIM, STATUS )
        IF ( OK ) THEN
           CALL BDA_MAPLQUAL( ZLOC, 'READ', ANYBAD, ZQPTR, STATUS )
           IF ( ANYBAD ) THEN
              CALL ARR_NBAD( ND, %VAL(ZQPTR), NBAD, STATUS )
              CALL MSG_SETI( 'NBAD', NBAD )
              CALL MSG_PRNT('WARNING: ^NBAD bad data points present'/
     :                                                  /' in $OBJ2')
           END IF
           CALL BDA_UNMAPLQUAL( ZLOC, STATUS )
        END IF
      END IF

*    If variances are to be used check that they are all >0
      IF(WEIGHT.OR.DENOISE)THEN
        CALL ARR_RANG1R(ND,%VAL(YVPTR),VMIN,VMAX,STATUS)
        IF(VMIN.LE.0.0)THEN
          CALL MSG_PRNT('Non-positive variance values in '//
     :    '$OBJ1 - proceeding without weighting')
          WEIGHT=.FALSE.
          DENOISE=.FALSE.
        ELSE
          CALL ARR_RANG1R(ND,%VAL(ZVPTR),VMIN,VMAX,STATUS)
          IF(VMIN.LE.0.0)THEN
            CALL MSG_PRNT('Non-positive variance values in '//
     :      '$OBJ2 - proceeding without weighting')
            WEIGHT=.FALSE.
            DENOISE=.FALSE.
          ENDIF
        ENDIF
      ENDIF

*    Create a cross-correlation object
      CALL USI_DCREAT( 'OUT', 'CROSS_CORR', 0, 0, STATUS )
      CALL USI_DASSOC( 'OUT', 'WRITE', XCLOC, STATUS )
      IF(STATUS.NE.SAI__OK) GO TO 9000

*    Create principal arrays
      NL = 2*LMAX + 1
      NDIM=1
      IDIM(1)=NL
      CALL BDA_CREBDS(XCLOC,NDIM,IDIM,.TRUE.,.FALSE.,.FALSE.,STATUS)

*    Map output data
      CALL BDA_MAPDATA(XCLOC,'WRITE',XCPTR,STATUS)
      IF(STATUS.NE.SAI__OK) GO TO 9000

*    Compute cross-correlation
      CALL TIM_XCOR(ND,%VAL(YDPTR),%VAL(ZDPTR),%VAL(YVPTR),%VAL(ZVPTR),
     :WEIGHT,DENOISE,NL,%VAL(XCPTR))

*    Enter lag values
      CALL BDA_PUTAXVAL(XCLOC,1,-LMAX*SCALE,SCALE,NL,STATUS)

*    Copy ancillary information from first non-primitive reg. spaced input file
      IF(YREG)THEN
        CALL DAT_CLONE(YLOC,ILOC,STATUS)
        BASEFILE=1
      ELSE IF(ZREG)THEN
        CALL DAT_CLONE(ZLOC,ILOC,STATUS)
        BASEFILE=2
      ELSE IF(.NOT.YPRIM)THEN
        CALL DAT_CLONE(YLOC,ILOC,STATUS)
        BASEFILE=1
      ELSE IF(.NOT.ZPRIM)THEN
        CALL DAT_CLONE(ZLOC,ILOC,STATUS)
        BASEFILE=2
      ENDIF
D     print *,'yprim,zprim,yreg,zreg: ',yprim,zprim,yreg,zreg
D     print *,'basefile: ',basefile

*    Create or copy data and axis ancillaries
      CALL BDA_PUTLABEL(XCLOC,'Cross-correlation',STATUS)
      CALL BDA_PUTAXLABEL(XCLOC,1,'Lag',STATUS)
      CALL BDA_PUTAXWID(XCLOC,1,SCALE,STATUS)
      IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)
      IF(.NOT.(YPRIM.AND.ZPRIM))THEN
        CALL BDA_GETAXUNITS(ILOC,1,STRING,STATUS)
        CALL BDA_PUTAXUNITS(XCLOC,1,STRING,STATUS)
        IF(STATUS.NE.SAI__OK) CALL ERR_ANNUL(STATUS)

*    Copy MORE
        CALL HDX_CCOPY(ILOC,XCLOC,'MORE',STATUS)
        IF(STATUS.NE.SAI__OK) CALL ERR_ANNUL(STATUS)

* History entry (copy history file from ILOC, if available)
        CALL HIST_OK(ILOC,OK,STATUS)
        IF(OK)THEN
          CALL HIST_COPY(ILOC,XCLOC,STATUS)
        ELSE
          BASEFILE=0
        ENDIF
        IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)
      ENDIF
      CALL HIST_ADD(XCLOC,VERSION,STATUS)
      CALL USI_NAMEI(NLINE,TEXT,STATUS)
      IF(BASEFILE.GT.0)THEN
        NLINE=NLINE+1
        CALL MSG_SETI('BF',BASEFILE)
        CALL MSG_MAKE('NOTE: Above HISTORY records refer to '//
     :  'dataset ^BF',TEXT(NLINE),TEXTLEN)
      ENDIF
      IF(WEIGHT)THEN
        STRING='Weighted cross-correlation'
      ELSE
        STRING='Unweighted cross-correlation'
      ENDIF
      NLINE=NLINE+1
      IF(DENOISE)THEN
        TEXT(NLINE)=STRING(1:CHR_LEN(STRING))//'    Noise '//
     :  'contribution removed from denominator'
      ELSE
        TEXT(NLINE)=STRING(1:CHR_LEN(STRING))//'    No noise correction'
      ENDIF
D     print *,'History text: ',text
      CALL HIST_PTXT(XCLOC,NLINE,TEXT,STATUS)

* Tidy up
      CALL AST_CLOSE(STATUS)

* Exit
 9000 CONTINUE

      END
