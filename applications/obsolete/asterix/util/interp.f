*+  INTERP - Spline interpolation on bad datapoints.
      SUBROUTINE INTERP(STATUS)
*
*    Description :
*
*       Takes an n-dimensional array of data and fits a spline to points
*       which are of "good" quality.
*       The "bad" points are then reconstituted using the spline coefficients.
*       The quality of these points is then set to the PATCHED value and
*       the badbits mask set such that patched values will be treated as
*       good by subsequent applications.
*       The errors are set to the average variance of the "Good" points
*       in that particular slice of data.
*
*    Restrictions :
*    Parameters :
*
*      OVER        _LOGICAL       Overwrite input file ?
*      INPUT       _CHAR          Name of the input file
*      OUTPUT      _CHAR          Name of the output file
*      KWIDTH      _CHAR          The number of bins between each knot.
*
*    Method :
*
*         Uses the NAG routines E02BAF and E02BBF.
*    Deficiencies :
*         Cannot handle data with dimensions higher than four.
*    Bugs :
*    Authors :
*     Richard Saxton
*    History :
*
*      5 May 88 : V1.0-2 Original (LTVAD::RDS)
*     28 Feb 94 : V1.7-0 Quality handling updated (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     13 Apr 95 : V1.8-1 New data interfaces (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      BYTE			BIT_ANDUB
      BYTE			BIT_NOTUB
*
*    Local Constants :
*
      INTEGER MAXDIM,MAXKNOT
      PARAMETER (MAXDIM=4)
      PARAMETER (MAXKNOT=500)                   !Maximum number of knots
*
*    Local variables :
*
      INTEGER NDIM                              !Number of dims. of the data
      INTEGER DIM(MAXDIM)                       !Dimensions of the input data
      INTEGER TDIM(MAXDIM)                      !Dimensions of the temp arrays
      INTEGER NQDIM                             !Number of dims. of quality
      INTEGER QDIM(MAXDIM)                      !Dimensions of the quality array
      INTEGER NVDIM                             !Number of dims. of the variance
      INTEGER VDIM(MAXDIM)                      !Dimensions of the variance
      INTEGER LP
      CHARACTER*20 LABEL(MAXDIM)
      LOGICAL LDARRAY,LDQUAL,LDVAR              !Are the data,quality,variance
*                                               !   arrays present ?
      REAL START(MAXDIM),STOP(MAXDIM)           !Extreme values of each axis
      REAL WIDTH(MAXDIM)                        !Width of bins in each axis
      INTEGER APP_DIM                           !Axis to fit and interpolate.
      INTEGER ORDER(MAXDIM)                     !Order of the axes in the
*                                               ! temporary arrays relative to
*                                               ! the original arrays.
      INTEGER COUNT                             !
      INTEGER TDATA_PNTR,TQUAL_PNTR,TVAR_PNTR   !Pointers to temp arrays.
      INTEGER D_PNTR,Q_PNTR,V_PNTR              !Pointers to data,quality and
*                                               ! variance arrays.
      INTEGER GOODX_PNTR                        !Pntr to "good" x values array
      INTEGER GOODY_PNTR                        !Pntr to corresponding counts
      INTEGER BADX_PNTR                         !Pntr to "bad" x values array
      INTEGER BADY_PNTR                         !Pntr to "bad" counts array
      INTEGER GOODBIN_PNTR                      !Pntr to "good" bin values
      INTEGER BADBIN_PNTR                       !Pntr to "bad" bin values
      INTEGER NGOOD,NBAD                        !No. of good and bad points.
      INTEGER WEIGHT_PNTR                       !Pntr to weights array
      INTEGER WORK_PNTR                         !Pntr to work space
      INTEGER XD_PNTR                           !Pntr to work space
      INTEGER YD_PNTR                           !Pntr to work space
      INTEGER AXPNTR(4)                         !Pntr to each axis array
      LOGICAL JUMPOUT
      INTEGER KWIDTH                            !No. of bins between knots
      REAL    RWIDTH                            !Distance in X between knots.
      INTEGER LP2,LP3,LP4                       !Define slice of arrays
      INTEGER N7                                !No. of intervals of spline + 7
      DOUBLE PRECISION KNOT(MAXKNOT)            !Knot positions
      DOUBLE PRECISION COEFF(MAXKNOT)           !Spline coefficients
      INTEGER SS                                !Residue of sum squares of fit.
      REAL AVGE                                 !Average value of variance of
*                                               ! the good pnts in a slice.
      INTEGER AMAX(MAXDIM),AMIN(MAXDIM)         !Extreme values of each dimens.
      BYTE BADBITS                              !Mask for quality checking
      INTEGER FIXTOT                            !Total no of interpolated pnts.
      LOGICAL LAX                               !Is axis info present
      LOGICAL LREG                              !Is axis regularly spaced

      CHARACTER*80 PATH(5)                      !Input data path
      CHARACTER*6 TOTCHAR                       !No pixels replaced (char form)
      CHARACTER*2 CLP                           !Loop number as a character

      INTEGER			IFID			! I/p dataset id
      INTEGER 			IDUM
      INTEGER 			NCHAR
      INTEGER 			NFAIL                   ! # failures in SPLREC
      INTEGER 			NLINES                  ! No of lines of text
      INTEGER			OFID			! O/p dataset id

      LOGICAL 			OVER                    ! Overwrite input file?

      BYTE 			MASKOUT                 ! O/p quality mask

*  Version:
      CHARACTER*30		VERSION
	PARAMETER		( VERSION = 'INTERP Version 1.8-1' )
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Announce version
      CALL MSG_PRNT( VERSION )

*  Initialise the Asterix common blocks.
      CALL AST_INIT

*  Set data dimensions to one.
      DO LP = 1, 4
        DIM(LP) = 1
        TDIM(LP) = 1
      END DO

*  Ask for input and output filename
*  Should the input file be overwritten ?
      CALL USI_GET0L('OVER',OVER,STATUS)
*
      IF (OVER) THEN
*
         CALL USI_TASSOCI('INP','*','UPDATE',ILOC,STATUS)
*
*   Clone an output locator
         CALL ADI_CLONE( IFID, OFID, STATUS )
*
      ELSE
*
         CALL USI_TASSOC2('INP','OUT','READ',IFID, OFID, STATUS )

*   Copy all components from old file into new file
         CALL ADI_FCOPY(IFID,OFID,STATUS)
*
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 99
*
* Trace path of input data.
      CALL USI_NAMEI(NLINES,PATH,STATUS)
*
* Check components in this file and get dimensions
*
      CALL BDI_CHKDATA(OFID,LDARRAY,NDIM,DIM,STATUS)
      CALL BDI_CHKQUAL(OFID,LDQUAL,NQDIM,QDIM,STATUS)
      CALL BDI_CHKVAR (OFID,LDVAR,NVDIM,VDIM,STATUS)
*
      IF (.NOT.LDARRAY) THEN
          STATUS=SAI__ERROR
          CALL ERR_REP(' ','No data array in this file',STATUS)
      ENDIF
      IF (.NOT.LDQUAL) THEN
          STATUS=SAI__ERROR
          CALL ERR_REP(' ','No quality array in this file',STATUS)
      ENDIF
*
* Check shape of data
      IF (NDIM .GT. 4) THEN
          STATUS=SAI__ERROR
          CALL ERR_REP(' ','Data array has more than four dimensions',
     :                     ' it will have to be binned up *',STATUS)
      ENDIF
*
      IF (STATUS.NE.SAI__OK) GOTO 99
*
*  Go thru each axis in turn.
      DO LP=1,NDIM
*
         CALL BDI_CHKAXVAL(OFID,LP,LAX,LREG,DIM(LP),STATUS)
*
* If axis info present
         IF (LAX) THEN
*
*  Map the axis array
           CALL BDI_MAPAXVAL(OFID,'READ',LP,AXPNTR(LP),STATUS)
*
*  Get label
           CALL BDI_GETAXLABEL(OFID,LP,LABEL(LP),STATUS)
*
           IF (STATUS .NE. SAI__OK) THEN
              CALL MSG_PRNT('Error getting axis values')
              GOTO 99
           ENDIF
*
*  Calculate MAX and MIN axis values
           CALL ARR_RANG1R( DIM(LP),%VAL(AXPNTR(LP)),START(LP),
     :                      STOP(LP),STATUS)
*
* If axis values not present take the pixel numbers.
         ELSE
*
           CALL MSG_SETI( 'AX', LP )
           CALL MSG_PRNT( ' Values for axis ^AX not present, choose'//
     :                    ' range in pixels' )
           START(LP)=1.0
           STOP(LP)=REAL(DIM(LP))
*
*  Produce label for this axis
           CALL CHR_ITOC(LP,CLP,IDUM)
           LABEL(LP) = 'Dimension ' // CLP
*
         ENDIF
*
*  Calculate the width of each pixel
         WIDTH(LP) = (STOP(LP)-START(LP)) / REAL(DIM(LP)-1)
*
      ENDDO
*
      IF (STATUS .NE. SAI__OK) GOTO 99
*
* Set mins and maxs to 1
      DO LP=1,4
         AMIN(LP)=1
         AMAX(LP)=1
      ENDDO

*  Ask the user which ranges are required.
      CALL USI_RANGES( NDIM,LABEL,START,STOP,WIDTH,APP_DIM,
     :                            AMIN,AMAX,STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Want to create a temporary array containing the subset of the data array
*  given by the user limits AMIN and AMAX.
*  Order this array so that the axis being fitted APP_DIM comes first,
*  for efficiency.
      ORDER(1)=APP_DIM
      COUNT=2
      DO LP=1,4
         IF (LP .NE. APP_DIM) THEN
             ORDER(COUNT)=LP
             COUNT=COUNT+1
         END IF
      END DO

*  Find the dimensions of this new array. Dimensions between NDIM and 4 will
*  have been set to 1 at the start of this code.
      DO LP=1,NDIM
         TDIM(LP)=AMAX(ORDER(LP)) - AMIN(ORDER(LP)) + 1
      ENDDO

*  Create temporary data,variance,quality arrays to hold the data subset
      CALL DYN_MAPR(4,TDIM,TDATA_PNTR,STATUS)
      CALL DYN_MAPB(4,TDIM,TQUAL_PNTR,STATUS)
      IF (LDVAR) CALL DYN_MAPR(4,TDIM,TVAR_PNTR,STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error creating temporary space')
         GOTO 99
      ENDIF

*  Map the input data,variance,quality arrays
      CALL BDI_MAPDATA(OFID,'UPDATE',D_PNTR,STATUS)
      CALL BDI_MAPQUAL(OFID,'UPDATE',Q_PNTR,STATUS)
      IF (LDVAR) CALL BDI_MAPVAR(OFID,'UPDATE',V_PNTR,STATUS)
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error mapping input arrays')
         GOTO 99
      ENDIF

*  Get the BADBITS mask
      CALL BDI_GETMASK(OFID,BADBITS,STATUS)

*  If badbits mask not found set to QUAL__MASK
      IF ( STATUS .NE. SAI__OK ) THEN
        BADBITS = QUAL__MASK
        CALL ERR_ANNUL( STATUS )
      ENDIF

*    Check shape of quality array is the same as the data array
      DO LP=1,NDIM
         IF (DIM(LP) .NE. QDIM(LP)) THEN
           STATUS=SAI__ERROR
           CALL ERR_REP(' ',
     :      'Quality and data arrays have different dimensions',STATUS)
           GOTO 99
         ENDIF
      ENDDO

*    Copy required slice of data,quality,variance into temp arrays.
      CALL DTA_COPYSLICER( DIM(1),DIM(2),DIM(3),DIM(4),%VAL(D_PNTR),
     :              AMIN,AMAX,ORDER,TDIM(1),TDIM(2),TDIM(3),TDIM(4),
     :                                             %VAL(TDATA_PNTR) )
*
      CALL DTA_COPYSLICEB( DIM(1),DIM(2),DIM(3),DIM(4),%VAL(Q_PNTR),
     :              AMIN,AMAX,ORDER,TDIM(1),TDIM(2),TDIM(3),TDIM(4),
     :                                             %VAL(TQUAL_PNTR) )
*
      IF (LDVAR) THEN
*
         CALL DTA_COPYSLICER( DIM(1),DIM(2),DIM(3),DIM(4),%VAL(V_PNTR),
     :              AMIN,AMAX,ORDER,TDIM(1),TDIM(2),TDIM(3),TDIM(4),
     :                                                %VAL(TVAR_PNTR) )
*
      ENDIF

*  Create temp arrays to hold an array of "good" and "bad" X and Y values
*  where "X" stands for the dimension being fitted i.e. APP_DIM, and
*  "Y" means the counts in that array element.
      CALL DYN_MAPR(1,TDIM(1),GOODX_PNTR,STATUS)
      CALL DYN_MAPR(1,TDIM(1),GOODY_PNTR,STATUS)
      CALL DYN_MAPR(1,TDIM(1),BADX_PNTR,STATUS)
      CALL DYN_MAPR(1,TDIM(1),BADY_PNTR,STATUS)

*  Also create arrays to hold the integer bin positions of the good and bad
*  pixels.
      CALL DYN_MAPI(1,TDIM(1),GOODBIN_PNTR,STATUS)
      CALL DYN_MAPI(1,TDIM(1),BADBIN_PNTR,STATUS)
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error creating temporary space')
         GOTO 99
      ENDIF

*  Ask user for the distance in bins between the knots in the spline.
*  Must be greater than four.
      JUMPOUT=.FALSE.
      DO WHILE ( .NOT. JUMPOUT)
        CALL USI_GET0I('KWIDTH',KWIDTH,STATUS)
        IF (KWIDTH.GT.4) THEN
          JUMPOUT=.TRUE.
        ELSE
          CALL MSG_OUT(' ','Width must be greater than 4 bins',
     :                                   STATUS)
          CALL USI_CANCL('KWIDTH',STATUS)
        END IF
      END DO
      RWIDTH=REAL(KWIDTH)*WIDTH(ORDER(1))

*    Create a temporary mapped area for the weights on each good point
      CALL DYN_MAPD(1,TDIM(1),WEIGHT_PNTR,STATUS)
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error creating temporary space')
         GOTO 99
      ENDIF

*    If there is no variance array then do an unweighted fit i.e. set weights
*    to 1
      IF (.NOT. LDVAR) THEN
        CALL ARR_INIT1D(1.0,TDIM(1),%VAL(WEIGHT_PNTR),STATUS)
      END IF
*
* Loop over the four dimensional temporary array.
*  Fit a spline to the Good points and interpolate the bad points of
*  one slice of the requested dimension APP_DIM at a time.
*
* Map 3 temporary double precision arrays for math_splfit
      CALL DYN_MAPD(1,TDIM(1),WORK_PNTR,STATUS)
      CALL DYN_MAPD(1,TDIM(1),XD_PNTR,STATUS)
      CALL DYN_MAPD(1,TDIM(1),YD_PNTR,STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error creating temporary space')
         GOTO 99
      ENDIF
*
      FIXTOT=0
*
      DO LP4=1,(AMAX(ORDER(4))-AMIN(ORDER(4))+1)
*
        DO LP3=1,(AMAX(ORDER(3))-AMIN(ORDER(3))+1)
*
          DO LP2=1,(AMAX(ORDER(2))-AMIN(ORDER(2))+1)
*
* Split this slice into good and bad arrays
*
            CALL UTIL_QUALSPLIT(LP2,LP3,LP4,TDIM(1),TDIM(2),
     :           TDIM(3),TDIM(4),%VAL(TQUAL_PNTR),
     :           BADBITS,%VAL(TDATA_PNTR),NGOOD,%VAL(GOODBIN_PNTR),
     :           %VAL(GOODY_PNTR),NBAD,%VAL(BADBIN_PNTR) )
*
            FIXTOT=FIXTOT+NBAD
*
* Fix "bad" points if there are any
*
           IF (NBAD .GT. 0) THEN
*
* Calculate the weight for each point in this slice by setting it
*  at 1.0 over the variance, if there is a variance.
	      IF (LDVAR) THEN
                CALL MATH_SPLWEIGHTS(NGOOD,%VAL(GOODBIN_PNTR),LP2,LP3,
     :                            LP4,TDIM(1),TDIM(2),TDIM(3),TDIM(4),
     :                               %VAL(TVAR_PNTR),%VAL(WEIGHT_PNTR))
              ENDIF
*
* Calculate the actual axis values from the bin positions.
              CALL UTIL_AXVAL(NGOOD,%VAL(GOODBIN_PNTR),WIDTH(ORDER(1))
     :              ,START(ORDER(1)),AMIN(ORDER(1)),%VAL(GOODX_PNTR))
*
              CALL UTIL_AXVAL(NBAD,%VAL(BADBIN_PNTR),WIDTH(ORDER(1))
     :              ,START(ORDER(1)),AMIN(ORDER(1)),%VAL(BADX_PNTR))
*
* Fit a spline to the "good" points
*
              CALL MATH_SPLFIT(NGOOD,%VAL(GOODX_PNTR),%VAL(GOODY_PNTR)
     :                 ,%VAL(WEIGHT_PNTR),%VAL(XD_PNTR),%VAL(YD_PNTR),
     :                  %VAL(WORK_PNTR),RWIDTH,N7,KNOT,COEFF,SS,STATUS)
*
* Check status returned by NAG routine
              IF (STATUS .NE. SAI__OK) GOTO 99
*
* Use the spline coefficients to interpolate the BAD points.
*
              CALL MATH_SPLREC(NBAD,%VAL(BADX_PNTR),N7,KNOT,COEFF,
     :                                  %VAL(BADY_PNTR),NFAIL,STATUS)
*
              IF (STATUS .NE. SAI__OK) GOTO 99
*
* Bad points at the edge of a slice get set to zero by MATH_SPLREC
*  Set these to the closest 'good' value.
*
              IF (NFAIL.GT.0) THEN
                 CALL INTERP_EDGE(NBAD,%VAL(BADBIN_PNTR),NGOOD,
     :             %VAL(GOODBIN_PNTR),%VAL(GOODY_PNTR),%VAL(BADY_PNTR))
              ENDIF
*
* Find the average value of the variance of the GOOD points in this slice.
*
              IF (LDVAR) THEN
                 CALL INTERP_AVSLICE(NGOOD,%VAL(GOODBIN_PNTR),LP2,
     :                              LP3,LP4,TDIM(1),TDIM(2),TDIM(3),
     :                                  TDIM(4),%VAL(TVAR_PNTR),AVGE)
              ENDIF
*
* Update the Temp arrays with the new Y values for the "BAD" x positions.
*
              CALL INTERP_UPDATE ( NBAD,%VAL(BADBIN_PNTR),
     :            %VAL(BADY_PNTR),LP2,LP3,LP4,LDVAR,AVGE,TDIM(1),
     :            TDIM(2),TDIM(3),TDIM(4),%VAL(TDATA_PNTR),
     :                          %VAL(TQUAL_PNTR),%VAL(TVAR_PNTR) )
*
            ENDIF
*
          ENDDO
*
        ENDDO
*
      ENDDO

*  Write the temporary arrays back into the relevant slice of the original
*  arrays.
      CALL DTA_WRITESLICER (TDIM(1),TDIM(2),TDIM(3),TDIM(4),
     :              %VAL(TDATA_PNTR),AMIN,AMAX,ORDER,DIM(1),
     :                      DIM(2),DIM(3),DIM(4),%VAL(D_PNTR))
*
      CALL DTA_WRITESLICEB (TDIM(1),TDIM(2),TDIM(3),TDIM(4),
     :              %VAL(TQUAL_PNTR),AMIN,AMAX,ORDER,DIM(1),
     :                      DIM(2),DIM(3),DIM(4),%VAL(Q_PNTR))
*
      IF (LDVAR) THEN
         CALL DTA_WRITESLICER(TDIM(1),TDIM(2),TDIM(3),TDIM(4),
     :                 %VAL(TVAR_PNTR),AMIN,AMAX,ORDER,DIM(1),
     :                       DIM(2),DIM(3),DIM(4),%VAL(V_PNTR))
      ENDIF

*  Change the badbits value so that the PATCHED bit is zero (ie. patched
*  points will be treated as good). We do this by ANDing the existing mask
*  with the inverse of the PATCH bit pattern.
      MASKOUT = BIT_ANDUB(BADBITS,BIT_NOTUB(QUAL__PATCHED))
      CALL BDI_PUTMASK( OFID, MASKOUT, STATUS )

*  Update history record
      CALL HSI_ADD( OFID, VERSION, STATUS )

*  Add action record
      CALL CHR_ITOC( FIXTOT, TOTCHAR, NCHAR )
      PATH(NLINES+1) = TOTCHAR//' bad quality pixels replaced'
      CALL HSI_PTXT(OFID,NLINES+1,PATH,STATUS)

      CALL MSG_SETI( 'NFIX', FIXTOT )
      CALL MSG_PRNT( '^NFIX points have been replaced' )

*    Unmap and annul all locators
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  INTERP_AVSLICE - Finds the average value of a slice of data
      SUBROUTINE INTERP_AVSLICE(NPTS,X,LP2,LP3,LP4,DIM1,DIM2,
     :                                     DIM3,DIM4,DATA,AVGE)
* Description :
*     This takes a slice of a data array and finds the average.
* Method :
*     Maps the data as a 4-d array.
* History :
*     May 18 1988   original (LTVAD::RDS)
* Type Definitions :
      IMPLICIT NONE
* Import :
      INTEGER NPTS                            !Number of BINS IN AXIS 1.
      INTEGER X(NPTS)                         !Position of data in 1st dim.
      INTEGER LP2,LP3,LP4                     !Define slice of output arrays
      INTEGER DIM1,DIM2,DIM3,DIM4             !Dimensions of data array
      REAL DATA(DIM1,DIM2,DIM3,DIM4)          !Data array to be updated
* Export :
      REAL AVGE                               !Average value for slice
* Local variables :
      INTEGER LP
      REAL TOTAL
*-
      TOTAL=0.0
* Loop over the 1st dimension
      DO LP=1,NPTS
*
         TOTAL=TOTAL + DATA(X(LP),LP2,LP3,LP4)
*
      ENDDO
*
      AVGE = TOTAL / REAL(NPTS)
*
      END


*+  INTERP_EDGE - Updates data array with interpolated values.
      SUBROUTINE INTERP_EDGE(NBAD,BADX,NGOOD,GOODX,GOODY,BADY)
* Description :
*     This sets BAD points which are outside the range of points used
*    to produce a spline fit to the value of the nearest good point.
* Method :
* History :
*     JUNE 16 1988   original (LTVAD::RDS)
* Type Definitions :
      IMPLICIT NONE
* Import :
      INTEGER NBAD                            !Number of 'BAD' bins
      INTEGER BADX(NBAD)                      !Positions of 'BAD' bins
      INTEGER NGOOD                           !Number of 'GOOD' bins
      INTEGER GOODX(NGOOD)                    !Positions of 'GOOD' bins
      REAL GOODY(NGOOD)                       !Values of GOOD bins
* Import/Export :
      REAL BADY(NBAD)                         !Values of BAD bins
* Export :
* Local variables :
      INTEGER LP
      INTEGER COUNT
*-
      COUNT=0
*
      DO LP=1,NBAD
*
* If bad X value before first good value, set BAD value to the first GOOD value
* If bad X value after last good value, set BAD value to the last GOOD value
*
         IF (BADX(LP).LT.GOODX(1)) THEN
            BADY(LP)=GOODY(1)
            COUNT=COUNT+1
         ELSE IF(BADX(LP) .GT. GOODX(NGOOD)) THEN
            BADY(LP)=GOODY(NGOOD)
            COUNT=COUNT+1
         ENDIF
*
      ENDDO
*
      CALL MSG_SETI( 'NC', COUNT )
      CALL MSG_PRNT( 'Setting ^NC edge point(s) to the nearest'/
     :               /' GOOD value' )

      END


*+  INTERP_UPDATE - Updates data array with interpolated values.
      SUBROUTINE INTERP_UPDATE(NBAD,X,Y,LP2,LP3,LP4,LDVAR,AVGE,
     :                         DIM1,DIM2,DIM3,DIM4,DATA,QUAL,VAR)
* Description :
*     This takes a list of interpolated values and inserts them into a
*    mapped array. It also changes the quality and variance arrays as
*    necessary. Quality is set to the patched value i.e. the second highest
*    bit is set true.
* Method :
*     The variance of the reconstituted "bad" values is set to the average
*    variance of the "good" datapoints in that slice.
* History :
*     May 13 1988   original (LTVAD::RDS)
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'QUAL_PAR'                      !Quality values
* Import :
      INTEGER NBAD                            !Number of interpolated values
      INTEGER X(NBAD)                         !Position of data in 1st dim.
      REAL  Y(NBAD)                           !Corresponding data values
      INTEGER LP2,LP3,LP4                     !Define slice of output arrays
      INTEGER DIM1,DIM2,DIM3,DIM4             !Dimensions of data and qual array
      LOGICAL LDVAR                           !Was variance in input file?
      REAL AVGE                               !Average variance for slice
* Import-Export :
      REAL DATA(DIM1,DIM2,DIM3,DIM4)          !Data array to be updated
      BYTE QUAL(DIM1,DIM2,DIM3,DIM4)          !Quality array to be updated
      REAL VAR (DIM1,DIM2,DIM3,DIM4)          !Variance array to be updated
* Export :
* Local variables :
      INTEGER LP
*-

*    Loop over the interpolated values.
      DO LP=1,NBAD
*
        DATA (X(LP),LP2,LP3,LP4) = Y(LP)

*      Quality
        QUAL (X(LP),LP2,LP3,LP4) = QUAL__PATCHED

*      Variance
        IF ( LDVAR ) THEN
          VAR (X(LP),LP2,LP3,LP4) = AVGE
        END IF

      END DO

      END
