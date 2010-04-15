C+
      SUBROUTINE CALFIT(STATUS)
C
C            C A L F I T
C
C     Command Name:
C        CALFIT
C
C     Function:
C        Fit a calibration curve to a polarization spectrum
C
C     Description:
C        The AAO Spectropolarimeter allows the insertion of a polarizer
C        which gives a 100% circular polarization for calibrating the
C        efficiency of the instrument. CALFIT is used to fit a Chebyshev
C        polynomial to an observed stokes parameter spectrum obtained
C        with this calibrator. The fitted curve is output as another
C        Stokes spectrum which may be used to calibrate other datasets
C        using the CALIB command.
C
C     Parameters:
C    (1) INPUT      (TSP, 1D)  The input dataset, a spectrum with one
C                               Stokes parameter which will be fitted.
C    (2) DEGREE     (Integer)  The degree of the polynomial to be fitted.
C    (3) OUTPUT     (TSP, 1D)  The output dataset, equivalent in structure
C                               to INPUT, but with Intensity array set to
C                               unity, and the Stokes array containing
C                               the fitted curve. The variance is set to
C                               zero.
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 28/4/1988
C
C-
C
C  History:
C    28/4/1988   Original Version.   JAB/AAO
C   Modified:
C    20/2/1997  - BKM/Starlink/RAL
C                 Convert from use of NAG to Starlink PDA library.
C


      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR,SPTR,VPTR,XPTR,YPTR,WPTR,W1PTR,W2PTR,SIZE

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,OLOC,SLOC,IDLOC,VDLOC,SDLOC
      CHARACTER*(DAT__SZLOC) XLOC,YLOC,WLOC,W1LOC,W2LOC
      INTEGER ACTDIM,DIMS(7)
      INTEGER NUM,DEG
      LOGICAL QZ,UZ,VZ

*  Get the data

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Get degree for polynomial

      CALL PAR_GET0I('DEGREE',DEG,STATUS)

*  Create the output dataset

      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output

      CALL TSP_COPY(LOC,OLOC,STATUS)

*  Get the size of the dataset

      CALL TSP_SIZE(OLOC,7,DIMS,ACTDIM,STATUS)
      IF (ACTDIM .NE. 1) THEN
          CALL MSG_OUT('OUT','Invalid Dimensions',STATUS)
          STATUS = USER__001
      ENDIF
      SIZE = DIMS(1)

*  Map the Intensity data

      CALL TSP_MAP_DATA(OLOC,'UPDATE',IPTR,IDLOC,STATUS)

*  Map the Stokes data and variance

*  First find out which stokes parameters are present, and map whichever
*  one is found first in the sequence Q,U,V
      CALL TSP_STOKES(OLOC,NUM,QZ,UZ,VZ,STATUS)
      IF (QZ) THEN

*  Map Q if it present
          CALL TSP_GET_STOKES(OLOC,'Q',SLOC,STATUS)
          CALL TSP_MAP_DATA(SLOC,'UPDATE',SPTR,SDLOC,STATUS)
          CALL TSP_MAP_VAR(SLOC,'UPDATE',VPTR,VDLOC,STATUS)
      ELSE IF (UZ) THEN

*  Otherwise map U
          CALL TSP_GET_STOKES(OLOC,'U',SLOC,STATUS)
          CALL TSP_MAP_DATA(SLOC,'UPDATE',SPTR,SDLOC,STATUS)
          CALL TSP_MAP_VAR(SLOC,'UPDATE',VPTR,VDLOC,STATUS)
      ELSE IF (VZ) THEN

*  or if no Q and U map V
          CALL TSP_GET_STOKES(OLOC,'V',SLOC,STATUS)
          CALL TSP_MAP_DATA(SLOC,'UPDATE',SPTR,SDLOC,STATUS)
          CALL TSP_MAP_VAR(SLOC,'UPDATE',VPTR,VDLOC,STATUS)
      ELSE

*  Error if there are no stokes parameters in the data set
          CALL MSG_OUT(' ','No Stokes Parameter Found',STATUS)
          GOTO 100
      ENDIF

*  Get Workspace arrays

      CALL TSP_TEMP(SIZE,'_DOUBLE',XPTR,XLOC,STATUS)
      CALL TSP_TEMP(SIZE,'_DOUBLE',YPTR,YLOC,STATUS)
      CALL TSP_TEMP(SIZE,'_DOUBLE',WPTR,WLOC,STATUS)
      CALL TSP_TEMP(3*SIZE+3*(DEG+1),'_DOUBLE',W1PTR,W1LOC,STATUS)
      CALL TSP_TEMP(SIZE,'_DOUBLE',W2PTR,W2LOC,STATUS)

*  Do the fit

      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_CALFIT(SIZE,DEG,%VAL(IPTR),%VAL(SPTR),%VAL(VPTR),
     :      %VAL(XPTR),%VAL(YPTR),%VAL(WPTR),%VAL(W1PTR),
     :      %VAL(W2PTR),STATUS)
      ENDIF

*  Tidy up

      CALL TSP_UNMAP(XLOC,STATUS)
      CALL TSP_UNMAP(YLOC,STATUS)
      CALL TSP_UNMAP(WLOC,STATUS)
      CALL TSP_UNMAP(W1LOC,STATUS)
      CALL TSP_UNMAP(W2LOC,STATUS)
      CALL TSP_UNMAP(VDLOC,STATUS)
      CALL TSP_UNMAP(SDLOC,STATUS)
      CALL DAT_ANNUL(SLOC,STATUS)
100   CONTINUE
      CALL TSP_UNMAP(IDLOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      END



      SUBROUTINE TSP_CALFIT(SIZE,DEG,I,S,V,X,Y,W,A,R,STATUS)
*+
*
*   Subroutine to do a chebyshev polynomial fit to the variation
*   of polarization with channel number
*
*   Points are weighted according to their variances and the fit is
*   performed using the Starlink PDA library routine PDA_DPOLFT. The fitted
*   values replace the stokes data, and the intensity is returned as one.
*
*   (>)  SIZE  (Integer)             Size of the datasets
*   (>)  DEG   (Integer)             Degree of polynomial to fit
*   (>)  I     (Real array(SIZE))    The Intensity data
*   (>)  S     (Real array(SIZE))    The Stokes data
*   (>)  V     (Real array(SIZE))    The variance on the Stokes data
*   (>)  X     (Double array(SIZE))  Workspace array for X data
*   (>)  Y     (Double array(SIZE))  Workspace array for Y data
*   (>)  W     (Double array(SIZE))  Workspace array for weights
*   (>)  A     (Double array(3*SIZE+3*(DEG+1)) Workspace and output array
*   (>)  R     (Double array(SIZE))  Fit at degree DEG at all X(I)
*   (>)  STATUS (Integer)            Status value
*
C  History:
C    1988   Original Version.   JAB/AAO
C  Modifications:
C    20/2/1997 Conversion to use PDA_DPOLFT instead of NAG routine E02ADF
C              BKM/Starlink/RAL
*+

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

      INTEGER SIZE,DEG

*  Subroutine parameters
      REAL I(SIZE),S(SIZE),V(SIZE)
      DOUBLE PRECISION X(SIZE),Y(SIZE),W(SIZE),A(3*SIZE+3*(DEG+1)),
     :                 R(SIZE)
      INTEGER STATUS

*  Local variables
      INTEGER IFAIL
      INTEGER J,NDEG
      DOUBLE PRECISION EPS

*  Calculate Data Arrays and Weights (Double Precision)
*  Set the weight to a very low value if the intensity or stokes data
*  is bad, or if the intensity is very small

      DO J=1,SIZE
          X(J) = J
          IF (I(J) .EQ. VAL__BADR) THEN
               W(J) = 1E-37
               Y(J) = 0.0
          ELSE IF (I(J) .GT. 0.00001) THEN
               Y(J) = S(J)/I(J)
               W(J) =I(J)*I(J)/V(J)
          ELSE
               Y(J) = 0.0
               W(J) = 1E-37
          ENDIF
      ENDDO

*  Do Chebyshev Polynomial Fit

      EPS = 0.0D0
      CALL PDA_DPOLFT(SIZE, X, Y, W, DEG, NDEG, EPS, R, IFAIL, A,
     :                STATUS)
      IF (NDEG .NE. DEG .OR. IFAIL .NE. 1) THEN
          CALL MSG_SETI('IFAIL',IFAIL)
          CALL MSG_OUT('MSG','PDA_DPOLFT Error - IFAIL = ^IFAIL',STATUS)
          RETURN
      ENDIF

* Results of fit of order DEG are present in the array R

*  Plug results into data array

      DO J=1,SIZE

          I(J) = 1.0
          S(J) = R(J)
          V(J) = 0.0
      ENDDO
      END

