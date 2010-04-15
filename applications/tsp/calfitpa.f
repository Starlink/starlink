C+
      SUBROUTINE CALFITPA(STATUS)
C
C            C A L F I T P A
C
C     Command Name:
C        CALFITPA
C
C     Function:
C        Fit a calibration curve to the polarization position angle
C
C     Description:
C        A polarimeter using a rotating superachromatic half-wave plate
C        made on the Pancharatnam design will result in position angles
C        which have a slight wavelength dependence due to the variation
C        of the angle of the plates fast axis with wavelength. CALFITPA
C        can be used to fit a calibration curve to an observation of
C        an object with known position angle (e.g. an observation with
C        a calibration polarizer) which can then be used to
C        calibrate other data. A Chebyshev polynomial is fitted to the
C        position angle data to obtain an output spectrum with
C        can then be used as input to the CALPA command.
C
C     Parameters:
C    (1) INPUT      (TSP, 1D)  The input dataset, a polarization spectrum
C                               which will be fitted to.
C    (2) DEGREE     (Integer)  The degree of the polynomial to be fitted.
C    (3) PA         (Real)     The position angle of the calibration source.
C    (4) OUTPUT     (TSP, 1D)  The output dataset, equivalent in structure
C                               to INPUT, but with Intensity array set to
C                               unity, and the Stokes arrays containing
C                               the fitted curve. The variance is set to
C                               zero.
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 19/11/1991
C
C-
C
C  History:
C    25/8/1990   Original Version.   JAB/AAO
*   Modified:
*    20/2/1997  - BKM/Starlink/RAl
*                 Convert from use of NAG to Starlink PDA library.
C


      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR,QSPTR,USPTR,QVPTR,UVPTR,XPTR,YPTR,WPTR,W1PTR,W2PTR,
     :        SIZE

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,OLOC,QSLOC,USLOC,IDLOC,QVDLOC,UVDLOC,
     :    QSDLOC,USDLOC
      CHARACTER*(DAT__SZLOC) XLOC,YLOC,WLOC,W1LOC,W2LOC
      INTEGER ACTDIM,DIMS(7)
      INTEGER DEG
      REAL PA

*  Get the data

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Get degree for polynomial

      CALL PAR_GET0I('DEGREE',DEG,STATUS)

*  Get Position angle

      CALL PAR_GET0R('PA',PA,STATUS)
      PA = PA*3.14159265/180

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

*  Q stokes parameter
      CALL TSP_GET_STOKES(OLOC,'Q',QSLOC,STATUS)
      CALL TSP_MAP_DATA(QSLOC,'UPDATE',QSPTR,QSDLOC,STATUS)
      CALL TSP_MAP_VAR(QSLOC,'UPDATE',QVPTR,QVDLOC,STATUS)

*  U stokes parameter
      CALL TSP_GET_STOKES(OLOC,'U',USLOC,STATUS)
      CALL TSP_MAP_DATA(USLOC,'UPDATE',USPTR,USDLOC,STATUS)
      CALL TSP_MAP_VAR(USLOC,'UPDATE',UVPTR,UVDLOC,STATUS)

*  Get Workspace arrays

      CALL TSP_TEMP(SIZE,'_DOUBLE',XPTR,XLOC,STATUS)
      CALL TSP_TEMP(SIZE,'_DOUBLE',YPTR,YLOC,STATUS)
      CALL TSP_TEMP(SIZE,'_DOUBLE',WPTR,WLOC,STATUS)
      CALL TSP_TEMP(3*SIZE+3*(DEG+1),'_DOUBLE',W1PTR,W1LOC,STATUS)
      CALL TSP_TEMP(SIZE,'_DOUBLE',W2PTR,W2LOC,STATUS)

*  Do the fit

      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_CALFITPA(SIZE,DEG,PA,%VAL(IPTR),%VAL(QSPTR),
     :      %VAL(QVPTR),%VAL(USPTR),%VAL(UVPTR),
     :      %VAL(XPTR),%VAL(YPTR),%VAL(WPTR),%VAL(W1PTR),%VAL(W2PTR),
     :      STATUS)
      ENDIF

*  Tidy up

      CALL TSP_UNMAP(XLOC,STATUS)
      CALL TSP_UNMAP(YLOC,STATUS)
      CALL TSP_UNMAP(WLOC,STATUS)
      CALL TSP_UNMAP(W1LOC,STATUS)
      CALL TSP_UNMAP(W2LOC,STATUS)
      CALL TSP_UNMAP(QVDLOC,STATUS)
      CALL TSP_UNMAP(QSDLOC,STATUS)
      CALL DAT_ANNUL(QSLOC,STATUS)
      CALL TSP_UNMAP(UVDLOC,STATUS)
      CALL TSP_UNMAP(USDLOC,STATUS)
      CALL DAT_ANNUL(USLOC,STATUS)
100   CONTINUE
      CALL TSP_UNMAP(IDLOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      END



      SUBROUTINE TSP_CALFITPA(SIZE,DEG,PA,I,Q,VQ,U,VU,X,Y,W,A,R,STATUS)
*+
*
*   Subroutine to do a chebyshev polynomial fit to the variation
*   of position angle with channel number
*
*   Points are weighted according to their variances and the fit is
*   performed using the NAG routine E02ADF. The fitted values replace
*   the stokes data, and the intensity is returned as one.
*
*   (>)  SIZE  (Integer)             Size of the datasets
*   (>)  DEG   (Integer)             Degree of polynomial to fit
*   (>)  PA    (Real)                PA to assume for data
*   (>)  I     (Real array(SIZE))    The Intensity data
*   (>)  Q     (Real array(SIZE))    The Q Stokes data
*   (>)  VQ    (Real array(SIZE))    The variance on the Q Stokes data
*   (>)  U     (Real array(SIZE))    The U Stokes data
*   (>)  VU    (Real array(SIZE))    The variance on the U Stokes data
*   (>)  X     (Double array(SIZE))  Workspace array for X data
*   (>)  Y     (Double array(SIZE))  Workspace array for Y data
*   (>)  W     (Double array(SIZE))  Workspace array for weights
*   (>)  A    (Double array(3*SIZE+3*(DEG+1)))  Workspace array
*   (>)  R    (Double array(SIZE))  Workspace array - fit of X at degree DEG
*   (>)  STATUS (Integer)            Status value
*
*
*   Jeremy Bailey    25/8/1990
*
*   Modified:
*    19/11/91  -   Add support for bad values
*    20/2/1997 Conversion to use PDA_DPOLFT instead of NAG routine E02ADF
C              BKM/Starlink/RAL
*+
      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE,DEG
      REAL PA
      REAL I(SIZE),Q(SIZE),VQ(SIZE),U(SIZE),VU(SIZE)
      DOUBLE PRECISION X(SIZE),Y(SIZE),W(SIZE),A(3*SIZE+3*(DEG+1)),
     :                 R(SIZE)
      INTEGER STATUS

*  Local variables
      INTEGER IFAIL
      INTEGER J,NDEG
      REAL TWOTHETA,POL,XX
      DOUBLE PRECISION EPS

*  Calculate Data Arrays and Weights (Double Precision)

*   Loop over points
      DO J=1,SIZE

*   Set X to the channel number
          X(J) = J

*    Use Q and U to calculate the value of TWOTHETA (twice the
*    polarization position angle) which is the quantity we fit to.
          IF (Q(J) .EQ. VAL__BADR .OR. U(J) .EQ. VAL__BADR) THEN
               TWOTHETA = VAL__BADR
          ELSE
               TWOTHETA = ATAN2(U(J),Q(J))
          ENDIF

*   Determine the weight for the position angle which depends on the
*   polarization level. If intensity or stokes parameters are bad
*   set weight to a very low value
          IF (I(J) .EQ. VAL__BADR .OR. TWOTHETA .EQ. VAL__BADR) THEN
              W(J) = 1D-37
              Y(J) = 0D0
          ELSE IF (I(J) .GT. 1E-10) THEN
              POL = (SQRT(Q(J)*Q(J)+U(J)*U(J)))
              XX = Q(J)*Q(J)*VU(J) + U(J)*U(J)*VQ(J)
              XX= 0.5*SQRT(XX)/(POL*POL)
              W(J) = 1.0/XX
              Y(J) = TWOTHETA
          ELSE
              W(J) = 1D-37
              Y(J) = 0D0
          ENDIF
      ENDDO

*  Do Chebyshev Polynomial Fit

      EPS = 0.0D0
      CALL PDA_DPOLFT(SIZE,X,Y,W,DEG,NDEG,EPS,R,IFAIL,A,STATUS)
      IF (IFAIL .NE. 1) THEN
          CALL MSG_SETI('IFAIL',IFAIL)
          CALL MSG_OUT('MSG','PDA_DPOLFT Error - IFAIL = ^IFAIL',STATUS)
          RETURN
      ENDIF

*  Calculate Fitted Curve

      DO J=1,SIZE

          TWOTHETA = R(J)

*  Plug the values back into the data by setting the intensity to one
*  and the U and Q to sin and cos of the position angle
          I(J) = 1.0
          U(J) = SIN(TWOTHETA-2*PA)
          Q(J) = COS(TWOTHETA-2*PA)

*  Set variance to zero
          VQ(J) = 0.0
          VU(J) = 0.0
      ENDDO
      END

