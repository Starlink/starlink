
      SUBROUTINE TSP_PHSMAG(SIZE,Y,YE,WAVES,CHAN,STATUS)
*+
*
*  T S P _ P H S M A G
*
*  PHASEPLOT command - Convert data to magnitudes
*
*  Convert data to magnitudes. The data is assumed to be flux in Jy
*  and is converted according to the standard zero point, if the wavelength
*  corresponds to one of the standard bands. Otherwise an arbitrary zero
*  point is used.
*
*  (>)  SIZE  (Integer)   Number of data points
*  (!)  Y     (Real)      Array of data to be converted
*  (!)  YE    (Real)      Array of error values
*  (>)  WAVES (Real)      Array of channel wavelengths
*  (>)  CHAN  (Integer)   Current channel
*  (!)  STATUS(Integer)   Status value
*
*   Jeremy Bailey  28/2/1988
*
*   Modified:
*      11/12/1991
*
*+
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL Y(SIZE),YE(SIZE),WAVES(*)
      INTEGER CHAN
      INTEGER STATUS

*  Local variables
      REAL ZEROPT
      REAL M,M2
      INTEGER I

      IF (STATUS .EQ. SAI__OK) THEN

*  Find magnitude zero point for wavelength
*  (values from Bessell 1979, PASP 91,589 for UBVRI,
*   Allen and Cragg, 1983, MN 203,777 for JHKL)


         IF (WAVES(CHAN) .EQ. 3600.0) THEN

*  U band
             ZEROPT = 1810
         ELSE IF (WAVES(CHAN) .EQ. 4400.0) THEN

*  B band
             ZEROPT = 4260
         ELSE IF (WAVES(CHAN) .EQ. 5500.0) THEN

*  V band
             ZEROPT = 3640
         ELSE IF (WAVES(CHAN) .EQ. 6400.0) THEN

*  R band
             ZEROPT = 3080
         ELSE IF (WAVES(CHAN) .EQ. 7900.0) THEN

*  I band
             ZEROPT = 2250
         ELSE IF (WAVES(CHAN) .EQ. 12000.0) THEN

*  J band
             ZEROPT = 1640
         ELSE IF (WAVES(CHAN) .EQ. 16400.0) THEN

*  H band
             ZEROPT = 1030
         ELSE IF (WAVES(CHAN) .EQ. 22000.0) THEN

*  K band
             ZEROPT = 650
         ELSE
             ZEROPT = 2000
         ENDIF

*  Convert the data

         DO I=1,SIZE
              IF (Y(I) .EQ. VAL__BADR) THEN
                  YE(I) = 0.0
              ELSE IF (Y(I) .GT. 0.0) THEN

*  Calculate magnitude and magnitude error
                  M = -2.5 * ALOG10(Y(I)/ZEROPT)
                  M2 = -2.5 * ALOG10((Y(I)+YE(I))/ZEROPT)
                  YE(I) = M-M2
                  Y(I) = M
              ELSE
                  Y(I) = VAL__BADR
                  YE(I) = 0.0
              ENDIF
         ENDDO
      ENDIF
      END



      SUBROUTINE TSP_PHSFLUX(SIZE,Y,YE,STATUS)
*+
*
*  T S P _ P H S F L U X
*
*  PHASEPLOT command - Convert data to Flux in mJy
*
*  Simply multiply by 1000 to go from Jy to mJy
*
*  Parameters
*
*   (>)  SIZE    (Integer)          Size of array
*   (!)  Y       (Real array(SIZE)) Array of flux values
*   (!)  YE      (Real array(SIZE)) Array of errors
*   (!)  STATUS  (Integer)          Status value
*
*   Jeremy Bailey   28/2/1988
*
*   Modified:
*      11/12/1991
*
*+
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL Y(SIZE),YE(SIZE)
      INTEGER STATUS

*  Local variable
      INTEGER I

      IF (STATUS .EQ. SAI__OK) THEN

*  Loop over points multiplying data and error by 1000
         DO I=1,SIZE
              IF (Y(I) .NE. VAL__BADR) THEN
                  Y(I) = 1000.0*Y(I)
                  YE(I) = 1000.0*YE(I)
              ENDIF
         ENDDO
      ENDIF
      END




      SUBROUTINE TSP_PHSSTOKES(SIZE,Y,YE,S,SE,STATUS)
*+
*
*  T S P _ P H S S T O K E S
*
*  PHASEPLOT command - Convert data to percentage Stokes parameter
*
*  Parameters
*
*  (>) SIZE  (Integer)  Number of data points
*  (!) Y     (Real)     On input - Array of Intensity data
*                       On output - Array of Percentage Stokes data
*  (!) YE    (Real)     On input - Array of Intensity errors
*                       On output - Array of Percentage Stokes errors
*  (>) S     (Real)     Array of Stokes data in intensity units
*  (>) SE    (Real)     Array of Stokes errors in intensity units
*  (!) STATUS (Integer) Status value
*
*   Jeremy Bailey    28/2/1988
*
*  Modified:
*      11/12/1991
*
*+
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL Y(SIZE),YE(SIZE),S(SIZE),SE(SIZE)
      INTEGER STATUS

*  Local variables
      REAL X,XX,ST
      INTEGER I

      IF (STATUS .EQ. SAI__OK) THEN

*  Loop over points
         DO I=1,SIZE
              IF (Y(I) .EQ. 0.0 .OR. Y(I) .EQ. VAL__BADR) THEN
                  YE(I)=0.0
              ELSE

*  Calculate percentage Stokes
                  ST = S(I)/Y(I) * 100.0

*  Calculate error on percentage Stokes parameter
                  X = YE(I)*YE(I)/Y(I)/Y(I)
                  IF (S(I) .NE. 0.0) THEN
                      XX = X+SE(I)*SE(I)/S(I)/S(I)
                  ELSE
                      XX = 0.0
                  ENDIF
                  IF (XX .GT. 0.0) THEN
                      YE(I) = ABS(ST * SQRT(XX))
                  ELSE
                      YE(I) = 0.0
                  ENDIF

*  Return percentage Stokes in original data array
                  Y(I) = ST
              ENDIF
         ENDDO
      ENDIF
      END




      SUBROUTINE TSP_PHSPOL(SIZE,Y,YE,Q,QE,U,UE,STATUS)
*+
*
*  T S P _ P H S P O L
*
*  PHASEPLOT command
*
*  Convert data to percentage Polarization. The intensity and stokes
*  parameter arrays and their errors are used to calculate percentage
*  polarization, and errors on percentage polarization. These replace
*  the input intensity and intensity error arrays for return to the
*  calling program.
*
*  (>) SIZE  (Integer)  Number of data points
*  (!) Y     (Real)     On input - Array of Intensity data
*                       On output - Array of Percentage polarization data
*  (!) YE    (Real)     On input - Array of Intensity errors
*                       On output - Array of Percentage polarization errors
*  (>) Q     (Real)     Array of Q Stokes data in intensity units
*  (>) QE    (Real)     Array of Q Stokes errors in intensity units
*  (>) U     (Real)     Array of U Stokes data in intensity units
*  (>) UE    (Real)     Array of U Stokes errors in intensity units
*  (!) STATUS (Integer) Status value
*
*  Jeremy Bailey    28/2/1988
*
*  Modified:
*     11/12/1991
*
*+
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL Y(SIZE),YE(SIZE),U(SIZE),UE(SIZE),Q(SIZE),QE(SIZE)
      INTEGER STATUS

*  Local variables
      REAL X,XX,QS,US,QSE,USE
      INTEGER I

      IF (STATUS .EQ. SAI__OK) THEN
         DO I=1,SIZE

*  Calculate percentage Stokes parameters and errors

              IF (Y(I) .EQ. 0.0 .OR. Y(I) .EQ. VAL__BADR) THEN

*  If data bad set Stokes parameters to zero

                  QSE=0.0
                  USE=0.0
                  QS=0.0
                  US=0.0
              ELSE

*  Calculate percentage Q Stokes parameter

                  QS = Q(I)/Y(I) * 100.0

*  Calculate errors on Q

                  X = YE(I)*YE(I)/Y(I)/Y(I)
                  IF (Q(I) .NE. 0.0) THEN
                      XX = X+QE(I)*QE(I)/Q(I)/Q(I)
                  ELSE
                      XX = 0.0
                  ENDIF
                  IF (XX .GT. 0.0) THEN
                      QSE = ABS(QS * SQRT(XX))
                  ELSE
                      QSE = 0.0
                  ENDIF

*  Calculate percentage U Stokes parameter

                  US = U(I)/Y(I) * 100.0

*  Calculate errors on U

                  X = YE(I)*YE(I)/Y(I)/Y(I)
                  IF (U(I) .NE. 0.0) THEN
                      XX = X+UE(I)*UE(I)/U(I)/U(I)
                  ELSE
                      XX = 0.0
                  ENDIF
                  IF (XX .GT. 0.0) THEN
                      USE = ABS(US * SQRT(XX))
                  ELSE
                      USE = 0.0
                  ENDIF
              ENDIF

*  Calculate polarization from Stokes parameters

              IF (Y(I) .NE. VAL__BADR) THEN
                  Y(I) = SQRT(QS*QS+US*US)

*  Calculate error on polarization

                  IF (Y(I) .GT. 1E-4) THEN
                      YE(I)=(SQRT(QS*QS*QSE*QSE+US*US*USE*USE))/Y(I)
                  ELSE
                      YE(I)=0.0
                  ENDIF
              ENDIF
         ENDDO
      ENDIF
      END



      SUBROUTINE TSP_PHSTHETA(SIZE,Y,YE,Q,QE,U,UE,STATUS)
*+
*
*  T S P _ P H S T H E T A
*
*  PHASEPLOT command
*
*  Convert data to position angle
*  Convert data to position angle in degrees. The intensity and stokes
*  parameter arrays and their errors are used to calculate position
*  angle, and errors on position angle. These replace
*  the input intensity and intensity error arrays for return to the
*  calling program.
*
*
*  (>) SIZE  (Integer)  Number of data points
*  (!) Y     (Real)     On input - Array of Intensity data
*                       On output - Array of Position angle data
*  (!) YE    (Real)     On input - Array of Intensity errors
*                       On output - Array of Position angle errors
*  (>) Q     (Real)     Array of Q Stokes data in intensity units
*  (>) QE    (Real)     Array of Q Stokes errors in intensity units
*  (>) U     (Real)     Array of U Stokes data in intensity units
*  (>) UE    (Real)     Array of U Stokes errors in intensity units
*  (!) STATUS (Integer) Status value
*
*  Jeremy Bailey    28/2/1988
*
*  Modified:
*     11/12/1991
*
*+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL Y(SIZE),YE(SIZE),U(SIZE),UE(SIZE),Q(SIZE),QE(SIZE)
      INTEGER STATUS

*  Local variables
      REAL X,XX,QS,US,QSE,USE,P
      INTEGER I
      REAL DEGRAD

      DEGRAD = 45.0/ATAN(1.0)
      IF (STATUS .EQ. SAI__OK) THEN
         DO I=1,SIZE

*  calculate percentage Stokes parameters and errors

              IF (Y(I) .EQ. 0.0 .OR. Y(I) .EQ. VAL__BADR) THEN

*  If data is bad set Stokes parameters to zero

                  QSE=0.0
                  USE=0.0
                  QS=0.0
                  US=0.0
              ELSE

*  Calculate Q percentage Stokes parameters

                  QS = Q(I)/Y(I) * 100.0

*  Calculate errors on Q

                  X = YE(I)*YE(I)/Y(I)/Y(I)
                  IF (Q(I) .NE. 0.0) THEN
                      XX = X+QE(I)*QE(I)/Q(I)/Q(I)
                  ELSE
                      XX = 0.0
                  ENDIF
                  IF (XX .GT. 0.0) THEN
                      QSE = ABS(QS * SQRT(XX))
                  ELSE
                      QSE = 0.0
                  ENDIF

*  Calculate U percentage Stokes parameters

                  US = U(I)/Y(I) * 100.0

*  Calculate errors on U

                  X = YE(I)*YE(I)/Y(I)/Y(I)
                  IF (U(I) .NE. 0.0) THEN
                      XX = X+UE(I)*UE(I)/U(I)/U(I)
                  ELSE
                      XX = 0.0
                  ENDIF
                  IF (XX .GT. 0.0) THEN
                      USE = ABS(US * SQRT(XX))
                  ELSE
                      USE = 0.0
                  ENDIF
              ENDIF

*  Calculate position  angle and error from Stokes parameters

              IF (Y(I) .NE. VAL__BADR) THEN

*  Calculate Percentage polarization (we need it for the error calculation)

                  P = SQRT(QS*QS+US*US)

*  Calculate position angle in degrees

                  IF (QS .NE. 0.0) THEN
                      X=DEGRAD*ATAN2(US,QS)
                      IF (X .LT. 0.0) X=X+360.0
                      Y(I)=X/2.0
                  ELSE
                      Y(I)=0.0
                  ENDIF

*  Calculate error on position angle

                  IF (P .GT. 1E-4) THEN
                      YE(I)=(0.5*SQRT(QS*QS*USE*USE+US*US*QSE*QSE))/
     :                    (P*P)
                      YE(I)=ABS(YE(I)*57.2958)
                  ELSE
                      YE(I)=0.0
                  ENDIF
              ENDIF
         ENDDO
      ENDIF
      END


