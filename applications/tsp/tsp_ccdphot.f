      SUBROUTINE TSP_CCDPHOT(N1,N2,N3,X,Y,R,FLUXCAL,IN,OUT)
*+
*
*   Perform aperture photometry on a time series image.
*
*   For each frame in the image sum all the intensity within the specified
*   aperture, and scale to corresponding flux level
*
*   (>)  N1    (Integer)  First dimension of input cube
*   (>)  N2    (Integer)  Second dimension of input cube
*   (>)  N3    (Integer)  Third dimension of input cube
*   (>)  X     (Real)     X position of centre of aperture
*   (>)  Y     (Real)     Y position of centre of aperture
*   (>)  R     (Real)     Radius of aperture
*   (>)  FLUXCAL  (Real)  Counts per Jansky
*   (>)  IN    (Real array (N1,N2,N3))  Input time series image
*   (>)  OUT   (Real array (N3)) Output light curve
*
*
*   Jeremy Bailey   26/10/1989
*+
      IMPLICIT NONE

*  Parameters
      INTEGER N1,N2,N3
      REAL X,Y,R,IN(N1,N2,N3),OUT(N3)
      REAL FLUXCAL

*  Local variables
      INTEGER I1,I2,I3
      INTEGER LX,HX,LY,HY
      REAL DX,DY,SUM

*  Get limits for pixel search
      LX = INT(X-R)
      HX = INT(X+R)+1
      LY = INT(Y-R)
      HY = INT(Y+R)+1

*  Make sure we stay within image
      IF (LX .LT. 1) LX=1
      IF (LX .GT. N1) LX = N1
      IF (LY .LT. 1) LY=1
      IF (LY .GT. N2) LY = N2

*  Loop over frames summing all pixels within aperture
      DO I3=1,N3
          SUM = 0.0
          DO I2 = LY,HY
              DO I1 = LX,HX

*  Determine distance from centre
                  DX = REAL(I1)-X
                  DY = REAL(I2)-Y

*  Sum pixels with distance less than R
                  IF (SQRT(DX*DX+DY*DY) .LE. R) THEN
                      SUM = SUM+IN(I1,I2,I3)
                  ENDIF
              ENDDO
          ENDDO

*  Scale to flux
          OUT(I3) = SUM/FLUXCAL
      ENDDO
      END




      SUBROUTINE TSP_CCDPOL(N,E,O,OFFSET,S,I,V,VV,STATUS)
*+
*   T S P _ C C D P O L
*
*   CCDPOL command
*
*   From two light curves for the O and E star images determine the
*   intensity and Stokes parameters
*
*   Parameters:
*
*   (>)  N      (Integer)         Number of points
*   (>)  E      (Real array(N))   E light curve
*   (>)  O      (Real array(N))   O light curve
*   (>)  OFFSET (Real)            Polarization offset
*   (<)  S      (Real array(N))   Stokes parameter output
*   (<)  I      (Real array(N))   Intensity output
*   (<)  V      (Real array(N))   Variance output
*   (<)  VV     (Real array(N))   Stokes variance output
*   (!)  STATUS (Integer)         Status value
*
*   Jeremy Bailey   1989/10/26
*
*+

      IMPLICIT NONE

*  Parameters
      INTEGER N
      REAL E(N),O(N),S(N),I(N),V(N),VV(N)
      REAL OFFSET
      INTEGER STATUS

*  Local variables
      INTEGER J
      DOUBLE PRECISION SUMS,SUMI,SQS,SQI

*  Zero sums
      SUMS = 0D0
      SUMI = 0D0
      SQS = 0D0
      SQI = 0D0

*  Loop over points
      DO J=1,N

*  Stokes parameter is difference of E and O
          S(J) = (E(J)-O(J))

*  Intensity is sum of E and O
          I(J) = E(J)+O(J)

*  Correct for a percentage polarization offset
          S(J) = S(J) - OFFSET*I(J)/100.0

*  Zero variances
          V(J) = 0.0
          VV(J) = 0.0


*  Add to sums
          SUMS = SUMS+S(J)
          SUMI = SUMI+I(J)
          SQS = SQS+S(J)*S(J)
          SQI = SQI+I(J)*I(J)
      ENDDO

*  Calculate total and errors of intensity and polarization
      IF (N .GT. 1) THEN
          SQS = SQRT((SQS-SUMS*SUMS/N)/(N*(N-1)))
      ELSE
          SQS = 0D0
      ENDIF
      IF (N .GT. 1 .AND. SUMI .GE. 0D0) THEN
          SQI = SQRT((SQI-SUMI*SUMI/N)/(N*(N-1)))
      ELSE
          SQI = 0D0
      ENDIF
      SUMS = SUMS/N
      SUMI = SUMI/N

*  Output final results
      CALL MSG_FMTD('SUMI','F12.2',SUMI)
      CALL MSG_FMTD('SQI','F9.2',SQI)
      CALL MSG_OUT(' ',' Intensity     ^SUMI +/- ^SQI',STATUS)
      CALL MSG_FMTD('SUMS','F12.2',SUMS*100D0/SUMI)
      CALL MSG_FMTD('SQS','F9.2',SQS*100D0/SUMI)
      CALL MSG_OUT(' ',' Polarization  ^SUMS +/- ^SQS',STATUS)
      END


      SUBROUTINE TSP_CCDPHOTM(N,I,STATUS)
*+
*
*  T S P _ C C D P H O T M
*
*  CCDPHOT command
*
*  Output the mean and error of the intensity to the terminal
*  The mean and standard deviation of the points in the array I
*  are calculated and output on the terminal
*
*  Parameters:
*
*  (>)   N     (Integer)        Number of points
*  (>)   I     (Real array(N))  Intensity array
*  (!)   STATUS (Integer)       Status value
*
*  Jeremy Bailey   26/10/1989
*
*+
      IMPLICIT NONE

*  Parameters
      INTEGER N
      REAL I(N)
      INTEGER STATUS

*  Local variables
      INTEGER J
      DOUBLE PRECISION SUMI,SQI

*  Zero sums
      SUMI = 0D0
      SQI = 0D0

*  Calculate sum and sum of squares of data
      DO J=1,N
          SUMI = SUMI+I(J)
          SQI = SQI+I(J)*I(J)
      ENDDO

*  Calculate mean and error
      IF (N .GT. 1 .AND. SUMI .GE. 0D0) THEN
          SQI = SQRT((SQI-SUMI*SUMI/N)/(N*(N-1)))
      ELSE
          SQI = 0D0
      ENDIF
      SUMI = SUMI/N

*  Output results
      CALL MSG_FMTD('SUMI','F12.2',SUMI)
      CALL MSG_FMTD('SQI','F9.2',SQI)
      CALL MSG_OUT(' ',' Intensity     ^SUMI +/- ^SQI',STATUS)
      END


      SUBROUTINE TSP_CCDPHOT_COPYT(N,IN,OUT)
*+
*  T S P _ C C D P H O T _ C O P Y T
*
*  CCDPHOT command
*
*  Copy a double precision array from input to output
*
*  Used to copy the time axis of the input 3D dataset into the output
*  light curve dataset.
*
*  Parameters:
*
*   (>)  N       (Integer)  Number of points
*   (>)  IN      (Double array (N))  Input array
*   (<)  OUT     (Double array (N))  Output array
*
*  Jeremy Bailey   26/10/1989
*+

      IMPLICIT NONE

*  Parameters
      INTEGER N
      DOUBLE PRECISION IN(N),OUT(N)

*  Local variable
      INTEGER I

*  Copy the data
      DO I=1,N
          OUT(I) = IN(I)
      ENDDO
      END


      SUBROUTINE TSP_CCDPHOT_LAMBDA(IN,OUT)
*+
*  T S P _ C C D P H O T _ L A M B D A
*
*  CCDPHOT command
*
*  Copy a single value from input to output
*
*  Used to write the wavelength into a mapped array
*  Also used by TSEXTRACT command
*
*  Parameters:
*
*   (>)  IN  (Real)   The input value
*   (<)  OUT (Real)   The output value
*
*   Jeremy Bailey   26/10/1989
*+
      IMPLICIT NONE
      REAL IN,OUT
      OUT=IN
      END






