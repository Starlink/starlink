
      SUBROUTINE TSP_CCD2ST_OLD(NY,SIZE,I1,I2,ASTART,BSTART,OESEP,
     :   WIDTH,AP,BIAS,RDN,PHOTADU,INT,STOKES,VSTOKES,IX,OX)
*+
*
*   Reduce CCD spectropolarimetry using the Scaling method
*
*   (>)  NY      (Integer)  Size of spatial axis of input arrays
*   (>)  SIZE    (Integer)  Number of spectral channels
*   (>)  I1      (Real array(NY,SIZE) First input array
*   (>)  I2      (Real array(NY,SIZE) Second input array
*   (>)  ASTART  (Integer)  Start of A aperture
*   (>)  BSTART  (Integer)  Start of B aperture
*   (>)  OESEP   (Integer)  Separation of O and E spectra
*   (>)  WIDTH   (Integer)  Width of each spectrum
*   (>)  AP      (Char)     Name of aperture (A or B)
*   (>)  BIAS    (Real)     Bias level to be subtracted from CCD data
*   (>)  RDN     (Real)     Readout noise in electrons
*   (>)  PHOTADU (Real)     Photons per ADU
*   (<)  INT     (Real array(SIZE))  Output intensity
*   (<)  STOKES  (Real array(SIZE))  Output Stokes data
*   (<)  VSTOKES (Real array(SIZE))  Variance on Stokes parameter
*   (>)  IX      (Real array(SIZE))  Input X axis array
*   (<)  OX      (Real array(SIZE))  Output X axis array
*
*
*   Jeremy Bailey   24/9/1988
*
*   Modified:
*      20/11/1991    Handle bad values
*+

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE,NY,ASTART,BSTART,OESEP,WIDTH
      CHARACTER*(*) AP
      REAL BIAS,PHOTADU,RDN
      REAL I1(NY,SIZE),I2(NY,SIZE)
      REAL INT(SIZE),STOKES(SIZE),VSTOKES(SIZE)
      REAL IX(SIZE),OX(SIZE)

*  Local variables
      INTEGER I,J,K
      REAL SPEC2(8)
      INTEGER PT(8)
      REAL STAR,SKY
      REAL RAT

*  Copy X axis to output
      DO I=1,SIZE
         OX(I)=IX(I)
      ENDDO

*  Determine position ranges to extract data over
      IF (AP .EQ. 'A') THEN
          PT(1) = ASTART
          PT(2) = ASTART+OESEP
          PT(3) = BSTART
          PT(4) = BSTART+OESEP
      ELSE
          PT(1) = BSTART
          PT(2) = BSTART+OESEP
          PT(3) = ASTART
          PT(4) = ASTART+OESEP
      ENDIF

*  Extract 8 spectra (star and sky for each of O and E and each
*  of two polarization states)
      DO I=1,SIZE
          DO J=1,4
              SPEC2(J) = I1(PT(J),I)
              SPEC2(J+4) = I2(PT(J),I)
              DO K=1,WIDTH-1
                  SPEC2(J) = SPEC2(J)+I1(PT(J)+K,I)
                  SPEC2(J+4) = SPEC2(J+4)+I2(PT(J)+K,I)
              ENDDO

*  Subtract bias
              SPEC2(J)=SPEC2(J) - BIAS * REAL(WIDTH)
              SPEC2(J+4) = SPEC2(J+4) - BIAS * REAL(WIDTH)

*  Scale to detected photons
              SPEC2(J)=SPEC2(J)*PHOTADU
              SPEC2(J+4)=SPEC2(J+4)*PHOTADU
          ENDDO

*  Determine ratio of E and O data
          IF (spec2(2)+spec2(6) .NE. 0.0) THEN
             RAT = (SPEC2(1)+SPEC2(5))/(SPEC2(2)+SPEC2(6))
          ELSE
             RAT = 1.0
          ENDIF

*  Scale up the O data to match that of the E data
          SPEC2(2) = SPEC2(2)*RAT
          SPEC2(6) = SPEC2(6)*RAT

*  Determine star intensity
          STAR = SPEC2(1)+SPEC2(2)+SPEC2(5)+SPEC2(6)

*  Determine sky intensity
          SKY = SPEC2(3)+SPEC2(4)+SPEC2(7)+SPEC2(8)

*  Output intensity is star minus sky
          INT(I) = STAR-SKY

*  Variance is photon statistics (quadratic sum of star and sky photons)
*   plus readout noise (scaled by total number of pixels used, 8*width)
          VSTOKES(I) = SQRT(STAR*STAR+SKY*SKY)
     :        + 8.0*RDN*RDN*REAL(WIDTH)

*  Determine Stokes parameter for star
          STAR = SPEC2(2)-SPEC2(1)+SPEC2(5)-SPEC2(6)

*  Determine Stokes parameter for sky
          SKY = SPEC2(4)-SPEC2(3)+SPEC2(7)-SPEC2(8)

*  Output Stokes parameter is star minus sky
          STOKES(I) = STAR-SKY

      ENDDO
      END


      SUBROUTINE TSP_CCD2STOKES(NY,SIZE,I1,I2,ASTART,BSTART,OESEP,
     :   WIDTH,AP,BIAS,RDN,PHOTADU,INT,STOKES,VSTOKES,IX,OX)
*+
*
*   Reduce CCD spectropolarimetry using the Ratio method
*
*   (>)  NY      (Integer)  Size of spatial axis of input arrays
*   (>)  SIZE    (Integer)  Number of spectral channels
*   (>)  I1      (Real array(NY,SIZE) First input array
*   (>)  I2      (Real array(NY,SIZE) Second input array
*   (>)  ASTART  (Integer)  Start of A aperture
*   (>)  BSTART  (Integer)  Start of B aperture
*   (>)  OESEP   (Integer)  Separation of O and E spectra
*   (>)  WIDTH   (Integer)  Width of each spectrum
*   (>)  AP      (Char)     Name of aperture (A or B)
*   (>)  BIAS    (Real)     Bias level to be subtracted from CCD data
*   (>)  RDN     (Real)     Readout noise in electrons
*   (>)  PHOTADU (Real)     Photons per ADU
*   (<)  INT     (Real array(SIZE))  Output intensity
*   (<)  STOKES  (Real array(SIZE))  Output Stokes data
*   (<)  VSTOKES (Real array(SIZE))  Variance on Stokes parameter
*   (>)  IX      (Real array(SIZE))  Input X axis array
*   (<)  OX      (Real array(SIZE))  Output X axis array
*
*
*   Jeremy Bailey   24/9/1988
*
*   Modified:
*      20/11/1991    Handle bad values
*+

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE,NY,ASTART,BSTART,OESEP,WIDTH
      CHARACTER*(*) AP
      REAL BIAS,PHOTADU,RDN
      REAL I1(NY,SIZE),I2(NY,SIZE)
      REAL INT(SIZE),STOKES(SIZE),VSTOKES(SIZE)
      REAL IX(SIZE),OX(SIZE)

*  Local variables
      INTEGER I,J,K
      REAL SPEC2(8)
      INTEGER PT(8)
      REAL STAR,SKY
      REAL RAT
      INTEGER STATUS

      STATUS=0

*  Copy X axis to output
      DO I=1,SIZE
         OX(I)=IX(I)
      ENDDO

*  Determine position ranges to extract data over
      IF (AP .EQ. 'A') THEN
          PT(1) = ASTART
          PT(2) = ASTART+OESEP
          PT(3) = BSTART
          PT(4) = BSTART+OESEP
      ELSE
          PT(1) = BSTART
          PT(2) = BSTART+OESEP
          PT(3) = ASTART
          PT(4) = ASTART+OESEP
      ENDIF

*  Extract 8 spectra (star and sky for each of O and E and each
*  of two polarization states)

      DO I=1,SIZE
          DO J=1,4
              SPEC2(J) = I1(PT(J),I)
              SPEC2(J+4) = I2(PT(J),I)
              DO K=1,WIDTH-1
                  SPEC2(J) = SPEC2(J)+I1(PT(J)+K,I)
                  SPEC2(J+4) = SPEC2(J+4)+I2(PT(J)+K,I)
              ENDDO

*  Subtract bias
              SPEC2(J)=SPEC2(J) - BIAS * REAL(WIDTH)
              SPEC2(J+4) = SPEC2(J+4) - BIAS * REAL(WIDTH)

*  Scale to detected photons
              SPEC2(J)=SPEC2(J)*PHOTADU
              SPEC2(J+4)=SPEC2(J+4)*PHOTADU
          ENDDO

*  Determine star intensity
          STAR = SPEC2(1)+SPEC2(2)+SPEC2(5)+SPEC2(6)

*  Determine sky intensity
          SKY = SPEC2(3)+SPEC2(4)+SPEC2(7)+SPEC2(8)

*  Output intensity is star minus sky
          INT(I) = STAR-SKY

*  Variance is photon statistics (quadratic sum of star and sky photons)
*   plus readout noise (scaled by total number of pixels used, 8*width)
          VSTOKES(I) = SQRT(STAR*STAR+SKY*SKY)
     :        + 8.0*RDN*RDN*REAL(WIDTH)

*   Determine ratio
          RAT = (SPEC2(2)-SPEC2(4))/(SPEC2(1)-SPEC2(3))
          RAT = RAT/((SPEC2(6)-SPEC2(8))/(SPEC2(5)-SPEC2(7)))

*   Check that it is positive and we can take square root of it
          IF (RAT .LE. 0.0) THEN
              CALL MSG_SETI('CHAN',I)
              CALL MSG_OUT(' ','Negative square root in channel ^CHAN',
     :             STATUS)
              RAT = 0.0
          ELSE
              RAT = SQRT(RAT)
          ENDIF

*  Determine output Stokes parameter
          STOKES(I) = INT(I)*(RAT-1)/(RAT+1)

      ENDDO
      END

