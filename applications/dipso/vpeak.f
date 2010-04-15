      SUBROUTINE VPEAK (NPT, X, Y)
C
C  Subroutine searches for the highest peak in a series of (x,y) datapairs
C and finds the x-value at which the peak is a maximum by fitting a parabola
C to its central 3 points by the method of least squares.
C  Required subroutine: PDGFIT
C
      IMPLICIT NONE
C Imports:
      INTEGER NPT                         ! number of (x,y) datapairs
      REAL X(*)                           ! array of data x-values
      REAL Y(*)                           ! array of data y-values
C Local:
      INTEGER MAXDEX                      ! index of maximum y-value in array Y
      DOUBLE PRECISION XPEAK(3)           ! array to hold 3 x-values of peak
      DOUBLE PRECISION YPEAK(3)           ! array to hold 3 y-values of peak
      DOUBLE PRECISION SDS(3)             ! array of standard deviations
      DOUBLE PRECISION COEF(3)            ! array of coefficients
      DOUBLE PRECISION DUM                ! useless extra variable
      DOUBLE PRECISION DUM1               ! another useless extra variable
      DOUBLE PRECISION DUMM
      INTEGER I                           ! do-loop variable
C
C  Find index of maximum y-value: this will mark centre of peak.
      MAXDEX = 1
      DO I = 2,NPT
        IF ( Y(I) .GT. Y(MAXDEX) )  MAXDEX = I
      ENDDO
      IF ((MAXDEX .EQ. 1) .OR. (MAXDEX .EQ. NPT)) THEN
        WRITE (*,
     :  '(''   PDGPEAK:  peak is at end of data;  no parabolic fit'')')
        WRITE (*,
     :  '(''   PDGPEAK:  peak y value ='',1PE12.3)') Y(MAXDEX)
        WRITE (*,
     :  '(''               at x value ='',1PE12.3)') X(MAXDEX)
        RETURN
      ENDIF
C
C  Set up peak arrays.
      DO I = 1,3
        SDS(I) = 1.0D+00
        XPEAK(I) = X(MAXDEX-2+I)
        YPEAK(I) = Y(MAXDEX-2+I)
      ENDDO
C
C  Get parabola (2nd-order least squares polynomial) fit for peak data.
      CALL PDGFIT (3, XPEAK, YPEAK, SDS, 2, DUM, DUM1, COEF)
C
C  Find position of peak implied by shift coefficients.
      DUM = -0.5D+00 * COEF(2)/COEF(3)
      DUMM = DUM
      DUM = COEF(1) + COEF(2)*DUM + COEF(3)*DUM**2
      WRITE (*,
     : '(''   PDGPEAK:  peak y value ='',1PE12.3)') REAL(DUM)
       WRITE (*,
     : '(''              at x value ='',1PE12.3)') REAL(DUMM)
      RETURN
      END
