
      SUBROUTINE TSP_PHSSCALE(SIZE,XB,Y,YE,IMIN,IMAX,STATUS)
*+
*
*  T S P _ P H S S C A L E
*
*  PHASEPLOT command - plot scaling calculations
*
*  Calculate Maximum and minimum values for plot scaling
*
*  (>) SIZE   (Integer)   Number of data points
*  (>) XB     (Double)    Array of binned X data
*  (>) Y      (Real)      Array of Y data
*  (>) YE     (Real)      Array of Y errors
*  (<) IMIN   (Real)      Minimum value for scaling
*  (<) IMAX   (Real)      Maximum value for scaling
*  (!) STATUS (Integer)   Status value
*
*  Jeremy Bailey   28/2/1988
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
      REAL Y(SIZE), YE(SIZE)
      DOUBLE PRECISION XB(SIZE)
      REAL IMIN,IMAX
      INTEGER STATUS

*  Local variables
      INTEGER I
      LOGICAL ERRORS

      IF (STATUS .EQ. SAI__OK) THEN

*  Get errors parameter

          CALL PAR_GET0L('ERRORS',ERRORS,STATUS)


*  Set initial maximum and minimum

          IMIN = VAL__MAXR
          IMAX = VAL__MINR

*  Find maximum and minimum ignoring points with no data

          DO I=1,SIZE
              IF (Y(I) .NE. VAL__BADR) THEN
                  IF (ERRORS) THEN
                      IF (Y(I)+YE(I) .GT. IMAX) IMAX = Y(I)+YE(I)
                      IF (Y(I)-YE(I) .LT. IMIN) IMIN = Y(I)-YE(I)
                  ELSE
                      IF (Y(I) .GT. IMAX) IMAX = Y(I)
                      IF (Y(I) .LT. IMIN) IMIN = Y(I)
                  ENDIF
              ENDIF
          ENDDO

      ENDIF
      END

