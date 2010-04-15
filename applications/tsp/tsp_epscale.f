       SUBROUTINE TSP_EPSCALE(SIZE,INT,P,IMIN,IMAX,PMIN,PMAX,STATUS)
*+
*
*  T S P _ E P S C A L E
*
*   Determine maximum and minimum values of intensity and polarization
*   for use in the plot scaling
*
*   (>)  SIZE   (Integer)            Size of the arrays
*   (>)  INT    (Real array(SIZE))   Intensity array
*   (>)  P      (Real array(SIZE))   Percentage polarization array
*   (<)  IMIN   (Real)               Minimum intensity value
*   (<)  IMAX   (Real)               Maximum intensity value
*   (<)  PMIN   (Real)               Mininum polarization value
*   (<)  PMAX   (Real)               Maximum polarization value
*   (!)  STATUS (Integer)            Status value
*
*    Jeremy Bailey    12/7/1990
*
*    Modified:
*       6/12/1991    Handle bad values
*
*+
       IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
       INTEGER SIZE
       REAL INT(SIZE),P(SIZE)
       REAL IMIN,IMAX,PMIN,PMAX
       INTEGER STATUS

*  Local variables
       REAL RANGE
       INTEGER I

       IF (STATUS .EQ. SAI__OK) THEN


*  Set initial values
          IMIN = VAL__MAXR
          IMAX = VAL__MINR

*  Loop over good data values data replacing current value of IMAX with
*  any data value larger than IMAX, similarly for IMIN
          DO I = 2,SIZE-1
            IF (INT(I) .NE. VAL__BADR) THEN
              IF (INT(I) .GT. IMAX) THEN
                  IMAX=INT(I)
              ENDIF
              IF (INT(I) .LT. IMIN) THEN
                  IMIN=INT(I)
              ENDIF
            ENDIF
          ENDDO

*  Expand range slightly for tidier plot
          RANGE=IMAX-IMIN
          IMAX=IMAX+0.05*RANGE
          IMIN=IMIN-0.05*RANGE


*  Force PMIN to zero
          PMIN = 0.0

*  Set initial value for PMAX
          PMAX = VAL__MINR

*  Loop over good data values data replacing current value of PMAX with
*  any data value larger than PMAX,
          DO I = 1,SIZE
            IF (P(I) .NE. VAL__BADR) THEN
              IF (P(I) .GT. PMAX) THEN
                  PMAX=P(I)
              ENDIF
            ENDIF
          ENDDO

*  Expand range slightly
          RANGE=PMAX-PMIN
          PMAX=PMAX+0.05*RANGE
       ENDIF
       END
