       SUBROUTINE TSP_QUPLTSCALE(SIZE,Q,U,QMIN,QMAX,UMIN,UMAX,STATUS)
*+
*
*  T S P _ Q U P L T S C A L E
*
*   Determine maximum and minimum values of the Qand U arrays
*   for use in the plot scaling
*
*   (>)  SIZE   (Integer)            Size of the arrays
*   (>)  Q      (Real array(SIZE))   Q array
*   (>)  U      (Real array(SIZE))   U array
*   (<)  QMIN   (Real)               Minimum Q value
*   (<)  QMAX   (Real)               Maximum Q value
*   (<)  UMIN   (Real)               Mininum U value
*   (<)  UMAX   (Real)               Maximum U value
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
       REAL Q(SIZE),U(SIZE)
       REAL QMIN,QMAX,UMIN,UMAX
       INTEGER STATUS

*  Local variables
       INTEGER I
       REAL RANGE

       IF (STATUS .EQ. SAI__OK) THEN

*  Set initial max and min values

          QMIN = VAL__MAXR
          QMAX = VAL__MINR

*  Loop over good data values data replacing current value of QMAX with
*  any data value larger than QMAX, similarly for QMIN

          DO I = 1,SIZE
            IF (Q(I) .NE. VAL__BADR) THEN
              IF (Q(I) .GT. QMAX) THEN
                  QMAX=Q(I)
              ENDIF
              IF (Q(I) .LT. QMIN) THEN
                  QMIN=Q(I)
              ENDIF
            ENDIF
          ENDDO

*  Expand range slightly for tidier plot

          RANGE=QMAX-QMIN
          QMAX=QMAX+0.05*RANGE
          QMIN=QMIN-0.05*RANGE

*  Set initial max and min values

          UMIN = VAL__MAXR
          UMAX = VAL__MINR

*  Loop over good data values data replacing current value of UMAX with
*  any data value larger than UMAX, similarly for UMIN

          DO I = 1,SIZE
            IF (U(I) .NE. VAL__BADR) THEN
              IF (U(I) .GT. UMAX) THEN
                  UMAX=U(I)
              ENDIF
              IF (U(I) .LT. UMIN) THEN
                  UMIN=U(I)
              ENDIF
            ENDIF
          ENDDO

*  Expand range slightly for tidier plot

          RANGE=UMAX-UMIN
          UMAX=UMAX+0.05*RANGE
       ENDIF
       END

