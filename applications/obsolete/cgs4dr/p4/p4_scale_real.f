*+  P4_SCALE_REAL - get data from data file and scale for screen
      SUBROUTINE P4_SCALE_REAL ( DIM1, DIM2, DATA,
     :                LOW, HIGH, CMIN, CMAX, IDATA, STATUS )
*    Authors :
*      K. Krisciunas (JACH::KEVIN)
*      P. N. Daly (JACH::PND)
*    History :
*      02-Apr-1993: Original version (KLK)
*      06-Jul-1993: Add MIN function (PND)
*      20-Aug-1993: Add MAX function (PND)
*      11-Nov-1993: Adamize it and change logic (PND)
*      28-Mar-1994: If HIGH or LOW are at machine precision, abort (PND)
*       4-Aug-1994: Port to Unix (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'         ! Defines SAI__OK etc

      INTEGER DIM1, DIM2	! dimensions of data array
      REAL DATA (DIM1, DIM2)	! the data
      INTEGER IDATA(DIM1, DIM2)	! integer array for eventual PGPIXL call
      REAL LOW, HIGH		! range of data in data file
      INTEGER CMIN, CMAX	! minimum / maximum number of colors
      INTEGER STATUS
      INTEGER I, J, ITEMP
      REAL SCALE

*    Return if status entry is not OK
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set the scale factor
      SCALE = ( REAL(CMAX - CMIN) )  / ( HIGH - LOW )

*    Loop and set integer data array to scaled values
      DO J = 1, DIM2, 1
        DO I = 1, DIM1, 1
          ITEMP = NINT ( ( ( DATA(I,J) - LOW ) * SCALE ) + REAL(CMIN) )
          IF ( ITEMP .LT. CMIN ) THEN
             IDATA(I,J) = CMIN
          ELSE IF ( ITEMP .GT. CMAX ) THEN
             IDATA(I,J) = CMAX
          ELSE
             IDATA(I,J) = ITEMP
          ENDIF
        ENDDO
      ENDDO

*    Exit subroutine
      END
