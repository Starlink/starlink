*+  P4_EXTRACT_CT - Get red, green, blue values from file
      SUBROUTINE P4_EXTRACT_CT( DATA, DIM1, DIM2, RED, GREEN, BLUE, STATUS )
*    Authors :
*      K. Krisciunas (JACH::KEVIN)
*    History :
*      07-Apr-1993: original version
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER STATUS
      INTEGER DIM1, DIM2	! size of data array
      REAL DATA (DIM1,DIM2)	! data from file, range 0.0 to 255.0
      REAL RED (256)		! data scaled 0.0 to 1.0
      REAL GREEN(256)		!          "
      REAL BLUE(256)		!          "
      INTEGER J

*    Return if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set the colour levels
      DO J = 1,DIM2,1
        RED(J) = DATA(1,J) / 255.0
        GREEN(J) = DATA(2,J) / 255.0
        BLUE(J) = DATA(3,J) / 255.0
      ENDDO

      END
