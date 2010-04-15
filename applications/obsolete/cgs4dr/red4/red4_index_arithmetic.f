*+ RED4_INDEX_ARITHMETIC
      SUBROUTINE RED4_INDEX_ARITHMETIC (OPERATION, INTDATA, INTQUAL,
     :   INTVAR, INTDIM1, INTDIM2, INDEX, INDEX_DIM1, INDEX_DIM2,
     :   DET_INDEX, OBSDATA, OBSQUAL, OBSVAR, OBSDIM1, OBSDIM2, STATUS)
*    Description :
*     This subroutine performs the 4 arithmetic functions, ADD, SUBTRACT,
*     MULTIPLY and DIVIDE between an integration array and those components
*     of a full observation result array that were taken at the same
*     detector position
*    Invocation :
*     CALL RED4_INDEX_ARITHMETIC (OPERATION, INTDATA, INTQUAL,
*    :   INTVAR, INTDIM1, INTDIM2, INDEX, INDEX_DIM1, INDEX_DIM2,
*    :   DET_INDEX, OBSDATA, OBSQUAL, OBSVAR, OBSDIM1, OBSDIM2, STATUS)
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     More comments needed.
*     This could be another routine which might require local double
*     precision variables.
*     It would have been more efficient to have used integer codes
*     for OPERATION rather than a character.
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Lightfoot (REVAD::JFL)
*     Steven Beard (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*          1989 ?: Original version.                         (JFL)
*      3-Sep-1990: History added. GOOD and BAD parameters
*                  added. Code spaced out. Unused variables
*                  removed. Some comments added. Code
*                  modified so the bad practice of testing
*                  a REAL variable for equality is avoided.  (SMB)
*     19-Feb-1993: Conform to error strategy                 (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'PRM_PAR'
*    Status :
      INTEGER STATUS
*    Input :
      CHARACTER*(*) OPERATION            ! operation to be performed:-
*                                             ADD - add the OBS to the INT
*                                             SUBTRACT - sub the OBS from the INT
*                                             MULTIPLY - mult the INT by the OBS
*                                             DIVIDE - divide the INT by the OBS
      INTEGER INTDIM1, INTDIM2           ! dims of integration data
      INTEGER INDEX_DIM1, INDEX_DIM2     ! dims of index array
      INTEGER*2 INDEX(INDEX_DIM1,INDEX_DIM2) ! the array that points to the
*                                              columns of the observation data
*                                              that correspond to integrations
*                                              taken at various detector positions
      INTEGER DET_INDEX                   ! the detector position at which this
*                                              integration was taken
*
      INTEGER OBSDIM1, OBSDIM2            ! dims of observation data
      REAL OBSDATA(OBSDIM1,OBSDIM2)       ! observation data array
      BYTE OBSQUAL(OBSDIM1,OBSDIM2)       ! observation data quality
      REAL OBSVAR(OBSDIM1,OBSDIM2)        ! observation data variances
*    Input-Output:
      REAL INTDATA(INTDIM1,INTDIM2)       ! integration data array
      BYTE INTQUAL(INTDIM1,INTDIM2)       ! integration quality array
      REAL INTVAR(INTDIM1,INTDIM2)        ! integration data variances

*    External references :
*    Global variables :
*    Local Constants :
      BYTE GOOD, BAD                      ! Good and bad quality values
      PARAMETER ( GOOD = 0,
     :            BAD  = 1 )

*    Local variables :
      INTEGER I, J                        ! DO loop
      INTEGER IPOS                        ! position in observation data array
*                                              of column in integration array
      INTEGER QUAL1, QUAL2                ! temporary storage for quality
      REAL SQUARE
*    Internal References :
*    Local data :
*-

*    Return if entry status bad
      IF (STATUS .NE. ADAM__OK) RETURN

*   Decide which operation is to be performed.
      IF ( OPERATION .EQ. 'ADD' ) THEN

*      Add the integration and observation data arrays at the same index
*      positions, and add the variance arrays.
         DO J = 1, INTDIM2
            DO I = 1, INTDIM1

               IPOS = INDEX(I,DET_INDEX)
               QUAL1 = INTQUAL(I,J)
               QUAL2 = OBSQUAL(IPOS,J)
               INTQUAL(I,J) = IOR (QUAL1, QUAL2)

               IF ( INTQUAL(I,J) .EQ. GOOD ) THEN

                  INTDATA(I,J) = INTDATA(I,J) + OBSDATA(IPOS,J)
                  INTVAR(I,J) = INTVAR(I,J) + OBSVAR(I,J)
               ELSE

                  INTDATA(I,J) = 0.0
                  INTVAR(I,J) = 0.0
               ENDIF
            END DO
         END DO
      ELSE IF ( OPERATION .EQ. 'SUBTRACT' ) THEN

*      Subtract the observation data array from the integration data
*      array at the same index positions, and add the variance arrays.
         DO J = 1, INTDIM2
            DO I = 1, INTDIM1

               IPOS = INDEX(I,DET_INDEX)
               QUAL1 = INTQUAL(I,J)
               QUAL2 = OBSQUAL(IPOS,J)
               INTQUAL(I,J) = IOR (QUAL1, QUAL2)

               IF ( INTQUAL(I,J) .EQ. GOOD ) THEN

                  INTDATA(I,J) = INTDATA(I,J) - OBSDATA(IPOS,J)
                  INTVAR(I,J) = INTVAR(I,J) + OBSVAR(I,J)
               ELSE

                  INTDATA(I,J) = 0.0
                  INTVAR(I,J) = 0.0
               ENDIF
            END DO
         END DO
      ELSE IF ( OPERATION .EQ. 'MULTIPLY' ) THEN

*      Multiply the integration and observation data arrays at the same index
*      positions, and combine the variance arrays.
         DO J = 1, INTDIM2
            DO I = 1, INTDIM1

               IPOS = INDEX(I,DET_INDEX)
               QUAL1 = INTQUAL(I,J)
               QUAL2 = OBSQUAL(IPOS,J)
               INTQUAL(I,J) = IOR (QUAL1, QUAL2)

               IF ( INTQUAL(I,J) .EQ. GOOD ) THEN

                  INTVAR(I,J) =
     :               INTVAR(I,J)*OBSDATA(IPOS,J)*OBSDATA(IPOS,J) +
     :               OBSVAR(IPOS,J)*INTDATA(I,J)*INTDATA(I,J)

                  INTDATA(I,J) = INTDATA(I,J) * OBSDATA(IPOS,J)
               ELSE

                  INTVAR(I,J) = 0.0
                  INTDATA(I,J) = 0.0
               ENDIF
            END DO
         END DO
      ELSE IF ( OPERATION .EQ. 'DIVIDE' ) THEN

*      Divide the integration data array by the observation data array
*      at the same index positions, and combine the variance arrays.
*      If a divide by zero would occur, set the quality of the result
*      to BAD.
         DO J = 1, INTDIM2
            DO I = 1, INTDIM1

               IPOS = INDEX(I,DET_INDEX)
               QUAL1 = INTQUAL(I,J)
               QUAL2 = OBSQUAL(IPOS,J)
               INTQUAL(I,J) = IOR (QUAL1, QUAL2)

               IF ( INTQUAL(I,J) .EQ. GOOD ) THEN

                  SQUARE = OBSDATA(IPOS,J)*OBSDATA(IPOS,J)

                  IF ( ABS(SQUARE) .GT. VAL__MINR ) THEN

                     INTVAR(I,J) =
     :                  INTVAR(I,J)/SQUARE +
     :                  OBSVAR(IPOS,J)*INTDATA(I,J)*INTDATA(I,J)/
     :                  (SQUARE*SQUARE)

                     INTDATA(I,J) = INTDATA(I,J) / OBSDATA(IPOS,J)
                  ELSE

                     INTVAR(I,J) = 0.0
                     INTDATA(I,J) = 0.0
                     INTQUAL(I,J) = BAD
                  ENDIF
               ELSE

                  INTVAR(I,J) = 0.0
                  INTDATA(I,J) = 0.0
               ENDIF
            END DO
         END DO
      ENDIF

      END
