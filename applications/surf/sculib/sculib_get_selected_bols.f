*+  SCULIB_SELECTED_BOLS - copy bolometers belonging to a specified sub-
*                          instrument from the input data array to the output
      SUBROUTINE SCULIB_SELECTED_BOLS (N_BOL_IN, N_POS, IN_BOL_ADC,
     :  IN_BOL_CHAN, IN_DATA, IN_VARIANCE, IN_QUALITY, BOL_TYPE,
     :  SUB_INSTRUMENT, N_BOL_OUT, OUT_BOL_ADC, OUT_BOL_CHAN, 
     :  OUT_DATA, OUT_VARIANCE, OUT_QUALITY, STATUS)
*    Description :
*     <description of what the subroutine does>
*    Invocation :
*     CALL name[(argument_list)]
*    Parameters :
*     N_BOL_IN                     = INTEGER (Given)
*           number of bolometers in input array
*     N_POS                        = INTEGER (Given)
*           number of positions measured in input array
*     IN_BOL_ADC (N_BOL_IN)        = INTEGER (Given)
*           ADC numbers of bolometers in input array
*     IN_BOL_CHAN (N_BOL_IN)       = INTEGER (Given)
*           channel numbers of bolometers in input array
*     IN_DATA (N_BOL_IN,N_POS)     = REAL (Given)
*           input data array
*     IN_VARIANCE (N_BOL_IN,N_POS) = REAL (Given)
*           variance on IN_DATA
*     IN_QUALITY (N_BOL_IN,N_POS)  = INTEGER (Given)
*           quality on IN_DATA
*     BOL_TYPE (SCUBA__NUM_CHAN,SCUBA__NUM_ADC)
*                                  = CHARACTER*(*) (Given)
*           types of bolometers
*     SUB_INSTRUMENT               = CHARACTER*(*) (Given)
*           the name of the sub-instrument for which data are to be extracted
*     N_BOL_OUT                    = 
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     author (institution::username)
*    History :
*     $Id$
*     date:  changes (institution::username)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*     <any INCLUDE files containing global constant definitions>
*    Import :
*     <declarations and descriptions for imported arguments>
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
*     <declarations and descriptions for exported arguments>
*    Status :
*     <declaration for status argument>
*    External references :
*     <declarations for external function references>
*    Global variables :
*     <any INCLUDE files for global variables held in named COMMON>
*    Local Constants :
*    Local variables :
      INTEGER      IN_BOL                  ! bolometer index in input array
      INTEGER      IN_POINTER (MAX_BOL)    ! pointer from index in output array
                                           ! to index in input array
      INTEGER      OUT_BOL                 ! bolometer index in output array
      INTEGER      POS                     ! position index in DO loop
      CHARACTER*15 STEMP                   ! copy of SUB_INSTRUMENT
      CHARACTER*15 STEMP1                  ! scratch string
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

      STEMP = SUB_INSTRUMENT
      CALL CHR_UCASE (STEMP)

*  calculate the output ADC and channel arrays

      OUT_BOL = 0

      IF (N_BOL_IN .GT. 0) THEN

         DO IN_BOL = 1, N_BOL_IN
            STEMP2 = BOL_TYPE(IN_BOL_CHAN(IN_BOL),IN_BOL_ADC(IN_BOL))
            CALL CHR_UCASE (STEMP2)

            IF (STEMP .EQ. STEMP2) THEN

*  OK, bolometer belongs to the required sub-instrument, setup output chan
*  and ADC arrays

               OUT_BOL = OUT_BOL + 1
               OUT_BOL_CHAN(OUT_BOL) = IN_BOL_CHAN(IN_BOL)
               OUT_BOL_ADC(OUT_BOL) = IN_BOL_ADC(IN_BOL)
               IN_POINTER(OUT_BOL) = IN_BOL
            END IF

         END DO

      END IF

*  check that the output array is the right size

      IF (OUT_BOL .NE. N_BOL_OUT) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_SELECTED_BOLS: the size of '//
     :     'the output array does not match the number of '//
     :     'bolometers selected', STATUS)
      ELSE

*  copy over the required data

         IF (N_POS .GT. 0) THEN

            DO POS = 1, N_POS
               DO OUT_BOL = 1, N_OUT_BOL
                  OUT_BOL_DATA (OUT_BOL,POS) = 
     :              IN_BOL_DATA (IN_POINTER(OUT_BOL),POS)
                  OUT_BOL_VARIANCE (OUT_BOL,POS) =
     :              IN_BOL_VARIANCE (IN_POINTER(OUT_BOL),POS)
                  OUT_BOL_QUALITY (OUT_BOL,POS) =
     :              IN_BOL_QUALITY (IN_POINTER(OUT_BOL),POS)
               END DO
           END DO

        END IF
      END IF

      END
