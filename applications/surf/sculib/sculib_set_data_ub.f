      SUBROUTINE SCULIB_SET_DATA_UB (USE_THIS, IN_DATA, N_BOLS, N_POS, 
     :     N_BEAM, BOL_S, POS_S, BVALUE, STATUS)
*+
*  Name:
*     SCULIB_SET_QUAL

*  Purpose:
*     set data to a byte value given a mask 

*  Language:
*     Starlink Fortran 77
*  

*    Invocation :
*     CALL SCULIB_SET_DATA_UB(USE_THIS, IN_DATA, N_BOLS, N_POS, N_BEAM, 
*    :  BOL_S, POS_S, VALUE, STATUS)


*    Description :

*    Parameters :
*     USE_THIS   = LOGICAL (Given)
*           Describes whether the mask should be set to the value (TRUE)
*           or whether the rest of the data should be set (FALSE)
*     IN_DATA (N_BOLS, N_POS, N_BEAM)
*                             = BYTE (Given and returned)
*           the data to be masked
*     N_BOLS                  = INTEGER (Given)
*           number of bolometers measured
*     N_POS                   = INTEGER (Given)
*           number of positions measured
*     N_BEAM                  = INTEGER (Given)
*           number of beams used
*     BOL_S (N_BOLS)          = INTEGER (Given)
*           array containing 1s for bolometers whose quality is to be 
*           changed
*     POS_S (N_POS)           = INTEGER (Given)
*           array containing 1s for positions whose quality is to be
*           changed
*     BVALUE                  = BYTE (Given)
*           The value of the masked/unmasked data
*     STATUS                  = INTEGER (Given and returned)
*           global status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*       Tim Jenness (JACH)
*    History :
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*    Import :
      INTEGER N_BOLS
      INTEGER N_POS
      INTEGER N_BEAM
      INTEGER BOL_S (N_BOLS)
      INTEGER POS_S (N_POS)
      LOGICAL USE_THIS
      BYTE    BVALUE

*    Import-Export :
      BYTE    IN_DATA (N_BOLS, N_POS, N_BEAM)
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER BEAM                      ! beam index in DO loop
      INTEGER BOL                       ! bolometer index in DO loop
      INTEGER POS                       ! measured position index in DO loop
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

*     Do this if we are changing the section
      IF (USE_THIS) THEN

*     Set the masked data to the given value
         DO POS = 1, N_POS
            IF (POS_S(POS) .EQ. 1) THEN

               DO BOL = 1, N_BOLS
                  IF (BOL_S(BOL) .EQ. 1) THEN

                     DO BEAM = 1, N_BEAM
*       Dont set BAD pixels                        
                        IF (IN_DATA(BOL, POS, BEAM).NE.VAL__BADUB) THEN
                           IN_DATA(BOL, POS, BEAM) = BVALUE
                        END IF
                     END DO
         
                  END IF
               END DO

            END IF
         END DO

      ELSE

*     If the mask specified that the unmasked data is to be affected
*     then do this.

         DO POS = 1, N_POS

            IF (POS_S(POS) .EQ. 0) THEN
*       We KNOW that all this data is unmasked
               DO BOL = 1, N_BOLS
                  DO BEAM = 1, N_BEAM
                     IN_DATA(BOL, POS, BEAM) = BVALUE
                  END DO
               END DO
            ELSE
*       Only set to VALUE if not masked
               DO BOL = 1, N_BOLS
                  IF (BOL_S(BOL) .EQ. 0) THEN
                     DO BEAM = 1, N_BEAM
*       Dont set BAD pixels                        
                        IF (IN_DATA(BOL, POS, BEAM).NE.VAL__BADUB) THEN
                           IN_DATA(BOL, POS, BEAM) = BVALUE
                        END IF
                     END DO
                  END IF
               END DO
            END IF
         END DO

      END IF

      END
