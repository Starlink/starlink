      SUBROUTINE SCULIB_SET_DATA (USE_THIS, N_BOLS, N_POS,
     :     N_BEAM, MASK, VALUE, IN_DATA, STATUS)
*+
*  Name:
*     SCULIB_SET_QUAL

*  Purpose:
*     set data to a real value given a byte mask 

*  Language:
*     Starlink Fortran 77
*  

*    Invocation :
*     CALL SCULIB_SET_DATA (USE_THIS, IN_DATA, N_PTS, N_BEAM, MASK, 
*    :       VALUE, STATUS)

*    Description :
*       This routine uses a byte mask (N_BOLS * N_POS) to set a data
*       value in the output. A mask value of 1 indicates that a value
*       should be changed. This routine does not distinguish 'beams'

*    Parameters :
*     USE_THIS   = LOGICAL (Given)
*           Describes whether the mask should be set to the value (TRUE)
*           or whether the rest of the data should be set (FALSE)
*     IN_DATA (N_BOLS, N_POS, N_BEAM)
*                             = REAL (Given and returned)
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
*     VALUE               = REAL (Given)
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
      BYTE    MASK (N_BOLS, N_POS) 
      LOGICAL USE_THIS
      REAL    VALUE

*    Import-Export :
      REAL    IN_DATA (N_BOLS, N_POS, N_BEAM)
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

         DO BOL = 1, N_BOLS

            DO POS = 1, N_POS

               IF (MASK(BOL,POS) .NE. 0) THEN

                  DO BEAM = 1, N_BEAM
*       Dont set BAD pixels                        
                     IF (IN_DATA(BOL, POS, BEAM) .NE. VAL__BADR) THEN
                        IN_DATA(BOL, POS, BEAM) = VALUE
                     END IF
                  END DO
               END IF
            END DO
         END DO

*     Set unmasked data to value
      ELSE

         DO BOL = 1, N_BOLS
            DO POS = 1, N_POS

               IF (MASK(BOL, POS) .EQ. 0) THEN

                  DO BEAM = 1, N_BEAM
*     Dont set BAD pixels                        
                     IF (IN_DATA(BOL, POS, BEAM) .NE. VAL__BADR) THEN
                        IN_DATA(BOL, POS, BEAM) = VALUE
                     END IF
                  END DO
               END IF
            END DO
         END DO

      END IF

      END
