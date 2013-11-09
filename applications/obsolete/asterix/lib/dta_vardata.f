*+DTA_VARDATA - Sets variance values from the data array
      SUBROUTINE DTA_VARDATA(IDIM1, IDIM2, IDIM3, IDIM4, IDIM5,
     &                                   IDIM6, IDIM7, DATA, VAR)
*    Description :
*     Copies the data array into the variance array but sets the
*     variance to one if the data is zero. NB: This assumes that
*     the data is raw i.e. is purely dominated by photon statistics
*    History :
*     May 6 1992 original (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      INTEGER IDIM1,IDIM2,IDIM3,IDIM4,IDIM5,IDIM6,IDIM7    ! Dimensions of arrays
      REAL DATA(IDIM1,IDIM2,IDIM3,IDIM4,IDIM5,IDIM6,IDIM7) ! Input data array
*    Import-Export :
*    Export :
      REAL VAR(IDIM1,IDIM2,IDIM3,IDIM4,IDIM5,IDIM6,IDIM7)  ! Variance array
*    Local Constants :
*    Local variables :
      INTEGER  LP1,LP2,LP3,LP4,LP5,LP6,LP7                 ! Variables for LP.
*    Data :
*-
* Loop over the data
      DO LP7=1,IDIM7
       DO LP6=1,IDIM6
        DO LP5=1,IDIM5
         DO LP4=1,IDIM4
          DO LP3=1,IDIM3
           DO LP2=1,IDIM2
            DO LP1=1,IDIM1
*
               IF (DATA(LP1,LP2,LP3,LP4,LP5,LP6,LP7) .NE. 0) THEN

                  VAR(LP1,LP2,LP3,LP4,LP5,LP6,LP7) =
     &                     DATA(LP1,LP2,LP3,LP4,LP5,LP6,LP7)

               ELSE
*
*           Set the variance to be one photon
                  VAR(LP1,LP2,LP3,LP4,LP5,LP6,LP7) = 1.0

               ENDIF
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      END
