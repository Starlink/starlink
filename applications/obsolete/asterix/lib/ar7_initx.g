*+  AR7_INIT<T> - Initialise 7-D array with <TYPE> value
      SUBROUTINE AR7_INIT<T>(VAL,DIMS,X,STATUS)
*    Description :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
      INTEGER STATUS
*    Import :
      <TYPE> VAL			! value to be inserted
      INTEGER DIMS(7)		! size of array
*    Import-Export :
      <TYPE> X(*)			! array
*    Local Variables:
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL AR7_INIT<T>_INT(VAL,DIMS(1),DIMS(2),DIMS(3),DIMS(4),
     :                                 DIMS(5),DIMS(6),DIMS(7),X)

      ENDIF

      END


*+  AR7_INIT<T>_INT
      SUBROUTINE AR7_INIT<T>_INT(VAL,D1,D2,D3,D4,D5,D6,D7,X)
*    Description :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      <TYPE> VAL			! value to be inserted
      INTEGER D1,D2,D3,D4,D5,D6,D7	! size of array
*    Import-Export :
      <TYPE> X(D1,D2,D3,D4,D5,D6,D7)	! array
*    Local Variables:
      INTEGER I,J,K,L,M,N,O
*-
      DO I=1,D7
        DO J=1,D6
          DO K=1,D5
            DO L=1,D4
              DO M=1,D3
                DO N=1,D2
                  DO O=1,D1
                    X(O,N,M,L,K,J,I)=VAL
                  ENDDO
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      END
