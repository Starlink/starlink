*+  AR7_NORM - Normalises data array for a given axis of a 7D array
      SUBROUTINE AR7_NORM( WIDTH, DIMS, DIM, VAL, STATUS )
*
*    Description :
*
*     Normalises data array about given axis
*
*    Authors :
*
*     David Allan  (BHVAD::DJA)
*
*    History :
*
*     11 Oct 88 : Original ( DJA )
*     18 May 89 : STATUS added ( DJA )
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER                 STATUS
*
*    Import :
*
      INTEGER                 DIMS(DAT__MXDIM)	   ! Size of each dimension
      INTEGER                 DIM                  ! Dimension to n'ise
      REAL                    WIDTH(*)             ! Bin widths
*
*    Import-Export :
*
      REAL                    VAL(*)
*-

      IF ( STATUS .EQ. SAI__OK ) THEN
        CALL AR7_NORM_SUB(WIDTH,DIMS(1),DIMS(2),DIMS(3),DIMS(4),
     :                          DIMS(5),DIMS(6),DIMS(7),DIM,VAL)
      END IF

      END


*+  AR7_NORM_SUB
      SUBROUTINE AR7_NORM_SUB(WIDTH,L1,L2,L3,L4,L5,L6,L7,DIM,VAL)
*
*    Description :
*
*     Normalises data array
*
*    Authors :
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      INTEGER                 L1,L2,L3,L4,L5,L6,L7 ! size of each dimension
      INTEGER                 DIM                  ! no. of dimension being
                                                   ! de-normalised
      REAL                    WIDTH(*)             ! bin widths
*
*    Import-Export :
*
      REAL                    VAL(L1,L2,L3,L4,L5,L6,L7)	   ! value array
*
*    Local Variables:
*
      INTEGER I,J,K,L,M,N,O
*-

      IF (DIM .EQ. 1) THEN
        DO O = 1, L7
          DO N = 1, L6
            DO M = 1, L5
              DO L = 1, L4
                DO K = 1, L3
                  DO J = 1, L2
                    DO I = 1, L1
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O) / WIDTH(I)

                    END DO
                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      ELSE IF (DIM .EQ. 2) THEN
        DO O = 1, L7
          DO N = 1, L6
            DO M = 1, L5
              DO L = 1, L4
                DO K = 1, L3
                  DO J = 1, L2
                    DO I = 1, L1
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O) / WIDTH(J)

                    END DO
                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      ELSE IF (DIM .EQ. 3) THEN
        DO O = 1, L7
          DO N = 1, L6
            DO M = 1, L5
              DO L = 1, L4
                DO K = 1, L3
                  DO J = 1, L2
                    DO I = 1, L1
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O) / WIDTH(K)

                    END DO
                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      ELSE IF (DIM .EQ. 4) THEN
        DO O = 1, L7
          DO N = 1, L6
            DO M = 1, L5
              DO L = 1, L4
                DO K = 1, L3
                  DO J = 1, L2
                    DO I = 1, L1
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O) / WIDTH(L)

                    END DO
                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      ELSE IF (DIM .EQ. 5) THEN
        DO O = 1, L7
          DO N = 1, L6
            DO M = 1, L5
              DO L = 1, L4
                DO K = 1, L3
                  DO J = 1, L2
                    DO I = 1, L1
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O) / WIDTH(m)

                    END DO
                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      ELSE IF (DIM .EQ. 6) THEN
        DO O = 1, L7
          DO N = 1, L6
            DO M = 1, L5
              DO L = 1, L4
                DO K = 1, L3
                  DO J = 1, L2
                    DO I = 1, L1
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O) / WIDTH(N)

                    END DO
                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      ELSE IF (DIM .EQ. 7) THEN
        DO O = 1, L7
          DO N = 1, L6
            DO M = 1, L5
              DO L = 1, L4
                DO K = 1, L3
                  DO J = 1, L2
                    DO I = 1, L1
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O) / WIDTH(O)

                    END DO
                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      END IF

      END
