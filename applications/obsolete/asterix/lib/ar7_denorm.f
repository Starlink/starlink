*+  AR7_DENORM - Denormalises 7D data array with respect to given axis
      SUBROUTINE AR7_DENORM( WIDTH, DIMS, DIM, VAL, STATUS )
*
*    Description :
*
*     Denormalises data array
*
*    Authors :
*
*     A. McFadzean (BHVAD::ADM)
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     16 Aug 88 : Original ( ADM )
*     11 Oct 88 : Now copes with de-normalisation axis being any size. Also
*                 renamed to AR7_DENORM. ( DJA )
*     18 May 89 : STATUS added ( DJA )
*      7 Nov 89 : Workings pushed down one level (RJV)
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
      INTEGER                 DIMS(DAT__MXDIM)     ! size of each dimension
      INTEGER                 DIM                  ! no. of dimension being
                                                   ! de-normalised
      REAL                    WIDTH(*)             ! bin widths
*
*    Import-Export :
*
      REAL                    VAL(*)		   ! value array
*-

*    Check status
      IF ( STATUS .EQ. SAI__OK ) THEN

        CALL AR7_DENORM_INT(WIDTH,DIMS(1),DIMS(2),DIMS(3),DIMS(4),
     :                            DIMS(5),DIMS(6),DIMS(7),DIM,VAL)

      ENDIF

      END


*+  AR7_DENORM_INT
      SUBROUTINE AR7_DENORM_INT (WIDTH,L1,L2,L3,L4,L5,L6,L7,DIM,VAL)
*
*    Description :
*
*     Denormalises data array
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
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O) * WIDTH(I)

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
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O) * WIDTH(J)

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
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O) * WIDTH(K)

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
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O) * WIDTH(L)

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
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O) * WIDTH(m)

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
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O) * WIDTH(N)

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
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O) * WIDTH(O)

                    END DO
                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      END IF

      END
