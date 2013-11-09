*+  AR7_NORMV - Normalises VARIANCE for a given axis of a 7D array
      SUBROUTINE AR7_NORMV( WIDTH, DIMS, DIM, VAL, STATUS )
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
      REAL                    WIDTH(*)     ! Bin widths
*
*    Import-Export :
*
      REAL                    VAL(*)
*
*    Local Variables:
*
      INTEGER                 WDIM
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

        WDIM=DIMS(DIM)
        CALL AR7_NORMV_SUB(WDIM,WIDTH,DIMS(1),DIMS(2),DIMS(3),DIMS(4),
     :                              DIMS(5),DIMS(6),DIMS(7),DIM,VAL)

      ENDIF

      END

*+
      SUBROUTINE AR7_NORMV_SUB(WDIM,WIDTH,D1,D2,D3,D4,D5,D6,D7,DIM,VAL)
*
*    Type Definitions :
*
      IMPLICIT NONE

*    Import :
*
      INTEGER                 D1,D2,D3,D4,D5,D6,D7 ! Size of each dimension
      INTEGER                 DIM                  ! Dimension to n'ise
      INTEGER                 WDIM
      REAL                    WIDTH(WDIM)          ! Bin widths
*
*    Import-Export :
*
      REAL                    VAL(D1,D2,D3,D4,D5,D6,D7)
*
*    Local Variables:
*
      INTEGER                 I,J,K,L,M,N,O        ! Loop indices
*-

      DO O=1,D7
        DO N=1,D6
          DO M=1,D5
            DO L=1,D4
              DO K=1,D3
                DO J=1,D2
                  DO I=1,D1

                    IF ( DIM .EQ. 1 ) THEN
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O)/
     :                                                WIDTH(I)**2

                    ELSE IF ( DIM .EQ. 2 ) THEN
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O)/
     :                                                WIDTH(J)**2

                    ELSE IF ( DIM .EQ. 3 ) THEN
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O)/
     :                                                WIDTH(K)**2

                    ELSE IF ( DIM .EQ. 4 ) THEN
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O)/
     :                                                WIDTH(L)**2

                    ELSE IF ( DIM .EQ. 5 ) THEN
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O)/
     :                                                WIDTH(M)**2

                    ELSE IF ( DIM .EQ. 6 ) THEN
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O)/
     :                                                WIDTH(N)**2

                    ELSE IF ( DIM .EQ. 7 ) THEN
                      VAL(I,J,K,L,M,N,O) = VAL(I,J,K,L,M,N,O)/
     :                                                WIDTH(O)**2

                    ENDIF

                  ENDDO

                ENDDO

              ENDDO

            ENDDO

          ENDDO

        ENDDO

      ENDDO

      END
