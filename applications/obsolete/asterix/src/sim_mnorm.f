*+  SIM_MNORM - Convert model to cumulative probability
      SUBROUTINE SIM_MNORM( N, MODEL, QOK, QUAL, INDEX, TOT, STATUS )
*
*    Description :
*
*     Converts a model to a cumulative probability distribution,
*     and creates an indexing array.
*
*    History :
*
*     25 Jul 91 : Original (DJA)
*     22 Feb 93 : Now calculates probability bin boundaries (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER                     N                   ! # values
      LOGICAL                     QOK                 ! Use model quality?
      LOGICAL                     QUAL(N)             ! Model quality array
      REAL                        MODEL(N)            ! Model array
*
*    Export :
*
      REAL                        INDEX(N+1)          ! Probability index array
      REAL                        TOT                 ! Sum in array
*
*    Local variables :
*
      DOUBLE PRECISION            ACCIN               ! Index accumulator

      INTEGER                     I                   ! Loop variable
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Quality present?
      TOT = 0.0
      IF ( QOK ) THEN

*      Find total in good data
        DO I = 1, N
          IF ( QUAL(I) ) TOT = TOT + MODEL(I)
        END DO

*      Create normalised distribution
        INDEX(1) = MODEL(1)/TOT
        DO I = 2, N
          IF ( QUAL(I) ) THEN
            INDEX(I) = INDEX(I-1)+MODEL(I)/TOT
          ELSE
            INDEX(I) = INDEX(I-1)
          END IF
        END DO

      ELSE
        DO I = 1, N
          TOT = TOT + MODEL(I)
        END DO

*      Create normalised distribution
        INDEX(1) = 0.0
        ACCIN = 0.0D0
        DO I = 2, N+1
          ACCIN = ACCIN + DBLE(MODEL(I-1)/TOT)
          INDEX(I) = REAL(ACCIN)
        END DO

      END IF

      END
