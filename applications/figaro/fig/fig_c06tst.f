C+
      LOGICAL FUNCTION FIG_C06TST(N)
C
C     F I G _ C 0 6 T S T
C
C     Tests a single number for conformance with the NAG C06xxx routine
C     criterion for factorisation.  This is designed to be called by
C     FIG_C06CHK, so see that routine for more details.
C
C     Well, NAG has left the Figaro scene, and there are no restrictions
C     any more. This routine now always returns `true'.
C
C     Parameters   (">" input, "<" output)
C
C     (>) N           (Integer) The number to be tested.
C
C     Returns
C
C     (<) FIG_C06TST  (Logical) True if N conforms to the C06 requirements.
C
C                                         KS / AAO  4th Sept 1986.
C
C     This is a re-packaging of an original routine by JOS / AAO.
C+
      IMPLICIT NONE
      INTEGER N
      FIG_C06TST = .TRUE.
      RETURN

C
C     Follows the old routine:
C
c     IMPLICIT NONE
C
C     Parameters
C
c     INTEGER N
C
C     Local variables
C
c     INTEGER FACTORS, LFX, X
c     INTEGER FACTOR(8) / 2, 3, 5, 7, 11, 13, 17, 19/
c     LOGICAL TESTED
C
C
C     Copy of N to divide down by found factors
C
c     X = N
c     FACTORS = 0
C
C     Start with smallest factor
C
c     LFX = 1
C
C     Treat 1 as a special case and trap the case 0 >= N
C
c     IF (N.EQ.1) THEN
c        TESTED = .TRUE.
c        FIG_C06TST = .TRUE.
c     ELSE IF (N.LE.0) THEN
c        TESTED = .TRUE.
c        FIG_C06TST = .FALSE.
c     ELSE
c        TESTED = .FALSE.
c     END IF
C
c     DO WHILE (.NOT.TESTED)
C
C        Finshed and result found if is a factor
C
c        IF (X .EQ. FACTOR(LFX)) THEN
c           FIG_C06TST = .TRUE.
c           TESTED = .TRUE.
c        ELSE
C
C           Can it be divided by this factor?
C
c           IF (MOD(X,FACTOR(LFX)) .EQ. 0) THEN
c              X = X/FACTOR(LFX)                          ! YES - divide
C
C              Too many factors?
C
c              FACTORS = FACTORS + 1
c              IF (FACTORS.GT.20) THEN                    ! Yes, break out
c                 TESTED = .TRUE.
c                 FIG_C06TST = .FALSE.
c              END IF
c           ELSE
c              LFX = LFX + 1                              ! NO - next ...
c              IF (LFX.GT.8) THEN                         ! ... factor
c                 TESTED = .TRUE.
c                 FIG_C06TST = .FALSE.
c              END IF
c           END IF                                        ! ... factor
c        END IF
c     END DO
C
      END
