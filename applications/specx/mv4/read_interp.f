*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------
      SUBROUTINE READ_INTERP_DATA (NDATA, M, N, BUF1, BUF2, N1, N2,
     &           HIT_DATA, INTERP)

*     Interpolate spectrum

      IMPLICIT   NONE

*     Formal parameters

      INTEGER    NDATA
      INTEGER    M, N
      REAL       BUF1(*)
      REAL       BUF2(*)
      INTEGER    N1, N2
      LOGICAL    INTERP
      LOGICAL    HIT_DATA

*     Common blocks

      INCLUDE 'CUBE'
      INCLUDE 'MAPS'
      INCLUDE 'MAPHD'
      INCLUDE 'WEIGHTS'
      INCLUDE 'CNF_PAR'

*     Local variables

      INTEGER    I, J, K
      INTEGER    MMIN, MMAX
      INTEGER    NMIN, NMAX
      INTEGER    IJOFFSET
      INTEGER    IJPOS
      REAL       CURRENT_WEIGHT
      REAL       SIGMA_W
      LOGICAL    GXMINUS, GXPLUS
      LOGICAL    GYMINUS, GYPLUS

*  Ok, go...

      INTERP = .FALSE.

      MMIN = MAX (M-IXMAX, 1)
      MMAX = MIN (M+IXMAX, MSTEP)

      NMIN = MAX (N-IYMAX, 1)
      NMAX = MIN (N+IYMAX, NSTEP)

      SIGMA_W = 0.0
      CALL INIT_ARRAY (NDATA, BUF2, 0.0)

      GXMINUS = .FALSE.
      GXPLUS  = .FALSE.
      GYMINUS = .FALSE.
      GYPLUS  = .FALSE.

*     IF (M.GE.32 .and. M.LE.45 .and. N.eq.52) THEN
*       PRINT *
*       PRINT *, 'M, N = ', M, N
*       PRINT *, 'MMIN,  MMAX  = ', MMIN,  MMAX
*       PRINT *, 'NMIN,  NMAX  = ', NMIN,  NMAX
*     END IF

      DO J = NMIN, NMAX
        DO I = MMIN, MMAX

          IJOFFSET = 4*((J-1)*MSTEP + (I-1))
          CALL XCOPY (4,
     :         %VAL(CNF_PVAL(CURRENT_INDEX_ADDRESS)+IJOFFSET), IJPOS)

*         IF (M.GE.32 .and. M.LE.45 .and. N.eq.52) THEN
*           PRINT *,'Index value for (i,j) = ',I,J,' =',IJPOS
*         END IF

          IF (IJPOS.GT.0) THEN

            HIT_DATA = .TRUE.

            IF (I.LE.M) GXMINUS = .TRUE.
            IF (I.GE.M) GXPLUS  = .TRUE.
            IF (J.LE.N) GYMINUS = .TRUE.
            IF (J.GE.N) GYPLUS  = .TRUE.

            CALL READ_NEW_CUBE2 (I, J, NDATA, N1, N2, BUF1)

            CURRENT_WEIGHT = WEIGHT(IABS(M-I),IABS(N-J))
            SIGMA_W = SIGMA_W + CURRENT_WEIGHT
            DO K = 1,NDATA
              BUF2(K) = BUF2(K) + CURRENT_WEIGHT*BUF1(K)
            END DO

          END IF
        END DO
      END DO

*     IF (M.GE.32 .and. M.LE.45 .and. N.eq.52) THEN
*       PRINT *, 'GXM, GXP, GYM, GYP = ', GXMINUS,GXPLUS,GYMINUS,GYPLUS
*     END IF

      IF (      GXMINUS .AND. GXPLUS
     &    .AND. GYMINUS .AND. GYPLUS) THEN
        INTERP = .TRUE.
        DO K = 1,NDATA
          BUF2(K) = BUF2(K)/SIGMA_W
        END DO
*       PRINT *, 'Interp set for array element (m,n) =', M, N
      END IF

      RETURN
      END
