*  History:
*     10 Dec 1993 (hme):
*        Change terminal output "^z" or "CTRL(Z)" to EOF.
*     31 Jan 1994 (hme):
*        Make PUSHED a logical variable.
*------------------------------------------------------------------------

      SUBROUTINE CALC_GAUSS (XSCALE, IFAIL)

C  Routine to generate selective sum of current gaussians in model

      INCLUDE  'SPECX_PARS'
      INCLUDE  'STACKCOMM'
      INCLUDE  'FLAGCOMM'

      INTEGER*4 LIMITS(2)
      REAL*4    XSCALE(*)
      CHARACTER XTITLE*10, XUNITS*6

      COMMON /LINFT/ NXOLD,NGOLD,PARAM(30)

      LOGICAL   PUSHED
      INTEGER   GEN_ILEN

C  Ok, go...

      IFAIL  = 0
      PUSHED = .FALSE.

      IXN    = GEN_ILEN (XAXIS_NAME)
      IXU    = GEN_ILEN (XAXIS_UNITS)
      XTITLE = XAXIS_NAME
      XUNITS = XAXIS_UNITS

C     Push the current scan into the stack and initialize array to zero

      CALL PUSH
      PUSHED = .TRUE.

      CALL SETXNEW (XSCALE, IFAIL)
      IF (IFAIL.NE.0) GO TO 999

      CALL INIT_ARRAY (NTOT(NQUAD), DATA, 0.0)

C     Find out which lines to use:

      ISTAT = 0
      DO WHILE (ISTAT.NE.2)
         IF (LIMITS(1).EQ.0) LIMITS(1) = 1
         CALL GEN_GETI4A
     &        ('Line or range of lines to model? (EOF to finish)',
     &          LIMITS, 2, 'I2'',''I2', LIMITS, ISTAT)
         IF (ISTAT.LT.0) THEN
           IFAIL = 18
           GO TO 999

         ELSE IF (ISTAT.LT.2) THEN
           LAST = LIMITS(2)
           IF (LIMITS(2).EQ.0) LAST = LIMITS(1)
           DO I = LIMITS(1), LAST
             DO NQ = 1,NQUAD
               IF (MASK(NQ).EQ.1)   THEN
                 AMP = PARAM(3*I-2)
                 WID = PARAM(3*I-1)/(2.*SQRT(ALOG(2.)))
                 POS = PARAM(3*I)
                 IF (WID.GT.0.0 .AND. AMP.NE.0.0) THEN
                   DO N = NTOT(NQ-1)+1,NTOT(NQ)
                     DATA(N) = DATA(N)
     &                         + AMP * EXP(-((XSCALE(N)-POS)/WID)**2)
                   END DO  ! channels
                 END IF
               END IF
             END DO  ! quadrants
           END DO  !  lines
         END IF
      END DO  !  ranges of lines

      RETURN

C     Error return

  999 CONTINUE

      IF (PUSHED) CALL POP

      RETURN
      END
