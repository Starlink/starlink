*  History:
*     19 Nov 1993 (hme):
*        TABs removed.
*     20 July 2000 (ajc):
*        Missing commas in FORMAT
*        Change TYPE * to PRINT *
*        Unused IFORM, FORMAT, IANS, PROMPT
C------------------------------------------------------------------------------

      SUBROUTINE LINFIT (NQ, XSCALE, BUF, IFAIL)

C   This subroutine performs a least squares fit of a sum of gaussians
C   to the data in the X-register of 'STACK'
C
C     Formal parameters:
C        XSCALE  - values of independent co-ordinate at each data pt.
C        BUF     - workspace array
C        IFAIL   - set to 0 on output if b-l removal successful
C

      IMPLICIT  NONE

*     Formal parameters

      INTEGER   NQ
      REAL      XSCALE(*)
      REAL      BUF(*)
      INTEGER   IFAIL

*     Common blocks

      INCLUDE 'FLAGCOMM'
      INCLUDE 'SPECX_PARS'
      INCLUDE 'STACKCOMM'
      INCLUDE 'STAKPAR'
      INCLUDE 'CNF_PAR'

      REAL      XSC
      REAL      Y
      INTEGER   N
      INTEGER   NPARAM

      COMMON /GFUNC/  XSC(LSPMAX),Y(LSPMAX),N,NPARAM

      INTEGER   NXOLD
      INTEGER   NG
      REAL      PARAM(30)
      REAL      VAR(10,3)
      COMMON /LINFT/  NXOLD,NG,PARAM,VAR


      REAL      RESID(LSPMAX)
      REAL      C(30)
      REAL      G(LSPMAX)

      COMMON /WORK/   RESID,C,G

*     Local variables

      INTEGER   I, J
      INTEGER   IERX
      INTEGER   ITS
      INTEGER   IAPTR
      INTEGER   IWPTR
      INTEGER   NBA
      INTEGER   NBW
      INTEGER   NDEF
      INTEGER   NST
      INTEGER   NCH
      INTEGER   NCONST
      INTEGER   ICON(30)
      INTEGER   ISTAT
      LOGICAL   PUSHED
      REAL      ASUMSQ
      REAL*8    SUMSQ
      CHARACTER XTITLE*6

*     Functions

      INTEGER     NTOT
      INTEGER     IGETVM, IFREEVM
      EXTERNAL    GAUSSF

*  Ok, go...

      IFAIL  =  0
      NCONST =  0
      PUSHED = .FALSE.

      XTITLE =  XAXIS_UNITS

      CALL PUSH
      PUSHED = .TRUE.

C   First determine pairs of points between which function is to be fitted

      NDEF = NPR
      NST  = NTOT(NQ-1)+1
      NCH  = NPTS(NQ)
      CALL SETXNEW (XSCALE, IFAIL)
      IF (IFAIL.NE.0) GO TO 999

      CALL GETPTS (IBLP, 1, 8, NDEF, NPR, XSCALE(NST),
     &             DATA(NST), NCH, IFAIL)
      IF (IFAIL.NE.0) GO TO 999

C   Determine no. of gaussians to be fitted
C   and find estimates of parameters ( amp,width,pos'n)
C   Then copy to initial guess array.

      CALL ENTER_GAUSS (IFAIL)
      IF (IFAIL.NE.0) GO TO 999

C   Copy appropriate points to arrays for GAUSSF

      N = 0
      DO I = 1,NPR
        DO J = IBLP(2*I-1),IBLP(2*I)
          IF (DATA(NST+J-1).NE.BADPIX_VAL) THEN
            N      = N + 1
            Y(N)   = DATA(NST+J-1)
            XSC(N) = XSCALE(NST+J-1)
          END IF
        END DO
      END DO

C   Zero baseline of ALL quadrants

      DO J = 1, NTOT(NQ)
        DATA(J) = 0.0
      END DO

C   Set up ICON and other parameters for LMM2

      IERX   = IER+100*NCONST
      ITS    = MAXITS
      NPARAM = 0
      DO I = 1,NG
        DO J = 1,3
          NPARAM        = NPARAM+1
          PARAM(NPARAM) = VAR(I,J)
          ICON(NPARAM)  = 0
        END DO
      END DO

      IF (NPARAM.GT.30)  THEN
        WRITE (ILOUT,1120) NPARAM
        IFAIL = 50
        GO TO 999
      END IF

C   Calculate required workspace and get some virtual memory
C   NOTE: Size of workspace has to allow for final call to GAUSSF to
C         get residuals, so *NCH instead of *N

      NBW = 0
      NBA = 4*NCH*NPARAM
      ISTAT = IGETVM (NBA, .TRUE., 'LINFIT', IAPTR)
      IF (ISTAT .ne. 0) THEN
        PRINT *,'Trouble getting virtual memory for A array'
        IFAIL = 51
        GO TO 999
      END IF

      NBW = 4*NPARAM*(NPARAM+5)
      ISTAT = IGETVM (NBW, .TRUE., 'LINFIT', IWPTR)
      IF (ISTAT .ne. 0) THEN
        PRINT *,'Trouble getting virtual memory for W array'
        IFAIL = 51
        GO TO 999
      END IF

C   Calculate parameters of best-fit baseline

  145 CALL LMM2 (PARAM,RESID,%VAL(CNF_PVAL(IAPTR)),SUMSQ,N,NPARAM,TOL,
     &          1.5,0.5,ITS,IERX,C,G,%VAL(CNF_PVAL(IWPTR)),
     :          ICON,ILOUT,GAUSSF)
      IF(IERX.NE.1)   THEN
        WRITE(ILOUT,1060) IERX
        IFAIL = 9
        NXOLD = NXS
        GO TO 999
      END IF

C   Output parameters of least-squares baseline

      ASUMSQ = SUMSQ
      WRITE (ILOUT,1070) ITS,ASUMSQ
      CALL DISPLAY_GAUSS (IFAIL)

C   Calculate least square baseline

      N = NCH
      DO I = 1, NCH
        XSC(I) = XSCALE(NST+I-1)
        Y(I)   = DATA(NST+I-1)
      END DO
      CALL GAUSSF (PARAM, RESID, %VAL(CNF_PVAL(IAPTR)), SUMSQ, 1)
      DO I = 1, NCH
        DATA(NST+I-1) = DATA(NST+I-1) - RESID(I)
      END DO
      IFAIL = 0
      NXOLD = NXS

*     Standard and error return

  999 CONTINUE

      IF (PUSHED .and. IFAIL.ne.0) CALL POP

      IF (NBA .ne. 0) THEN
        ISTAT = IFREEVM (IAPTR)
        IF (ISTAT .ne. 0) THEN
          PRINT *, 'Trouble freeing workspace array A virtual memory'
          PRINT *, 'NBA, IAPTR = ', NBA, IAPTR
          IFAIL = 51
        END IF
      END IF

      IF (NBW .ne. 0) THEN
        ISTAT =  IFREEVM (IWPTR)
        IF (ISTAT .ne. 0) THEN
          PRINT *, 'Trouble freeing workspace array W virtual memory'
          PRINT *, 'NBW, IWPTR = ', NBW, IWPTR
          IFAIL = 51
        END IF
      END IF

      RETURN

 1060 FORMAT(10X,'*** FAILED TO CONVERGE - IER=',I2,'***'//)
 1070 FORMAT(//10X,'No of Iterations =',I6,10X,'Final SUMSQ =',E11.4)
 1120 FORMAT(' Too many free parameters; NPARAM=',I2,' and limit of 30')

      END


