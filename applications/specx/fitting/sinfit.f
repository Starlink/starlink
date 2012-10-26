*  History:
*     19 Nov 1993 (hme):
*        Remove INCLUDE '($SSDEF)'.
*        TABs removed.
*     10 Dec 1993 (hme):
*        Change terminal output "^z" or "CTRL(Z)" to EOF.
*     09 Jan 1994 (rp):
*        Change SETX to SETXNEW and install single error return
*     15 Jan 1994 (rp):
*        Make array sizes depend of SPECX_PARS where sensible
*     20 jul 1995 (rpt)
*        BUG: PROMPT array size needs to be larger than 256. Fixed.
*     20 July 2000 (ajc):
*        Insert miising commas in formats
*        Change TYPE * to PRINT *
*        Don't split string constant across lines
*        Unused IFORM, K, IANS
C-----------------------------------------------------------------------------

      SUBROUTINE SINFIT (NQ, XSCALE, BUF, IFAIL)

C   This subroutine performs a least squares fit of polynomial+sinusoids
C   to the data in the x-register of 'stack'
C
C     FORMAL PARAMETERS:
C        XSCALE  - values of independent co-ordinate at each data pt.
C        BUF     - workspace array
C        IFAIL   - set to 0 on output if b-l removal successful

      IMPLICIT  NONE

C     Formal parameters:

      INTEGER   NQ
      REAL      XSCALE(1)
      REAL      BUF(1)
      INTEGER   IFAIL

C     Global variables:

      INCLUDE 'FLAGCOMM'
      INCLUDE 'SPECX_PARS'
      INCLUDE 'STAKPAR'
      INCLUDE 'STACKCOMM'
      INCLUDE 'CNF_PAR'

C     Local variables:

      INTEGER   ICON(30)
      INTEGER   IERX
      INTEGER   ITS
      INTEGER   NCONST
      REAL      PARAM(30)
      REAL*8    SUMSQ


      INTEGER   I, J
      INTEGER   IAPTR
      INTEGER   IWPTR
      INTEGER   IERR
      INTEGER   ISTAT
      INTEGER   JDEF
      INTEGER   NBA
      INTEGER   NBW
      INTEGER   NCH
      INTEGER   NDEF
      INTEGER   NF
      INTEGER   NLIM
      INTEGER   NPOLD
      INTEGER   NSOLD
      INTEGER   NST

      REAL      AMP
      REAL      ANG
      REAL      ASUMSQ
      REAL      PHASE
      REAL      PI
      REAL      SIGN

      LOGICAL   PUSHED

      CHARACTER FORMAT*32
      CHARACTER PROMPT*512
      CHARACTER XTITLE*6

C     Communication with fitting subroutines

      REAL      XSC(LSPMAX)
      REAL      Y(LSPMAX)
      INTEGER   N
      INTEGER   NPARAM
      COMMON /GFUNC/  XSC, Y, N, NPARAM

      INTEGER    NP
      INTEGER    NS
      INTEGER    NXOLD
      REAL       PPAR(30)
      INTEGER    FPAR
      INTEGER    SPAR(28)
      COMMON /SINFT/  NP, NS, NXOLD, PPAR, FPAR, SPAR

      REAL      RESID(LSPMAX)
      REAL      C(30)
      REAL      G(LSPMAX)
      COMMON /WORK/   RESID,C,G

C     Functions

      INTEGER   IGETVM
      INTEGER   IFREEVM
      INTEGER   GEN_ILEN
      INTEGER   NTOT

      EXTERNAL  FUNVAL

C  Ok, go...

      PI     = 3.141593

      IFAIL  = 0
      PUSHED = .FALSE.

      NPOLD  = NP
      NSOLD  = NS
      NXOLD  = NXS
      IFAIL  = 0
      NCONST = 0
      XTITLE = XAXIS_UNITS

      CALL PUSH
      PUSHED = .TRUE.

C   First determine pairs of points between which function is to be fitted

      CALL SETXNEW (XSCALE, IFAIL)
      IF (IFAIL.NE.0) GO TO 999

      NDEF = NPR
      NST  = NTOT(NQ-1)+1
      NCH  = NPTS(NQ)
      CALL GETPTS (IBLP,1,8,NDEF,NPR,XSCALE(NST),DATA(NST),NCH,IFAIL)
      IF (IFAIL.NE.0) GO TO 999

C   Determine no. of polynomial terms to be fitted
C   Also find estimates of parameters ( coefficients of x**n )

  100 PROMPT      = ' '
      PROMPT(1:2) = '''"'
      NP          = 0
      IF (NXS.EQ.NXOLD .AND. NPOLD.NE.0) THEN
        WRITE (PROMPT, 1035, IOSTAT=IERR) PPAR(1)
 1035   FORMAT ('[',E9.2,'] ','"')
      END IF
      PROMPT=
     &'"/'' Estimate of polynomial coefficients for x**n''/'//
     &'  '' One at a time, EOF to finish''//'//
     &'  '' Current units are '//XTITLE//'''//''# (x** 0) '//
     &   PROMPT(1:GEN_ILEN(PROMPT))
      FORMAT = ' '
      DO 110 NP = 1, 30
  105   CALL GEN_GETR4 (PROMPT, PPAR(NP), FORMAT, PPAR(NP), JDEF)
        IF (JDEF.EQ.2) THEN
          GO TO 120
        ELSE IF(JDEF.EQ.-1) THEN
          FORMAT = ' '
          WRITE (PROMPT, 106) NP-1
  106     FORMAT ('# (x**',I2.1,') ')
          GO TO 105
        END IF
        WRITE (PROMPT,106) NP
        FORMAT = ' '
        IF (NXS.EQ.NXOLD .AND. NP.LT.NPOLD) FORMAT = 'E9.2'
  110 CONTINUE

      NP = 31
      WRITE (ILOUT, 1040)

  120 NP = NP - 1

C   Determine fundamental frequency of harmonic terms

      NS=0
      IF(NP.GT.27)   GO TO 127
      FORMAT=' '
      IF(NXS.EQ.NXOLD) FORMAT='F8.4'
      CALL GEN_GETR4('Estimate of fundamental period,-ve if const;'//
     &               'EOF=none', FPAR, FORMAT, FPAR, JDEF)
      IF(JDEF.EQ.2)   GO TO 127

C   Determine number of harmonic terms
C   and get initial estimates of amplitude and phase

  121 NLIM        = (29-NP)/2
      PROMPT      = ' '
      PROMPT(1:2) = '''"'
      IF (NXS.EQ.NXOLD .AND. NSOLD.NE.0) THEN
        WRITE (PROMPT, 1036) (SPAR(J),J=1,2)
 1036   FORMAT('[',F6.1,1X,F5.1,'] ','"')
      END IF
      PROMPT =
     &'"/'' Estimates of Amplitude and phase for each harmonic''/'//
     &'  '' Term at a time, EOF to finish''//'//
     &'  '' Current units are '//XTITLE//'''//''# '
     &   //PROMPT(1:GEN_ILEN(PROMPT))
      FORMAT = ' '
      DO 125 NS = 1, NLIM
  115   CALL GEN_GETR4A (PROMPT, SPAR(NS*2-1), 2,
     &                   FORMAT, SPAR(NS*2-1), JDEF)
        IF(JDEF.EQ.2) THEN
          GO TO 126
        ELSE IF (JDEF.EQ.-1) THEN
          FORMAT = ' '
          PROMPT = '#'
          GO TO 115
        END IF
        PROMPT = '#'
        FORMAT = ' '
        IF (NXS.EQ.NXOLD .AND. NS.LT.MIN (NLIM, NSOLD))
     &    FORMAT='F6.1,1X,F5.1'
  125 CONTINUE
      NS = NLIM + 1
      WRITE (ILOUT,1040)
  126 NS = NS - 1

C   Copy all parameters to PARAM

  127 NPARAM = 0
      DO I = 1, NP
        PARAM(I) = PPAR(I)
        NPARAM   = NPARAM+1
        ICON(I)  = 0
      END DO

      IF (NS.EQ.0)   GO TO 135
      NPARAM       = NPARAM+1
      ICON(NPARAM) = 0
      IF(FPAR.LT.0.0)   THEN
        ICON(NPARAM) = 1
        NCONST       = NCONST + 1
      END IF
      PARAM(NPARAM) = ABS(FPAR)

      DO I = 1, NS
        NPARAM        = NPARAM + 1
        ICON(NPARAM)  = 0
        PARAM(NPARAM) = SPAR(2*I-1)
        NPARAM        = NPARAM + 1
        ICON(NPARAM)  = 0
        PHASE         = SPAR(2*I)
        NF            = PHASE/ABS(FPAR/I)
        PHASE         = PHASE-NF*ABS(FPAR/I)
        PHASE         = 2.*PI*PHASE/ABS(FPAR/I)
        PARAM(NPARAM) = PHASE
      END DO

C   Copy appropriate points to XSC

  135 N = 0
      DO I = 1,NPR
        DO J = IBLP(2*I-1),IBLP(2*I)
          IF (DATA(NST+J-1).NE.BADPIX_VAL) THEN
            N      = N+1
            Y(N)   = DATA(NST+J-1)
            XSC(N) = XSCALE(NST+J-1)
          END IF
        END DO
      END DO

      IF (N.EQ.0) THEN
        PRINT *, 'No data points selected!'
        CALL POP
        IFAIL = 5
        GO TO 999
      END IF

C   Zero baselines of ALL quadrants

      DO I = 1, NTOT(NQ)
        DATA(I) = 0.0
      END DO

C   Set up other parameters for LMM2

      IERX = IER + 100*NCONST
      ITS  = MAXITS

*     PRINT *, ' -- sinfit --'
*     PRINT *, '    NCONST = ', NCONST
*     PRINT *, '    ITS    = ', ITS
*     PRINT *, '    NPARAM = ', NPARAM
*     PRINT *, '    ILOUT  = ', ILOUT
*     PRINT *, '    TOL    = ', TOL

C   Calculate required workspace and get some virtual memory

      NBW = 0
      NBA = 4*NCH*NPARAM
      ISTAT = IGETVM (NBA, .TRUE., 'SINFIT', IAPTR)
      IF (ISTAT .ne. 0) THEN
        PRINT *,'Trouble getting virtual memory for A array'
        IFAIL = 51
        GO TO 999
      END IF

      NBW= 4 * (NPARAM*(NPARAM+5) + NCONST)
      ISTAT = IGETVM (NBW, .TRUE., 'SINFIT', IWPTR)
      IF (ISTAT .ne. 0) THEN
        PRINT *,'Trouble getting virtual memory for W array'
        IFAIL=51
        GO TO 999
      END IF

C   Calculate parameters of best-fit baseline

  141 CALL LMM2 (PARAM, RESID, %VAL(CNF_PVAL(IAPTR)), 
     :           SUMSQ, N, NPARAM, TOL,
     &           1.5, 0.5, ITS, IERX, C, G, %VAL(CNF_PVAL(IWPTR)), ICON,
     &           ILOUT, FUNVAL)
      IF(IERX.NE.1)   THEN
        WRITE (ILOUT,1060) IERX
        IFAIL = 9
        NPOLD = NP
        NSOLD = NS
        GO TO 999
      END IF

C   Copy best-fit parameters to arrays

      DO I = 1, NP
        PPAR(I) = PARAM(I)
      END DO

      IF (NS.EQ.0)   GO TO 150
      SIGN = FPAR/ABS(FPAR)
      FPAR = SIGN*PARAM(NP+1)
      DO I = 1, NS
        ANG   = 2.*PI*I/ABS(FPAR)
        AMP   = PARAM(NP+2*I)
        PHASE = PARAM(NP+2*I+1)
        IF(AMP.LT.0.0)   THEN
          PHASE =  PHASE + PI
          AMP   = -AMP
        END IF
        NF          = PHASE/(2.*PI)
        PHASE       = PHASE - NF*2.*PI
        PHASE       = PHASE/ANG
        SPAR(2*I-1) = AMP
        SPAR(2*I)   = PHASE
      END DO

C   Output parameters of least-squares baseline

  150 ASUMSQ = SUMSQ
      WRITE (ILOUT,1070) ITS, ASUMSQ
      WRITE (ILOUT,1080)
      WRITE (ILOUT,1090)
      DO 160 I = 1, NP
  160 WRITE (ILOUT,1100) I-1, PPAR(I)
      IF (NS.EQ.0)   GO TO 165
      WRITE (ILOUT,1110) ABS(FPAR), XTITLE
      WRITE (ILOUT,1120) XTITLE

      DO 162 I = 1, NS
  162 WRITE (ILOUT,1130) I, (SPAR(J),J=2*I-1,2*I)

C   Calculate least square baseline

  165 N = NCH
      DO I = 1, NCH
        XSC(I) = XSCALE(NST+I-1)
        Y(I)   = DATA(NST+I-1)
      END DO
      CALL FUNVAL (PARAM, RESID, %VAL(CNF_PVAL(IAPTR)), SUMSQ, 1)
      DO I = 1, NCH
        DATA(NST+I-1) = DATA(NST+I-1) - RESID(I)
      END DO
      IFAIL = 0
      NPOLD = NP
      NSOLD = NS

C     Standard and error return

  999 CONTINUE

      IF (PUSHED .and. IFAIL.NE.0) CALL POP

      IF (NBA .ne. 0) THEN
        ISTAT = IFREEVM (IAPTR)
        IF (ISTAT .ne. 0) THEN
          PRINT *,'Trouble freeing workspace array A virtual memory'
          PRINT *,'NBA,IAPTR',NBA,IAPTR
          IFAIL = 51
        END IF
      END IF

      IF (NBW .ne. 0)THEN
        ISTAT = IFREEVM (IWPTR)
        IF (ISTAT .ne. 0) THEN
          PRINT *,'Trouble freeing workspace array W virtual memory'
          PRINT *,'NBW,IWPTR',NBW,IWPTR
          IFAIL = 51
        END IF
      END IF

      RETURN

 1040 FORMAT(' *** No more terms ***')
 1060 FORMAT(10X,'*** FAILED TO CONVERGE - IER=',I2,'***'//)
 1070 FORMAT(//10X,'No of Iterations =',I6,10X,'Final SUMSQ =',E11.4)
 1080 FORMAT(/1X,T20,'Parameters of Least-Square Baseline')
 1090 FORMAT(1X,T18,'Polynomial terms - Coefficients of X**n')
 1100 FORMAT(27X,I2,4X,F6.2)
 1110 FORMAT('0',T20,'Fundamental period is ',G11.4,' ',A6)
 1120 FORMAT('0',T20,'Harmonic  Amplitude    Phase(',A6,')')
 1130 FORMAT(23X,I2,5X,F6.2,8X,F6.2)
 1150 FORMAT(' Too many free parameters; limit of 30')

      END


