*  History:
*     10 Dec 1993 (hme):
*        Change terminal output "^z" or "CTRL(Z)" to EOF.
*     20 jul 1995 (rpt)
*        BUG: PROMPT array size needs to be larger than 256. Fixed.
*     20 Sep 2000 (ajc):
*        Don't split strings acroos lines
*        Unused IANS
*        Missing commas in FORMAT
C------------------------------------------------------------------------------

      SUBROUTINE ENTER_GAUSS (IFAIL)

C   This subroutine accepts definitions of an arbitrary number of gaussian
C   line profiles (up to 10) in terms of their amplitude, width and position.

      REAL*4    INPUT(3)
      CHARACTER FORMAT*32, PROMPT*512, XTITLE*6

      INCLUDE 'FLAGCOMM'

      REAL*4    VAR(10,3),PARAM(30)
      COMMON /LINFT/  NXOLD,NGOLD,PARAM,VAR

      INTEGER   GEN_ILEN

* Ok, go...

      IFAIL  = 0
      XTITLE = XAXIS_UNITS

C   Determine no. of gaussians to be fitted
C   and find estimates of parameters ( amp,width,pos'n)

  100 PROMPT = '''"'
      IF (NXS.EQ.NXOLD .AND. NGOLD.NE.0) THEN
        WRITE (PROMPT, 1035, IOSTAT=IERR) (PARAM(J),J=1,3)
      END IF
      PROMPT =
     &'"/'' Estimates of Amp.,Width(FWHM) and Pos''''n for each line''/'
     &//'   '' Line at a time, EOF to finish''//'
     &//'   '' Current units are '//XTITLE//'''//''Line  1: '
     &//PROMPT(1:GEN_ILEN(PROMPT))
      FORMAT = ' '

      DO NG = 1,10

  105   CALL GEN_GETR4A (PROMPT, PARAM(NG*3-2), 3, FORMAT,
     &                   INPUT, JDEF)

        IF (JDEF.EQ.2) THEN
          GO TO 120
        ELSE IF (JDEF.EQ.-1) THEN
          FORMAT = ' '
          WRITE (PROMPT, '(''Line '',I2.1,'': '')') NG
          GO TO 105
        ELSE
          IF (INPUT(1).EQ.0 .OR. INPUT(2).LE.0) THEN
            IFAIL = 71
            RETURN
          END IF
          DO J = 1,3
            VAR(NG,J)         = INPUT(J)
            PARAM(3*(NG-1)+J) = INPUT(J)
          END DO
        END IF

        WRITE (PROMPT, '(''Line '',I2.1,'': '')') NG+1
        IF (NXS.EQ.NXOLD.AND.NG.LT.NGOLD) THEN
          FORMAT = 'F6.1,1X,F4.1,1X,F5.1'
        ELSE
          FORMAT = ' '
        END IF

      END DO

      NG = 11
      WRITE (ILOUT,1040)

  120 NG    = NG-1
      NGOLD = NG

      RETURN

 1035 FORMAT('[',F6.1,1X,F4.1,1X,F7.1,'] ''"')
 1040 FORMAT(' *** Reached maximum of 10 lines ***')

      END

C-----------------------------------------------------------------------
