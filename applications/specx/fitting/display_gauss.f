*  History:
*     31 Jan 1994 (hme):
*        Disuse <> in formats.
*     20 Sep 2000 (ajc):
*        Missing comma in FORMAT
C-----------------------------------------------------------------------

      SUBROUTINE DISPLAY_GAUSS (IFAIL)

      IMPLICIT  NONE

C     Formal parameter

      INTEGER IFAIL

C     Local variables:

      INTEGER   I, J
      INTEGER   IERR
      INTEGER   IXU
      CHARACTER XTITLE*6

C     Common blocks:

      INCLUDE 'FLAGCOMM'

      INTEGER  NXOLD
      INTEGER  NGOLD
      REAL     PARAM
      COMMON /LINFT/  NXOLD, NGOLD, PARAM(30)

C     Functions

      INTEGER  GEN_ILEN

*  Ok, go...

      IFAIL  = 0
      XTITLE = XAXIS_UNITS
      IXU    = GEN_ILEN (XAXIS_UNITS)

      WRITE (ILOUT,1080)
      WRITE (ILOUT,1090) XTITLE, XTITLE

      IF (NXS.EQ.2 .and. ABS_FREQ) THEN
        DO I = 1, NGOLD
          WRITE (ILOUT,1110, IOSTAT=IERR) I,(PARAM(J),J=3*I-2,3*I)
        END DO
      ELSE
        DO I = 1,NGOLD
          WRITE (ILOUT,1100, IOSTAT=IERR) I,(PARAM(J),J=3*I-2,3*I)
        END DO
      END IF

 1080 FORMAT(/1X,T20,'Parameters of current gaussian model'/)
 1090 FORMAT(18X,'N       Amp.        Width (',A,
     &           ')    Pos''n (',A,')')
 1100 FORMAT(17X,I2,4X,F7.3,9X,F6.2,8X,F9.2)
 1110 FORMAT(17X,I2,4X,F7.3,7X,F9.5,6X,F12.5)

      RETURN
      END

