*    History:
*     01-Aug-1995 (rpt):
*        ICOLOR support added.
*     20-Sep-2000 (ajc):
*        Unused GEN_ILEN
*-----------------------------------------------------------------------

      SUBROUTINE PLTDAT (NPTS, NQUAD, ITIP, IWEIGHT, LCOLOR,
     &                   JPLOT, TOPSCAL, XSCALE, DATA)

C  Routine to write out current x-y pairs to data file for plotting

      IMPLICIT  NONE

C     Include files (I)

      INCLUDE  'SPECX_PARS'

C     Formal parameters:

      INTEGER   NPTS(NQMAX)
      INTEGER   NQUAD
      INTEGER   ITIP
      INTEGER   IWEIGHT
      INTEGER   LCOLOR
      INTEGER   JPLOT
      LOGICAL   TOPSCAL
      REAL      XSCALE(*)
      REAL      DATA(*)

C     Include files (II)

      INCLUDE  'DOPPLER'
      INCLUDE  'FLAGCOMM'

      INTEGER   PLOT_UNIT
      COMMON /PLTDEV/ PLOT_UNIT

      INTEGER   NXOLD
      INTEGER   NGOLD
      REAL      PARAM
      COMMON /LINFT/  NXOLD, NGOLD, PARAM(30)

C     Local variables

      INTEGER   J
      LOGICAL   ICONT

C     Functions

      INTEGER   NTOT


C  Ok, go...

*     Overlay next plot

      BACKSPACE (PLOT_UNIT)
      ICONT = .TRUE.
      WRITE (PLOT_UNIT) ICONT

      WRITE (PLOT_UNIT) NPTS, NQUAD, ICHAR, ITIP, IPEN,
     &                  MASK, NMASK, XFAC, IWEIGHT, LCOLOR,
     &                  BADPIX_VAL, TOPSCAL, OSCFREQ, DOPPFAC
      WRITE (PLOT_UNIT) (XSCALE(J), J=1, NTOT(NQUAD))
      WRITE (PLOT_UNIT) (DATA(J),   J=1, NTOT(NQUAD))

      JPLOT = JPLOT+1

      ICONT = .FALSE.
      WRITE (PLOT_UNIT) ICONT

      RETURN
      END
