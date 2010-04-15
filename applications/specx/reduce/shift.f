C-----------------------------------------------------------------------

      SUBROUTINE SHIFT (XSCALE, BUF, IFAIL)

C   Routine to translate spectrum sideways within the quadrant,
C   amending DFCEN appropriately.

C   History:
C      20-SEP-2000 (AJC):
C         Unused UNEQUAL
C-

      IMPLICIT   NONE

C     Formal parameters:

      REAL       XSCALE(*)
      REAL       BUF(*)
      INTEGER    IFAIL

C     Include files:

      INCLUDE   'STACKCOMM'
      INCLUDE   'FLAGCOMM'

C     Functions

      LOGICAL    DOQUAD
      INTEGER    NTOT

C     Local variables

      INTEGER    NQ
      INTEGER    JDEF
      INTEGER    NBOT,  NTOP
      INTEGER    NDAT
      INTEGER    NMID
      INTEGER    NST
      REAL       XMID
      REAL       XPT
      CHARACTER  DEFAULT_FORMAT*8

C     Ok, go...

      IFAIL = 0

C     Set the X-scale

      CALL SETXNEW (XSCALE, IFAIL)

C     Find out how far to shift the spectrum

      DEFAULT_FORMAT = ' '
      IF (NXS.EQ.NXSSS) DEFAULT_FORMAT = 'F7.2'
      CALL GEN_GETR4 ('Shift spectrum? ('//XAXIS_UNITS//')',
     &                ACHAN, DEFAULT_FORMAT, ACHAN, JDEF)

C   Shift the spectrum, amend the observed freqs if LSRFLG=0

      DO NQ = 1, NQUAD
        IF (DOQUAD(NQ)) THEN

          NDAT = NPTS(NQ)
          NST  = NTOT(NQ-1)+1

          XPT  = ACHAN/XFAC(NQ)
          NMID = (NDAT+1)/2
          XMID = 0.5 * (XSCALE(NMID)+XSCALE(NDAT+1-NMID)) - ACHAN

          CALL GRID_DATA (NDAT, XSCALE(NST), DATA(NST), BUF, NDAT,
     &                    XFAC(NQ), XMID, NBOT, NTOP, BADPIX_VAL,
     &                    IFAIL)

          CALL LSRCOR    (LSRFLG, VSL, VES, VTE, VLSR,
     &                    IDATE, ITIME, IUTFLG, RA, DEC,
     &                    JFREST(NQ), JFCEN(NQ), LOFREQ(NQ),
     &                    IFFREQ(NQ), -XPT,      JFINC(NQ))
        END IF
      END DO

C   Save X-axis units so we know if ACHAN can be used as default next time

      NXSSS = NXS

      RETURN
      END

C-----------------------------------------------------------------------
