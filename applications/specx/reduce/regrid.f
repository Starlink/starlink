*  History:
*     22 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     01 Jan 1994 (rp):
*        Replace with more recent VAX version, make same change
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*     15 Jun 1995 (rp)
*        Update to increase number of channels if necessary to cover full
*        width of original spectrum.
*      6 Jun 2000 (ajc):
*        Replace 'type *' with 'PRINT *'
*        Unused IERR, NEWCH
C------------------------------------------------------------------

      SUBROUTINE REGRID (NQ, XSCALE, BUF, IFAIL)

C   Routine to regrid DATA onto another (user specified) grid
C   Updated 19-Aug-91 to allow for non-uniform data with
C   polynomial frequency correction (as for example applies to AOS).

      IMPLICIT   NONE

C     Formal parameters:

      INTEGER   NQ
      REAL*8    XSCALE(*)  ! it's not R*8, but we can pretend it is!
      REAL      BUF(*)
      INTEGER   IFAIL

C     Include files

      INCLUDE   'SPECX_PARS'
      INCLUDE   'STAKPAR'
      INCLUDE   'FLAGCOMM'
      INCLUDE   'STACKCOMM'

C     Local variables

      LOGICAL   FCSAVE
      INTEGER   I
      INTEGER   ISTAT
      INTEGER   JMID
      INTEGER   NBOT
      INTEGER   NDAT
      INTEGER   NOFF
      INTEGER   NL, NH
      INTEGER   NOUT
      INTEGER   NRET
      INTEGER   NST
      INTEGER   NTOP
      REAL      CL, CH
      REAL      CHAN_SHIFT
      REAL*8    GRIDN
      REAL*8    GRIDNN
      REAL*8    XMID
      REAL*8    XMID_NEW
      REAL*8    XFACOLD
      REAL*8    XFAC8(8)

C     Functions:

      INTEGER   NTOT

C  Ok, go...

      IFAIL = 0

C     First call SETX with no polynomial correction in:

      FCSAVE   =  FCORRECT
      FCORRECT = .FALSE.
      CALL SETX8  (XSCALE, XFAC8, NRET, IFAIL)
      IF (IFAIL.NE.0) RETURN

C     Save "un-linearized" value of XFAC(NQ) so that final
C     (and guaranteed linear) scale can be set properly

      XFACOLD = XFAC8(NQ)

C     Establish the nominal X-value at the centre of the input sector
C     (assume that departures from non-linear scale are small
C     enough that linear interpolation is OK)

      NOFF  = NTOT(NQ-1)
      NST   = NOFF + 1

      NDAT  = NPTS(NQ)
      JMID  = NDAT/2
      XMID  = 0.5*(XSCALE(NOFF+JMID)+XSCALE(NOFF+NDAT+1-JMID))

C     Now recalculate X-scale with polynomial corrections in
C     (only do this if necessary)

      IF (FCSAVE) THEN
        FCORRECT = .TRUE.
        CALL SETX8  (XSCALE, XFAC8, NRET, IFAIL)
      END IF
      IF (IFAIL.NE.0) RETURN

C     Ask for the regridding interval and centre

      CALL GEN_GETR8 ('New gridding interval? ('//XAXIS_UNITS//')',
     &                 XFACOLD, 'G10.3', GRIDNN, ISTAT)
      CALL GEN_GETR8 ('Centre of new grid? ('//XAXIS_UNITS//')',
     &                 XMID, 'F19.6', XMID_NEW, ISTAT)

C     "Condition" XSCALE array to improve numerical accuracy by
C     removing the value at the centre of the output spectrum:
C     (Also have to remove it from centre of output spectrum of course,
C      but do this by calling GRID_DATA8 with XMID argument set to zero)

      DO I = 1, NPTS(NQ)
        XSCALE(NST+I-1) = XSCALE(NST+I-1) - XMID_NEW
      END DO

C     Decide on the number of output channels. In principle want to
C     allow at least enough to represent the spectrum over its full
C     width, but perhaps there is some reason to increase or reduce
C     the number? Calculate default (odd or even as original) but ask
C     for any other value...

      CL = (XSCALE(NST)        - 0.5*XFACOLD)/GRIDNN
      CH = (XSCALE(NST+NDAT-1) + 0.5*XFACOLD)/GRIDNN
      IF (MOD(NPTS(NQ),2).EQ.1) THEN
        NL = INT (ABS (CL) - 0.5)
        NH = INT (ABS (CH) - 0.5)
        NOUT = 2*MAX(NL,NH) + 1
      ELSE
        NL = INT (ABS (CL))
        NH = INT (ABS (CH))
        NOUT = 2*MAX(NL,NH)
      END IF

      CALL GEN_GETI4 ('Number of output channels? ',
     &                 NOUT,  'I5',  NOUT,  ISTAT)

CD    PRINT *, ' -- regrid --'
CD    PRINT *, '    First and last XSCALE values: ',
CD   &             XSCALE(1), XSCALE(NPTS(NQ))
CD    PRINT *, '    JFINC, XFACOLD, XFAC8(NQ), GRIDN, GRIDNN'
CD    PRINT *, '    ', JFINC(NQ), XFACOLD, XFAC8(NQ), GRIDN, GRIDNN

      GRIDN = GRIDNN

      CALL GRID_DATA8 (NDAT, XSCALE(NST), DATA(NST), BUF, NOUT,
     &                 GRIDN, 0.D0, NBOT, NTOP, BADPIX_VAL, IFAIL)
      IF (IFAIL.NE.0) GO TO 99

C     Copy data back into the data array
C     (Squeeze up data if we have empty spaces, expand it otherwise)

      CALL SPLICEQ (NQUAD, NQ, NOUT, BUF, LSTK-LHEAD, NPTS, DATA, IFAIL)
      IF (IFAIL.NE.0) GO TO 99

C     Update frequency interval

      JFINC(NQ)   = JFINC(NQ)*(GRIDN/XFACOLD)
CD    PRINT *, '    Final JFINC = ', JFINC(NQ)

C     Since the centre freq etc may have shifted, fix up header parameters
C     Give channel shift in units of new channels.

      CHAN_SHIFT = (XMID_NEW - XMID)/GRIDN

      PRINT *, 'Shift of ', CHAN_SHIFT, ' channels: fixup headers'

      CALL LSRCOR (LSRFLG, VSL,   VES,     VTE,    VLSR,
     &             IDATE,  ITIME, IUTFLG,  RA,     DEC,
     &             JFREST(NQ), JFCEN(NQ), LOFREQ(NQ),  IFFREQ(NQ),
     &             CHAN_SHIFT, JFINC(NQ))

C     Data are now on a linear array so we do *not* need any
C     polynomial frequency correction.

      FCORRECT = .FALSE.

      RETURN

C     -----------

C     Error return

   99 CONTINUE
      PRINT *,'Current X-data may have been corrupted!'
      RETURN

      END

*-------------------------------------------------------------------------
