*  History:
*     09 Feb 1993
*        Add a doulbe alias for VLSR_0 to fix call to SETXDOPP and
*        SETXVEL. Also declare DV and DF double for similar reasons.
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused ISB, VTOT
*-----------------------------------------------------------------------

      SUBROUTINE CALC_IMAGE (NQ, LOFREQ, IFFREQ, JFCEN, JFREST, JFINC,
     &                       IQCEN, VLSR, VTE, VES, VSL, LSRFLG,
     &                       RA, DEC, ITIME, IDATE, IUTFLG,
     &                       IMAGE_FREQ, VLSR_0, IFAIL)

*  Routine to let you alter the frequencies in the map header to those
*  corresponding to observations in the other sideband. This to allow
*  you to identify confusing lines more easily etc.

*  Routine applies only to nominated sub-band of spectrum

      IMPLICIT  NONE

*  Formal parameters:

      INTEGER          NQ
      DOUBLE PRECISION LOFREQ(*), IFFREQ(*)
      INTEGER          JFCEN(*),  JFREST(*),  JFINC(*)
      INTEGER          IQCEN
      REAL             VLSR, VTE, VES, VSL
      INTEGER          LSRFLG
      DOUBLE PRECISION RA,   DEC
      CHARACTER        ITIME*8,   IDATE*9
      BYTE             IUTFLG
      DOUBLE PRECISION IMAGE_FREQ    ! Hz
      REAL             VLSR_0
      INTEGER          IFAIL

*  Common blocks and include files

      INCLUDE  'FLAGCOMM'

*  Local variables

      LOGICAL   USE_LOIF
      LOGICAL   USE_VELS

      INTEGER   IERR
      INTEGER   ISTAT

      CHARACTER VFRAME*4
      CHARACTER VDEF*3

      DOUBLE PRECISION   DF
      DOUBLE PRECISION   DV
      DOUBLE PRECISION   VLSR_0D
      DOUBLE PRECISION   FROBS
      DOUBLE PRECISION   FRDIS
      DOUBLE PRECISION   FCEN_T

*  Ok, go...

*     Work out TOTAL radial velocity used when the data were taken.
*     Assume that as with JCMT the LSR correction is applied to bring
*     the spectrum centre to the chosen frequency in the source frame.

      USE_VELS = (VSL.ne.0.D0 .or. VES.ne.0.D0 .or. VTE.ne.0.D0)

      IF (.NOT.USE_VELS) THEN
        CALL SETXASTRO   (ITIME, IDATE, ALAT,  ALONG,  TIMCOR, IUTFLG,
     &                    DEC,   RA,    VSL,   VES,    VTE,    ISTAT)
      END IF

*     Find velocity frame and velocity law used during observation

      CALL VELDECODE (LSRFLG, VFRAME, VDEF)

*     Choose a value of rest frequency applicable to the *observation*.
*     (value for display comes for free, but is not used...)

CD    PRINT *, '-- calc_image --'
CD    PRINT *, '   input value of IQCEN  = ', IQCEN
CD    PRINT *, '   input value of FCEN   = ', FCEN
CD    PRINT *, '   input value of JFCEN  = ', JFCEN
CD    PRINT *, '   input value of JFREST = ', JFREST
CD    PRINT *, '   header value of LOFREQ(1) = ', LOFREQ(NQ)
CD    PRINT *, '   header value of IFFREQ(1) = ', IFFREQ(NQ)

      CALL SETXFREST (IQCEN, NQ, JFCEN, JFREST, FCEN, FROBS, FRDIS)

CD    PRINT *, '   rest freq used for observation, = ', FROBS

*     Convert header centre frequency JFCEN to telluric value,
*     returned in FCEN_T (GHz).

      USE_LOIF = (LOFREQ(NQ) .NE. 0.D0)
CD    PRINT *, '   use_loif = ', use_loif

      IF (USE_LOIF) THEN
        FCEN_T = LOFREQ(NQ) + IFFREQ(NQ)
CD      PRINT *, '   telluric centre freq from LO and IF = ', FCEN_T
      ELSE
        CALL SETXFTCEN (JFCEN(NQ), VFRAME, VDEF,
     &                  VTE, VES, VSL, VLSR, FCEN_T)
CD    PRINT *, '   telluric centre freq from vels = ', FCEN_T
      END IF

*     Calculate LSR velocity (radio def'n) of centre channel while
*     we are here...

      VLSR_0 = 0.0
      VLSR_0D = VLSR_0
      CALL SETXDOPP   (VLSR_0D, 1, 'LSR ', 'RAD',
     &                 VTE, VES, VSL, 0.0, FCEN_T, FROBS)
      CALL SETXVEL    (VLSR_0D, 1, 'RAD', FROBS, DV , ISTAT)
      VLSR_0 = VLSR_0D
      WRITE (6,
     &     '('' Centre channel VLSR (Rad def''''n)'',G13.6)',
     &       IOSTAT=IERR) VLSR_0

*     Correct the telluric frequency to other sideband

      DF = + 2.D3 * (LOFREQ(NQ)-FCEN_T)
      WRITE (6,
     &     '('' Sector '',I2.1,'' : New I.F. = '', F11.6, '' GHz'')',
     &       IOSTAT=IERR) NQ, DF/2.D3

*     Correct back to offset frequency in *source* frame using appropriate
*     velocity scaling law (RADIO, OPTICAL or RELATIVISTIC).

      CALL SETXDOPP   (DF, 1, VFRAME, VDEF,
     &                 VTE, VES, VSL, VLSR, FCEN_T, FROBS)
CD    PRINT *, '   corrected frequency offset (MHz) (~2*i.f.) = ', DF

*     Now calculate image_frequency in *source* frame
*     (cf routine SETXFREQ, with ABS_FREQ = .T. and REST_REL = .T.)

      IMAGE_FREQ = 1.D9 * (FROBS + DF/1000.)
CD    PRINT *, '   final image frequency (Hz) = ', image_freq

      RETURN
      END

*-----------------------------------------------------------------------
