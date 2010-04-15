*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*     18 Sep 2000 (ajc)
*        Change IUTFLG to BYTE
C-----------------------------------------------------------------------

      SUBROUTINE LSRCOR (LSRFLG, VSL,   VES,     VTE,    VLSR,
     &                   DATE,   TIME,  IUTFLG,  RA,     DEC,
     &                   JFREST, JFCEN, LOFREQ,  IFFREQ, CHAN_SHIFT,
     &                   JFINC)

C  Routine to amend velocity/frequency information in header to reflect
C  shift in data. Exact changes depend on whether or not frequency is
C  in frame of rest or in lsr frame
C
C  Sense is such that a shift of CHAN_SHIFT channels refers to the
C  viewing window, not the DATA, so that for a shift of the data to
C  the right call is with -ve CHAN_SHIFT.
C
C  With V2 headers that *do* have LO and IF information, then need to
C  update the IF rather than the centre frequency (or better, as well as).

      IMPLICIT  NONE

*     Formal parameters

      LOGICAL   LSRFLG
      REAL      VTE, VES, VSL
      REAL      VLSR
      CHARACTER DATE*9
      CHARACTER TIME*8
      BYTE      IUTFLG
      REAL*8    RA
      REAL*8    DEC
      INTEGER   JFREST
      INTEGER   JFCEN
      REAL*8    LOFREQ
      REAL*8    IFFREQ
      REAL      CHAN_SHIFT
      INTEGER   JFINC

*     Include files

      INCLUDE   'FLAGCOMM'

*     Local variables:

      REAL*8     DF
      REAL*8     DFTELL
      INTEGER    ISTAT
      CHARACTER  VFRAME*4
      CHARACTER  VDEF*3

      DOUBLE PRECISION FCEN_T
      DOUBLE PRECISION FROBS
      DOUBLE PRECISION FRDIS

*  Ok, go..

*     First do all necessary astronomy to find all interframe velocities
*     projected onto the source direction.

      IF (VSL.eq.0.0 .and. VES.eq.0.0 .and. VTE.eq.0.0) THEN
        CALL SETXASTRO   (TIME, DATE, ALAT,  ALONG,  TIMCOR, IUTFLG,
     &                    DEC,  RA,   VSL,   VES,    VTE,    ISTAT)
      END IF

*     Find velocity frame and velocity law used during observation

      CALL VELDECODE (LSRFLG, VFRAME, VDEF)

      WRITE (ILOUT,*) 'Data obs''d in ',VFRAME,' frame; ',
     &       VDEF, ' velocity law; Velocity = ', VLSR, ' km/s'

*     Choose a value of rest frequency applicable to the *observation*.
*     (value for display comes for free, but is not used...)

      CALL SETXFREST (1, 1, JFCEN, JFREST, FCEN, FROBS, FRDIS)
CD    PRINT *, ' -- lsrcor --'
CD    PRINT *, '    Rest freq, observed, = ', FROBS

*     Convert header centre frequency JFCEN to telluric value,
*     returned in FCEN_T (GHz).

      CALL SETXFTCEN (JFCEN, VFRAME, VDEF,
     &                VTE, VES, VSL, VLSR, FCEN_T)
CD    PRINT *, '    Telluric centre frequency = ', FCEN_T, ' GHz'

*     Calculate desired offset in telluric frequency; just multiply number
*     of channels by (telluric) channel width. Result in MHz.

      DFTELL = 1.0D-6 * CHAN_SHIFT * DFLOAT(JFINC)
      DF     = DFTELL
CD    PRINT *, '    Telluric frequency shift of scan centre = ',
CD   &             DF, ' MHz'

*     Correct to offset frequency in *source* frame using appropriate
*     velocity scaling law (RADIO, OPTICAL or RELATIVISTIC).

      CALL SETXDOPP   (DF, 1, VFRAME, VDEF,
     &                 VTE, VES, VSL, VLSR, FCEN_T, FROBS)
CD    PRINT *, '    Telluric centre freq = ', FCEN_T, ' GHz'

*     Final step is to calculate total frequency and amend JFCEN accordingly.
*     (cf routine SETXFREQ, with ABS_FREQ = .T. and REST_REL = .T.)

      JFCEN = 1.D6 * (FROBS + DF/1000.)
CD    Print *, '    Doppler corrected centre frequency = ', JFCEN, ' kHz'

*     If LO and Intermediate frequencies are set, then amend IF also
*     to reflect shift in channels. Required shift was saved in DFTELL

      IF (LOFREQ.ne.0.D0 .and. IFFREQ.ne.0.D0) THEN
        IFFREQ = IFFREQ + 1.D-3*DFTELL
      END IF

      RETURN
      END
