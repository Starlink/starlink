*  History:
*     22 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*      6 Jun 2000 (ajc):
*        Replace 'Type *' with 'PRINT *'
*        Don't split string constant across lines
*        Unused VTOT
*        Initialise ISB so Linux behaves the same as other platforms
*-----------------------------------------------------------------------

      SUBROUTINE CHANGE_SIDEBAND (XSCALE, BUF, IFAIL)

*  Routine to let you alter the frequencies in the map header to those
*  corresponding to observations in the other sideband. This to allow
*  you to identify confusing lines more easily etc.

      IMPLICIT  NONE

*  Formal parameters:

      REAL      XSCALE(*)
      REAL      BUF(*)
      INTEGER   IFAIL

*  Common blocks and include files

      INCLUDE  'NEWXY'
      INCLUDE  'FLAGCOMM'
      INCLUDE  'STACKCOMM'

*  Local variables

      LOGICAL   USE_LO
      INTEGER   IERR
      INTEGER   ISB
      INTEGER   NQ
      INTEGER   ISTAT
      INTEGER   JFCEN_TEMP(8)
*      REAL      DF
      CHARACTER VFRAME*4
      CHARACTER VDEF*3

      DOUBLE PRECISION   DF

      DOUBLE PRECISION   FROBS
      DOUBLE PRECISION   FRDIS
      DOUBLE PRECISION   FCEN_T

*  Ok, go...

      ISB = 0

*     If the Lo frequency is not set in the scan header, read IF and
*     sideband from the terminal.

      IF (LOFREQ(1).eq.0.D0 .and. IFFREQ(1).eq.0.D0) THEN

        WRITE (6,*) 'LO/IF frequencies not defined...'
        CALL GEN_GETSTR ('Current sideband? (U/L)', SIDEBAND, 'A1',
     &                    SIDEBAND, IERR)

        CALL UUCASE (SIDEBAND)
        IF (SIDEBAND.EQ.'U') THEN
          ISB = +1
        ELSE IF (SIDEBAND.EQ.'L') THEN
          ISB = -1
        ELSE
          IFAIL = 21
          RETURN
        END IF

        CALL GEN_GETR8  ('First i.f.? (GHz)', FIRST_IF, 'F9.6',
     &                    FIRST_IF, IERR)
        USE_LO = .FALSE.
      ELSE
        USE_LO = .TRUE.
      END IF

*     Work out TOTAL radial velocity used when the data were taken.
*     Assume that as with JCMT the LSR correction is applied to bring
*     the spectrum centre to the chosen frequency in the source frame.

      CALL SETXASTRO   (ITIME, IDATE, ALAT,  ALONG,  TIMCOR, IUTFLG,
     &                  DEC,   RA,   VSL,   VES,    VTE,    ISTAT)

*     Find velocity frame and velocity law used during observation

      CALL VELDECODE (LSRFLG, VFRAME, VDEF)

CD    WRITE (ILOUT,*) 'Data obs''d in ',VFRAME,' frame; ',
CD   &       VDEF, ' velocity law; Velocity = ', VLSR, ' km/s'

*     Save values of JFCEN in temporary storage

      CALL XCOPY (32, JFCEN, JFCEN_TEMP)

*     For each quadrant...

      DO NQ = 1, NQUAD

*       Choose a value of rest frequency applicable to the *observation*.
*       (value for display comes for free, but is not used...)

        CALL SETXFREST (IQCEN, NQ, JFCEN, JFREST, FCEN, FROBS, FRDIS)
CD      PRINT *, 'Rest freq used for observation, = ', FROBS

*       Convert header centre frequency JFCEN to telluric value,
*       returned in FCEN_T (GHz).

        CALL SETXFTCEN (JFCEN(NQ), VFRAME, VDEF,
     &                  VTE, VES, VSL, VLSR, FCEN_T)
CD      PRINT *, 'Telluric centre frequency = ', FCEN_T, ' GHz'

*       Correct the telluric frequency to other sideband

        IF (USE_LO) THEN
CD        PRINT *, 'Local oscillator frequency = ', LOFREQ(NQ), ' GHz'
          DF = + 2.D3 * (LOFREQ(NQ)-FCEN_T)
          WRITE (6, '('' Sector '',I2,'': First I.F. = '','//
     &                   'F9.6, '' GHz'')') NQ, DF/2.D3
        ELSE
          DF = - 2.D3 * DFLOAT(ISB) * FIRST_IF
        END IF

*       Correct back to offset frequency in *source* frame using appropriate
*       velocity scaling law (RADIO, OPTICAL or RELATIVISTIC).

        CALL SETXDOPP   (DF, 1, VFRAME, VDEF,
     &                   VTE, VES, VSL, VLSR, FCEN_T, FROBS)

*       Final step is to calculate total frequency and amend JFCEN accordingly.
*       (cf routine SETXFREQ, with ABS_FREQ = .T. and REST_REL = .T.)

        JFCEN_TEMP(NQ) = 1.D6 * (FROBS + DF/1000.D0)
CD      PRINT *, 'Observation frame centre freq = ',
CD   &           JFCEN(NQ)/1.D6, ' GHz'

*       Correct the frequency increment (*-1)

        JFINC(NQ)  = - JFINC(NQ)

        IF (USE_LO) THEN
          IFFREQ(NQ) = -IFFREQ(NQ)
        END IF

      END DO

      CALL XCOPY (32, JFCEN_TEMP, JFCEN)
      ISB = -ISB

*     Warning message

      PRINT *
      PRINT *, ' --- Header entries changed to other sideband ---'
      PRINT *, '     Note that f_rest still refers to frequency  '
      PRINT *, '     used as reference in velocity transformation:'
      PRINT *, '     You should not normally need to change this.'
      PRINT *

      FREEX         = .TRUE.
      ISETSC        = .TRUE.
      CHANGE_SCALES = .FALSE.

      RETURN
      END

*-----------------------------------------------------------------------
