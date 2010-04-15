
*  History:
*     19 Nov 1993 (hme):
*        Disuse SPECX_CTRLC error handler.
*        Replace STR$UPCASE with CHR_UCASE.
*     31 Dec 1993 (rp):
*        Replace with V6.3 module
*        Apply same edit as 19-Nov
*     09 Dec 1993 (rp):
*        Remove SETX routine (has been replaced by SETXNEW everywhere)
*     15 Jan 1994 (rp):
*        Change CHR_UCASE to UUCASE
*        Declare array sizes using parameters from SPECX_PARS
*     05 Jun 1996 (timj)
*        Tidy up DOUBLE PREC and REALS - segments with Sun compilers
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Remove escapes from WRITE statement
*     16 May 2003 (timj):
*        Prevent warnings from valgrind. Only need to copy the actual
*        number of quadrants to XFAC. Note the maximum number (since
*        we do not initialize all 8).
C-----------------------------------------------------------------------

      SUBROUTINE SETXNEW (XSCALE, ISTAT)

      IMPLICIT  NONE

*     Formal parameters:

      REAL              XSCALE(*)
      INTEGER           ISTAT

*     Global variables:

      INCLUDE          'SPECX_PARS'

*     Local variables:

      INTEGER           I
      INTEGER           NRET
      DOUBLE PRECISION  XSCALE8(LSPMAX)
      DOUBLE PRECISION  XFAC8(NQMAX)

*  Ok, go...

      CALL SETX8 (XSCALE8, XFAC8, NRET, ISTAT)
      IF (ISTAT.NE.0) RETURN

      DO I = 1, NRET
        XSCALE(I) = SNGL(XSCALE8(I))
      END DO

      RETURN
      END

C-----------------------------------------------------------------------

      SUBROUTINE SETX8 (XSCALE8, XFAC8, NRET, ISTAT)

C   Routine to calculate the X-axis in appropriate units ( velocity,
C   frequency, points or user-defined ) at points corresponding to
C   the data points, for all quadrants.
C
C   Updated Feb '92 to allow for other velocity reference frames and
C   transformation laws:
C
C   Two-way transformations:
C
C   - For *input*, we are given the velocity frame, velocity law and
C     a radial velocity. We assume that the first LO was corrected to
C     bring a line at velocity VLSR and frequency JFCEN (in this frame)
C     to the center of the passband. So the LO correction was clearly to
C     the nominal *source rest frame* (as indicated by VLSR).
C     The implication is that the radial velocity must be included in the
C     doppler corrections (consider a source at a very large velocity, much
C     larger than the interframe velocities, which must still be corrected
C     to bring the line to the center of the passband.)
C
C   - For *output*, to be consistent we should do the same. So we would have
C     to add VRAD on again. However this has the undesirable side-effect that
C     the line will automatically come out at zero velocity! (by definition).
C     In order to display the line at the value of VRAD in the desired frame
C     the display correction should be only back to the frame, and not to the
C     the source. Thus velocities will be *radial velocities as measured by
C     an observer in the nominated frame, applying the specified velocity law*.
C     An unfortunate consequence is that frequencies also will be as measured
C     with a frequency meter by an observer sitting in the reference frame.
C     In order to measure line frequencies say in the source rest frame one
C     has to correct to the source frame by setting the velocity in the
C     reference frame equal to VLSR (or in general, VRAD).
C
C     What if we have data recorded under one set of assumptions and want to
C     display it according to another? For *display* purposes we need a separate
C     value of velocity which can be applied to the new frame only. We
C     default to the input values, and demand that all values
C     (frame, vel-law and radial velocity) are set if you change the frame.

      IMPLICIT  NONE

C     Formal parameters

      DOUBLE PRECISION XSCALE8(1)      ! Location to return calculated X array
      DOUBLE PRECISION XFAC8(8)        ! Double precision channel separations
      INTEGER          NRET            ! Number of useful channels in XSCALE
      INTEGER          ISTAT           ! General status return

C     Include files

      INCLUDE  'FLAGCOMM'
      INCLUDE  'STACKCOMM'
      INCLUDE  'DOPPLER'

C     Other variables

C      INTEGER   I               ! General counter
      INTEGER   ISB             ! Sideband sign
      INTEGER   NOFF            ! Offset to first channel in XSCALE for sector
      INTEGER   NQ              ! Current sector

      REAL      VRAD               ! Radial velocity for display
      CHARACTER VFRAME*4,VFRAME2*4 ! 'TELL', 'HELI', 'GEO' or 'LSR'
      CHARACTER VDEF*3,VDEF2*3     ! 'OPT', 'RAD', or 'REL'

      DOUBLE PRECISION  FROBS   ! Rest freq. assumed used to correct obsn freq
      DOUBLE PRECISION  FRDIS   ! Rest freq. to be used for display (GHz)
      DOUBLE PRECISION  FCEN_T  ! Telluric quadrant centre freq (GHz)

C     Functions

      INTEGER  NTOT

C   OK, Go...

      ISTAT = 0

C----------------
C   Points scale: Pts labelled sequentially 1 -> NTOT(NQUAD)
C----------------

      IF (NXS .EQ. 1)   THEN
        CALL SETXPTS (XSCALE8, NTOT(NQUAD), XFAC8(1), ISTAT)
        DO NQ = 1, NQUAD
          XFAC8(NQ) = XFAC8(1)
        END DO

C                 -----------------------------------------
C
C     F R E Q U E N C Y   A N D   V E L O C I T Y   P R E A M B L E
C     - - - - - - - - -   - - -   - - - - - - - -   - - - - - - - -
C
C     There are far too many frequencies to keep track of!
C     For reference, the following are already defined:
C
C            JFREST(NQ)     =  Rest freq stored in scan header (KHz)
C            JFCEN(NQ)      =  Sector center freq (before any correction for
C                              non-linearity) in frame defined by LSRFLG (KHz)
C            FCEN(NQ)       =  Alternative rest freq supplied by S-L-R-F (GHz)
C            JFINC(NQ)      =  Fixed channel separation in telescope frame (Hz)

      ELSE IF (NXS.EQ.2 .OR. NXS.EQ.3)   THEN
C       First evaluate radial velocities etc if not available in header:

        IF (VSL.eq.0.0 .and. VES.eq.0.0 .and. VTE.eq.0.0) THEN
          CALL SETXASTRO   (ITIME, IDATE, ALAT,  ALONG,  TIMCOR, IUTFLG,
     &                      DEC,   RA,    VSL,   VES,    VTE,    ISTAT)
          IF (ISTAT.NE.0) THEN
            PRINT *,' --- setx --- problem with astrometry'
            RETURN
          END IF
        END IF

C       Find velocity frame and velocity law used during observation

        CALL VELDECODE (LSRFLG, VFRAME, VDEF)

CD      WRITE (ILOUT,*) 'Data obs''d in ',VFRAME,' frame; ',
CD   &         VDEF, ' velocity law; Velocity = ', VLSR, ' km/s'

        IF (CHANGE_FRAME) THEN
          VFRAME2 = VEL_REF
          VDEF2   = VEL_DEF
          VRAD    = VELOUT
        ELSE
          VFRAME2 = VFRAME
          VDEF2   = VDEF
          VRAD    = 0.0
        END IF

CD      WRITE (ILOUT,*) 'Data displayed in ',VFRAME2,' frame; ',
CD   &         VDEF2, ' velocity law; Vel. = ', VRAD, ' km/s'

C       Work out sideband sign, for use in generating OSCFREQ

        IF (SIDEBAND.EQ.'U') THEN
          ISB = +1
        ELSE
          ISB = -1
        END IF

C       Calculate baseline on sector-by-sector basis

        DO NQ = 1, NQUAD
          NOFF = NTOT(NQ-1) + 1

C         Set frequency offsets in tel frame and correct for any non-linearities
C         such as might be caused by an AOS

          CALL SETXINITX  (XSCALE8(NOFF), NPTS(NQ), JFINC(NQ),
     &                     FCORRECT,     6,        FRQCOEFF)

C         Choose a value of rest frequency applicable to the *observation*
C         and a (possibly) different value to use for *display*. Complicated
C         dependence on header variables.

          CALL SETXFREST (IQCEN, NQ, JFCEN, JFREST, FCEN, FROBS, FRDIS)
CD        PRINT *, 'Rest freq, observed, = ', FROBS
CD        PRINT *, 'Rest freq, display, =  ', FRDIS

C         Convert header centre frequency JFCEN to telluric value,
C         returned in FCEN_T (GHz). (Uses the LO and IF frequencies
C         if they are available in the header)

          IF (LOFREQ(NQ).eq.0.D0 .and. IFFREQ(NQ).eq.0.D0) THEN
            CALL SETXFTCEN (JFCEN(NQ), VFRAME, VDEF,
     &                      VTE, VES, VSL, VLSR, FCEN_T)
          ELSE
            FCEN_T  = LOFREQ(NQ) + IFFREQ(NQ)
          END IF

CD        PRINT *, 'Telluric center frequency = ', FCEN_T

          IF (IFFREQ(NQ).NE.0.D0) THEN
            OSCFREQ = FCEN_T - IFFREQ(NQ)
          ELSE IF (FIRST_IF.NE.0.0) THEN
            OSCFREQ = FCEN_T - ISB*FIRST_IF
          ELSE
            OSCFREQ = 0.D0
          END IF

C         (This gives the actual observed frequencies in all channels
C         by addition of FCEN_T(GHz) and current contents of XSCALE(MHz))

C         Correct to offset frequency in reference frame using appropriate
C         velocity scaling law (RADIO, OPTICAL or RELATIVISTIC). Rest freq
C         is set equal to FRDIS for display, may or may not be equal to FROBS.

          CALL SETXDOPP   (XSCALE8(NOFF), NPTS(NQ), VFRAME2, VDEF2,
     &                     VTE, VES, VSL, VRAD, FCEN_T, FRDIS)

C----------------
C  Frequency scale:
C----------------

C         Correct to relative or absolute frequencies as required:

          IF (NXS .EQ. 2) THEN
            CALL SETXFREQ (XSCALE8(NOFF), NPTS(NQ), ABS_FREQ, REST_REL,
     &                     FRDIS, XFAC8(NQ), REF_FREQ, ISTAT)

C----------------
C  Velocity scale:
C----------------

          ELSE IF (NXS .EQ. 3) THEN
            CALL SETXVEL (XSCALE8(NOFF), NPTS(NQ), VDEF2, FRDIS,
     &                    XFAC8(NQ), ISTAT)
CD          PRINT *, 'NQ, XFAC8 this quadrant = ', NQ, XFAC8(NQ)
          END IF

        END DO

C----------------
C   Other linear user scale:
C----------------

      ELSE IF (NXS .EQ. 4) THEN
        DO NQ = 1, NQUAD
          NOFF = NTOT(NQ-1) + 1
          CALL SETXUSER (XSCALE8(NOFF), NPTS(NQ), XFAC8(NQ), ISTAT)
        END DO
      END IF

      DO NQ = 1, NQUAD
        XFAC(NQ) = SNGL(XFAC8 (NQ))
      END DO

      NRET = NTOT(NQUAD)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SETXPTS (XSCALE8, N, XFAC8, ISTAT)

*  Called from SETX to establish scale of channels in arrays XSCALE

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER           N
      DOUBLE PRECISION  XSCALE8(N)
      DOUBLE PRECISION  XFAC8
      INTEGER           ISTAT

*     Local variables

      INTEGER   I

*  Ok, go...

      DO I = 1, N
        XSCALE8(I) = DFLOAT(I)
      END DO

      XFAC8 = 1.0D0

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SETXUSER (XSCALE8, N, XFAC8, ISTAT)

*  Called from SETX to establish linear user-defined scale in array XSCALE

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER           N
      DOUBLE PRECISION  XSCALE8(N)
      DOUBLE PRECISION  XFAC8
      INTEGER           ISTAT

*     Local variables

      INTEGER   I

*  Ok, go...

      DO I = 1, N
        XSCALE8(I) = DFLOAT(I-1)*XFAC8
      END DO

      XFAC8 = 1.0D0

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SETXASTRO (ITIME, IDATE, ALAT, ALONG, TIMCOR, IUTFLG,
     &                      DEC,   RA,    VSL,  VES,   VTE,    ISTAT)

*  Routine to work out all necessary astronomy for SETX.

      IMPLICIT  NONE

*     Formal parameters:

      CHARACTER          ITIME*(*)
      CHARACTER          IDATE*(*)
      DOUBLE PRECISION   ALAT
      DOUBLE PRECISION   ALONG
      DOUBLE PRECISION   TIMCOR
      DOUBLE PRECISION   RA
      DOUBLE PRECISION   DEC
      BYTE               IUTFLG
      REAL               VSL,  VES,  VTE
      INTEGER            ISTAT

*     Local variables:

      REAL               APRA,  APDEC
      REAL               HA
      DOUBLE PRECISION   XLST
      DOUBLE PRECISION   DATJUL
      DOUBLE PRECISION   UTD
      DOUBLE PRECISION   PI

      PARAMETER ( PI = 3.141592654D0 )

C  Ok, go...

C     Astronomical times (UTD, LST, Julian date)

      CALL ASTRO_TIMES (ITIME, IDATE, ALONG, TIMCOR, IUTFLG,
     &                  UTD, XLST, DATJUL)

C       Astronomical positions (RA, Dec, HA)

      APDEC = SNGL(DEC*PI/180.D0)             ! Gives DEC in radians
      APRA =  SNGL(RA*PI/180.D0)              ! ..... and RA in true radians
      HA   = MOD (SNGL(XLST*PI/12. - DBLE(APRA)), SNGL(2.D0*PI)) ! yields HA in radians

CD    PRINT *, '--- setx ---'
CD    PRINT *, '     apra (radians):   ', apra
CD    PRINT *, '     apdec (radians):  ', apdec
CD    PRINT *, '     lst (hours):      ', xlst
CD    PRINT *, '     ha (radians):     ', ha

C     Astronomical radial velocities (sun-lsr, earth-sun, tel-earth)

      CALL ASTRO_VELS  (DATJUL, APRA, APDEC, ALAT, HA, VSL, VES, VTE)

CD    PRINT *, '     vsl(km/s)         ', vsl
CD    PRINT *, '     ves(km/s)         ', ves
CD    PRINT *, '     vte(km/s)         ', vte
CD    PRINT *, '     vsl+ves+vte(km/s) ', vsl+ves+vte

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SETXINITX  (XSCALE8, N, JFINC, FCORRECT, NF, FRQCOEFF)

*     Initializes contents of XSCALE to telluric offsets in MHz, corrected
*     if necessary for any non-linearities such as those of an AOS

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER           N
      DOUBLE PRECISION  XSCALE8(N)
      INTEGER           JFINC
      LOGICAL           FCORRECT
      INTEGER           NF
      REAL              FRQCOEFF(NF)

*     Local variables:

      INTEGER           I
      DOUBLE PRECISION  CHMID

*  Ok, go...

*     Assign "true" (telescope frame) frequencies to channels and
*     correct for non-linear frequency scale if necessary (e.g. AOS)

      CHMID = 0.5D0*DFLOAT(N+1)

      DO I = 1, N
        XSCALE8(I) = 1.D-6*DFLOAT(JFINC) * (DFLOAT(I)-CHMID)
      END DO

      IF (FCORRECT) THEN
        CALL SETXFRQFIX (N, XSCALE8, NF, FRQCOEFF)
        PRINT *, ' -- AOS frequency scale linearization applied --'
      END IF

CD    PRINT *, ' -- setxinitx --'
CD    PRINT *, '    XSCALE values - beg, middle and end...'
CD    PRINT *, XSCALE(1), 0.5*(XSCALE(N/2)+XSCALE(N+1-N/2)), XSCALE(N)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SETXFRQFIX (N, X, NC, COEFF)

*  Routine to apply polynomial correction to raw frequency array, and
*  return corrected array in the same location. Note that the correction
*  polynomial is applied in the sense:
*          corrected = raw + polynomial
*  Polynomial coefficients work on supplied frequencies (assumed MHz)

      IMPLICIT NONE

*     Formal parameters

      INTEGER N             ! number of points in frequency array
      INTEGER NC            ! number of polynomial correction coefficients
      DOUBLE PRECISION X(N) ! Raw and corrected frequency array
      REAL      COEFF(NC)   ! Coefficients.

*     Other variables

      INTEGER  I, J, K      ! counting variables
      DOUBLE PRECISION V    ! temporary store for x(i)
      DOUBLE PRECISION W    ! accumulator for correction

*  Ok? then do it.

*     First check how many terms supplied are actually non-zero

      J = NC
      DO WHILE (COEFF(J).EQ.0.0 .AND. J.GE.1)
        J = J - 1
      END DO
      IF (J.EQ.0) RETURN

      DO I = 1, N
        V = X(I)
        W = DBLE(COEFF(J))
        IF (J.GE.2) THEN
          DO K = 2, J
            W = V*W + DBLE(COEFF(J+1-K))
          END DO
        END IF
        X(I) = X(I) + W
      END DO

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SETXFREST (IQCEN, NQ, JFCEN, JFREST, FCEN,
     &                      FROBS, FRDIS)

*     Routine to choose a "rest frequency" that will define the pure
*     frequency shift assumed to have been applied to the 1st LO in
*     making the observation.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER          IQCEN
      INTEGER          NQ
      INTEGER          JFCEN(*)
      INTEGER          JFREST(*)
      DOUBLE PRECISION FCEN(*)
      DOUBLE PRECISION FROBS
      DOUBLE PRECISION FRDIS

*     Local variables:

C      INTEGER          I
      INTEGER          NCEN

*  Ok, go...

*     Determine 'master' quadrant for this observation (if any)

      IF (IQCEN.EQ.0) THEN
        NCEN = NQ
      ELSE
        NCEN = IQCEN
      END IF

CD    PRINT *, '-- setxfrest --'
CD    PRINT *, '   centre quadrant = ', NCEN
CD    PRINT *, '   fcen array = ', (fcen(i),i=1,8)

*     Set rest frequency assumed to have been applied in observation

      IF (JFREST(NCEN).NE.0) THEN
        FROBS = 1.D-6 * DFLOAT(JFREST(NCEN))
CD      PRINT *, '   obs freq from jfrest = ', FROBS
      ELSE
        FROBS = 1.D-6 * DFLOAT(JFCEN(NCEN))
CD      PRINT *, '   obs freq from jfcen  = ', FROBS
      END IF


*     Choose a rest frequency to use for display
*     Default is that rest frequency of interest was arranged to
*     come out at centre of a spectrum; this is over-ridden by a
*     non-zero frequency in FCEN(NCEN)

      IF (DABS(FCEN(NCEN)).GT.1.0D-6)   THEN
        FRDIS = FCEN(NCEN)
        WRITE (6,*)
        WRITE (6,*) 'Warning ** Rest frequency set using values'//
     &              ' from SET-LINE-REST-FREQ.'
        WRITE (6,*) 'Do S-L-R-F to use defaults from header'
        WRITE (6,*)
      ELSE
        FRDIS = FROBS
      END IF

CD    PRINT *, '   dis frequency = ', FROBS

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SETXFTCEN  (JFCEN, VFRAME, VDEF, VTE, VES, VSL, VRAD,
     &                       FCEN_T)

*     Routine to evaluate frequency corresponding to center of quadrant
*     in telluric frame.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER           JFCEN          ! Header obs freq (KHz)
      CHARACTER         VFRAME*4
      CHARACTER         VDEF*3
      REAL              VTE, VES, VSL
      REAL              VRAD
      DOUBLE PRECISION  FCEN_T         ! Returned, sector centre freq (GHz)

*     Local variables

      REAL              VEL
      CHARACTER         VVFRAME*4
      CHARACTER         VVDEF*3
      DOUBLE PRECISION  VC

      PARAMETER ( VC = 299792.D0 )

*  Ok, go...

      VVFRAME = VFRAME
      VVDEF   = VDEF
      CALL UUCASE (VVFRAME)
      CALL UUCASE (VVDEF)

CD    PRINT *, ' -- setxftcen --'
CD    PRINT *, '    given velocity frame = ', VVFRAME
CD    PRINT *, '    velocity definition =  ', VVDEF

*     First find velocity of frame with respect to the telescope

      IF (VVFRAME.EQ.'TELL') THEN
        VEL = VRAD
      ELSE IF (VVFRAME.EQ.'GEO') THEN
        VEL = VRAD + VTE
      ELSE IF (VVFRAME.EQ.'HELI') THEN
        VEL = VRAD + VTE + VES
      ELSE IF (VVFRAME.EQ.'LSR') THEN
        VEL = VRAD + VTE + VES + VSL
      ELSE
        PRINT *, 'Unknown velocity frame: ', VVFRAME
        VEL = 0.0
      END IF

CD    PRINT *, '    velocity frame/tel  =  ', VEL

*     Now apply appropriate doppler correction

*     Radio definition, v/c = (f-f0)/f0

      IF (VVDEF.EQ.'RAD') THEN
        FCEN_T = 1.D-6*DFLOAT(JFCEN) * (1.D0 - DBLE(VEL)/VC)

*     Optical definition, v/c = (lambda-lambda0)/lambda0

      ELSE IF (VVDEF.EQ.'OPT') THEN
        FCEN_T = 1.D-6*DFLOAT(JFCEN) / (1.D0 + DBLE(VEL)/VC)

*     "Relativistic" correction (note, correct only if no transverse vel!)

      ELSE IF (VVDEF.EQ.'REL') THEN
        FCEN_T = 1.D-6*DFLOAT(JFCEN) *
     +           SQRT((1.D0-DBLE(VEL)/VC)/(1.D0+DBLE(VEL)/VC))

*     Missing or incorrect velocity definition...

      ELSE
        PRINT *, 'Unknown velocity correction type: ', VVDEF
        PRINT *, '... Using "RADIO" velocity definition'
        FCEN_T = 1.D-6*DFLOAT(JFCEN) * (1.D0 - DBLE(VEL)/VC)

      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SETXDOPP (XSCALE8, N, VFRAME, VDEF, VTE, VES, VSL,
     &                     VRAD, FCEN_T, FREST)

C     Correct to frequency in reference frame using appropriate velocity
C     scaling law (RADIO, OPTICAL or RELATIVISTIC). Note that sector
C     centre-freq and offsets (FCEN_T and XSCALE) are passed separately;
C     this is because XSCALE used to be single precision so couldn't handle
C     the sum in a recoverable way.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER           N
      DOUBLE PRECISION  XSCALE8(N)
      CHARACTER         VFRAME*4
      CHARACTER         VDEF*3
      REAL              VTE, VES, VSL
      REAL              VRAD
      DOUBLE PRECISION  FCEN_T             ! Telluric centre freq sector (GHz)
      DOUBLE PRECISION  FREST              ! Rest freq, GHz

*     Include files:

      INCLUDE          'DOPPLER'

*     Local variables:

      INTEGER           I
      REAL              VEL
      CHARACTER         VVFRAME*4
      CHARACTER         VVDEF*3
*     DOUBLE PRECISION  DOPPFAC
      DOUBLE PRECISION  OBS_FREQ

      DOUBLE PRECISION  VC

      PARAMETER ( VC = 299792.D0 )


*  Ok, go...

      VVFRAME = VFRAME
      VVDEF   = VDEF
      CALL UUCASE (VVFRAME)
      CALL UUCASE (VVDEF)

*     First find velocity of frame with respect to the telescope

CD     PRINT *, ' -- setxdopp --'
CD     PRINT *, '    velocity definition = ', VVDEF

      IF (VVFRAME.EQ.'TELL') THEN
        VEL = VRAD
      ELSE IF (VVFRAME.EQ.'GEO') THEN
        VEL = VRAD + VTE
      ELSE IF (VVFRAME.EQ.'HELI') THEN
        VEL = VRAD + VTE + VES
      ELSE IF (VVFRAME.EQ.'LSR') THEN
        VEL = VRAD + VTE + VES + VSL
      ELSE
        PRINT *, 'Unknown velocity frame: ', VVFRAME
        VEL = 0.0
      END IF

CD     PRINT *, '    Velocity of frame = ', VEL

*     Calculate appropriate doppler correction (depending on definitions)

      IF (VVDEF.EQ.'RAD') THEN
        DOPPFAC = (1.D0 - DBLE(VEL)/VC)
      ELSE IF (VVDEF.EQ.'OPT') THEN
        DOPPFAC = 1.D0 / (1.D0 + DBLE(VEL)/VC)
      ELSE IF (VVDEF.EQ.'REL') THEN
        DOPPFAC = SQRT ((1.D0-VEL/VC)/(1.D0+DBLE(VEL)/VC))
      ELSE
        PRINT *,'Unknown velocity correction type: ', VVDEF
        DOPPFAC = (1.D0 - DBLE(VEL)/VC)
      END IF

CD     PRINT *, '    doppler factor = ', DOPPFAC
CD     PRINT *, '    sector center freq (GHz)  = ', FCEN_T
CD     PRINT *, '    reference rest freq (GHz) = ', FREST

*     ... and apply to the XSCALE array
*     (retain in the form of offsets, but now from display rest frequency)
*     Note that doppler factor is applied as a division by the factor used
*     to scale from source to telescope, rather than as multiplying by a
*     similar factor for opposite velocity; this ensures that a scale value
*     originally at the rest frequency in the source frame will end up back
*     there in the display despite transforming to and from telluric frame.

CD     PRINT *, '    first and last channels:', xscale8(1), xscale8(n)

      DO I = 1, N
        OBS_FREQ  = FCEN_T + XSCALE8(I)*1.D-3
        XSCALE8(I) = 1000.D0* (OBS_FREQ/DOPPFAC - FREST)
      END DO

CD     PRINT *, '    first and last channels:', xscale8(1), xscale8(n)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SETXFREQ (XSCALE8, N, ABS_FREQ, REST_REL, FREST,
     &                     XFAC8, REF_FREQ, ISTAT)

*  Routine to assign frequencies to individual elements of XSCALE.
*  At the time this routine is called we have the frequencies defined in
*  some frame, relative to some canonical rest-frequency, and we also know
*  the rest frequency.
*
*  Display options specified by ABS_FREQ and REST_REL:
*
*                                      REST_REL
*                          TRUE                       FALSE
*  ABS_FREQ  --------------------------------------------------------------
*     TRUE   |               total frequency in given frame (GHz)         |
*            --------------------------------------------------------------
*     FALSE  |  offset from rest freq(MHz) | offset from mid-sector (MHz) |
*            --------------------------------------------------------------

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER           N
      DOUBLE PRECISION  XSCALE8(N)
      LOGICAL           ABS_FREQ
      LOGICAL           REST_REL
      DOUBLE PRECISION  FREST
      DOUBLE PRECISION  XFAC8
      DOUBLE PRECISION  REF_FREQ
      INTEGER           ISTAT

*     Local variables

      INTEGER           I
C      INTEGER           MID
C      REAL              XMID

*  Ok, go...

      ISTAT = 0

      IF (ABS_FREQ) THEN

        DO I = 1, N
          XSCALE8(I) = FREST + XSCALE8(I)/1000.D0
        END DO

      ELSE

        IF (.NOT.REST_REL) THEN

          DO I = 1, N
            XSCALE8(I) = XSCALE8(I) - 1000.D0 * (REF_FREQ - FREST)
          END DO

          PRINT *, ' -- setxfreq --'
          PRINT *, '    frequencies are relative to reference-freq!'
          PRINT *, '    current reference freq (GHz) = ', REF_FREQ

        ELSE

*         Do nothing; this is what the current contents of XSCALE *are*

        END IF
      END IF

*     Finally, set XFAC to some average value indicative of the whole array.

      IF (N.GT.1) THEN
        XFAC8 = (XSCALE8(N)-XSCALE8(1)) / DFLOAT(N-1)
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SETXVEL (XSCALE8, N, VDEF, FREST, XFAC8, ISTAT)

*  Called from SETX to assign velocities to elements of XSCALE.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER           N
      DOUBLE PRECISION  XSCALE8(N)
      CHARACTER         VDEF*3
      DOUBLE PRECISION  FREST
      DOUBLE PRECISION  XFAC8
      INTEGER           ISTAT

*     Local variables

      INTEGER          I
      CHARACTER        VVDEF*3
      DOUBLE PRECISION FRESTM
      DOUBLE PRECISION  VC

      PARAMETER ( VC = 299792.D0 )

*  Ok, go...

      ISTAT = 0

      VVDEF = VDEF
      CALL UUCASE (VVDEF)

*     Evaluate doppler velocity within final frame according to velocity law
*     Cannot be represented by simple doppler factor for OPT and REL laws,
*     so apply as well.

      FRESTM = 1.0D3 * FREST

CD    PRINT *, ' -- setxvel --'
CD    PRINT *, '    first and last channels: ', xscale8(1), xscale8(n)

      IF (VVDEF.EQ.'RAD') THEN
        DO I = 1, N
          XSCALE8(I) = - VC*XSCALE8(I)/FRESTM
        END DO

      ELSE IF (VVDEF.EQ.'OPT') THEN
        DO I = 1, N
          XSCALE8(I) = - VC*XSCALE8(I)/(FRESTM+XSCALE8(I))
        END DO

      ELSE IF (VVDEF.EQ.'REL') THEN
        DO I = 1, N
          XSCALE8(I) = VC * (1.D0-((FRESTM+XSCALE8(I))/FRESTM)**2)
     &                    / (1.D0+((FRESTM+XSCALE8(I))/FRESTM)**2)
        END DO

      ELSE
        WRITE (6,*) 'Unknown velocity definition: ', VDEF
        ISTAT = 18
        RETURN
      END IF

CD    PRINT *, '    first and last channels: ', xscale8(1), xscale8(n)

*     Finally, produce an estimate of the average velocity increment

      IF (N.GT.1) THEN
        XFAC8 = (XSCALE8(N)-XSCALE8(1)) / DFLOAT(N-1)
CD      PRINT *, '    estimate of XFAC = ', xfac8
      END IF

      RETURN
      END

*-----------------------------------------------------------------------



