C+
      SUBROUTINE FIG_FIGWAVES(GRATS,STEP,GRTPCH,GRATORD,GRT0ORD,GRTA2S,
     :                         GRATSP,DETSP,LENGTH,WIDTH,WAVES,STATUS)
C
C     F I G _ F I G W A V E S
C
C     Calculates the wavelength values for a FIGS exposure, given
C     the grating details.  This is a utility routine for FIGS32N,
C     based closely on the Pascal routine FDR_WAVES used by the FIGS
C     data acquisition system.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) GRATS     (Real) Grating start step.  Ie the step position
C                   for detector n = 1 at grating position m = 1
C     (>) STEP      (Real) Grating step value.  Ie amount grating
C                   steps going from position m =1 to m = 2 etc.
C     (>) GRTPCH    (Real) Grating pitch.
C     (>) GRATORD   (Real) Grating order.
C     (>) GRT0ORD   (Real) Grating step position at 0th order.
C     (>) GRTA2S    (Real) Grating arc sec per halfstep.
C     (>) GRATSP    (Real) Number of grating positions used.
C     (>) DETSP     (Real) The detector spacing in steps.
C     (<) LENGTH    (Integer) Number of elements in spectrum at
C                   these settings.
C     (<) WIDTH     (Real) The width of a detector in wavelength terms.
C     (<) WAVES     (Real array) Returned with the wavelength values
C                   for each grating position and detector.  First are
C                   all the values for the detectors at grating step 1,
C                   then the values for grating step 2, etc.
C     (<) STATUS    (Integer) Status return code.  0 => OK.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     PAR_WRUSER    (PAR_ routine) Send string to user.
C
C                                            KS / AAO 27th May 1985
C     Modified:
C
C     22ns July 1986.  KS / AAO.  WIDTH added.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER LENGTH,STATUS
      REAL GRATS, STEP, GRTPCH, GRATORD, GRT0ORD, GRTA2S, GRATSP, DETSP
      REAL WIDTH, WAVES(416)
C
C     Local variables
C
      INTEGER DET, GPOSN, PTR
      REAL    ANGLE, FACTOR, STEPV
C
C     Arcsec/radian conversion factor
C
      REAL RADPA
      PARAMETER (RADPA = 2.0*3.14159265/(360.0*60.0*60.0))
C
C     Number of FIGS detectors
C
      INTEGER DETECTORS
      PARAMETER (DETECTORS=16)
C
C     Check any parameters that could blow up this routine - this check
C     is to prevent crashes rather than an exhaustive validation

      IF (GRTA2S.LE.0.0) THEN
         CALL PAR_WRUSER(
     :         'Invalid grating parameter; GRTA2S is negative',STATUS)
         STATUS=1
         GO TO 500
      END IF
      IF (GRATORD.LE.0.0) THEN
         CALL PAR_WRUSER(
     :         'Invalid grating parameter; GRATORD is negative',STATUS)
         STATUS=1
         GO TO 500
      END IF
      LENGTH=GRATSP*DETECTORS
      IF (LENGTH.GT.416) THEN
         CALL PAR_WRUSER(
     :         'Invalid grating parameter; GRATSP is too large',STATUS)
         STATUS=1
         GO TO 500
       END IF
C
C      If OK, calculate wavelengths
C
       FACTOR=2.0*GRTPCH/GRATORD
       PTR=1
       DO GPOSN=1,INT(GRATSP)
          DO DET=1,DETECTORS
             STEPV= GRATS + DETSP*(DET-1) + (GPOSN-1)*STEP
             ANGLE=ABS((STEPV-GRT0ORD)*GRTA2S)*RADPA
             WAVES(PTR)=FACTOR*SIN(ANGLE)
             PTR=PTR+1
          END DO
      END DO
C
C     Calculate WIDTH as one sixth of the detector spacing.
C
      WIDTH=(WAVES(2)-WAVES(1))/6.0
C
      STATUS=0
C
  500 CONTINUE
C
      END
