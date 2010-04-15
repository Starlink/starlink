C+
      SUBROUTINE HCROSS
C
C     H C R O S S
C
C     Main body of the Figaro HCROSS function.  This computes
C     the cross-correlation of two spectra and the location of the
C     central peak of the cross-correlation.  It can be used to
C     determine a relative shift between two spectra. Routines added to
C     calculate the confidence levels and improved error estimates
C     (CCFPAR) according to analysis in Heavens A.F.,1993, MNRAS, 263,
C     735.  Routine added to calculate the redshift and error in
C     redshift directly (GET_LOGSTEP and CALCRS).  The cross-correlation
C     function can also be saved in a disk structure.
C
C     Command parameters -
C
C     SPECTRUM    (Character) The spectrum to be compared with
C                 a template spectrum.
C     TEMPLATE    (Character) The template spectrum to be used.
C                 The two spectra should be the same length.
C     BASERED     (Numeric) Redshift of template spectrum.
C                 Negative=blueshift.  Required if VELOCITIES=FALSE
C     BASERR      (Numeric) Redshift error of template.
C                 Required if VELOCITIES=FALSE
C     BASEVEL     (Numeric) Recession velocity in km/s
C                 (see keyword VELOCITIES below) of template spectrum.
C                 Negative=blueshift. Required if VELOCITIES=TRUE
C     BASEVERR    (Numeric) Velocity error of template (km/s).
C                 Required if VELOCITIES=TRUE
C     XSTART      (Numeric) Data with an axis data value less than
C                 XSTART will be ignored in the cross-correlation.
C     XEND        (Numeric) Data with an axis data value greater than
C                 XEND will also be ignored.  Note that these values are
C                 used to determine the channel numbers to be used
C                 for SPECTRUM, and the same ones will be used for
C                 TEMPLATE, even if TEMPLATE has a  different axis
C                 structure.
C     CBPC        (Numeric) Percentage of spectrum covered by a cosine
C                 bell prior to application of the FFT.
C     CROSS       (Character) the name of the data structure to hold
C                 the cross-correlation, if it is to be saved.
C                 The file created will be cross.dst, and will look
C                 like an ordinary spectrum - ie can be plotted by
C                 SPLOT, etc.  CROSS is ignored if RECORD is not
C                 specified.
C
C     Command keywords -
C
C     FITCONT     If specified, a continuum fit is performed on the
C                 two spectra prior to application of the cosine bell.
C     RECORD      If specified, the cross-correlation of the two
C                 spectra will be recorded as a new data structure.
C     VELOCITIES  If true, template redshift and error are entered in
C                 km/s (Negative=blueshift), and results are printed in km/s.
C                 Recommended option, and default, is FALSE
C                 i.e. a redshift z is expected and returned.
C                 Velocity option only makes sense if v<<c.
C                 v=cz and is accurate to O(z)
C
C     User variables used -
C
C     SHIFT       (Numeric) The relative shift of the two spectra.
C     REDSHIFT    (Numeric) The redshift of the spectrum.
C     REDERR      (Numeric) Redshift error of spectrum.
C     VELOCITY    (Numeric) Recession velocity of spectrum (km/s):
C                           Only meaningful if <<c.
C     VELERR      (Numeric) Recession velocity error of spectrum (km/s)
C     CONF        (Numeric) The confidence in the redshift of the spectrum
C     WARN        (Numeric) =1 if routine detects badly-matched spectra.
C                           =0 otherwise (not a guarantee of good spectra)
C
C                                             KS / CIT 3rd Oct 1983
C     Modified:
C
C      6th May 1985  KS / AAO.  FITCONT and CBPC parameters added.
C     21st Nov 1988  JM / RAL. Modified to use DSA_ routines.
C                    Dynamic memory handling changed to use
C                    DYN_ routines
C     23rd Aug 1990  KS / AAO. Minor tidying. Changed STATUS in
C                    PAR_WRUSER calls to IGNORE, modified calculation of
C                    workspace pointers.
C     16th Jan 1991  JMS / AAO. Included PAR_ABORT and STATUS checks for
C                    user requested aborts.
C     21st Jan 1991  JMS / AAO. Restricted maximum allowed dimensions of
C                    data arrays to 1D.
C     12th Feb 1991  JMS / AAO. Included check to test that the data
C                    arrays of the two spectra match in size.
C     13th Feb 1991  JMS / AAO. Added an extra check to test that the
C                    X-axes match. Now aborts if specified range is of
C                    zero length.
C     14th Feb 1991  JMS / AAO. Changed minimum range length to two
C                    pixels.
C     22nd Feb 1991  JMS / AAO. Changed minimum range length to three
C                    pixels.
C     23rd Sep 1992  HME / UoE, Starlink.  INCLUDE changed. Call
C                    PAR_WRUSER rather than DSA_WRUSER.
C      5th Oct 1993  AFH,NEJ / UoE. Routines added to calculate the
C                    confidence levels and improved error estimates
C                    (CCFPAR) to comply with Heavens A.F.,1993, MNRAS,
C                    263,735. Routine added to calculate the redshift
C                    and error in redshift directly (GET_LOGSTEP and
C                    CALCRS).
C     17th Mar 1994  **J changed to **REAL(J) (also **(2*J)) in
C                    CALC_JTH_MOMENT
C     13th Jan 1995  AFH / UoE.  Error calculation can fail if spectrum
C                    and template are very different.  Tonry and Davis
C                    result is returned, with a warning message.
C                    - Output format is more compact.
C                    - Option to use km/s rather than redshifts
C                    introduced (via VELOCITIES keyword).
C     25th Apr 1995  HME / UoE, Starlink. No longer use NAG.
C      9th Jan 1996  AFH / UoE. Bug in 0^0 calculation on alphas
C                    in CALC_JTH_MOMENT fixed.  Minor bug fixed in
C                    WARNING message (see 13/1/95) appearing when
C                    autocorrelating.
C     25th Mar 1996  HME / UoE, Starlink. Use PDA_DERF, not
C                    inadvertently DERF.
C     2005 June 7    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions used
C
      LOGICAL PAR_ABORT
      INTEGER DSA_TYPESIZE

C
C     Local constants
C
      REAL    C          ! Speed of light in km/s
      PARAMETER (C=2.997924590E5)
      INTEGER NW         ! Number of contemporaneous work arrays
      PARAMETER (NW = 8)
C
C     Local variables
C
      INTEGER ARRAY0     ! Pointer to workspace holding SPECT data
      INTEGER ARRAY1     ! Pointer to workspace holding TEMPLATE data
      REAL    BASERED    ! Redshift of template
      REAL    BASERR     ! Error in redshift of template
      REAL    BASEVEL    ! Recession velocity of template (km/s)
      REAL    BASEVERR   ! Error in velocity of template (km/s)
      INTEGER BDOUB      ! Number of bytes per double prec. number
      INTEGER BFLOAT     ! Number of bytes per floating point number
      INTEGER BYTES      ! Number of bytes
      REAL    CBPC       ! Cosine bell percentage coverage
      LOGICAL CFIT       ! If false, disables the usual continuum fit
      INTEGER CFN        ! Pointer to the correlation function array
      REAL    CONF       ! The confidence in the redshift
      INTEGER CPTR       ! Dynamic memory pointer to CORRL data
      REAL    DELTA      ! The error in SHIFT
      LOGICAL DELTA_OK   ! TRUE if error calculated by Fourier methods
C                          FALSE if width of ccf used (only if Fourier
C                          fails)
      INTEGER DIMS(1)    ! Accommodates data dimensions
      INTEGER FSTAT      ! Running status for calculation of SUMMAT
C                          returned as non-zero from CCFPAR if a problem
C                          of division by zero occurs.
      INTEGER FT0        ! Pointer to the fourier transform of SPECT
C                          data
      INTEGER FT1        ! Pointer to the fourier transform of TEMPLATE
C                          data
      INTEGER FTACFN     ! Pointer to the fourier transform of
C                          antisymetric part of the correlation function
      INTEGER FTFCN      ! Pointer to the fourier transform of CORRL
C                          data
      REAL    WARN       ! Set to 1 if routine detects
C                          badly matched spectra.  0 otherwise
      INTEGER IGNORE     ! Status for VAR_SETNUM and PAR_WRUSER calls
      LOGICAL ISNEW      ! Is address new to CNF?
      INTEGER IXST       ! Pixel number associated with XSTART
      INTEGER IXEN       ! Pixel number associated with XEND
      INTEGER KZ(4)      ! Defines the cosine bell used to filter the
C                          FTs
      REAL    LOGSTEP    ! The constant in the transformation used to
C                          bin the spectra
      INTEGER NDIM       ! Number of dimensions in data
      INTEGER NELM       ! Number of data elements in object
      INTEGER NEXT       ! Used in encoding STRING to report answer
      DOUBLE PRECISION  NITEMS(1)  ! Axis numeric items retrieved
      LOGICAL NORM       ! True if cross-correlation fn. to be
C                          normalised
      INTEGER NX0        ! The number of elements in the two spectra.
      INTEGER NX         ! Either equal to NX or next highest power of 2
      LOGICAL RECORD     ! True if correlation fn. to be written to a
C                          file
      REAL    REDERR     ! The error in REDSHIFT
      REAL    REDSHIFT   ! Shift of spectrum accounting for template
C                          shift
      REAL    SHIFT      ! Shift of peak of correlation fn. from zero
C                          point
      INTEGER SLOT       ! Slot number
      INTEGER SLOTW(NW)  ! Slot numbers for workspace
      INTEGER SPTR       ! Pointer to SPECT data
      INTEGER STATUS     ! Running status for DSA_ routines
      CHARACTER STRING*100 ! String used to report answer
      INTEGER TPTR       ! Pointer to TEMPLATE data
      REAL    VELERR     ! Recession velocity error of spectrum (km/s)
      LOGICAL VELOCITIES ! True if input/output is in km/s
C                          False if input and output are redshifts
      REAL    VELOCITY   ! Recession velocity (in km/s) of spectrum
      REAL    WIDTH      ! The width of the correlation function
      INTEGER WPTR       ! Temporary pointer
      REAL    XEND       ! Last axis data value used
      REAL    XSTART     ! First axis data value used
      INTEGER XV         ! Pointer to array holding spectra pixel values
      REAL    ZPC        ! Percentage of spectrum covered at each end
C                          by a cosine bell prior to fourier
C                          transformation

C
C     Initial values
C
      STATUS = 0
      FSTAT  = 0
C
C     Open DSA
C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Get byte size for float and double precision numbers
C
      BFLOAT=DSA_TYPESIZE('FLOAT',STATUS)
      BDOUB=DSA_TYPESIZE('DOUBLE',STATUS)
C
C     Get the name of SPECTRUM and open the file
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Ditto TEMPLATE
C
      CALL DSA_INPUT('TEMPL','TEMPLATE',STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Get sizes of both data arrays
C
      CALL DSA_DATA_SIZE ('SPECT',1,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
      CALL DSA_DATA_SIZE ('TEMPL',1,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
      CALL DSA_MATCH_SIZES('SPECT','TEMPL',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
      CALL DSA_MATCH_AXIS('SPECT',1,'TEMPL',1,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Find out if user wishes to use km/s for template and spectrum
C     input and output, rather than redshifts.

      CALL PAR_RDKEY('VELOCITIES',.FALSE.,VELOCITIES)

C
C     Get redshift of template and error
C     If VELOCITIES keyword is set TRUE, redshifts will be velocities in km/s:
C
      IF(VELOCITIES)THEN
         CALL PAR_RDVAL('BASEVEL',-1.0E30,1.0E30,0.0,' ',BASEVEL)
         CALL PAR_RDVAL('BASEVERR',-1.0E30,1.0E30,0.0,' ',BASEVERR)
         BASERED = BASEVEL/C
         BASERR  = BASEVERR/C
      ELSE
         CALL PAR_RDVAL('BASERED',-1.0E30,1.0E30,0.0,' ',BASERED)
         CALL PAR_RDVAL('BASERR',-1.0E30,1.0E30,0.0,' ',BASERR)
      END IF
C
C     Use the utility routine DSA_AXIS_RANGE to get XSTART and XEND, then reset
C     NX0 to reflect the length of the spectrum to be actually used
C
      CALL DSA_AXIS_RANGE('SPECT',1,' ',.FALSE.,XSTART,XEND,
     :                     IXST,IXEN,STATUS)
C
C     See if there is any indication that the input spectrum and template
C     are binned on a log scale. They should be. Note that NITEMS(1) as
C     returned by DSA_GET_AXIS_INFO is the log binning flag.
C
      CALL DSA_GET_AXIS_INFO('SPECT',1,0,0,
     :                        1,NITEMS,STATUS)
      IF(NITEMS(1).NE.1.0) THEN
         CALL PAR_WRUSER(
     :          'Warning spectrum data does not appear to be scrunched',
     :   STATUS)
      END IF
C
      CALL DSA_GET_AXIS_INFO('TEMPL',1,0,0,
     :                        1,NITEMS,STATUS)
      IF(NITEMS(1).NE.1.0) THEN
         CALL PAR_WRUSER(
     :          'Warning template data does not appear to be scrunched',
     :   STATUS)
      END IF
C
C     Get LOGSTEP so we can work out the redshift directly
C
      CALL GET_LOGSTEP(XSTART,XEND,IXST,IXEN,LOGSTEP)
C
C     Check the specified range (exit if less than or equal to three pixels)
C     This is done because if range is too small it causes a stack dump to
C     occur.
C
      IF ((IXEN-IXST).LE.3) THEN
         CALL PAR_WRUSER('The range you have specified is too small.'//
     :      ' Try again with different XSTART and XEND values.',STATUS)
         GOTO 500
      END IF
C
      NX0=IXEN-IXST+1
      IF(STATUS.NE.0)GOTO 500
C
C     The cross-correlation needs a lot of workspace, so grab that now.
C
      CALL GEN_POWER2(NX0,NX)
C
C     Calculate values for pointers into work area.
C
      CALL DSA_GET_WORK_ARRAY(NX0,'FLOAT',ARRAY0,SLOTW(1),STATUS)
      CALL DSA_GET_WORK_ARRAY(NX0,'FLOAT',ARRAY1,SLOTW(2),STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'DOUBLE',FT0,SLOTW(3),STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'DOUBLE',FT1,SLOTW(4),STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'DOUBLE',FTACFN,SLOTW(5),STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'DOUBLE',FTFCN,SLOTW(6),STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'FLOAT',XV,SLOTW(7),STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'FLOAT',CFN,SLOTW(8),STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     See if continuum is to be fitted, and get cosine bell coverage
C
      CALL PAR_RDKEY('FITCONT',.TRUE.,CFIT)
      CALL PAR_RDVAL('CBPC',0.,100.,10.,'percent',CBPC)
C
C     Find out if the cross-correlation is to be recorded, and if so,
C     get the name of the output file.
C
      CALL PAR_RDKEY('RECORD',.FALSE.,RECORD)
      IF (PAR_ABORT())GOTO 500         ! User requested abort
      IF (RECORD) THEN
         CALL DSA_OUTPUT('CORRL','CROSS',' ',0,0,STATUS)
      END IF
C
C     Read the spectrum and template data into the work arrays
C
      CALL DSA_MAP_DATA('SPECT','READ','FLOAT',WPTR,SLOT,STATUS)
      CALL DYN_INCAD(WPTR,'FLOAT',IXST-1,SPTR,ISNEW,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
      BYTES=BFLOAT*NX0
      CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(SPTR)),%VAL(CNF_PVAL(ARRAY0)))
      IF (ISNEW) CALL CNF_UNREGP(TPTR)
C
      CALL DSA_MAP_DATA('TEMPL','READ','FLOAT',WPTR,SLOT,STATUS)
      CALL DYN_INCAD(WPTR,'FLOAT',IXST-1,TPTR,ISNEW,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
      BYTES=BFLOAT*NX0
      CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(TPTR)),%VAL(CNF_PVAL(ARRAY1)))
      IF (ISNEW) CALL CNF_UNREGP(TPTR)
C
C     Pick reasonable values for the fourier domain filter - this
C     section could be refined, but these will do.  Old values
C     gave lower confidences, so some short-wavelength power has been
C     restored.
C
      ZPC=CBPC
      KZ(1)=5
      KZ(2)=MIN(20,NX-2)

      KZ(3)=MAX(NX/4-KZ(2),KZ(2)+1)
      KZ(4)=MAX(NX/4-KZ(1),KZ(3)+1)

C Old values were:
C
C      KZ(3)=MAX(NX/6,KZ(2)+1)
C      KZ(4)=MIN(2*KZ(3),NX)
C
C     Perform the cross-correlation
C
      NORM=.TRUE.
      CALL FIG_CROSS(%VAL(CNF_PVAL(ARRAY0)),%VAL(CNF_PVAL(ARRAY1)),
     :               NX0,NX,CFIT,ZPC,KZ,NORM,%VAL(CNF_PVAL(FT0)),
     :               %VAL(CNF_PVAL(FT1)),%VAL(CNF_PVAL(FTFCN)),
     :               %VAL(CNF_PVAL(XV)),%VAL(CNF_PVAL(CFN)),
     :               SHIFT,WIDTH)
C
C     Calculate the cross correlation parameters
C
      CALL CCFPAR(%VAL(CNF_PVAL(CFN)),%VAL(CNF_PVAL(FT1)),SHIFT,NX,
     :            CONF, %VAL(CNF_PVAL(FT0)),%VAL(CNF_PVAL(FTACFN)),
     :            DELTA,%VAL(CNF_PVAL(FTFCN)),KZ,FSTAT,DELTA_OK)
      IF (FSTAT.NE.0) GO TO 500
C
C Warning if spectra so badly matched that TD error estimate (from FIG_CROSS) has to
C be used:
C
      IF(.NOT.DELTA_OK)THEN
         DELTA=WIDTH
         CALL PAR_WRUSER(
     >          ' WARNING: Template and spectrum badly matched.',STATUS)
         WARN=1.
      ELSE
         WARN=0.
      END IF

C
C     Calculate the redshift and errors etc
C
      CALL CALCRS(REDSHIFT,SHIFT,BASERED,BASERR,LOGSTEP,DELTA,REDERR)

C
C If VELOCITIES keyword is set TRUE, outputs are changed to
C velocities in km/s:
C
      VELOCITY = REDSHIFT*C
      VELERR   = REDERR*C

      IF(VELOCITIES)THEN

C New output format:
C
         STRING='Shift='
         CALL ICH_ENCODE(STRING,SHIFT,7,5,NEXT)
         STRING(NEXT:)=' pixels. Velocity='
         CALL ICH_ENCODE(STRING,VELOCITY,NEXT+19,5,NEXT)
         STRING(NEXT:)=' +/-'
         CALL ICH_ENCODE(STRING,VELERR,NEXT+5,4,NEXT)
         STRING(NEXT:)=' km/s. Confidence='
         CALL ICH_ENCODE(STRING,CONF,NEXT+19,4,NEXT)
         CALL PAR_WRUSER(STRING,STATUS)

      ELSE

C
C New output format:
C
         STRING='Shift='
         CALL ICH_ENCODE(STRING,SHIFT,7,5,NEXT)
         STRING(NEXT:)=' pixels. Redshift='
         CALL ICH_ENCODE(STRING,REDSHIFT,NEXT+19,5,NEXT)
         STRING(NEXT:)=' +/-'
         CALL ICH_ENCODE(STRING,REDERR,NEXT+5,4,NEXT)
         STRING(NEXT:)='. Confidence='
         CALL ICH_ENCODE(STRING,CONF,NEXT+14,4,NEXT)
         CALL PAR_WRUSER(STRING,STATUS)
      END IF

C
C     Set the user variables SHIFT, REDSHIFT, REDERR, VELOCITY, VELERR, CONF
C     and WARN:
C
      CALL VAR_SETNUM('SHIFT',0,0,SHIFT,IGNORE)
      CALL VAR_SETNUM('REDSHIFT',0,0,REDSHIFT,IGNORE)
      CALL VAR_SETNUM('REDERR',0,0,REDERR,IGNORE)
      CALL VAR_SETNUM('VELOCITY',0,0,VELOCITY,IGNORE)
      CALL VAR_SETNUM('VELERR',0,0,VELERR,IGNORE)
      CALL VAR_SETNUM('CONF',0,0,CONF,IGNORE)
      CALL VAR_SETNUM('WARN',0,0,WARN,IGNORE)
C
C     Now, if required, create the output structure for the
C     cross-correlation.
C
      IF (RECORD) THEN
         CALL DSA_RESHAPE_DATA('CORRL','SPECT',1,NX,STATUS)
         CALL DSA_MAP_DATA('CORRL','WRITE','FLOAT',CPTR,SLOT,STATUS)
         IF(STATUS.NE.0)GOTO 500

         BYTES=NX*BFLOAT
         CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(CFN)),%VAL(CNF_PVAL(CPTR)))
      END IF

  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)

      END

C+
      SUBROUTINE GET_LOGSTEP(XBEGIN,XFINISH,IXBEG,IXFIN,LOGSTEP)
C
C     Routine to calculate the value of LOGSTEP in the transformation
C     Nbin = ln(LAMBDA)/LOGSHIFT + B
C     NB LOGSTEP = ln(LAMBDA2/LAMBDA1)/(Nbin1-Nbin2)
C
C     Parameters - (">" input, "<" output)
C     (>)  IXBEG   (Integer) Pixel number associated with XSTART
C     (>)  IXFIN   (Integer) Pixel number associated with XFINISH
C     (>)  XBEGIN  (Real) First axis data value used
C     (>)  XFINISH (Real) Last axis data value used
C     (<)  LOGSTEP (Real) Constant in binning mapping
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IXBEG, IXFIN
      REAL XBEGIN, XFINISH, LOGSTEP
C
C     Local variables
C
C      INTEGER STATUS
      REAL V
C      CHARACTER*80 STRING
C
C     And now the very complicated equation.
C
      LOGSTEP = (LOG(XFINISH)-LOG(XBEGIN))/
     >       REAL(IXFIN-IXBEG)
C
C     Output equivalent velocity step is cross-checked with scrunch
C
      V = (EXP(LOGSTEP)-1.0)*299792.458
C
C      WRITE (STRING,'(A,F12.3,A)') 'Vel./bin = ',V
C     &            ,' km/sec - (delta log wavelength in vel. units)'
C      CALL PAR_WRUSER(STRING,STATUS)
C
      END

C+
      SUBROUTINE CALCRS(REDSHIFT,SHIFT,BASERED,BASERR,LOGSTEP,DELTA,
     >                                REDERR)
C
C     This is a subroutine to compute the red shift directly from the shift in
C     pixels.
C
C     Parameters -  (">" input, "<" output)
C
C     (<) REDSHIFT  (Real) The redshift of the spectrum
C                   taking into account the redshift of the template
C     (>) SHIFT     (Real) The shift of the template in terms of pixels
C     (>) BASERED   (Real) The redshift of the template spectrum
C     (>) BASERR    (Real) The error in the red shift of the template spectrum
C     (>) LOGSTEP   (Real) The logarithmic scaling factor
C     (>) DELTA     (Real) The error in SHIFT in pixels
C     (<) REDERR    (Real) the error in REDSHIFT
C+
      IMPLICIT NONE
C
C     Parameters
C
      REAL SHIFT,BASERED,BASERR,REDSHIFT,LOGSTEP,DELTA,REDERR
      REAL REDERR1, REDERR2
C
C     Now calculate the redshift directly
C
      REDSHIFT = EXP(LOGSTEP*SHIFT)-1.0
      REDERR1  = ABS(EXP(LOGSTEP*(SHIFT+DELTA))-1.0-REDSHIFT)
      REDERR2  = ABS(EXP(LOGSTEP*(SHIFT-DELTA))-1.0-REDSHIFT)
      REDERR   = MAX(REDERR1,REDERR2)
C
C     Correct for redshift of template
C
      REDSHIFT =  REDSHIFT + BASERED + REDSHIFT*BASERED
      REDERR   =  (1.0+BASERED**2)*REDERR**2 +
     :            (1.0+REDSHIFT**2)*BASERR**2
      REDERR   =  SQRT(REDERR)
C
      END
C-------------------------------------------------------------------------
C+
      SUBROUTINE CCFPAR(CFN,FT1,SHIFT,NX,CONF,FT0,FTACFN,
     >                         DELTA,FTCFN,KZ,FSTAT,DELTA_OK)
C
C     CCFPAR
C
C     Routine which will work out the error in the position of the peak of the
C     cross correlation of two spectra and the confidence level associated with
C     that peak.
C
C     Parameters - (">" input, "<" output, "W" workspace)
C
C     (>) CFN       (Real array CFN(NX)) The correlation function.
C     (>) FT1       (Complex array FT1(NX)) The fourier transform of the
C                   template.
C     (>) SHIFT     (Real) The shift of the peak of the correlation function
C                   from the zero point.
C     (>) NX        (Integer) Either equal to the number of pixels in spectrum
C                   or to the next highest number which is an integer power of 2.
C                   Note that most of the work and input arrays must be NX long.
C     (<) CONF      (Real) The confidence in the redshift of the spectrum.
C     (>) FT0       (Complex array FT0(NX)) The fourier transform of the spectrum.
C     (W) FTACFN    (Complex array FTACFN(NX)) The fourier transform of the
C                   antisymetric part of the correlation function.
C     (<) DELTA     (Real) The error in SHIFT in pixels.  Set to zero if spectra
C                   are so badly matched that error estimate is nonsense.
C     (<) DELTA_OK  (Logical) TRUE if Fourier method has calculated error.
C                   FALSE if delta has been set to zero.  If this fails, the
C                   template and spectrum are so different as to make the
C                   CCF method useless.  An error can still be assigned, and is
C                   in the routine FIG_CROSS.  The calling routine uses this value
C                   and reports a warning message.
C     (>) FSTAT     (Integer) Running status for calculation of SUMMAT
C                   returned as non-zero if a problem of division by zero occurs.
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,KZ(4),FSTAT
      COMPLEX FT1(NX),FTACFN(NX),FT0(NX),FTCFN(NX)
      REAL SHIFT,CONF,DELTA,CFN(NX)
      LOGICAL DELTA_OK
C
C     Local variables
C
      REAL SIGMA0_SQUARED_A,SIGMA1_SQUARED_A,SIGMA2_SQUARED_A
      REAL SIGMA0_SQUARED_T,SIGMA1_SQUARED_T,SUMMAT,DELTASQ
      REAL R,GAMMA,HEIGHT,REAL_HEIGHT,ERF,ERFC,K,SIGMA0_SQUARED_S
      INTEGER IFAIL,I,STATUS
C
C     Functions used
C
      DOUBLE PRECISION PDA_DERF
C
C     Recreate the cross correlation function.  Note that FTCFN is
C     FT0* x FT1, not vice-versa.
C
      CALL JTY_CORRELATE(NX,FT1,FT0,FTCFN)
C
C     Filter the cross correlation function and the template and spectrum
C     NB the filter function for the two spectra is the square root of the
C     filter function for the correlation function.
C
      CALL JTY_FILTER (NX,KZ(1),KZ(2),KZ(3),KZ(4),FTCFN)
      CALL JTY_FILTERSQRT (NX,KZ(1),KZ(2),KZ(3),KZ(4),FT0)
      CALL JTY_FILTERSQRT (NX,KZ(1),KZ(2),KZ(3),KZ(4),FT1)
C
C     Calculate the moments of the power spectrum of the spectrum
C     and the template needed
C
      CALL CALC_JTH_MOM(FT0,NX,0,SIGMA0_SQUARED_S)
      CALL CALC_JTH_MOM(FT1,NX,0,SIGMA0_SQUARED_T)
      CALL CALC_JTH_MOM(FT1,NX,1,SIGMA1_SQUARED_T)
C
C     Scale the fourier transform of the correlation function properly
C
      DO I = 1,NX
         FTCFN(I) = FTCFN(I) /
     >        (SQRT(SIGMA0_SQUARED_S*SIGMA0_SQUARED_T)*REAL(NX))
      END DO
C
C     Calculate the height of the peak in the correlation function
C
      REAL_HEIGHT = CFN(1)
      DO I =2,NX
         IF (CFN(I).GT.REAL_HEIGHT) THEN
            REAL_HEIGHT = CFN(I)
         END IF
      END DO
C
C     Calculate the transform of the antisymmetric part of the cross
C     correlation function
C
      DO I = 1,NX/2
         K = 2.0*3.14159265*(REAL(I)-1.0)/REAL(NX)
         FTACFN(I) = CMPLX(0.0,AIMAG(FTCFN(I)*EXP(CMPLX(0.0,K*SHIFT))))
      END DO
C
C     Calculate the moments of the power spectrum of the antisymmetric
C     part of the cross correlation needed.
C
      CALL CALC_JTH_MOM(FTACFN,NX,0,SIGMA0_SQUARED_A)
      CALL CALC_JTH_MOM(FTACFN,NX,1,SIGMA1_SQUARED_A)
      CALL CALC_JTH_MOM(FTACFN,NX,2,SIGMA2_SQUARED_A)
C
C     Calculate the sum of (power in antisymmetric correlation function)/
C     (power in template). The antisymmetric power should only be non-
C     zero if the template power is non-zero.
C
      SUMMAT = 0.0
C
      DO I = 1,NX/2-1
         IF(ABS(FT1(I)).EQ.0.0 .AND. ABS(FTACFN(I)).GT.0.0)THEN
            FSTAT = 1
            CALL PAR_WRUSER
     :        (' Division by zero calculating error.',STATUS)
            GOTO 500
         ELSE IF(ABS(FTACFN(I)).GT.0.0 .AND. ABS(FT1(I)).GT.0.0 ) THEN
            SUMMAT = SUMMAT + ( ABS(FTACFN(I)) / ABS(FT1(I)) )**2
         END IF
      END DO
C
      SUMMAT = SUMMAT*2.0
C
C     Calculate DELTA the error in the position of the peak in pixels
C
      DELTASQ = (2.0*SIGMA1_SQUARED_A*SIGMA0_SQUARED_T**2) /
     >       ((1.0-2.0*SIGMA0_SQUARED_T*SUMMAT)*SIGMA1_SQUARED_T**2)

      IF(DELTASQ.GE.0.0)THEN
         DELTA = SQRT(DELTASQ)
         DELTA_OK = .TRUE.
      ELSE
         DELTA = 0.0
         DELTA_OK = .FALSE.
      END IF
C
C     Calculate the confidence in the value of shift. Height is the
C     height of the cross correlation peak in units of the noise r.m.s.
C     To avoid crashes when a spectrum is autocorrelated (when there is
C     no antisymmetric part of the ccf), put in a test:

      IF(SIGMA0_SQUARED_A.GT.0.0)THEN
         R      = SQRT(SIGMA1_SQUARED_A/SIGMA2_SQUARED_A)
         GAMMA  = SIGMA1_SQUARED_A/
     :            SQRT(SIGMA0_SQUARED_A*SIGMA2_SQUARED_A)
         HEIGHT = REAL_HEIGHT/SQRT(2.0*SIGMA0_SQUARED_A)
C
C     Calculate the ERF and ERFC functions using PDA functions
C
         IFAIL = 0
         ERF   = REAL(PDA_DERF(
     :      DBLE(GAMMA*HEIGHT/SQRT(2.0*(1.0-GAMMA**2))),IFAIL) )
         IFAIL = 0
         ERFC  =  1.0 - REAL(PDA_DERF(
     :      DBLE(HEIGHT/SQRT(2.0*(1.0-GAMMA**2))),IFAIL) )
C
         CONF  = EXP(-REAL(NX)*0.5*(ERFC+GAMMA*EXP(-HEIGHT**2/2.0)
     :        *(1.0+ERF))/(4.0*3.14159265*R))
C
      ELSE
         R = 0.0
         GAMMA = 0.0
         HEIGHT = 0.0
         CONF = 1.0
      END IF

 500  CONTINUE
C
      END

C+
      SUBROUTINE CALC_JTH_MOM(ARRAY,NX,J,SIGMAJ_SQUARED)
C
C     CALC_JTH_MOM
C
C     This subroutine calculates the 2Jth moment of an array where the
C     2Jth moment is the sum over I from 1 to NX of
C     ARRAY(I)*(2*pi*(I-1)/NX)**(2*J).
C     It is designed for calculating power in one dimensional fourier
C     transforms of real data so a normality factor of 1/(NX**2)
C     is included. The power as in I > NX/2 is the same as in I < NX/2
C     so only half the items are summed and the result doubled.
C
C     Parameters (">" input, "<" output)
C
C     (>) ARRAY            (Complex array ARRAY(NX)) The array whose
C                          2Jth moment is to be calculated
C     (>) NX               (Integer) The dimension of ARRAY
C     (>) J                (Integer) J
C     (<) SIGMAJ_SQUARED   (Real) The 2Jth moment
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,J
      REAL SIGMAJ_SQUARED
      COMPLEX ARRAY(NX)
C
C     Local variables
C
      INTEGER I
      REAL TWOPI
C
C     Define TWOPI
C
      PARAMETER (TWOPI = 2.0*3.14159265)
C
C     Calculate the Jth moment
C
      SIGMAJ_SQUARED = 0.0
C
      IF( J.EQ.0 )THEN
         DO I =1,NX/2
            SIGMAJ_SQUARED = SIGMAJ_SQUARED +
     :             ABS(ARRAY(I))**2
         END DO
      ELSE
         DO I = 1,NX/2
            SIGMAJ_SQUARED = SIGMAJ_SQUARED +
     :             (ABS(ARRAY(I))*(REAL(I-1))**REAL(J))**2
         END DO
      END IF

      SIGMAJ_SQUARED = (2.0*SIGMAJ_SQUARED/REAL(NX)**2) *
     :                (TWOPI/REAL(NX))**REAL(2*J)
C
      END


      SUBROUTINE JTY_FILTERSQRT(N,K1,K2,K3,K4,X)
* Filter a Fourier tranform by multiplying by a cosine bell.NB
* The filter used here is the square root of the filter used
* in the figaro subroutine JTY_FILTER

C Parameters

      IMPLICIT NONE
      INTEGER      K1,K2,K3,K4,N
      COMPLEX*8    X(1)

C Local variables

      INTEGER      J,NUMBER,NUMA
      REAL         ARG,FACTOR

      DO J = 1,N
          IF(J.LT.N/2+1) THEN
              NUMBER = J - 1
          ELSE
              NUMBER = J - N - 1
          END IF
          NUMA = ABS(NUMBER)
          IF(NUMA.LT.K1.OR.NUMA.GT.K4) THEN
              FACTOR = 0
          ELSE IF(NUMA.LT.K2) THEN
              ARG = 3.14159 * FLOAT(NUMA-K1)/FLOAT(K2-K1)
              FACTOR = SQRT(.5 * (1-COS(ARG)))
          ELSE IF(NUMA.GT.K3) THEN
              ARG = 3.14159 * FLOAT(NUMA-K3)/FLOAT(K4-K3)
              FACTOR = SQRT(.5 * (1+COS(ARG)))
          ELSE
              FACTOR = 1
          END IF
          X(J) = X(J) * FACTOR
      END DO
      RETURN
      END
