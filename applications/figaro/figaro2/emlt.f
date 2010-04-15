C+
      SUBROUTINE EMLT
C
C     E M L T
C
C     Figaro version of the original SDRSYS routine EMLT, which analyses
C     emission lines in a spectrum, fitting gaussians to the strongest
C     lines and logging their positions, widths and centers.
C     Optionally, it will also give line centers using a centre of
C     moment analysis, and can also produce a synthetic spectrum
C     generated from the positions and widths of the located lines.
C     Note: Figaro and SDRSYS differ in their pixel numbering, Figaro
C     counting from 1 and SDRSYS counting from 0, so there will be a
C     discrepancy of 1 between the output from the two versions for any
C     pixel-number values; wavelength values produced by the two should
C     be the same.
C
C     Parameters -
C
C     SPECTRUM    (Character) The name of the spectrum to be analysed.
C     XSTART      (Numeric) The first X-value to be used for the
C                 analysis.
C     XEND        (Numeric) The last X-value to be used for the
C                 analysis.
C     LINES       (Numeric) If LINES is zero, all lines that can be
C                 fitted are listed.  Otherwise, it gives the number of
C                 lines to be included in the analysis, starting with
C                 the strongest and cutting off the weaker lines.
C     FWHM        (Numeric) If non-zero, all lines fitted are
C                 constrained to a full width at half maximum of this
C                 value - in pixels.
C     OUTPUT      (Character) The name of any synthetic spectrum to be
C                 generated.
C
C     Keywords -
C
C     MOMENTS     If specified, a center of moment analysis is also
C                 performed on all lines found.
C     SYNTH       If specified, a synthetic spectrum is generated.
C
C     User variables -  (">" input, "<" output)
C
C     (<) EMLT_LINES    (Real) Number of lines found.
C     (<) EMLT_BIN      (Real array) List of line centres (pixels).
C     (<) EMLT_POS      (Real array) List of line centres (wavelength
C                                    units).
C     (<) EMLT_FWHM_BIN (Real array) List of FWHM (pixels).
C     (<) EMLT_FWHM_ANG (Real array) List of FWHM (wavelength units).
C     (<) EMLT_STREN    (Real array) List of line strengths.
C     (<) EMLT_PEAK     (Real array) List of peak heights.
C
C                                               KS / AAO  4th March 1988
C     Modified:
C
C     16 Aug 1988  Some output formatting problems corrected.  KS/AAO.
C     25 Jan 1989  Bug in dispersion calculation when only a subset of
C                  the data used now corrected.  KS/AAO.
C     24 Sep 1992  HME / UoE, Starlink.  Lowercase file names.
C                  INCLUDE changed.
C     10 Feb 1993  Bugs fixed in FIG_EMLT. See FIG_EMLT for details. KS/AAO.
C     15 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  No concurrent mapping. Had to swap _AXIS_RANGE in
C                  front of mapping axis data.
C     18 Jul 1996  MJCL / Starlink, UCL.  Set variables for storage of
C                  file names to 132 chars.
C     10 May 1999  TCDA / Starlink, RAL. Line values now read out to
C                  ADAM parameters.
C     26 Oct 2001  ACD / UoE, Starlink. All the lines values written out
C                  into ADAM parameter arrays rather than just writing the
C                  values for a single line to scalar ADAM parameters.
C                  Also write a parameter giving the number of lines.
C     06 Nov 2001  ACD / UoE, Starlink. Documented output parameters in
C                  the prologue comments.
C     24 Apr 2002  ACD / UoE, Starlink. Ensured that the output ADAM
C                  parameters have the correct values when no lines are
C                  found.
C     2005 June 10 MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions used
C
      LOGICAL GEN_CHKNSF
      INTEGER ICH_LEN, DSA_TYPESIZE
C
C     Local variables
C
      INTEGER   AXDIMS(6)        ! Axis array dimensions
      INTEGER   BINPTR           ! Dynamic-memory pointer for workspace
                                 ! array
      INTEGER   BNAPTR           ! Dynamic-memory pointer for workspace
      INTEGER   BYTES            ! Number of bytes in one cross-section
      LOGICAL   CALIB            ! Axis is calibrated (not just 1..NX)?
      INTEGER   DIMS(6)          ! Data array dimensions
      INTEGER   DPTR             ! Dynamic-memory pointer for spectral
                                 ! data
      DOUBLE PRECISION DUMMY     ! Dummy numeric argument
      INTEGER   FAPTR            ! Dynamic-memory pointer for workspace
      REAL      FWHM             ! Value of FWHM parameter
      INTEGER   FWHPTR           ! Dynamic-memory pointer for workspace
      INTEGER   IGNORE           ! Returned status value - ignored
      INTEGER   IPTR             ! Dynamic-memory pointer for input data
      LOGICAL   ISNEW            ! Is address new to CNF?
      LOGICAL   ISNEWX           ! Is XPTR address new to CNF?
      INTEGER   ISPEC            ! Loop index through data
                                 ! cross-sections
      INTEGER   IXEN             ! Last spectral element to be used
      INTEGER   IXSPEC           ! Counts through axis cross-sections
      INTEGER   IXST             ! First spectral element to be used
      INTEGER   LINES            ! Value of LINES parameter
      INTEGER   LOGLU            ! Logical unit number for output file
      LOGICAL   MOMENTS          ! Value of MOMENTS keyword
      CHARACTER NAME*132         ! Full name of results file
      INTEGER   NASPEC           ! Number of cross-sections in axis data
      INTEGER   NAXDIM           ! Number of axis array dimensions
      INTEGER   NAXELM           ! Number of axis array elements
      INTEGER   NDIM             ! Number of data array dimensions
      INTEGER   NELM             ! Number of data array elements
      INTEGER   NOCENS           ! Moments keyword, numeric value for
                                 ! FIG_EMLT
      INTEGER   NSPECT           ! Number of spectra in data
      INTEGER   NX               ! First dimension of data array
      INTEGER   OPTR             ! Dynamic-memory pointer for output
                                 ! data
      INTEGER   PKPTR            ! Dynamic-memory pointer for workspace
      LOGICAL   PISNEW           ! Previous CNF pointer new?
      LOGICAL   PISNX            ! Previous CNF pointer new?
      INTEGER   SLOT             ! Used for DSA_ slot values
      INTEGER   STAPTR           ! Dynamic-memory pointer for workspace
      INTEGER   STATUS           ! Running status for DSA_ routines
      CHARACTER STRING*20        ! Used for formatting spectrum numbers
      INTEGER   STRPTR           ! Dynamic-memory pointer for workspace
                                 ! array
      LOGICAL   SYNTH            ! Value of SYNTH keyword
      INTEGER   TPTR             ! Temp dynamic-memory pointer
      CHARACTER UNITS*10         ! Axis units
      REAL      VALUE            ! Real value of numeric parameter
      REAL      WEND             ! Last axis value - ignored
      INTEGER   WIDPTR           ! Dynamic-memory pointer for workspace
                                 ! array
      INTEGER   WPTR             ! Dynamic-memory pointer for workspace
                                 ! array
      REAL      WSTART           ! First axis value - ignored
      INTEGER   XBASE            ! Dynamic-memory pointer for start of
                                 ! axis data
      LOGICAL   XEXIST           ! True if there is an axis data array
      INTEGER   XPTR             ! Dynamic-memory pointer for axis data
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get name of input file and open it
C
      CALL DSA_INPUT ('SPECT','SPECTRUM',STATUS)
C
C     Get data dimensions
C
      CALL DSA_DATA_SIZE ('SPECT',6,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GO TO 500
      NX = DIMS(1)
      NSPECT = NELM/NX
C
C     Get range of data to be examined.
C
      CALL DSA_AXIS_RANGE ('SPECT',1,' ',.FALSE.,WSTART,WEND,
     :                     IXST,IXEN,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     See if there is any axis data, and if so, get its size
C     and its units, and map it.
C
      CALL DSA_SEEK_AXIS ('SPECT',1,XEXIST,STATUS)
      IF (XEXIST) THEN
         CALL DSA_AXIS_SIZE ('SPECT',1,6,NAXDIM,AXDIMS,NAXELM,STATUS)
         IF (STATUS.NE.0) GO TO 500
         NASPEC = NAXELM / AXDIMS(1)
         CALL DSA_GET_AXIS_INFO ('SPECT',1,1,UNITS,0,DUMMY,STATUS)
         CALL DSA_MAP_AXIS_DATA ('SPECT',1,'READ','FLOAT',XPTR,
     :                           SLOT,STATUS)
      END IF
      IF (STATUS.NE.0) GO TO 500
C
C     Get the rest of the fitting parameters - MOMENTS, LINES, FWHM
C
      CALL PAR_RDVAL ('LINES',0.0,FLOAT(NX/5 + 1),0.0,' ',VALUE)
      LINES = INT(VALUE)
      CALL PAR_RDVAL ('FWHM',0.0,50.0,0.0,'Pixels',FWHM)
      CALL PAR_RDKEY ('MOMENTS',.FALSE.,MOMENTS)
      IF (MOMENTS) THEN
         NOCENS = 0
      ELSE
         NOCENS = 1
      END IF
C
C     See if a synthetic spectrum is required, and if so open it
C     and map its data.
C
      CALL PAR_RDKEY ('SYNTH',.FALSE.,SYNTH)
      IF (SYNTH) THEN
         CALL DSA_OUTPUT ('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
         CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',OPTR,SLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500
      END IF
C
C     FIG_EMLT needs a certain amount of workspace.  In total eight real
C     arrays, all (NX/5 + 1) elements long are needed.
C
      CALL DSA_GET_WORK_ARRAY (NX/5+1,'FLOAT',STRPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY (NX/5+1,'FLOAT',WIDPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY (NX/5+1,'FLOAT',BINPTR,SLOT,STATUS)
C
C     Get additional workspace for the arrays to hold the values to be
C     written as ADAM parameter arrays.  This workspace is also passed
C     to FIG_EMLT.
C
      CALL DSA_GET_WORK_ARRAY (NX/5+1,'FLOAT',BNAPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY (NX/5+1,'FLOAT',FAPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY (NX/5+1,'FLOAT',FWHPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY (NX/5+1,'FLOAT',STAPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY (NX/5+1,'FLOAT',PKPTR,SLOT,STATUS)
C
C     We have to be a little bit careful about the data array we pass
C     to FIG_EMLT, since it modifies it in the course of its analysis.
C     If we are creating a synthetic spectrum, this is fine, since we
C     can just pass it the original contents of the output array (which
C     match the input), let it clobber it, and then let it write the
C     synthetic spectrum into it.  If we just have an input spectrum
C     to work with, however, we need to get workspace that we can copy
C     the data into before letting FIG_EMLT loose on it.  We also
C     need to map the input spectrum.
C
      IF (.NOT.SYNTH) THEN
         CALL DSA_MAP_DATA ('SPECT','READ','FLOAT',IPTR,SLOT,STATUS)
         CALL DSA_GET_WORK_ARRAY (NX,'FLOAT',WPTR,SLOT,STATUS)
      END IF
C
C     Open an output file for the results
C
      CALL DSA_OPEN_TEXT_FILE ('emlt.lis',' ','NEW',.TRUE.,LOGLU,
     :                         NAME,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     See if we do have calibrated X-axis data
C
      CALIB = .FALSE.
      IF (XEXIST) CALIB = .NOT. GEN_CHKNSF (%VAL(CNF_PVAL(XPTR)),NX)
C
C     Now we loop through all the spectra in the DATA.  If we are
C     producing a synthetic spectrum, we can just pass FIG_EMLT the
C     output array.  Otherwise, we copy the data into the work array
C     first.  If the data is wavelength calibrated, we have to run
C     through the wavelength array as well as the data array.
C
      CALL PAR_WRUSER(' ',STATUS)
      IXSPEC = 1
      PISNEW = .FALSE.
      PISNX = .FALSE.
      XBASE = XPTR
      BYTES = NX * DSA_TYPESIZE ('FLOAT',STATUS)
      DO ISPEC=1,NSPECT
         IF (NSPECT.GT.1) THEN
            WRITE (STRING,*,IOSTAT=IGNORE) 'Spectrum ',ISPEC
            CALL PAR_WRUSER(STRING,IGNORE)
            WRITE (LOGLU,'(A)',IOSTAT=IGNORE) STRING
         END IF
         IF (SYNTH) THEN
            DPTR = OPTR

            CALL DYN_INCAD(OPTR,'FLOAT',NX,TPTR,ISNEW,STATUS)
            IF (PISNEW) CALL CNF_UNREGP(OPTR)
            OPTR=TPTR
            PISNEW = ISNEW
         ELSE
            CALL GEN_MOVE (BYTES,%VAL(CNF_PVAL(IPTR)),
     :                     %VAL(CNF_PVAL(WPTR)))

            CALL DYN_INCAD(IPTR,'FLOAT',NX,TPTR,ISNEW,STATUS)
            IF (PISNEW) CALL CNF_UNREGP(IPTR)
            IPTR=TPTR
            PISNEW = ISNEW

            DPTR = WPTR
         END IF
         CALL FIG_EMLT (%VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(DPTR)),NX,
     :                  IXST,IXEN,FWHM,NOCENS,LINES,
     :                  %VAL(CNF_PVAL(STRPTR)),%VAL(CNF_PVAL(WIDPTR)),
     :                  %VAL(CNF_PVAL(BINPTR)),%VAL(CNF_PVAL(BNAPTR)),
     :                  %VAL(CNF_PVAL(FAPTR)),%VAL(CNF_PVAL(FWHPTR)),
     :                  %VAL(CNF_PVAL(STAPTR)),%VAL(CNF_PVAL(PKPTR)),
     :                  CALIB,SYNTH,UNITS,LOGLU)
         IF (CALIB) THEN
            IXSPEC = IXSPEC + 1
            IF (IXSPEC.GT.NASPEC) THEN
               IXSPEC = 1
               XPTR = XBASE
            ELSE
               CALL DYN_INCAD(XPTR,'FLOAT',NX,TPTR,ISNEWX,STATUS)
               IF (PISNX) CALL CNF_UNREGP(XPTR)
               XPTR=TPTR
               PISNX = ISNEWX
            END IF
         END IF
      END DO
      IF (ISNEW) THEN
         IF (SYNTH) CALL CNF_UNREGP(OPTR)
      ELSE
         CALL CNF_UNREGP(IPTR)
      END IF
      IF (ISNEWX) CALL CNF_UNREGP(XPTR)
C
C     Let the user know the file being used.
C
      CALL PAR_WRUSER(' ',IGNORE)
      CALL PAR_WRUSER('Results written to '//NAME(:ICH_LEN(NAME)),
     :                                                      STATUS)
C
C     Tidy up
C
  500 CONTINUE
      CALL DSA_CLOSE (STATUS)
C
      END
C+
      SUBROUTINE FIG_EMLT (X,Y,NX,N1,N2,FWHM0,NOCENS,LINES,
     :                     STR,WID,BIN,
     :                     BINA,FAA,FWHMAA,STRENA,PEAKA,
     :                     CALIB,OUTSET,UNITS,LOGLU)
C
C     F I G _ E M L T
C
C     Analyses emission lines in a spectrum, producing a list of
C     their centers and widths, optionally from a center of moments
C     analysis, usually from a gaussian fit.  This is essentially
C     the bulk of the code from the SDRSYS routine EMLT, hacked
C     about dreadfully to turn it into the main routine of a Figaro
C     application.  The body of the code is unchanged, but all the
C     output formatting has been recoded (probably to its detriment)
C     and the routine now no longer assumes that the input specturm is
C     2048 elements.  It has also been modified to use a wavelengths
C     array rather than the start wavelength and dispersion used by
C     SDRSYS.  Note also that Figaro numbers channels from 1..NX,
C     while SDRSYS numbered channels from 0..NX-1, so the channel
C     numbers (but not the axis values) produced by this routine will
C     all be one greater than those produced by the original SDRSYS EMLT.
C
C     Parameters -   (">" input, "<" output, "!" modified, "W" workspace)
C
C     (>) X       (Real array) The axis values for the data.
C     (!) Y       (Real array) The data values.  This is modified by
C                 the program as a normal part of its processing,
C                 and should be regarded as destroyed.  If OUTSET is
C                 specified as true, Y is returned containing a synthetic
C                 spectrum generated from the fitted lines.
C     (>) NX      (Integer) The number of elements in the data.
C     (>) N1      (Integer) The number of the first element to be used
C                 in the analysis.
C     (>) N2      (Integer) The number of the last element to be used
C                 in the analysis.
C     (>) FWHM0   (Real) If non-zero, all lines fitted are constrained
C                 to this half-width (in pixels).
C     (>) NOCENS  (Integer) If set to 1, the center of moments analysis
C                 is bypassed.
C     (>) LINES   (Integer) If non-zero, only the LINES strongest lines
C                 in the spectrum will be analysed.
C     (W) STR     (Real array) Used to accumulate the line strengths.
C     (W) WID     (Real array) Used to accumulate the line widths.
C     (W) BIN     (Real array) Used to accumulate the line centers.
C     (W) BINA    (Real array) Used to accumulate values for ADAM parameters.
C     (W) FAA     (Real array) Used to accumulate values for ADAM parameters.
C     (W) FWHMAA  (Real array) Used to accumulate values for ADAM parameters.
C     (W) STRENA  (Real array) Used to accumulate values for ADAM parameters.
C     (W) PEAKA   (Real array) Used to accumulate values for ADAM parameters.
C                 STR, WID, BIN BINA, FAA, FWHMAA, STRENA and PEAKA should
C                 all be of at least NX/5 +1 elements
C     (>) CALIB   (Logical) Indicates if the X values are to be used.
C     (>) OUTSET  (Logical) If true, a synthetic spectum based on the
C                 line analysis will be generated in Y.
C     (>) UNITS   (Character) The axis units.  Ideally, UNITS is ten
C                 characters long and is centered.  It is only used to
C                 generate the headings in the formatted output.
C     (>) LOGLU   (Integer) If non-zero, a logical unit number on which
C                 the reported lines are logged.
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     FIG_GFIT    Fits a gaussian to an emission line.
C     FIG_XVALUE  Interpolates between values in the axis array.
C
C     History -
C
C     Sometime       Original SDRSYS version, John Straede.
C     26th Feb 1988  Original re-write for Figaro.  KS / AAO.
C     16th Aug 1988  Some formatting problems corrected.  KS/AAO.
C     25th Jan 1989  Bug in dispersion calculation when only a subset of
C                    the data used now corrected.  KS/AAO.
C     10th Feb 1993  Bug fixes. X array was being used even if CALIB was false,
C                    which we got away with on VMS but crashed under UNIX. Also
C                    changed so mean (peak-weighted) width calculated only for
C                    selected strongest lines. G format seems poor on SUNs, so
C                    no longer used. KS/AAO.
C     30th Jul 1996  Moved DATA statements.  MJCL/Starlink, UCL.
C     26th Oct 2001  All the lines values written out into ADAM parameter
C                    arrays rather than just writing the values for a single
C                    line to scalar ADAM parameters.  Also write a parameter
C                    giving the number of lines.  ACD / UoE, Starlink.
C     31st Oct 2001  Corrected arguments in call to VAR_SETARY.
C                    ACD / UoE, Starlink.
C     24th Apr 2002  Ensured that the output ADAM parameters have the correct
C                    values when no lines are found.  ACD / UoE, Starlink.
C+
      IMPLICIT NONE
C
C   PARAMETERS
      LOGICAL OUTSET,CALIB
      INTEGER NX,N1,N2,NOCENS,LINES,LOGLU
      REAL X(NX),Y(NX),STR(NX/5+1),WID(NX/5+1),BIN(NX/5+1),FWHM0
      REAL BINA(NX/5+1),FAA(NX/5+1),FWHMAA(NX/5+1),STRENA(NX/5+1)
      REAL PEAKA(NX/5+1)
      CHARACTER*(*) UNITS
C
C   FUNCTIONS
      REAL FIG_XVALUE
C
C   VARIABLES
      INTEGER MINSTR,N10,N11,STATUS,IGNORE,LINE,NHC,NL,NH,NLCP1X
      INTEGER N1002,N1004,NHCP1,NLCP1,N1004X,NHCP1X,N101,N102,NC
      INTEGER NLC,N142,N144L,N144,NDPA,N160,N180,N210,N230,NXL,NXH,N220
      INTEGER NMIN,NMAX
      REAL ARND,FA,STRMIN,DISPER,STRENS,FWHMS,YCL,YCH,SIGM,SIGMX
      REAL XCMOM,SIGMH,SIGML,XCENTR,S2NF,XC,FWHM,STREN,C0,PEAK,ALPHA
      REAL PEAKO2,XHW,XL,XH,XV,RL,RH,FWHMA,STRMAX
      CHARACTER LOG*64,FMTSTR*35
      REAL RLINE
C
C   MESSAGES
      CHARACTER M1*33,M2*33,M1A*5,MH1A*58,MH1B*58,MH2A*43,MH2B*43
C
C   ERROR MESSAGES
      CHARACTER EM2*16
C
      DATA MH1A
     :   /'    Bin                    Width      Integrated    Peak '/
      DATA MH1B
     :   /'  Number  Angstroms    Bins Angstroms  Strength    Height'/
      DATA MH2A/'Centre of      Centroid          Integrated'/
      DATA MH2B/'  moment                          Strength '/
      DATA M1  /'Mean fullwidth at half maximum = '/
      DATA M2  /'                               = '/
      DATA M1A /' bins'/
      DATA EM2 /'No Gaussian Fits'/
C
C   REMOVE SINGLE BIN FEATURES
C
      NMIN = N1 + 2
      NMAX = N2 - 4
C
      DO 10 N10 = 2,NX-1
C
         IF (Y(N10-1) .NE. 0.0)   GO TO 10
         IF (Y(N10+1) .NE. 0.0)   GO TO 10
C
            Y(N10) = 0.0
C
  10     CONTINUE
C
C   REMOVE DOUBLE BIN FEATURES
C
         DO 11 N11 = 2,NX-2
C
         IF (Y(N11-1) .NE. 0.0)   GO TO 11
         IF (Y(N11+2) .NE. 0.0)   GO TO 11
C
            Y(N11)   = 0.0
            Y(N11+1) = 0.0
C
  11     CONTINUE
C
C   FIND LINES
C
      IF (NOCENS .EQ. 1)   GO TO 99
      CALL PAR_WRUSER (' ',STATUS)
      CALL PAR_WRUSER (MH2A,STATUS)
      CALL PAR_WRUSER (MH2B,STATUS)
      CALL PAR_WRUSER (' ',STATUS)
      IF (LOGLU.NE.0) WRITE (LOGLU,'(/A)',IOSTAT=IGNORE) MH2A
      IF (LOGLU.NE.0) WRITE (LOGLU,'(A/)',IOSTAT=IGNORE) MH2B
  99  CONTINUE
      LINE = 0
      NHC = NMIN
      STRENS = 0.0
      FWHMS = 0.0
C
 100  CONTINUE
      NL = NHC
 1001 CONTINUE
      IF (NL .GT. NMAX)   GO TO 109
C                                   FINISHED
      NL = NL +1
      IF (Y(NL+1) .EQ. 0.0)   GO TO 1001
C                                        NEXT DATA POINT NULL - ADVANCE
      NH = NL +5
      FWHM = FWHM0
      CALL FIG_GFIT (XC,FWHM,STREN,NLC,NHC,C0,S2NF,Y,NX,NL,NH)
      IF (STREN .LE. 0.0)   GO TO 100
      IF (NOCENS .EQ. 1)   GO TO 104
C
C   FIND LIMITS OF LINE
      NLCP1 = NLC +1
      NHCP1 = NHC +1
      YCL = Y(NLCP1)
      YCH = Y(NHCP1)
C
      DO 1002 N1002 = NLCP1,NHCP1
C
         NLCP1X = N1002
         IF (Y(N1002+1) .GT. YCL)   GO TO 1003
 1002    CONTINUE
C
 1003 CONTINUE
C
      DO 1004 N1004 = NLCP1,NHCP1
C
         N1004X = NLCP1 +NHCP1 -N1004
         NHCP1X = N1004X
         IF (Y(N1004X-1) .GT. YCH)   GO TO 1005
 1004    CONTINUE
C
 1005 CONTINUE
      NLCP1 = NLCP1X
      NHCP1 = NHCP1X
      IF (NLCP1 .GE. NHCP1)   GO TO 104
C
C   CENTRE OF MOMENT
      SIGM  = 0.0
      SIGMX = 0.0
C
      DO 101 N101 = NLCP1,NHCP1
C
         SIGM = SIGM +Y(N101)
         SIGMX = SIGMX + FLOAT(N101 -NLCP1)*Y(N101)
  101    CONTINUE
C
      XCMOM = FLOAT(NLCP1) + SIGMX /SIGM
C
C   CENTROID
      SIGMH = SIGM /2.
C
      DO 102 N102 = NLCP1,NHCP1
C
         SIGML = SIGMH
         SIGMH = SIGML -Y(N102)
         NC = N102 -2
         IF (SIGMH .LT. 0.0)   GO TO 103
  102    CONTINUE
C
  103 CONTINUE
      XCENTR = FLOAT(NC) + SIGML /Y(NC+2) +1.5
C
C   STRENGTH
      WRITE (LOG,'(F8.1,7X,F8.1,7X,G12.4)',IOSTAT=IGNORE)
     :                                           XCMOM,XCENTR,STREN
      CALL PAR_WRUSER (LOG,STATUS)
      IF (LOGLU.NE.0) WRITE (LOGLU,'(A)',IOSTAT=IGNORE) LOG
  104 CONTINUE
      IF (S2NF .LE. 0.0)   GO TO 100
      LINE = LINE +1
      STR(LINE) = STREN
      WID(LINE) = FWHM
      BIN(LINE) = XC
      IF (FWHM0 .GT. 0.0)   GO TO 100
      IF (FWHM .LE. 0.0)   GO TO 100
C
C   ACCUMULATE FULLWIDTH-HALFMAXIMUM WEIGHTED BY PEAK HEIGHT
C   (This is where the original JOS code calculated these quantities. This
C   gives them for ALL the lines, not just those selected - since the latter
C   seems to be preferred, the calculation is repeated later just for the lines
C   selected - KS)
C
      FWHMS = FWHMS + STREN
      STRENS = STRENS + STREN /FWHM
      GO TO 100
C
 109  CONTINUE
C
      IF (LINE .GT. 0)   GO TO 141
C                                  GAUSSIAN FITTED LINES FOUND
C     NO GAUSSIAN FITTED LINES
         CALL PAR_WRUSER (EM2,STATUS)
         IF (LOGLU.NE.0) WRITE (LOGLU,'(A)',IOSTAT=IGNORE) EM2
C
C       Set the ADAM parameter holding the number of lines to zero and
C       write dummy values to the first elements of the array parameters
C       holding the details of each line.
C
         CALL VAR_SETNUM ('EMLT_LINES', 0, 0, 0.0E0, STATUS)
C
         BINA(1) = 0.0E0
         FAA(1) = 0.0E0
         WID(1) = 0.0E0
         FWHMAA(1) = 0.0E0
         STRENA(1) = 0.0E0
         PEAKA(1) = 0.0E0
C
         CALL VAR_SETARY ('EMLT_BIN', 1, BINA, STATUS)
         CALL VAR_SETARY ('EMLT_POS', 1, FAA, STATUS)
         CALL VAR_SETARY ('EMLT_FWHM_BIN', 1, WID, STATUS)
         CALL VAR_SETARY ('EMLT_FWHM_ANG', 1, FWHMAA, STATUS)
         CALL VAR_SETARY ('EMLT_STREN', 1, STRENA, STATUS)
         CALL VAR_SETARY ('EMLT_PEAK', 1, PEAKA, STATUS)
C
C       Go to exit.
C
         GO TO 300
C
C   ?? DO WE HAVE MORE THAN WE WANT ??
  141 CONTINUE
      IF (LINES .LE. 0)   GO TO 150
C                                   WANT ALL THERE IS
      IF (LINE .LE. LINES)   GO TO 150
C                                      WANTED ALL WE GOT
         IF (LINE .LE. 1)   GO TO 150
C                                     ONLY ONE LINE - NOTHING TO REJECT
C     FIND WEAKEST LINE
         STRMIN = STR(1)
         MINSTR = 1
C
         DO 142 N142 = 2,LINE
C
C
            IF (STR(N142) .GT. STRMIN)   GO TO 142
C                                                  STRONGER
C           NEW WEAKEST LINE
               STRMIN = STR(N142)
               MINSTR = N142
C
  142       CONTINUE
C
C     REJECT WEAKEST LINE
         N144L = MINSTR + 1
         IF (N144L .GT. LINE)   GO TO 145
C                                         WEAKEST LINE IS LAST
            DO 144 N144 = N144L,LINE
C
               STR(N144 - 1) = STR(N144)
               WID(N144 - 1) = WID(N144)
               BIN(N144 - 1) = BIN(N144)
  144          CONTINUE
C
C     DECREMENT LINE COUNT
 145     CONTINUE
         LINE = LINE - 1
         GO TO 141
C                  SEE IF THERE ARE STILL TOO MANY LINES
C
 150  CONTINUE
C
C   LOG RESULTS
C
C
      IF (CALIB) THEN
         DISPER = ABS(X(NX)-X(1))/FLOAT(NX-1)
      ELSE
         DISPER = 1.0
      END IF
      ARND = 0.05
      NDPA = 1
      IF (DISPER .GE. 2.0)   GO TO 151
      ARND = 0.005
      NDPA = 2
      IF (DISPER .GE. 0.2)   GO TO 151
      ARND = 0.0005
      NDPA = 3
      IF (DISPER .GE. 0.02)   GO TO 151
      ARND = 0.00005
      NDPA = 4
C
  151 CONTINUE
      IF (CALIB) THEN
         MH1B(11:20) = UNITS
         MH1B(29:38) = UNITS
      ELSE
         MH1B(11:20) = ' X-units  '
         MH1B(29:38) = ' X-units  '
      END IF
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER(MH1A,STATUS)
      CALL PAR_WRUSER(MH1B,STATUS)
      CALL PAR_WRUSER(' ',STATUS)
      IF (LOGLU.NE.0) WRITE (LOGLU,'(/A)',IOSTAT=IGNORE) MH1A
      IF (LOGLU.NE.0) WRITE (LOGLU,'(A/)',IOSTAT=IGNORE) MH1B
C
C  OUTPUT FORMAT
      STRMAX = STR(1)
      FWHMS=0.0
      STRENS=0.0
      DO 160 N160 = 1,LINE
         STREN = STR(N160)
         IF (CALIB) THEN
            FA = FIG_XVALUE (BIN(N160)+1.0,X,NX)
            FWHM = ABS (FIG_XVALUE (BIN(N160)+1.0+WID(N160)*.5,X,NX) -
     :                    FIG_XVALUE (BIN(N160)+1.0-WID(N160)*.5,X,NX))
            STREN = STREN * FWHM / WID(N160)
         END IF
         FWHMS=FWHMS+STR(N160)
         STRENS=STRENS+STR(N160)/WID(N160)
         IF (STREN.GT.STRMAX) STRMAX=STREN
  160    CONTINUE
      FMTSTR = '(F8.2,F12.N,F7.2,F10.N,F10.3,F10.3)'
      FMTSTR(11:11) = CHAR (ICHAR('0') + NDPA)
      FMTSTR(22:22) = FMTSTR(11:11)
      IF ((STRMAX.GT.32000.0).OR.(STRMAX.LT.0.1))
     :                          FMTSTR(24:34) = '2(1PE10.3)'
C
      DO 180 N180 = 1,LINE
C
C     BIN NUMBER, WIDTH (BINS)
C     POSITION & WIDTH IN AXIS UNITS
         IF (.NOT.CALIB)   GO TO 163

C        POSITION (AXIS UNITS)
            FA = FIG_XVALUE (BIN(N180)+1.0,X,NX)
C
C        WIDTH (ANGSTROM)
            FWHM = ABS (FIG_XVALUE (BIN(N180)+1.0+WID(N180)*.5,X,NX) -
     :                    FIG_XVALUE (BIN(N180)+1.0-WID(N180)*.5,X,NX))
C
  163    CONTINUE
C
C     INTEGRATED STRENGTH
         STREN = STR(N180)
         IF (CALIB)   STREN = STREN * FWHM / WID(N180)
C
C     PEAK HEIGHT
  165    CONTINUE
         PEAK = 0.939437 *STR(N180) /WID(N180)
C
  169    CONTINUE
         WRITE (LOG,FMTSTR,IOSTAT=IGNORE) BIN(N180)+1.0,FA,WID(N180),
     :                                                FWHM,STREN,PEAK
C
C        Accumulate the computed values in work arrays.
C
         BINA(N180) = BIN(N180)+1.0
         FAA(N180) = FA
         FWHMAA(N180) = FWHM
         STRENA(N180) = STREN
         PEAKA(N180) = PEAK
C
         IF (.NOT.CALIB) THEN
            LOG(9:20)='      --'
            LOG(28:37)='      --'
         END IF
         CALL PAR_WRUSER (LOG,STATUS)
         IF (LOGLU.NE.0) WRITE (LOGLU,'(A)',IOSTAT=IGNORE) LOG
  180    CONTINUE
C
C        Write the number of lines as an ADAM parameter and write the
C        line details as ADAM parameter arrays.
C
         RLINE = REAL(LINE)
         CALL VAR_SETNUM ('EMLT_LINES', 0, 0, RLINE, STATUS)
C
         CALL VAR_SETARY ('EMLT_BIN', LINE, BINA, STATUS)
         CALL VAR_SETARY ('EMLT_POS', LINE, FAA, STATUS)
         CALL VAR_SETARY ('EMLT_FWHM_BIN', LINE, WID, STATUS)
         CALL VAR_SETARY ('EMLT_FWHM_ANG', LINE, FWHMAA, STATUS)
         CALL VAR_SETARY ('EMLT_STREN', LINE, STRENA, STATUS)
         CALL VAR_SETARY ('EMLT_PEAK', LINE, PEAKA, STATUS)
C
C   CONSTRUCT 'MARK' SCAN
C
      IF (.NOT.OUTSET)   GO TO 250
C
C   ZAP ORIGINAL DATA
C
      DO 210 N210 = 1,NX
C
         Y(N210) = 0.0
  210    CONTINUE
C
      DO 230 N230 = 1,LINE
C
         FWHM = WID(N230)
         ALPHA = 1.665109 /FWHM
         XC = BIN(N230)
         PEAKO2 = 0.469719 *STR(N230) /FWHM
         XHW = 4.163 *FWHM
         XL = XC -XHW
         NXL = IFIX(XL +0.5)
         NXL = MAX0(1,NXL)
         XH = XC +XHW
         NXH = IFIX(XH +1.5)
         NXH = MIN0(NXH,NX)
C
         DO 220 N220 = NXL,NXH
C
            XV = FLOAT(N220-1) -XC
            RL = (XV -0.25) *ALPHA
            RH = (XV +0.25) *ALPHA
            Y(N220) = Y(N220) + PEAKO2 *(EXP(-RL*RL) +EXP(-RH*RH))
  220       CONTINUE
C
  230    CONTINUE
  250 CONTINUE
      IF (STRENS .LE. 0.0)   GO TO 300
C
         FWHMA = FWHMS / STRENS
         FMTSTR='(A,F10.N,1X,A)'
         FMTSTR(8:8) = CHAR (ICHAR('0') + NDPA)
         WRITE (LOG,FMTSTR,IOSTAT=IGNORE) M1,FWHMA,M1A
         CALL PAR_WRUSER(' ',STATUS)
         CALL PAR_WRUSER(LOG,STATUS)
         IF (LOGLU.NE.0) WRITE (LOGLU,'(/A)',IOSTAT=IGNORE) LOG
C
         IF (.NOT.CALIB)   GO TO 260
C
         FWHMA = FWHMA *DISPER
         WRITE (LOG,FMTSTR,IOSTAT=IGNORE) M2,FWHMA,UNITS
         CALL PAR_WRUSER(LOG,STATUS)
         IF (LOGLU.NE.0) WRITE (LOGLU,'(A)',IOSTAT=IGNORE) LOG
C
  260    CONTINUE
C
  300 CONTINUE
      END
