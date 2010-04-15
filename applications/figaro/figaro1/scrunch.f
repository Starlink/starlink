C+
      SUBROUTINE SCRUNCH
C
C     S C R U N C H
C
C     Figaro routine to scrunch a spectrum or set of spectra.  Can
C     scrunch either into a linear wavelength scale, where the
C     wavelength increment from bin to bin is constant across the
C     spectrum, or into a logarithmic scale, where the increment of the
C     log of the wavelength from bin to bin is constant.  The operation
C     is performed by the routine FIG_REBIN.
C
C     If the input file is 2D data, then it is treated as a set of
C     1D spectra and each is scrunched individually.   If the wavelength
C     array is 1D, then this single array will be used for all
C     the spectra.  If it is 2D, then each spectrum will be scrunched
C     according to the corresponding cross-section of the wavelength
C     array.
C
C     The routine can either conserve flux or the mean value
C     of the data.  Conserving flux is appropriate where the data is
C     actually in flux units, but not when the data is in magnitude
C     units.  (Consider the case where each input bin maps to two
C     output bins; if the data is in flux units - photon counts, for
C     example - then flux should be conserved and the mean data level
C     should drop by a factor 2; if the data is in magnitudes, then
C     the rebinning should not change the mean level.)  The program
C     does not try to determine for itself whether the data is in flux
C     or in magnitudes - it uses a command keyword.
C
C     Command parameters -
C
C     SPECTRUM     (Character) The name of the spectrum to be scrunched.
C     WSTART       (Numeric) The wavelength of the CENTER of the first
C                  bin of the resulting scrunched spectrum.
C     WEND         (Numeric) The wavelength of the CENTER of the final
C                  bin of the resulting scrunched spectrum.  If WEND is
C                  less than WSTART, then SCRUNCH assumes that it is the
C                  increment rather than the final value that is being
C                  specified.  If the scrunch is logarithmic and WSTART
C                  is greater than WEND, SCRUNCH assumes that the WEND
C                  value represents a velocity in km/sec.  These
C                  assumptions can be controlled directly by the
C                  keywords INCREMENT and FINAL, if they will not give
C                  the desired effect.
C     BINS         (Numeric) The number of bins for the resulting
C                  spectrum.
C     OUTPUT       (Character) The name of the resulting spectrum.
C                  Note that SCRUNCH cannot rebin a spectrum into itself
C                  and so will always create a new output file.
C
C     Command keywords -
C
C     LOG          Bin into logarithmic wavelength bins.
C     MEAN         Conserve mean data level rather than flux.
C     FLUX         Conserve flux rather than mean data level.
C     LINEAR       Use linear interpolation when rebinning.
C     QUAD         Use quadratic interpolation when rebinning.
C     INCREMENT    WEND is an increment value, even though it is >
C                  WSTART.
C     FINAL        WEND is a final value, even though it is < WSTART.
C
C     User variables -
C
C     SCRUNCH_INC  Is set to the wavelength increment if linear
C                  rebinning is used, and to the velocity increment if
C                  log rebinning is used.
C     SCRUNCH_END  Is set to the final wavelength value.  (This is for
C                  those cases where the WEND value represents an
C                  increment.)
C
C                                              KS / CIT 7th Feb 1984
C     Modified:
C
C     28th Mar 1985  KS / AAO.  AXIS(1) log flag now written into the
C                    output file rather than the input!
C     29th Apr 1985  KS / AAO.  SCRUNCH_INC now set.
C     12th Sep 1985  KS / AAO.  Now works on a double precision
C                    wavelength array, to handle high dispersion data.
C     30th Dec 1985  KS / AAO.  Ends of spectra are now zeroed properly
C                    when they represent wavelength values outside the
C                    range of the input spectrum.
C     2nd  Jan 1986  KS / AAO.  Use of WEND as an increment value
C                    introduced, along with INCREMENT and FINAL keywords
C                    and setting of SCRUNCH_INC in the log case and setting
C                    of SCRUNCH_END. Now operates on a 2D set of spectra
C                    as well as on a single spectrum.  Error arrays now
C                    no longer copied over.
C     4th  May 1986  KS / AAO.  Initial values for flags now set
C                    explicitly instead of being assumed to default to
C                    .FALSE.
C     17th Oct 1988  KS / AAO.  GEN_ELEMD values properly cast to float
C                    before being passed to ICH_ENCODE.
C     3rd  Nov 1988  JM / RAL. Modified to use DSA_ routines
C                    Dynamic memory handling changed to use
C                    DYN_ routines
C     2nd  Mar 1990  KS / AAO. DSA_GET/SET_AXIS_INFO now support the log
C                    flag, so that is now used.  DYN_INCREMENT used
C                    instead of assuming DYNAMIC_MEM elements are bytes.
C     29th Sep 1992  HME / UoE, Starlink.  INCLUDE changed. Call
C                    ICH_ENCODE with float, not double.
C      6th Oct 1993  HME / UoE, Starlink.  Mapping the output for update
C                    causes DSA_MAP_DATA to unflag whatever garbage may
C                    be in the new array.  Change access to "write".
C                    (This is for the output data only, any other output
C                    arrays are still mapped update.)
C     24th Jul 1996  MJCL / Starlink, UCL.  Changed type of INVOKE.
C     12th Dec 1997  ACD / UoE, Starlink.  Increased the maximum size
C                    of the rebinned data from 50000 to 5,000,000
C                    elements.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      DOUBLE PRECISION GEN_ELEMD
      INTEGER  ICH_ENCODE
      INTEGER  ICH_FOLD
      INTEGER  ICH_LEN
      LOGICAL  PAR_GIVEN
C
C     Local variables
C
      DOUBLE PRECISION C         ! Speed of light
      CHARACTER CITEMS(2)*32     ! Axis character items retrieved
      INTEGER   CSTAT            ! Used to format character strings
      DOUBLE PRECISION DELTA     ! Wavelength increment
      INTEGER   DIMS(5)          ! Accommodates axis-data dimensions
      DOUBLE PRECISION DWEND     ! Final wavelength with 'DOUBLE'
                                 ! precision
      DOUBLE PRECISION DWSTART   ! Start wavelength with 'DOUBLE'
                                 ! precision
      LOGICAL   EXIST            ! Used to check for existence of axis
                                 ! data
      LOGICAL   FINAL            ! True if  WEND is a final value
      LOGICAL   FLUX             ! Conserve flux?
      INTEGER   IDIM             ! Loop variable
      INTEGER   IMODE            ! Selects mode of transfer for input
                                 ! bins
      LOGICAL   INCREM           ! True if WEND is an increment value
      INTEGER   INVOKE           ! Used to format character strings
      LOGICAL   ISNEW            ! Is address new to CNF?
      LOGICAL   ISNEWX           ! Is XPTR address new to CNF?
      LOGICAL   ISNRZ            ! Is address RZPTR new to CNF?
      INTEGER   IQUAD            ! Equals 0 for lin. interp. non-zero
                                 ! for quadratic
      INTEGER   ISPECT           ! Loop variable
      LOGICAL   LINEAR           ! If true, linear interpolation used
      LOGICAL   LOGW             ! Wavelengths in WAVES logarithmic?
      LOGICAL   LOGWR            ! Wavelengths in WAVESR logarithmic?
      LOGICAL   MEAN             ! Conserve mean data level, not flux?
      INTEGER   NADD             ! No. of input bins added to form one
                                 ! output bin
      INTEGER   NBINR            ! Number of elements (bins) in
                                 ! rebinned data
      INTEGER   NCITEMS          ! Number of axis character items
                                 ! retrieved
      INTEGER   NDIM             ! Number of dimensions in axis data
      INTEGER   NELM             ! No. of elements
      INTEGER   NEXT             ! Used in formatting strings
      DOUBLE PRECISION NITEMS(1) ! Axis numeric items retrieved
      INTEGER   NNITEMS          ! Number of axis numeric items
                                 ! retrieved
      INTEGER   NSPECT           ! No. of spectra in SPECT
      INTEGER   NX               ! Number of elements (bins) in input
                                 ! data
      INTEGER   NXSPECT          ! No. of spectra in axis structure
      LOGICAL   PISNEW           ! Previous CNF pointer to data new?
      LOGICAL   PISNRZ           ! Previous CNF pointer to output data
                                 ! new?
      LOGICAL   PISNX            ! Previous CNF pointer wavelengths new?
      INTEGER   PSTAT            ! Status for PAR_ routines
      LOGICAL   QUAD             ! Quadratic interpolation used?
      REAL      RESET            ! Reset value for real parameter
      INTEGER   RXPTR            ! Pointer to output wavelength array
                                 ! (Mode 1 only)
      INTEGER   RZPTR            ! Pointer to output array containing
                                 ! rebinned data
      INTEGER   SLOT             ! Slot number
      REAL      SSKEW            ! No. of bins the input array is to
                                 ! be shifted
      INTEGER   STATUS           ! Running status for DSA_ routines
      CHARACTER STRING*72        ! Used to format strings
      INTEGER   TPTR             ! Temporary pointer
      REAL      VALUE            ! Value of some real parameters
      REAL      WEND             ! End wavelength
      REAL      WSTART           ! Start wavelength
      INTEGER   XPTR             ! Pointer to input wavelength array
                                 ! (Mode 1 only)
      CHARACTER XUNITS*72        ! Axis data units
      INTEGER   ZPTR             ! Pointer to input data
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Real value limits
C
      REAL FMAX,FMIN
      PARAMETER (FMAX=1.7E38,FMIN=-1.7E38)
C
C     Velocity of light in Km/sec
C
      DATA C/299792.458/
C
C     Initial values
C
      NCITEMS=2
      NNITEMS=1
      STATUS=0
C
C     Open DSA
C
      CALL DSA_OPEN(STATUS)
C
C     Get 'SPECTRUM' and open the input file
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
C
C     Get dimensions of AXIS(1) data and map it.
C
      CALL DSA_SEEK_AXIS('SPECT',1,EXIST,STATUS)
      IF(.NOT.EXIST)THEN
         CALL PAR_WRUSER('No axis data array',PSTAT)
         GOTO 500
      END IF
      CALL DSA_AXIS_SIZE('SPECT',1,5,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      NXSPECT=1
      IF (NDIM.GT.1) THEN
         DO IDIM=2,NDIM
            NXSPECT=NXSPECT*DIMS(IDIM)
         END DO
      END IF

      CALL DSA_MAP_AXIS_DATA('SPECT',1,'READ','DOUBLE',XPTR,SLOT,STATUS)
C
C     Get dimensions of data, check it for compatibility with the
C     AXIS(1) data, then map it.
C
      CALL DSA_DATA_SIZE('SPECT',5,NDIM,DIMS,NELM,STATUS)
      IF (DIMS(1).NE.NX) THEN
         CALL PAR_WRUSER('Data & wavelength arrays differ in size',
     :                   PSTAT)
         GO TO 500
      END IF
      NSPECT=1
      IF (NDIM.GT.1) THEN
         DO IDIM=2,NDIM
            NSPECT=NSPECT*DIMS(IDIM)
         END DO
      END IF
      IF ((NXSPECT.NE.1).AND.(NXSPECT.NE.NSPECT)) THEN
         CALL PAR_WRUSER(
     :       'Wavelength and data arrays have incompatible dimensions',
     :       PSTAT)
         GO TO 500
      END IF
      CALL DSA_MAP_DATA('SPECT','READ','FLOAT',ZPTR,SLOT,STATUS)
C
C     Determine the wavelength units being used for the data
C
      CALL DSA_GET_AXIS_INFO('SPECT',1,NCITEMS,CITEMS,
     :                        NNITEMS,NITEMS,STATUS)
      IF(STATUS.NE.0)GOTO 500
      XUNITS=CITEMS(1)
      STRING=XUNITS
      INVOKE=ICH_FOLD(STRING)
      IF ((STRING.NE.'MICRONS').AND.(STRING.NE.'ANGSTROMS')) THEN
         CALL PAR_WRUSER('WARNING - input data has wavelength in '//
     :                    XUNITS(:ICH_LEN(XUNITS)),PSTAT)
         CALL PAR_WRUSER(
     :       'which is not an expected unit.  However, the scrunching',
     :                                                       PSTAT)
         CALL PAR_WRUSER('can still be performed OK',PSTAT)
      END IF
C
C     Get scrunch parameters - start & end wavelength, # bins.
C     (The reset values are just the current limits rounded to 10
C     angstroms - probably not very useful).  Also find if data
C     is to be binned logarithmically.
C
      CALL PAR_RDKEY('LOG',.FALSE.,LOGWR)

      RESET=(INT(GEN_ELEMD(%VAL(CNF_PVAL(XPTR)),1))/10)*10
      CALL PAR_RDVAL('WSTART',FMIN,FMAX,RESET,XUNITS,WSTART)

      RESET=(INT(GEN_ELEMD(%VAL(CNF_PVAL(XPTR)),NX))/10+1)*10
      CALL PAR_RDVAL('WEND',FMIN,FMAX,RESET,XUNITS,WEND)

      CALL PAR_RDVAL('BINS',1.,5.0E6,FLOAT(NX),' ',VALUE)
      NBINR=VALUE
C
C     Sort out whether the WEND value is meant to be an incremental
C     value or not.
C
      DWSTART=DBLE(WSTART)
      DWEND=DBLE(WEND)
      IF (WSTART.GT.WEND) THEN
         IF (PAR_GIVEN('INCREMENT')) THEN
            CALL PAR_RDKEY('INCREMENT',.TRUE.,INCREM)
         ELSE
            CALL PAR_RDKEY('FINAL',.FALSE.,FINAL)
            INCREM=.NOT.FINAL
         END IF
      ELSE
         IF (PAR_GIVEN('FINAL')) THEN
            CALL PAR_RDKEY('FINAL',.TRUE.,FINAL)
            INCREM=.NOT.FINAL
         ELSE
            CALL PAR_RDKEY('INCREMENT',.FALSE.,INCREM)
         END IF
      END IF
C
C     If it is an incremental value, calculate the final wavelength
C
      IF (INCREM) THEN
         DELTA=DWEND
         IF (.NOT.LOGWR) THEN
            DWEND=DWSTART+(NBINR-1)*DELTA
         ELSE
            DWEND=EXP(LOG(DWSTART)+(NBINR-1)*LOG(DELTA/C+1))
         END IF
      END IF
C
C     Get all the keywords.  Note that FLUX and MEAN are mutually
C     exclusive, as are LINEAR and QUAD.
C
      FLUX=.FALSE.
      MEAN=.FALSE.
      IF (PAR_GIVEN('FLUX')) THEN
         CALL PAR_RDKEY('FLUX',.FALSE.,FLUX)
         IF (PAR_GIVEN('MEAN')) THEN
            CALL PAR_RDKEY('MEAN',.TRUE.,MEAN)
            IF (MEAN.AND.FLUX) THEN
               CALL PAR_WRUSER('FLUX and MEAN are mutually exclusive',
     :                                                         PSTAT)
               GO TO 500
            END IF
         END IF
         MEAN=.NOT.FLUX
      ELSE
         CALL PAR_RDKEY('MEAN',.FALSE.,MEAN)
         FLUX=.NOT.MEAN
      END IF
C
      QUAD=.FALSE.
      LINEAR=.FALSE.
      IF (PAR_GIVEN('LINEAR')) THEN
         CALL PAR_RDKEY('LINEAR',.FALSE.,LINEAR)
         IF (PAR_GIVEN('QUAD')) THEN
            CALL PAR_RDKEY('QUAD',.TRUE.,QUAD)
            IF (QUAD.AND.LINEAR) THEN
               CALL PAR_WRUSER('LINEAR and QUAD are mutually exclusive',
     :                                                         PSTAT)
               GO TO 500
            END IF
         END IF
         QUAD=.NOT.LINEAR
      ELSE
         CALL PAR_RDKEY('QUAD',.TRUE.,QUAD)
         LINEAR=.NOT.QUAD
      END IF
C
C     See if there is any indication that the input spectrum is
C     binned on a log scale - it really shouldn't be, because if it
C     is it's about to be scrunched twice.  Note that NITEMS(1) as
C     returned by DSA_GET_AXIS_INFO is the log binning flag.
C
      LOGW=NITEMS(1).NE.0.0
      IF (LOGW) THEN
         CALL PAR_WRUSER(
     :     'WARNING- Input data is already scrunched to a log scale',
     :                                                        PSTAT)
      END IF
C
C     Create output spectrum modelled on input but without
C     data and axis structures.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','SPECT',NO_DATA,NEW_FILE,STATUS)
C
C     Create output axis and data arrays of appropriate size.
C     Note that NDIM and DIMS still contain the values
C     obtained for the input data array.
C
      DIMS(1)=NBINR
      CALL DSA_RESHAPE_DATA('OUTPUT','SPECT',NDIM,DIMS,STATUS)
      CALL DSA_RESHAPE_AXIS('OUTPUT',1,'SPECT',1,1,NBINR,STATUS)
C
C     Set LOG flag in output data
C
      IF (LOGWR) THEN
         NITEMS(1)=1.0
         NCITEMS=0
         CALL DSA_SET_AXIS_INFO ('OUTPUT',1,NCITEMS,CITEMS,
     :                                     NNITEMS,NITEMS,STATUS)
      END IF
C
C     Then map the output data arrays
C
      CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',RZPTR,SLOT,STATUS)
      CALL DSA_MAP_AXIS_DATA('OUTPUT',1,'UPDATE','DOUBLE',
     :                       RXPTR,SLOT,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Fill up the output wavelength array
C
      CALL FIG_WFILLD(DWSTART,DWEND,LOGWR,NBINR,%VAL(CNF_PVAL(RXPTR)))
C
C     A little information for the user about the input data..
C
      CALL PAR_WRUSER(' ',PSTAT)
      STRING='Original wavelength range was '
      VALUE=GEN_ELEMD(%VAL(CNF_PVAL(XPTR)),1)
      CSTAT=ICH_ENCODE(STRING,VALUE,31,3,NEXT)
      STRING(NEXT:)=' to '
      VALUE=GEN_ELEMD(%VAL(CNF_PVAL(XPTR)),NX)
      CSTAT=ICH_ENCODE(STRING,VALUE,NEXT+4,3,NEXT)
      STRING(NEXT:)=' '//XUNITS
      CALL PAR_WRUSER(STRING,PSTAT)
      CALL PAR_WRUSER(' ',PSTAT)
C
C     Now we're finally in a position to perform the scrunch..
C
      IMODE=1
      IQUAD=0
      IF (QUAD) IQUAD=1
      NADD=1
      SSKEW=0.

      PISNEW = .FALSE.
      PISNRZ = .FALSE.
      PISNX = .FALSE.

      DO ISPECT=1,NSPECT
         CALL FIG_REBIND(IMODE,IQUAD,%VAL(CNF_PVAL(ZPTR)),NX,
     :                   %VAL(CNF_PVAL(RZPTR)),NBINR,NADD,SSKEW,FLUX,
     :                   %VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(RXPTR)),
     :                   LOGW,LOGWR)

C       Increment the pointers for the next spectrum, tidying up
C       unwanted CNF resources as they're no longer needed.
         CALL DYN_INCAD(ZPTR,'FLOAT',NX,TPTR,ISNEW,STATUS)
         IF (ISNEW) CALL CNF_UNREGP(ZPTR)
         ZPTR=TPTR
         PISNEW = ISNEW

         CALL DYN_INCAD(RZPTR,'FLOAT',NBINR,TPTR,ISNRZ,STATUS)
         IF (ISNRZ) CALL CNF_UNREGP(RZPTR)
         RZPTR=TPTR
         PISNRZ = ISNRZ

         IF (NXSPECT.GT.1) THEN
            CALL DYN_INCAD(XPTR,'DOUBLE',NX,TPTR,ISNEWX,STATUS)
            IF (ISNEWX) CALL CNF_UNREGP(XPTR)
            XPTR=TPTR
            PISNX = ISNEWX
         END IF
      END DO
      IF (ISNEW) CALL CNF_UNREGP(ZPTR)
      IF (ISNRZ) CALL CNF_UNREGP(RZPTR)
      IF (NXSPECT.GT.1.AND.ISNEWX) CALL CNF_UNREGP(XPTR)
C
C     In 2D, explain how the axis data is being used.
C
      IF (NSPECT.GT.1) THEN
         CALL PAR_WRUSER('2-dimensional data array scrunched using',
     :                                                         PSTAT)
         IF (NXSPECT.GT.1) THEN
            CALL PAR_WRUSER('a 2-dimensional wavelength array.',PSTAT)
         ELSE
            CALL PAR_WRUSER('a single wavelength array.',PSTAT)
         END IF
         CALL PAR_WRUSER(' ',PSTAT)
      END IF
C
C     And something about the output data and set the SCRUNCH_INC and
C     SCRUNCH_END variables.
C
      STRING='Data rebinned into '
      CSTAT=ICH_ENCODE(STRING,FLOAT(NBINR),20,0,NEXT)
      STRING(NEXT:)=' bins from '
      VALUE=GEN_ELEMD(%VAL(CNF_PVAL(RXPTR)),1)
      CSTAT=ICH_ENCODE(STRING,VALUE,NEXT+11,2,NEXT)
      STRING(NEXT:)=' to '
      VALUE=GEN_ELEMD(%VAL(CNF_PVAL(RXPTR)),NBINR)
      CSTAT=ICH_ENCODE(STRING,VALUE,NEXT+4,2,NEXT)
      IF (LOGWR) STRING(NEXT:)=' (on a log scale)'
      CALL PAR_WRUSER(STRING,PSTAT)
      CALL VAR_SETNUM('SCRUNCH_END',0,0,VALUE,CSTAT)
      STRING='Giving a wavelength increment of '
      IF (LOGWR) THEN
         DELTA=(LOG(DWEND)-LOG(DWSTART))/(NBINR-1)
         VALUE=C*(EXP(DELTA)-1)
      ELSE
         VALUE=(DWEND-DWSTART)/(NBINR-1)
      END IF
      CSTAT=ICH_ENCODE(STRING,VALUE,34,4,NEXT)
      IF (LOGWR) THEN
         STRING(NEXT:)=' Km/sec'
      ELSE
         STRING(NEXT:)=' '//XUNITS
      END IF
      CALL PAR_WRUSER(STRING,PSTAT)
      CALL VAR_SETNUM('SCRUNCH_INC',0,0,VALUE,CSTAT)
      CALL PAR_WRUSER(' ',PSTAT)

  500 CONTINUE

C     Now tidy up

      CALL DSA_CLOSE(STATUS)

      END

