C+
      SUBROUTINE FSCRUNCH
C
C     F S C R U N C H
C
C     Figaro routine to scrunch a spectrum or set of spectra in which
C     the input wavelength ranges for the various pixels are not
C     necessarily continuous, and may overlap.  It can scrunch
C     either into a linear wavelength scale, where the wavelength
C     increment from bin to bin is constant across the spectrum, or
C     into a logarithmic scale, where the increment of the log of
C     the wavelength from bin to bin is constant.
C
C     If the input file is 2D data, then it is treated as a set of
C     1D spectra and each is scrunched individually.   If the wavelength
C     array (.X.DATA) is 1D, then this single array will be used for all
C     the spectra.  If it is 2D, then each spectrum will be scrunched
C     according to the corresponding cross-section of the wavelength
C     array.
C
C     The routine can either conserve flux or the mean value
C     of the data.  Conserving flux is appropriate where the data is
C     actually in flux units (photons/sec, for example), but not when
C     the data is in units of flux per unit wavelength (AB magnitudes,
C     Janskys, etc). Consider the case where each input bin maps to two
C     output bins; if the data is in flux units - photon counts, for
C     example - then flux should be conserved and the mean data level
C     should drop by a factor 2; if the data is in magnitudes, then
C     the rebinning should not change the mean level.  The program
C     tries to determine for itself whether the data is in flux
C     or in flux per unit wavelength by looking at the units, but
C     uses a command keyword (DENSITY) to confirm its guess.
C
C     Command parameters -
C
C     SPECTRUM     (Character) The name of the spectrum to be scrunched.
C     WSTART       (Numeric) The wavelength of the CENTER of the first
C                  bin of the resulting scrunched spectrum.
C     WEND         (Numeric) The wavelength of the CENTER of the final
C                  bin of the resulting scrunched spectrum.  If WEND is
C                  less than WSTART, then FSCRUNCH assumes that it is
C                  the increment rather than the final value that is
C                  being specified.  If the scrunch is logarithmic and
C                  WSTART is greater than WEND, FSCRUNCH assumes that
C                  the WEND value represents a velocity in km/sec. These
C                  assumptions can be controlled directly by the
C                  keywords INCREMENT and FINAL, if they will not give
C                  the desired effect.
C     BINS         (Numeric) The number of bins for the resulting
C                  spectrum.
C     INORDER      (Numeric) The order of local fit to be used for the
C                  input data.   Can be 0,1 or 2.
C     OUTPUT       (Character) The name of the resulting spectrum.
C                  Note that FSCRUNCH cannot rebin a spectrum into
C                  itself and so will always create a new output file.
C
C     Command keywords -
C
C     LOG          Bin into logarithmic wavelength bins.
C     DENSITY      Treat input data as being in units of flux per unit
C                  wavelength.
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
C     Input data -
C
C     The input file is expected to contain a data array giving
C     the wavelengths of the centres of the data elements, and an
C     width specification which can be either a single number,
C     a 1D array, or a 2D array, giving the wavelength range covered
C     by each of the input data elements.  If an error array exists
C     this will be used as well.
C                                              KS / AAO 17th June 1986
C     Modified:
C
C     14th Jul 1986  KS / AAO.  Data units string cleaned before output.
C                    Wavelength increment formatted by ICH_CF instead
C                    of ICH_ENCODE - allows automatic precision setting.
C     22nd Apr 1991  KS / AAO.  Modified to use DSA routines.
C     29th Sep 1992  HME / UoE, Starlink.  INCLUDE changed. Call
C                    PAR_WRUSER rather than DSA_WRUSER, FIG_SETERR
C                    rather than SETERR. Call ICH_ENCODE with a single-
C                    precision number, not double.
C     2005 June 7    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT, PAR_GIVEN, GEN_INCCHKD
      INTEGER ICH_CLEAN,ICH_ENCODE,ICH_FOLD,ICH_LEN
      DOUBLE PRECISION GEN_ELEMD
      CHARACTER*16 ICH_CF
C
C     Real value limits
C
      REAL FMAX,FMIN
      PARAMETER (FMAX=1.7E38,FMIN=-1.7E38)
C
C     Local variables
C
      DOUBLE PRECISION C         ! Speed of light
      LOGICAL   DEFDEN           ! True if default is flux density units
      DOUBLE PRECISION DELTA     ! Wavelength increment
      INTEGER   DIMS(5)          ! Data array dimensions
      INTEGER   DSA_STATUS       ! Inherited status used by DSA routines
      DOUBLE PRECISION DWEND     ! WEND in double precision
      DOUBLE PRECISION DWSTART   ! WSTART in double precision
      DOUBLE PRECISION DVALUE    ! General double-precision real variable
      INTEGER   EPTR             ! Dynamic mem pointer for input error array
      LOGICAL   ERRORS           ! True if spectrum has error information
      LOGICAL   EXIST            ! True if width values exist in data
      LOGICAL   FAULT            ! Indicates non-DSA fault encountered
      LOGICAL   FINAL            ! Value of FINAL keyword
      LOGICAL   FLUXDEN          ! Value of DENSITY keyword
      LOGICAL   INCREM           ! Value of INCREMENT keyword
      INTEGER   INTERP           ! Value of INORDER keyword
      INTEGER   INVOKE           ! Dummy function value
      LOGICAL   ISNEW            ! Is address new to CNF?
      LOGICAL   ISNEWE           ! Is EPTR address new to CNF?
      LOGICAL   ISNEWW           ! Is WPTR address new to CNF?
      LOGICAL   ISNEWX           ! Is XPTR address new to CNF?
      LOGICAL   ISNRE            ! Is address REPTR new to CNF?
      LOGICAL   ISNRZ            ! Is address RZPTR new to CNF?
      INTEGER   ISPECT           ! Loop index through spectra
      LOGICAL   LOGW             ! Input data are logarithmically
                                 ! binned?
      LOGICAL   LOGWR            ! Value of LOG keyword
      INTEGER   NBINR            ! Number of elements in scrunched
                                 ! spectra
      INTEGER   NCHXU            ! Number of characters in X-units
                                 ! string
      INTEGER   NDIM             ! Number of data dimensions
      INTEGER   NELM             ! Number of main data array elements
      INTEGER   NEXT             ! Next character position in string
      INTEGER   NSPECT           ! Number of spectra in data
      INTEGER   NWID             ! Number of width values
      INTEGER   NX               ! Length of input spectra
      INTEGER   NXELM            ! Number of X-axis data array elements
      INTEGER   NXSPECT          ! Number of 'spectra' in x-axis arrays
      INTEGER   OTPTR            ! Temporary dynamic mem pointer
      LOGICAL   PISNE            ! Previous CNF pointer to errors new?
      LOGICAL   PISNEW           ! Previous CNF pointer to data new?
      LOGICAL   PISNRE           ! Previous CNF pointer to output errors
                                 ! new?
      LOGICAL   PISNRZ           ! Previous CNF pointer to output data
                                 ! new?
      LOGICAL   PISNW            ! Previous CNF pointer widths new?
      LOGICAL   PISNX            ! Previous CNF pointer wavelengths new?
      INTEGER   REPTR            ! Dynamic mem pointer for rebinned
                                 ! error array
      REAL      RESET            ! Used for default wavelength values
      INTEGER   RXPTR            ! Dynamic mem pointer for output
                                 ! X-data array
      INTEGER   RZPTR            ! Dynamic mem pointer for output data
                                 ! array
      LOGICAL   SINGLE           ! True if width info is a single value
      INTEGER   SLOT             ! Slot number for mapped arrays-ignored
      INTEGER   STATUS           ! General status variable
      CHARACTER STRING*72        ! General character variable
      INTEGER   TPTR             ! Temporary dynamic mem pointer
      REAL      VALUE            ! General single-precision real
                                 ! variable
      INTEGER   WPTR             ! Dynamic mem pointer for width array
      INTEGER   WKPTR            ! Dynamic mem pointer for work array
      REAL      WEND             ! Value of WEND parameter
      REAL      WSTART           ! Value of WSTART parameter
      REAL      WIDTH            ! Single width value
      INTEGER   XPTR             ! Dynamic mem pointer for input X-data
                                 ! array
      CHARACTER XUNITS*72        ! X-axis data units
      INTEGER   ZPTR             ! Dynamic mem pointer for input data
                                 ! array
      CHARACTER ZUNITS*72        ! Data units
C
C     Parameters for DSA_OUTPUT
C
      INTEGER   NO_DATA,NEW_FILE
      PARAMETER (NO_DATA=1,NEW_FILE=1)
C
C     Velocity of light in Km/sec
C
      DATA C/299792.458/
C
C     Initial values
C
      FAULT=.FALSE.
C
C     Initialise DSA routines.
C
      DSA_STATUS=0
      CALL DSA_OPEN(DSA_STATUS)
C
C     Open the input spectrum
C
      CALL DSA_INPUT ('SPECT','SPECTRUM',DSA_STATUS)
C
C     Get dimensions of X data and map it.
C
      CALL DSA_AXIS_SIZE ('SPECT',1,5,NDIM,DIMS,NXELM,DSA_STATUS)
      NX=DIMS(1)
      NXSPECT=NXELM/NX
      CALL DSA_MAP_AXIS_DATA ('SPECT',1,'READ','DOUBLE',XPTR,
     :                        SLOT,DSA_STATUS)
      IF (DSA_STATUS.NE.0) GO TO 500    ! Error exit
C
C     Make sure the X array is in increasing order
C
      TPTR=XPTR
      PISNEW = .FALSE.
      DO ISPECT=1,NXSPECT
         IF (.NOT.GEN_INCCHKD(%VAL(CNF_PVAL(TPTR)),NX)) THEN
            CALL PAR_WRUSER(
     :         'Wavelength array needs to be in increasing order',
     :                                                     STATUS)
            FAULT=.TRUE.
            GO TO 500
         END IF
         CALL DYN_INCAD(TPTR,'DOUBLE',NX,OTPTR,ISNEW,STATUS)
         IF (PISNEW) CALL CNF_UNREGP(TPTR)
         TPTR = OTPTR
         PISNEW = ISNEW
      END DO
      IF (ISNEW) CALL CNF_UNREGP(TPTR)
C
C     Now look at the X-axis width information. We can accept this as
C     a scalar, a 1D array, or a 2D array, but it ought to exist.
C
      CALL DSA_SEEK_WIDTH ('SPECT',1,EXIST,SINGLE,DVALUE,DSA_STATUS)
      IF (.NOT.EXIST) THEN
         CALL PAR_WRUSER(
     :      'Note: X-axis has no explicit width information.',
     :                                                     STATUS)
         CALL PAR_WRUSER(
     :      'Will assume data covers an unbroken range in X.',
     :                                                     STATUS)
         SINGLE=.FALSE.
      END IF
      IF (SINGLE) THEN
C
C        Width is just a single scalar, so we will use that.
C
         WIDTH=DVALUE
         NWID=1
      ELSE
C
C        Width is an array, so check its dimensions and map it.
C
         CALL DSA_MAP_WIDTH ('SPECT',1,'READ','FLOAT',WPTR,SLOT,
     :                       DSA_STATUS)
         NWID=NX
      END IF
C
C     Get dimensions of main data array and map it.
C
      CALL DSA_DATA_SIZE ('SPECT',5,NDIM,DIMS,NELM,DSA_STATUS)
      NSPECT=NELM/NX
      CALL DSA_MAP_DATA ('SPECT','READ','FLOAT',ZPTR,SLOT,DSA_STATUS)
C
C     Look for an error array.  If it exists, map it.
C
      CALL DSA_SEEK_ERRORS ('SPECT',ERRORS,DSA_STATUS)
      IF (ERRORS) THEN
         CALL DSA_MAP_ERRORS ('SPECT','READ','FLOAT',EPTR,
     :                        SLOT,DSA_STATUS)
      END IF
C
C     Determine the wavelength units being used for the data, and also
C     set the logarithmic binning flag.
C
      CALL DSA_GET_AXIS_INFO ('SPECT',1,1,XUNITS,1,DVALUE,DSA_STATUS)
      IF (DSA_STATUS.NE.0) GO TO 500   ! Error exit
      LOGW=(DVALUE.NE.0.0)
      NCHXU=ICH_LEN(XUNITS)
      STRING=XUNITS
      INVOKE=ICH_FOLD(STRING)
      IF ((STRING.NE.'MICRONS').AND.(STRING.NE.'ANGSTROMS')) THEN
         IF (STRING.EQ.' ') THEN
            CALL PAR_WRUSER(
     :         'WARNING - no input data units specified. However, '//
     :         'the scrunching can still be performed OK.',STATUS)
         ELSE
            CALL PAR_WRUSER(
     :         'WARNING - input data has wavelength in "'//
     :         XUNITS(:NCHXU)//
     :         '", which is not an expected unit. However, '//
     :         'the scrunching can still be performed OK.',STATUS)
         END IF
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
      CALL PAR_RDVAL('BINS',1.,50000.,FLOAT(NX),' ',VALUE)
      NBINR=VALUE
      IF (PAR_ABORT()) GO TO 500     ! User requested abort
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
      IF (PAR_ABORT()) GO TO 500     ! User requested abort
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
C     Make sure the output wavelength array is in ascending order.
C
      IF (DWEND.LE.DWSTART) THEN
         CALL PAR_WRUSER(
     :     'Output wavelength values must be in increasing order',
     :                                                      STATUS)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     See if data is to be treated as flux per unit wavelength.  We may
C     be able to guess at this from the units used, but we only use this
C     guess to set the default value for the keyword.
C
      CALL DSA_GET_DATA_INFO ('SPECT',1,ZUNITS,0,DVALUE,DSA_STATUS)
      IF (DSA_STATUS.NE.0) GO TO 500   ! Error exit
      DEFDEN=.FALSE.
      CALL PAR_WRUSER(' ',STATUS)
      INVOKE=ICH_CLEAN(ZUNITS)
      IF (ZUNITS.EQ.' ') THEN
         CALL PAR_WRUSER('No units specified for input data',STATUS)
      ELSE
         STRING='Data is in units of '//ZUNITS
         CALL PAR_WRUSER(STRING(:ICH_LEN(STRING)),STATUS)
      END IF
      CALL PAR_WRUSER(' ',STATUS)
      INVOKE=ICH_FOLD(ZUNITS)
      IF (INDEX(ZUNITS,'AB ').NE.0) DEFDEN=.TRUE.
      IF (INDEX(ZUNITS,'MJY').NE.0) DEFDEN=.TRUE.
      IF (INDEX(ZUNITS,'JANSKY').NE.0) DEFDEN=.TRUE.
      IF (INDEX(ZUNITS,'/A').NE.0) DEFDEN=.TRUE.
C
C     See how the user wants it treated.
C
      CALL PAR_RDKEY('DENSITY',DEFDEN,FLUXDEN)
C
C     See what order is to be used for local interpolation
C
      CALL PAR_RDVAL('INORDER',0.,2.,2.,' ',VALUE)
      IF (PAR_ABORT()) GO TO 500     ! User requested abort
      INTERP=VALUE
C
C     See if there is any indication that the input spectrum is
C     binned on a log scale - it really shouldn't be, because if it
C     is it's about to be scrunched twice.
C
      IF (LOGW) THEN
         CALL PAR_WRUSER(
     :     'WARNING- Input data is already scrunched to a log scale',
     :                                                        STATUS)
      END IF
C
C     Create the new output file, using the spectrum as a basis, with
C     the data arrays all reshaped.
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','SPECT',NO_DATA,NEW_FILE,
     :                                                    DSA_STATUS)
      DIMS(1)=NBINR
      CALL DSA_RESHAPE_DATA ('OUTPUT','SPECT',NDIM,DIMS,DSA_STATUS)
      CALL DSA_RESHAPE_AXIS ('OUTPUT',1,'SPECT',1,1,NBINR,DSA_STATUS)
C
C     Set LOG flag in output data
C
      IF (LOGWR) THEN
         DVALUE=1.0
         CALL DSA_SET_AXIS_INFO ('OUTPUT',1,1,XUNITS,1,DVALUE,
     :                                                     DSA_STATUS)
      END IF
C
C     Then map the output data arrays
C
      CALL DSA_MAP_AXIS_DATA ('OUTPUT',1,'WRITE','DOUBLE',RXPTR,SLOT,
     :                        DSA_STATUS)
      CALL DSA_MAP_DATA ('OUTPUT','WRITE','FLOAT',RZPTR,SLOT,
     :                   DSA_STATUS)
      IF (ERRORS) THEN
         CALL DSA_MAP_ERRORS ('OUTPUT','WRITE','FLOAT',REPTR,SLOT,
     :                        DSA_STATUS)
      END IF
C
C     Fill up the output wavelength array
C
      CALL FIG_WFILLD(DWSTART,DWEND,LOGWR,NBINR,%VAL(CNF_PVAL(RXPTR)))
C
C     A little information for the user about the input data..
C
      CALL PAR_WRUSER(' ',STATUS)
      STRING='Original wavelength range was '
      VALUE=GEN_ELEMD(%VAL(CNF_PVAL(XPTR)),1)
      STATUS=ICH_ENCODE(STRING,VALUE,31,3,NEXT)
      STRING(NEXT:)=' to '
      VALUE=GEN_ELEMD(%VAL(CNF_PVAL(XPTR)),NX)
      STATUS=ICH_ENCODE(STRING,VALUE,NEXT+4,3,NEXT)
      STRING(NEXT:)=' '//XUNITS
      CALL PAR_WRUSER(STRING,STATUS)
      CALL PAR_WRUSER(' ',STATUS)
C
C     Get a workspace array
C
      CALL DSA_GET_WORK_ARRAY (NBINR,'FLOAT',WKPTR,SLOT,DSA_STATUS)
      IF (DSA_STATUS.NE.0) GO TO 500    ! Error exit
C
C     Now we're finally in a position to perform the scrunch..
C     Note that the call has to be different for the cases where we have
C     a width array held in dynamic memory and where we have a single
C     width value held in WIDTH.
C
      PISNEW = .FALSE.
      PISNRZ = .FALSE.
      PISNE = .FALSE.
      PISNRE = .FALSE.
      PISNX = .FALSE.
      PISNW = .FALSE.
      DO ISPECT=1,NSPECT
         IF (SINGLE) THEN
            CALL FIG_FREBIN(%VAL(CNF_PVAL(ZPTR)),NX,NBINR,
     :                      %VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(RXPTR)),
     :                      NWID,WIDTH,ERRORS,%VAL(CNF_PVAL(EPTR)),
     :                      LOGWR,INTERP,FLUXDEN,%VAL(CNF_PVAL(WKPTR)),
     :                      %VAL(CNF_PVAL(RZPTR)),%VAL(CNF_PVAL(REPTR)))
         ELSE
            CALL FIG_FREBIN(%VAL(CNF_PVAL(ZPTR)),NX,NBINR,
     :                      %VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(RXPTR)),
     :                      NWID,%VAL(CNF_PVAL(WPTR)),ERRORS,
     :                      %VAL(CNF_PVAL(EPTR)),LOGWR,INTERP,FLUXDEN,
     :                      %VAL(CNF_PVAL(WKPTR)),%VAL(CNF_PVAL(RZPTR)),
     :                      %VAL(CNF_PVAL(REPTR)))
         END IF

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

         IF (ERRORS) THEN
            CALL DYN_INCAD(EPTR,'FLOAT',NX,TPTR,ISNEWE,STATUS)
            IF (ISNEWE) CALL CNF_UNREGP(EPTR)
            EPTR=TPTR
            PISNE = ISNEWE

            CALL DYN_INCAD(REPTR,'FLOAT',NBINR,TPTR,ISNRE,STATUS)
            IF (ISNRE) CALL CNF_UNREGP(REPTR)
            REPTR=TPTR
            PISNRE = ISNRE
         END IF

         IF (NXSPECT.GT.1) THEN
            CALL DYN_INCAD(XPTR,'DOUBLE',NX,TPTR,ISNEWX,STATUS)
            IF (ISNEWX) CALL CNF_UNREGP(XPTR)
            XPTR=TPTR
            PISNX = ISNEWX

            CALL DYN_INCAD(WPTR,'FLOAT',NX,TPTR,ISNEWW,STATUS)
            IF (ISNEWW) CALL CNF_UNREGP(WPTR)
            WPTR=TPTR
            PISNW = ISNEWW
         END IF
      END DO
      IF (ISNEW) CALL CNF_UNREGP(ZPTR)
      IF (ISNRZ) CALL CNF_UNREGP(RZPTR)
      IF (ERRORS) THEN
         IF (ISNEWE) CALL CNF_UNREGP(EPTR)
         IF (ISNRE)  CALL CNF_UNREGP(REPTR)
      END IF
      IF (NXSPECT.GT.1) THEN
         IF (ISNEWX)  CALL CNF_UNREGP(XPTR)
         IF (ISNEWW) CALL CNF_UNREGP(WPTR)
      END IF
C
C     In 2D, explain how the X data is being used.
C
      IF (NSPECT.GT.1) THEN
         CALL PAR_WRUSER('2-dimensional data array scrunched using',
     :                   STATUS)
         IF (NXSPECT.GT.1) THEN
            CALL PAR_WRUSER('a 2-dimensional wavelength array,',STATUS)
         ELSE
            CALL PAR_WRUSER('a single wavelength array,',STATUS)
         END IF
         IF (SINGLE) THEN
            CALL PAR_WRUSER('and a constant width value.',STATUS)
         ELSE IF (NXSPECT.EQ.1) THEN
            CALL PAR_WRUSER('and a single width array.',STATUS)
         ELSE
            CALL PAR_WRUSER('a 2-dimensional width array.',STATUS)
         END IF
         CALL PAR_WRUSER(' ',STATUS)
      END IF
C
C     And something about the output data and set the SCRUNCH_INC and
C     SCRUNCH_END variables.
C
      STRING='Data rebinned into '
      STATUS=ICH_ENCODE(STRING,REAL(NBINR),20,0,NEXT)
      STRING(NEXT:)=' bins from '
      VALUE=GEN_ELEMD(%VAL(CNF_PVAL(RXPTR)),1)
      STATUS=ICH_ENCODE(STRING,VALUE,NEXT+11,2,NEXT)
      STRING(NEXT:)=' to '
      VALUE=GEN_ELEMD(%VAL(CNF_PVAL(RXPTR)),NBINR)
      STATUS=ICH_ENCODE(STRING,VALUE,NEXT+4,2,NEXT)
      IF (LOGWR) STRING(NEXT:)=' (on a log scale)'
      CALL PAR_WRUSER(STRING,STATUS)
      CALL VAR_SETNUM('SCRUNCH_END',0,0,VALUE,STATUS)
      STRING='Giving a wavelength increment of '
      IF (LOGWR) THEN
         DELTA=(LOG(DWEND)-LOG(DWSTART))/(NBINR-1)
         VALUE=C*(EXP(DELTA)-1)
      ELSE
         VALUE=(DWEND-DWSTART)/(NBINR-1)
      END IF
      STRING(34:)=ICH_CF(VALUE)
      NEXT=ICH_LEN(STRING)+1
      IF (LOGWR) THEN
         STRING(NEXT:)=' Km/sec'
      ELSE
         STRING(NEXT:)=' '//XUNITS
      END IF
      CALL PAR_WRUSER(STRING,STATUS)
      CALL VAR_SETNUM('SCRUNCH_INC',0,0,VALUE,STATUS)
      CALL PAR_WRUSER(' ',STATUS)
C
C     Now tidy up
C
  500 CONTINUE
      CALL DSA_CLOSE(DSA_STATUS)
C
      IF (FAULT) CALL FIG_SETERR
C
      END
