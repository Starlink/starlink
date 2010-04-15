C+
      SUBROUTINE ISCRUNCH
C
C     I S C R U N C H      /      I S C R U N I
C
C     Applies the set of polynomial fits determind by IARC to
C     an image, re-binning each cross-section of the image to
C     either a linear or logarithmic wavelength scale.  ISCRUNCH
C     uses the results from a single 2D arc fit as performed by
C     IARC to get the channel/wavelength relation.  ISCRUNI uses
C     two such fits, and performs a linear interpolation between
C     the two.
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
C     IMAGE      (Character) Image to be scrunched.
C     FILE       (Character) File containing results of 2D arc fit
C                as produced by IARC.  If no extension supplied,
C                .IAR is assumed.
C     FILE2      (Character) ISCRUNI only. Second file containing
C                IARC results.
C     FRACTION   (Numeric) ISCRUNI only.  Controls the interpolation
C                between the two sets of wavelength information.
C                Value used=
C                    (File value)+(File2 value - File value)*FRACTION
C     WSTART     (Numeric) The wavelength of the CENTER of the first
C                bin of the resulting scrunched spectrum.
C     WEND       (Numeric) The wavelength of the CENTER of the final
C                bin of the resulting scrunched spectrum.  If WEND is
C                less than WSTART, then SCRUNCH assumes that it is the
C                increment rather than the final value that is being
C                specified.  If the scrunch is logarithmic and WSTART
C                is greater than WEND, SCRUNCH assumes that the WEND
C                value represents a velocity in km/sec.  These
C                assumptions can be controlled directly by the keywords
C                INCREMENT and FINAL, if they will not give the desired
C                effect.
C     BINS       (Numeric) The number of bins for the resulting
C                spectrum.
C     OUTPUT     (Character) Name of resulting image.  Note that an
C                image cannot be scrunched into itself, so a new output
C                file will always be created.
C
C     Command keywords -
C
C     LOG        Bin into logarithmic wavelength bins.
C     DENSITY    Treat input data as being in units of flux per unit
C                wavelength.
C     LINEAR     Use linear interpolation when rebinning.
C     QUAD       Use quadratic interpolation when rebinning.
C     INCREMENT  WEND is an increment value, even though it is > WSTART.
C     FINAL      WEND is a final value, even though it is < WSTART.
C
C     User variables - None
C
C                                         KS / CIT 22nd June 1984
C     Modified:
C
C     20th Dec 1984 KS/AAO Now creates an AXIS(1) structure if one
C                   doesn't exist.  Bug causing 2D AXIS(1) arrays to be
C                   created now fixed.
C     13th Aug 1985 KS/AAO Bug in 'LOG' mode fixed.  AXIS(1) units and
C                   label now generated in output file.
C     30th Mar 1987 KS/AAO Now works internally in double precision,
C                   to improve results at high dispersion.  Use of WEND
C                   as an increment value introduced, along with
C                   INCREMENT and FINAL keywords - same as for SCRUNCH.
C                   Keywords MEAN and FLUX (always confusing) replaced
C                   by DENSITY - which program now tries to guess at -
C                   this is the same scheme as that used by FSCRUNCH.
C     12th Sep 1989 JM / RAL. Modified to use DSA_ routines.  Dynamic-
C                   memory handling changed to use DYN_ routines.
C     23rd Nov 1989 KS/AAO. Long term bug in ISCRUNI (was using same
C                   file for both coefficient sets) fixed.  Introduced
C                   use of DYN_INCREMENT to tidy up dynamic-memory
C                   handling slightly. FIG_RD2DRC removed from this file
C                   - now in separate library.  PAR_ABORT calls
C                   scattered about the code as well.
C     17th Dec 1990 KS/AAO. Somewhere, the speed of light had got lost!
C                   Now set to the correct value.  Also, SJM's bug fix
C                   for the declaration of GEN_EPOLYD incorporated in
C                   FIG_WGEN/2.
C     23nd Sep 1992 HME / UoE, Starlink.  INCLUDE changed. Lowercase
C                   extension .iar.
C     28th Jul 1993 HME / UoE, Starlink.  Increase declared length of
C                   file name(s) to 132. Disuse STR$UPCASE.
C     10th Jan 1995 HME / UoE, Starlink.  Change output map access from
C                   update to write.  In the case of logarithmic
C                   binning, set the output axis flags accordingly.
C     2005 June 15  MJC / Starlink  Use CNF_PVAL for pointers to
C                   mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER DSA_TYPESIZE
      INTEGER ICH_CLEAN
      INTEGER ICH_LEN
      INTEGER ICH_FOLD
      LOGICAL GEN_EXIST
      LOGICAL PAR_ABORT
      LOGICAL PAR_GIVEN
C
C     Local variables
C
      INTEGER   BYTES            ! Size of wavelength array in bytes
      DOUBLE PRECISION C         ! Speed of light
      CHARACTER CITEMS(2)*32     ! Axis character items retrieved
      CHARACTER COMMAND*8        ! Figaro command name
      INTEGER   CPTR1            ! Dynamic-memory pointer for workspace
      INTEGER   CPTR2            ! Dynamic-memory pointer for workspace
      LOGICAL   DEFDEN           ! Assume data units flux/unit
                                 ! wavelength?
      DOUBLE PRECISION DELTA     ! Wavelength increment
      DOUBLE PRECISION DWEND     ! Final wavelength
      DOUBLE PRECISION DWSTART     ! Start wavelength
      INTEGER   DIMS(2)          ! Image dimensions
      LOGICAL   FAULT            ! True if non-DSA fault occurs
      CHARACTER FILE*132         ! File containing IARC results
      CHARACTER FILE2*132        ! Second file containing IARC results
                                 ! (ISCRUNI)
      LOGICAL   FINAL            ! True if  WEND is a final value
      LOGICAL   FLUX             ! Conserve flux?
      LOGICAL   FLUXDEN          ! Mean value of data to be conserved?
      REAL      FRACT            ! Fractional weight give to first .IAR
                                 ! file
      INTEGER   FSTAT            ! Status for FIG routines
      INTEGER   I                ! Loop variable
      INTEGER   IBRACK           ! Used to format strings
      INTEGER   IDOT             ! Used to format strings
      INTEGER   IMPTR            ! Dynamic-memory pointer for image data
      LOGICAL   INCREM           ! If true,  WEND is an increment value
      INTEGER   INVOKE           ! Used to format character strings
      INTEGER   IRPT             ! No of .IAR files used
      LOGICAL   ISNEWI           ! Is IMPTR address new to CNF?
      LOGICAL   ISNEWO           ! Is OUPTR address new to CNF?
      INTEGER   IY               ! Loop variable
      LOGICAL   LINEAR           ! Use linear interp.for rebinning?
      LOGICAL   LOGW             ! Wavelengths in WAVES are logarithmic?
      LOGICAL   LOGWR            ! Data are rebinned logarithmically?
      INTEGER   IMODE            ! Selects mode of transfer for input
                                 ! bins
      INTEGER   IQUAD            ! Equals 0 for lin. interp. non-zero
                                 ! for quadratic
      INTEGER   NADD             ! No. of input bins added to form one
                                 ! output bin
      CHARACTER NAME*132         ! Filename
      INTEGER   NBINR            ! Number of elements (bins) in rebinned
                                 ! data
      INTEGER   NCITEMS          ! Number of axis character items
                                 ! retrieved
      INTEGER   NDIM             ! Number of image dimensions
      INTEGER   NELM             ! Number of elements in image - ignored
      INTEGER   NELEMD           ! Number of 'DOUBLE' workspace array
                                 ! elements
      DOUBLE PRECISION NITEMS(1) ! Axis numeric items retrieved
      INTEGER   NNITEMS          ! Number of axis numeric items
                                 ! retrieved
      INTEGER   NX               ! First dimension of image
      INTEGER   NY               ! Second dimension of image
      INTEGER   OUPTR            ! Dynamic-memory pointer for OUTPUT
                                 ! data
      INTEGER   OXPTR            ! Dynamic-memory pointer for axis data
      CHARACTER PNAME*6          ! Parameter name to get file name
      LOGICAL   PISNI            ! Previous CNF IMPTR pointer new?
      LOGICAL   PISNO            ! Previous CNF OUPTR pointer new?
      INTEGER   PSTAT            ! Status for PAR routines
      LOGICAL   QUAD             ! Use quadratic interp.for rebinning?
      REAL      RESET            ! Reset value for real parameter
      LOGICAL   SCRUNI           ! True if Figaro command is ISCRUNI
      INTEGER   SLOT             ! Slot number for mapped data - ignored
      REAL      SSKEW            ! No. of bins the input array is to be
                                 ! shifted
      INTEGER   STATUS           ! Running status for DSA routines
      CHARACTER STRING*64        ! Used to format user messages
      INTEGER   TPTR             ! Temporary dynamic mem pointer
      CHARACTER TYPE*16          ! Data type
      REAL      VALUE            ! Work variable
      REAL      WEND             ! End wavelength
      INTEGER   WIPTR            ! Dynamic-memory pointer for workspace
      REAL      WMAX             ! Maximum wavelength of first .IAR file
      REAL      WMAX2            ! Maximum wavelength of second .IAR
                                 ! file
      REAL      WMIN             ! Minimum wavelength of first .IAR file
      REAL      WMIN2            ! Minimum wavelength of second .IAR
                                 ! file
      INTEGER   WOPTR            ! Dynamic-memory pointer for workspace
      REAL      WSTART           ! Start wavelength
      CHARACTER ZUNITS*32        ! Data units
C
C     Real value limits
C
      REAL FMAX, FMIN
      PARAMETER (FMAX=1.7E38, FMIN=-1.7E38)
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)
C
C     Velocity of light in Km/sec
C
      DATA C/299792.458/
C
C     Initial values
C
      STATUS=0
      NCITEMS=2
      NNITEMS=0
      FAULT=.FALSE.
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
C
C     Open IMAGE file
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
C
C     Get size of data in IMAGE
C
      CALL DSA_DATA_SIZE ('IMAGE',2,NDIM,DIMS,NELM,STATUS)

      IF (STATUS.NE.0)GOTO 500
      IF (NDIM.EQ.1) THEN
         NY=1
         NX=DIMS(1)
      ELSE IF (NDIM.EQ.2) THEN
         NY=DIMS(2)
         NX=DIMS(1)
      END IF
C
C     Find which command we are servicing - ISCRUNCH or ISCRUNI
C
      CALL PAR_COMMAND(COMMAND)
      SCRUNI=COMMAND.EQ.'ISCRUNI'
C
C     Get names of arcfit result files.  At this point we quickly
C     check that they exist, but don't need to read them yet.
C
      PNAME='FILE'
      IF (SCRUNI) THEN
         IRPT=2
      ELSE
         IRPT=1
      END IF
      DO I=1,IRPT
         CALL PAR_RDCHAR(PNAME,' ',NAME)
         IF (PAR_ABORT()) GO TO 500
         IBRACK=MAX(1,INDEX(NAME,']'))
         IDOT=INDEX(NAME(IBRACK:),'.')
         IF (IDOT.EQ.0) NAME=NAME(:ICH_LEN(NAME))//'.iar'
         IF (.NOT.GEN_EXIST(NAME)) THEN
            CALL PAR_WRUSER(NAME(:ICH_LEN(NAME))//' does not exist',
     :                                                        PSTAT)
            FAULT=.TRUE.
            GO TO 500
         END IF
         IF (I.EQ.1) THEN
            FILE=NAME
            PNAME='FILE2'
         ELSE
            FILE2=NAME
         END IF
      END DO
C
C     If needed, get value of interpolation fraction
C
      IF (SCRUNI) CALL PAR_RDVAL('FRACTION',0.,1.,.5,' ',FRACT)
C
C     Get the number of bins in the final spectra (NBINR)
C
      CALL PAR_RDVAL('BINS',1.,50000.,FLOAT(NX),' ',VALUE)
      NBINR=VALUE
      IF (PAR_ABORT()) GO TO 500
C
C     Get workspace.  Need two arrays to hold wavelength values - one
C     input and one output, input NX elements, output NBINR elements.
C     Also need one (for ISCRUNCH) or two (for ISCRUNI) coefficient
C     arrays, each NY*11.
C
      NELEMD=NX+NBINR+NY*11
      IF (SCRUNI) NELEMD=NELEMD+NY*11
      CALL DSA_GET_WORK_ARRAY(NX,'DOUBLE',WIPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NBINR,'DOUBLE',WOPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NY*11,'DOUBLE',CPTR1,SLOT,STATUS)
      IF (SCRUNI)
     :  CALL DSA_GET_WORK_ARRAY(NY*11,'DOUBLE',CPTR2,SLOT,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Read the coefficients from the fit files produced by IARC
C
      CALL FIG_RD2DRC(FILE,NX,NY,%VAL(CNF_PVAL(CPTR1)),WMIN,WMAX,FSTAT)
      IF (FSTAT.NE.0) THEN
         FAULT=.TRUE.
         GO TO 500
      END IF
      IF (SCRUNI) THEN
         CALL FIG_RD2DRC(FILE2,NX,NY,%VAL(CNF_PVAL(CPTR2)),WMIN2,WMAX2,
     :                   FSTAT)
         IF (FSTAT.NE.0) THEN
            FAULT=.TRUE.
            GO TO 500
         END IF
         WMIN=MIN(WMIN,WMIN2)
         WMAX=MAX(WMAX,WMAX2)
      END IF
C
C     Get scrunch parameters - start & end wavelength.
C     (The reset values are just the current limits rounded to 10
C     angstroms - probably not very useful).  Also find if data
C     is to be binned logarithmically.
C
      CALL PAR_RDKEY('LOG',.FALSE.,LOGWR)
      RESET=(INT(WMIN)/10)*10
      CALL PAR_RDVAL('WSTART',FMIN,FMAX,RESET,' ',WSTART)
      RESET=(INT(WMAX)/10+1)*10
      CALL PAR_RDVAL('WEND',FMIN,FMAX,RESET,' ',WEND)
      IF (PAR_ABORT()) GO TO 500
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
      IF (PAR_ABORT()) GO TO 500
C
C     If it is an incremental value, calculate the final wavelength
C     Otherwise, calculate the incremental value, which we will need
C     to determine the precision needed for the output array.
C
      IF (INCREM) THEN
         DELTA=DWEND
         IF (.NOT.LOGWR) THEN
            DWEND=DWSTART+(NBINR-1)*DELTA
         ELSE
            DWEND=EXP(LOG(DWSTART)+(NBINR-1)*LOG(DELTA/C+1))
         END IF
      ELSE
         DELTA=(DWEND-DWSTART)/(NBINR-1)
      END IF
C
C     See if data is to be treated as flux per unit wavelength.  We may
C     be able to guess at this from the units used, but we only use this
C     guess to set the default value for the keyword.
C
      ZUNITS=' '

      CALL DSA_GET_DATA_INFO('IMAGE',NCITEMS,CITEMS,NNITEMS,NITEMS,
     :                        STATUS)
      ZUNITS=CITEMS(1)
      DEFDEN=.FALSE.
      CALL PAR_WRUSER(' ',PSTAT)
      INVOKE=ICH_CLEAN(ZUNITS)
      IF (ZUNITS.EQ.' ') THEN
         CALL PAR_WRUSER('No units specified for input data',PSTAT)
      ELSE
         STRING='Data is in units of '//ZUNITS
         CALL PAR_WRUSER(STRING(:ICH_LEN(STRING)),PSTAT)
      END IF
      CALL PAR_WRUSER(' ',PSTAT)
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
C     See if quadratic or linear interpolation is to be used.
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
               FAULT=.TRUE.
               GO TO 500
            END IF
         END IF
         QUAD=.NOT.LINEAR
      ELSE
         CALL PAR_RDKEY('QUAD',.TRUE.,QUAD)
         LINEAR=.NOT.QUAD
      END IF
      IF (PAR_ABORT()) GO TO 500
C
C     Create the output structure based on IMAGE but without the data
C     and axis structure.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',NO_DATA,NEW_FILE,STATUS)
C
C     Create the X and Z output data arrays - note that DIMS(2) will
C     still be NY, for a 2D image, and NDIM will still be 1 or 2.
C     Precision used for output array depends on relative values of
C     the average wavelength and the incremental wavelength.
C
      DIMS(1)=NBINR
      CALL DSA_RESHAPE_DATA('OUTPUT','IMAGE',NDIM,DIMS,STATUS)
      CALL DSA_RESHAPE_AXIS('OUTPUT',1,'IMAGE',1,1,DIMS,STATUS)
      IF(STATUS.NE.0)GOTO 500
      IF ((ABS(DWEND+DWSTART)*0.5/ABS(DELTA)).GT.50000) THEN
         TYPE='DOUBLE'
      ELSE
         TYPE='FLOAT'
      END IF
      CALL DSA_COERCE_AXIS_DATA('OUTPUT',1,TYPE,1,DIMS,STATUS)
      CALL DSA_MAP_AXIS_DATA('OUTPUT',1,'WRITE','DOUBLE',OXPTR,SLOT,
     :                        STATUS)
C
C     Map the input and output arrays
C
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',IMPTR,SLOT,STATUS)
      CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',OUPTR,SLOT,STATUS)
      IF(STATUS.NE.0)GOTO 500
C
C     Now generate the wavelength values for the output data
C
      CALL FIG_WFILLD(DWSTART,DWEND,LOGWR,NBINR,%VAL(CNF_PVAL(WOPTR)))
C
C     Loop through each cross-section of the image, generating the
C     input wavelength array and then scrunching the data accordingly.
C     Note that the image pointers have to be incremented after each
C     cross-section.
C
      IMODE=1
      IQUAD=0
      IF (QUAD) IQUAD=1
      NADD=1
      SSKEW=0.
      FLUX=.NOT.FLUXDEN
      PISNI = .FALSE.
      PISNO = .FALSE.
      DO IY=1,NY
         IF (SCRUNI) THEN
            CALL FIG_WGEN2(IY,NX,NY,FRACT,%VAL(CNF_PVAL(CPTR1)),
     :                     %VAL(CNF_PVAL(CPTR2)),%VAL(CNF_PVAL(WIPTR)))
         ELSE
            CALL FIG_WGEN(IY,NX,NY,%VAL(CNF_PVAL(CPTR1)),
     :                    %VAL(CNF_PVAL(WIPTR)))
         END IF
         CALL FIG_REBIND(IMODE,IQUAD,%VAL(CNF_PVAL(IMPTR)),NX,
     :                   %VAL(CNF_PVAL(OUPTR)),NBINR,NADD,SSKEW,FLUX,
     :                   %VAL(CNF_PVAL(WIPTR)),%VAL(CNF_PVAL(WOPTR)),
     :                   LOGW,LOGWR)

         CALL DYN_INCAD(IMPTR,'FLOAT',NX,TPTR,ISNEWI,STATUS)
         IF (PISNI) CALL CNF_UNREGP(IMPTR)
         IMPTR = TPTR
         PISNI = ISNEWI

         CALL DYN_INCAD(OUPTR,'FLOAT',NBINR,TPTR,ISNEWO,STATUS)
         IF (PISNO) CALL CNF_UNREGP(OUPTR)
         OUPTR = TPTR
         PISNO = ISNEWO
      END DO
      IF (ISNEWI) CALL CNF_UNREGP(IMPTR)
      IF (ISNEWO) CALL CNF_UNREGP(OUPTR)
C
C     Now write the output wavelengths into the output X array.
C
      BYTES=NBINR*DSA_TYPESIZE('DOUBLE',STATUS)
      CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(WOPTR)),%VAL(CNF_PVAL(OXPTR)))
C
C     Change the x- label and units
C     If binning was logarithmic, also set the numeric items accordingly
C
      CITEMS(1)='Angstroms'
      CITEMS(2)='Wavelength'
      IF (LOGWR) THEN
         NNITEMS=1
         NITEMS(1)=1D0
      END IF
      CALL DSA_SET_AXIS_INFO('OUTPUT',1,NCITEMS,CITEMS,NNITEMS,NITEMS,
     :                       STATUS)
  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)
      IF (FAULT) CALL FIG_SETERR
C
      END
C+
      SUBROUTINE FIG_WGEN(IY,NX,NY,COEFFS,WARRAY)
C
C     F I G _ W G E N
C
C     Generates an array of wavelength values from a single set
C     of 2D arc coefficients.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) IY     (Integer) The set of coefficients to be used
C     (>) NX     (Integer) The number of wavelength values to be
C                generated - ie the X dimension of the image the
C                coefficients apply to.
C     (>) NY     (Integer) The number of sets of coefficients - ie
C                the Y dimension of the image the coefficients apply to.
C     (>) COEFFS (Double precision COEFFS(11,NY)) The coefficients.
C                For each row, the constant term is the last non-zero
C                term.
C     (<) WARRAY (Double precision array WARRAY(NX)) The resulting
C                wavelengths.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     GEN_EPOLYD (GEN_ package) Evaluate double precision polynomial
C
C                                            KS / CIT 24th June 1984
C     Modified:
C
C     30th March 1987.  KS/AAO.  WARRAY made double precision.
C     17th Dec 1990.    KS/AAO.  GEN_EPOLYD now correctly declared as
C                       double (apparently it was OK on the VAX, but not
C                       on a SUN.  Thanks, SJM.)
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IY,NX,NY
      DOUBLE PRECISION COEFFS(11,NY), WARRAY(NX)
C
C     Functions
C
      DOUBLE PRECISION GEN_EPOLYD
C
C     Local variables
C
      INTEGER I, NCOEFF
C
C     Find out how many coefficients are being used, then
C     evaluate polynomial for all the elements of the array
C
      NCOEFF=1
      DO I=2,11
         IF (COEFFS(I,IY).NE.0.) NCOEFF=I
      END DO
      DO I=1,NX
         WARRAY(I)=GEN_EPOLYD(DBLE(I),COEFFS(1,IY),NCOEFF)
      END DO
C
      END
C+
      SUBROUTINE FIG_WGEN2(IY,NX,NY,FRACT,COEFFS,COEFFS2,WARRAY)
C
C     F I G _ W G E N 2
C
C     Generates an array of wavelength values from two sets of 2D
C     arc coefficients.  For each element, the value from the first
C     set of coeffients (Val1) and from the second set (Val2) is
C     calculated, and the value used is given by
C     Value = Val1 + (Val2 - Val1) * FRACT
C     where FRACT is the parameter passed to this routine.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) IY      (Integer) The set of coefficients to be used
C     (>) NX      (Integer) The number of wavelength values to be
C                 generated - ie the X dimension of the image the
C                 coefficients apply to.
C     (>) NY      (Integer) The number of sets of coefficients - ie
C                 the Y dimension of the image the coefficients apply to.
C     (>) FRACT   (Real) The value used to control the interpolation
C                 between the two sets of coefficients.  See above.
C     (>) COEFFS  (Double precision COEFFS(11,NY)) The first set of
C                 coefficients.  For each row, the constant term is the
C                 last non-zero term.
C     (>) COEFFS2 (Double precision COEFFS2(11,NY)) The second set of
C                 coefficients.  Constant term as for COEFFS.
C     (<) WARRAY  (Double precision array WARRAY(NX)) The resulting
C                 wavelengths.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     GEN_EPOLYD (GEN_ package) Evaluate double precision polynomial
C
C                                            KS / CIT 24th June 1984
C     Modified:
C
C     30th March 1987.  KS/AAO.  WARRAY made double precision.
C     17th Dec 1990.    KS/AAO.  GEN_EPOLYD now correctly declared as
C                       double (apparently it was OK on the VAX, but not
C                       on a SUN.  Thanks, SJM.)
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IY,NX,NY
      REAL    FRACT
      DOUBLE PRECISION COEFFS(11,NY), COEFFS2(11,NY), WARRAY(NX)
C
C     Functions
C
      DOUBLE PRECISION GEN_EPOLYD
C
C     Local variables
C
      INTEGER I, NCOEFF, NCOEFF2
      DOUBLE PRECISION VAL1, VAL2
C
C     Find out how many coefficients are being used, then
C     evaluate polynomial for all the elements of the array
C
      NCOEFF=1
      NCOEFF2=1
      DO I=2,11
         IF (COEFFS(I,IY).NE.0.) NCOEFF=I
         IF (COEFFS2(I,IY).NE.0.) NCOEFF2=I
      END DO
      DO I=1,NX
         VAL1=GEN_EPOLYD(DBLE(I),COEFFS(1,IY),NCOEFF)
         VAL2=GEN_EPOLYD(DBLE(I),COEFFS2(1,IY),NCOEFF2)
         WARRAY(I)=VAL1+(VAL2-VAL1)*FRACT
      END DO
C
      END
