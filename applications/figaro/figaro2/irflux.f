C+
      SUBROUTINE IRFLUX
C
C     I R F L U X
C
C     Flux calibrates an IR spectrum using a black body model for a
C     standard star
C
C     Command parameters -
C
C     SPECTRUM  The name of the structure containing the first image.
C
C     STANDARD  The name of the structure containing the second
C               image data.
C
C     TEMP      The temperature of the black body to be used.
C
C     CALTYPE   The type of calibration data. A single character as
C               follows:
C                  'J','H','K','L','M' - magnitude in a standard band
C                  'F' - Flux at specified wavelength
C
C     MAG       The magnitude of the standard used.
C
C     FLUX      The flux in mJy of the standard.
C
C     WAVE      The wavelength at which the flux is specified.
C
C     OUTPUT    The name of the result of the operation.  This can
C               be the same as for SPECTRUM.  If not, a new structure
C               is created, with everything but the data a direct
C               copy of the input.
C
C                                          JAB / JAC  19th Sep 1990
C     Modified:
C
C     25th Feb 1990  JAB / JAC.  Adapted from FIGSFLUX.
C     19th Sep 1990  JAB / JAC.  Add additional methods of specifying
C                                    calibration.
C     30th Nov 1990  JAB / JAC.  Correct Flux zero points and effective
C                                    wavelengths.
C      8th Dec 1990  JAB / JAC.  Use data quality.
C     15th Feb 1991  HME / UoE.  Reshuffle DSA_ calls. Map output for
C                    update rather than write. Remove FIG_ prefix.
C                    Revise (FIG_)FLUX subroutine. If SAME, output is
C                    model flux density, errors are zero, but bad pixels
C                    remain bad.
C     8th Mar. 1991  JMS / AAO. Added IRFLUX_ prefix to all subroutine
C                    names.
C     22nd Sep 1992  HME / UoE, Starlink.  TABs removed, INCLUDE
C                    changed. Lowercase file name. Don't tread
C                    LIB$GET_LUN as logical.
C     21st Jul 1993  HME / UoE, Starlink.  Use DSA_*_LU.
C     28th Jul 1993  HME / UoE, Starlink. Disuse STR$UPCASE.
C     29th Jul 1996  MJCL / Starlink, UCL.  Added PAR_ABORT checks.
C      5th Feb 1998  BLY / RAL, Starlink.  Added type definitions for
C                    ICH_ENCODE and ICH_FOLD functions, a type
C                    definition for INVOKE to keep the Linux compiler
C                    happy.
C      5th Mar 1998  ACD / UoE, Starlink.  Fixed a bug in the call to
C                    PAR_RDVAL to get variable FLAM where the default
C                    was incorrectly specified as INTEGER instead of
C                    REAL.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL DSA_SAME_DATA
      INTEGER DSA_TYPESIZE
      INTEGER ICH_ENCODE, ICH_FOLD
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
C
C     Local variables
C
      CHARACTER*1  CALTYPE       ! Type of calibration data
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      DOUBLE PRECISION  DUMMY    ! Dummy value
      LOGICAL      ERROR1        ! Spectrum has an error structure?
      LOGICAL      ERROR2        ! Standard has an error structure?
      INTEGER      ESPTR         ! Dynamic-memory pointer to standard
                                 ! errors
      INTEGER      ESSLOT        ! Map slot number of standard errors
      REAL         FLAM          ! Flux at calibration wavelength
      INTEGER      IGNORE        ! Used to pass ignorable status
      INTEGER      INVOKE        ! Dummy for ICH_ENCODE, ICH_FOLD
      REAL         KDEF          ! Default value for K magnitude
      REAL         MAG           ! Magnitude value of standard
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NP            ! Needed for ICH_ENCODE
      INTEGER      NX            ! Size of 1st dimension
      CHARACTER*64 OBJECT        ! Object name
      INTEGER      OEPTR         ! Dynamic-memory pointer to output
                                 ! errors
      INTEGER      OESLOT        ! Map slot number of output errors
      INTEGER      OPTR          ! Dynamic-memory pointer to data array
      INTEGER      OQPTR         ! Dynamic-memory pointer to output
                                 ! quality array
      INTEGER      OQSLOT        ! Map slot number of output quality
                                 ! array
      INTEGER      OSLOT         ! Map slot number of input data array
      LOGICAL      SAME          ! Standard is the same as the spectrum?
      INTEGER      SPTR          ! Dynamic-memory pointer to standard
                                 ! data array
      INTEGER      SSLOT         ! Map slot number of standard data
                                 ! array
      INTEGER      SQPTR         ! Dynamic-memory pointer to standard
                                 ! quality
      INTEGER      SQSLOT        ! Map slot number of standard quality
                                 ! array
      INTEGER      STATUS        ! Running status for DSA_ routines
      CHARACTER*12 STR           ! String buffer for K mag
      CHARACTER    STRINGS(2)*16 ! Units and label information for new
                                 ! data
      REAL         TEMP          ! Temperature of standard
      REAL         WAVE          ! Calibration wavelength
      INTEGER      XPTR          ! Dynamic-memory pointer to output
                                 ! x-axis data
      INTEGER      XSLOT         ! Map slot number for output x-axis
                                 ! data
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input name
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get standard name
C
      CALL DSA_INPUT('STAND','STANDARD',STATUS)
      SAME=DSA_SAME_DATA('SPECT','STAND',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE('SPECT',1,NDIM,DIMS,NX,STATUS)
C
C     Check dimensions of standard data are same as those of spectrum
C     Automatically allows for case where two spectra are the same
C
      CALL DSA_MATCH_SIZES('SPECT','STAND',STATUS)
C
C     Get Temperature of standard
C
      IF (STATUS.NE.0) GOTO 500
      CALL PAR_RDVAL('TEMP',0.0,30000.0,10000.0,' ',TEMP)
      IF ( PAR_ABORT() ) GO TO 500
C
C     Get magnitude of standard.  The program attempts to determine
C     the default value from the object name and a file of standards and
C     their magnitudes.
C
      CALL DSA_OBJECT_NAME ('STAND', OBJECT, STATUS)
      IF ((STATUS.EQ.0).AND.(OBJECT.NE.' ')) THEN
         CALL IRFLUX_KFLUX (OBJECT,KDEF,STATUS)
         IF (STATUS.EQ.0) THEN
             CALL PAR_SDVAL('KMAG',KDEF,IGNORE)
             INVOKE = ICH_ENCODE(STR,KDEF,1,2,NP)
             CALL PAR_WRUSER('Standard has K magnitude '//STR,STATUS)
         END IF
      END IF
      STATUS=0
C
C     Get Type of calibration data
C
      CALL PAR_RDCHAR('CALTYPE','K',CALTYPE)
      IF ( PAR_ABORT() ) GO TO 500
      INVOKE = ICH_FOLD(CALTYPE)
      IF (CALTYPE .EQ. 'J' .OR. CALTYPE .EQ. 'K' .OR.
     :    CALTYPE .EQ. 'H' .OR. CALTYPE .EQ. 'L' .OR.
     :    CALTYPE .EQ. 'M') THEN
          CALL PAR_RDVAL('MAG',-5.0,20.0,KDEF,' ',MAG)
          IF (CALTYPE .EQ. 'J') THEN
              FLAM = 164E4*10**(-0.4*MAG)
              WAVE = 1.20
          ELSE IF (CALTYPE .EQ. 'H') THEN
              FLAM = 103E4*10**(-0.4*MAG)
              WAVE = 1.64
          ELSE IF (CALTYPE .EQ. 'K') THEN
              FLAM = 65E4*10**(-0.4*MAG)
              WAVE = 2.2
          ELSE IF (CALTYPE .EQ. 'L') THEN
              FLAM = 25E4*10**(-0.4*MAG)
              WAVE = 3.8
          ELSE IF (CALTYPE .EQ. 'M') THEN
              FLAM = 17E4*10**(-0.4*MAG)
              WAVE = 4.8
          END IF
      ELSE IF (CALTYPE .EQ. 'F') THEN
          CALL PAR_RDVAL('FLUX',-1.0E30,1.0E30,1.0E0,' ',FLAM)
          CALL PAR_RDVAL('WAVE',1.0E-6,1.0E6,2.2E0,' ',WAVE)
      ELSE
          CALL PAR_WRUSER('CALTYPE must be J, H, K, L, M or F',STATUS)
          GO TO 500
      END IF
C
C     Get output structure.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Decide on quality handling.
C
      CALL DSA_USE_QUALITY( 'SPECT', STATUS )
      CALL DSA_USE_QUALITY( 'STAND', STATUS )
      CALL DSA_USE_QUALITY( 'OUTPUT', STATUS )
C
C     Dedide on error handling: OUTPUT should have or not have errors
C     depending on SPECT (that is provided for by the DSA_OUTPUT call.
C     If it has, STANDS errors either exist or are assumed to be zero.
C
      CALL DSA_SEEK_ERRORS('SPECT',ERROR1,STATUS)
      CALL DSA_SEEK_ERRORS('STAND',ERROR2,STATUS)
C
C     However, if SPECT and STAND are the same, the result is just the
C     model spectrum, which is perfect data. So write an error array
C     with zeroes and prevent the FLUX subroutine from propagating errors.
C
      IF ( SAME ) THEN
         CALL DSA_MAP_ERRORS( 'OUTPUT', 'WRITE', 'FLOAT', OEPTR,
     :                        OESLOT, STATUS )
         CALL GEN_FILL( NX*DSA_TYPESIZE('FLOAT',STATUS), 0,
     :                  %VAL(CNF_PVAL(OEPTR)) )
         ERROR1 = .FALSE.
      END IF
C
C     Map output data
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      CALL DSA_MAP_QUALITY('OUTPUT','UPDATE','BYTE',OQPTR,OQSLOT,STATUS)
      CALL DSA_SEEK_ERRORS('SPECT',ERROR1,STATUS)
      IF (ERROR1) THEN
         CALL DSA_MAP_ERRORS('OUTPUT','UPDATE','FLOAT',OEPTR,OESLOT,
     :                       STATUS)
      END IF
C
C     Map wavelength array
C
      CALL DSA_MAP_AXIS_DATA('OUTPUT',1,'READ','FLOAT',XPTR,XSLOT,
     :                       STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map standard data
C
      IF (.NOT.SAME) THEN
         CALL DSA_MAP_DATA('STAND','READ','FLOAT',SPTR,SSLOT,STATUS)
         CALL DSA_MAP_QUALITY('STAND','READ','BYTE',SQPTR,SQSLOT,
     :        STATUS)

         IF (ERROR1) THEN
C
C     Get an error array for the standard, will be zero if not found in
C     data file. Thus ERROR2 is not needed in FLUX.
C
            CALL DSA_MAP_ERRORS('STAND','READ','FLOAT',ESPTR,
     :                          ESSLOT,STATUS)
            IF ( .NOT. ERROR2 ) THEN
               CALL PAR_WRUSER('No error information on standard data',
     :                                                          IGNORE)
               CALL PAR_WRUSER('Will assume errors of zero',IGNORE)
            END IF
         END IF
      ELSE
         SPTR=OPTR
         ESPTR=OEPTR
         SQPTR=OQPTR
         ERROR2=ERROR1
      END IF
      ERROR2=ERROR1
      IF (STATUS.NE.0) GOTO 500
C
C     Operate on the quality arrays
C
      IF ( .NOT. SAME )
     :   CALL IRFLUX_IRFLUXQ(NX,%VAL(CNF_PVAL(OQPTR)),
     :                       %VAL(CNF_PVAL(SQPTR)))
C
C     Operate on the images
C
      CALL IRFLUX_FLUX(ERROR1,NX,FLAM,WAVE,TEMP,%VAL(CNF_PVAL(OPTR)),
     :                 %VAL(CNF_PVAL(OEPTR)),%VAL(CNF_PVAL(OQPTR)),
     :                 %VAL(CNF_PVAL(SPTR)),%VAL(CNF_PVAL(ESPTR)),
     :                 %VAL(CNF_PVAL(XPTR)))
C
C     Set the units and label for the output data
C
      STRINGS(1)='mJy'
      STRINGS(2)='Flux'
      CALL DSA_SET_DATA_INFO('OUTPUT',2,STRINGS,0,DUMMY,STATUS)
C
C     Close down everything
C
500   CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END

      SUBROUTINE IRFLUX_FLUX(ERROR1,N,FLAM,WAVE,TEMP,STAR,ESTAR,
     :    QUALITY,STAN,ESTAN,LAMBDA)
C
C     Divide a spectrum by a standard taking account of errors
C     and generating a flux calibrated result in mJy
C
C     ERROR1    (Logical) - True if the star spectrum has errors
C     N         (Integer) - Number of elements of the spectra
C     FLAM      (Real)    - Flux of standard at calibration wavelength
C     WAVE      (Real)    - Calibration wavelength
C     K         (Real)    - K magnitude of the standard
C     TEMP      (Real)    - Temperature of standard
C     STAR      (Real array STAR(N)) - The star spectrum
C     ESTAR     (Real array ESTAR(N)) - The errors on the star spectrum
C     QUALITY   (Byte array QUALITY(N)) - The output quality array
C     STAN      (Real array STAN(N)) - The standard spectrum
C     ESTAN     (Real array ESTAN(N)) - The errors on the standard
C     LAMBDA    (Real array LAMBDA(N)) - The wavelength array
C
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL ERROR1
      INTEGER N
      REAL FLAM,WAVE,TEMP
      REAL STAR(N), ESTAR(N), STAN(N), ESTAN(N), LAMBDA(N)
      BYTE QUALITY(N)
C
C     Local variables
C
      INTEGER I
      REAL F
C
C     Functions
C
      REAL IRFLUX_LFLUX


      DO 1 I = 1, N
         IF ( QUALITY(I) .EQ. 0 ) THEN
            F = IRFLUX_LFLUX(FLAM,WAVE,TEMP,LAMBDA(I))
            IF ( STAN(I) .NE. 0 ) THEN
C
C     Handle errors if required. Must be done before changing STAR.
C
               IF ( ERROR1 ) THEN
                  ESTAR(I) = F * SQRT(
     :               ESTAR(I)*ESTAR(I) / (STAN(I)*STAN(I))
     :             + ESTAN(I)*ESTAN(I) * STAR(I)*STAR(I)/STAN(I)**4 )
               END IF
C
C     Divide star by standard and convert to flux.
C
               STAR(I) = STAR(I) / STAN(I) * F
            ELSE
               QUALITY(I) = 1
            END IF
         END IF
    1 CONTINUE
      END

      REAL FUNCTION IRFLUX_LFLUX(FLAM,WAVE,TEMP,L)
C
C     Return the flux in mJy of a black body which has flux FLAM
C     at wavelength WAVE, temperature T.
C     Return the value for wavelength L microns
C
      REAL WAVE,FLAM,L,TEMP
      REAL F,LC,F22,L2
C
C     Calculate flux in mJy
C
      LC = L*1E-4
      L2 = WAVE*1E-4
      F = 1.0/(LC*LC*LC*(EXP(1.43883/(LC*TEMP))-1.))
      F22 = 1.0/(L2*L2*L2*(EXP(1.43883/(L2*TEMP))-1.))
C
C     Convert to mJy
C
      IRFLUX_LFLUX=(F*FLAM)/F22
      END

      SUBROUTINE IRFLUX_KFLUX (OBJECT,KDEF,STATUS)
C
C     I R F L U X _ K F L U X
C
C     Given the name of a standard star, attempts to match that to
C     one of the stars whose K magnitudes are held in a file
C     KMAGS.DAT   This file is searched for in the normal Figaro
C     directory sequence.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) OBJECT    (Character) The name of the object, from the
C                   standard structure.
C     (<) KDEF      (Real) The default K magnitude to be used - ie the
C                   K magnitude for the object found in the file.
C     (<) STATUS    (Integer) Status code. 0 => OK, a value was found.
C                   Non-zero codes indicate an error - an error message
C                   will have been output by this routine.
C
C     Common variables used - None
C
C     Subroutines / functions used -
C
C     FIG_OPFILE    Open file, searching Figaro directories for it.
C     GEN_FORTERR   Decode a Fortran error code
C     ICH_ENCODE    Encode a real number into a string
C     ICH_LEN       Position of last non-blank char in string
C     ICH_NUMBR     Decode a number from a string
C     ICH_VERIF     Position of first char not in specified list
C     ICH_WORD      Delimit word from string
C     DSA_FREE_LU   Release logical unit number
C     DSA_GET_LU    Obtain logical unit number
C     PAR_WRUSER    Output message to user
C     ICH_FOLD      Convert string to upper case
C
C                                            KS / AAO 11th Nov 1987
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      REAL    KDEF
      CHARACTER*(*) OBJECT
C
C     Functions
C
      INTEGER ICH_LEN, ICH_ENCODE, ICH_NUMBR, ICH_VERIF, ICH_WORD,
     :   ICH_FOLD
C
C     Local variables
C
      LOGICAL FOUND, GOTLU, LUOPEN, MORE
      INTEGER I, IGNORE, INVOKE, IST, LENF, LU, LWORD, NEXT, NPTR, POSN
      CHARACTER CHAR*1, FILENAME*64, LINE*64, NAME*32, STRING*80
      CHARACTER WORD*32
C
C     Initial values
C
      GOTLU=.FALSE.
      LUOPEN=.FALSE.
C
C     Don't bother if the object is blank.
C
      IF (OBJECT.EQ.' ') THEN
         STATUS=0
         GO TO 500
      END IF
C
C     Try to tidy up the object name - it will probably have been
C     entered by hand, so may have spaces, may be in upper or lower
C     case, may not have a letter prefix.
C
      CALL PAR_WRUSER(' ',STATUS)
      STRING = 'Standard name given as '//
     :                              OBJECT(:ICH_LEN(OBJECT))
      CALL PAR_WRUSER(STRING,STATUS)
      NPTR=0
      DO I=1,MIN(LEN(OBJECT),LEN(NAME))
         CHAR=OBJECT(I:I)
         IF (CHAR.NE.' ') THEN
            NPTR=NPTR+1
            NAME(NPTR:NPTR)=CHAR
         END IF
      END DO
      INVOKE=ICH_FOLD(NAME)
      IF ((NAME(1:1).GE.'0').AND.(NAME(1:1).LE.'9')) THEN
         NAME='BS'//NAME
         NPTR=NPTR+2
         CALL PAR_WRUSER('Will assume this is '//NAME(:NPTR),STATUS)
      END IF
C
C     Get a logical unit
C
      CALL DSA_GET_LU(LU,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER(
     :        'Unable to get logical unit to access flux file',STATUS)
         STATUS=1
         GO TO 500
      END IF
      GOTLU=.TRUE.
C
C     Now try to open the file
C
      CALL FIG_OPFILE ('kmags','dat',LU,STATUS)
      IF (STATUS.NE.0) THEN
         CALL GEN_FORTERR(STATUS,.FALSE.,STRING)
         CALL PAR_WRUSER(
     :     'Unable to open standard K-magnitude file KMAGS.DAT',STATUS)
         CALL PAR_WRUSER(STRING,STATUS)
         GO TO 500
      END IF
      LUOPEN=.TRUE.
      INQUIRE (LU,NAME=FILENAME,IOSTAT=IGNORE)
C
C     Now work through list of objects, trying to find a matching name
C
      FOUND=.FALSE.
      MORE=.TRUE.
      DO WHILE (MORE)
         READ (LU,'(A)',IOSTAT=STATUS) LINE
         IF (STATUS.LT.0) THEN
            MORE=.FALSE.
         ELSE IF (STATUS.GT.0) THEN
            CALL GEN_FORTERR(STATUS,.FALSE.,STRING)
            CALL PAR_WRUSER('Error reading from K-magnitude file',
     :                                                         STATUS)
            CALL PAR_WRUSER(FILENAME(:ICH_LEN(FILENAME)),STATUS)
            CALL PAR_WRUSER(STRING,STATUS)
            MORE=.FALSE.
         ELSE IF (LINE.NE.' ') THEN
            IST=ICH_VERIF(LINE,1,' ')
            IF (LINE(IST:IST).NE.'*') THEN
               POSN=ICH_WORD(LINE,IST,' ,',' ',WORD,LWORD,CHAR)
               INVOKE=ICH_FOLD(WORD)
               IF (WORD(:LWORD).EQ.NAME(:NPTR)) THEN
                  MORE=.FALSE.
                  IF (POSN.GT.0) THEN
                     STATUS=ICH_NUMBR(LINE,POSN+1,' ,',KDEF,NEXT)
                     IF (STATUS.EQ.0) FOUND=.TRUE.
                  END IF
                  IF (.NOT.FOUND) THEN
                     CALL PAR_WRUSER(
     :                  'Invalid record in K-magnitude file '//
     :                  FILENAME(:ICH_LEN(FILENAME)),STATUS)
                     CALL PAR_WRUSER(LINE,STATUS)
                  END IF
               END IF
            END IF
         END IF
      END DO
C
      IF (.NOT.FOUND) THEN
         CALL PAR_WRUSER('Unable to find an entry for '//NAME(:NPTR),
     :                                                         STATUS)
         CALL PAR_WRUSER('in file '//FILENAME(:ICH_LEN(FILENAME)),
     :                                                         STATUS)
         CALL PAR_WRUSER(' ',STATUS)
         STATUS=1
      ELSE
         LENF=ICH_LEN(FILENAME)
         STRING=FILENAME(:LENF)//' gives the K-mag of '//
     :                                          NAME(:NPTR)//' as '
         INVOKE=ICH_ENCODE(STRING,KDEF,LENF+NPTR+25,3,NEXT)
         CALL PAR_WRUSER(STRING(:NEXT-1),STATUS)
         CALL PAR_WRUSER(' ',STATUS)
         STATUS=0
      END IF
C
C     Tidy up on exit
C
  500 CONTINUE
      IF (LUOPEN) CLOSE (LU,IOSTAT=IGNORE)
      IGNORE=0
      IF (GOTLU) CALL DSA_FREE_LU(LU,IGNORE)
C
      END

      SUBROUTINE IRFLUX_IRFLUXQ(NX,OUT,ST)

C     Set quality bad in output if it is bad in either spectrum or standard

      IMPLICIT NONE
      INTEGER NX
      BYTE OUT(NX),ST(NX)
      INTEGER IX

      DO IX=1,NX
          IF ( ST(IX) .NE. 0 ) THEN
              OUT(IX) = 1
          END IF
      END DO
      END
