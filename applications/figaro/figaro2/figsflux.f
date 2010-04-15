C+
      SUBROUTINE FIGSFLUX
C
C     F I G S F L U X
C
C     Flux calibrates a FIGS spectrum using a standard spectrum
C
C     Command parameters -
C
C     SPECTRUM  The name of the structure containing the first image.
C
C     STANDARD  The name of the structure containing the second
C               image data.
C
C     KMAG      The K magnitude of the standard used
C
C     OUTPUT    The name of the result of the operation.  This can
C               be the same as for SPECTRUM.  If not, a new structure
C               is created, with everything but the data a direct
C               copy of the input.
C
C                                          JAB / AAO  14th June 1985
C     Modified:
C
C     12th Aug 1985  KS / AAO.  Now expects error arrays to be
C                    percentage values rather than absolute values.
C     22nd July 1986 KS / AAO.  Reverts to use of absolute error values.
C     24th Aug 1987  DJA/ AAO.  Revised DSA_ routines - some specs
C                    changed. Now uses DYN_ routines for dynamic-memory
C                    handling.
C     11th Nov 1987  KS / AAO.  Now attempts to determine K mag from
C                    the object name and a file of magnitudes.
C     13th Oct 1988  KS / AAO.  Check added to trap the case where there
C                    is no object name.
C     13th Dec 1990  JAB/ JAC.  Use data quality.
C     14th Feb 1991  HME/ UoE.  Reshuffle DSA_ calls. Map output for
C                    update rather than write. Remove FIG_ prefix.
C                    Revise (FIG_)FLUX subroutine. If SAME, output is
C                    model flux density, errors are zero, but bad pixels
C                    remain bad.
C     8th Mar. 1991  JMS / AAO. Added FIGSFLUX_ prefix to all subroutine
C                    names.
C     22nd Sep 1992  HME / UoE, Starlink. TABs removed, INCLUDE changed.
C                    Lowercase file name. Don't handle LIB$GET_LUN as
C                    logical.
C     21st Jul 1993  HME / UoE, Starlink. Use DSA_*_LU.
C     28th Jul 1993  HME / UoE, Starlink. Disuse STR$UPCASE.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C     2005 Aug 15    TIMJ / JACH. X ** -Y is not allowed. Use X ** (-Y)
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL DSA_SAME_DATA
      INTEGER DSA_TYPESIZE
C
C     Local variables
C
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      DOUBLE PRECISION  DUMMY    ! Dummy for double-precision value
      LOGICAL      ERROR1        ! Spectrum has an error structure?
      LOGICAL      ERROR2        ! Standard has an error structure?
      INTEGER      ESPTR         ! Dynamic-memory pointer to standard
                                 ! errors
      INTEGER      ESSLOT        ! Map slot number of standard errors
      INTEGER      IGNORE        ! Used to pass ignorable status
      REAL         K             ! K magnitude value of standard
      REAL         KDEF          ! Default value for K magnitude
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NX            ! Size of 1st dimension
      CHARACTER    OBJECT*64     ! Name of standard
      INTEGER      OEPTR         ! Dynamic-memory pointer to output
                                 ! errors
      INTEGER      OESLOT        ! Map slot number of output errors
      INTEGER      OPTR          ! Dynamic-memory pointer to data array
      INTEGER      OSLOT         ! Map slot number of input data array
      INTEGER      OQPTR         ! Dynamic-memory pointer to output
                                 ! quality array
      INTEGER      OQSLOT        ! Map slot number of output quality
                                 ! array
      LOGICAL      SAME          ! standard is the same as the spectrum?
      INTEGER      SPTR          ! Dynamic-memory pointer to standard
                                 ! data array
      INTEGER      SSLOT         ! Map slot number of standard data
                                 ! array
      INTEGER      SQPTR         ! Dynamic-memory pointer to standard
                                 ! quality
      INTEGER      SQSLOT        ! Map slot number of standard quality
                                 ! array
      INTEGER      STATUS        ! Running status for DSA_ routines
      CHARACTER    STRINGS(2)*16 ! Units and label information for new
                                 ! data
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
C     Get K magnitude of standard.  The program attempts to determine
C     the default value from the object name and a file of standards and
C     their magnitudes.
C
      IF (STATUS.NE.0) GOTO 500
      CALL DSA_OBJECT_NAME ('STAND', OBJECT, STATUS)
      IF ((STATUS.EQ.0).AND.(OBJECT.NE.' ')) THEN
         CALL FIGSFLUX_KFLUX (OBJECT,KDEF,STATUS)
         IF (STATUS.EQ.0) CALL PAR_SDVAL('KMAG',KDEF,IGNORE)
      END IF
      STATUS=0
      CALL PAR_RDVAL('KMAG',-5.0,20.0,KDEF,' ',K)
C
C     Get output structure name as a complete copy of SPECT or even the
C     same data (same file).
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
C     Map output data (which are (a copy of) input data).
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      CALL DSA_MAP_QUALITY('OUTPUT','UPDATE','BYTE',OQPTR,OQSLOT,
     :   STATUS)
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
C     Map standard data, if SAME we got them already.
C
      IF (.NOT.SAME) THEN
         CALL DSA_MAP_DATA('STAND','READ','FLOAT',SPTR,SSLOT,STATUS)
         CALL DSA_MAP_QUALITY('STAND','READ','BYTE',SQPTR,SQSLOT,STATUS)

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
      END IF
      ERROR2=ERROR1
      IF (STATUS.NE.0) GOTO 500
C
C     Operate on the quality arrays
C
      IF ( .NOT. SAME )
     :   CALL FIGSFLUX_FIGSFLUXQ(NX,%VAL(CNF_PVAL(OQPTR)),
     :                           %VAL(CNF_PVAL(SQPTR)))
C
C     Operate on the images
C
      CALL FIGSFLUX_FLUX(ERROR1,NX,K,%VAL(CNF_PVAL(OPTR)),
     :                   %VAL(CNF_PVAL(OEPTR)),%VAL(CNF_PVAL(OQPTR)),
     :                   %VAL(CNF_PVAL(SPTR)),%VAL(CNF_PVAL(ESPTR)),
     :                   %VAL(CNF_PVAL(XPTR)))
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

      SUBROUTINE FIGSFLUX_FLUX(ERROR1,N,K,STAR,ESTAR,QUALITY,STAN,
     :     ESTAN,LAMBDA)
C
C     Divide a spectrum by a standard taking account of errors
C     and generating a flux calibrated result in mJy
C
C     ERROR1    (Logical) - True if the star spectrum has errors
C     N         (Integer) - Number of elements of the spectra
C     K         (Real)    - K magnitude of the standard
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
      REAL K
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
      REAL FIGSFLUX_LFLUX


      DO 1 I = 1, N
         IF ( QUALITY(I) .EQ. 0 ) THEN
            F = FIGSFLUX_LFLUX(K,LAMBDA(I))
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

      REAL FUNCTION FIGSFLUX_LFLUX(K,L)
C
C     Return the flux in mJy of a standard star of K magnitude K
C     at wavelength L microns
C
C     Formula is valid for the AAO G dwarf standards and is based
C     on that given by D. Allen in the IRPS user's guide.
C
C     Probably unreliable below 1.4 microns and above 4.2 microns
C
C     Modified 30th Dec 1985 (KS / AAO) to incorporate constants
C     for range below 1.4 microns supplied by Jeremy Bailey.
C
      REAL K,L
      REAL F
C
C     Calculate flux in erg s(-1) cm(-2) um(-1)
C
      IF (L .LT. 1.4) THEN
         F=2.6E-6*(10**(-0.4*K))*(L*L-4.03*L+4.30)
      ELSE IF (L .LT. 1.9) THEN
         F=1.27E-6*(10**(-0.4*K))*(L*L-4.82*L+6.068)
      ELSE IF (L .LT. 2.6) THEN
         F=6.88E-6*(10**(-0.4*K))*(L**(-3.63))
      ELSE
         F=9.41E-6*(10**(-0.4*K))*(L**(-3.91))
      END IF
C
C     Convert to mJy
C
      FIGSFLUX_LFLUX=F*0.333333E12*L*L
      END

      SUBROUTINE FIGSFLUX_KFLUX (OBJECT,KDEF,STATUS)
C
C     F I G S F L U X _ K F L U X
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
      STRING='Standard name given as '//
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
      IF (GOTLU) CALL DSA_FREE_LU(LU,IGNORE)
C
      END

      SUBROUTINE FIGSFLUX_FIGSFLUXQ(NX,OUT,ST)

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
