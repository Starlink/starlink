C+
      SUBROUTINE ABCONV
C
C     A B C O N V  /  F L C O N V  /  I R C O N V
C
C     Converts a spectrum into AB magnitudes (ABCONV) or f-lambda
C     units (erg/s/cm**2/Angstrom) (FLCONV), or W/m**2/um (IRCONV).
C     The original units of the data may be Jy (Janskys), mJy
C     (milli-Janskys), or uJy (micro-Janskys).  Other
C     possibilities may be added later.
C
C     Command parameters -
C
C     SPECTRUM The name of the structure containing the spectrum.
C              currently used for the spectrum.  For FLCONV
C              an x-axis data structure giving the wavelengths of the
C              data elements is also required.
C
C     OUTPUT   The name of the result of the operation.  This can
C              be the same as for SPECTRUM. If not, a new structure
C              is created, with everything but the data a direct
C              copy of the input.
C
C     Command keywords  - None
C
C     User variables used - None
C                                      KS / CIT 18th May 1984
C     Modified -
C
C     16 Jan 1985  KS / AAO.  FLCONV code added.
C     12 Aug 1987  DJA/ AAO.  Revised DSA_ routines added - some specs
C                  have changed. Dynamic memory handling modified - now
C                  uses DYN_ routines
C     06 Dec 1990  JAB / JACH.  Add IRCONV, use errors and allow
C                  wavelength in microns.
C     14 Feb 1991  HME / UoE.  Change Z.UNITS strings. Support bad
C                  values. Don't use FIG_ prefix.
C     21 Feb 1991  HME / UoE.  Bug fix: Set flagged in output, if it
C                  has no quality.
C     05 Apr 1991  KS / AAO.  Merged with recent changes made at AAO,
C                  trap confusing error message when units are blank,
C                  give subroutines ABCONV_ prefix, trap too-complex X
C                  arrays.
C     08 Sep 1992  HME / UoE, Starlink. Changed INCLUDE.
C     15 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  Bad pixel handling.
C     10 Apr 1997  MJCL / Starlink, UCL.  PAR_ABORT checking.
C     2005 June 10 MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C     2015 June 19 MJC / EAO  Switch to IAU/FITS standard naming for
C                  the units.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER ICH_CLEAN, ICH_FOLD, ICH_LEN
C
C     Local variables
C
      LOGICAL      ANGST         ! Units in Angstrom?
      REAL         BADVAL        ! Bad value
      CHARACTER    COMMAND*64    ! The actual Figaro command entered
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      DOUBLE PRECISION DUMMY     ! Dummy argument
      INTEGER      EPTR          ! Dynamic-memory pointer to error array
      LOGICAL      ERRORS        ! True if errors are present
      INTEGER      ESLOT         ! Map slot number of error data
      LOGICAL      FAULT         ! Set if a non-DSA error is deteccted
      LOGICAL      FLCO          ! TRUE if the Figaro command was FLCONV
      INTEGER      I             ! General loop variable
      INTEGER      IGNORE        ! Used to pass ignorable status
      LOGICAL      IRCO          ! TRUE if the Figaro command was IRCONV
      DOUBLE PRECISION MAGNITUDES ! Indicates TRUE if data in magnitudes
      LOGICAL      MICRONS       ! True if wavelength in microns
      INTEGER      NCH           ! Index of a substring in a string
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      NTYPE         ! Type of units, used to index
                                 ! ULEN/UNITAB
      INTEGER      NXELM         ! Number of elements in x-axis
      INTEGER      OPTR          ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER      OSLOT         ! Map slot number for output data array
      INTEGER      STATUS        ! Running status for DSA_ routines
      INTEGER      ULEN(3)       ! Length in characters of the different
                                 ! units
      CHARACTER    UNITAB(3)*3   ! The names of the different units
      CHARACTER    UNITS*64      ! The units of the data
      INTEGER      XDIMS(10)     ! X-axis data array dimensions
      LOGICAL      XEXIST        ! TRUE if an x-axis array exists
      INTEGER      XNDIM         ! Number of x-axis data array
                                 ! dimensions
      INTEGER      XPTR          ! Dynamic-memory pointer to x-axis data
      INTEGER      XSLOT         ! Map slot number of x-axis data
      CHARACTER    XUNITS*64     ! The units of the x-axis data
C
      DATA UNITAB/'Jy','mJy','uJy'/
      DATA ULEN/2,3,3/
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      CALL DSA_GET_FLAG_VALUE( 'FLOAT', BADVAL, STATUS )
      IF (STATUS.NE.0) GO TO 500
C
C     Get command
C
      CALL PAR_COMMAND(COMMAND)
      FLCO=(COMMAND.EQ.'FLCONV')
      IRCO=(COMMAND.EQ.'IRCONV')
C
C     Get input name
C
      CALL DSA_INPUT ('SPECT','SPECTRUM',STATUS)
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE ('SPECT',10,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Find out if units of data are compatible with the conversion
C     to be attempted.
C
      CALL DSA_GET_DATA_INFO ('SPECT',1,UNITS,0,DUMMY,STATUS)
      NTYPE=0
      NCH=ICH_FOLD(UNITS)
      NCH=ICH_CLEAN(UNITS)
      IF ((INDEX(UNITS,'JANSKY').NE.0).OR.
     :    (INDEX(UNITS,'ansky').NE.0).OR.
     :    (INDEX(UNITS,'Jy').NE.0).OR.
     :    (INDEX(UNITS,'JY').NE.0)) THEN
         IF ((INDEX(UNITS,'MICRO').NE.0).OR.
     :       (INDEX(UNITS,'icro').NE.0).OR.
     :       (INDEX(UNITS,'uJy').NE.0).OR.
     :       (INDEX(UNITS,'UJY').NE.0)) THEN
            NTYPE=3
         ELSE IF ((INDEX(UNITS,'MILLI').NE.0).OR.
     :            (INDEX(UNITS,'illi').NE.0).OR.
     :            (INDEX(UNITS,'mJy').NE.0).OR.
     :            (INDEX(UNITS,'MJY').NE.0)) THEN
            NTYPE=2
         ELSE
            NTYPE=1
         END IF
      END IF
      IF (NTYPE.EQ.0) THEN
         IF (UNITS.EQ.' ') THEN
            CALL PAR_WRUSER(
     :         'Cannot convert - no units specified for data',IGNORE)
         ELSE
            CALL PAR_WRUSER('Don''t know how to convert from '//
     :                                 UNITS(:ICH_LEN(UNITS)),IGNORE)
         END IF
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     For FLCONV & IRCONV check on the units of the X data array, and on the
C     dimensions of the array - we can only handle the simpler cases here,
C     and can't handle, say, a 2D X-array that matches the 1st and 3rd data
C     array dimensions (although this is legitimate, and allowed by
C     the DSA routines).
C
      IF (FLCO .OR. IRCO) THEN
         CALL DSA_SEEK_AXIS('SPECT',1,XEXIST,STATUS)
         IF ((STATUS.NE.0).OR.(.NOT.XEXIST)) THEN
            CALL PAR_WRUSER
     :           ('There is no x-axis array - '//COMMAND(1:6)//
     :        ' needs one',IGNORE)
            GOTO 500
         END IF
         CALL DSA_GET_AXIS_INFO('SPECT',1,1,XUNITS,0,DUMMY,STATUS)
         NCH=ICH_FOLD(XUNITS)
         NCH=ICH_CLEAN(XUNITS)
         MICRONS = INDEX(XUNITS,'MICRON').NE.0.OR.
     :             INDEX(XUNITS,'icron') .NE.0.OR.
     :             INDEX(XUNITS,'UM')    .NE.0.OR.
     :             INDEX(XUNITS,'um')    .NE.0
         ANGST = INDEX(XUNITS,'ANGSTROM').NE.0.OR.
     :           INDEX(XUNITS,'ngstrom') .NE.0
         IF ((.NOT. MICRONS).AND..NOT.ANGST) THEN
            CALL PAR_WRUSER('Warning: Cannot interpret X unit, '//
     :                                     'assume Angstrom.', IGNORE)
         END IF
         CALL DSA_AXIS_SIZE ('SPECT',1,10,XNDIM,XDIMS,NXELM,STATUS)
         IF (XNDIM.GT.1) THEN
            DO I=1,XNDIM
               IF (XDIMS(I).NE.DIMS(I)) FAULT=.TRUE.
            END DO
            IF (FAULT) THEN
               CALL PAR_WRUSER('The dimensions of the wavelength array'
     :          //' bear too complex a relation to those of the data.'
     :                                                        ,IGNORE)
               CALL PAR_WRUSER('This routine cannot handle them.',
     :                                                         IGNORE)
               GO TO 500    ! Error exit
            END IF
         END IF
      END IF
C
C     Get output structure name
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     This routine may produce bad values.
C
      CALL DSA_SET_FLAGGED_VALUES( 'OUTPUT', .TRUE., STATUS )
      CALL DSA_USE_FLAGGED_VALUES( 'OUTPUT', STATUS )
C
C     Map output data
C
      CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map the errors
C
      CALL DSA_SEEK_ERRORS('OUTPUT',ERRORS,STATUS)
      IF (ERRORS) THEN
          CALL DSA_MAP_ERRORS('OUTPUT','UPDATE','FLOAT',EPTR,ESLOT,
     :                        STATUS)
      END IF
C
C     For FLCONV or IRCONV, map the X data as well.
C
      IF (FLCO .OR. IRCO) THEN
         CALL DSA_MAP_AXIS_DATA ('SPECT',1,'READ','FLOAT',XPTR,
     :                           XSLOT,STATUS)
      END IF
      IF (STATUS.NE.0) GOTO 500
C
C     Operate on the data. Note that these routines can operate
C     on their data in situ.
C
      IF (IRCO) THEN
         CALL ABCONV_IRCON(%VAL(CNF_PVAL(OPTR)),%VAL(CNF_PVAL(EPTR)),
     :                     NELM,NTYPE,%VAL(CNF_PVAL(XPTR)),NXELM,
     :                     ERRORS,MICRONS,%VAL(CNF_PVAL(OPTR)),
     :                     %VAL(CNF_PVAL(EPTR)),BADVAL)
         UNITS='W/(m**2*um)'

      ELSE IF (.NOT.FLCO) THEN
         CALL ABCONV_ABCON(%VAL(CNF_PVAL(OPTR)),%VAL(CNF_PVAL(EPTR)),
     ;                     NELM,NTYPE,ERRORS,%VAL(CNF_PVAL(OPTR)),
     :                     %VAL(CNF_PVAL(EPTR)),BADVAL)
         UNITS='AB magnitudes'

      ELSE
         CALL ABCONV_FLCON(%VAL(CNF_PVAL(OPTR)),%VAL(CNF_PVAL(EPTR)),
     :                     NELM,NTYPE,%VAL(CNF_PVAL(XPTR)),NXELM,
     :                     ERRORS,MICRONS,%VAL(CNF_PVAL(OPTR)),
     :                     %VAL(CNF_PVAL(EPTR)),BADVAL)
         UNITS='erg/(s*cm**2*Angstrom)'
      END IF
C
C     Say what we did
C
      CALL PAR_WRUSER(' ',IGNORE)
      CALL PAR_WRUSER('Data converted from '//
     :       UNITAB(NTYPE)(:ULEN(NTYPE))//' to '//
     :       UNITS(:ICH_LEN(UNITS)),IGNORE)
C
C     Change the units and set the magnitude flag
C
      MAGNITUDES=0.0
      IF (COMMAND.EQ.'ABCONV') MAGNITUDES=1.0
      CALL DSA_SET_DATA_INFO('OUTPUT',1,UNITS,1,MAGNITUDES,STATUS)
C
  500 CONTINUE
C
C     Closedown everything
C
      CALL DSA_CLOSE (STATUS)
      IF (FAULT) CALL FIG_SETERR
C
      END
C+
      SUBROUTINE ABCONV_ABCON (IN,INE,NELM,NTYPE,ERRORS,OUT,OUTE,BADVAL)
C
C     A B C O N V _ A B C O N
C
C     Converts a spectrum from its current units into AB magnitudes.
C     Only a limited set of units are supported as yet, listed below.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) IN     (Real array IN(NELM)) The input data.
C     (>) INE    (Real array INE(NELM)) The input errors.
C     (>) NELM   (Integer) The number of elements in IN.
C     (>) NTYPE  (Integer) Indicates the current units.  Can be
C                1 => Janskys
C                2 => Milli-Janskys
C                3 => Micro-Janskys
C     (>) ERRORS (Logical) TRUE if errors are present.
C     (>) BADVAL (Real) The bad value.
C     (<) OUT    (Real array OUT(NELM)) The converted data.
C     (<) OUTE   (Real array OUTE(NELM)) The converted errors.
C
C     Note that IN and OUT may be the same array in the calling program.
C
C     Common variables used - None
C
C     Functions / subroutines used - None
C
C     Note: The conversion from Jansky units is based on the relation
C     LOG(FV)=-0.4AB+6.56 which holds if FV is in milli-Janskys (quoted
C     in Fillipenko and Greenstein, PASP 1984)
C     Note: The routine returns without action and without message, if
C     NTYPE has an illegal value.
C
C                                            KS / CIT 18th May 1984
C     06-DEC-1990  JAB:
C        Error handling.
C     09-FEB-1991  HME:
C        Rename from FIG_ABCONV, support bad values.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM, NTYPE
      REAL    IN(NELM), INE(NELM), OUT(NELM), OUTE(NELM), BADVAL
      LOGICAL ERRORS
C
C     Local variables
C
      INTEGER IELM
      REAL    SCALE, SCALES(3)
C
      DATA SCALES/1000.,1.0,0.001/
C
      IF ((NTYPE.GT.0).AND.(NTYPE.LE.3)) THEN
         SCALE=SCALES(NTYPE)
         DO IELM=1,NELM
            IF ( IN(IELM) .GT. 0. .AND. IN(IELM) .NE. BADVAL ) THEN
               IF ( ERRORS ) THEN
                  IF ( INE(IELM) .NE. BADVAL ) THEN
                     OUTE(IELM) = INE(IELM) / IN(IELM) / LOG(10.) * 2.5
                  ELSE
                     OUTE(IELM) = BADVAL
                  END IF
               END IF
               OUT(IELM) = ( 6.56 - LOG10( IN(IELM) * SCALE ) ) * 2.5
            ELSE
               OUT(IELM)=BADVAL
               IF ( ERRORS ) OUTE(IELM) = BADVAL
            END IF
         END DO
      END IF
C
      END
C+
      SUBROUTINE ABCONV_FLCON (IN,INE,NELM,NTYPE,WAVES,NWAV,ERRORS,
     :      MICRONS,OUT,OUTE,BADVAL)
C
C     A B C O N V _ F L C O N
C
C     Converts a spectrum from its current units into
C     erg/s/cm**2/Angstrom.
C     Only a limited set of units are supported as yet, listed below.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) IN     (Real array IN(NELM)) The input data.
C     (>) INE    (Real array INE(NELM)) The input errors.
C     (>) NELM   (Integer) The number of elements in IN.
C     (>) NTYPE  (Integer) Indicates the current units.  Can be
C                1 => Jy (Janskys)
C                2 => mJy (Milli-Janskys)
C                3 => uJy (Micro-Janskys)
C     (>) WAVES  (Real array WAVES(NWAV)) The wavelengths of the
C                elements of the input data.  If NELM>NWAV, the
C                routine re-uses the WAVES values from 1 once it
C                passes NWAV.  (This convention allows IN to be
C                a 2D array of dimensions NWAV by n, the various
C                1D slices all having the same wavelengths).
C     (>) NWAV   (Integer) The number of wavelength values.
C     (>) ERRORS (Logical) True if errors are present.
C     (>) MICRONS (Logical) True if wavelengths are in microns rather
C                   than Angstroms
C     (>) BADVAL (Real) The bad value.
C     (<) OUT    (Real array OUT(NELM)) The converted data.
C     (<) OUTE   (Real array OUTE(NELM)) The converted errors.
C
C     Note that IN and OUT may be the same array in the calling program.
C
C     Common variables used - None
C
C     Functions / subroutines used - None
C
C     Note: The conversion from Jansky units is based on the relation
C     FL=(2.998E-5/WAVELENGTH**2)*FV which holds if FV is in Janskys.
C
C                                            KS / CIT 16th Jan 1985
C     06-DEC-1990  JAB:
C        Error handling. Wavelength in Angstrom or micron.
C     09-FEB-1991  HME:
C        Rename from FIG_FLCONV, support bad values.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM, NTYPE, NWAV
      REAL    IN(NELM), INE(NELM), WAVES(NWAV), OUT(NELM), OUTE(NELM)
      LOGICAL ERRORS,MICRONS
      REAL    BADVAL
C
C     Local variables
C
      INTEGER IELM, IWPTR
      REAL    SCALE, SCALES(3), WSCALE, FAC
C
      DATA SCALES/1.0,0.001,0.000001/
C
      IF (MICRONS) THEN
          WSCALE = 1E8
      ELSE
          WSCALE = 1.0
      END IF
      IF ((NTYPE.GT.0).AND.(NTYPE.LE.3)) THEN
         SCALE=SCALES(NTYPE)*2.998E-5/WSCALE
         IWPTR=1
         DO IELM=1,NELM
            IF ( IN(IELM) .NE. BADVAL ) THEN
               FAC = SCALE/(WAVES(IWPTR)*WAVES(IWPTR))
               OUT(IELM)=FAC*IN(IELM)
               IF ( ERRORS ) THEN
                  IF ( INE(IELM) .NE. BADVAL ) THEN
                     OUTE(IELM) = FAC * INE(IELM)
                  ELSE
                     OUTE(IELM) = BADVAL
                  END IF
               END IF
            ELSE
               OUT(IELM) = BADVAL
               IF (ERRORS) OUTE(IELM) = BADVAL
            END IF
            IWPTR=IWPTR+1
            IF (IWPTR.GT.NWAV) IWPTR=1
         END DO
      END IF
C
      END
C+
      SUBROUTINE ABCONV_IRCON (IN,INE,NELM,NTYPE,WAVES,NWAV,ERRORS,
     :      MICRONS,OUT,OUTE,BADVAL)
C
C     A B C O N V _ I R C O N
C
C     Converts a spectrum from its current units into W/m**2/um.
C     Only a limited set of units are supported as yet, listed below.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) IN     (Real array IN(NELM)) The input data.
C     (>) INE    (Real array INE(NELM)) The input errors.
C     (>) NELM   (Integer) The number of elements in IN.
C     (>) NTYPE  (Integer) Indicates the current units.  Can be
C                1 => Jy (Janskys)
C                2 => mJy (Milli-Janskys)
C                3 => uJy (Micro-Janskys)
C     (>) WAVES  (Real array WAVES(NWAV)) The wavelengths of the
C                elements of the input data.  If NELM>NWAV, the
C                routine re-uses the WAVES values from 1 once it
C                passes NWAV.  (This convention allows IN to be
C                a 2D array of dimensions NWAV by n, the various
C                1D slices all having the same wavelengths).
C     (>) NWAV   (Integer) The number of wavelength values.
C     (>) ERRORS (Logical) True if errors are present.
C     (>) MICRONS (Logical) True if wavelengths are in microns rather
C                   than Angstroms
C     (>) BADVAL (Real) The bad value.
C     (<) OUT    (Real array OUT(NELM)) The converted data.
C     (<) OUTE   (Real array OUTE(NELM)) The converted errors.
C
C     Note that IN and OUT may be the same array in the calling program.
C
C     Common variables used - None
C
C     Functions / subroutines used - None
C
C     Note: The conversion from Jansky units is based on the relation
C     FL=(2.998E-12/WAVELENGTH**2)*FV which holds if FV is in Janskys
C     and wavelength is in microns.
C
C
C     06-DEC-1990  JAB:
C        Original modified from FIG_FLCONV. Error handling.
C     09-FEB-1991  HME:
C        Rename from FIG_IRCONV, support bad values.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM, NTYPE, NWAV
      REAL    IN(NELM), INE(NELM), WAVES(NWAV), OUT(NELM), OUTE(NELM)
      LOGICAL ERRORS,MICRONS
      REAL    BADVAL
C
C     Local variables
C
      INTEGER IELM, IWPTR
      REAL    SCALE, SCALES(3), WSCALE, FAC
C
      DATA SCALES/1.0,0.001,0.000001/
C
      IF (MICRONS) THEN
          WSCALE = 1.0
      ELSE
          WSCALE = 1.0E-8
      END IF
      IF ((NTYPE.GT.0).AND.(NTYPE.LE.3)) THEN
         SCALE=SCALES(NTYPE)*2.9979E-12/WSCALE
         IWPTR=1
         DO IELM=1,NELM
            IF ( IN(IELM) .NE. BADVAL ) THEN
               FAC = SCALE/(WAVES(IWPTR)*WAVES(IWPTR))
               OUT(IELM)=FAC*IN(IELM)
               IF ( ERRORS ) THEN
                  IF ( INE(IELM) .NE. BADVAL) THEN
                     OUTE(IELM) = FAC * INE(IELM)
                  ELSE
                     OUTE(IELM) = BADVAL
                  END IF
               END IF
            ELSE
               OUT(IELM) = BADVAL
               IF (ERRORS) OUTE(IELM) = BADVAL
            END IF
            IWPTR=IWPTR+1
            IF (IWPTR.GT.NWAV) IWPTR=1
         END DO
      END IF
C
      END
