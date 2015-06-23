C+
      SUBROUTINE FWCONV
C
C     F W C O N V
C
C     Converts flux units of a spectrum from/to either
C     F_lambda (erg/s/cm**2/Angstrom), AB magnitudes or F_nu (Jy).
C
C     Command parameters -
C
C     SPECTRUM The name of the structure containing the spectrum.
C              An x-axis data structure giving the wavelengths of the
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
C
C     History -
C
C     4th  Feb 1991  SMH/ AAO.  Original version, based on ABCONV code,
C                    but performing a larger number of possible
C                    conversions.
C     5th  Apr 1991  KS / AAO.  Some minor cosmetic changes prior to
C                    release.
C     22nd Sep 1992  HME / UoE, Starlink. TABs removed, INCLUDE changed.
C     3rd  Jul 2001  VGG / RAL. Error propagation included.
C     7th  Jul 200.  ACD / UoE. Tidied up.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C     2015 June 19   MJC / EAO  Switch to IAU/FITS standard naming for
C                    the units.
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
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      DOUBLE PRECISION DUMMY     ! Dummy argument
      LOGICAL      FAULT         ! Set if a non-DSA error is deteccted
      INTEGER      I             ! General loop variable
      INTEGER      IGNORE        ! Used to pass ignorable status
      INTEGER      INTYPE        ! Type of input units, used to index
                                 ! ULEN/UNITAB
      DOUBLE PRECISION MAGNITUDES ! Indicates TRUE if data to be in
                                 ! magnitudes
      INTEGER      NCH           ! Index of a substring in a string
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      OUTYPE        ! Type of output units
      INTEGER      OPTR          ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER      OSLOT         ! Map slot number for output data array
      INTEGER      STATUS        ! Running status for DSA_ routines
      INTEGER      ULEN(5)       ! Length in characters of the different
                                 ! units
      CHARACTER    UNITAB(5)*20  ! The names of the different units
      CHARACTER    UNITS*64      ! The units of the data
      LOGICAL      VEXIST        ! TRUE if a variance array exists
      INTEGER      VPTR          ! Dynamic-memory pointer to input
                                 ! variance array
      INTEGER      VSLOT         ! Map slot number for input variance
                                 ! array
      INTEGER      XDIMS(10)     ! X-axis data array dimensions
      LOGICAL      XEXIST        ! TRUE if an x-axis array exists
      INTEGER      XNDIM         ! Number of x-axis data array
                                 ! dimensions
      INTEGER      XNELM         ! Number of elements in X-axis array
      INTEGER      XPTR          ! Dynamic-memory pointer to x-axis data
      INTEGER      XSLOT         ! Map slot number of x-axis data
      CHARACTER    XUNITS*64     ! The units of the x-axis data
C
      DATA UNITAB/'Jy','mJy','uJy',
     :            'erg/s/cm**2/Angstrom','AB magnitudes'/
      DATA ULEN/2,3,3,20,13/
C
C     Initialisation of DSA_ routines
C
      FAULT=.FALSE.
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input name
C
      CALL DSA_INPUT ('SPECT','SPECTRUM',STATUS)
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE ('SPECT',10,NDIM,DIMS,NELM,STATUS)
C
C     Get input units. Find out if units of data are compatible
C     with the conversion to be attempted.
C
      CALL DSA_GET_DATA_INFO ('SPECT',1,UNITS,0,DUMMY,STATUS)
      NCH=ICH_FOLD(UNITS)
      NCH=ICH_CLEAN(UNITS)
      CALL INTERPRET_FLUX(UNITS,INTYPE)
      IF (INTYPE.LE.0.OR.INTYPE.GE.6) THEN
         IF (UNITS.EQ.' ') THEN
            CALL PAR_WRUSER(
     :         'Cannot convert - no units specified for data',IGNORE)
         ELSE
            CALL PAR_WRUSER('Don''t know how to convert from '//
     :                                 UNITS(:ICH_LEN(UNITS)),IGNORE)
         END IF
         FAULT=.TRUE.
         GO TO 500
      ELSE
         CALL PAR_WRUSER('Input units are: '//UNITS(:NCH),IGNORE)
         CALL PAR_WRUSER('These are being treated as: '//
     :                          UNITAB(INTYPE)(:ULEN(INTYPE)),IGNORE)
      END IF
C
C     Get output units
C
      CALL PAR_RDCHAR('FLUX_UNIT',' ',UNITS)
      CALL INTERPRET_FLUX(UNITS,OUTYPE)
      IF (OUTYPE.LE.0.OR.OUTYPE.GE.6) THEN
         IF (UNITS.EQ.' ') THEN
            CALL PAR_WRUSER(
     :         'Cannot convert - no units specified for data',IGNORE)
         ELSE
            CALL PAR_WRUSER('Don''t know how to convert to '//
     :                                 UNITS(:ICH_LEN(UNITS)),IGNORE)
         END IF
         FAULT=.TRUE.
         GO TO 500
      ELSE
         CALL PAR_WRUSER('Output units will be: '//
     :                          UNITAB(OUTYPE)(:ULEN(OUTYPE)),IGNORE)
       END IF
      IF (INTYPE.EQ.OUTYPE) THEN
         CALL PAR_WRUSER('Input and output units identical',IGNORE)
         GOTO 500
      END IF
C
C     Check on the units of the X data array, and on the
C     dimensions of the array - we can only handle the simpler cases here,
C     and can't handle, say, a 2D X-array that matches the 1st and 3rd data
C     array dimensions (although this is legitimate, and allowed by
C     the DSA routines).
C
      CALL DSA_SEEK_AXIS('SPECT',1,XEXIST,STATUS)
      IF ((STATUS.NE.0).OR.(.NOT.XEXIST)) THEN
         CALL PAR_WRUSER
     :           ('There is no x-axis array - FWCONV needs one',IGNORE)
         FAULT=.TRUE.
         GOTO 500
      END IF

      CALL DSA_GET_AXIS_INFO('SPECT',1,1,XUNITS,0,DUMMY,STATUS)
      NCH=ICH_FOLD(XUNITS)
      NCH=ICH_CLEAN(XUNITS)
      IF (INDEX(XUNITS,'ANGSTROM').EQ.0.AND.
     :    INDEX(XUNITS,'ngstrom').EQ.0 ) THEN
         CALL PAR_WRUSER(
     :      'Warning: X-data does not appear to be in Angstroms',
     :                                                         IGNORE)
      END IF

      CALL DSA_AXIS_SIZE ('SPECT',1,10,XNDIM,XDIMS,XNELM,STATUS)
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


C
C     Get output structure name
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map output data
C
      CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Also map the X data.
C
      CALL DSA_MAP_AXIS_DATA ('SPECT',1,'READ','FLOAT',XPTR,
     :                        XSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500

C
C     Operate on the data. Note that FIG_FWCONV can operate
C     on data in situ.
C
      CALL FIG_FWCONV(%VAL(CNF_PVAL(OPTR)),NELM,INTYPE,OUTYPE,
     :                %VAL(CNF_PVAL(OPTR)),%VAL(CNF_PVAL(XPTR)),XNELM)
      UNITS=UNITAB(OUTYPE)(:ULEN(OUTYPE))
C
C
C     Check for the existence of errors and map the variance array if
C     one exists.
C
      VEXIST = .FALSE.
      CALL DSA_SEEK_VARIANCE ('SPECT',VEXIST,STATUS)
      IF (VEXIST) THEN
         CALL DSA_MAP_VARIANCE ('OUTPUT','UPDATE','FLOAT',VPTR,
     :                          VSLOT, STATUS)
C
        CALL FIG_FWCONV_VAR(%VAL(CNF_PVAL(VPTR)),NELM,INTYPE,OUTYPE,
     :                      %VAL(CNF_PVAL(VPTR)),%VAL(CNF_PVAL(XPTR)),
     :                      XNELM)
      END IF
C
C     Say what we did
C
      CALL PAR_WRUSER(' ',IGNORE)
      CALL PAR_WRUSER('Data converted from '//
     :       UNITAB(INTYPE)(:ULEN(INTYPE))//' to '//
     :       UNITS(:ICH_LEN(UNITS)),IGNORE)
C
C     Change the units and set the magnitude flag
C
      MAGNITUDES=0.0
      IF (OUTYPE.EQ.5) MAGNITUDES=1.0
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
      SUBROUTINE INTERPRET_FLUX(UNITS,TYPE)
C
C     To determine flux units and return a coded integer (TYPE) such that
C        (1) = micro-Janskys
C        (2) = milli-Janskys
C        (3) = Janskys
C        (4) = erg/s/cm**2/Angstrom
C        (5) = AB magnitudes
C+
      INTEGER TYPE
      CHARACTER UNITS*(*)
C
      TYPE=0
      IF (INDEX(UNITS,'AB').NE.0) THEN
         TYPE=5
      ELSE IF ((INDEX(UNITS,'Erg').NE.0).OR.(INDEX(UNITS,'ERG').NE.0)
     :                             .OR.(INDEX(UNITS,'erg').NE.0)) THEN
         TYPE=4
      ELSE IF ((INDEX(UNITS,'JANSKY').NE.0).OR.
     :         (INDEX(UNITS,'ansky').NE.0).OR.
     :         (INDEX(UNITS,'Jy').NE.0).OR.
     :         (INDEX(UNITS,'JY').NE.0)) THEN
         IF ((INDEX(UNITS,'MICRO').NE.0).OR.
     :       (INDEX(UNITS,'icro').NE.0).OR.
     :       (INDEX(UNITS,'uJy').NE.0).OR.
     :       (INDEX(UNITS,'UJY').NE.0)) THEN
            TYPE=3
         ELSE IF ((INDEX(UNITS,'MILLI').NE.0).OR.
     :            (INDEX(UNITS,'illi').NE.0).OR.
     :            (INDEX(UNITS,'mJy').NE.0).OR.
     :            (INDEX(UNITS,'MJY').NE.0)) THEN
            TYPE=2
         ELSE
            TYPE=1
         END IF
      END IF
      RETURN
      END
C+
      SUBROUTINE FIG_FWCONV (IN,NELM,INTYPE,OUTYPE,OUT,WAVES,NWAV)
C
C     F I G _ F W C O N V
C
C     Converts units of a spectrum, with options of
C     erg/s/cm**2/Angstrom, mJy, or AB magnitudes.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) IN     (Real array IN(NELM)) The input data.
C     (>) NELM   (Integer) The number of elements in IN.
C     (>) INTYPE (Integer) Indicates input units of either
C                1 => Jy (Janskys)
C                2 => mJy (Milli-Janskys)
C                3 => uJy (Micro-Janskys)
C                4 => erg/s/cm**2/Angstrom
C                5 => AB magnitudes
C     (>) OUTYPE (Integer) Indicates output units (as for INTYPE)
C     (>) WAVES  (Real array WAVES(NWAV)) The wavelengths of the
C                elements of the input data.  If NELM>NWAV, the
C                routine re-uses the WAVES values from 1 once it
C                passes NWAV.  (This convention allows IN to be
C                a 2D array of dimensions NWAV by n, the various
C                1D slices all having the same wavelengths).
C     (>) NWAV   (Integer) The number of wavelength values.
C     (<) OUT    (Real array OUT(NELM)) The converted data.
C
C     Note that IN and OUT may be the same array in the calling program.
C
C     Common variables used - None
C
C     Functions / subroutines used - None
C
C     Note: The conversions are based on the relations
C         FL=(2.998E-8/WAVELENGTH**2)*FV
C         LOG(FV)=-0.4AB+6.56
C     where FL is flux in erg/s/cm**2/Angstrom
C           FV is flux in mJy and
C           AB is in AB magnitudes
C     (quoted in Fillipenko and Greenstein, PASP 1984)
C
C                                            KS / CIT 16th Jan 1985
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM, INTYPE, OUTYPE, NWAV
      REAL    IN(NELM), WAVES(NWAV), OUT(NELM)
C
C     Local variables
C
      INTEGER IELM, IWPTR
      REAL    AMAX, FLAG, FMIN, SCALE, SCALES(5), F_NU
C
      DATA SCALES/1000.,1.0,0.001,1.0,1.0/
      DATA FLAG,FMIN/10000.0,-1.7E38/
C
      SCALE=SCALES(INTYPE)/SCALES(OUTYPE)

      IF (INTYPE.LE.3) THEN
C       Input units in Janskys (or subunits thereof)

        IF (OUTYPE.LE.3) THEN
C         Simple conversion to other Jansky subunit
          IWPTR=1
          DO IELM=1,NELM
            OUT(IELM)=SCALE*IN(IELM)
            IWPTR=IWPTR+1
            IF (IWPTR.GT.NWAV) IWPTR=1
          END DO
        ELSE IF (OUTYPE.EQ.4) THEN
C         Conversion to erg/s/cm**2/Angstrom
          SCALE=SCALE*2.998E-8
          IWPTR=1
          DO IELM=1,NELM
            OUT(IELM)=SCALE*IN(IELM)/(WAVES(IWPTR)*WAVES(IWPTR))
            IWPTR=IWPTR+1
            IF (IWPTR.GT.NWAV) IWPTR=1
          END DO
        ELSE IF (OUTYPE.EQ.5) THEN
C         Conversion to AB magnitudes


            AMAX=FMIN
            DO IELM=1,NELM
              IF (IN(IELM).GT.0.) THEN
                   OUT(IELM)=(6.56-LOG10(IN(IELM)*SCALE))*2.5
                   AMAX=MAX(AMAX,OUT(IELM))
              ELSE
                 OUT(IELM)=FLAG
              END IF
            END DO
            DO IELM=1,NELM
              IF (OUT(IELM).EQ.FLAG) OUT(IELM)=AMAX
            END DO

        END IF


      ELSE IF (INTYPE.EQ.4) THEN
C       Input units in erg/s/cm**2/Angstrom
        SCALE=SCALE/2.998E-8
        IWPTR=1
        IF (OUTYPE.LE.3) THEN
C         Conversion to Jansky (or a subunit)
          DO IELM=1,NELM
            OUT(IELM)=IN(IELM)*WAVES(IWPTR)*WAVES(IWPTR)*SCALE
            IWPTR=IWPTR+1
            IF (IWPTR.GT.NWAV) IWPTR=1
          END DO
        ELSE IF (OUTYPE.EQ.5) THEN
C         Conversion to AB magnitudes
          IWPTR=1
          AMAX=FMIN
          DO IELM=1,NELM
            F_NU=IN(IELM)*WAVES(IWPTR)*WAVES(IWPTR)*SCALE
            IWPTR=IWPTR+1
            IF (IWPTR.GT.NWAV) IWPTR=1
            IF (F_NU.GT.0.) THEN
               OUT(IELM)=(6.56-LOG10(F_NU))*2.5
               AMAX=MAX(AMAX,OUT(IELM))
            ELSE
               OUT(IELM)=FLAG
            END IF
          END DO
          DO IELM=1,NELM
            IF (OUT(IELM).EQ.FLAG) OUT(IELM)=AMAX
          END DO
        END IF
      ELSE IF (INTYPE.EQ.5) THEN
C       Input units in AB magnitudes
        IF (OUTYPE.LE.3) THEN
C         Conversion to Jansky (or a subunit)
          DO IELM=1,NELM
            OUT(IELM)=(10.0**(6.56-0.4*IN(IELM)))*SCALE
          END DO
        ELSE IF (OUTYPE.EQ.4) THEN
C         Conversion to erg/s/cm**2/Angstrom
          SCALE=SCALE*2.998E-8
          IWPTR=1
          DO IELM=1,NELM
            F_NU=10.0**(6.56-0.4*IN(IELM))
            OUT(IELM)=SCALE*F_NU/(WAVES(IWPTR)*WAVES(IWPTR))
            IWPTR=IWPTR+1
            IF (IWPTR.GT.NWAV) IWPTR=1
          END DO
        END IF
      END IF
C
      END
C+
      SUBROUTINE FIG_FWCONV_VAR (IN,NELM,INTYPE,OUTYPE,OUT,WAVES,
     :                   NWAV)
C
C     F I G _ F W C O N V _ V A R
C
C     Converts units of the variance of a spectrum, with options of
C     erg/s/cm**2/Angstrom, mJy, or AB magnitudes.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) IN     (Real array IN(NELM)) The input variance data.
C     (>) NELM   (Integer) The number of elements in IN.
C     (>) INTYPE (Integer) Indicates input units of either
C                1 => Jy (Janskys)
C                2 => mJy (Milli-Janskys)
C                3 => uJy (Micro-Janskys)
C                4 => erg/s/cm**2/Angstrom
C                5 => AB magnitudes
C     (>) OUTYPE (Integer) Indicates output units (as for INTYPE)
C     (>) WAVES  (Real array WAVES(NWAV)) The wavelengths of the
C                elements of the input data.  If NELM>NWAV, the
C                routine re-uses the WAVES values from 1 once it
C                passes NWAV.  (This convention allows IN to be
C                a 2D array of dimensions NWAV by n, the various
C                1D slices all having the same wavelengths).
C     (>) NWAV   (Integer) The number of wavelength values.
C     (<) OUT    (Real array OUT(NELM)) The converted data.
C
C     Note that IN and OUT may be the same array in the calling program.
C
C     Common variables used - None
C
C     Functions / subroutines used - None
C
C     Note: The conversions are based on the relations
C         FL=(2.998E-8/WAVELENGTH**2)*FV
C         LOG(FV)=-0.4AB+6.56
C     where FL is flux in erg/s/cm**2/Angstrom
C           FV is flux in mJy and
C           AB is in AB magnitudes
C     (quoted in Fillipenko and Greenstein, PASP 1984)
C
C                                            VGG / RAL 3rd Jul 2001
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM, INTYPE, OUTYPE, NWAV
      REAL    IN(NELM), WAVES(NWAV), OUT(NELM)
C
C     Local variables
C
      INTEGER IELM, IWPTR
      REAL    AMAX, FLAG, FMIN, SCALE, SCALES(5)
      DOUBLE PRECISION F_NU, TEMP
C
      DATA SCALES/1000.,1.0,0.001,1.0,1.0/
      DATA FLAG,FMIN/10000.0,-1.7E38/
C
      SCALE=SCALES(INTYPE)/SCALES(OUTYPE)

      IF (INTYPE.LE.3) THEN
C       Input units in Janskys (or subunits thereof)

        IF (OUTYPE.LE.3) THEN
C         Simple conversion to other Jansky subunit
          IWPTR=1
          DO IELM=1,NELM

            TEMP=SCALE*SQRT(IN(IELM))
            OUT(IELM)=TEMP*TEMP

            IWPTR=IWPTR+1
            IF (IWPTR.GT.NWAV) IWPTR=1
          END DO
        ELSE IF (OUTYPE.EQ.4) THEN
C         Conversion to erg/s/cm**2/Angstrom
          SCALE=SCALE*2.998E-8
          IWPTR=1
          DO IELM=1,NELM

            TEMP = SCALE*SQRT(IN(IELM))/(WAVES(IWPTR)*WAVES(IWPTR))
            OUT(IELM)=TEMP*TEMP
            IWPTR=IWPTR+1
            IF (IWPTR.GT.NWAV) IWPTR=1
          END DO
        ELSE IF (OUTYPE.EQ.5) THEN
C         Conversion to AB magnitudes

            AMAX=FMIN
            DO IELM=1,NELM
               IF (IN(IELM).GT.0.) THEN
                 TEMP=(6.56-LOG10(SQRT(IN(IELM))))*SCALE*2.5
                 OUT(IELM)=TEMP*TEMP
                 AMAX=MAX(AMAX,OUT(IELM))
               ELSE
                 OUT(IELM)=FLAG
               END IF
            END DO
            DO IELM=1,NELM
               IF (OUT(IELM).EQ.FLAG) OUT(IELM)=AMAX
            END DO



        END IF


      ELSE IF (INTYPE.EQ.4) THEN
C       Input units in erg/s/cm**2/Angstrom
        SCALE=SCALE/2.998E-8
        IWPTR=1
        IF (OUTYPE.LE.3) THEN
C         Conversion to Jansky (or a subunit)
          DO IELM=1,NELM

            TEMP = SQRT(IN(IELM))*WAVES(IWPTR)*WAVES(IWPTR)*SCALE
            OUT(IELM)=TEMP*TEMP

            IWPTR=IWPTR+1
            IF (IWPTR.GT.NWAV) IWPTR=1
          END DO

        ELSE IF (OUTYPE.EQ.5) THEN
          IWPTR=1
          AMAX=FMIN
          DO IELM=1,NELM

          F_NU=SQRT(IN(IELM))*WAVES(IWPTR)*WAVES(IWPTR)*SCALE
          IWPTR=IWPTR+1
          IF (IWPTR.GT.NWAV) IWPTR=1
             IF (F_NU.GT.0.) THEN
                TEMP=(6.56-LOG10(F_NU))*2.5
                OUT(IELM)=TEMP*TEMP
                AMAX=MAX(AMAX,OUT(IELM))
             ELSE
                OUT(IELM)=FLAG
             END IF
          END DO
          DO IELM=1,NELM
             IF (OUT(IELM).EQ.FLAG) OUT(IELM)=AMAX
          END DO


        END IF

      ELSE IF (INTYPE.EQ.5) THEN
C       Input units in AB magnitudes
        IF (OUTYPE.LE.3) THEN
C         Conversion to Jansky (or a subunit)
          DO IELM=1,NELM


            TEMP = (10.0**(6.56-0.4*SQRT(IN(IELM))))*SCALE
            OUT(IELM)=TEMP*TEMP


          END DO

        ELSE IF (OUTYPE.EQ.4) THEN
C         Conversion to erg/s/cm**2/Angstrom
          SCALE=SCALE*2.998E-8
          IWPTR=1
          DO IELM=1,NELM

            F_NU=10.0**(6.56-0.4*SQRT(IN(IELM)))

            TEMP = SCALE*F_NU/(WAVES(IWPTR)*WAVES(IWPTR))
            OUT(IELM)=TEMP*TEMP

            IWPTR=IWPTR+1
            IF (IWPTR.GT.NWAV) IWPTR=1

          END DO
        END IF
      END IF
C
      END
