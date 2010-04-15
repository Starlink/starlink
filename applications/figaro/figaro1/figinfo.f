C+
C                            F I G I N F O
C
C  Description:
C     FIGINFO provides a way of looking at the contents of a Figaro data
C     file through Figaro's eyes. You can do an EXAM of file, but this
C     doesn't necessarily tell you how Figaro will interpret what you
C     find there, particularly in the case of awkward things like the
C     flag that indicates whether or not the file's main data array may
C     contain flagged data values (which doesn't necessarily mean that
C     it does, just that it might). This particular flag can be a
C     problem for Figaro files, partly because the default rules - how
C     you interpret its absence - is different in the two data formats,
C     NDF and .DST.  If it is set when the file does not in fact contain
C     flagged values then  processing the file can be inefficient,
C     particularly for large files. If the file does contain flagged
C     data values but the flag is not set, then very odd results can be
C     obtained when the file is processed. FIGINFO uses the same file
C     access routines as a normal Figaro program to interpret the file
C     contents. It also provides a couple of options for manipulating
C     the 'may contain flagged data values' flag, should it be mis-set.
C
C  Parameters:
C
C     INPUT     (Character) Is the name of the file to be checked.
C
C  Keywords:
C
C     CHECK_FLAGS   If set, FIGINFO reads the data in the main data
C                   array to see if it does in fact contain flagged data
C                   values. If it does not, the 'may contain flagged
C                   data values' flag is cleared. (This option is only
C                   offered if the flag was initially set.)
C     CLEAR_FLAG    A DANGEROUS option that clears the 'may contain
C                   flagged data values' flag without testing the
C                   actual data. (This option is only offered if the
C                   flag was initially set and CHECK_FLAGS was not
C                   specified.)
C     SET_FLAG      A relatively safe option that sets the 'may contain
C                   flagged data values' flag. The actual data is not
C                   tested. (This option is only offered if the flag was
C                   not initially set.)
C-
C  History:
C     31st Jan 1995  Original version. KS/AAO.
C     13th Mar 1996  Adapt for FDA library.
C                    Input read-only. Use DSA_INPUT_UPDATE.
C     2005 June 7    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+

      SUBROUTINE FIGINFO
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
CC
C     Functions used
C
      CHARACTER ICH_CI*13
      INTEGER   ICH_LEN
C
C     Local variables
C
      CHARACTER ACCESS*1         ! Access code for FITS item - ignored
      LOGICAL   CHECK            ! Value of CHECK parameter
      LOGICAL   CLEAR_FLAG       ! Indicates flagged data flag to be cleared
      INTEGER   DIMS(10)         ! Dimensions of data array
      DOUBLE PRECISION DOUBLE_FLAG! Flagged value used for flagged data
      INTEGER   DPTR             ! Dynamic memory element for main data array
      INTEGER   ELEMENTS         ! Number of elements in FITS item - ignored
      LOGICAL   ERRORS           ! True if error array present
      LOGICAL   EXIST            ! True if data item exists
      LOGICAL   FLAGS_EXIST      ! True if data values are flagged
      REAL      FLOAT_FLAG       ! Flagged value used for flagged data
      INTEGER   IDIM             ! Index through axes
      INTEGER   IGNORE           ! Used to ignore uninteresting status
      INTEGER   INT_FLAG         ! Flagged value used for flagged data
      INTEGER   IPT              ! Pointer to character in STRING
      CHARACTER ITEM*16          ! Name of FITS keyword - ignored
      INTEGER   NDIM             ! Dimensionality of data array
      INTEGER   NELM             ! Number of elements in data array
      INTEGER   NFLAGGED         ! Number of flagged values in data array
      LOGICAL   NONE             ! True if no error information available
      CHARACTER NUMBER*13        ! Used to format a number
      LOGICAL   SET_FLAG         ! Indicates flagged data flag to be set
      INTEGER*2 SHORT_FLAG       ! Flagged value used for flagged data
      LOGICAL   SINGLE           ! True if width value is fixed - ignored
      INTEGER   SLOT             ! Map slot used for main data array
      INTEGER   STATUS           ! Inherited status variable
      CHARACTER STRING*256       ! Used to build up report string
      INTEGER   STRLEN           ! Length of FITS character item - ignored
      LOGICAL   STRUCT           ! True if main data array is structured
      CHARACTER STRUCT_TYPE*32   ! Type of main array data if structured
      CHARACTER TYPE*32          ! Type to use for main data array data
      LOGICAL   VARIANCE         ! True if variance array exists
      DOUBLE PRECISION WIDTH     ! Fixed axis width value - ignored
C
C     Open input file.
C
      STATUS=0
      CALL DSA_OPEN (STATUS)
      CALL DSA_INPUT_UPDATE ('INPUT','INPUT',STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL PAR_WRUSER(' ',IGNORE)
C
C     Check out the main data array. Report its size.
C
      CALL DSA_DATA_SIZE ('INPUT',10,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GO TO 500
      STRING='Main data array has dimensions '
      IPT=ICH_LEN(STRING)+2
      CALL DSA_ENCDIM(STRING,NDIM,DIMS,IPT)
      CALL PAR_WRUSER (STRING(:IPT),IGNORE)
C
C     Now see if the data array is complex
C
      CALL DSA_SEEK_IMAGINARY ('INPUT',EXIST,STATUS)
      IF (STATUS.NE.0) GO TO 500
      IF (EXIST) THEN
         CALL PAR_WRUSER(
     :     'The data array is complex (it has an imaginary part)',
     :                                                        IGNORE)
      END IF
C
C     Now look for any error information
C
      CALL DSA_ERROR_INFORMATION ('INPUT',ERRORS,VARIANCE,NONE,STATUS)
      IF (STATUS.NE.0) GO TO 500
      IF (ERRORS) THEN
         CALL PAR_WRUSER(
     :   'The file contains error information in an uncertainty array',
     :                                                          IGNORE)
      END IF
      IF (VARIANCE) THEN
         CALL PAR_WRUSER(
     :   'The file contains error information in a variance array',
     :                                                          IGNORE)
      END IF
      IF (NONE) THEN
         CALL PAR_WRUSER ('The file has no error information',IGNORE)
      END IF
C
C     Look for a quality array
C
      CALL DSA_SEEK_QUALITY ('INPUT',EXIST,STATUS)
      IF (STATUS.NE.0) GO TO 500
      IF (EXIST) THEN
         CALL PAR_WRUSER('The file contains a separate quality array',
     :                                                         IGNORE)
      ELSE
         CALL PAR_WRUSER ('The file has no separate quality array',
     :                                                         IGNORE)
      END IF
C
C     Look for flagged data values
C
      CALL DSA_SEEK_FLAGGED_VALUES ('INPUT',FLAGS_EXIST,STATUS)
      IF (STATUS.NE.0) GO TO 500
      IF (FLAGS_EXIST) THEN
         CALL PAR_WRUSER(
     :        'The main data array MAY contain flagged data values',
     :                                                         IGNORE)
      ELSE
         CALL PAR_WRUSER ('No flagged data values present',IGNORE)
      END IF
C
C     See if there is a FITS structure present
C
      CALL DSA_NTH_FITS_ITEM ('INPUT',1,EXIST,ITEM,ACCESS,ELEMENTS,
     :                                               STRLEN,STATUS)
      IF (STATUS.NE.0) GO TO 500
      IF (EXIST) THEN
         CALL PAR_WRUSER('FITS keywords are present in the file',IGNORE)
      ELSE
         CALL PAR_WRUSER ('No FITS keywords present',IGNORE)
      END IF
C
C     Loop through the various axes
C
      DO IDIM=1,NDIM
         CALL PAR_WRUSER(' ',IGNORE)
         CALL PAR_WRUSER('Looking at information for axis '//
     :                                              ICH_CI(IDIM),IGNORE)
C
C        See if there is an axis data array
C
         CALL DSA_SEEK_AXIS ('INPUT',IDIM,EXIST,STATUS)
         IF (STATUS.NE.0) GO TO 500
         IF (EXIST) THEN
            CALL PAR_WRUSER('There is an axis data array present',
     :                                                         IGNORE)
         ELSE
            CALL PAR_WRUSER ('There is no axis data array present',
     :                                                         IGNORE)
         END IF
C
C        See if there is width information associated with this axis
C        - this is sufficiently unusual that we don't mention it
C        unless we actually find it.
C
         CALL DSA_SEEK_WIDTH ('INPUT',IDIM,EXIST,SINGLE,WIDTH,STATUS)
         IF (STATUS.NE.0) GO TO 500
         IF (EXIST) THEN
            CALL PAR_WRUSER(
     :         'There is axis ''width'' information present',IGNORE)
         END IF
      END DO
C
      CALL PAR_WRUSER (' ',IGNORE)
C
C     If the file is flagged as 'may have flagged data values' then
C     it may be worth checking to see if it really does have them. If
C     it doesn't, then processing them can be a significant waste of time.
C     If we said we couldn't handle the flagged values and mapped the
C     data array, the DSA system would pass through the array remembering
C     the location of any flagged pixels and removing the actual flagged
C     data value. If it then didn't find any, it would then remove the flag
C     indicating the possible presence of flagged data values. However, to
C     do this it uses a lot of temporary storage and this can be very heavy
C     with really large data cubes, for example. We can do it more simply
C     ourselves.
C
      IF (FLAGS_EXIST) THEN
C
C        There may be flagged data values in the file. See if we are
C        to check for them.
C
         CLEAR_FLAG=.FALSE.
         CALL PAR_RDKEY('CHECK_FLAGS',.FALSE.,CHECK)
         IF (CHECK) THEN
C
C           We are. First we tell the system that we can handle flagged
C           values - this means we will get the raw data array from DSA
C           with no messing about, so this should be very efficient. We
C           find out the data type to use for the file and map using that
C           type.
C
            CALL DSA_USE_FLAGGED_VALUES ('INPUT',STATUS)
            CALL DSA_DATA_TYPE ('INPUT',TYPE,STRUCT,STATUS)
            IF (STRUCT) THEN
               STRUCT_TYPE=TYPE
               CALL DSA_PREFERRED_TYPE (STRUCT_TYPE,TYPE,STATUS)
            END IF
            IF ((TYPE.NE.'FLOAT').AND.(TYPE.NE.'DOUBLE')
     :               .AND.(TYPE.NE.'SHORT').AND.(TYPE.NE.'INT')
     :                           .AND.(TYPE.NE.'USHORT')) TYPE='INT'
            CALL DSA_MAP_DATA ('INPUT','READ',TYPE,DPTR,SLOT,STATUS)
            IF (STATUS.NE.0) GO TO 500
C
C           Now we handle the data array using its natural type. (We
C           don't bother with 'CHAR', which we expect to be an unlikely
C           data type, and 'BYTE' is hard to handle in Fortran. We treat
C           'USHORT' the same as we do 'SHORT' - we're only checking
C           the data for equality, so the intrepretation of the sign
C           bit doesn't matter.
C
            IF (TYPE.EQ.'FLOAT') THEN
               CALL DSA_GET_FLAG_VALUE (TYPE,FLOAT_FLAG,STATUS)
               CALL FIGINFO_CHECK_F (%VAL(CNF_PVAL(DPTR)),NELM,
     :                               FLOAT_FLAG,NFLAGGED)
            ELSE IF (TYPE.EQ.'DOUBLE') THEN
               CALL DSA_GET_FLAG_VALUE (TYPE,DOUBLE_FLAG,STATUS)
               CALL FIGINFO_CHECK_D (%VAL(CNF_PVAL(DPTR)),NELM,
     :                               DOUBLE_FLAG,NFLAGGED)
            ELSE IF ((TYPE.EQ.'SHORT').OR.(TYPE.EQ.'USHORT')) THEN
               CALL DSA_GET_FLAG_VALUE (TYPE,SHORT_FLAG,STATUS)
               CALL FIGINFO_CHECK_S (%VAL(CNF_PVAL(DPTR)),NELM,INT_FLAG,
     :                               NFLAGGED)
            ELSE IF (TYPE.EQ.'INT') THEN
               CALL DSA_GET_FLAG_VALUE (TYPE,INT_FLAG,STATUS)
               CALL FIGINFO_CHECK_I (%VAL(CNF_PVAL(DPTR)),NELM,INT_FLAG,
     :                               NFLAGGED)
            END IF
            CALL DSA_UNMAP(SLOT,STATUS)
            IF (NFLAGGED.EQ.0) THEN
               CALL PAR_WRUSER (
     :           'Data array did not in fact contain flagged data',
     :                                                        IGNORE)
               CLEAR_FLAG=.TRUE.
            ELSE
               NUMBER=ICH_CI(NFLAGGED)
               CALL PAR_WRUSER ('Data array contained '//
     :             NUMBER(:ICH_LEN(NUMBER))//' flagged data value(s)',
     :                                                        IGNORE)
            END IF
C
         ELSE
C
C           If the CHECK option was not taken, we can still offer to
C           clear the 'may have flagged data' flag. This should be
C           regarded as a dangerous thing to do, and is only justified in
C           cases where the data array is so huge that we can justify
C           avoiding the overhead of checking it. (This should be a
C           hidden parameter!)
C
            CALL PAR_RDKEY ('CLEAR_FLAG',.FALSE.,CLEAR_FLAG)
C
         END IF
C
C        At the end of this, we may have CLEAR_FLAG set, either
C        because there were no flagged data values in the data, or
C        because the CLEAR_FLAG keyword was specified. Either way, we
C        clear the flag.
C
         IF (CLEAR_FLAG) THEN
            CALL DSA_SET_FLAGGED_VALUES ('INPUT',.FALSE.,STATUS)
            IF (STATUS.NE.0) GO TO 500
            CALL PAR_WRUSER ('Flagged data flag has been reset',
     :                                                        IGNORE)

         END IF
C
      ELSE
C
C        The 'may contain flagged data' flag is not set. There is always
C        the off-chance that it does contain flagged data and the flag was
C        cleared by accident (a program bug, or someone using CLEAR_FLAG
C        when they shouldn't!). So we have the option to reset it.
C
         CALL PAR_RDKEY ('SET_FLAG',.FALSE.,SET_FLAG)
         IF (SET_FLAG) THEN
            CALL DSA_SET_FLAGGED_VALUES ('INPUT',.TRUE.,STATUS)
            IF (STATUS.NE.0) GO TO 500
            CALL PAR_WRUSER ('Flagged data flag has been set',IGNORE)
         END IF

      END IF
C
C     Exit
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END

      SUBROUTINE FIGINFO_CHECK_I (ARRAY,NELM,FLAG,NFLAGGED)
C
C     Utility routine for FIGINFO. This routine counts the number
C     of the NELM elements in the integer array ARRAY actually contain
C     the flag value FLAG. This number is returned in NFLAGGED.
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      INTEGER ARRAY(NELM),FLAG
      INTEGER NFLAGGED
C
C     Local variables
C
      INTEGER I
C
      NFLAGGED=0
      DO I=1,NELM
         IF (ARRAY(I).EQ.FLAG) NFLAGGED=NFLAGGED+1
      END DO
C
      END

      SUBROUTINE FIGINFO_CHECK_D (ARRAY,NELM,FLAG,NFLAGGED)
C
C     Utility routine for FIGINFO. This routine counts the number
C     of the NELM elements in the double precision array ARRAY actually
C     contain the flag value FLAG. This number is returned in NFLAGGED.
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      DOUBLE PRECISION ARRAY(NELM),FLAG
      INTEGER NFLAGGED
C
C     Local variables
C
      INTEGER I
C
      NFLAGGED=0
      DO I=1,NELM
         IF (ARRAY(I).EQ.FLAG) NFLAGGED=NFLAGGED+1
      END DO
C
      END

      SUBROUTINE FIGINFO_CHECK_F (ARRAY,NELM,FLAG,NFLAGGED)
C
C     Utility routine for FIGINFO. This routine counts the number
C     of the NELM elements in the real array ARRAY actually contain
C     the flag value FLAG. This number is returned in NFLAGGED.
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      REAL ARRAY(NELM),FLAG
      INTEGER NFLAGGED
C
C     Local variables
C
      INTEGER I
C
      NFLAGGED=0
      DO I=1,NELM
         IF (ARRAY(I).EQ.FLAG) NFLAGGED=NFLAGGED+1
      END DO
C
      END

      SUBROUTINE FIGINFO_CHECK_S (ARRAY,NELM,FLAG,NFLAGGED)
C
C     Utility routine for FIGINFO. This routine counts the number
C     of the NELM elements in the integer*2 array ARRAY actually contain
C     the flag value FLAG. This number is returned in NFLAGGED.
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      INTEGER*2 ARRAY(NELM),FLAG
      INTEGER NFLAGGED
C
C     Local variables
C
      INTEGER I
C
      NFLAGGED=0
      DO I=1,NELM
         IF (ARRAY(I).EQ.FLAG) NFLAGGED=NFLAGGED+1
      END DO
C
      END
