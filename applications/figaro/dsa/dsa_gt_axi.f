C+
C                      D S A _ G E T _ A X I S _ I N F O
C
C  Routine name:
C     DSA_GET_AXIS_INFO
C
C  Function:
C     Obtains information held in an axis structure.
C
C  Description:
C     Obtains a number of items from an axis structure.  If these items
C     do not in fact exist, then character items are returned as blank,
C     and numeric items are returned as zero.  At present, only a very
C     limited number of items are supported.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_GET_AXIS_INFO (REF_NAME,AXIS,CHAR_ITEMS,CHAR_ARRAY,
C                                          NUM_ITEMS,NUM_ARRAY,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME      (Fixed string,descr) The reference name used for
C                       the input structure whose axis is in question.
C     (>) AXIS          (Integer,ref) The number of the axis.
C     (>) CHAR_ITEMS    (Integer,ref) The number of character items to
C                       be returned.
C     (<) CHAR_ARRAY    (Fixed string array,descr) The character items
C                       returned.  CHAR_ARRAY(1) is the axis units,
C                       and CHAR_ARRAY(2) is the axis label.  At present,
C                       only those are supported.
C     (>) NUM_ITEMS     (Integer,ref) The number of numeric items to
C                       be returned.
C     (<) NUM_ARRAY     (Double precision array,ref) The numeric items
C                       returned.  At present only NUM_ARRAY(1) is
C                       supported, this being the `log binning' flag
C                       (non-zero => `log binning' flag set).
C     (!) STATUS        (Integer,ref) Status code.  If bad status is
C                       passed, this routine returns immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     DSA_FIND_REF, DSA_WRUSER, DSA_VALIDATE_AXIS,
C     DSA__AXIS_LFLAG_NAME, DSA__AXIS_UNITS_NAME, DSA__AXIS_LABEL_NAME,
C     ICH_FOLD, DTA_SZVAR, DTA_RDVARC, DTA_RDVARD, ICH_CLEAN
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_CLEAN     Clip a string at first non-printing character.
C     ICH_FOLD      Convert a string to upper case.
C     DSA_FIND_REF  Look up reference name in common tables.
C     DSA_WRUSER    Output a message string to the user.
C     DTA_SZVAR     Get dimensions of a data object
C     DTA_RDVARC    Read a character data object
C     DTA_RDVARD    Read a double presision data object
C     DSA_VALIDATE_AXIS    Make sure than an axis number is valid.
C     DSA__AXIS_LFLAG_NAME Get name of data object used for log binning flag.
C     DSA__AXIS_LABEL_NAME Get name of data object used for axis label.
C     DSA__AXIS_UNITS_NAME Get name of data object used for axis units.
C
C  History:
C     8th  July 1987  Original version.  KS / AAO.
C     15th Dec  1989  Added log binning flag to NUM_ARRAY.  Recoded to
C                     use DSA__ routines to get data object names.  KS / AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C     12th Oct 1992   There seems to be a problem for DAT_GET (in
C                     DTA_RDVAR) to get a _DOUBLE from an _INTEGER.
C                     Here we try to circumvent the problem by using
C                     DTA_RDVARI.  HME / UoE, Starlink.
C+
      SUBROUTINE DSA_GET_AXIS_INFO (REF_NAME,AXIS,CHAR_ITEMS,CHAR_ARRAY,
     :                                       NUM_ITEMS,NUM_ARRAY,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER AXIS, CHAR_ITEMS, NUM_ITEMS, STATUS
      DOUBLE PRECISION NUM_ARRAY(*)
      CHARACTER*(*) REF_NAME, CHAR_ARRAY(*)
C
C     Functions used
C
      INTEGER   ICH_CLEAN, ICH_FOLD
C
C     Local variables
C
      INTEGER   DTA_STATUS                  ! Status from DTA_ routines
      INTEGER   I                           ! Loop variable
      INTEGER   INVOKE                      ! Dummy function value
      INTEGER   LENGTH                      ! Object name length
      CHARACTER NAME*128                    ! Name of axis objects
      INTEGER   NCH                         ! Number of characters in string
      INTEGER   NDIM                        ! Dimensions of axis objects
      CHARACTER OBJ_NAME*32                 ! DTA_ name of data object - ignored
      CHARACTER REF_NAME_UC*32              ! Upper case version of ref_name
      INTEGER   REF_SLOT                    ! Reference table slot #
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the tables.
C
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
      CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Make sure the value of AXIS is legal.
C
      CALL DSA_VALIDATE_AXIS (AXIS,REF_NAME_UC,STATUS)
      IF (STATUS.NE.0) GO TO 500     ! Error exit
C
C     Start by blanking out all the character items
C
      DO I=1,CHAR_ITEMS
         CHAR_ARRAY(I)=' '
      END DO
C
C     Only one numeric item, the log binning flag, is supported.
C
      IF (NUM_ITEMS.GE.1) THEN
         CALL DSA__AXIS_LFLAG_NAME (REF_SLOT,AXIS,NAME,LENGTH)
         CALL DTA_RDVARI(NAME,1,I,DTA_STATUS)
         NUM_ARRAY(1)=FLOAT(I)
         IF (DTA_STATUS.NE.0) NUM_ARRAY(1)=0.0
      END IF
C
C     Get the axis units
C
      IF (CHAR_ITEMS.GE.1) THEN
         CALL DSA__AXIS_UNITS_NAME (REF_SLOT,AXIS,NAME,LENGTH)
         CALL DTA_SZVAR(NAME,1,NDIM,NCH,DTA_STATUS)
         IF (DTA_STATUS.EQ.0) THEN
            NCH=MIN(NCH,LEN(CHAR_ARRAY(1)))
            CALL DTA_RDVARC(NAME,NCH,CHAR_ARRAY(1),DTA_STATUS)
            INVOKE=ICH_CLEAN(CHAR_ARRAY(1))
        END IF
      END IF
C
C     Get the axis label
C
      IF (CHAR_ITEMS.GE.2) THEN
         CALL DSA__AXIS_LABEL_NAME (REF_SLOT,AXIS,NAME,LENGTH)
         CALL DTA_SZVAR(NAME,1,NDIM,NCH,DTA_STATUS)
         IF (DTA_STATUS.EQ.0) THEN
            NCH=MIN(NCH,LEN(CHAR_ARRAY(2)))
            CALL DTA_RDVARC(NAME,NCH,CHAR_ARRAY(2),DTA_STATUS)
            INVOKE=ICH_CLEAN(CHAR_ARRAY(2))
         END IF
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
