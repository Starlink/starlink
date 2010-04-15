C+
C                        D S A _ S T R U C T C
C
C  Routine name:
C     DSA_STRUCTC
C
C  Function:
C     Processes data into a contracted data structure.
C
C  Description:
C     Given an array of specified type, this routine copies it into
C     a contracted data structure, calculating suitable scaling and
C     offset factors.
C
C  Language:
C     FORTRAN
C
C  Call:
C      CALL DSA_STRUCTC (NAME,ELEMENTS,BASE_TYPE,BASE_ARRAY,
C                                         TYPE_CODE,ADDRESS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NAME       (Fixed string,descr) The DTA_ system name of the
C                    complex structure in question - ie the name of an
C                    object of type 'CSTRUCT'.
C     (>) ELEMENTS   (Integer,ref) The number of elements in the
C                    contracted structure array.
C     (>) BASE_TYPE  (Integer,ref) Integer code for the type of array
C                    used as the base array.  This should always be
C                    SHORT_TYPE.  (see TYPE_CODE).
C     (>) BASE_ARRAY (Integer,ref) The address of the base array.
C     (>) TYPE_CODE  (Integer,ref) Integer code for the type of array
C                    from which the contracted structure is to be produced.
C                    This is the internal DSA_ system code (ie one of the
C                    FLOAT_TYPE, INT_TYPE etc codes defined in the include
C                    file DSA_TYPES.INC).  This routine assumes TYPE_CODE
C                    is valid, and is not a character type.
C     (>) ADDRESS    (Integer,ref) The address of the data array to be
C                    converted.  That is, ADDRESS points to an array of type
C                    given by TYPE_CODE,  BASE_ARRAY points to one of
C                    type SHORT, and this routine calculates suitable values
C                    for BSCALE and BZERO and applies them to the latter
C                    to generate the former.
C     (!) STATUS     (Integer,ref) Status code.  If bad status is passed,
C                    this routine will return immediately.
C
C  External variables used:
C     Common variables internal to te DSA_ system.
C
C  External subroutines / functions used:
C     DTA_WRVARD, DTA_ERROR, ICH_CI, ICH_LEN, DSA_WRUSER, DSA_WRNAME
C     DSA_CSTRF, DSA_CSTRS, DSA_CSTRB, DSA_CSTRD, DSA_CSTRI
C
C  Prior requirements:
C     This routine is intended for call by DSA_UNMAP, which should
C     have done all the necessary prior processing.
C
C  Authors: Keith Shortridge, AAO
C           Malcolm J. Currie, Starlink
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (<) DTA_CODE  Laste DTA_ system error code
C
C  Subroutine / function details:
C     DTA_WRVARD    Set double precision value of a data object
C     DTA_ERROR     Get error message for a DTA_ error code
C     ICH_CI        Format an integer
C     ICH_LEN       Position of last non blank char in string
C     DSA_WRNAME    Output the full name of a structure to the user
C     DSA_WRUSER    Output message to user
C     DSA_CSTRx     Calculate and apply scaling factor and offset to array
C
C  History:
C     17th July 1987   Original version.  KS / AAO.
C     25th Apr  1989   Support for USHORT type added.  KS / AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C     2005 May 31       Use CNF_PVAL for pointers to mapped data. MJC
C+
      SUBROUTINE DSA_STRUCTC (NAME,ELEMENTS,BASE_TYPE,BASE_ARRAY,
     :                                       TYPE_CODE,ADDRESS,STATUS)
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters
C
      INTEGER ELEMENTS,BASE_TYPE,BASE_ARRAY,TYPE_CODE,ADDRESS,STATUS
      CHARACTER*(*) NAME
C
C     Functions used
C
      CHARACTER ICH_CI*12
      INTEGER ICH_LEN
C
C     DSA_ system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     DSA_ system common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      DOUBLE PRECISION BSCALE         ! Scaling factor for structure
      DOUBLE PRECISION BZERO          ! Offset value for structure
      INTEGER   DTA_STATUS            ! Status return from DTA_ routines
      CHARACTER ERROR*64              ! DTA_ system error text
      INTEGER   ERRORS                ! Number of conversion errors
      INTEGER   LENGTH                ! Number of non-blank chars in NAME
      CHARACTER NUMBER*12             ! Used to format ERRORS
      CHARACTER TYPE*8                ! Type of data in use
      CHARACTER STRING*80             ! Local string storage
C
C     DSA_ system type definitions.  Supplies FLOAT_TYPE, INT_TYPE etc,
C     as well as the array TYPE_NAMES.
C
      INCLUDE 'DSA_TYPES'
C
C     Immediate return if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     Make sure the input array type was correct
C
      LENGTH=ICH_LEN(NAME)
      IF (BASE_TYPE.NE.SHORT_TYPE) THEN
         CALL DSA_WRUSER('The contracted data structure ')
         CALL DSA_WRNAME(NAME)
         CALL DSA_WRUSER(' is based on an array of type ')
         TYPE=TYPE_NAMES(TYPE_CODE)
         CALL DSA_WRUSER(TYPE(:ICH_LEN(TYPE)))
         CALL DSA_WRUSER(', when it should be of type SHORT.')
         CALL DSA_WRFLUSH
         STATUS=DSA__INVTYP
         GO TO 500            ! Error exit
      END IF
C
C     Calculate and apply BSCALE and BZERO according to the type of the data.
C
      ERRORS=0
      IF (TYPE_CODE.EQ.FLOAT_TYPE) THEN
         CALL DSA_CSTRF (ELEMENTS,BSCALE,BZERO,
     :                   %VAL( CNF_PVAL(BASE_ARRAY) ),
     :                   %VAL( CNF_PVAL(ADDRESS) ),ERRORS)
      ELSE IF (TYPE_CODE.EQ.DOUBLE_TYPE) THEN
         CALL DSA_CSTRD (ELEMENTS,BSCALE,BZERO,
     :                   %VAL( CNF_PVAL(BASE_ARRAY) ),
     :                   %VAL( CNF_PVAL(ADDRESS) ),ERRORS)
      ELSE IF (TYPE_CODE.EQ.BYTE_TYPE) THEN
         CALL DSA_CSTRB (ELEMENTS,BSCALE,BZERO,
     :                   %VAL( CNF_PVAL(BASE_ARRAY) ),
     :                   %VAL( CNF_PVAL(ADDRESS) ),ERRORS)
      ELSE IF (TYPE_CODE.EQ.INT_TYPE) THEN
         CALL DSA_CSTRI (ELEMENTS,BSCALE,BZERO,
     :                   %VAL( CNF_PVAL(BASE_ARRAY) ),
     :                   %VAL( CNF_PVAL(ADDRESS) ),ERRORS)
      ELSE IF (TYPE_CODE.EQ.SHORT_TYPE) THEN
         CALL DSA_CSTRS (ELEMENTS,BSCALE,BZERO,
     :                   %VAL( CNF_PVAL(BASE_ARRAY) ),
     :                   %VAL( CNF_PVAL(ADDRESS) ),ERRORS)
      ELSE IF (TYPE_CODE.EQ.USHORT_TYPE) THEN
         CALL DSA_CSTRU (ELEMENTS,BSCALE,BZERO,
     :                   %VAL( CNF_PVAL(BASE_ARRAY) ),
     :                   %VAL( CNF_PVAL(ADDRESS) ),ERRORS)
      END IF
C
C     Log any conversion errors
C
      IF (ERRORS.NE.0) THEN
         CALL DSA_WRUSER('Note: ')
         NUMBER=ICH_CI(ERRORS)
         CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
         CALL DSA_WRUSER(
     :               ' numeric error(s) occurred while calculating ')
         CALL DSA_WRUSER('the scaling and offset values BSCALE and ')
         CALL DSA_WRUSER('BZERO to reconvert a ')
         TYPE=TYPE_NAMES(TYPE_CODE)
         CALL DSA_WRUSER(TYPE(:ICH_LEN(TYPE)))
         CALL DSA_WRUSER(' version of the contracted array ')
         CALL DSA_WRNAME(NAME)
         CALL DSA_WRUSER('.')
         CALL DSA_WRFLUSH
      END IF
C
C     Set the BSCALE and BZERO values from the structure.  (We get
C     them as double precision, to make sure we don't loose anything.)
C
      STRING=NAME(:LENGTH)//'.BSCALE'
      CALL DTA_WRVARD(STRING,1,BSCALE,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
         CALL DSA_WRUSER('Unable to set the scaling factor ')
         CALL DSA_WRUSER('"BSCALE" in the contracted data structure ')
         CALL DSA_WRNAME(NAME)
         CALL DTA_ERROR(DTA_STATUS,ERROR)
         CALL DSA_WRUSER('. ')
         CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
         CALL DSA_WRUSER('.')
         CALL DSA_WRFLUSH
         STATUS=DSA__DTAERR
         DTA_CODE=DTA_STATUS
      END IF
      STRING=NAME(:LENGTH)//'.BZERO'
      CALL DTA_WRVARD(STRING,1,BZERO,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
         CALL DSA_WRUSER('Unable to set the offset value ')
         CALL DSA_WRUSER('"BZERO" in the contracted data structure ')
         CALL DSA_WRNAME(NAME)
         CALL DTA_ERROR(DTA_STATUS,ERROR)
         CALL DSA_WRUSER('. ')
         CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
         CALL DSA_WRUSER('.')
         CALL DSA_WRFLUSH
         STATUS=DSA__DTAERR
         DTA_CODE=DTA_STATUS
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
