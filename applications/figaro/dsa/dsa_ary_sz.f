C+
C                         D S A _ A R R A Y _ S I Z E
C
C  Routine name:
C     DSA_ARRAY_SIZE
C
C  Function:
C     Returns the dimensions of a named data array.
C
C  Description:
C     Given the DTA_ system name of a data array, this routine
C     returns its dimensions.  At present, it can only handle
C     a limited number of structured array types. This routine
C     outputs no error messages, but does return an error string
C     if an error occurs.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_ARRAY_SIZE (NAME,MAXDIM,NDIM,DIMS,ERROR,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NAME       (Fixed string,descr) The DTA_ system name of the
C                    array in question.
C     (>) MAXDIM     (Integer,ref) The maximum number of dimensions
C                    for the data.
C     (<) NDIM       (Integer,ref) The actual number of dimensions in the
C                    array.
C     (>) DIMS       (Integer array,ref) The dimensions of the array.
C                    DIMS must have at least MAXDIM elements.
C     (<) ERROR      (Fixed string,descr) If an error occurs, ERROR
C                    is returned containing a description of the error.
C     (!) STATUS     (Integer,ref) Returned status value.  If a non-zero
C                    status value is passed, this routine returns immediately.
C
C  External variables used -
C     Only common variables used internally by the DSA_ package.
C
C  External subroutines / functions used:
C     DTA_ERROR, DTA_TYVAR, DTA_SZVAR, DTA_STRUC, ICH_LEN, DSA__STRUCT_ARRAY,
C     DSA__ARRAY_ORIGIN.
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C
C  Note:
C     This is an DSA_ system internal routine, and should not be
C     called directly from outside the DSA_ package.
C-
C  Common variable details:
C     (<) DTA_CODE     (Integer) Last DTA_ system error code.
C     (>) MAX_AXES     (Integer parameter) Maximum number of axes for data.
C
C  Subroutine / function details:
C     DTA_STRUC    Determines if a data object is a structure.
C     DTA_ERROR    Returns an error string given a DTA_ error code.
C     DTA_TYVAR    Returns the type of a data object.
C     DTA_SZVAR    Returns the dimensions of a data object.
C     ICH_LEN      Position of last non-blank char in string.
C     DSA__STRUCT_ARRAY  Get variant and actual array name for structure array.
C     DSA__ARRAY_ORIGIN  Get origin values for a structured array.
C
C  History:
C     22nd June 1987    Original version.  KS / AAO.
C     26th Feb  1990    Restriction on trailing blanks in NAME removed. KS/AAO.
C     15th Apr  1990    Support for SGP38 structured types added. KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_ARRAY_SIZE (NAME,MAXDIM,NDIM,DIMS,ERROR,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER MAXDIM, NDIM, DIMS(MAXDIM), STATUS
      CHARACTER*(*) NAME, ERROR
C
C     DSA_ system common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     Local variables
C
      CHARACTER ARRAY_NAME*80              ! Name of actual array in structure
      INTEGER   DTA_STATUS                 ! Status returned by DTA_ routines
      INTEGER   IDIM                       ! Index through array dimensions
      LOGICAL   KNOWN                      ! True if a known structure type
      INTEGER   LENAME                     ! # chars in ARRAY_NAME
      INTEGER   ODIM                       ! Number of origin values
      INTEGER   ORIGIN(MAX_AXES)           ! Origins for structured arrays
      LOGICAL   STRUCT                     ! True if named object a structure
      CHARACTER TYPE*16                    ! Type of named object
      CHARACTER VARIANT*16                 ! Variant for structured array
C
C     If bad status passed, return now.
C
      IF (STATUS.NE.0) RETURN
C
C     See if the object is primitive or not
C
      CALL DTA_STRUC (NAME,STRUCT,DTA_STATUS)
      IF (DTA_STATUS.NE.0) GO TO 500
C
C     If it is, get its dimensions
C
      IF (.NOT.STRUCT) THEN
         CALL DTA_SZVAR (NAME,MAXDIM,NDIM,DIMS,DTA_STATUS)
         IF (DTA_STATUS.NE.0) GO TO 500
      ELSE
C
C        If it isn't, see if we know the type
C
         CALL DTA_TYVAR (NAME,TYPE,DTA_STATUS)
         IF (DTA_STATUS.NE.0) GO TO 500
C
C        Look to see if this might be a structured array of a type we
C        know about.  If it is, get the name of its actual array object.
C
         CALL DSA__STRUCT_ARRAY (NAME,ICH_LEN(NAME),TYPE,VARIANT,
     :                                ARRAY_NAME,LENAME,KNOWN,STATUS)
         IF (STATUS.NE.0) GO TO 500
         IF (KNOWN) THEN
            CALL DTA_SZVAR(ARRAY_NAME(:LENAME),MAXDIM,NDIM,
     :                                               DIMS,DTA_STATUS)
            IF (DTA_STATUS.NE.0) GO TO 500
C
C           A structured array may have origin values.  Attempt to
C           apply these.  If the origin is positive, we can potentially
C           process the array into its full size.  If its 0 or -ve, for
C           some fancy reason like fourier data, we can't handle that here.
C
            CALL DSA__ARRAY_ORIGIN (NAME,ICH_LEN(NAME),TYPE,MAX_AXES,
     :                                              ORIGIN,ODIM,STATUS)
            IF (STATUS.NE.0) GO TO 500
            DO IDIM=1,NDIM
               IF (ORIGIN(IDIM).GT.1) THEN
                  DIMS(IDIM)=DIMS(IDIM)+ORIGIN(IDIM)
               END IF
            END DO
         ELSE
            STATUS=DSA__NOARRY
            ERROR='Structure type '//TYPE(:ICH_LEN(TYPE))//
     :                             ' not recognised as an array'
         END IF
      END IF
C
C     On way out, see if an error code needs an error string generating.
C
  500 CONTINUE
      IF (DTA_STATUS.NE.0) THEN
         STATUS=DSA__DTAERR
         DTA_CODE=DTA_STATUS
         CALL DTA_ERROR(DTA_STATUS,ERROR)
      END IF
C
      END
