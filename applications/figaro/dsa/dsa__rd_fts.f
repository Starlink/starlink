C+
C                      D S A _ _ R E A D _ F I T S _ {x}
C
C  Routine name:
C     DSA__READ_FITS_{x}
C
C  Function:
C     Read the actual value of a FITS item.
C
C  Routines:
C     This covers a set of routines, one for each of a set of data
C     types: DSA__READ_FITS_C (character string), DSA__READ_FITS_D (double
C     precision), DSA__READ_FITS_F (single precision floating point),
C     DSA__READ_FITS_L (logical), DSA__READ_FITS_S (short integer), and
C     DSA__READ_FITS_I (integer).
C
C  Description:
C     This is a set of utility routines for use with DSA_GET_FITS_{x}.
C     Once DSA__PRE_GET_FITS had been called, this routine uses the
C     information it returns to get the actual value of the FITS item
C     specified.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__READ_FITS_{x} (STRING,CODE,REF_NAME,ITEM,VALUE,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) STRING          (Fixed string,descr) The DTA_ name
C                         of the object used for the FITS value, for
C                         original Figaro format data, as a formatted
C                         version of the data for NDF formatted data.
C     (>) CODE            (Integer, ref) A code indicating the type of
C                         data structure holding the FITS items.
C                         CODE and STRING should be as returned by
C                         DSA__PRE_GET_FITS.
C     (>) REF_NAME        (Fixed string,descr) The reference name
C                         used to identify the structure.  This is only
C                         used to generate error messages.
C     (>) ITEM            (Fixed string,descr) The name of the item
C                         in question - (this is a FITS keyword).
C                         Case-insensitive. This is only used to
C                         generate error messages.
C     (<) VALUE           (Any, ref/descr) The value of the item
C                         in question.   DSA__READ_FITS_C expects a
C                         string passed by descriptor, all others
C                         expect a logical or numeric quantity passed
C                         by reference.
C     (!) STATUS          (Integer,ref) Status value.  If non-zero
C                         status is passed to it, this routine will
C                         return immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     DSA_POST_GET_FITS, DSA_GET_FITS_ERR, ICH_NUMBR, ICH_NUMBD,
C     DTA_SZVAR, DTA_RDVARB, DTA_RDVARD, DTA_RDVARF, DTA_RDVARI,
C     DTA_RDVARS, DTA_RDVARC
C
C  Version date: 31st August 1992.
C
C  Prior requirements:
C     DSA__PRE_GET_FITS should have been called to determin the values
C     of CODE and STRING.
C
C  Authors: Keith Shortridge, AAO
C           Horst Meyerdierks, UoE, Starlink
C-
C  Subroutine / function details:
C     DSA_GET_FITS_ERR      Output error message on decoding error
C     DSA_POST_GET_FITS     Test DTA_ status after read from FITS structure
C     ICH_NUMBR             Decode a string into a single precision number
C     ICH_NUMBD             Decode a string into a double precision number
C     DTA_SZVAR             Get size of a data object
C     DTA_RDVAR{x}          Read an item from a data object
C
C  History:
C     7th  Feb 1990  Original version.  KS / AAO.
C     31st Aug 1992  Change error message in ..._S to refer to signed
C                    integers, rather than unsigned. HME / UoE, Starlink.
C+
      SUBROUTINE DSA__READ_FITS_L (STRING,CODE,REF_NAME,ITEM,
     :                                                 VALUE,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL VALUE
      INTEGER CODE, STATUS
      CHARACTER*(*) STRING, REF_NAME, ITEM
C
C     DSA common and parameter definitions
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      BYTE      BVAL            ! Byte value used in structure for logicals
      CHARACTER CHAR*1          ! First character of STRING.
      INTEGER   DTA_STATUS      ! Status returned by DTA routine
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
      IF (CODE.EQ.NDF_CODE) THEN
         CHAR=STRING(1:1)
         IF ((CHAR.EQ.'T').OR.(CHAR.EQ.'t')) THEN
            VALUE=.TRUE.
         ELSE IF ((CHAR.EQ.'Y').OR.(CHAR.EQ.'y')) THEN
            VALUE=.TRUE.
         ELSE
            VALUE=.FALSE.
         END IF
      ELSE
         CALL DTA_RDVARB (STRING,1,BVAL,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_POST_GET_FITS(DTA_STATUS,STRING,STATUS)
         END IF
         VALUE=BVAL.NE.0
      END IF
C
      END

      SUBROUTINE DSA__READ_FITS_F (STRING,CODE,REF_NAME,ITEM,
     :                                                VALUE,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER CODE, STATUS
      REAL    VALUE
      CHARACTER*(*) STRING, REF_NAME, ITEM
C
C     Functions
C
      INTEGER ICH_NUMBR
C
C     DSA common and parameter definitions
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   DTA_STATUS      ! Status returned by DTA routine
      INTEGER   NEXT            ! Next position in string - ignored
      INTEGER   NUM_STATUS      ! Status of attempt to decode string into number
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
      IF (CODE.EQ.NDF_CODE) THEN
         NUM_STATUS=ICH_NUMBR(STRING,1,' ',VALUE,NEXT)
         IF (NUM_STATUS.NE.0) THEN
            CALL DSA_GET_FITS_ERR (STRING,'is not a valid number',
     :                                            REF_NAME,ITEM,STATUS)
         END IF
      ELSE
         CALL DTA_RDVARF (STRING,1,VALUE,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_POST_GET_FITS(DTA_STATUS,STRING,STATUS)
         END IF
      END IF
C
      END

      SUBROUTINE DSA__READ_FITS_D (STRING,CODE,REF_NAME,ITEM,
     :                                                VALUE,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER CODE, STATUS
      DOUBLE PRECISION VALUE
      CHARACTER*(*) STRING, REF_NAME, ITEM
C
C     Functions
C
      INTEGER ICH_NUMBD
C
C     DSA common and parameter definitions
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   DTA_STATUS      ! Status returned by DTA routine
      INTEGER   NEXT            ! Next position in string - ignored
      INTEGER   NSFIG           ! Number of significant figures - ignored
      INTEGER   NUM_STATUS      ! Status of attempt to decode string into number
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
      IF (CODE.EQ.NDF_CODE) THEN
         NUM_STATUS=ICH_NUMBD(STRING,1,' ',VALUE,NSFIG,NEXT)
         IF (NUM_STATUS.NE.0) THEN
            CALL DSA_GET_FITS_ERR (STRING,'is not a valid number',
     :                                            REF_NAME,ITEM,STATUS)
         END IF
      ELSE
         CALL DTA_RDVARD (STRING,1,VALUE,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_POST_GET_FITS(DTA_STATUS,STRING,STATUS)
         END IF
      END IF
C
      END

      SUBROUTINE DSA__READ_FITS_I (STRING,CODE,REF_NAME,ITEM,
     :                                                VALUE,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER CODE, STATUS, VALUE
      CHARACTER*(*) STRING, REF_NAME, ITEM
C
C     Functions
C
      INTEGER ICH_NUMBD
C
C     DSA common and parameter definitions
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   DTA_STATUS      ! Status returned by DTA routine
      INTEGER   NEXT            ! Next position in string - ignored
      INTEGER   NSFIG           ! Number of significant figures - ignored
      INTEGER   NUM_STATUS      ! Status of attempt to decode string into number
      DOUBLE    PRECISION DVALUE! Real number decoded from string
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
      IF (CODE.EQ.NDF_CODE) THEN
         NUM_STATUS=ICH_NUMBD(STRING,1,' ',DVALUE,NSFIG,NEXT)
         IF (NUM_STATUS.NE.0) THEN
            CALL DSA_GET_FITS_ERR (STRING,'is not a valid number',
     :                                            REF_NAME,ITEM,STATUS)
         END IF
         VALUE=NINT(DVALUE)
      ELSE
         CALL DTA_RDVARI (STRING,1,VALUE,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_POST_GET_FITS(DTA_STATUS,STRING,STATUS)
         END IF
      END IF
C
      END

      SUBROUTINE DSA__READ_FITS_S (STRING,CODE,REF_NAME,ITEM,
     :                                                VALUE,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER CODE, STATUS
      INTEGER*2 VALUE
      CHARACTER*(*) STRING, REF_NAME, ITEM
C
C     Functions
C
      INTEGER ICH_NUMBR
C
C     DSA common and parameter definitions
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   DTA_STATUS      ! Status returned by DTA routine
      INTEGER   NEXT            ! Next position in string - ignored
      INTEGER   NUM_STATUS      ! Status of attempt to decode string into number
      REAL      RVALUE          ! Real number decoded from string
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
      IF (CODE.EQ.NDF_CODE) THEN
         NUM_STATUS=ICH_NUMBR(STRING,1,' ',RVALUE,NEXT)
         IF (NUM_STATUS.NE.0) THEN
            CALL DSA_GET_FITS_ERR (STRING,'is not a valid number',
     :                                            REF_NAME,ITEM,STATUS)
         END IF
         IF ((RVALUE.LT.-32768.0).OR.(RVALUE.GT.32767.0)) THEN
            CALL DSA_GET_FITS_ERR (STRING,
     :                    'cannot be read as a signed short integer',
     :                                            REF_NAME,ITEM,STATUS)
            VALUE=0
         ELSE
            VALUE=NINT(RVALUE)
         END IF
      ELSE
         CALL DTA_RDVARS (STRING,1,VALUE,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_POST_GET_FITS(DTA_STATUS,STRING,STATUS)
         END IF
      END IF
C
      END

      SUBROUTINE DSA__READ_FITS_C (STRING,CODE,REF_NAME,ITEM,
     :                                                VALUE,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER CODE, STATUS
      CHARACTER*(*) STRING, REF_NAME, ITEM, VALUE
C
C     DSA common and parameter definitions
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   DIMS(2)         ! Used to get dimensions of string item
      INTEGER   DTA_STATUS      ! Status returned by DTA routine
      INTEGER   NCHAR           ! Characters read from string
      INTEGER   NDIM            ! Dimensions of string
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
      IF (CODE.EQ.NDF_CODE) THEN
         VALUE=STRING
      ELSE
         CALL DTA_SZVAR (STRING,2,NDIM,DIMS,DTA_STATUS)
         NCHAR=MIN(DIMS(1),LEN(VALUE))
         CALL DTA_RDVARC (STRING,NCHAR,VALUE,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_POST_GET_FITS(DTA_STATUS,STRING,STATUS)
         END IF
      END IF
C
      END
