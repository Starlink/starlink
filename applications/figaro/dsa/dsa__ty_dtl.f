C+
C                     D S A _ _ T Y P E _ D E T A I L S
C
C  Routine name:
C     DSA__TYPE_DETAILS
C
C  Function:
C     Takes apart a DSA type specification.
C
C  Description:
C     This routine provides the basic support for the specification of
C     structured array types.  It takes a type string as might be specified
C     by a top-level DSA caller (or user), that is either a primitive type
C     such as 'FLOAT', or a structured type such as 'COMPLEX', or a structured
C     type with qualifiers such as 'COMPLEX/DOUBLE'.  It returns the basic
C     type of the structure - something that other DSA routines can use to
C     control the basic shape of the structure, eg 'COMPLEX', the type for the
C     main data array, any variant string that should be included in the
C     structure, and the type to use when creating the structured array (if
C     the array is primitive, this last is blank).
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__TYPE_DETAILS (REF_SLOT,TYPE,BASIC_TYPE,VARIANT,DATA_TYPE,
C                                                    STRUCT_TYPE,KNOWN)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT      (Integer) The reference slot for a file connected
C                       with this type.  (It may be that the file's structure
C                       influences the way the type is interpreted, as
C                       happens for 'CSTRUCT' and 'SCALED').  If zero,
C                       no file is open, and this cannot be made use of.
C     (>) TYPE          (Fixed string,descr) The DSA type specification.
C     (<) BASIC_TYPE    (Fixed string,descr) The basic type of the structure.
C     (<) VARIANT       (Fixed string,descr) The variant string to be used
C                       for the structure.
C     (<) DATA_TYPE     (Fixed string,descr) The type for the main array in
C                       the structure.  (Or arrays, for complex data.)
C     (<) STRUCT_TYPE   (Fixed string,descr) The type for the overall
C                       structure containing the array.  If this is blank,
C                       the array is primitive.
C     (<) KNOWN         (Logical,ref) Returned true if this type is known to
C                       the system.
C
C  External variables used:
C     Only common variables internal to the DSA system.
C
C  External subroutines / functions used:
C     ICH_FOLD
C
C  Prior requirements:
C     If REF_SLOT is non-zero the file it refers to must be open.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 8th Feb 1995
C-
C  Subroutine / function details:
C     ICH_FOLD          Convert string to upper case
C
C  Common variable details:
C     (>) NDF_FORMAT    (Logical array) Indicates structure format is Starlink's
C                       NDF format (described in SGP38).  If false, format is
C                       original Figaro format (DST files).
C     (>) DISK_FORMAT   (Integer) Code controlling which formats are supported.
C     (>) NDF_THEN_DST  (Integer parameter) DST and NDF supported, NDF default.
C     (>) NDF_ONLY      (Integer parameter) Only NDF format is supported.
C
C  History:
C     26th Apr 1990.   Original version.  KS / AAO.
C     27th Apr 1990.   Now allows for 'ARRAY' and 'COMPLEX_ARRAY'.  These are
C                      not really types we expect, but may be returned by some
C                      of the type enquiry routines until they get fixed.
C                      KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C      8th Feb 1995    Now generates SIMPLE structured arrays rather than
C                      primitive arrays when an NDF format file is being
C                      used. KS/AAO.
C+
      SUBROUTINE DSA__TYPE_DETAILS (REF_SLOT,TYPE,BASIC_TYPE,VARIANT,
     :                                     DATA_TYPE,STRUCT_TYPE,KNOWN)

C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL KNOWN
      INTEGER REF_SLOT
      CHARACTER*(*) TYPE, BASIC_TYPE, VARIANT, DATA_TYPE, STRUCT_TYPE
C
C     Functions used
C
      INTEGER ICH_FOLD
C
C     DSA common definitions
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      LOGICAL   CHECK_TYPE         ! Indicates we have to check validity of type
      CHARACTER DEF_TYPE*16        ! Default type for structure
      INTEGER   ISLSH              ! Position of any '/' in TYPE
      INTEGER   INVOKE             ! Dummy function return value
      LOGICAL   NDF                ! True if we are to assume NDF format
      CHARACTER STRING*16          ! Part of TYPE to test for structure name
      CHARACTER TYPE_UC*32         ! Upper case version of TYPE
C
C     Start by making sure type is in upper case
C
      TYPE_UC=TYPE
      INVOKE=ICH_FOLD(TYPE_UC)
C
C     If a reference slot has been specified, see if this is an NDF format
C     file.  If not, assume the default file type.
C
      IF (REF_SLOT.EQ.0) THEN
         NDF=(DISK_FORMAT.EQ.NDF_ONLY).OR.(DISK_FORMAT.EQ.NDF_THEN_DST)
      ELSE
         NDF=NDF_FORMAT(REF_SLOT)
      END IF
C
C     Does the type specify one of the known structures?  Trim this
C     off before any qualifiers before testing the name.
C
      KNOWN=.FALSE.
      ISLSH=INDEX(TYPE_UC,'/')
      IF (ISLSH.GT.1) THEN
         STRING=TYPE_UC(:ISLSH-1)
      ELSE
         STRING=TYPE_UC
      END IF
      IF ((STRING.EQ.'COMPLEX').OR.(STRING.EQ.'COMPLEX_ARRAY')) THEN
         STRUCT_TYPE='COMPLEX_ARRAY'
         VARIANT='SIMPLE'
         KNOWN=.TRUE.
         DEF_TYPE='FLOAT'
         BASIC_TYPE='COMPLEX'
      ELSE IF ((STRING.EQ.'CSTRUCT').OR.(STRING.EQ.'SCALED')) THEN
         KNOWN=.TRUE.
         DEF_TYPE='SHORT'
         IF (NDF) THEN
            VARIANT='SCALED'
            STRUCT_TYPE='ARRAY'
            BASIC_TYPE='SCALED'
         ELSE
            VARIANT=' '
            STRUCT_TYPE='CSTRUCT'
            BASIC_TYPE='CSTRUCT'
         END IF
      ELSE IF ((STRING.EQ.'SIMPLE').OR.(STRING.EQ.'ARRAY')) THEN
         KNOWN=.TRUE.
         DEF_TYPE='FLOAT'
         BASIC_TYPE='SIMPLE'
         IF (NDF) THEN
            VARIANT='SIMPLE'
            STRUCT_TYPE='ARRAY'
         ELSE
            VARIANT=' '
            STRUCT_TYPE=' '
         END IF
      END IF
C
C     At the end of all that, if KNOWN is set, this was one of the
C     structured types, and any qualifier must be an explicit array type.
C
      IF (KNOWN) THEN
         IF (ISLSH.GT.0) THEN
            DATA_TYPE=TYPE_UC(ISLSH+1:)
            CHECK_TYPE=.TRUE.
         ELSE
            DATA_TYPE=DEF_TYPE
            CHECK_TYPE=.FALSE.
         END IF
      ELSE
C
C        If it wasn't known, and there was a qualifier, it had to be
C        a known structured type, so this must be an error.  Exit with
C        KNOWN set false.  Otherwise, it must be a primitive type.
C
         IF (ISLSH.GT.0) GO TO 500
         DATA_TYPE=STRING
         CHECK_TYPE=.TRUE.
         BASIC_TYPE='SIMPLE'
C
C        If we are dealing with an NDF file, we attempt to follow the
C        Starlink preferred style and generate a SIMPLE structured array
C        rather than the primitive type that has actually been specified.
C
         IF (NDF) THEN
            STRUCT_TYPE='ARRAY'
            VARIANT='SIMPLE'
         ELSE
            STRUCT_TYPE=' '
            VARIANT=' '
         END IF
      END IF
C
C     Now we check that the data type we have is valid - the only exception
C     is if we're taking the default for a structured type, in which case
C     we know it must be valid.
C
      IF (CHECK_TYPE) THEN
         KNOWN=.FALSE.
         IF (DATA_TYPE.EQ.'FLOAT') THEN
            KNOWN=.TRUE.
         ELSE IF (DATA_TYPE.EQ.'SHORT') THEN
            KNOWN=.TRUE.
         ELSE IF (DATA_TYPE.EQ.'USHORT') THEN
            KNOWN=.TRUE.
         ELSE IF (DATA_TYPE.EQ.'DOUBLE') THEN
            KNOWN=.TRUE.
         ELSE IF (DATA_TYPE.EQ.'INT') THEN
            KNOWN=.TRUE.
         ELSE IF (DATA_TYPE.EQ.'BYTE') THEN
            KNOWN=.TRUE.
         ELSE IF (DATA_TYPE.EQ.'CHAR') THEN
            KNOWN=.TRUE.
         END IF
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
