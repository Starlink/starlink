C+
C
C                      D S A _ Q U A L I T Y _ T Y P E
C
C  Routine name:
C     DSA_QUALITY_TYPE
C
C  Function:
C     Returns the type of the quality data array.
C
C  Description:
C     This routine returns the DTA_ system type of the quality data array
C     of a structure.  If the array does not exist, an error message is
C     output and bad status is returned.  If the data array is a
C     structured type, this is also indicated.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_QUALITY_TYPE (REF_NAME,TYPE,STRUCT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (<) TYPE         (Fixed string,descr) The DTA_ system type of the
C                      quality data array.
C     (<) STRUCT       (Logical,ref) Set to indicate whether or not the
C                      array is a structured type.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     DSA_REF_SLOT, DSA__QUAL_NAME, DSA_ARRAY_TYPE
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     data structure must have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_REF_SLOT    Look up reference name in common tables.
C     DSA__QUAL_NAME  Get the name of the data quality array.
C     DSA_ARRAY_TYPE  Get the type of a named array.
C
C  History:
C     7th  Dec  1989.   Original version.  KS / AAO.
C     22nd Feb  1990.   Modified to remove assumptions about structure
C                       names.  Now uses DSA__ routines. KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_QUALITY_TYPE (REF_NAME,TYPE,STRUCT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL STRUCT
      CHARACTER*(*) REF_NAME,TYPE
      INTEGER STATUS
C
C     Local variables
C
      INTEGER   LENGTH                      ! Object name length
      CHARACTER OBJ_NAME*128                ! DTA_ name of data object
      INTEGER   REF_SLOT                    ! Reference table slot # - ignored
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the tables.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      IF (STATUS.EQ.0) THEN
C
C        Generate the array name and get the type.
C
         CALL DSA__QUAL_NAME (REF_SLOT,OBJ_NAME,LENGTH)
         CALL DSA_ARRAY_TYPE (OBJ_NAME(:LENGTH),'quality array',TYPE,
     :                                                 STRUCT,STATUS)
      END IF
C
      END
