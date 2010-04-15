C+
C
C                      D S A _ D A T A _ T Y P E
C
C  Routine name:
C     DSA_DATA_TYPE
C
C  Function:
C     Returns the type of the main data array.
C
C  Description:
C     This routine returns the DTA_ system type of the main data array
C     of a structure.  If the array does not exist, an error message is
C     output and bad status is returned.  If the data array is a
C     structured type, this is also indicated.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_DATA_TYPE (REF_NAME,TYPE,STRUCT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (<) TYPE         (Fixed string,descr) The DTA_ system type of the
C                      data array.
C     (<) STRUCT       (Logical,ref) Set to indicate whether or not the
C                      array is a structured type.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     DSA_FIND_REF, ICH_FOLD, DSA_ARRAY_TYPE, DSA__ARRAY, DSA__DATA_NAME
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
C     ICH_FOLD       Convert a string to upper case.
C     DSA_FIND_REF   Look up reference name in common tables.
C     DSA__ARRAY     Determines if a named object is a (structured) array.
C     DSA__DATA_NAME Gets name of main data array in a structure.
C     DSA_ARRAY_TYPE Gets type of a named data array
C
C  History:
C     25th Aug 1988.   Original version.  KS / AAO.
C     9th  Sept 1988.  Now sets bad status properly on errors.  KS/AAO.
C     15th Dec 1989.   Now uses DSA__ routines for structure details.  KS/AAO.
C     22nd Feb 1990.   Now uses DSA_ARRAY_TYPE to actually get type. KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_DATA_TYPE (REF_NAME,TYPE,STRUCT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL STRUCT
      CHARACTER*(*) REF_NAME,TYPE
      INTEGER STATUS
C
C     Functions used
C
      LOGICAL   DSA__ARRAY
      INTEGER   ICH_FOLD
C
C     Local variables
C
      INTEGER   INVOKE                      ! Dummy function value
      INTEGER   LENGTH                      ! Object name length
      CHARACTER OBJ_NAME*128                ! DTA_ name of data object
      CHARACTER REF_NAME_UC*32              ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                    ! Reference table slot #
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
      IF (STATUS.NE.0) GO TO 500   ! Error exit
C
C     See if the reference name points directly to an array.  If not,
C     get the name of the main data array.
C
      IF (.NOT.DSA__ARRAY(OBJ_NAME(:LENGTH))) THEN
         CALL DSA__DATA_NAME (REF_SLOT,OBJ_NAME,LENGTH)
      END IF
C
C     Now get its type.
C
      CALL DSA_ARRAY_TYPE (OBJ_NAME(:LENGTH),'main data array',
     :                                            TYPE,STRUCT,STATUS)
C
C     Exit
C
  500 CONTINUE
C
      END
