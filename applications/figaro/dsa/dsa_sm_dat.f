C+
C                        D S A _ S A M E _ D A T A
C
C  Routine name:
C     DSA_SAME_DATA
C
C  Function:
C     Determines if the data arrays for two structures are the same.
C
C  Description:
C     Generally, the DSA_ package hides the external details of just what
C     the reference names it uses actually refer to.  Sometimes, a program
C     needs to know if the main data arrays associated with two reference
C     names are in fact the same data if it is to organise its workspace
C     properly.  This routine can be used to find this out.
C
C  Language:
C     FORTRAN
C
C  Call:
C     SAME = DSA_SAME_DATA (REF_NAME,REF_NAME2,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME      (Fixed string,descr) The reference name for the
C                       first structure.
C     (>) REF_NAME2     (Fixed string,descr) The reference name for the
C                       second structure.
C     (!) STATUS        (Integer,ref) Status code.  If bad status is
C                       passed to this routine it returns immediately.
C
C  Returns:
C
C     (<) SAME          (Logical,function value) True if the two have
C                       the same data array.
C
C  External subroutines / functions used:
C     DSA_FIND_REF, DTA_STRUC, DSA__DATA_NAME, ICH_FOLD
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and
C     both structures must have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_FOLD       Convert string to upper case
C     DTA_STRUC      Determine if an object is primitive or a structure
C     DSA_FIND_REF   Look up a reference name in the common tables
C     DSA__DATA_NAME get DTA name of main data array in structure
C
C  History:
C     3rd  Aug 1987.  Original version.  KS / AAO.
C     27th Feb 1990.  Now uses DSA__ routines for structure details instead
C                     of assuming the original Figaro data format.  KS/AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992   Ensure function value is defined before allowing a
C                     RETURN (the DecStation compiler spotted this one) KS/AAO.
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      LOGICAL FUNCTION DSA_SAME_DATA (REF_NAME,REF_NAME2,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) REF_NAME, REF_NAME2
C
C     Functions used
C
      INTEGER ICH_FOLD
C
C     Local variables
C
      INTEGER   DTA_STATUS                  ! Status from DTA_ routines
      INTEGER   INVOKE                      ! Dummy function value
      INTEGER   LENGTH                      ! Length of top level names
      CHARACTER OBJ_NAME*80                 ! Top level name, 1st structure
      CHARACTER OBJ_NAME2*80                ! Top level name, 2nd structure
      CHARACTER REF_NAME_UC*32              ! Upper case version of REF_NAME(2)
      INTEGER   REF_SLOT                    ! Common reference slot - ignored
      LOGICAL   STRUCT                      ! Indicates object is a structure
C
C     Return immediately on bad status
C
      DSA_SAME_DATA=.FALSE.
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference slots of each, and generate the data array
C     names - note that comparing the actual names does not work.
C
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
      CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
      CALL DTA_STRUC(OBJ_NAME(:LENGTH),STRUCT,DTA_STATUS)
      IF (STRUCT) CALL DSA__DATA_NAME (REF_SLOT,OBJ_NAME,LENGTH)
C
      REF_NAME_UC=REF_NAME2
      INVOKE=ICH_FOLD(REF_NAME_UC)
      CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,OBJ_NAME2,LENGTH,STATUS)
      CALL DTA_STRUC(OBJ_NAME2(:LENGTH),STRUCT,DTA_STATUS)
      IF (STRUCT) CALL DSA__DATA_NAME (REF_SLOT,OBJ_NAME2,LENGTH)
C
      DSA_SAME_DATA=OBJ_NAME.EQ.OBJ_NAME2
C
      END
