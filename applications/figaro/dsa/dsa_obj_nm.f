C+
C                     D S A _ O B J E C T _ N A M E
C
C  Routine name:
C     DSA_OBJECT_NAME
C
C  Function:
C     Returns the name of the object associated with a data structure
C
C  Description:
C     Most Figaro data structures opened, for example, by DSA_INPUT
C     will have an object name associated with them.  This routine
C     returns that name, if it exists, and returns a blank string
C     if it does not.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_OBJECT_NAME (REF_NAME,OBJECT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name used to
C                      identify the structure.
C     (<) OBJECT       (Fixed string,descr) The name of the object
C                      associated with the structure.
C     (!) STATUS       (Integer,ref) Status code.  If bad status is
C                      passed, this routine returns immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     DSA_REF_SLOT, DSA__OBJECT_NAME, DTA_SZVAR, DTA_RDVARC, ICH_CLEAN
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     structure in question should have been already opened.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_REF_SLOT     Look up reference name in common tables.
C     DSA__OBJECT_NAME Get name of item holding object name.
C     DTA_SZVAR        Get size of a data object.
C     DTA_RDVARC       Read from a character data object.
C     ICH_CLEAN        Truncate string at first non-printing character.
C
C  History:
C     29th July 1987    Original version.  KS / AAO.
C     16th Jan  1990    Now uses DSA__ routines to handle the structure
C                       details.  KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C+
      SUBROUTINE DSA_OBJECT_NAME (REF_NAME,OBJECT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) REF_NAME, OBJECT
C
C     Functions used
C
      INTEGER ICH_CLEAN
C
C     Local variables
C
      INTEGER   DTA_STATUS                  ! DTA_ routine status codes
      INTEGER   INVOKE                      ! Dummy function value
      INTEGER   LENGTH                      ! Length of structure name
      CHARACTER NAME*80                     ! DTA_ name for object name
      INTEGER   NCH                         ! Characters in object name
      INTEGER   NDIM                        ! # of dimensions in error array
      INTEGER   REF_SLOT                    ! Reference table slot #
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Blank out the object name, in case we have to return prematurely
C
      OBJECT=' '
C
C     Look up the reference name in the tables and generate the name
C     of the object string.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500          ! Error exit
      CALL DSA__OBJECT_NAME (REF_SLOT,NAME,LENGTH)
C
C     Now see if we can get its size.
C
      CALL DTA_SZVAR(NAME,1,NDIM,NCH,DTA_STATUS)
      IF (DTA_STATUS.EQ.0) THEN
C
C        It seems to exist, so read it.
C
         NCH=MIN(NCH,LEN(OBJECT))
         CALL DTA_RDVARC(NAME,NCH,OBJECT,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            OBJECT=' '
         ELSE
            INVOKE=ICH_CLEAN(OBJECT)
         END IF
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
