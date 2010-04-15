C+
C                     D S A _ S E T _ O B J E C T
C
C  Routine name:
C     DSA_SET_OBJECT
C
C  Function:
C     Sets the name of the object associated with a data structure
C
C  Description:
C     Most Figaro data structures opened, for example, by DSA_INPUT
C     will have an object name associated with them.  This routine
C     changes that name, if it exists, and creates such a name if
C     it does not.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SET_OBJECT (REF_NAME,OBJECT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name used to
C                      identify the structure.
C     (>) OBJECT       (Fixed string,descr) The name of the object
C                      to be associated with the structure.
C     (!) STATUS       (Integer,ref) Status code.  If bad status is
C                      passed, this routine returns immediately.
C
C  External variables used:
C     Only common variables used internally by the DSA_ routines.
C
C  External subroutines / functions used:
C     DSA_REF_SLOT, DSA_GET_ACTUAL_NAME, DSA_WRUSER, DTA_DLVAR,
C     DTA_ERROR, DTA_SZVAR, DTA_WRVARC, ICH_LEN, DSA__OBJECT_NAME,
C     DSA__CREATE_OBJECT
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     structure in question should have been already opened.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variables used:
C     (<) DTA_CODE     (Integer) Last DTA_ error code
C
C  Subroutine / function details:
C     DSA_REF_SLOT     Look up reference name in common tables.
C     DSA_GET_ACTUAL_NAME  Get full structure name from ref name.
C     DSA_WRUSER       Write message to user.
C     DSA__OBJECT_NAME Get name of data item holding object name
C     DSA__CREATE_OBJECT Create data item holding object name.
C     DTA_DLVAR        Delete a data object.
C     DTA_SZVAR        Get size of a data object.
C     DTA_WRVARC       Write to a character data object.
C     ICH_LEN          Position of last non-blank char in string.
C
C  History:
C     24th Aug 1987    Original version.  KS / AAO.
C     16th Jan 1990    Modified to use DSA__ routines to handle details
C                      of structure contents.  KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992    Remove unused variable declarations. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C
C+
      SUBROUTINE DSA_SET_OBJECT (REF_NAME,OBJECT,STATUS)
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
      INTEGER ICH_LEN
C
C     DSA_ system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     DSA_ common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      LOGICAL   CREATE                      ! Indicates object needs creating
      INTEGER   DTA_STATUS                  ! DTA_ routine status codes
      CHARACTER ERROR*64                    ! DTA_ error description
      INTEGER   IGNORE                      ! Dummy status argument
      INTEGER   LENGTH                      ! Length of structure name
      INTEGER   LENOBJ                      ! Characters in OBJECT
      CHARACTER NAME*80                     ! DTA_ name for object name
      INTEGER   NCH                         ! Characters in object name
      INTEGER   NDIM                        ! # of dimensions in object string
      INTEGER   REF_SLOT                    ! Reference table slot #
      CHARACTER STRUCTURE*80                ! Full structure name
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the tables, and hence get the
C     name of the structure item that holds the object name.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500          ! Error exit
      CALL DSA__OBJECT_NAME (REF_SLOT,NAME,LENGTH)
C
C     Generate the name of the object string and see if we can
C     get its size.
C
      LENOBJ=ICH_LEN(OBJECT)
      CREATE=.FALSE.
      CALL DTA_SZVAR(NAME,1,NDIM,NCH,DTA_STATUS)
      IF (DTA_STATUS.EQ.0) THEN
C
C        It seems to exist.  See if it's large enough, and delete
C        it if it isn't.
C
         IF (NCH.LT.LENOBJ) THEN
            CALL DTA_DLVAR(NAME,DTA_STATUS)
            IF (DTA_STATUS.NE.0) THEN
               CALL DTA_ERROR (DTA_STATUS,ERROR)
               CALL DSA_WRUSER(
     :           'Error trying to delete existing object name in ')
               IGNORE=0
               CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,IGNORE)
               CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
               CALL DSA_WRUSER('. ')
               CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
               CALL DSA_WRUSER('.')
               CALL DSA_WRFLUSH
               STATUS=DSA__DTAERR
               DTA_CODE=DTA_STATUS
               GO TO 500      ! Error exit
            END IF
            CREATE=.TRUE.
         END IF
      ELSE
C
C        If it didn't exist, it will have to be created
C
         CREATE=.TRUE.
      END IF
C
C     If we're going to have to create it, do so.
C
      IF (CREATE) THEN
         CALL DSA__CREATE_OBJECT(REF_SLOT,MAX(32,LENOBJ),DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DTA_ERROR (DTA_STATUS,ERROR)
            CALL DSA_WRUSER(
     :           'Error trying to create object name element in ')
            IGNORE=0
            CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,IGNORE)
            CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
            CALL DSA_WRUSER('. ')
            CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
            CALL DSA_WRUSER('.')
            CALL DSA_WRFLUSH
            STATUS=DSA__DTAERR
            DTA_CODE=DTA_STATUS
            GO TO 500      ! Error exit
         END IF
      END IF
C
C     At this point, we know it exists, and is big enough.
C     So write to it.
C
      CALL DTA_WRVARC (NAME(:LENGTH),LENOBJ,OBJECT,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
         CALL DTA_ERROR (DTA_STATUS,ERROR)
         CALL DSA_WRUSER('Error trying to write to object name in ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,IGNORE)
         CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER('. ')
         CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
         CALL DSA_WRUSER('.')
         CALL DSA_WRFLUSH
         STATUS=DSA__DTAERR
         DTA_CODE=DTA_STATUS
         GO TO 500      ! Error exit
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
