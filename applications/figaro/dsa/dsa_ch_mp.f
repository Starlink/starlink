C+
C                   D S A _ C H E C K _ M A P P I N G
C
C  Routine name:
C     DSA_CHECK_MAPPING
C
C  Function:
C     Checks that a specified structure has no active mappings.
C
C  Description:
C     Given the DTA name of a structure or substructure, this
C     routine checks that it contains no arrays that are currently
C     actively being mapped.  If so, it produces an error message
C     and returns a bad status code.  If any arrays in the structure
C     are still mapped as a result of previous access but have been
C     released by the application program then this routine will
C     close them down fully.  This routine needs to be called before
C     a structure is deleted or a top level structure is closed.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_CHECK_MAPPING (OBJ_NAME,LENGTH,DESCRIP,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) OBJ_NAME       (Fixed string,descr) The DTA system name for the
C                        structure to be checked.
C     (>) LENGTH         (Integer,ref) The number of characters in
C                        OBJ_NAME to be used.  This routine only makes
C                        use of OBJ_NAME(:LENGTH), so there may be
C                        extra characters in the string.
C     (>) DESCRIP        (Fixed string, descr) Used to format an error
C                        message if mapped arrays are found.  The message
C                        will be of the form 'Error while attempting to '//
C                        DESCRIP//OBJ_NAME.  So it should be something
C                        like 'close the structure '.
C     (!) STATUS         (Integer,ref) Status code.  If bad status is passed
C                        to it, this routine will return immediately.
C
C  External variables used:
C     Only common variables internal to the DSA_ routines.
C
C  External subroutines / functions used:
C     ICH_LEN, DSA_WRUSER, DSA_FREE_WORKSPACE, DSA_WRNAME, DTA_FRVAR,
C     DTA_ERROR
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C    (<)  DTA_CODE     (Integer) Last DTA_ system failure status code.
C    (>)  MAX_MAPS     (Integer parameter) Maximum number of mapped arrays.
C    (!)  MAP_USED     (Logical array) Indicates map slot in use.
C    (!)  MAP_WORK     (Integer array) Workspace slot associated with array.
C    (>)  MAP_ACTNAM   (String array) Names of actual mapped arrays.
C    (>)  MAP_COUNT    (Integer array) Reference count for this array.
C
C  Subroutine / function details:
C    ICH_LEN        Position of last non-blank char in string
C    DSA_WRUSER     Output message to user
C    DSA_FREE_WORKSPACE   Release a workspace array
C    DSA_WRNAME     Output data object name to user
C    DTA_FRVAR      Unmap a data object
C    DTA_ERROR      Get error description from DTA_ status code
C
C  History:
C     14th Dec  1989  Original version.  KS / AAO.
C     2nd  May  1990  Checks against MAP_ACTNAM rather than against MAP_NAMES
C                     (the two differ if the array is structured).  Allows
C                     OBJ_NAME to be an actual array and not just a structure.
C                     KS/AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992   Remove unused variable declarations. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_CHECK_MAPPING (OBJ_NAME,LENGTH,DESCRIP,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER LENGTH, STATUS
      CHARACTER*(*) OBJ_NAME, DESCRIP
C
C     Functions used
C
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
      INTEGER   DTA_STATUS                  ! DTA_ status code.
      CHARACTER END_CHAR*1                  ! Name terminating character.
      CHARACTER ERROR*64                    ! DTA_ error description
      INTEGER   I                           ! Loop variable
      INTEGER   WORK_SLOT                   ! Work array associated with data
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Given the structure name, see if any of the mapped arrays match it.
C     If so, it may be that they are in fact no longer in use but have not
C     been physically unmapped yet.  If this is the case, their reference
C     counts will be zero and we can actually unmap them now.  Otherwise
C     we have an error condition.  (The use of END_CHAR is to make sure
C     we are looking at a complete specification, not an unlucky abbreviation.)
C
      DO I=1,MAX_MAPS
         IF (MAP_USED(I)) THEN
            END_CHAR=MAP_ACTNAM(I)(LENGTH+1:LENGTH+1)
            IF ((END_CHAR.EQ.' ').OR.(END_CHAR.EQ.'.')) THEN
               IF (MAP_ACTNAM(I)(:LENGTH).EQ.OBJ_NAME(:LENGTH)) THEN
                  IF (MAP_COUNT(I).GT.0) THEN
                     CALL DSA_WRUSER('Attempt to ')
                     CALL DSA_WRUSER(DESCRIP)
                     CALL DSA_WRNAME(OBJ_NAME(:LENGTH))
                     CALL DSA_WRUSER(
     :                           ' while it still has arrays mapped.')
                     CALL DSA_WRUSER(' Probable programming error.')
                     CALL DSA_WRFLUSH
                     STATUS=DSA__OBJMAP
                     GO TO 500      ! Error exit
                  ELSE
                     CALL DTA_FRVAR(MAP_ACTNAM(I),DTA_STATUS)
                     IF (DTA_STATUS.NE.0) THEN
                        CALL DSA_WRUSER('Error unmapping the array ')
                        CALL DSA_WRNAME(MAP_ACTNAM(I))
                        CALL DTA_ERROR(DTA_STATUS,ERROR)
                        CALL DSA_WRUSER('. ')
                        CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
                        CALL DSA_WRUSER('.')
                        CALL DSA_WRFLUSH
                        DTA_CODE=DTA_STATUS
                        STATUS=DSA__DTAERR
                        GO TO 500    ! Error exit
                     END IF
C
C                    If there were any workspace arrays associated with this
C                    mapped array, free them now.  Note that DSA_FREE_WORKSPACE
C                    mapipulates the MAP_WORK values in common, which is what
C                    makes this while loop work.
C
                     DO WHILE (MAP_WORK(I).NE.0)
                        WORK_SLOT=MAP_WORK(I)
                        CALL DSA_FREE_WORKSPACE (WORK_SLOT,STATUS)
                        IF (STATUS.NE.0) GO TO 500    ! Error exit
                     END DO
                  END IF
                  MAP_USED(I)=.FALSE.
               END IF
            END IF
         END IF
      END DO
C
C     Exit
C
  500 CONTINUE
C
      END
