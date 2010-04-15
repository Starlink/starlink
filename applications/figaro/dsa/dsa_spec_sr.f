C+
C             D S A _ S P E C I F I C _ S T R U C T U R E
C
C  Routine name:
C     DSA_SPECIFIC_STRUCTURE
C
C  Function:
C     Returns the DTA_ system name of an application-specific substructure
C
C  Description:
C     A Figaro application that needs to access data objects that are
C     specific to its processing needs to be able to find the name of
C     a sub-structure in which these may be stored.  This routine returns
C     the DTA_ system name of such a structure (and optionally creates
C     the structure).  The structure identifier passed to this routine
C     is just that - an identifier.  This routine should be thought of as
C     using information internal to the DSA_ system (possibly supplied by
C     control files) that enable it to determine the actual sub-structure
C     (and its type) to be used for this purpose.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SPECIFIC_STRUCTURE (REF_NAME,STRUCT_ID,MODE,STRUCTURE,
C                                                             STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME      (Fixed string,descr) The reference name of the
C                       structure to contain the sub-structure in question.
C     (>) STRUCT_ID     (Fixed string,descr) A string identifying the
C                       sub-structure in question.
C     (>) MODE          (Fixed string,descr) One of blank, 'READ', 'WRITE' or
C                       'UPDATE' (only the first character is significant).
C                       If the mode is 'READ' or 'UPDATE', the sub-structure
C                       must already exist, and it will be considered an
C                       error if it does not.  If it is 'WRITE', the
C                       sub-structure will be created if it does not exist.
C                       If blank, the existence or otherwise of the sub-
C                       structure is ignored.
C     (<) STRUCTURE     (Fixed string,descr) The DTA_ system name of the
C                       sub-structure.  This can be used to generate the
C                       names of the structure elements (preferably using
C                       DTA_CRNAM).
C     (!) STATUS        (Integer,ref) Status code.  If bad status is passed
C                       to this routine, it will return immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     ICH_LEN, ICH_FOLD, DSA_REF_SLOT, DSA_WRUSER, DSA_GET_ACTUAL_NAME
C     DSA__TOP_ITEM_TYPE, DSA__CREATE_EXTRA, DSA__EXTRA_NAME, DTA_TYVAR,
C     DTA_CRVAR, DTA_ERROR
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the DSA_ system, and
C     the structure associated with REF_NAME must have been opened.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_LEN      Position of last non-blank char in string
C     ICH_FOLD     Convert string to upper case
C     DSA_REF_SLOT Look up reference name in common tables
C     DSA_WRUSER   Output message to user
C     DSA_GET_ACTUAL_NAME  Get full structure name from ref name
C     DSA__TOP_ITEM_TYPE   Classify a top level item
C     DSA__CREATE_EXTRA    Make sure the main extra information structure exists
C     DSA__EXTRA_NAME      Get the name of the main extra information structure
C     DTA_TYVAR    Get type of data object
C     DTA_CRVAR    Create data object
C     DTA_ERROR    Get message string from DTA error code
C
C  History:
C     19th Aug 1987.   Original version.  KS / AAO.
C     30th Aug 1988.   Warning removed for .OBS.  KS/AAO.
C     12th Mar 1990.   Now uses DSA__ routines rather than assuming the
C                      original Figaro data format.  KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C     16th Jun 1993    Make local upper case STRUCT_ID longer (80
C                      char.). HME/UoE, Starlink.
C+
      SUBROUTINE DSA_SPECIFIC_STRUCTURE (REF_NAME,STRUCT_ID,MODE,
     :                                              STRUCTURE,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) REF_NAME, STRUCT_ID, MODE, STRUCTURE
C
C     Functions used
C
      INTEGER ICH_LEN, ICH_FOLD
C
C     DSA_ system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      LOGICAL   AXIS_TYPE               ! Name is an axis structure
      LOGICAL   DATA_TYPE               ! Name is a data structure
      INTEGER   DTA_STATUS              ! Status from DTA_ routines
      CHARACTER ERROR*64                ! DTA_ error description
      LOGICAL   EXIST                   ! True if sub-structure exists
      INTEGER   IGNORE                  ! Dummy status argument
      INTEGER   INVOKE                  ! Dummy function value
      INTEGER   LENGTH                  ! Characters in OBJ_NAME
      CHARACTER OBJ_NAME*80             ! Name of main extra structure
      INTEGER   REF_SLOT                ! Common reference slot number - ignored
      CHARACTER STRUCT_ID_UC*80         ! Upper case version of STRUCT_ID
      CHARACTER STRUCTURE_NAME*80       ! Full structure name
      CHARACTER TYPE*8                  ! Structure type - ignored
C
C     Return immediately if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the tables
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500      ! Error exit
C
C     See if this is one of the standard axis or data structures - if so,
C     it isn't applications-specific, and someone is cheating.
C
      STRUCT_ID_UC=STRUCT_ID
      INVOKE=ICH_FOLD(STRUCT_ID_UC)
      CALL DSA__TOP_ITEM_TYPE (REF_SLOT,STRUCT_ID_UC,.FALSE.,AXIS_TYPE,
     :                                                     DATA_TYPE)
      IF (AXIS_TYPE.OR.DATA_TYPE) THEN
         CALL DSA_WRUSER('Warning: This application is treating the '//
     :                              'standard sub-structure "')
         CALL DSA_WRUSER(STRUCT_ID_UC(:ICH_LEN(STRUCT_ID_UC)))
         CALL DSA_WRUSER('" as an applications-specific structure. ')
         CALL DSA_WRUSER('While this may work, it is bad practice.')
         CALL DSA_WRFLUSH
      END IF
C
C     Construct the name of the required sub-structure
C
      CALL DSA__EXTRA_NAME (REF_SLOT,OBJ_NAME,LENGTH)
      STRUCTURE=OBJ_NAME(:LENGTH)//'.'//STRUCT_ID_UC
C
C     If necessary, test to see if the sub-structure does in fact exist
C
      IF (MODE.NE.' ') THEN
         CALL DTA_TYVAR(STRUCTURE,TYPE,DTA_STATUS)
         EXIST=DTA_STATUS.EQ.0
C
C        Error if it had to exist and didn't
C
         IF ((MODE(1:1).NE.'W').AND.(MODE(1:1).NE.'w')) THEN
            IF (.NOT.EXIST) THEN
               CALL DSA_WRUSER(
     :                    'Error trying to access the sub-structure ')
               CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
               CALL DSA_WRUSER(', which is the "')
               CALL DSA_WRUSER(STRUCT_ID(:ICH_LEN(STRUCT_ID)))
               CALL DSA_WRUSER('" specific structure for ')
               IGNORE=0
               CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE_NAME,IGNORE)
               CALL DSA_WRUSER(STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
               CALL DSA_WRUSER('.')
               CALL DSA_WRFLUSH
               STATUS=DSA__SPCSTR
            END IF
         ELSE
C
C           If it didn't exist but has to, try to create it
C
            IF (.NOT.EXIST) THEN
               CALL DSA__CREATE_EXTRA (REF_SLOT,DTA_STATUS)
               IF (DTA_STATUS.EQ.0) THEN
                  CALL DTA_CRVAR(STRUCTURE,'Struct',DTA_STATUS)
               END IF
               IF (DTA_STATUS.NE.0) THEN
                  CALL DSA_WRUSER('Unable to create the sub-structure ')
                  CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
                  CALL DSA_WRUSER(', which is the "')
                  CALL DSA_WRUSER(STRUCT_ID(:ICH_LEN(STRUCT_ID)))
                  CALL DSA_WRUSER('" specific structure for ')
                  IGNORE=0
                  CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE_NAME,
     :                                                         IGNORE)
                  CALL DSA_WRUSER(
     :                       STRUCTURE_NAME(:ICH_LEN(STRUCTURE_NAME)))
                  CALL DSA_WRUSER('. ')
                  CALL DTA_ERROR(DTA_STATUS,ERROR)
                  CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
                  CALL DSA_WRUSER('.')
                  CALL DSA_WRFLUSH
                  STATUS=DSA__SPCSTR
               END IF
            END IF
         END IF
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
