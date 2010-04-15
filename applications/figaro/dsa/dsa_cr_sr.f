C+
C                    D S A _ C R E A T E _ S T R U C T U R E
C
C  Routine name:
C     DSA_CREATE_STRUCTURE
C
C  Function:
C     Creates a structure of defined type as named by a Figaro parameter
C
C  Description:
C     DSA_CREATE_STRUCTURE gets the value of a specified Figaro parameter,
C     which should give the name of a data structure file, and creates in
C     that file a structure as specified by a structure definition file (as
C     read by DSA_READ_STRUCT_DEF) and opens it for output, associating it
C     with a specified reference name.   Note that at least in this
C     implementation, the output structure specified can only be a file
C     name and not a structure within that file, since it is unclear what
C     else should be at the upper levels.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_CREATE_STRUCTURE (REF_NAME,PARM_NAME,STRUCTURE_ID,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME       (Fixed string,descr) The reference name to be
C                        associated with the opened data object.
C     (>) PARM_NAME      (Fixed string, descr) The name of the Figaro
C                        parameter giving the name of the data object
C                        to be created - this has to be just a file name.
C     (>) STRUCTURE_ID   (Fixed string,descr) The name of a structure type
C                        as already defined by a structure definition file.
C     (!) STATUS         (Integer,ref) Status code.  If a bad status value
C                        is passed, this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA package.
C
C  External subroutines / functions used:
C     DSA_CREATE_NAMED_STRUCTURE, PAR_RDCHAR, PAR_BATCH, PAR_CNPAR,
C     PAR_ABORT, PAR_SDCHAR, ICH_FOLD, ICH_LEN, DSA_FIND_REF, DSA_WRUSER
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the DSA_ system, and
C     the structure definition should have been processed by DSA_READ_
C     STRUCT_DEF.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_CREATE_NAMED_STRUCTURE Create an output structure in a named file.
C     DSA_WRUSER       Output message to user
C     ICH_LEN          Position of last non-blank char in string.
C     ICH_FOLD         Convert string to upper case
C     PAR_RDCHAR       Get value of character parameter.
C     PAR_ABORT        Test the parameter system abort flag.
C     PAR_BATCH        Determine whether or not in batch mode.
C     PAR_CNPAR        Cancel the current value of a parameter.
C     PAR_SDCHAR       Set the default for a character parameter.
C
C  Common variables used:
C     (<) PARM_VALUE   (String array) Parameter values associated with
C                      reference names.
C  History:
C     3rd Sept 1987   Original version.  KS / AAO.
C     5th  Sept 1988  Modified to use PAR_ABORT and the new '/NOCHECK'
C                     facility of PAR_RDCHAR.  KS / AAO.
C     9th  Sept 1988  Now sets parameter value used in common.  KS / AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C     26th Jul 1996   Catenations for Linux.  MJCL/Starlink, UCL.
C+
      SUBROUTINE DSA_CREATE_STRUCTURE (REF_NAME,PARM_NAME,
     :                                            STRUCTURE_ID,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) REF_NAME, PARM_NAME, STRUCTURE_ID
      INTEGER STATUS
C
C     Functions used
C
      LOGICAL PAR_ABORT, PAR_BATCH
      INTEGER ICH_FOLD, ICH_LEN
C
C     DSA_ system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      LOGICAL   BATCH                           ! True if in batch mode
      INTEGER   IGNORE                          ! Ignored status code
      INTEGER   INVOKE                          ! Dummy function value
      INTEGER   LENGTH                          ! Characters in NAME - ignored
      CHARACTER NAME*64                         ! DTA object name - ignored
      CHARACTER REF_NAME_UC*32                  ! Upper case reference name
      LOGICAL   RETRY                           ! Controls retry loop
      INTEGER   SLOT                            ! Common index for ref name
      CHARACTER STRUCTURE_NAME*80               ! Name of input structure
      CHARACTER STRING*80                       ! Local string storage
C
C     DSA system common
C
      INCLUDE 'DSA_COMMON'
C
C     Check for bad passed status
C
      IF (STATUS.NE.0) RETURN
C
C     The following 'get parameter, try to open file, cancel and
C     retry on open failure' loop continues until a satisfactory
C     parameter value is obtained.  In batch mode, we don't have that
C     option, of course.
C
      BATCH=PAR_BATCH()
      RETRY=.TRUE.
      DO WHILE (RETRY)
         STRING=PARM_NAME(:ICH_LEN(PARM_NAME))//'/NOCHECK'
         CALL PAR_RDCHAR (STRING,' ',STRUCTURE_NAME)
         IF (PAR_ABORT()) THEN
            STATUS=DSA__ABORT
            RETRY=.FALSE.
         ELSE IF (STRUCTURE_NAME.EQ.' ') THEN
            CALL DSA_WRUSER ('Output file names cannot be blank.')
            CALL DSA_WRFLUSH
            CALL PAR_CNPAR (PARM_NAME)
         ELSE
            CALL DSA_CREATE_NAMED_STRUCTURE (REF_NAME,STRUCTURE_NAME,
     :                                           STRUCTURE_ID,STATUS)
            IF (((STATUS.EQ.DSA__NOOBJ).OR.(STATUS.EQ.DSA__DTAERR))
     :                                        .AND.(.NOT.BATCH)) THEN
               CALL PAR_CNPAR (PARM_NAME)
               CALL PAR_SDCHAR(PARM_NAME,' ',IGNORE)
               STATUS=0
            ELSE
               RETRY=.FALSE.
            END IF
         END IF
      END DO
      IF (STATUS.NE.0) GO TO 500   ! Error exit
C
C     Now, set the parameter value for the reference slot.
C
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
      CALL DSA_FIND_REF(REF_NAME_UC,SLOT,NAME,LENGTH,STATUS)
      IF (STATUS.NE.0) GO TO 500     ! Error exit
      PARM_VALUE(SLOT)=STRUCTURE_NAME
C
C     Exit
C
  500 CONTINUE
C
      END
