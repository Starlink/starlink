C+
C                            D S A _ O U T P U T
C
C  Routine name:
C     DSA_OUTPUT
C
C  Function:
C     Gets the name of a Figaro output structure from a parameter & opens it.
C
C  Description:
C     DSA_OUTPUT gets the value of a specified Figaro parameter.  The
C     parameter should give the name of a data structure object - either
C     just a file name, eg DISK:[DIR]FILE, or a structured name combining
C     a file name with the name of a structure within that file, eg
C     DISK$DATA:[DIR.SUBDIR]FILE.DATA_STRUCTURE.  This structure is then,
C     opened for output, being associated with a specified reference name.
C     If the structure object is already known to the DSA system, having
C     been opened already, then that already open structure will be used,
C     unless the call explicitly specifies that a new structure is to be
C     created.  If the structure object is not already open, a new one will
C     be created, based on another - already opened - whose reference name
C     is supplied.  If no basis name is supplied (ie is passed as blank),
C     then an empty data structure will be created.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_OUTPUT (REF_NAME,PARM_NAME,BASIS_NAME,
C                                  BASIS_FLAGS,DETAIL_FLAGS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME       (Fixed string,descr) The reference name to be
C                        associated with the opened data object.
C     (>) PARM_NAME      (Fixed string, descr) The name of the Figaro
C                        parameter giving the name of the data object.
C     (>) BASIS_NAME     (Fixed string,descr) The reference name of the
C                        already opened data object to serve as the basis
C                        for the output.  Can be blank, in which case
C                        an empty structure will be created, if needed.
C     (>) BASIS_FLAGS    (Integer,ref) Flags that control the use made of
C                        the basis object.  Those used at present are:
C                        bit 0:  If set, data and axis structures are not to
C                        be copied. This usually implies that the new
C                        structure will have differently dimensioned data
C                        arrays which will be created by routines such as
C                        DSA_RESHAPE_AXIS, DSA_RESHAPE_DATA.
C     (>) DETAIL_FLAGS   (Integer,ref) Flags that control other details of
C                        the structure opening.  Those used at present are:
C                        bit 0: If set, creation of a new data structure is
C                        forced, even if one of the same name is already
C                        known to the system.
C                        Note that bit 0 is the least significant bit.
C     (!) STATUS         (Integer,ref) Status code.  If a bad status value
C                        is passed, this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA system.
C
C  External subroutines / functions used:
C     DSA_NAMED_OUTPUT, PAR_RDCHAR, PAR_BATCH, PAR_CNPAR, PAR_ABORT,
C     PAR_SDCHAR, ICH_FOLD, ICH_LEN, DSA_WRUSER, DSA_FIND_REF
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the DSA_ system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variables used:
C     (!) PARM_VALUE   (String array) Parameter values associated with
C                      reference names.
C
C  Subroutine / function details:
C     DSA_NAMED_OUTPUT Open a specific Figaro structure for output.
C     DSA_FIND_REF     Look up a reference name in the common tables.
C     DSA_WRUSER       Output message to user
C     ICH_FOLD         Convert string to upper case
C     ICH_LEN          Position of last non-blank char in string.
C     PAR_RDCHAR       Get value of character parameter.
C     PAR_ABORT        Test the parameter system abort flag.
C     PAR_BATCH        Determine whether or not in batch mode.
C     PAR_CNPAR        Cancel the current value of a parameter.
C     PAR_SDCHAR       Set the default for a character parameter.
C
C  History:
C     16th July 1987   Original version.  KS / AAO.
C     5th  Sept 1988   Modified to use PAR_ABORT and the new '/NOCHECK'
C                      facility of PAR_RDCHAR.  KS / AAO.
C     9th  Sept 1988   Now sets default to parameter value used for
C                      BASIS_NAME.  KS/AAO.
C     14th Feb  1989   Comments revised.  KS/AAO.
C     2nd  Jan  1990   No longer defaults file name if data not copied. KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_OUTPUT (REF_NAME,PARM_NAME,BASIS_NAME,
     :                             BASIS_FLAGS,DETAIL_FLAGS,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) REF_NAME, PARM_NAME, BASIS_NAME
      INTEGER BASIS_FLAGS, DETAIL_FLAGS, STATUS
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
      CHARACTER DEFAULT*80                      ! Default parameter value
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
C     If the basis name is non-blank, look it up and see if any
C     parameter value is associated with it.  The code about setting the
C     default is OK so long as the only basis flag supported is the
C     "don't copy data" flag.
C
      IF (BASIS_NAME.NE.' ') THEN
         REF_NAME_UC=BASIS_NAME
         INVOKE=ICH_FOLD(REF_NAME_UC)
         CALL DSA_FIND_REF(REF_NAME_UC,SLOT,NAME,LENGTH,STATUS)
         IF (STATUS.NE.0) GO TO 500     ! Error exit
         DEFAULT=PARM_VALUE(SLOT)
         IF ((DEFAULT.NE.' ').AND.(BASIS_FLAGS.EQ.0)) THEN
            CALL PAR_SDCHAR(PARM_NAME,DEFAULT,IGNORE)
         END IF
      END IF
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
            CALL DSA_NAMED_OUTPUT (REF_NAME,STRUCTURE_NAME,BASIS_NAME,
     :                                 BASIS_FLAGS,DETAIL_FLAGS,STATUS)
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
