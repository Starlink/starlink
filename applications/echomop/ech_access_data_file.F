#if HAVE_CONFIG_H
# include "config.h"
#endif
      SUBROUTINE ECH_ACCESS_DATA_FILE(
     :           REQUIRED_OBJECT,
     :           TYPE,
     :           NEEDS_QUALITY,
     :           NEEDS_ERRORS,
     :           MAPPED_ADDRESS,
     :           MAPPED_QADDRESS,
     :           MAPPED_EADDRESS,
     :           IO_STRING,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_ACCESS_DATA_FILE

*  Purpose:
*     Controls access to image data file contents.

*  Description:
*     This routine is one of the most logically complex sections of code in
*     the package.  The module is overly large because it contains all of
*     the data file management code in one place. This approach should enhance
*     the chances of porting to a hostile environment easily because all
*     the changes will be localised in this module with minimal impact on
*     any other modules (except for ECH_ACCESS_OBJECT).  This module achieves
*     its functions by calling ECH_ACCESS_OBJECT to perform all the actual
*     work.  ECH_ACCESS_OBJECT in turn calls low-level interface routines
*     to perform simple operations such as file open/close, map etc.
*
*     The processes carried out are as follows:
*        Initialise container file interfaces if necessary.
*        Check if requested object has already been mapped.
*        If already mapped check that requested file matches the one
*        already in use.
*        If request is for an IMAGE object, check dimensions for consistency.
*        If request is for an IMAGE object, check that any associated quality
*        and error array requests can be done correctly or if re-mapping is
*        needed.
*        Try to map object (and associate arrays if requested).
*        If fails and object is `expected' in data file, then try to map it
*        from environment variables instead.
*        Save properties of mapped object array(s) in internal tables.
*
*     Most of the complexity lies in dealing with the possibility that the
*     user may have changed the file to which the object reference name
*     points. For example, the package may have mapped a data array for
*     reference name INPTIM when INPTIM=FIRST. This may be followed at
*     a later stage by a request when the user has changed to INPTIM=SECOND.
*     This module needs to
*     close down the mappings to FIRST, close the container file, open
*     SECOND and make new mappings in this case.
*
*     A second level of complexity is due to the fact that IMAGE objects
*     may consist of a data array and optional associated quality and
*     error arrays. Two logical variables are passed into the routine
*     detailing whether or not these associate arrays are required. The
*     module must then check the status of any current mappings to the
*     object. For example if an IMAGE data array only is mapped for read
*     access, and a request is made for update access to data+quality,
*     then the module would first need to unmap the data array, and then
*     re-map it (for update access), and then map the quality array as well.
*
*     A final source of difficulty is caused by the properties of the underlying
*     interface routines. These demand that once a named container eg FIRST
*     has been mapped to more than one reference name eg INPTIM=FIRST and
*     FFIELD=FIRST, then closing down and re-mapping so that say INPTIM=SECOND
*     cannnot be done unless the mapping FFIELD=FIRST is also closed down.
*     This imposes an overhead on such operations as well as increasing
*     the complexity of this module.
*
*     The result of all this is that as seen by the caller the module is
*     very simple. The caller requests access to a named object Eg INPTIM,
*     specifies if quality and errors are required, and is returned the
*     address(es) of the mapped data. The calling application does not
*     need to consider whether the data is already mapped, multiply mapped,
*     located in a different file to the one last mapped using INPTIM,
*     etc, etc. The only additional code necessary in the caller is to
*     decide what to do when mapping is not acheived. This will normally
*     be due to the user having supplied an incorrect file name and the
*     normal course of action will be to repeat the access request.
*     The module checks for this special case by examing the `status'
*     variable on entry. If it is set to ECH__RETRY_ACCESS then the
*     module will attempt to re-prompt the user for a container file name,
*     rather than just using the current value as it would normally do.
*
*     The following `error' codes are returned by this routine
*        ECH__IS_ACCESSED    object was already accessed
*        ECH__RETRY_ACCESS   a failure suggestive of incorrect file
*                            specification occured, the caller should
*                            probably re-try
*        ECH__WORKSPACE_PAR  object has been mapped from environment variables
*
*     The following codes denote `fatal' errors and will usually cause
*     the caller to abort further processing and return to either the
*     monolith main menu, or to exit completely.
*        ECH__ABORT_OPTION   a user requested abort has occured
*        ECH__DIM_CONFLICT   the dimensions of the image are inconsistent
*        ECH__BAD_VMEMORY    failure to allocate enough virtual memory

*  Invocation:
*     CALL ECH_ACCESS_DATA_FILE(
*    :     REQUIRED_OBJECT,
*    :     TYPE,
*    :     NEEDS_QUALITY,
*    :     NEEDS_ERRORS,
*    :     MAPPED_ADDRESS,
*    :     MAPPED_QADDRESS,
*    :     MAPPED_EADDRESS,
*    :     IO_STRING,
*    :     STATUS
*    :    )

*  Arguments:
*     REQUIRED_OBJECT = CHAR (Given)
*        Internal reference name of object.
*     TYPE = CHAR (Given)
*        Type of object.
*     NEEDS_QUALITY = LOGICAL (Given)
*        TRUE if IMAGE data quality needed too.
*     NEEDS_ERRORS = LOGICAL (Given)
*        TRUE if IMAGE data errors needed too.
*     MAPPED_ADDRESS = INTEGER (Returned)
*        Address of mapped data array.
*     MAPPED_QADDRESS = INTEGER (Returned)
*        Address of mapped data quality array.
*     MAPPED_EADDRESS = INTEGER (Returned)
*        Address of mapped data errors array.
*     IO_STRING = CHAR (Returned)
*        Text value of object (if type CHAR).
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release
*     1998 Mar  6 (ACD): Changed the specified access mode in a call
*        to ECH_ACCESS_OBJECT from 'R' to 'READ' to avoid an error in
*        the NDF library.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'ECH_INIT_RDCTN.INC'
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_USE_DIMEN.INC'

*  Arguments Given:
      CHARACTER*( * ) REQUIRED_OBJECT
      CHARACTER*( * ) TYPE
      LOGICAL NEEDS_QUALITY
      LOGICAL NEEDS_ERRORS

*  Arguments Returned:
      INTEGER MAPPED_ADDRESS
      INTEGER MAPPED_QADDRESS
      INTEGER MAPPED_EADDRESS
      CHARACTER*( * ) IO_STRING

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL DUMMY_VALUE

      INTEGER DUMDIM( MAX_DIMENSIONS )
      INTEGER I
      INTEGER IST
      INTEGER LIM1
      INTEGER LIM2
      INTEGER NEXT
      INTEGER VSTAT
      INTEGER ASIZE
      INTEGER NEW_ACCESS_COUNT
      INTEGER DUMMY_DIMEN
      INTEGER ILEN
      INTEGER IDUMMY
      INTEGER NEW_OBJECT_SIZE
      INTEGER NEW_OBJECT_HANDLE
      INTEGER NEW_Q_HANDLE
      INTEGER NEW_E_HANDLE
      INTEGER OBJECT_NUMBER
      INTEGER NEW_ADDRESS
      INTEGER NEW_Q_ADDRESS
      INTEGER NEW_E_ADDRESS
      INTEGER STATIC_INDICES
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER NCHAR3
      INTEGER NCHAR4

      LOGICAL RETRYING
      LOGICAL EQULS
      LOGICAL TERM
      LOGICAL OBJECT_ACTIVE
      LOGICAL ALREADY_ACTIVE
      LOGICAL OK_ACTIVE

      CHARACTER*255 IMAGE_NAME
      CHARACTER*255 OLD_IMAGE_NAME
      CHARACTER*132 SAVE_PATH
      CHARACTER*132 FULL_OBJECT_PATH
      CHARACTER*80 REQUIRED_FILE
      CHARACTER*80 WORK_STRING
      CHARACTER*80 TEMP_STRING
      CHARACTER*80 LAST_IMAGE
      CHARACTER*80 LAST_NAME
      CHARACTER*64 CSIZE
      CHARACTER*16 QTYMODE
      CHARACTER*12 REF_STR1
      CHARACTER*12 REF_STR2
      CHARACTER*12 REF_STR3
      CHARACTER*12 REF_STR4
      CHARACTER*6 MAPMODE
      CHARACTER*6 NEW_OBJECT_TYPE

      COMMON / LAST_ONE / LAST_IMAGE

*  Functions Called:
      INTEGER CHR_LEN
      INTEGER ECH_WORD_LEN
      INTEGER ECH_OBJ_IND
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Zero all variables (may not be needed...).
      DO I = 1, MAX_DIMENSIONS
         DUMDIM( I ) = 0
      END DO
      LIM1 = 0
      LIM2 = 0
      NEXT = 0
      VSTAT = 0
      ASIZE = 0
      NEW_ACCESS_COUNT = 0
      DUMMY_DIMEN = 0
      ILEN = 0
      IDUMMY = 0
      NEW_OBJECT_SIZE = 0
      STATIC_INDICES = 0

      EQULS = .FALSE.
      TERM = .FALSE.
      OK_ACTIVE = .FALSE.

*  Initialise local variables and flags.
      RETRYING = ( STATUS .EQ. ECH__RETRY_ACCESS )
      STATUS = SAI__OK
      MAPPED_ADDRESS = 0
      MAPPED_QADDRESS = 0
      MAPPED_EADDRESS = 0
      NEW_OBJECT_HANDLE = 0
      NEW_Q_HANDLE = 0
      NEW_E_HANDLE = 0
      NEW_ADDRESS = 0
      NEW_Q_ADDRESS = 0
      NEW_E_ADDRESS = 0
      OBJECT_NUMBER = 0

      IMAGE_NAME = 'UNSET AS YET!'
      LAST_NAME = 'UNSET AS YET!'

*  If neither the reduction file or any data frame has yet been opened
*  then inititialise the container file access environment.
      IF ( .NOT. RDCTN_FILE_OPEN .AND.
     :     .NOT. DATA_FILES_OPEN ) THEN
         CALL ECH_ACCESS_OBJECT( 'CONTAINER-FILE', 'OPEN',
     :        'ENVIRONMENT', 0, 0, 0, DUMDIM, MAX_DIMENSIONS, 0, ' ',
     :        STATUS )
         DATA_FILES_OPEN = .TRUE.
      END IF

*  Check status before proceeding.
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

*  Determine full path to requested object
*  Loop thru all known active objects
*     If object has already got an access active then
*        Record its address mapping(s)
*     Else
*        If slot is empty, remember its number for next use
*     END IF
      CALL ECH_GET_DATA_PATH( REQUIRED_OBJECT, FULL_OBJECT_PATH,
     :     TYPE, STATIC_INDICES, STATUS )
      ILEN = ECH_WORD_LEN( FULL_OBJECT_PATH )
      OBJECT_ACTIVE = .FALSE.
      ALREADY_ACTIVE = .FALSE.

      NEW_ACCESS_COUNT = 0
      I = 1
      DO WHILE ( .NOT. OBJECT_ACTIVE .AND. I .LE. ACCESS_COUNT )
         IF ( NEW_ACCESS_COUNT .EQ. 0 .AND.
     :        OBJECT_TYPE( I ) .NE. 'IMAGE' .AND.
     :        OBJECT_ADDRESS( I ) .EQ. 0 ) THEN
            NEW_ACCESS_COUNT = I

         ELSE IF ( FULL_OBJECT_PATH .EQ. OBJECT_NAME( I ) )  THEN
            IF ( OBJECT_STATICS( I ) .EQ. STATIC_INDICES ) THEN
               IF ( OBJECT_TYPE( I )  .EQ. 'IMAGE' .AND.
     :              ( OBJECT_ADDRESS( I ) .EQ. 0 .OR.
     :              OBJECT_ADDRESS( I ) .EQ. -1 ) ) THEN
                  NEW_ACCESS_COUNT = I

               ELSE
                  OBJECT_ACTIVE = .TRUE.
                  OBJECT_NUMBER = I
                  NEW_ACCESS_COUNT = I
                  MAPPED_ADDRESS = OBJECT_ADDRESS( I )
                  MAPPED_QADDRESS = OBJECT_QADDRESS( I )
                  MAPPED_EADDRESS = OBJECT_EADDRESS( I )
                  IF ( DIAGNOSTICS_ACTIVE ) THEN
                     CALL CHR_ITOC( OBJECT_NUMBER, REF_STR1, NCHAR1 )
                     REPORT_STRING = ' DATA Access: ' //
     :                     FULL_OBJECT_PATH( :ILEN ) //
     :                     ' already active slot ' //
     :                     REF_STR1( :NCHAR1 ) // '.'
                     CALL ECH_REPORT( RPM_LOG, REPORT_STRING )
                  END IF
               END IF
            END IF
         END IF

*     If object type is IMAGE then retreive the actual file name from
*     the object-property tables.
         IF ( REQUIRED_OBJECT .EQ. 'ARC' .AND.
     :        OBJECT_NAME( I )( :3 ) .EQ. 'ARC' .AND.
     :        OBJECT_TYPE( I ) .EQ. 'IMAGE' ) THEN
           CALL ECH_UPDATE_OBJECT_REF( REQUIRED_OBJECT, IDUMMY,
     :          IDUMMY, IDUMMY, LAST_NAME, DUMMY_VALUE, .FALSE.,
     :          STATUS )
           IO_STRING = LAST_NAME // ' '

         ELSE IF ( OBJECT_TYPE( I ) .EQ. 'IMAGE' .AND.
     :             REQUIRED_OBJECT .EQ. OBJECT_NAME( I ) ) THEN
           CALL ECH_UPDATE_OBJECT_REF( OBJECT_NAME( I ), IDUMMY,
     :          IDUMMY, IDUMMY, LAST_NAME, DUMMY_VALUE, .FALSE.,
     :          STATUS )
           IO_STRING = LAST_NAME // ' '
         END IF
         I = I + 1
      END DO

*  If no free slots found during scan, then next free slot is next
*  highest index value.  Initialise re-try variables for first attempt
*  at access.
      IF ( NEW_ACCESS_COUNT .EQ. 0 ) NEW_ACCESS_COUNT = I
      IF ( RETRYING ) OBJECT_ACTIVE = .FALSE.

*  We may return to this point when an access has failed and we are
*  re-trying it immediately, possibly with a different file name.
  1   CONTINUE

*  Entry point for ***RE-MAP***
*  Force any IMAGE object whose mapped address is non-positive to
*  be regarded as inactive.
      IF ( TYPE .EQ. 'IMAGE' .AND. ( MAPPED_ADDRESS .EQ. -1 .OR.
     :     MAPPED_ADDRESS .EQ. 0 ) ) OBJECT_ACTIVE = .FALSE.

*  If object not recorded as already active then
*  If type of object is IMAGE then
*  If possibly a list of image frames then
*  IMPORTANT NOTE : there is a significant difference here in the way
*                   that single image parameters, and list-of-image
*                   parameters are dealt with. Single image parameters
*                   have a wodge of code which copes with allowing the
*                   user to keep changing the actual container file
*                   referenced. In contrast a list-of-images MUST not
*                   be changed after it has been first input, as there
*                   is no code to detect changes and close and re-open
*                   the correct files. This could be added but it would
*                   be even more complicated than the current mess.
*   If we have name of last associated disk file then
*      Save name
*   Else if we are re-trying a failed access then
*      Force a re-prompt for file name (as it may be incorrect and
*      causing the failure)
*   Else
*      Get filename from image name parameter
*   END IF
      IF ( .NOT. OBJECT_ACTIVE ) THEN

*     IMAGE
*     =====

         IF ( TYPE .EQ. 'IMAGE' ) THEN
            NEW_OBJECT_TYPE = 'IMAGE'
            IF ( STATIC_INDICES .GT. 0 ) THEN
               IF ( LAST_NAME .NE. 'UNSET AS YET!' ) THEN
                  TEMP_STRING = LAST_NAME

               ELSE IF ( RETRYING ) THEN
                  WORK_STRING = 'FORCE-' // REQUIRED_OBJECT
                  CALL ECH_GET_PARAMETER(
     :                 WORK_STRING, 'CHAR', DUMMY_VALUE,
     :                 .FALSE., TEMP_STRING, 0, STATUS )
                  IF ( ECH_FATAL_ERROR( STATUS ) ) GO TO 999
                  CALL ECH_SETUP_OBJECT_REF( REQUIRED_OBJECT, IDUMMY,
     :              IDUMMY, IDUMMY, TEMP_STRING, 0., .FALSE.,
     :              ECH__IMAGE_LIST )

               ELSE
                  CALL ECH_GET_PARAMETER( REQUIRED_OBJECT, 'CHAR',
     :                  DUMMY_VALUE, .FALSE., TEMP_STRING, 0, STATUS )
                  IF ( ECH_FATAL_ERROR( STATUS ) ) GO TO 999
                  CALL ECH_SETUP_OBJECT_REF( REQUIRED_OBJECT, IDUMMY,
     :                 IDUMMY, IDUMMY, TEMP_STRING, 0., .FALSE.,
     :                 ECH__IMAGE_LIST )
               END IF

*           Exit now if image is ARC and value is NONE
*           This module contains special code to deal with the association
*           of 'switched' image naming parameters ARC and FFIELD with the
*           switch parameters TUNE_NOARC and TUNE_NOFLAT. This is messy and
*           should be changed in the future into a more general purpose
*           mechanism.
               IF ( REQUIRED_OBJECT .EQ. 'ARC' ) THEN
                  IF ( TEMP_STRING .EQ. 'NONE' .OR.
     :                 TEMP_STRING .EQ. 'none' ) THEN
                     CALL ECH_SET_PARAMETER( 'TUNE_NOARC', 'LOGICAL',
     :                    0., .TRUE., ' ', STATUS )
                     IO_STRING = 'NONE'
                     GO TO 900

                  ELSE
                     CALL ECH_SET_PARAMETER( 'TUNE_NOARC', 'LOGICAL',
     :                    0., .FALSE., ' ', STATUS )
                  END IF
               END IF

*           Snip out n'th image name from supplied list.
*           Open the container file.
               I = 1
               IST = 1
               IF ( STATUS .EQ. ECH__IS_ACCESSED ) STATUS = SAI__OK
               CALL ECH_PAR_OBJECT( TEMP_STRING, IST, LIM1, LIM2,
     :              EQULS, TERM, NEXT )
               REQUIRED_FILE = TEMP_STRING
               DO WHILE ( NEXT .GT. 0 )
                  IF ( I .EQ. DIMEN_VALUE( 1 ) ) THEN
                     REQUIRED_FILE = TEMP_STRING( LIM1 : LIM2 )
                  END IF
                  I = I + 1
                  CALL ECH_PAR_OBJECT( TEMP_STRING, IST, LIM1, LIM2,
     :                 EQULS, TERM, NEXT )
                  IST = NEXT
               END DO
               IF ( STATUS .NE. ECH__ABORT_OPTION )
     :            CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH, 'OPEN',
     :                 'STRUCTURE', 0, 0, 0, DUMDIM, MAX_DIMENSIONS,
     :                 0, REQUIRED_FILE( : LIM2 - LIM1 + 1 ), STATUS )

            ELSE
               IF ( IMAGE_NAME .EQ. 'UNSET AS YET!' ) THEN
                  IF ( RETRYING ) THEN
                     WORK_STRING = 'FORCE-' // FULL_OBJECT_PATH
                     CALL ECH_GET_PARAMETER( WORK_STRING, 'CHAR',
     :                    DUMMY_VALUE, .FALSE., TEMP_STRING, 0,
     :                    STATUS )
                     IF ( ECH_FATAL_ERROR( STATUS ) ) GO TO 999

                  ELSE
                     CALL ECH_GET_PARAMETER( FULL_OBJECT_PATH,
     :                    'CHAR', DUMMY_VALUE, .FALSE., TEMP_STRING,
     :                    0, STATUS )
                     IF ( ECH_FATAL_ERROR( STATUS ) ) GO TO 999
                  END IF

*              Exit now if image is FFIELD and value is NONE.
                  IF ( REQUIRED_OBJECT .EQ. 'FFIELD' ) THEN
                     IF ( TEMP_STRING .EQ. 'NONE' .OR.
     :                    TEMP_STRING .EQ. 'none' ) THEN
                        CALL ECH_SET_PARAMETER( 'TUNE_NOFLAT',
     :                       'LOGICAL', 0., .TRUE., ' ', STATUS )
                        IO_STRING = 'NONE'
                        GO TO 900

                     ELSE
                        CALL ECH_SET_PARAMETER( 'TUNE_NOFLAT',
     :                       'LOGICAL', 0., .FALSE., ' ', STATUS )
                     END IF
                  END IF

                  IF ( STATUS .EQ. ECH__IS_ACCESSED ) THEN
                     STATUS = SAI__OK
                  END IF
                  IF ( STATUS .NE. ECH__ABORT_OPTION ) THEN
                     CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :                    'OPEN', 'STRUCTURE', 0, 0, 0,
     :                    DUMDIM, MAX_DIMENSIONS, 0,
     :                    TEMP_STRING( :CHR_LEN( TEMP_STRING ) ),
     :                    STATUS )

                     CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :                    'READ-NAME', 'STRUCTURE', 0, 0, 0,
     :                    DUMDIM, MAX_DIMENSIONS, 0, IMAGE_NAME,
     :                    STATUS )
                     SAVE_PATH = FULL_OBJECT_PATH
                  END IF

               ELSE
                  FULL_OBJECT_PATH = SAVE_PATH
               END IF

               I = 1
               ALREADY_ACTIVE = .FALSE.
               DO WHILE ( .NOT. ALREADY_ACTIVE .AND.
     :                    I .LE. ACCESS_COUNT )
                  IF ( OBJECT_TYPE( I ) .EQ. 'IMAGE' .AND.
     :                 OBJECT_ADDRESS( I ) .NE. 0 .AND.
     :                 FULL_OBJECT_PATH .NE. OBJECT_NAME( I ) )
     :                 THEN
                     CALL ECH_ACCESS_OBJECT( OBJECT_NAME( I ),
     :                    'READ-NAME', 'STRUCTURE', 0, 0, 0,
     :                    DUMDIM, MAX_DIMENSIONS, 0,
     :                    OLD_IMAGE_NAME, STATUS )
                     IF ( IMAGE_NAME .EQ. OLD_IMAGE_NAME .AND.
     :                    STATUS .EQ. SAI__OK )  THEN
                        IF ( ( OBJECT_QADDRESS( I ) .NE. 0
     :                       .AND. .NOT. NEEDS_QUALITY ) .OR.
     :                       ( OBJECT_QADDRESS( I ) .EQ. 0
     :                       .AND. NEEDS_QUALITY ) ) THEN
                           IF ( OBJECT_STATICS( I ) .EQ.
     :                          STATIC_INDICES ) THEN
                              ALREADY_ACTIVE = .TRUE.
                              FULL_OBJECT_PATH = OBJECT_NAME( I )
                              OBJECT_NUMBER = I
                              MAPPED_ADDRESS = OBJECT_ADDRESS( I )
                              MAPPED_QADDRESS =
     :                              OBJECT_QADDRESS( I )
                              MAPPED_EADDRESS =
     :                              OBJECT_EADDRESS( I )
                           END IF
                        END IF
                     END IF
                  END IF
                  I = I + 1
               END DO

*           If object already active with incorrect mappings then
*              GO TO ***RE-MAP*** above
*           END IF
               IF ( ALREADY_ACTIVE ) THEN
                  GO TO 1
               END IF
            END IF

*        NOTE: this strategy presumes that the dimensions of the first
*            image opened are correct, and then validates the
*            dimensions of subsequent images by checking against these
*            values.
            IF ( STATUS .EQ. SAI__OK ) THEN
               DATA_FILES_OPEN = .TRUE.
               CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :              'READ-SIZE', 'IMAGE-DATA', NEW_OBJECT_SIZE,
     :              0, 0, DIMENSIONS, MAX_DIMENSIONS, NUM_DIM ,
     :              ' ', STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  STATUS = SAI__OK
                  CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :                    'UNMAP', 'STRUCTURE', 0, 0, 0,
     :                    DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
                  STATUS = ECH__RETRY_ACCESS

               ELSE IF ( NUM_DIM .LT. 2 ) THEN
                  IF ( REPORT_MODE .NE. 0 )
     :               CALL ECH_REPORT( 0, ' Image is 1-D!' )

               ELSE IF ( NY .EQ. 0 ) THEN
                  NX = DIMENSIONS( 1 )
                  NY = DIMENSIONS( 2 )

               ELSE IF ( DIMENSIONS( 1 ) .NE. NX ) THEN
                  STATUS = SAI__OK
                  CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :                 'UNMAP', 'STRUCTURE', 0, 0, 0,
     :                 DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
                  IF ( DIMENSIONS( 1 ) .EQ. NY .AND.
     :                 DIMENSIONS( 2 ) .EQ. NX ) THEN
                     CALL ECH_SET_CONTEXT(
     :                    'PROBLEM', 'Needs rotating' )

                  ELSE
                     CALL ECH_SET_CONTEXT(
     :                    'PROBLEM', 'Bad X dimension' )
                  END IF
                  STATUS = ECH__DIM_CONFLICT

               ELSE IF ( DIMENSIONS( 2 ) .NE. NY ) THEN
                  STATUS = SAI__OK
                  CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :                 'UNMAP', 'STRUCTURE', 0, 0, 0,
     :                 DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
                  STATUS = ECH__DIM_CONFLICT
                  CALL ECH_SET_CONTEXT( 'PROBLEM',
     :                 'Bad Y dimension')
               END IF
            END IF

*     OUTIMAGE
*     ========

*     Else if  object type is OUTIMAGE (output image file) then
*      If object to be created is RBNOBJ (rebinned image frame) then
*         NOTE : this special code for creating rebinned output image
*                frames is here because the overhead in generating
*                a general purpose solution was not worthwhile.
*                RBNOBJ is the only output image with dimensions
*                different from the input images, so its easier to
*                treat it as a unique special case. A more general solution
*                would need to use ECH_GET_DATA_PATH to read and interpret
*                parameterised dimensions and then use them in the creation.
*         Create the container file
*         Read the dimensions of any input image (last one accessed is used)
*         Read value of NX_REBIN object
*         Create output image data arrays of dimensions (NX_REBIN,NY)
         ELSE IF ( TYPE .EQ. 'OUTIMAGE' ) THEN
            NEW_OBJECT_TYPE = TYPE
            IF ( REQUIRED_OBJECT .EQ. 'RBNOBJ' ) THEN
               CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :              'CREATE', 'STRUCTURE', 0, 0, 0, DUMDIM,
     :              MAX_DIMENSIONS, 0, ' ', STATUS )
               CALL ECH_ACCESS_OBJECT( LAST_IMAGE,
     :              'READ-SIZE', 'IMAGE-DATA',
     :              NEW_OBJECT_SIZE, 0, 0, DIMENSIONS,
     :              MAX_DIMENSIONS, NUM_DIM , ' ', STATUS )
               CALL ECH_ACCESS_OBJECT( 'ECH_RDCTN.MAIN.NX_REBIN',
     :              'READ', 'INT ', 1,
#if HAVE_PERCENTLOC
     :              %LOC( DIMENSIONS ),
#elif HAVE_INTRINSIC_LOC
     :              LOC( DIMENSIONS ),
#else
               Do not have LOC or %LOC
#endif
     :              0, DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
               CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :              'CREATE', 'IMAGE-DATA,ERRORS', 0, 0, 0,
     :              DIMENSIONS, MAX_DIMENSIONS, NUM_DIM, ' ', STATUS )
               NEW_OBJECT_SIZE = 1
               DO I = 1, NUM_DIM
                  NEW_OBJECT_SIZE = NEW_OBJECT_SIZE * DIMENSIONS( I )
               END DO

            ELSE
               CALL ECH_ACCESS_OBJECT( LAST_IMAGE,
     :              'READ-SIZE', 'IMAGE-DATA',
     :              NEW_OBJECT_SIZE, 0, 0, DIMENSIONS,
     :              MAX_DIMENSIONS, NUM_DIM , ' ', STATUS )
               CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :              'CREATE', 'IMAGE-DATA', 0, 0, 0, DIMENSIONS,
     :              MAX_DIMENSIONS, NUM_DIM, ' ', STATUS )
               NEW_OBJECT_SIZE = 1
               DO I = 1, NUM_DIM
                  NEW_OBJECT_SIZE = NEW_OBJECT_SIZE * DIMENSIONS( I )
               END DO
            END IF

*     Else
*        Read the dimensions of the object
*        If got dimensions Ok then
*         Read object type and determine memory requirements
*        END IF
*     END IF
         ELSE
            CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :           'READ-SIZE', TYPE, 0, 0, 0,
     :           DIMENSIONS, MAX_DIMENSIONS,
     :           NUM_DIM , ' ', STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               NEW_OBJECT_SIZE = 1
               DO I = 1, NUM_DIM
                  NEW_OBJECT_SIZE = NEW_OBJECT_SIZE * DIMENSIONS( I )
               END DO
               CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :              'READ-TYPE', NEW_OBJECT_TYPE, 0, 0, 0,
     :              DUMDIM, MAX_DIMENSIONS, 0 , ' ', STATUS )
               NEW_Q_ADDRESS = DIMENSIONS( 1 )
            END IF
         END IF

*     If we have got this far ok then we have an open container file
*     and we now need to actually map or read the actual value of the
*     particular object we are interested in.
*
*     If object accessible then
*        If type of object is character string then
*           Read string text
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( NEW_OBJECT_TYPE .EQ. 'CHAR' .AND.
     :           TYPE .EQ. 'CHAR') THEN
               CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :              'READ', TYPE, NEW_OBJECT_SIZE, 0, 0,
     :              0 , 1, 0, TEMP_STRING, STATUS )
               IO_STRING = TEMP_STRING
               NEW_ADDRESS = 1

*        Else if object type is IMAGE then
*         Check if we need the quality info as well (array/flags)
*         Set update access if we are using quality
*         NOTE: the environments so far implemented both allow
*               access via a quality array, and via flag values
*               inserted into the data array. If a newly implemented
*               environment does not provide this capability then
*               it would need to be added into the ECH_ACCESS_OBJECT
*               routine (operation ENABLE).

            ELSE IF ( TYPE .EQ. 'IMAGE' ) THEN

*              Loop thru all known active objects
*               If found entry for requested object then
*                If disk file name has not been changed then
*                 If mapping differs from that requested then
*                  EG if mapping is DATA AND QUALITY and the  request is
*                     for DATA ONLY then we need to release the previous
*                     mappings and re-map just the DATA array.
*                   Make local copy of object properties
*                  END IF
*                 END IF
*                END IF
               I = 1
               OK_ACTIVE = .FALSE.
               DO WHILE ( .NOT. OK_ACTIVE .AND.
     :                    ( I .LE. ACCESS_COUNT ) )
                  IF ( object_type( i ) .EQ. 'IMAGE' .AND.
     :                 object_address( i ) .NE. 0   .AND.
     :                 full_object_path .NE. object_name( i ) ) THEN
                     CALL ECH_ACCESS_OBJECT( object_name(i),
     :                    'READ-NAME', 'STRUCTURE', 0, 0, 0,
     :                    dumdim, MAX_DIMENSIONS, 0, old_image_name,
     :                    status )
                     IF ( image_name .EQ. old_image_name .AND.
     :                    status .EQ. SAI__OK )  THEN
                        IF ( ( object_qaddress( i ) .NE. 0 )
     :                       .AND. needs_quality .OR.
     :                       ( ( object_qaddress( i ) .EQ. 0 )
     :                       .AND. .NOT. needs_quality ) ) THEN
                           IF ( object_statics( i ) .EQ.
     :                          static_indices ) THEN
                              ok_active = .TRUE.
                              full_object_path = object_name( i )
                              new_address = object_address( i )
                              new_q_address = object_qaddress( i )
                              new_e_address = object_eaddress( i )
                           END IF
                        END IF
                     END IF
                  END IF
                  i = i + 1
               END DO

               IF ( .NOT. OK_ACTIVE ) THEN
                  NEW_Q_ADDRESS = 0
                  NEW_E_ADDRESS = 0
                  IF ( NEEDS_QUALITY ) THEN
                     QTYMODE = 'IMAGE-QUALITY'

                  ELSE
                     QTYMODE = 'IMAGE-FLAGS'
                  END IF
                  MAPMODE = 'READ'
                  IF ( NEEDS_QUALITY ) MAPMODE = 'UPDATE'

*              Read the name of the container (synonymous with image name)
*              MAP the image data array to a floating point array in memory
*              NOTE: this strategy assumes that the environment can provide
*                    memory mapping, and also that it can provide for
*                    automatic type translation (eg INTEGER to FLOAT) if
*                    it is needed. If the environment does not provide these
*                    capabilites then they would need to be added into
*                    ECH_ACCESS_OBJECT( operation MAP-IMAGE ).
                  CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :                 'READ-NAME', 'STRUCTURE', 0, 0, 0, DUMDIM,
     :                 MAX_DIMENSIONS, 0, IMAGE_NAME, STATUS )
                  IO_STRING = IMAGE_NAME
                  CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :                 'MAP-IMAGE', 'FLOAT', 0, NEW_ADDRESS,
     :                 NEW_OBJECT_HANDLE, DUMDIM, MAX_DIMENSIONS, 0,
     :                 'READ', STATUS )
                  CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :                 'ENABLE', QTYMODE, 0, 0, 0, DUMDIM,
     :                 MAX_DIMENSIONS, 0, ' ', STATUS )

*              If data quality array requested then
*                 Map the data quality to a byte array in memory
*              END IF
*              If data errors array requested then
*                 Map the data errors to a floating point array in memory
*              END IF
                  LAST_IMAGE = FULL_OBJECT_PATH
                  IF ( NEEDS_QUALITY .AND. STATUS .EQ. SAI__OK ) THEN
                     DO I = 1, ACCESS_COUNT
                        IF ( OBJECT_QADDRESS( I ) .NE. 0 .AND.
     :                       OBJECT_ADDRESS( I ) .EQ. NEW_ADDRESS .AND.
     :                       OBJECT_NAME( I ) .NE. FULL_OBJECT_PATH
     :                       ) THEN
                           NEW_Q_ADDRESS = OBJECT_QADDRESS( I )
                           NEW_Q_HANDLE = OBJECT_QHANDLE( I )
                        END IF
                     END DO
                     CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :                    'MAP-QUALITY', 'BYTE',
     :                    0, NEW_Q_ADDRESS, NEW_Q_HANDLE,
     :                    DUMDIM, MAX_DIMENSIONS, 0, MAPMODE, STATUS )
                  END IF
                  IF ( NEEDS_ERRORS .AND. STATUS .EQ. SAI__OK ) THEN
                     CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :                    'MAP-ERRORS', 'FLOAT', 0, NEW_E_ADDRESS,
     :                    NEW_E_HANDLE, DUMDIM, MAX_DIMENSIONS, 0,
     :                    'READ', STATUS )

                  ELSE
                     NEW_E_ADDRESS = 0
                     NEW_E_HANDLE = 0
                  END IF
               END IF

*        Else if object type is an output image then
*           Map the image to a floating array with write access
*           If data quality array requested then
*              Map the data quality to a byte array in memory
*           END IF
*           If data errors array requested then
*              Map the data errors to a floating point array in memory
*           END IF
            ELSE IF ( type .EQ. 'OUTIMAGE' ) THEN
               CALL ECH_ACCESS_OBJECT( full_object_path,
     :              'MAP-IMAGE', 'FLOAT', 0, new_address,
     :              new_object_handle, dumdim, MAX_DIMENSIONS, 0,
     :              'WRITE', status )
               IF ( needs_errors .AND. status .EQ. SAI__OK ) THEN
                  CALL ECH_ACCESS_OBJECT( full_object_path,
     :                 'MAP-ERRORS', 'FLOAT', 0, new_e_address,
     :                 new_e_handle, dumdim, MAX_DIMENSIONS, 0, 'W',
     :                 status )
               END IF
               IF ( needs_quality .AND. status .EQ. SAI__OK ) THEN
                  CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :                 'MAP-QUALITY', 'BYTE',
     :                 0, NEW_Q_ADDRESS, NEW_Q_HANDLE,
     :                 DUMDIM, MAX_DIMENSIONS, 0, 'W', STATUS )
               END IF

*        Else
*           Map object (any other type) to an array in memory
            ELSE
               CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :              'MAP-READ', TYPE, NEW_OBJECT_SIZE,
     :              NEW_ADDRESS, NEW_OBJECT_HANDLE,
     :              DIMENSIONS, MAX_DIMENSIONS,
     :              NUM_DIM , ' ', STATUS )
               IF ( ECH_FATAL_ERROR( STATUS ) ) GO TO 999
            END IF

            mapped_address = new_address
            mapped_qaddress = new_q_address
            mapped_eaddress = new_e_address

*        If successfully mapped/read then
*           Record properties of mapping in object property tables
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( NEW_ACCESS_COUNT .GT. ACCESS_COUNT )
     :            ACCESS_COUNT = NEW_ACCESS_COUNT
               OBJECT_NAME( NEW_ACCESS_COUNT ) = FULL_OBJECT_PATH
               OBJECT_TYPE( NEW_ACCESS_COUNT ) = NEW_OBJECT_TYPE
               OBJECT_SIZE( NEW_ACCESS_COUNT ) = NEW_OBJECT_SIZE
               OBJECT_INDEX( NEW_ACCESS_COUNT ) =
     :                  ECH_OBJ_IND( REQUIRED_OBJECT )
               OBJECT_EHANDLE( NEW_ACCESS_COUNT ) = NEW_E_HANDLE
               OBJECT_QHANDLE( NEW_ACCESS_COUNT ) = NEW_Q_HANDLE
               OBJECT_HANDLE( NEW_ACCESS_COUNT ) = NEW_OBJECT_HANDLE
               OBJECT_ADDRESS( NEW_ACCESS_COUNT ) = NEW_ADDRESS
               OBJECT_QADDRESS( NEW_ACCESS_COUNT ) = NEW_Q_ADDRESS
               OBJECT_EADDRESS( NEW_ACCESS_COUNT ) = NEW_E_ADDRESS
               OBJECT_STATICS( NEW_ACCESS_COUNT ) = STATIC_INDICES
               OBJECT_ACTIVE = .TRUE.
               OBJECT_NUMBER = NEW_ACCESS_COUNT
            END IF

*     This section is a frill designed to allow for objects to fail
*     to be found in the image data file, and then to be provided by
*     the user by use of environment variables and vectors. This
*     facility is intended for simple objects such as lists of
*     bad-rows and columns, detector characteristics etc, which may
*     or may not be provided in the image header by the various
*     observatories generating the data. We only check those objects
*     which are explicitly expected to be in the data frame because
*     other objects which are expected to be found in reduction database
*     files are also searched for in the data frame first. This allows
*     the FUTURE possiblity of an observatory providing reduction
*     information (such as order trace polynomials say) in the
*     data frame. This information could then be used automatically
*     by the programs if present. This will require co-operation from
*     the observatory in naming the objects appropriately, or
*     the defition of appropriate pathways in the ECH_RDCTN.DEF
*     file. Eg. **REGISTERED PATH => TRC_POLY X.TRACE_COEFFS[maximum_poly]
*
*     If object was expected to be in the datafile then
*        If object is a vector then
*           Try to read its dimension from an environment variable
         ELSE
            IF ( EXPECT_IN_DATAFILE(
     :         ECH_OBJ_IND( REQUIRED_OBJECT ) ) ) THEN
               STATUS = SAI__OK
               VSTAT = 0
               ASIZE = 1
               DUMMY_DIMEN = 1
               CSIZE = WS_DIMENSIONS( ECH_OBJ_IND( REQUIRED_OBJECT ) )
               IF ( CSIZE .NE. ' ' ) THEN
                  CALL ECH_GET_PARAMETER( CSIZE, 'INT',
     :                 DUMMY_DIMEN, .FALSE., ' ', 0, STATUS )
                  VSTAT = STATUS
                  IF ( DUMMY_DIMEN .EQ. 0 .AND.
     :                 CSIZE .EQ. ' ' ) CSIZE = 'NOTINUSE'
                  IF ( DUMMY_DIMEN .EQ. 0 ) VSTAT = -1
                  ASIZE = MAX( DUMMY_DIMEN, 1 )
               END IF
               NEW_Q_ADDRESS = ASIZE
               NEW_OBJECT_TYPE = TYPE
               STATUS = SAI__OK
               CALL ECH_ACCESS_OBJECT( 'WORKSPACE',
     :              'MAP', TYPE, ASIZE, NEW_ADDRESS,
     :              NEW_OBJECT_HANDLE, DUMDIM, MAX_DIMENSIONS, 0,
     :              ' ', STATUS )
               NEW_OBJECT_SIZE = ASIZE

*           Reserve workspace memory space for object
*           If workspace obtained Ok then
*              Try to map object from an environment variable
*           Else
*              Report problem and return immediately
*           END IF
*           If failed to map variable then
*              Report name of variable looked for and name of variable
*                defining its size if a vector
*           END IF
               IF ( STATUS .EQ. SAI__OK ) THEN
                  STATUS = ECH__WORKSPACE_PAR
                  IF ( CSIZE .NE. ' ' ) THEN
                     CALL ECH_TRANSFER_INT( 0, %VAL( NEW_ADDRESS ) )
                     IF ( VSTAT .EQ. 0 ) THEN
                        CALL ECH_ACCESS_OBJECT(
     :                       REQUIRED_OBJECT, 'READ', TYPE,
     :                       ASIZE, NEW_ADDRESS, 0,
     :                       DUMDIM, MAX_DIMENSIONS, 0, ' ', VSTAT )
                     END IF
                     STATUS = ECH__IS_ACCESSED
                  END IF
                  IF ( NEW_ACCESS_COUNT .GT. ACCESS_COUNT )
     :               ACCESS_COUNT = NEW_ACCESS_COUNT
                  MAPPED_ADDRESS = NEW_ADDRESS
                  MAPPED_QADDRESS = NEW_Q_ADDRESS
                  OBJECT_NAME( NEW_ACCESS_COUNT ) =
     :                  FULL_OBJECT_PATH
                  OBJECT_INDEX( NEW_ACCESS_COUNT ) =
     :                  ECH_OBJ_IND( REQUIRED_OBJECT )
                  OBJECT_TYPE( NEW_ACCESS_COUNT ) =
     :                  NEW_OBJECT_TYPE
                  OBJECT_SIZE( NEW_ACCESS_COUNT ) =
     :                  NEW_OBJECT_SIZE
                  OBJECT_HANDLE( NEW_ACCESS_COUNT ) =
     :                  NEW_OBJECT_HANDLE
                  OBJECT_ADDRESS( NEW_ACCESS_COUNT ) =
     :                  NEW_ADDRESS
                  OBJECT_QADDRESS( NEW_ACCESS_COUNT ) =
     :                  NEW_Q_ADDRESS
                  OBJECT_STATICS( NEW_ACCESS_COUNT ) = 0
                  OBJECT_ACTIVE = .TRUE.
                  OBJECT_NUMBER = NEW_ACCESS_COUNT

               ELSE
                  CALL ECH_SET_CONTEXT( 'PROBLEM', 'Vmem exhausted' )
                  STATUS = ECH__BAD_VMEMORY
                  GO TO 999
               END IF
               IF ( VSTAT .NE. 0 ) THEN
                  WORK_STRING = ' Unable to obtain object ' //
     :                REQUIRED_OBJECT(
     :                :ECH_WORD_LEN( REQUIRED_OBJECT ) ) //
     :                ' from either input data frame or '
                  CALL ECH_REPORT( 0, WORK_STRING )
                  WORK_STRING = ' a user-supplied array.  ' //
     :                'Values may be ' //
     :                'supplied in variable array '//
     :                REQUIRED_OBJECT(
     :                :ECH_WORD_LEN( REQUIRED_OBJECT ) )
                  CALL ECH_REPORT( 0, WORK_STRING )
                  WORK_STRING = ' with number of elements' //
     :                ' specified using ' //
     :                CSIZE( :ECH_WORD_LEN( CSIZE ) ) // '.'
                  CALL ECH_REPORT( 0, WORK_STRING )
               END IF
            END IF
         END IF

*     If diagnostics are active report data access request and result.
         IF ( DIAGNOSTICS_ACTIVE ) THEN
            CALL CHR_ITOC( STATUS, REF_STR1, NCHAR1 )
            REPORT_STRING = ' DATA Access: ' //
     :            FULL_OBJECT_PATH( :ILEN ) //
     :            ', status: ' // REF_STR1( :NCHAR1 ) // '.'
            CALL ECH_REPORT( RPM_LOG, REPORT_STRING )
            IF ( OBJECT_NUMBER .GT. 0 ) THEN
               CALL CHR_ITOC( OBJECT_NUMBER, REF_STR1, NCHAR1 )
               CALL CHR_ITOC( OBJECT_SIZE( OBJECT_NUMBER ), REF_STR2,
     :              NCHAR2 )
               CALL CHR_ITOC( OBJECT_HANDLE( OBJECT_NUMBER ),
     :              REF_STR3, NCHAR3 )
               CALL CHR_ITOC( OBJECT_ADDRESS( OBJECT_NUMBER ),
     :              REF_STR4, NCHAR4 )
               REPORT_STRING = ' using slot ' //
     :               REF_STR1( :NCHAR1 ) // ' ' //
     :               OBJECT_TYPE( OBJECT_NUMBER )
     :               ( :ECH_WORD_LEN(
     :               OBJECT_TYPE( OBJECT_NUMBER ) ) ) //
     :               ' ' // REF_STR2( :NCHAR2 ) //
     :               ' bytes, handle: ' //
     :               REF_STR3( :NCHAR3 ) // '@' //
     :               REF_STR4( :NCHAR4 ) // '.'
              CALL ECH_REPORT( RPM_LOG, REPORT_STRING )
            END IF
         END IF

*  Return status indicating that object mapping is already in place.
      ELSE
         IF ( MAPPED_ADDRESS .NE. 0 .AND. MAPPED_ADDRESS .NE. -1
     :        .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = ECH__IS_ACCESSED

         ELSE
            STATUS = ECH__PAR_ACCESSED
         END IF
      END IF

      GO TO 999

*  We jump directly here when we have failed to correctly map the object
*  we wanted, and we need to clear all the properties we may have
*  already established for the object in our property tables.
 900  CONTINUE
      IF ( NEW_ACCESS_COUNT .GT. ACCESS_COUNT )
     :   ACCESS_COUNT = NEW_ACCESS_COUNT
      OBJECT_NAME( NEW_ACCESS_COUNT ) = FULL_OBJECT_PATH
      OBJECT_TYPE( NEW_ACCESS_COUNT ) = 'NULL'
      OBJECT_INDEX( NEW_ACCESS_COUNT ) = 0
      OBJECT_SIZE( NEW_ACCESS_COUNT ) = 0
      OBJECT_HANDLE( NEW_ACCESS_COUNT ) = 0
      OBJECT_ADDRESS( NEW_ACCESS_COUNT ) = 0
      OBJECT_STATICS( NEW_ACCESS_COUNT ) = 0
      OBJECT_ACTIVE = .TRUE.
      OBJECT_NUMBER = NEW_ACCESS_COUNT

 999  CONTINUE

      END
