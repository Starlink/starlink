*+  RED4_REMOVE_INTEGRATION - removes desired integration from the observation
*   result arrays
      SUBROUTINE RED4_REMOVE_INTEGRATION (STATUS)
*    Description :
*     This routine removes the nominated integration result from the coadded
*     result in the corresponding reduced observation file. The structure
*     that indicated the integration had been added is deleted. If the reduced
*     integration cannot be found or if the integration has not been coadded
*     into the result array in the first place, a warning message is output
*     and nothing is done. No check is made that the integration removed
*     had been reduced in the same way as the one that had been coadded.
*    Invocation :
*     CALL RED4_REMOVE_INTEGRATION (STATUS)
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     It would probably have been better to have called this routine
*     RED4_SUBTRACT_INTEGRATION, as it is the opposite of
*     RED4_ADD_INTEGRATION.
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Lightfoot (REVAD::JFL)
*     Steven Beard (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*          1989 ?: Original version.                             (JFL)
*     22-Jan-1990: History and Authors added. Modified to overcome
*                  compilation problems in RED4_DO_REMOVE.       (SMB)
*     24-Apr-1990: Because of memory corruption problems, the
*                  code needs to be compiled with array bounds
*                  checking switched on. The Figaro dynamic
*                  memory functions (DYN_ELEMENT, DYNAMIC_MEM,
*                  DYN_INCREMENT) would not allow this. Code
*                  modified to use %val() instead.               (SMB)
*     23-Jul-1990: Character handling improved, and modified
*                  to accept 4 digit observation numbers.
*                  Also code spaced out.                         (SMB)
*     29-Aug-1990: The 15 character limit on the length of
*                  the name of the DSA COADDS structures was
*                  causing problems when the observation and
*                  integration numbers became large. COADDS
*                  structure naming convention changed from
*                  Iyymmdd_oooo_iiii to I_oooo_iiii (to keep up
*                  with change made to RED4_ADD_INTEGRATION).    (SMB)
*     29-Aug-1990: Phase 1 of major changes: Header
*                  information in the observation and reduced
*                  observation files is now written in a .FITS
*                  structure. Linearisation coefficients are
*                  now obtained from a file rather than being
*                  buried in the data structure. KTC mode
*                  replaced by NDR.                              (SMB)
*      3-Sep-1990: Phase 2 of major changes: Header
*                  information in the integration and reduced
*                  integration files is now written in a .FITS
*                  structure. MAXINDDIM, MAXOBSDIM and
*                  MAXINTDIM parameters added.                   (SMB)
*     23-Oct-1990: Commented out code removed.                   (SMB)
*     31-Oct-1990: Modified to use RED4_DELETE_STRUCTURE.        (SMB)
*     17-Feb-1991: Commented out code removed.                   (SMB)
*     23-Feb-1993: Conform to error strategy                     (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER ICH_LEN                        ! Figaro string length function
*    Local Constants :
      INTEGER MAXINDDIM                      ! Maximum index dimensions
      PARAMETER ( MAXINDDIM = 2 )
      INTEGER MAXOBSDIM                      ! Maximum observation dimensions
      PARAMETER ( MAXOBSDIM = 2 )
      INTEGER MAXINTDIM                      ! Maximum integration dimensions
      PARAMETER ( MAXINTDIM = 2 )
      INTEGER DTA__OK                    ! DTA success status
      PARAMETER ( DTA__OK = 0 )
*    Local variables :
      LOGICAL FOUND                          ! T if integration has been coadded
      LOGICAL VARIANCE_MAP                   ! T if variance array mapped.
      INTEGER ADDRESS                        ! DSA mapping stuff
      INTEGER INDEX_PTR                      ! for index array...
      INTEGER INDEX_SLOT                     !        "
      INTEGER COADDS_PTR                     ! for coadds array
      INTEGER COADDS_SLOT                    !        "
      INTEGER OBSDATA_PTR                    ! for observation data array
      INTEGER OBSDATA_SLOT                   !        "
      INTEGER OBSVAR_PTR                     ! for observation variance array
      INTEGER OBSVAR_SLOT                    !        "
      INTEGER OBSQUAL_PTR                    ! for observation quality array
      INTEGER OBSQUAL_SLOT                   !        "
      INTEGER INTDATA_PTR                    ! for integration data array
      INTEGER INTDATA_SLOT                   !        "
      INTEGER INTQUAL_PTR                    ! for integration quality array
      INTEGER INTQUAL_SLOT                   !        "
      INTEGER INDEX_DIMS( MAXINDDIM )        ! dimensions of index array
      INTEGER OBSDIMS( MAXOBSDIM )           ! dims of observation data array
      INTEGER INTDIMS( MAXINTDIM )           ! dims of integration data array
      INTEGER DET_INDEX                      ! the index of the detector position
*                                            !    the integration was taken at
      INTEGER LDAY, LDATE, LHOUR             ! lengths of date strings
      INTEGER DTA_STATUS                     !
      INTEGER NELM_IND                       !
      INTEGER NELM_INT                       !
      INTEGER NELM_OBS                       !
      INTEGER NDIM                           !
      INTEGER N                              !
      INTEGER IGNORE                         ! unimportant parameter
      INTEGER CLEN                           ! Non-blank length of character string
      REAL INTEGRATION_TIM                   ! integration time in secs of the
*                                            !    reduced integration being added
      REAL OBSERVATION_TIM                   ! total integration time of the
*                                            !    points at detector index 1 in
*                                            !    the reduced observation
      CHARACTER*80 OBSREDFILE                ! name of reduced observation file
      CHARACTER*80 INTFILE                   ! basic name of integration file
*                                            !    (unreduced)
      CHARACTER*80 INTREDFILE                ! full name of intfile i.e. RDIR:
*                                            !    intfile
      CHARACTER*80 COADDED_INTS              ! DTA name of structure in reduced
*                                            !    observation file holding info
*                                            !    on integrations already added
      CHARACTER*80 COADD_NAME                ! name of structure in COADDED_INTS
*                                            !    detailing integration added
      CHARACTER*80 COADD_TEST                ! The name of the COADD structure
*                                            !    which would correspond to
*                                            !    a given integration.

      CHARACTER*80 OBS_TYPE                  ! The observation type (BIAS,
*                                            !    DARK, FLAT etc...)
      CHARACTER*80 TIME                      ! String holding date of last
*                                            !    addition
      CHARACTER*20 DAY, DATE, HOUR           ! Components of date
      CHARACTER*4 COMMENT                    ! Dummy comment
*    Internal References :
*    Local data :
*-

*    Return if entry status bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialisation of DSA_ routines
      CALL DSA_OPEN (STATUS)

*    Get the name of the integration file whose reduction is to be
*    removed
      CALL PAR_GET0C ('INTFILE', INTFILE, STATUS)

*    Construct the name of the reduced observation and reduced integration
*    files. e.g. original integration file is I890816_1_1
*                reduced integration file is RI890816_1_1
*                observation file is          O890816_1
*                reduced observation file is RO890816_1
      CALL RED4_INTTOROBS( INTFILE, OBSREDFILE, STATUS )
      CALL RED4_INTTORINT( INTFILE, INTREDFILE, STATUS )

*    Open reduced observation and integration files
      CALL DSA_NAMED_INPUT ('OBSRED', OBSREDFILE, STATUS)
      CALL DSA_NAMED_INPUT ('INTRED', INTREDFILE, STATUS)

*    Determine the observation type and issue a message.
      CALL DSA_GET_FITS_C( 'OBSRED', 'OBSTYPE', 0, OBS_TYPE, COMMENT,
     :  STATUS )

      CALL MSG_SETC( 'OBS_TYPE', OBS_TYPE )
      CALL MSG_SETC( 'INTREDFILE', INTREDFILE )
      CALL MSG_SETC( 'OBSREDFILE', OBSREDFILE )
      CALL MSG_OUT( ' ', 'Removing ^OBS_TYPE integration ^INTREDFILE '/
     :  /'from ^OBSREDFILE', STATUS )

*    Open the COADDS structure holding info on how many coadds have gone into
*    the result array, map in its data array, and get the DTA name of the
*    associated .MORE.CGS4_COADDS structure that will tell us which
*    integrations have already been 'added'
      CALL DSA_NAMED_INPUT ('COADDS', OBSREDFILE(:ICH_LEN(OBSREDFILE))//
     :   '.MORE.CGS4_COADDS', STATUS)

      CALL DSA_MAP_DATA ('COADDS', 'UPDATE', 'SHORT', ADDRESS,
     :   COADDS_SLOT, STATUS)
      COADDS_PTR = ADDRESS

      CALL DSA_SPECIFIC_STRUCTURE ('COADDS', 'COADDED_INTS',
     :   'UPDATE', COADDED_INTS, STATUS)

*    Search this structure for an occurence of the integration being
*    removed. If not present, the user is warned and the routine aborted.
      FOUND = .FALSE.

      IF (STATUS .EQ. SAI__OK) THEN

         N = 1
         DTA_STATUS = DTA__OK

*      Convert the integration name into its corresponding coadd
*      structure name
         CALL RED4_INTTOCOADD( INTFILE, COADD_TEST, STATUS )

         DO WHILE ( DTA_STATUS .EQ. DTA__OK )

*         Obtain the name of the next structure in the COADDED_INTS structure
            CALL DTA_NMVAR (COADDED_INTS, N, COADD_NAME, DTA_STATUS)

            IF ( DTA_STATUS .EQ. DTA__OK ) THEN

               IF ( COADD_NAME .EQ. COADD_TEST ) THEN

                  FOUND = .TRUE.
               ENDIF
            ENDIF

            N = N + 1
         END DO
      ENDIF

      DTA_STATUS = DTA__OK

      IF (.NOT. FOUND) THEN

         IF (STATUS .EQ. SAI__OK) THEN

            CALL MSG_OUT( ' ',
     :        'This integration is not in the coadd', STATUS )
            GOTO 500
         ENDIF
      ENDIF

*    OK, both integration and observation files exist and the integration
*    has been coadded -
*    Get the detector position index of this particular integration
      CALL DSA_GET_FITS_I( 'INTRED', 'DINDEX', 0, DET_INDEX, COMMENT,
     :  STATUS )

*   Abort if an error has occurred at this point. (This is done for
*   consistency with the old code. GOTOs should be removed eventually).
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS  = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REMOVE_INTEGRATION: '/
     :     /'First error in DSA routines', STATUS )
         GOTO 500
      END IF

*    If the detector index is 1, read in the INTEGRATION_TIMe of the
*    reduced integration and the same from the reduced observation file
*    The INTEGRATION_TIM object in the reduced observation file will
*    give the total integration time of the coadded points at detector
*    position 1.
      IF (DET_INDEX .EQ. 1) THEN

*       integration time for reduced integration
         CALL DSA_GET_FITS_F( 'INTRED', 'EXPOSED', 0, INTEGRATION_TIM,
     :     COMMENT, STATUS )

*       integration time for reduced observation:det_index 1
         CALL DSA_GET_FITS_F( 'OBSRED', 'EXPOSED', 0, OBSERVATION_TIM,
     :     COMMENT, STATUS )
      ENDIF

*   Abort if an error has occurred at this point. (This is done for
*   consistency with the old code. GOTOs should be removed eventually).
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS  = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REMOVE_INTEGRATION: '/
     :     /'Second error in DSA routines', STATUS )
         GOTO 500
      END IF

*    Map in the index array
      CALL DSA_NAMED_INPUT ('INDEX', OBSREDFILE(:ICH_LEN(OBSREDFILE))//
     :   '.MORE.CGS4_INDEX', STATUS)
      CALL DSA_MAP_DATA ('INDEX', 'READ', 'SHORT', ADDRESS, INDEX_SLOT,
     :   STATUS)
      INDEX_PTR = ADDRESS

*    check that the index array can handle this index position (always
*    should do but the program will crash*?! if it doesn't)
      CALL DSA_DATA_SIZE ('INDEX', MAXINDDIM, NDIM, INDEX_DIMS,
     :  NELM_IND, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN

         IF ((INDEX_DIMS(2) .LT. DET_INDEX) .OR.
     :       (DET_INDEX .LT.1 ))THEN

            CALL MSG_OUT( ' ', 'Detector position is outside range '/
     :        /'of the index array for this observation', STATUS )
            GOTO 500
         ENDIF
      ENDIF

*    Preparations are complete, ready to process the data.........
*    Use quality arrays, and map in the relevant data, quality, variances etc.,
*    for the reduced observation
      CALL DSA_USE_QUALITY ('OBSRED', STATUS)
      CALL DSA_MAP_DATA ('OBSRED', 'UPDATE', 'FLOAT', ADDRESS,
     :   OBSDATA_SLOT, STATUS)
      OBSDATA_PTR = ADDRESS
      CALL DSA_MAP_VARIANCE ('OBSRED', 'UPDATE', 'FLOAT', ADDRESS,
     :   OBSVAR_SLOT, STATUS)
      OBSVAR_PTR = ADDRESS

      IF ( STATUS .EQ. SAI__OK ) THEN

         VARIANCE_MAP = .TRUE.
      ELSE

         VARIANCE_MAP = .FALSE.
      END IF

      CALL DSA_MAP_QUALITY ('OBSRED', 'UPDATE', 'BYTE', ADDRESS,
     :   OBSQUAL_SLOT, STATUS)
      OBSQUAL_PTR = ADDRESS
      CALL DSA_DATA_SIZE ('OBSRED', MAXOBSDIM, NDIM, OBSDIMS, NELM_OBS,
     :  STATUS)

*    and for the reduced integration (errors derived from CIRACS coadding
*    statistics are ignored)
      CALL DSA_USE_QUALITY ('INTRED', STATUS)
      CALL DSA_MAP_DATA ('INTRED', 'READ', 'FLOAT', ADDRESS,
     :   INTDATA_SLOT, STATUS)
      INTDATA_PTR = ADDRESS
      CALL DSA_MAP_QUALITY ('INTRED', 'READ', 'BYTE', ADDRESS,
     :   INTQUAL_SLOT, STATUS)
      INTQUAL_PTR = ADDRESS
      CALL DSA_DATA_SIZE ('INTRED', MAXINTDIM, NDIM, INTDIMS, NELM_INT,
     :  STATUS)

*    Call the routine that does the processing, the check for STATUS stops
*    and adjustable array error if the routine is entered with some arrays
*    not successfully mapped
      IF (STATUS .EQ. SAI__OK) THEN

         CALL RED4_DO_REMOVE (%val(INTDATA_PTR),
     :                        %val(INTQUAL_PTR),
     :                        INTDIMS(1),
     :                        INTDIMS(2),
     :                        %val(INDEX_PTR),
     :                        INDEX_DIMS(1),
     :                        INDEX_DIMS(2),
     :                        DET_INDEX,
     :                        %val(OBSDATA_PTR),
     :                        %val(OBSVAR_PTR),
     :                        %val(OBSQUAL_PTR),
     :                        %val(COADDS_PTR),
     :                        OBSDIMS(1),
     :                        OBSDIMS(2),
     :                        STATUS)
      ENDIF

*    Check that the variance are non-negative, they'll crash DSA close
*    if they are. (The check is performed even if the status is bad).
      IF ( VARIANCE_MAP .AND. (NELM_OBS .GT. 0) ) THEN

         CALL GEN_CLIPF (%val(OBSVAR_PTR),
     :      NELM_OBS, 0.0, VAL__MAXR, IGNORE, IGNORE,
     :      %val(OBSVAR_PTR))
      ENDIF

*    If all OK, delete structure that said the integration had been
*    coadded
      CALL RED4_DELETE_STRUCTURE(
     :      COADDED_INTS(:ICH_LEN(COADDED_INTS))//'.'//COADD_TEST,
     :      STATUS )

*    If all OK and the detector index of the added integration is 1, then
*    subtract the integration's integration time from that of the observation,
*    and update the value in the observation reduction file
      IF ((STATUS .EQ. SAI__OK) .AND. (DET_INDEX .EQ. 1)) THEN

         OBSERVATION_TIM = OBSERVATION_TIM - INTEGRATION_TIM

         CALL DSA_PUT_FITS_F( 'OBSRED', 'EXPOSED', OBSERVATION_TIM,
     :     ' ', STATUS )
         CALL DSA_SET_EXPOSURE( 'OBSRED', OBSERVATION_TIM, STATUS )
      ENDIF

*    Finally, if all OK update the date of last access to the reduced
*    observation
      IF (STATUS .EQ. SAI__OK) THEN

         CALL GEN_TIME (6, DAY, LDAY, DATE, LDATE, HOUR, LHOUR)
         TIME = DATE(:LDATE)//' at '//HOUR(:LHOUR)
         CLEN = MAX( 1, ICH_LEN( TIME ) )

         CALL DSA_PUT_FITS_C( 'OBSRED', 'STREDUCE', TIME(1:CLEN),
     :     ' ', STATUS )
      ENDIF

 500  CONTINUE

      CALL DSA_CLOSE (STATUS)

      END
