*+  CRED4_OPEN_NB - Open the CRED4 noticeboard
      SUBROUTINE CRED4_OPEN_NB( STATUS )
*    Description :
*     This routine creates a memory-resident noticeboard and sets
*     values to from the ADAM parameter file.
*    Invocation :
*     CALL CRED4_OPEN_NB( VALUE, STATUS )
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*     21-Jun-1990: Original version.                            (SMB)
*     22-Jun-1990: Ability to select from a variety of stored
*                  noticeboards added.                          (SMB)
*     10-Sep-1990: The NBS save/restore mechanism was not
*                  designed for "restoring from a variety of
*                  different noticeboards". This routine will
*                  now only be used to restore the noticeboard
*                  once at the beginning of the data reduction
*                  session. Messages changed accordingly.       (SMB)
*     23-Oct-1990: Temporary debug statements inserted before
*                  each NBS call, to determine which NBS routine
*                  was creating the empty .ERR files.           (SMB)
*     23-Oct-1990: Temporary lines removed.
*                  NBS_RESTORE_NOTICEBOARD was the culprit.     (SMB)
*     30-Oct-1990: Bug fix: GRP_NEWSURFACE was not initialised. (SMB)
*      1-Nov-1990: Modified to initialise DISP_COUNTER.         (SMB)
*     22-Nov-1990: VERBOSE flag added.                          (SMB)
*     15-Jan-1993: Substantially modified to use a virtual NB   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ACT_ERR'
*    Status :
      INTEGER STATUS                  ! Global status
*    Global variables :
      INCLUDE 'CRED4COM.INC'          ! CRED4 common block
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Obtain the name of the noticeboard global section
      CALL PAR_GET0C( 'NOTICEBOARD', NOTICEBOARD, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'CRED4_OPEN_NB: '/
     :     /'Unable to obtain noticeboard global section name', STATUS )
      ELSE

        IF ( VERBOSE ) THEN
          CALL MSG_SETC( 'NB', NOTICEBOARD )
          CALL MSG_OUT( ' ', 'Noticeboard global section name = ^NB', STATUS )
        ENDIF

*      Define a noticeboard
        CALL CRED4_DEFINE_NB( STATUS )

*      Find the item identifiers
        CALL CRED4_FIND_NB( STATUS )

*      Initialise the flags in the noticeboard
        CALL CRED4_INIT_NBFLAGS( STATUS )

*      Restore common block from interface (parameter) file
        CALL CRED4_READ_PARAMETERS( STATUS )

*      Populate the noticeboard with common block values
        CALL CRED4_WRITE_NB( STATUS )

*      Is everything OK?
        IF ( STATUS .EQ. SAI__OK ) THEN
           SEQUENCE_SETUP = .TRUE.
           IF ( VERBOSE ) THEN
              CALL MSG_OUT( ' ', 'Noticeboard created OK', STATUS )
           ENDIF
        ENDIF
      ENDIF

      END
