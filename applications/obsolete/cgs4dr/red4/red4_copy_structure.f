*+  RED4_COPY_STRUCTURE - Copy a data structure
      SUBROUTINE RED4_COPY_STRUCTURE( SOURCE, DESTINATION, STATUS )
*    Description :
*     This routine copies the data structure of DTA address SOURCE
*     to the structure DESTINATION, deleting DESTINATION first if
*     it already exists.
*
*     The routine simply encapsulates a call to DTA_CYVAR, and has
*     been produced to simplify the high-level code by removing the
*     frequently used low level DTA calls and checks.
*    Invocation :
*      CALL RED4_COPY_STRUCTURE( SOURCE, DESTINATION, STATUS )
*    Parameters :
*     SOURCE        = CHARACTER*(*)( READ )
*        The DTA address of the structure to be copied.
*     DESTINATION   = CHARACTER*(*)( READ )
*        The DTA address of the destination structure. This will be
*        deleted if it already exists.
*     STATUS        = INTEGER( UPDATE )
*           Global ADAM status
*    Method :
*    Deficiencies :
*     DSA does not yet produce valid ADAM status values, so the symbol
*     DSA__DTAERR may be set to an illegal number.
*    Bugs :
*    Authors :
*     Steven Beard (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*     14-Sep-1990: Original version.               (SMB)
*     14-Aug-1992: Corrected error reporting.      (PND)
*      8-Sep-1992: Add further error reporting.    (PND)
*     18-Feb-1993: Conform to error strategy       (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
      INCLUDE 'RED4_COMMON.INC'
*    Import :
      CHARACTER*(*)
     :  SOURCE,            ! DTA address of source structure
     :  DESTINATION        ! DTA address of destination structure
*    Status :
      INTEGER
     :  STATUS             ! Global status
*    External references :
*    Global variables :
*    Local Constants :
      INTEGER DTA__OK                    ! DTA success status
      PARAMETER ( DTA__OK = 0 )
*    Local variables :
      CHARACTER*80
     :  ERROR              ! DTA error message
      INTEGER
     :  DTA_STATUS         ! DTA status
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Attempt to delete the destination structure, to ensure it does
*   not exist. Ignore the status returned.
      DTA_STATUS = DTA__OK
      CALL DTA_DLVAR( DESTINATION, DTA_STATUS )
      DTA_STATUS = DTA__OK

*   Use DTA_CYVAR to copy the structure specified.
      IF ( VERBOSE ) THEN
         CALL MSG_SETC( 'SOURCE', SOURCE )
         CALL MSG_SETC( 'DESTINATION', DESTINATION )
         CALL MSG_OUT( ' ',
     :     'Attempting to copy ^SOURCE to ^DESTINATION OK', STATUS )
      END IF
      CALL DTA_CYVAR( SOURCE, DESTINATION, DTA_STATUS )

*   If this has not worked, report an error.
      IF ( DTA_STATUS .NE. DTA__OK ) THEN

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'SOURCE', SOURCE )
         CALL ERR_REP( ' ', 'RED4_COPY_STRUCTURE: '/
     :     /'Error copying ^SOURCE', STATUS )
         CALL MSG_SETC( 'DESTINATION', DESTINATION )
         CALL ERR_REP( ' ', 'RED4_COPY_STRUCTURE: '/
     :     /'to ^DESTINATION', STATUS )
         CALL MSG_SETI( 'DTA_STATUS', DTA_STATUS )
         CALL ERR_REP( ' ', 'RED4_COPY_STRUCTURE: '/
     :     /'DTA status = ^DTA_STATUS (DTA reason follows)', STATUS )
         CALL DTA_ERROR( DTA_STATUS, ERROR )
         CALL MSG_SETC( 'ERROR', ERROR )
         CALL ERR_REP( ' ', 'RED4_COPY_STRUCTURE: '/
     :     /'^ERROR', STATUS )
      ELSE
        IF ( VERBOSE ) THEN
           CALL MSG_SETC( 'SOURCE', SOURCE )
           CALL MSG_SETC( 'DESTINATION', DESTINATION )
           CALL MSG_OUT( ' ',
     :       'Copied ^SOURCE to ^DESTINATION OK', STATUS )
        END IF
      END IF

      END
