*+  RED4_DELETE_STRUCTURE - Delete a data structure
      SUBROUTINE RED4_DELETE_STRUCTURE( NAME, STATUS )
*    Description :
*     This routine deletes the data structure of DTA address NAME.
*
*     The routine simply encapsulates a call to DTA_DLVAR, and has
*     been produced to simplify the high-level code by removing the
*     frequently used low level DTA calls and checks.
*    Invocation :
*      CALL RED4_DELETE_STRUCTURE( NAME, STATUS )
*    Parameters :
*     NAME          = CHARACTER*(*)( READ )
*        The DTA address of the structure to be deleted.
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
*     18-Sep-1990: Original version.               (SMB)
*     19-Feb-1993: Conform to error strategy       (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Import :
      CHARACTER*(*)
     :  NAME               ! DTA address of structure
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

*   Initialise the DTA status
      DTA_STATUS = DTA__OK

*   Delete the specified structure.
      CALL DTA_DLVAR( NAME, DTA_STATUS )

*   If this has not worked, report an error.
      IF ( DTA_STATUS .NE. DTA__OK ) THEN

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( ' ', 'RED4_DELETE_STRUCTURE: '/
     :     /'Error deleting ^NAME (DTA reason follows)', STATUS )
         CALL DTA_ERROR( DTA_STATUS, ERROR )
         CALL MSG_SETC( 'ERROR', ERROR )
         CALL ERR_REP( ' ', 'RED4_DELETE_STRUCTURE: '/
     :     /'^ERROR', STATUS )
      END IF

      END
