*+  RED4_COPY_OBS_STRUCTURE - Copy a data structure and delete unwanted items
      SUBROUTINE RED4_COPY_OBS_STRUCTURE( SOURCE, DESTINATION, STATUS )
*    Description :
*     This routine copies the data structure of DTA address SOURCE
*     to the structure DESTINATION, deleting DESTINATION first if
*     it already exists. Assuming that the structure copied was a FITS
*     structure describing an observation, the items that would not
*     be relevant in the COADDS structure of a reduced group file are
*     deleted. (These items are the ones which are unlikely to change
*     between the various observations).
*
*     The routine simply calls to DTA_CYVAR and DTA_DLVAR, and has
*     been produced to simplify the high-level code by removing the
*     frequently used low level DTA calls and checks.
*    Invocation :
*      CALL RED4_COPY_OBS_STRUCTURE( SOURCE, DESTINATION, STATUS )
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
*     At first it may seem silly to copy a whole structure and then delete
*     the unwanted items, but this is the easiest way of partially copying
*     a structure with DTA. If this strategy leads to inefficiency, the
*     routine could be modified to read, create and write each wanted item
*     instead. (N.B. DTA_CYVAR produced an "attempt to copy incompatible
*     objects" error when I tried to copy the wanted items individually).
*
*     DSA does not yet produce valid ADAM status values, so the symbol
*     DSA__DTAERR may be set to an illegal number.
*    Bugs :
*    Authors :
*     Steven Beard (REVAD::SMB)
*     Phil Daly  (JACH::PND)
*    History :
*      1-Oct-1990: Original version, based on RED4_COPY_STRUCTURE.        (SMB)
*     18-Feb-1993: Conform to error strategy                              (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Import :
      CHARACTER*(*)
     :  SOURCE,            ! DTA address of source structure
     :  DESTINATION        ! DTA address of destination structure
*    Status :
      INTEGER
     :  STATUS             ! Global status
*    External references :
      INTEGER
     :  CHR_LEN            ! Character length determining function
*    Global variables :
*    Local Constants :
      INTEGER DTA__OK                    ! DTA success status
      PARAMETER ( DTA__OK = 0 )
*    Local variables :
      CHARACTER*80
     :  OBJECT,            ! DTA data object
     :  ERROR              ! DTA error message
      INTEGER
     :  CLEN,              ! Non-blank length of character string
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
      CALL DTA_CYVAR( SOURCE, DESTINATION, DTA_STATUS )

*   Check this has worked.
      IF ( DTA_STATUS .EQ. DTA__OK ) THEN

*      Delete the items which are not required.
         CLEN = MAX( 1, CHR_LEN( DESTINATION ) )

         OBJECT = DESTINATION(1:CLEN) // '.INSTRUME'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.TELESCOP'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.SOFTWARE'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.OBSERVER'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.OBSREF'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.OBJECT'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.OBJCLASS'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.GRPNUM'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.GRATING'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.GLAMBDA'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.GANGLE'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.GORDER'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.GLPMM'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.SLIT'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.SANGLE'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.SLENGTH'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.SWIDTH'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.CVF'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.CLAMBDA'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.FILTERS'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.IRTANGLE'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.DETECTOR'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.DROWS'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.DCOLUMNS'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.DFOCUS'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.DENCBASE'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.DEXPTIME'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.DETINCR'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.DETNINCR'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.DETNINCR'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.DBIAS'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.DGATE'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.DRDTIME'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.DGAIN'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.DREF'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.LAMP'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )
         OBJECT = DESTINATION(1:CLEN) // '.BBTEMP'
         CALL DTA_DLVAR( OBJECT, DTA_STATUS )

*      Ignore the status if any of the deletions has failed, as not critical.
      ELSE

*      The copy has failed. Report an error.
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'SOURCE', SOURCE )
         CALL ERR_REP( ' ', 'RED4_COPY_OBS_STRUCTURE: '/
     :     /'Error copying from ^SOURCE', STATUS )
         CALL MSG_SETC( 'DESTINATION', DESTINATION )
         CALL ERR_REP( ' ', 'RED4_COPY_OBS_STRUCTURE: '/
     :     /'structure to ^DESTINATION (DTA reason follows)', STATUS )
         CALL DTA_ERROR( DTA_STATUS, ERROR )
         CALL MSG_SETC( 'ERROR', ERROR )
         CALL ERR_REP( ' ', 'RED4_COPY_OBS_STRUCTURE: '/
     :      /'^ERROR', STATUS )
      END IF

      END
