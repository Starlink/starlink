************************************************************************
*+  AGI_1RARPR - Read a real 4 element array

      SUBROUTINE AGI_1RARPR ( PICLOC, PTYPE, FOUND, PVAL, STATUS )

*    Description :
*     Read the contents of a real 4 element array
*
*    Invocation :
*     CALL AGI_1RARPR( PICLOC, PTYPE, FOUND, PVAL, STATUS )
*
*    Method :
*     Check status on entry.
*     If the given parameter is there then
*        Read the contents of the array.
*     Endif
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*    History :
*     July 1988
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*    Import :

*     Locator to picture
      CHARACTER * ( DAT__SZLOC ) PICLOC

*     Name of parameter to read
      CHARACTER * ( * ) PTYPE

*    Export :

*     Flag to indicate if parameter has been found
      LOGICAL FOUND

*     Array of values. Undefined if .NOT. FOUND
      REAL PVAL( 4 )

*    Status :
      INTEGER STATUS

*    Local variables :
      CHARACTER * ( DAT__SZLOC ) PARLOC
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check if the parameter is present
         PARLOC = ' '
         CALL AGI_1FPAR( PICLOC, PTYPE, PARLOC, FOUND, STATUS )

*   Read contents of element
         IF ( FOUND ) THEN
            CALL DAT_GETR( PARLOC, 1, 4, PVAL, STATUS )
            CALL DAT_ANNUL( PARLOC, STATUS )
            PARLOC = ' '
         ENDIF

      ENDIF

*      print*, '+++++ AGI_1RARPR +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

