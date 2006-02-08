************************************************************************
*+  AGI_1FPIC - Find a picture in a picture structure

      SUBROUTINE AGI_1FPIC ( PSTLOC, PICNUM, PICLOC, FOUND, STATUS )

*    Description :
*     Find a picture in a picture stucture.
*
*    Invocation :
*     CALL AGI_1FPIC( PSTLOC, PICNUM, PICLOC, FOUND, STATUS )
*
*    Method :
*     Check status on entry.
*     Inquire the current number of pictures in the array of pictures.
*     If number less than given picture number
*     or picture number less than one then
*        Indicate picture not found.
*     Else
*        Get locator to picture cell.
*     Endif
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*     David Berry
*
*    History :
*     July 1988
*     Feb 2006: Initialise the returned value of FOUND before checking status
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*    Import :

*     Locator to picture structure
      CHARACTER * ( DAT__SZLOC ) PSTLOC

*     Requested picture number in array of pictures
      INTEGER PICNUM

*    Export :

*     Locator to picture. Undefined in .NOT. FOUND
      CHARACTER * ( DAT__SZLOC ) PICLOC

*     Flag to indicate if picture has been found
      LOGICAL FOUND

*    Status :
      INTEGER STATUS

*    Local variables :
      INTEGER SIZE
*-

      FOUND = .FALSE.
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Get the number of current cells
         CALL DAT_SIZE( PSTLOC, SIZE, STATUS )

*   If there are fewer pictures than picnum then one cannot be found
*   or if picnum is invalid
         PICLOC = ' '
         IF ( ( SIZE .LT. PICNUM ) .OR. ( PICNUM .LT. 1 ) ) THEN
            FOUND = .FALSE.
         ELSE
            CALL DAT_CELL( PSTLOC, 1, PICNUM, PICLOC, STATUS )
            FOUND = .TRUE.
         ENDIF

      ENDIF

*      print*, '+++++ AGI_1FPIC +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

