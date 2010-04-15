*+  GETINP - get a locator to an IMAGE type structure for data input
      SUBROUTINE GETINP( PARNAM, LOCAT, STATUS )
*    Description :
*     Returns a locator, LOCAT, to an IMAGE type data structure associated with
*     the parameter name PARNAM. If the structure contains a TITLE component
*     this is written out to the user.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL GETINP( PARNAM, LOCAT, STATUS )
*    Parameters :
*     PARNAM = CHAR*(*)( READ )
*           Parameter name associated with the input IMAGE type structure.
*     LOCAT  = INTEGER ( WRITE )
*           Returns the locator to the object associated with the given
*           parameter name.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur. If an error occurs
*           during the execution of this routine STATUS will be returned
*           containing the appropriate error value.
*    Method :
*     If no error on entry then
*        Get a locator to the IMAGE type data structure associated with
*          parameter name given in PARNAM.
*        If the structure contains a TITLE component then
*           Read the TITLE component into TITLE and write it out to the user
*             via the message sytem.
*        Endif
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     01/12/1983 : Original version                (ROE::ASOC5)
*     17/02/1984 : Modified to use TITLE component (ROE::ASOC5)
*     10-MAR-94    DAT_ and CMP_ calls changed to NDF_ (SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*)
     :  PARNAM ! parameter name associated with the input data structure
*    Export :
      INTEGER
     :  LOCAT  ! locator to input data structure
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*72
     :  TITLE ! contents of the TITLE component if there is one
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       get a locator to the input image data structure associated with PARNAM
         CALL NDF_ASSOC( PARNAM, 'READ', LOCAT, STATUS )

         TITLE = ' '
         CALL NDF_CGET( LOCAT, 'TITLE', TITLE, STATUS )

*        write it out to the user
          CALL MSG_SETC( 'TITLE', TITLE )
          CALL MSG_OUT( 'INPUT_TITLE', 'Title = ^TITLE', STATUS )

      ENDIF

      END
