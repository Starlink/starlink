************************************************************************
*+  AGI_1PNEW - Save current picture parameters

      SUBROUTINE AGI_1PNEW ( WKNAME, PNAME, COMENT, DEVICE, NDC,
     :                       WORLD, MEMID, PICNUM, STATUS )

*    Description :
*     Save current picture parameters in the database and in the cache
*
*    Invocation :
*     CALL AGI_1PNEW( WKNAME, PNAME, COMENT, DEVICE, NDC, WORLD,
*    :                MEMID, PICNUM, STATUS )
*
*    Method :
*     Check status on entry.
*     Get a locator to the top level database structure.
*     Get a locator to the workstation.
*     Inquire how many pictures are on the workstation.
*     Make the picture number one greater than the number of pictures.
*     Create a new picture containing the passed parameters.
*     Indicate this is the active picture.
*     Write this picture into the cache.
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     Aug 1988
*     Jul 1989  Read database locator from common block
*     Nov 1989  PNAME - Remove leading blanks and change to upper case
*     Jun 1990  Added MEMID parameter
*     Sep 1990  Removed inquiries to the number of pictures
*     Jan 1993  Initialise header block if necessary
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'

*    Import :

*     Name of workstation
      CHARACTER * ( * ) WKNAME

*     Name of picture
      CHARACTER * ( * ) PNAME

*     Description of picture
      CHARACTER * ( * ) COMENT

*     Device coordinates of picture
      REAL DEVICE( 4 )

*     Normalised device coordinates of picture
      REAL NDC( 4 )

*     World coordinates of picture
      REAL WORLD( 4 )

*     Memory identifier
      INTEGER MEMID

*    Export :

*     Number of picture in array of pictures
      INTEGER PICNUM

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'agi_cache'

*    Local variables :
      INTEGER POINT

      CHARACTER * ( AGI__SZNAM ) LNAME
*-

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Use a local variable to manipulate the name string.
*   Remove leading blanks and change to upper case
         LNAME = PNAME
         CALL CHR_LDBLK( LNAME )
         CALL CHR_UCASE( LNAME )

*   Write the information into the database
         CALL AGI_1WPIC( WKNAME, LNAME, COMENT, DEVICE, NDC, WORLD,
     :                   MEMID, PICNUM, STATUS )

*   Indicate this is the active picture
         CALL AGI_1WPACT( WKNAME, PICNUM, STATUS )

*   Write the information into the cache
         CALL AGI_1WCACH( WKNAME, PICNUM, LNAME, COMENT, DEVICE, NDC,
     :                    WORLD, MEMID, POINT, STATUS )

*   Remember the number of pictures on this device
         IF ( CNUMPW .NE. WKNAME ) CHEAD = -1
         CNUMPW = WKNAME
         CNUMPS = PICNUM

      ENDIF

*      print*, '+++++ AGI_1PNEW +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

