      SUBROUTINE KPG1_GDPUT( IPIC, WDOM, DDOM, IPLOT, STATUS ) 
*+ 
* Name: 
*     KPG1_GDPUT

*  Purpose:
*     Save an AST Plot with a graphics database picture.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GDPUT( IPIC, WDOM, DDOM, IPLOT, STATUS )

*  Description:
*     This routine saves the supplied AST Plot (see SUN/210) in the AGI
*     database (see SUN/48) within the MORE structure of the specified 
*     picture. It can be retrieved if necessary using KPG1_GDGET (see the 
*     prologue of KPG1_GDGET for more information about using these two 
*     routines). 
*
*     If the supplied Plot contains a "AGI Data" Frame with the
*     Domain given by DDOM in which the axes are scaled and shifted
*     versions of the axes of the AGI world co-ordinate Frame
*     (specified by argument WDOM), then a TRANSFORM structure defining 
*     AGI Data co-ordinates is stored with the DATA picture. This is purely 
*     for the benefit of non-AST based applications which may use AGI Data 
*     co-ordinates (AST-based applications should always use the Plot 
*     stored with the picture in preference to the TRANSFORM structure 
*     stored in the AGI database).

*  Arguments:
*     IPIC = INTEGER (Given)
*        The AGI identifier for the picture. A value of -1 causes the
*        Plot to be stored with the current picture.
*     WDOM = CHARACTER * ( * ) (Given)
*        Domain name of the co-ordinate Frame within IPLOT corresponding 
*        to AGI world co-ordinates. "AGI_WORLD" is used if a blank value
*        is supplied.
*     DDOM = CHARACTER * ( * ) (Given)
*        Domain name of the co-ordinate Frame within IPLOT corresponding 
*        to AGI data co-ordinates. "AXIS" is used if a blank value
*        is supplied.
*     IPLOT = INTEGER (Given)
*        An AST pointer to the Plot to be stored with the picture. If
*        AST__NULL is supplied, any existing Plot stored with the picture is
*        deleted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The Base (GRAPHICS) Frame in the Plot should represent millimetres
*     from the bottom left corner of the view surface.
*     -  An error is reported if the Plot contains any Frames which have the 
*     Domain AGI_DATA.
*     -  The PGPLOT interface to the AGI library should be opened before
*     calling this routine.  
*     -  The Plot is stored in a component of the MORE structure named 
*     "AST_PLOT" and with type "WCS".

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-MAR-1998 (DSB):
*        Original version.
*     4-DEC-1998 (DSB):
*        Added facilities for storing a TRANFORM structure with the DATA
*        picture for the benefit of non-AST applications which require 
*        access to AGI Data co-ordinates.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Arguments Given:
      INTEGER IPIC
      CHARACTER WDOM*(*)
      CHARACTER DDOM*(*)
      INTEGER IPLOT

*  Status:
      INTEGER STATUS               ! Global status

*  Local Variables:
      CHARACTER DTOW( 2 )*130      ! Data->World transformation expressions
      CHARACTER MODE*6             ! Access mode for picture's MORE structure
      CHARACTER MORLOC*(DAT__SZLOC)! HDS locator for picture's MORE structure
      CHARACTER WTOD( 2 )*130      ! World->Data transformation expressions
      DOUBLE PRECISION C           ! Offset constant
      DOUBLE PRECISION M           ! Scale constant
      DOUBLE PRECISION XD( 2 )     ! X bounds of picture in AGI Data coords
      DOUBLE PRECISION XW( 2 )     ! X bounds of picture in AGI world coords
      DOUBLE PRECISION YD( 2 )     ! Y bounds of picture in AGI Data coords
      DOUBLE PRECISION YW( 2 )     ! Y bounds of picture in AGI world coords
      INTEGER FRM                  ! Pointer to next Frame
      INTEGER I                    ! Frame index
      INTEGER IDATA                ! Index of AXIS Frame in IPLOT
      INTEGER IFRM                 ! Index of AGI_DATA Frame
      INTEGER IPIC0                ! Original current AGI picture identifier 
      INTEGER IPICL                ! AGI identifier for picture to use
      INTEGER IWORLD               ! Index of AGI world co-ord Frame in IPLOT
      INTEGER MAP                  ! AST pointer to WORLD->DATA Mapping
      INTEGER NSUBS                ! No. of token substitutions made
      LOGICAL MORE                 ! Does picture have a MORE structure?
      LOGICAL THERE                ! Does MORE have an AST_PLOT component?
      REAL WX1, WX2, WY1, WY2      ! Bounds of picture in AGI world coords

*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that an AST Plot has been supplied.
      IF( IPLOT .NE. AST__NULL ) THEN 
         IF( .NOT. AST_ISAPLOT( IPLOT, STATUS ) ) THEN
            IF( STATUS .EQ. SAI__OK ) THEN
               CALL MSG_SETC( 'CLASS', AST_GETC( IPLOT, 'CLASS', 
     :                                           STATUS ) )
               STATUS = SAI__ERROR
               CALL ERR_REP( 'KPG1_GDPUT_1', 'KPG1_GDPUT: Programming'//
     :                    ' error - an AST ^CLASS has been supplied '//
     :                    'when a Plot was needed.', STATUS )
               GO TO 999
            END IF
         END IF
      END IF

*  Get the identifier for the picture to use.
      IF( IPIC .EQ. -1 ) THEN
         CALL AGI_ICURP( IPICL, STATUS )
      ELSE
         IPICL = IPIC
      END IF

*  Set the access mode required for the MORE structure, depending on
*  whether or not the picture already has a more structure.
      CALL AGI_IMORE( IPICL, MORE, STATUS ) 
      IF( MORE ) THEN
         MODE = 'UPDATE'
      ELSE
         MODE = 'WRITE'
      END IF

*  Get an HDS locator to the MORE structure.
      CALL AGI_MORE( IPICL, MODE, MORLOC, STATUS ) 

*  If supplied, see if the Plot contains any AGI_DATA frames. Component
*  Frames within CmpFrames are also checked.
      IF( IPLOT .NE. AST__NULL ) THEN
         CALL KPG1_ASFFR( IPLOT, 'AGI_DATA', IFRM, STATUS )

*  Report an error if an AGI_DATA Frame was found.
         IF( IFRM .NE. AST__NOFRAME .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPG1_GDPUT_2', 'KPG1_GDPUT: An attempt has'//
     :                    ' been made to store an AST Plot containing'//
     :                    ' an AGI_DATA Frame in the graphics '//
     :                    'database (programming error).', STATUS )

*  Otherwise, save the Plot in the database.
         ELSE
            CALL KPG1_WWRT( IPLOT, 'AST_PLOT', MORLOC, STATUS )
         END IF

*  If no Plot was supplied, delete the AST_PLOT component within MORE if it 
*  exists.
      ELSE
         CALL DAT_THERE( MORLOC, 'AST_PLOT', THERE, STATUS )
         IF( THERE ) CALL DAT_ERASE( MORLOC, 'AST_PLOT', STATUS )
      END IF

*  Find the index of the AXIS Frame (if any) in the Plot.
      IF( DDOM .NE. ' ' ) THEN
         CALL KPG1_ASFFR( IPLOT, DDOM, IDATA, STATUS )
      ELSE
         CALL KPG1_ASFFR( IPLOT, 'AXIS', IDATA, STATUS )
      END IF

*  Find the index of the AGI world co-ordinate Frame in the Plot.
      IF( WDOM .NE. ' ' ) THEN
         CALL KPG1_ASFFR( IPLOT, WDOM, IWORLD, STATUS )
      ELSE
         CALL KPG1_ASFFR( IPLOT, 'AGI_WORLD', IWORLD, STATUS )
      END IF

*  If both Frames were found, get the mapping from World to Data, and
*  simplify it.
      IF( IWORLD .NE. AST__NOFRAME .AND. IDATA .NE. AST__NOFRAME ) THEN
         MAP = AST_SIMPLIFY( AST_GETMAPPING( IPLOT, IWORLD, IDATA, 
     :                                       STATUS ), STATUS )

*  If the simplified Mapping is linear with independant axes (a ZoomMap or a 
*  WinMap), we can store an equivalent TRANSFORM structure in the AGI database 
*  giving AGI "Data" co-ordinates. 
         IF( AST_ISAZOOMMAP( MAP, STATUS ) .OR.
     :       AST_ISAWINMAP( MAP, STATUS ) ) THEN

*  Get the AGI world co-odinate bounds of the requested picture.
            IF( IPIC .NE. - 1 ) THEN
               CALL AGI_ICURP( IPIC0, STATUS )
               CALL AGI_SELP( IPIC, STATUS )
            END IF
            CALL AGI_IWOCO( WX1, WX2, WY1, WY2, STATUS ) 
            IF( IPIC .NE. - 1 ) CALL AGI_SELP( IPIC0, STATUS )

*  Transform these into Data co-ordinates using the above Mapping.
            XW( 1 ) = DBLE( WX1 )
            XW( 2 ) = DBLE( WX2 )
            YW( 1 ) = DBLE( WY1 )
            YW( 2 ) = DBLE( WY2 )

            CALL AST_TRAN2( MAP, 2, XW, YW, .TRUE., XD, YD, STATUS ) 

*  Do not store a transformation if it would be singular.
            IF( XW( 2 ) .NE. XW( 1 ) .AND.
     :          YW( 2 ) .NE. YW( 1 ) .AND.
     :          XD( 2 ) .NE. XD( 1 ) .AND.
     :          YD( 2 ) .NE. YD( 1 ) ) THEN

*  Create a TRANSFORM structure describing the Mapping, and store with
*  the picture in the AGI database.
               WTOD( 1 ) = 'DX = M*WX + C'
               M = ( XD( 2 ) - XD( 1 ) )/( XW( 2 ) - XW( 1 ) )
               C = XD( 1 ) - XW( 1 )*M
               CALL TRN_STOKD( 'M', M, WTOD( 1 ), NSUBS, STATUS )
               CALL TRN_STOKD( 'C', C, WTOD( 1 ), NSUBS, STATUS )

               WTOD( 2 ) = 'DY = M*WY + C'
               M = ( YD( 2 ) - YD( 1 ) )/( YW( 2 ) - YW( 1 ) )
               C = YD( 1 ) - YW( 1 )*M
               CALL TRN_STOKD( 'M', M, WTOD( 2 ), NSUBS, STATUS )
               CALL TRN_STOKD( 'C', C, WTOD( 2 ), NSUBS, STATUS )

               DTOW( 1 ) = 'WX = M*DX + C'
               M = ( XW( 2 ) - XW( 1 ) )/( XD( 2 ) - XD( 1 ) )
               C = XW( 1 ) - XD( 1 )*M
               CALL TRN_STOKD( 'M', M, DTOW( 1 ), NSUBS, STATUS )
               CALL TRN_STOKD( 'C', C, DTOW( 1 ), NSUBS, STATUS )

               DTOW( 2 ) = 'WY = M*DY + C'
               M = ( YW( 2 ) - YW( 1 ) )/( YD( 2 ) - YD( 1 ) )
               C = YW( 1 ) - YD( 1 )*M
               CALL TRN_STOKD( 'M', M, DTOW( 2 ), NSUBS, STATUS )
               CALL TRN_STOKD( 'C', C, DTOW( 2 ), NSUBS, STATUS )

               CALL AGI_TNEW( 2, 2, DTOW, WTOD, IPIC, STATUS ) 

            ENDIF

         END IF

*  Annul the Mapping pointer.
         CALL AST_ANNUL( MAP, STATUS )

      END IF

 999  CONTINUE

*  Annul the locator to the MORE structure.
      CALL DAT_ANNUL( MORLOC, STATUS )

      END
