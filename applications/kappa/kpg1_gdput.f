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
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER IPIC
      CHARACTER WDOM*(*)
      CHARACTER DDOM*(*)
      INTEGER IPLOT

*  Status:
      INTEGER STATUS               ! Global status

*  Local Constants:
      INTEGER NSAMP                ! No. of samples to test axis linearity
      PARAMETER ( NSAMP = 500 )

*  Local Variables:
      CHARACTER MODE*6             ! Access mode for picture's MORE structure
      CHARACTER MORLOC*(DAT__SZLOC)! HDS locator for picture's MORE structure
      DOUBLE PRECISION DX          ! X increment between axis samples
      DOUBLE PRECISION DY          ! Y increment between axis samples
      DOUBLE PRECISION OFFSET( 2 ) ! Offset constants
      DOUBLE PRECISION SCALE( 2 )  ! Scale constants
      DOUBLE PRECISION XD( NSAMP ) ! X samples in AGI Data coords
      DOUBLE PRECISION RMS         ! RMS error of fit
      DOUBLE PRECISION XW( NSAMP ) ! X samples in AGI world coords
      DOUBLE PRECISION YD( NSAMP ) ! Y samples in AGI Data coords
      DOUBLE PRECISION YW( NSAMP ) ! Y samples in AGI world coords
      INTEGER FRM                  ! Pointer to next Frame
      INTEGER I                    ! Frame index/loop counter
      INTEGER IDATA                ! Index of AXIS Frame in IPLOT
      INTEGER IFRM                 ! Index of AGI_DATA Frame
      INTEGER IPIC0                ! Original current AGI picture identifier 
      INTEGER IPICL                ! AGI identifier for picture to use
      INTEGER IWORLD               ! Index of AGI world co-ord Frame in IPLOT
      INTEGER MAP                  ! AST pointer to WORLD->DATA Mapping
      LOGICAL MORE                 ! Does picture have a MORE structure?
      LOGICAL THERE                ! Does MORE have an AST_PLOT component?
      LOGICAL XDLIN                ! X Data axis linear?
      LOGICAL YDLIN                ! Y Data axis linear?
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
      IF( IWORLD .NE. AST__NOFRAME .AND. IDATA .NE. AST__NOFRAME 
     :    .AND. STATUS .EQ. SAI__OK ) THEN
         MAP = AST_SIMPLIFY( AST_GETMAPPING( IPLOT, IWORLD, IDATA, 
     :                                       STATUS ), STATUS )

*  Cannot use the mapping unless it has 2 inputs and 2 outputs.
         IF( AST_GETI( MAP, 'NIN', STATUS ) .EQ. 2 .AND.
     :       AST_GETI( MAP, 'NOUT', STATUS ) .EQ. 2 .AND.
     :       STATUS .EQ. SAI__OK ) THEN

*  Ensure the requested AGI picture is current (saving the current
*  picture ID first).
            IF( IPIC .NE. - 1 ) THEN
               CALL AGI_ICURP( IPIC0, STATUS )
               CALL AGI_SELP( IPIC, STATUS )
            END IF

*  Get the AGI world co-odinate bounds of the requested picture.
            CALL AGI_IWOCO( WX1, WX2, WY1, WY2, STATUS ) 

*  Form a list of NSAMP points evenly spread in world co-ordinates along
*  each axis.
            DX = DBLE( WX2 - WX1 )/( NSAMP - 1 ) 
            DY = DBLE( WY2 - WY1 )/( NSAMP - 1 )
            DO I = 0, NSAMP - 1
               XW( I + 1 ) = DBLE( WX1 ) + DX*I
               YW( I + 1 ) = DBLE( WX1 ) + DX*I
            END DO

*  Transform these World Domain positions into the Data Domain.
            CALL AST_TRAN2( MAP, NSAMP, XW, YW, .TRUE., XD, YD, STATUS ) 

*  Attempt to fit a straight line through the XD and XW values.
            CALL KPG1_FIT1D( 1, NSAMP, XD, XW, SCALE( 1 ), OFFSET( 1 ), 
     :                       RMS, STATUS )

*  Consider the fit good if the RMS error is less than 0.1 of the increment
*  in XD between samples.
            XDLIN = ( RMS .LT. ABS( 0.1D0*SCALE( 1 )*DX ) )

*  Attempt to fit a straight line through the YD and YW values.
            CALL KPG1_FIT1D( 1, NSAMP, YD, YW, SCALE( 2 ), OFFSET( 2 ), 
     :                       RMS, STATUS )

*  Consider the fit good if the RMS error is less than 0.1 of the increment
*  in XD between samples.
            YDLIN = ( RMS .LT. ABS( 0.1D0*SCALE( 2 )*DY ) )

*  If succesful, and both are linear, and the two co-ordinate systems are 
*  not identical...
            IF( STATUS .EQ. SAI__OK .AND. XDLIN .AND. YDLIN .AND.
     :          ABS( SCALE( 1 ) - 1.0D0 ) .GE. VAL__EPSD .OR.
     :          ABS( OFFSET( 1 ) - 0.0D0 ) .GE. VAL__EPSD .OR.
     :          ABS( SCALE( 2 ) - 1.0D0 ) .GE. VAL__EPSD .OR.
     :          ABS( OFFSET( 2 ) - 0.0D0 ) .GE. VAL__EPSD ) THEN

*  Save a linear TRANSFORM structure with the current AGI picture.
               CALL KPG1_LITRD( SCALE, OFFSET, STATUS )

            END IF

*  If necessary, re-instate the original current AGI picture.
            IF( IPIC .NE. - 1 ) CALL AGI_SELP( IPIC0, STATUS )

         END IF

*  Annul the Mapping pointer.
         CALL AST_ANNUL( MAP, STATUS )

*  Annul any error since failure to store a Data TRANSFORM is not fatal.
         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

      END IF

 999  CONTINUE

*  Annul the locator to the MORE structure.
      CALL DAT_ANNUL( MORLOC, STATUS )

      END
