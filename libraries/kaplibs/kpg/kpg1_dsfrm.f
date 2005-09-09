      SUBROUTINE KPG1_DSFRM( FSET, TEXT, FULL, STATUS )
*+
*  Name:
*     KPG1_DSFRM

*  Purpose:
*     Display a textual description of the Current Frame in a FrameSet.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_DSFRM( FSET, TEXT, FULL, STATUS )

*  Description:
*     This routine displays a textual description of the Current Frame 
*     in the supplied AST FrameSet.

*  Arguments:
*     FSET = INTEGER (Given)
*        An AST pointer to the FrameSet.
*     TEXT = CHARACTER * ( * ) (Given)
*        Text to display before the Frame description. May contain MSG
*        tokens.
*     FULL = LOGICAL (Given)
*        Display full information?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-FEB-1998 (DSB):
*        Original version.
*     25-AUG-1999 (DSB):
*        Allow MSG tokens in TEXT.
*     20-MAR-2000 (DSB):
*        Normalize first pixel centre before display.
*     10-JAN-2003 (DSB):
*        Modified to display details of WCS SpecFrames.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER FSET
      CHARACTER TEXT*(*)
      LOGICAL FULL

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables :
      CHARACTER ATTRIB*20        ! AST Frame attribute name
      CHARACTER STEXT*50         ! Sub-frame header text
      CHARACTER UNIT*15          ! Units string
      DOUBLE PRECISION CFIRST( 1, NDF__MXDIM ) ! Frame coords of first pixel
      DOUBLE PRECISION GFIRST( 1, NDF__MXDIM ) ! GRID coords of first pixel
      INTEGER CFRM               ! Frame to be described
      INTEGER COUNT              ! Count of displayed sub-frames
      INTEGER FRMNAX             ! Frame dimensionality
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IAT                ! Current length of a string
      INTEGER IAXIS              ! Loop counter for axes
      INTEGER IBASE              ! Index of Base Frame in FrameSet
      INTEGER IGRID              ! Index of GRID Frame in FrameSet
      INTEGER NDIM               ! Number of dimensions
      LOGICAL GOTFS              ! Was a FrameSet supplied?
      LOGICAL SERIES             ! Frames in series?
      INTEGER FRM( 2*NDF__MXDIM )! Pointers to component Frames
      INTEGER TOP                ! Index of last Frame to be checked
      INTEGER INV1               ! Invert attribute for first component
      INTEGER INV2               ! Invert attribute for second component
*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get a pointer to the Frame to be described.
      GOTFS = AST_ISAFRAMESET( FSET, STATUS ) 

      IF( GOTFS ) THEN
         CFRM = AST_GETFRAME( FSET, AST__CURRENT, STATUS )

      ELSE IF( AST_ISAFRAME( FSET, STATUS ) ) THEN
         CFRM = AST_CLONE( FSET, STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'CLASS', AST_GETC( FSET, 'CLASS', STATUS ) )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_DSFRM_ERR', 'KPG1_DSFRM: Inappropriate '//
     :                 'AST Object (class ^CLASS) supplied '//
     :                 '(programming error).', STATUS )
      END IF      

*  Display the global properties of the Frame.
      CALL KPG1_DSFR1( CFRM, TEXT, 8, FULL, STATUS )

*  Get the Frame dimensionality.
      FRMNAX = AST_GETI( CFRM, 'NAXES', STATUS )

*  Only proceed if we have a FrameSet.
      IF( GOTFS ) THEN

*  See if there is a GRID Frame in the FrameSet. 
         CALL KPG1_ASFFR( FSET, 'GRID', IGRID, STATUS )
         IF( IGRID .NE. AST__NOFRAME ) THEN

*  Ensure the GRID Frame is the Base Frame.
            IBASE = AST_GETI( FSET, 'BASE', STATUS )
            CALL AST_SETI( FSET, 'BASE', IGRID, STATUS )

*  Gets its dimensionality. Pass on if it is more than NDF__MXDIM.
            NDIM = AST_GETI( FSET, 'NIN', STATUS )
            IF( NDIM .LE. NDF__MXDIM ) THEN

*  Store the GRID coordinates of the centre of the first pixel. This is
*  defined to be (1.0,1.0,...). This position will be mapped into the
*  other Frame, to find the coordinates of the first pixel.
               DO IAXIS = 1, NDIM
                  GFIRST( 1, IAXIS ) = 1.0
               END DO

*  Map the GRID coordinates at the centre of the first pixel to obtain the 
*  corresponding coordinates in the Frame. 
               CALL AST_TRANN( FSET, 1, NDIM, 1, GFIRST, .TRUE., FRMNAX,
     :                         1, CFIRST, STATUS )

*  Normalize the positions.
               CALL AST_NORM( FSET, CFIRST, STATUS )

*  Display the resulting coordinates.
               CALL MSG_SETC( 'FIRST', AST_FORMAT( FSET, 1, 
     :                                             CFIRST( 1, 1 ), 
     :                                             STATUS ) )

               DO IAXIS = 2, FRMNAX
                  CALL MSG_SETC( 'FIRST', ',' )
                  CALL MSG_SETC( 'FIRST', ' ' )
                  CALL MSG_SETC( 'FIRST', AST_FORMAT( FSET, IAXIS, 
     :                            CFIRST( 1, IAXIS ), STATUS ) )
               END DO

               CALL MSG_OUT( 'WCS_FIRSTP',
     :      '        First pixel centre  : ^FIRST', STATUS )
            END IF

*  Re-instate the original Base Frame.
            CALL AST_SETI( FSET, 'BASE', IBASE, STATUS )

         END IF

      END IF

*  Now display the axis number, label and units for each axis of the Frame.
      CALL MSG_BLANK( STATUS )
      DO IAXIS = 1, FRMNAX

*  Display the axis number.
         CALL MSG_SETI( 'IAXIS', IAXIS )
         CALL MSG_OUT( 'AXIS_NUMBER',
     :   '           Axis ^IAXIS:', STATUS )

*  Construct the name of the attribute holding the label for this axis.
         ATTRIB = 'LABEL('
         IAT = 6
         CALL CHR_PUTI( IAXIS, ATTRIB, IAT )
         CALL CHR_APPND( ')', ATTRIB, IAT )

*  Get the label and display it.
         CALL MSG_SETC( 'LABEL', AST_GETC( CFRM, ATTRIB( : IAT ), 
     :                                     STATUS ) )
         CALL MSG_OUT( 'AXIS_LABEL',
     :   '              Label: ^LABEL', STATUS )

*  Construct the name of the attribute holding the units for this axis.
         ATTRIB = 'UNIT('
         IAT = 5
         CALL CHR_PUTI( IAXIS, ATTRIB, IAT )
         CALL CHR_APPND( ')', ATTRIB, IAT )

*  Get the units string and display it (if not blank).
         UNIT = AST_GETC( CFRM, ATTRIB( : IAT ), STATUS )
         IF( UNIT .NE. ' ' ) THEN
            CALL MSG_SETC( 'UNIT', UNIT )
            CALL MSG_OUT( 'AXIS_UNITS',
     :      '              Units: ^UNIT', STATUS )
         END IF

*  Add a spacing line after the information for each axis.
         CALL MSG_BLANK( STATUS )

      END DO

*  If the current Frame is a CmpFrame, we now display details of its
*  component Frames.
      IF( FULL .AND. AST_ISACMPFRAME( CFRM, STATUS ) ) THEN
         FRM( 1 ) = CFRM
         I = 1
         TOP = 1
         COUNT = 1
         DO WHILE( I .LE. TOP .AND. TOP .LE. NDF__MXDIM ) 
            IF( AST_ISACMPFRAME( FRM( I ), STATUS ) ) THEN
               CALL AST_DECOMPOSE( FRM( I ), FRM( TOP + 1 ), 
     :                             FRM( TOP + 2 ), SERIES, INV1, INV2, 
     :                             STATUS )
               TOP = TOP + 2
               CALL AST_ANNUL( FRM( I ), STATUS )
            ELSE
               STEXT = ' '
               IAT = 8
               CALL CHR_APPND( 'Sub-frame', STEXT, IAT )
               IAT = IAT + 1
               CALL CHR_PUTI( COUNT, STEXT, IAT )
               CALL CHR_PUTC( ':', STEXT, IAT )

               CALL KPG1_DSFR1( FRM( I ), STEXT( : IAT ), 11, FULL, 
     :                          STATUS )
               COUNT = COUNT + 1
               CALL MSG_BLANK( STATUS )
            END IF
            I = I + 1
         END DO

      END IF


*  End the AST context.
      CALL AST_END( STATUS )

      END
