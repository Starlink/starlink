      SUBROUTINE KPG1_ASFGT( PDOM, PDIM, PEP, FRM, NAX, STATUS )
*+
*  Name:
*     KPG1_ASFGT

*  Purpose:
*     Create a new Frame with a Domain specified through the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASFGT( PDOM, PDIM, PEP, FRM, NAX, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine creates a new AST Frame with a Domain specified through
*     the environment. If the string obtained for the parameter looks
*     like an IRAS90 "Sky Co-ordinate System" (SCS) specification, then
*     a SkyFrame is returned with the properties specified by the SCS.
*     Otherwise, a simple Frame is returned with the specified Domain, 
*     the number of axes in the Frame being specified by another environment 
*     parameter.

*  Arguments:
*     PDOM = CHARACTER * ( * ) (Given)
*        Name of parameter to use to get Frame Domain.
*     PDIM = CHARACTER * ( * ) (Given)
*        Name of parameter to use to get number of Frame axes. Only
*        accessed if the value obtained for PDOM is not an IRAS90 SCS.
*     PEP = CHARACTER * ( * ) (Given)
*        Name of parameter to use to get the epoch of observation. Only
*        accessed if the value obtained for PDOM is an IRAS90 SCS.
*     FRM = INTEGER (Returned)
*        An AST pointer to the returned Frame. Returned equal to
*        AST__NULL if an error occurs.
*     NAX = INTEGER (Returned)
*        The number of axes in the returned Frame. Returned equal to zero
*        if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-SEP-1998 (DSB):
*        Original version.
*     16-DEC-1998 (DSB):
*        Added BASEPIC as a special case Domain.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      CHARACTER PDOM*(*)
      CHARACTER PDIM*(*)
      CHARACTER PEP*(*)

*  Arguments Returned:
      INTEGER FRM
      INTEGER NAX

*  Status:
      INTEGER STATUS

*  External References:
      LOGICAL KPG1_ASSIR         ! Is string an IRAS90 SCS?
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER DOM*30           ! Co-ordinate Frame specification
      CHARACTER TEXT*60          ! Attribute value or name
      INTEGER I                  ! Axis count
      INTEGER IAT                ! No. of characters in a string
*.

*  Initialise
      FRM = AST__NULL
      NAX = 0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the string describing the required co-ordinate Frame.
      CALL PAR_GET0C( PDOM, DOM, STATUS )

*  Convert to upper case, and remove blanks.
      CALL CHR_UCASE( DOM )
      CALL CHR_RMBLK( DOM )

*  Create a default SkyFrame.
      FRM = AST_SKYFRAME( ' ', STATUS )
      NAX = 2

*  Is the supplied string "SKY" or an IRAS90 SCS? If so, the SkyFrame 
*  properties are changed to match the SCS. If not, we need to return a 
*  simple Frame instead of a SkyFrame.
      IF( DOM .NE. 'SKY' .AND. .NOT. KPG1_ASSIR( FRM, DOM, PEP, 
     :                                          STATUS ) ) THEN

*  Annul the SkyFrame.
         CALL AST_ANNUL( FRM, STATUS )

*  Get the number of axes for the Frame.
         IF( DOM .EQ. 'GRAPHICS' .OR. DOM .EQ. 'BASEPIC' ) THEN
            NAX = 2
         ELSE
            CALL PAR_GDR0I( PDIM, 2, 1, NDF__MXDIM, .FALSE., NAX, 
     :                      STATUS )
         END IF

*  Create a Frame with this many axes.
         FRM = AST_FRAME( NAX, ' ', STATUS )

*  Give it the supplied Domain.
         CALL AST_SETC( FRM, 'DOMAIN', DOM( : CHR_LEN( DOM ) ), STATUS )

*  If the DOMAIN was one of the standard Domains, set up Title, Symbols,
*  units, labels, etc.

*  Pixel co-ordinates...
         IF( DOM .EQ. 'PIXEL' ) THEN

*  Do not set a title since the title produced by the NDF library includes 
*  the pixel origin which we do not know here. Setting a title would mean
*  that the Frame would not match another PIXEL Frame (because the other
*  Frames Title would be set to a different value). 

*  For each axis, set up a label, symbol and unit value.
            DO I = 1, NAX
               IAT = 0
               CALL CHR_PUTI( I, TEXT, IAT )
               CALL AST_SETC( FRM, 'Label(' // TEXT( : IAT ) // ')',
     :                     'Pixel coordinate ' // TEXT( : IAT ), 
     :                     STATUS )
               CALL AST_SETC( FRM, 'Symbol(' // TEXT( : IAT ) // ')',
     :                     'p' // TEXT( : IAT ), STATUS )
               CALL AST_SETC( FRM, 'Unit(' // TEXT( : IAT ) // ')',
     :                     'pixel', STATUS )
            END DO

*  Data grid co-ordinates...
         ELSE IF( DOM .EQ. 'GRID' ) THEN

*  Store a string holding the coordinates at the centre of the first pixel.
            TEXT = '('
            IAT = 1
            DO I = 1, NAX
               IF ( I .GT. 1 ) CALL CHR_PUTC( ',', TEXT, IAT )
               CALL CHR_APPND( '1', TEXT, IAT )
            END DO
            CALL CHR_APPND( ')', TEXT, IAT )

*  Set up a suitable Frame title.
            IF ( NAX .EQ. 1 ) THEN
               CALL AST_SETC( FRM, 'Title', 'Data grid index; first '//
     :                        'pixel at '//TEXT( : IAT ), STATUS )
            ELSE
               CALL AST_SETC( FRM, 'Title', 'Data grid indices; '//
     :                        'first pixel at '//TEXT( : IAT ), STATUS ) 
            END IF

*  For each axis, set up a label, symbol and unit value.
            DO I = 1, NAX
               IAT = 0
               CALL CHR_PUTI( I, TEXT, IAT )
               CALL AST_SETC( FRM, 'Label(' // TEXT( : IAT ) // ')',
     :                     'Data grid index ' // TEXT( : IAT ), STATUS )
               CALL AST_SETC( FRM, 'Symbol(' // TEXT( : IAT ) // ')',
     :                     'g' // TEXT( : IAT ), STATUS )
               CALL AST_SETC( FRM, 'Unit(' // TEXT( : IAT ) // ')',
     :                     'pixel', STATUS )
            END DO

*  Graphical co-ordinates (symbols and labels are not set since these are
*  not set either by KPG1_GDGET or AST_PLOT)...
         ELSE IF( DOM .EQ. 'GRAPHICS' ) THEN
            CALL AST_SETC( FRM, 'Title', 'Graphical Coordinates', 
     :                     STATUS )
            CALL AST_SETC( FRM, 'Unit(1)', 'mm', STATUS )
            CALL AST_SETC( FRM, 'Unit(2)', 'mm', STATUS )

*  AGI BASE picture world co-ordinates.
         ELSE IF( DOM .EQ. 'BASEPIC' ) THEN
            CALL AST_SETC( FRM, 'Title', 'Normalised world '//
     :                     'co-ordinates in the AGI BASE picture.', 
     :                     STATUS )
         END IF

      END IF

*  If an error has occurred, annul the returned pointer, and return zero 
*  axes, and goive a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL AST_ANNUL( FRM, STATUS )
         NAX = 0
         CALL MSG_SETC( 'P', PDOM )
         CALL ERR_REP( 'KPG1_ASFGT_ERR', 'Failed to create a new '//
     :                 'co-ordinate Frame using parameter %^P.', 
     :                 STATUS )
      END IF

      END
