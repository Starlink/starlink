      SUBROUTINE KPG1_RDLST( PARAM, CURFRM, IWCS, NPOS, NAX, IPPOS,
     :                       IPID, TITLE, STATUS )
*+
*  Name:
*     KPG1_RDLST

*  Purpose:
*     Reads a set of positions from a CAT catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_RDLST( PARAM, CURFRM, IWCS, NPOS, NAX, IPPOS, IPID, TITLE, 
*                      STATUS )

*  Description:
*     This routine reads a FrameSet, and a set of positions with
*     associated integer identifiers from a CAT catalogue. An error is
*     reported if the catalogue does not contain a FrameSet. The FrameSet
*     should be stored as an AST Dump in the textual information associated
*     with the catalogue. Such catalogues can be created using KPG1_WRLST. 
*
*     It is assumed that the columns containing the axis values have CAT
*     names equal to the Symbol attribute of the corresponding AST Axis.
*     The catalogue columns from which to read the axis values are chosen
*     by matching column names with Axis Symbols (only columns containing 
*     floating point values are considered). Frames are checked in the
*     following order: the Base Frame, the Current Frame, all other
*     Frames in order of increasing Frame index. AN error is reported if no
*     Frame has a set of corresponding columns.
*
*     It is assumed that position identifiers are stored in an integer column 
*     with name PIDENT. If no such column is found, the returned position
*     identifiers start at 1 and increase monotonically.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     CURFRM = LOGICAL (Given)
*        If .TRUE. the positions read from the catalogue are Mapped
*        into the Current Frame of the associated FrameSet before being 
*        returned. Otherwise, they are returned in the Base Frame.
*     IWCS = INTEGER (Returned)
*        An AST pointer to the FrameSet read from the catalogue.
*     NPOS = INTEGER (Returned)
*        The number of positions returned.
*     NAX = INTEGER (Returned)
*        The number of axes in the Frame requested by CURFRM.
*     IPPOS = INTEGER (Returned)
*        A pointer to a 2-dimensional DOUBLE PRECISION array holding the
*        returned positions. Element (I,J) of this array gives axis J for
*        position I. The first axis will have NPOS elements, and the
*        second will have NAX elements.
*     IPID = INTEGER (Returned)
*        A pointer to a 1-dimensional INTEGER array holding the integer
*        identifiers for the returned positions. The array will have NPOS
*        elements.
*     TITLE = CHARACTER * ( * ) (Returned)
*        The value of the TITLE parameter in the supplied catalogue.
*        Returned blank if there is no TITLE parameter.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-OCT-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CAT_PAR'          ! CAT constants
      INCLUDE 'CAT_ERR'          ! CAT error constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      CHARACTER PARAM*(*)
      LOGICAL CURFRM

*  Arguments Returned:
      INTEGER IWCS
      INTEGER NPOS
      INTEGER NAX
      INTEGER IPPOS
      INTEGER IPID
      CHARACTER TITLE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ATTR*10          ! Attribute name
      CHARACTER CNAME*128        ! Catalogue name
      CHARACTER SYM*20           ! Axis symbol
      INTEGER CI                 ! Catalogue identifier
      INTEGER DTYPE              ! Data type identifier
      INTEGER FRM                ! Frame pointer
      INTEGER GAXIS( NDF__MXDIM )! CAT identifiers for axis columns
      INTEGER GID                ! CAT identifier for PIDENT column
      INTEGER GTTL               ! CAT identifier for TITLE parameter
      INTEGER I                  ! Loop count
      INTEGER IBASE              ! Index of Base Frame
      INTEGER ICURR              ! Index of Current Frame
      INTEGER IFRM               ! Frame index
      INTEGER IPCAT              ! Pointer to memory holding catalogue positions
      INTEGER IREQ               ! Index of requested Frame
      INTEGER J                  ! Axis index
      INTEGER JAT                ! No. of characters in a string
      INTEGER MAP                ! Pointer to mapping from file to IWCS
      INTEGER NAXCAT             ! No. of axes in Frame read from catalogue
      LOGICAL DONE               ! Have we read enough AST Objects?
      LOGICAL THERE              ! Was FrameSet found?

*.

*  Initialise.
      NPOS = 0
      IPPOS = 0
      IPID = 0
      TITLE = ' '

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Open the input catalogue.
      CALL LPG_CATASSOC( PARAM, 'READ', CI, STATUS )

*  Get the catalogue name for use in error messages.
      CALL CAT_TIQAC( CI, 'NAME', CNAME, STATUS )

*  Reset the pointer for the next item of textual information to be read
*  from the catalogue.
      CALL CAT_RSTXT( CI, STATUS )

*  Read AST Objects from the catalogue until a FrameSet is obtained, or no
*  more Objects are left.
      DONE = .FALSE.
      DO WHILE( .NOT. DONE .AND. STATUS .EQ. SAI__OK ) 
         CALL KPG1_RCATW( CI, IWCS, STATUS )

         IF( IWCS .NE. AST__NULL ) THEN
            IF( AST_ISAFRAMESET( IWCS, STATUS ) ) THEN
               DONE = .TRUE.
            ELSE
               CALL AST_ANNUL( IWCS, STATUS )
            END IF

         ELSE
            DONE = .TRUE.
         END IF

      END DO

*  If no FrameSet was found, report an error.
      IF( IWCS .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'CAT', CNAME )
         CALL ERR_REP( 'KPG1_RDLST_ERR1', 'Supplied catalogue '//
     :                 '''^CAT'' contains no WCS information.', STATUS )
         GO TO 999
      END IF

*  Note the indices of the Base and Current Frames in the FrameSet read
*  from the catalogue.
      IBASE = AST_GETI( IWCS, 'BASE', STATUS )
      ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Loop round each Frame in the FrameSet, looking for a Frame with axis
*  Symbols for which columns exist. The Base and Current Frames are checked 
*  first ( indices "-1" and "0" ).
      DO I = -1, AST_GETI( IWCS, 'NFRAME', STATUS )

*  Get the index of the Frame to be checked next. Check the Base Frame first. 
*  After the Base Frame has been checked, check the Current Frame, then check 
*  each subsequent Frame in order, jumping over the Base and Current Frames.
         IF( I .EQ. -1 ) THEN
            IFRM = IBASE

         ELSE IF( I .EQ. 0 ) THEN
            IFRM = ICURR

         ELSE IF( I .NE. IBASE .AND. I .NE. ICURR ) THEN
            IFRM = I

         ELSE 
            IFRM = AST__NOFRAME

         END IF

*  Check the Frame if required.
         IF( IFRM .NE. AST__NOFRAME ) THEN

*  Get a pointer to the Frame.
            FRM = AST_GETFRAME( IWCS, IFRM, STATUS )

*  Loop round each axis of the Frame.
            NAX = AST_GETI( FRM, 'NAXES', STATUS )
            DO J = 1, NAX

*  Get the Axis Symbol attribute.
               ATTR = 'SYMBOL('
               JAT = 7
               CALL CHR_PUTI( J, ATTR, JAT )
               CALL CHR_APPND( ')', ATTR, JAT )
               SYM = AST_GETC( FRM, ATTR( : JAT ), STATUS )

*  Look for a catalogue column with this name.
               CALL CAT_TIDNT( CI, SYM, GAXIS( J ), STATUS )

*  If not found, annul the error and pass on to the next Frame.               
               IF( STATUS .EQ. CAT__NOCMP ) THEN
                  CALL ERR_ANNUL( STATUS )
                  GO TO 10
               END IF

*  Check the column contains floating point values. Pass on to the next
*  Frame if it does not.
               CALL CAT_TIQAI( GAXIS( J ), 'DTYPE', DTYPE, STATUS ) 
               IF( DTYPE .NE. CAT__TYPER .AND. 
     :             DTYPE .NE. CAT__TYPED ) GO TO 10

            END DO

*  We only arrive here if a floating point column corresponding to each Frame 
*  Axis was found. Jump out of the Frame loop, retaining the details of
*  the Frame which has just been checked.
            GO TO 20

*  Arrive here if columns could not be found for the Frame being checked.
*  Annul the Frame pointer, and go round again to check the next Frame.
 10         CONTINUE
            CALL AST_ANNUL( FRM, STATUS )
            IFRM = AST__NOFRAME

         END IF

      END DO

*  Arrive here when a matching Frame has been found, or all Frames have
*  been checked.
 20   CONTINUE

*  If no Frame was found for which columns exist, report an error.
      IF( IFRM .EQ. AST__NOFRAME .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'CAT', CNAME )
         CALL ERR_REP( 'KPG1_RDLST_ERR2', 'Could not find columns in '//
     :                 '''^CAT'' containing positions in any of the '//
     :                 'WCS Frames in the catalogue.', STATUS )
         GO TO 999
      END IF

*  Find the number of rows in the catalogue. This is the number of
*  positions to be read.
      CALL CAT_TROWS( CI, NPOS, STATUS )

*  Allocate memory to hold the positions read from the catalogue.
      CALL PSX_CALLOC( NPOS*NAX, '_DOUBLE', IPPOS, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Copy the axis values from the catalogue columns into this array.
      CALL KPG1_CTCPD( CI, NAX, GAXIS, NPOS, %VAL( IPPOS ), STATUS )

*  Get the Mapping from the Frame in which the positions are stored in
*  the catalogue, to the Frame requested by argument CURFRM. Store a
*  null pointer if the positions are already in the requested Frame.
      IF( CURFRM ) THEN
         IREQ = ICURR
      ELSE
         IREQ = IBASE
      END IF

      IF( IFRM .EQ. IREQ ) THEN
         MAP = AST__NULL    

      ELSE
         MAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS, IFRM, IREQ, 
     :                                       STATUS ), STATUS )

         IF( AST_ISAUNITMAP( MAP, STATUS ) ) THEN
            CALL AST_ANNUL( MAP, STATUS )
         END IF

      END IF

*  If a Mapping is required, transform the positions.
      IF( MAP .NE. AST__NULL ) THEN 

*  Save the pointer to the catalogue positions and the number of axes in
*  the catalogue Frame in different local variables
         IPCAT = IPPOS
         NAXCAT = NAX

*  Get the number of axes in the requested Frame.
         IF( CURFRM ) THEN
            NAX = AST_GETI( IWCS, 'NOUT', STATUS )
         ELSE
            NAX = AST_GETI( IWCS, 'NIN', STATUS )
         END IF
 
*  Allocate memory to hold the returned positions in the requested Frame.
         CALL PSX_CALLOC( NPOS*NAX, '_DOUBLE', IPPOS, STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999
         
*  Map the positions.
         CALL AST_TRANN( MAP, NPOS, NAXCAT, NPOS, %VAL( IPCAT ), .TRUE., 
     :                   NAX, NPOS, %VAL( IPPOS ), STATUS ) 

*  Free the memory holding thre positions read from the catalogue.
         CALL PSX_FREE( IPCAT, STATUS )

      END IF

*  Allocate memory to hold the returned position identifiers.
      CALL PSX_CALLOC( NPOS, '_INTEGER', IPID, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  See if there is column with name "PIDENT" in the catalogue. If found, this
*  column is assumed to hold positions identifiers.
      CALL CAT_TIDNT( CI, 'PIDENT', GID, STATUS )

*  If it exists, check that it has type INTEGER.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL CAT_TIQAI( GID, 'DTYPE', DTYPE, STATUS) 

*  If not, issue a warning message and ignore the PIDENT column.
         IF( DTYPE .NE. CAT__TYPEI ) THEN
            CALL MSG_SETC( 'CAT', CNAME )
            CALL MSG_OUT( 'KPG1_RDLST_MSG1', 'WARNING: The position '//
     :                    'identifiers (column ''PIDENT'') in '//
     :                    'catalogue  ''^CAT'' are not integers and '//
     :                    'will be ignored.', STATUS )
            GID = CAT__NOID
         END IF

*  If the component was not found, annul the error, and assume monotonic 
*  position identifiers.
      ELSE IF( STATUS .EQ. CAT__NOCMP ) THEN
         CALL ERR_ANNUL( STATUS )
         GID = CAT__NOID
      END IF

*  Store the positions identifiers. Monotonic identifiers starting at 1
*  are stored if no suitable PIDENT column was found in the catalogue.
      CALL KPG1_CTCPI( CI, 1, GID, NPOS, %VAL( IPID ), STATUS )

*  Get an identifier for the TITLE parameter in the catalogue.
      CALL CAT_TIDNT( CI, 'TITLE', GTTL, STATUS )

*  If it exists, get its value.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL CAT_TIQAC( GTTL, 'VALUE', TITLE, STATUS) 

*  Otherwise, annul the error and return a blank string.
      ELSE IF( STATUS .EQ. CAT__NOCMP ) THEN
         CALL ERR_ANNUL( STATUS )
         TITLE = ' '
      END IF

*  Tidy up.
*  ========
 999  CONTINUE

*  Export the FrameSet pointer.
      CALL AST_EXPORT( IWCS, STATUS )

*  If an error has occurrred, Give a context message, and free the
*  returned pointers, etc.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL AST_ANNUL( IWCS, STATUS )

         CALL PSX_FREE( IPPOS, STATUS )
         CALL PSX_FREE( IPID, STATUS )

         NPOS = 0
         IPPOS = 0
         IPID = 0
         TITLE = ' '

         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_RDLST_ERR3', 'Error reading a positions '//
     :                 'list using parameter %^PARAM.', STATUS )

      END IF

*  Release the catalogue identifier.
      CALL CAT_TRLSE( CI, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
