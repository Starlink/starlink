      SUBROUTINE KPG1_RDLST( PARAM, CURFRM, IWCS, NPOS, NAX, IPPOS,
     :                       IPID, TITLE, STATUS )
*+
*  Name:
*     KPG1_RDLST

*  Purpose:
*     Reads a set of positions from a positions list file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_RDLST( PARAM, CURFRM, IWCS, NPOS, NAX, IPPOS, IPID, TITLE, 
*                      STATUS )

*  Description:
*     This routine reads a FrameSet, and a set of positions with
*     associated integer identifiers from a positions list text file. 
*     The file will normally have been created by KPG1_WRLST. If the supplied 
*     positions list does not contain a FrameSet, then the positions are 
*     assumed to be pixel co-ordinates and a FrameSet is returned containing 
*     a single PIXEL Domain Frame.
*
*     The positions read from the file asre assumed to be Base Frame
*     positions (within the FrameSet stored in the file), but they may
*     optionally be mapped into the Current Frame of the FrameSet before
*     being returned.
*
*     The file must contain integer position identifiers in column 1. If
*     any decimal points are found in the first column an error is
*     reported.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     CURFRM = LOGICAL (Given)
*        If .TRUE. the positions read from the positions list are Mapped
*        into the Current Frame of the associated FrameSet before being 
*        returned. Otherwise, they are returne din the Base Frame.
*     IWCS = INTEGER (Returned)
*        An AST pointer to a FrameSet read from the positions list, or a
*        default PIXEL Frame if the file contains no FrameSet.
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
*        The title stored at the top of the text file. Returned
*        blank if there is no title.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO error constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Global Variables:
      INCLUDE 'KPG_AST'          ! KPG AST common blocks.
*        ASTLN = INTEGER (Write)
*           Index of previous GRP element to be read.
*        ASTGRP = INTEGER (Write)
*           GRP identifier for group holding text to be read.

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

*  External References:
      EXTERNAL KPG1_ASGFR        ! Source function for AST Channel

*  Local Variables:
      CHARACTER BUFFER*(GRP__SZNAM)! Text read from file
      CHARACTER C*1              ! Current character within file line
      CHARACTER FNAME*(GRP__SZFNM)! File name
      INTEGER CHAN               ! AST pointer to Channel
      INTEGER FD                 ! FIO file descriptor for input file
      INTEGER I                  ! Loop count
      INTEGER IAT                ! No. of characters in a string
      INTEGER ICOM               ! Index of start of end of line comment
      INTEGER IGRP1              ! Identifier for comments group 
      INTEGER IGRP2              ! Identifier for non-comments group
      INTEGER IPW                ! Pointer to workspace for file positions
      INTEGER LINE               ! Index of current file line
      INTEGER MAP                ! Pointer to mapping from file to IWCS
      INTEGER NAXB               ! No. of axes in Base Frame read from file
      INTEGER NAXC               ! No. of axes in Current Frame
      INTEGER NCHAR              ! Length of text read from file
      INTEGER NCOL               ! No. of axis value columns in file
      INTEGER NW                 ! No. of words in current text line
      INTEGER OBJ                ! Pointer to AST Object read from file
      LOGICAL INWRD              ! Is current character within a word?
      LOGICAL FRIPW              ! Free the IPW pointer?
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

*  Initialise a flag to indicate that the IPW pointer does not need to be
*  freed.
      FRIPW = .FALSE.

*  Copy the contents of the file into two GRP groups, one holding the
*  comment lines and the other holding the non-comment lines. Also find
*  the number of columns of axis values in the file.
*  ====================================================================

*  Obtain the input text file, and save its name.
      CALL FIO_ASSOC( PARAM, 'READ', 'LIST', 0, FD, STATUS )
      CALL FIO_FNAME( FD, FNAME, STATUS ) 

*  Create a GRP group to hold all comment lines read from the file.
      CALL GRP_NEW( 'Comment lines', IGRP1, STATUS )

*  Create a GRP group to hold the positions lines read from the file.
      CALL GRP_NEW( 'Non-comment lines', IGRP2, STATUS )

*  Initialise the total number of columns (including identifiers).
      NCOL = 0

*  Initialise the line number.
      LINE = 0

*  Loop round until an error is encountered (this will happen when the end of 
*  file is reached, if not before).
      DO WHILE ( STATUS .EQ. SAI__OK )

*  Read a record of the text file.
         BUFFER = ' '
         CALL FIO_READ( FD, BUFFER, NCHAR, STATUS )

*  Increment the line number.
         LINE = LINE + 1

*  Remove leading spaces.
         CALL CHR_LDBLK( BUFFER )

*  If the current line looks like a title line, and we do not currently
*  have a title, save the title from this line.
         IF( BUFFER( : 10 ) .EQ. '#  Title: ' .AND. 
     :       TITLE .EQ. ' ' ) TITLE = BUFFER( 11 : )

*  Store whole line comments in the first group.
         IF( BUFFER( 1 : 1 ) .EQ. '#' ) THEN
            CALL GRP_PUT( IGRP1, 1, BUFFER( : NCHAR ), 0, STATUS ) 

*  Store position lines in the second group, excluding any end of line
*  comments. 
         ELSE IF( BUFFER .NE. ' ' ) THEN
            ICOM = INDEX( BUFFER, '#' ) 
            IF( ICOM .NE. 0 ) NCHAR = ICOM - 1
            CALL GRP_PUT( IGRP2, 1, BUFFER( : NCHAR ), 0, STATUS ) 

*  Count the number of words in this position line, and report an error if 
*  the first word in any column contains a decimal point. 
            NW = 0
            INWRD = .FALSE.
            DO I = 1, NCHAR

               C = BUFFER( I : I )

               IF( C .EQ. ' ' ) THEN
                  INWRD = .FALSE.
               ELSE IF( .NOT. INWRD ) THEN
                  INWRD = .TRUE.
                  NW = NW + 1
               END IF

               IF( NW .EQ. 1 .AND. C .EQ. '.' .AND.
     :             STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'LINE', LINE )               
                  CALL MSG_SETC( 'FILE', FNAME )               
                  CALL MSG_SETC( 'BUF', BUFFER )               
                  CALL ERR_REP( 'KPG1_RDLST_ERR1', 'Non-integer '//
     :                          'position identifier found at line '//
     :                          '^LINE in file ^FILE: ''^BUF''', 
     :                          STATUS )
                  GO TO 999

               END IF

            END DO

*  If this is the first position line to be found, store the number of
*  words as the total number of columns.
            IF( NCOL .EQ. 0 ) THEN
               NCOL = NW

*  Otherwise, report an error if this line has a different number of
*  columns.
            ELSE IF( NCOL .NE. NW .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'LINE', LINE )               
               CALL MSG_SETC( 'FILE', FNAME )               
               CALL ERR_REP( 'KPG1_RDLST_ERR2', 'Inconsistent number '//
     :                       'of columns found at line ^LINE in file '//
     :                       '^FILE.', STATUS )
            END IF

         END IF

      END DO

*  If an end-of-file error has been reported, annul it.
      IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )

*  Close the file.
      CALL FIO_CLOSE( FD, STATUS )

*  Adjust the number of columns in the file to exclude the identifier
*  column.
      NCOL = NCOL - 1

*  Read the FrameSet from the positions list.
*  ==========================================
*  Create an AST Channel which can be used to read AST Objects from the 
*  comments group. Set the Skip attribute non-zero so that non-AST data which 
*  occur between AST Objects are ignored.
      CHAN = AST_CHANNEL( KPG1_ASGFR, AST_NULL, 'SKIP=1', STATUS )

*  Store the identifier for the group containing the comments in common so
*  that it can be used by the source function KPG1_ASGFR. Also initialise
*  the index of the last group element to have been read.
      ASTGRP = IGRP1
      ASTLN = 0

*  Attempt to read an Object from the group.
      OBJ = AST_READ( CHAN, STATUS )

*  If nothing was read from the file, create a default FrameSet holding a 
*  single default PIXEL Frame.
      IF( OBJ .EQ. AST__NULL ) THEN
         IWCS = AST_FRAMESET( AST_FRAME( NCOL, 'DOMAIN=PIXEL', STATUS ), 
     :                        ' ', STATUS )

*  If an AST Object was read from the file...
      ELSE

*  If a Frame was read from the file, create a FrameSet holding the Frame.
         IF( AST_GETC( OBJ, 'CLASS', STATUS ) .EQ. 'FRAME' ) THEN
            IWCS = AST_FRAMESET( OBJ, ' ', STATUS )

*  If a FrameSet was read from the file, use it.
         ELSE IF( AST_ISAFRAMESET( OBJ, STATUS ) ) THEN
            IWCS = AST_CLONE( OBJ, STATUS )

*  Report an error for any other class of AST Object.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETC( 'CLASS', AST_GETC( OBJ, 'CLASS', STATUS ) )
            CALL MSG_SETC( 'FILE', FNAME )               

            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPG1_RDLST_ERR3', 'AST Object of class '//
     :                 '^CLASS found in file ^FILE. This application '//
     :                 'requires a FrameSet (possible programming '//
     :                 'error).', STATUS )
         END IF

*  Find the number of Axes in the Base Frame.
         NAXB = AST_GETI( IWCS, 'NIN', STATUS )

*  Report an error if the number of columns is not equal to the number
*  of Base Frame axes.
         IF( NAXB .NE. NCOL .AND. STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETI( 'NAX', NAXB )
            CALL MSG_SETI( 'NC', NCOL )
            CALL MSG_SETC( 'FILE', FNAME )               

            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPG1_RDLST_ERR4', 'Number of columns (^NC)'//
     :                    'in file ^FILE does not equal the number of'//
     :                    ' axes (^NAX) in the Base Frame of the '//
     :                    'FrameSet stored in the file (possible '//
     :                    'programming error).', STATUS )
            GO TO 999

         END IF

      END IF

*  Read the positions from the file and map them into the required Frame.
*  ======================================================================
*  Get the Mapping from the Base Frame to the required Frame.
      IF( CURFRM ) THEN
         MAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS, AST__BASE, 
     :                                       AST__CURRENT, STATUS ),
     :                       STATUS )

*  If positions are required in the Base Frame, store a null pointer to
*  indicate that no mapping is required.
      ELSE
         MAP = AST__NULL 
      END IF

*  Get the number of positions supplied in the file. Abort if it is empty
      CALL GRP_GRPSZ( IGRP2, NPOS, STATUS )
      IF( NPOS .EQ. 0 ) GO TO 999

*  Allocate memory to hold the identifiers read from the file.
      CALL PSX_CALLOC( NPOS, '_INTEGER', IPID, STATUS )

*  Allocate memory to hold the positions read from the file. 
      CALL PSX_CALLOC( NAXB*NPOS, '_DOUBLE', IPW, STATUS )
      FRIPW = .TRUE.

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Read the positions and identifiers from the second GRP group into the 
*  work array. These are assumed to be Base Frame positions.
      CALL KPG1_RDLS2( IGRP2, NPOS, NAXB, %VAL( IPW ), %VAL( IPID ), 
     :                 STATUS )

*  If the positions do not need to be mapped, just return the pointer to 
*  the above work array. Also return the number of axes.
      IF( MAP .EQ. AST__NULL .OR. AST_ISAUNITMAP( MAP, STATUS ) ) THEN
         IPPOS = IPW
         FRIPW = .FALSE.
         NAX = NAXB

*  Otherwise, they need to be mapped from Base to Current Frame.
      ELSE

*  Find the number of axes in the current Frame of the returned FrameSet.
         NAXC = AST_GETI( IWCS, 'NOUT', STATUS )

*  Allocate memory to hold the returned positions.
         CALL PSX_CALLOC( NAXC*NPOS, '_DOUBLE', IPPOS, STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Transform the positions read from the file using this Mapping.
         CALL AST_TRANN( MAP, NPOS, NAXB, NPOS, %VAL( IPW ), .TRUE., 
     :                   NAXC, NPOS, %VAL( IPPOS ), STATUS ) 

*  Return the number of axes.
         NAX = NAXC

      END IF

 999  CONTINUE

*  Delete the groups.
      CALL GRP_DELET( IGRP1, STATUS )
      CALL GRP_DELET( IGRP2, STATUS )

*  Annul the work space if required.
      IF( FRIPW ) CALL PSX_FREE( IPW, STATUS )

*  Export the FrameSet pointer in case it was created in this routine.
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
         CALL ERR_REP( 'KPG1_RDLST_ERR5', 'Error reading a positions '//
     :                 'list using parameter %^PARAM.', STATUS )

      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
