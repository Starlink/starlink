      SUBROUTINE KPG1_WRTA2( PARAM, ARRDIM, NPOS, NAX, POS, IWCS,
     :                       TITLE, ID0, IDENTS, KEYMAP, LABS, HIST,
     :                       STATUS )
*+
*  Name:
*     KPG1_WRTA2

*  Purpose:
*     Puts a set of positions into a text file as a CAT catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_WRTA2( PARAM, ARRDIM, NPOS, NAX, POS, IWCS, TITLE,
*                      ID0, IDENTS, KEYMAP, LABS, HIST, STATUS )

*  Description:
*     This routine writes the supplied positions to a CAT catalogue
*     (see SUN/181). A dump of the supplied FrameSet (if any) is included
*     in the text file as a set of "text" lines. A column is created
*     with name "PIDENT" to contain the integer identifiers. A column is
*     also created for each axis of the Base Frame, with a name equal to
*     the Symbol attribute of the Axis (AXIS_<n> is used if the Symbol is
*     blank). The catalogue can be read using KPG1_RDLST (and also XCATVIEW
*     etc.).

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     ARRDIM = INTEGER (Given)
*        The size of the first dimension of the positions array. This must
*        be larger than or equal to NPOS.
*     NPOS = INTEGER (Given)
*        The number of positions to store in the file.
*     NAX = INTEGER (Given)
*        The number of axes for each position.
*     POS( ARRDIM, NAX ) = DOUBLE PRECISION (Given)
*        The positions to store in the file. POS( I, J ) should give the
*        axis J value for position I. The positions should be in the Base
*        Frame of the FrameSet supplied using argument IWCS.
*     IWCS = INTEGER (Given)
*        A pointer to an AST FrameSet to store with the positions.
*     TITLE = CHARACTER * ( * ) (Given)
*        A title to store at the top of the text file.
*     ID0 = INTEGER (Given)
*        The integer identifier value to associate with the first
*        supplied position. Identifiers for subsequent positions increase
*        by 1 for each position. If this is supplied less than or equal
*        to zero, then its value is ignored and the identifiers supplied
*        in array IDENTS are used instead.
*     IDENTS( NPOS ) = INTEGER (Given)
*        The individual integer identifiers to associate with each
*        position. Only accessed if ID0 is less than or equal to zero.
*     KEYMAP = INTEGER (Given)
*        An optional AST KeyMap containing data for extra columns to
*        add to the catalogue. It can be used (for instance) to add
*        character columns to the catalogue. If a value of AST__NULL
*        is supplied, no extra columns are added to the catalogue.
*        Otherwise, the column names and values can be specified using
*        either of the following two schemes:
*
*        - If the KeyMap contains an entry called "COLNAMES", it is
*        assumed to be a vector entry holding the names of the columns.
*        For each of these column names, the value to store in a
*        particular row of the catalogue is assumed to be held in a
*        scalar KeyMap entry with key "<colname>_<row number>" ( "_1"
*        for the first row). If no such entry exists for a particular
*        row, then the value is marked as bad in the catalogue. The data
*        type of the column is determined from the first row value found
*        for the column.
*
*        - If the KeyMap does not contain an entry called "COLNAMES",
*        it is assumed that each entry in the KeyMap contains a vector
*        holding all the values for a single column, in row order. The
*        entry key is used as the column name, and the column data type
*        is determined from the entry data type.
*     LABS = INTEGER (Given)
*        A GRP group identifier containing the labels to be associated
*        with the positions. The number of elements in this group should
*        be equal to NPOS. If GRP__NOID is supplied, no label column will
*        be created.
*     HIST = INTEGER (Given)
*        A GRP group identifier containing history text to store with the
*        catalogue.  If GRP__NOID is supplied, no history information
*        will be stored with the catalogue.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998, 2003 Central Laboratory of the Research Councils.
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-SEP-1998 (DSB):
*        Original version.
*     7-AUG-2003 (DSB):
*        Normalise axis values before appending to the output catalogue.
*     20-NOV-2006 (DSB):
*        Renamed from kpg1_wrls2.f to kpg1_wrta2.f, and added argument
*        LABS.
*     14-DEC-2006 (DSB):
*        Mark columns of celestial longitude or latitude values as such even
*        if the supplied Frame also contains one or more non-celestial axes.
*     25-JAN-2007 (DSB):
*        Added parameter HIST.
*     27-APR-2009 (DSB):
*        Added parameter KEYMAP.
*     28-APR-2009 (DSB):
*        Add a second scheme for holding column values in a KeyMap,
*        based on the use of scalar entries rather than vector entries.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CAT_PAR'          ! CAT constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'CNF_PAR'          ! CNF constants and function declarations
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER ARRDIM
      INTEGER NPOS
      INTEGER NAX
      DOUBLE PRECISION POS( ARRDIM, NAX )
      INTEGER IWCS
      CHARACTER TITLE*(*)
      INTEGER ID0
      INTEGER IDENTS( NPOS )
      INTEGER KEYMAP
      INTEGER LABS
      INTEGER HIST

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER MXDIM              ! Max no. of axes
      PARAMETER ( MXDIM = 50 )

      INTEGER MXKEYS             ! Max no. of keys allowed in KeyMap
      PARAMETER ( MXKEYS = 10 )

*  Local Variables:
      CHARACTER ATTR*10          ! AST attribute name
      CHARACTER BUFFER*80        ! Text buffer
      CHARACTER HTEXT*(GRP__SZNAM)! A line of history text
      CHARACTER KEY*40           ! KeyMap entry key
      CHARACTER LAB*50           ! Axis label
      CHARACTER LABEL*100        ! Position label
      CHARACTER SYM*20           ! Axis symbol
      CHARACTER UNT*20           ! Axis units
      CHARACTER COLNAM( MXKEYS )*30! Column names from KeyMap
      DOUBLE PRECISION C( MXDIM) ! Buffer for a single position
      INTEGER AXFRM              ! 1D Frame holding current axis
      INTEGER AXMAP              ! Mapping from full Frame to 1D Frame
      INTEGER CATYPE             ! CAT column data type
      INTEGER CI                 ! CAT identifier for catalogue
      INTEGER COLID( -1:MXDIM )  ! CAT identifiers for columns
      INTEGER FRM                ! Pointer to Frame
      INTEGER I                  ! Position index
      INTEGER IAT                ! No. of characters in string
      INTEGER IAT0               ! Length of key root
      INTEGER IPTEXT             ! Pointer to text buffer
      INTEGER J                  ! Axis index
      INTEGER KMCOL( MXKEYS )    ! CAT identifiers for extra columns
      INTEGER KMLEN( MXKEYS )    ! No. of values for each extra column
      INTEGER NKMCOL             ! No. of columns supplied in KeyMap
      INTEGER LABLEN             ! Length of longest label
      INTEGER LENC               ! Length of formatted KeyMap entry
      INTEGER LWCS               ! Pointer to the FrameSet to be stored
      INTEGER MAXLEN             ! Maximum formatted string length
      INTEGER NHIST              ! Number of lines of history text
      INTEGER NLAB               ! Number of labels supplied
      INTEGER QI                 ! CAT identifier for another parameter
      INTEGER SCHEME             ! Says how columns are stored in KeyMap
      INTEGER TI                 ! CAT identifier for TITLE parameter
      INTEGER TYPE               ! AST KeyMap entry data type
      INTEGER VLEN               ! Used length of value string
      LOGICAL COPIED             ! Has a copy of the Frameet been taken?
      LOGICAL DONE               ! Leave the loop?
      LOGICAL GOTVAL             ! Does the KeyMap have the entry?
      LOGICAL HASSKY             ! Does the Frame contain a SkyFrame?
      LOGICAL JUNK               ! Unused
      LOGICAL SKYAX              ! Is the Axis a SkyAxis?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST Context.
      CALL AST_BEGIN( STATUS )

*  Take a clone of the FrameSet pointer, and indicate that we have a
*  clone and not a copy.
      LWCS = AST_CLONE( IWCS, STATUS )
      COPIED = .FALSE.

*  Get a pointer to the Base Frame.
      FRM = AST_GETFRAME( LWCS, AST__BASE, STATUS )

*  Create the output catalogue.
      CALL LPG_CATCREAT( PARAM, CI, STATUS )

*  Store the supplied title as the catalogue's TITLE parameter.
      CALL CAT_PPTAC( CI, 'TITLE', MAX( 1, CHR_LEN( TITLE ) ),
     :                CAT__SCALR, 1, ' ', ' ', .TRUE., 'Title', TITLE,
     :                TI, STATUS)

*  Create a column to hold integer identifiers for each position.
      CALL CAT_CNEWS( CI, 'PIDENT', CAT__TYPEI, 0, ' ', ' ',
     :                'Position identifier', COLID( 0 ), STATUS )

*  Initialise a flag to indicate we have not yet found any columns holding
*  celestial longitude or latitude values.
      HASSKY = .FALSE.

*  Loop round creating columns for each axis.
      DO I = 1, NAX

*  Get the Symbol, Unit and Label attributes for this axis.
         ATTR = 'Symbol('
         IAT = 7
         CALL CHR_PUTI( I, ATTR, IAT )
         CALL CHR_APPND( ')', ATTR, IAT )
         SYM = AST_GETC( FRM, ATTR( : IAT ), STATUS )

         ATTR( : 6 ) = ' Label'
         LAB = AST_GETC( FRM, ATTR( : IAT ), STATUS )

         ATTR( : 6 ) = '  Unit'
         UNT = AST_GETC( FRM, ATTR( : IAT ), STATUS )

*  The axis symbol is used as the column name. If the symbol is blank,
*  use "AXIS_<i>" instead. Take a copy of the FrameSet first to avoid
*  changing the original.
         IF( SYM .EQ. ' ' ) THEN
            SYM = 'AXIS_'
            IAT = 5
            CALL CHR_PUTI( I, SYM, IAT )

            IF( .NOT. COPIED ) THEN
               CALL AST_ANNUL( LWCS, STATUS )
               LWCS = AST_COPY( IWCS, STATUS )
               COPIED = .TRUE.
            END IF

            ATTR( : 6 ) = 'Symbol'
            CALL AST_SETC( FRM, ATTR, SYM( : IAT ), STATUS )

         END IF

*  See if this axis is a SkyAxis (assumed to be the case if the
*  associated Domain is "SKY")..
         AXFRM = AST_PICKAXES( FRM, 1, I, AXMAP, STATUS )
         SKYAX = ( AST_GETC( AXFRM, 'Domain', STATUS ) .EQ. 'SKY' )

*  If this axis is a SkyAxis, use special Units strings which indicate to
*  the CAT_ library that the column represents a angle.
         IF( SKYAX ) THEN
            HASSKY = .TRUE.
            ATTR = 'FORMAT('
            IAT = 7
            CALL CHR_PUTI( I, ATTR, IAT )
            CALL CHR_APPND( ')', ATTR, IAT )
            UNT = 'RADIANS{'
            IAT = 8
            CALL CHR_APPND( AST_GETC( FRM, ATTR, STATUS ), UNT, IAT )
            CALL CHR_APPND( '}', UNT, IAT )
         END IF

*  Create the column.
         CALL CAT_CNEWS( CI, SYM, CAT__TYPED, 0, UNT, ' ', LAB,
     :                   COLID( I ), STATUS )

      END DO

*  If a SkyAxis was found in the Frame, store the epoch and equinox as
*  catalogue parameters.
      IF( HASSKY ) THEN
         CALL CAT_PPTSC( CI, 'EPOCH', AST_GETC( FRM, 'EPOCH', STATUS ),
     :                   ' ', QI, STATUS )
         CALL CAT_TATTI( QI, 'CSIZE', 12, STATUS )
         CALL CAT_PPTSC( CI, 'EQUINOX', AST_GETC( FRM, 'EQUINOX',
     :                                            STATUS ),
     :                   ' ', QI, STATUS )
         CALL CAT_TATTI( QI, 'CSIZE', 12, STATUS )
      END IF

*  Create a column to hold labels, if required.
      IF( LABS .NE. GRP__NOID ) THEN
         CALL GRP_GRPSZ( LABS, NLAB, STATUS )

         LABLEN = 0
         DO I = 1, NLAB
            CALL GRP_GET( LABS, I, 1, LABEL, STATUS )
            LABLEN = MAX( LABLEN, CHR_LEN( LABEL ) )
         END DO

         CALL CAT_CNEWS( CI, 'LABEL', CAT__TYPEC, LABLEN, ' ', ' ',
     :                   'Position label', COLID( -1 ), STATUS )
      END IF

*  Create columns to hold the entries in any supplied KeyMap.
      NKMCOL = 0
      MAXLEN = 0

      IF( KEYMAP .NE. AST__NULL ) THEN

*  Scheme 1 - does the KeyMap have an entry called COLNAMES? If so, it
*  is a vector holding the column names. Each column value is in a separate
*  scalar entry.
         IF( AST_MAPHASKEY( KEYMAP, 'COLNAMES', STATUS ) ) THEN
            SCHEME = 1

*  Get the number of columns described by the KeyMap and check it is
*  not too high.
            NKMCOL = AST_MAPLENGTH( KEYMAP, 'COLNAMES', STATUS )
            IF( NKMCOL .GT. MXKEYS .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'N', NKMCOL )
               CALL MSG_SETI( 'M', MXKEYS )
               CALL MSG_SETC( 'P', PARAM )
               CALL ERR_REP( ' ', 'Cannot create ^N extra columns in '//
     :                       'the catalogue associated with parameter'//
     :                       ' ''^P'' - no more than ^M are allowed '//
     :                       '(programming error).', STATUS )
               GO TO 999
            END IF

*  Read all the column names into a local array.
            GOTVAL = AST_MAPGET1C( KEYMAP, 'COLNAMES', NKMCOL, NKMCOL,
     :                             COLNAM, STATUS )

*  To create the catalogue column, we need to determine a data type for
*  the column, and the maximum length of a formatted column value. The
*  data type we get from the first value found in the KeyMap. The
*  formatted length can also be taken from the first value unless the
*  data type is string, in which case we meed to scan all column values
*  to find the longest string. Loop round all columns described by the
*  KeyMap.
            DO I = 1, NKMCOL

*  Form the root key name for KeyMap entries holding values for this
*  catalogue column. The row number needs to be appended to this root
*  key to form the full key.
               KEY = COLNAM( I )
               IAT0 = CHR_LEN( KEY )
               CALL CHR_APPND( '_', KEY, IAT0 )

*  Inidcate we do not yet know the column data type.
               TYPE = AST__BADTYPE

*  Loop round the rows looking for the first row with a value for this
*  column.
               J = 1
               DONE = .FALSE.
               DO WHILE( .NOT. DONE .AND. STATUS .EQ. SAI__OK )

*  Form the full key for the entry holding the value of column I, row J.
                  IAT = IAT0
                  CALL CHR_PUTI( J, KEY, IAT )

*  If the entry exists in the KeyMap...
                  IF( AST_MAPHASKEY( KEYMAP, KEY( : IAT ),
     :                               STATUS ) ) THEN

*  If this is the first value found, get its data type, and store the
*  length of the formatted value
                     IF( TYPE .EQ. AST__BADTYPE ) THEN
                        TYPE = AST_MAPTYPE( KEYMAP, KEY( : IAT ),
     :                                      STATUS )
                        LENC = AST_MAPLENC( KEYMAP, KEY( : IAT ),
     :                                      STATUS )

*  If this is not the first value found, update the maximum length of
*  a formatted column value.
                     ELSE
                        LENC = MAX( LENC, AST_MAPLENC( KEYMAP,
     :                                                 KEY( : IAT ),
     :                                                 STATUS ) )
                     END IF

*  If the column holds strings, we need to continue looping to find the
*  length of the longest string in the column. Otherwise, we can leave
*  the loop now since all formatted values will be equal in length.
                     IF( TYPE .NE. AST__STRINGTYPE ) DONE = .TRUE.
                  END IF

*  Find the index of the next row, or leave the loop if all rows have been
*  done.
                  IF( J .LT. NPOS ) THEN
                     J = J + 1
                  ELSE
                     DONE = .TRUE.
                  END IF

               END DO

*  Set the maximum number of values in the column.
               KMLEN( I ) = NPOS

*  If no values were found for this column, set the column size to
*  zero to indicate that the catalogue column should not be created.
               IF( TYPE .EQ. AST__BADTYPE ) THEN
                  KMLEN( I ) = 0
                  LENC = 0

*  Otherwise, convert the data type from AST to CAT.
               ELSE IF( TYPE .EQ . AST__INTTYPE ) THEN
                  CATYPE = CAT__TYPEI

               ELSE IF( TYPE .EQ . AST__DOUBLETYPE ) THEN
                  CATYPE = CAT__TYPED

               ELSE IF( TYPE .EQ . AST__FLOATTYPE ) THEN
                  CATYPE = CAT__TYPER

               ELSE IF( TYPE .EQ . AST__STRINGTYPE ) THEN
                  CATYPE = CAT__TYPEC

*  Report an error for complex data types.
               ELSE IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'K', COLNAM( I ) )
                  CALL MSG_SETC( 'P', PARAM )
                  CALL ERR_REP( ' ', 'Cannot create the ''^K'' column'//
     :                          ' within the catalogue associated '//
     :                          'with parameter ''^P'' - the column '//
     :                          'data type is inappropriate '//
     :                          '(programming error).', STATUS )
                  GO TO 999
               END IF

*  If any values were supplied for the column, update the maximum
*  formatted length of any column, and create  the column, storing its
*  CAT identifier in KMCOL.
               IF( KMLEN( I ) .GT. 0 ) THEN
                  MAXLEN = MAX( MAXLEN, LENC )
                  CALL CAT_CNEWS( CI, COLNAM( I ), CATYPE, LENC, ' ',
     :                            ' ', ' ', KMCOL( I ), STATUS )
               END IF

            END DO

*  Scheme 2 - each entry is a vector representing a whole column, with
*  the entry key being the column name.
         ELSE
            SCHEME = 2

*  Get the number of columns described by the KeyMap and check it is
*  not too high.
            NKMCOL = AST_MAPSIZE( KEYMAP, STATUS )
            IF( NKMCOL .GT. MXKEYS .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'N', NKMCOL )
               CALL MSG_SETI( 'M', MXKEYS )
               CALL MSG_SETC( 'P', PARAM )
               CALL ERR_REP( ' ', 'Cannot create ^N extra columns in '//
     :                       'the catalogue associated with parameter'//
     :                       ' ''^P'' - no more than ^M are allowed '//
     :                       '(programming error).', STATUS )
               GO TO 999
            END IF

*  Loop round all columns described by the KeyMap.
            DO I = 1, NKMCOL

*  Get the entry key and store it as the column name.
               KEY = AST_MAPKEY( KEYMAP, I, STATUS )
               COLNAM( I ) = KEY

*  Get the column data type, and convert from AST to CAT.
               TYPE = AST_MAPTYPE( KEYMAP, KEY, STATUS )

               IF( TYPE .EQ . AST__INTTYPE ) THEN
                  CATYPE = CAT__TYPEI

               ELSE IF( TYPE .EQ . AST__DOUBLETYPE ) THEN
                  CATYPE = CAT__TYPED

               ELSE IF( TYPE .EQ . AST__FLOATTYPE ) THEN
                  CATYPE = CAT__TYPER

               ELSE IF( TYPE .EQ . AST__STRINGTYPE ) THEN
                  CATYPE = CAT__TYPEC

*  Report an error for complex data types.
               ELSE IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'K', KEY )
                  CALL MSG_SETC( 'P', PARAM )
                  CALL ERR_REP( ' ', 'Cannot create the ''^K'' column'//
     :                          ' within the catalogue associated '//
     :                          'with parameter ''^P'' - the column '//
     :                          'data type is inappropriate '//
     :                          '(programming error).', STATUS )
                  GO TO 999
               END IF

*  Get the maximum formatted length of any element in this column, and
*  update the maximum formatted length of any column.
               LENC = AST_MAPLENC( KEYMAP, KEY, STATUS )
               MAXLEN = MAX( MAXLEN, LENC )

*  Create the column, storing its CAT identifier in KMCOL.
               CALL CAT_CNEWS( CI, COLNAM( I ), CATYPE, LENC, ' ', ' ',
     :                         ' ', KMCOL( I ), STATUS )

*  Set the maximum number of values in the column.
               KMLEN( I ) = AST_MAPLENGTH( KEYMAP, KEY, STATUS )

            END DO
         END IF
      END IF

*  Allocate memory to hold a string that is large enough for the longest
*  formatted value in the KeyMap.
      IF( MAXLEN .GT. 0 ) CALL PSX_CALLOC( MAXLEN, '_CHAR', IPTEXT,
     :                                     STATUS )

*  Ensure we can use the IPTEXT pointer safely.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each supplied position.
      DO I = 1, NPOS

*  Store column values in the current row buffer...

*  The integer identifier.
         IF( ID0 .GT. 0 ) THEN
            CALL CAT_PUT0I( COLID( 0 ), ID0 + I - 1, .FALSE., STATUS )
         ELSE
            CALL CAT_PUT0I( COLID( 0 ), IDENTS( I ), .FALSE., STATUS )
         END IF

*  The label.
         IF( LABS .NE. GRP__NOID ) THEN
            IF( I .LE. NLAB ) THEN
               CALL GRP_GET( LABS, I, 1, LABEL, STATUS )
            ELSE
               LABEL = ' '
            END IF
            CALL CAT_PUT0C( COLID( -1 ), LABEL,
     :                      ( CHR_LEN( LABEL ) .EQ. 0 ), STATUS )
         END IF

*  Normalise the position.
         DO J = 1, NAX
            C( J ) = POS( I, J )
         END DO
         CALL AST_NORM( FRM, C, STATUS )

*  Put each normalised axis value into the current row buffer.
         DO J = 1, NAX
            CALL CAT_PUT0D( COLID( J ), C( J ),
     :                      ( C( J ) .EQ. AST__BAD ), STATUS )
         END DO

*  Loop round each extra column supplied in the KeyMap, skipping empty
*  columns.
         IF( MAXLEN .GT. 0 ) THEN
            DO J = 1, NKMCOL
               IF( KMLEN( J ) .GT. 0 ) THEN

*  If the KeyMap uses the scheme based on scalar-values...
                  IF( SCHEME .EQ. 1 ) THEN

*  Form the key for the entry holding the column value for the current
*  row.
                     KEY = COLNAM( J )
                     IAT = CHR_LEN( KEY )
                     CALL CHR_APPND( '_', KEY, IAT )
                     CALL CHR_PUTI( I, KEY, IAT )

*  Atemmpt to get the formatted value from the KeyMap.
                     GOTVAL = AST_MAPGET0C( KEYMAP, KEY,
     :                                    %VAL( CNF_PVAL( IPTEXT ) ),
     :                                    VLEN, STATUS, %VAL(LEN(KEY)),
     :                                    %VAL(MAXLEN) )

*  If the KeyMap uses the scheme based on vector-values...
                  ELSE

*  The key is just the column name.
                     KEY = COLNAM( J )

*  If the current row is within the bounds of the vector, get the
*  formatted value of the vector element corresponding to the current
*  row.
                     IF( I .LE. KMLEN( J ) ) THEN
                        GOTVAL = AST_MAPGETELEMC( KEYMAP, KEY, I,
     :                                       %VAL( CNF_PVAL( IPTEXT ) ),
     :                                       STATUS, %VAL( LEN( KEY ) ),
     :                                       %VAL( MAXLEN ) )
                     ELSE
                        GOTVAL = .FALSE.
                     END IF
                  END IF

*  If no value was obtained, fill the value with spaces.
                  IF( .NOT. GOTVAL ) CALL CHR_FILL( ' ',
     :                                       %VAL( CNF_PVAL(IPTEXT) ),
     :                                       %VAL( 1 ), %VAL( MAXLEN ) )

*  Store the value.
                  CALL CAT_PUT0C( KMCOL( J ), %VAL( CNF_PVAL(IPTEXT) ),
     :                            .NOT. GOTVAL, STATUS, %VAL( MAXLEN ) )
               END IF
            END DO
         END IF

*  Append the current row buffer to the catalogue.
         CALL CAT_RAPND( CI, STATUS )

      END DO

*  Now dump the FrameSet if one was supplied.
      IF( LWCS .NE. AST__NULL ) THEN

*  Add a header to the textual information.
         CALL CAT_PUTXT( CI, 'COMMENT', ' ', STATUS )
         CALL CAT_PUTXT( CI, 'COMMENT', '      Co-ordinate system '//
     :                   'information follows, stored ', STATUS )
         CALL CAT_PUTXT( CI, 'COMMENT', '      as an AST FrameSet '//
     :                   '(see Starlink User Note 210).', STATUS )

         IF( NAX .EQ. 1 ) THEN
            BUFFER = '      The axis value stored in column 2 '
            IAT = 40
         ELSE
            BUFFER = '      The axis values stored in columns 2 to '
            IAT = 45
            CALL CHR_PUTI( NAX + 1, BUFFER, IAT )
            IAT = IAT + 1
         END IF
         CALL CHR_APPND( 'of the', BUFFER, IAT )

         CALL CAT_PUTXT( CI, 'COMMENT', BUFFER( : IAT ), STATUS )
         CALL CAT_PUTXT( CI, 'COMMENT', '      table refer to the '//
     :                   'Base Frame within this FrameSet.', STATUS )
         CALL CAT_PUTXT( CI, 'COMMENT', ' ', STATUS )

*  Write out the WCS information.
         CALL KPG1_WCATW( LWCS, CI, STATUS )

      END IF

*  Now dump the history text if any was supplied.
      IF( HIST .NE. GRP__NOID ) THEN
         CALL CAT_PUTXT( CI, 'HISTORY', ' ', STATUS )
         CALL GRP_GRPSZ( HIST, NHIST, STATUS )
         DO I = 1, NHIST
            CALL GRP_GET( HIST, I, 1, HTEXT, STATUS )
            CALL CAT_PUTXT( CI, 'HISTORY', HTEXT( : CHR_LEN( HTEXT ) ),
     :                      STATUS )
         END DO
         CALL CAT_PUTXT( CI, 'HISTORY', ' ', STATUS )
      END IF

 999  CONTINUE

*  Free work space.
      IF( MAXLEN .GT. 0 ) CALL PSX_FREE( IPTEXT, STATUS )

*  Release the catalogue.
      CALL CAT_TRLSE( CI, STATUS )

*  Emnd the AST Context.
      CALL AST_END( STATUS )

      END
