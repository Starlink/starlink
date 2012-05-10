      SUBROUTINE KPG1_RDCAT( PARAM, CURFRM, KEYMAP, LABS, IWCS, NPOS,
     :                       NAX, IPPOS, IPID, TITLE, NAME, STATUS )
*+
*  Name:
*     KPG1_RDCAT

*  Purpose:
*     Reads a set of positions with labels from a CAT catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_RDCAT( PARAM, CURFRM, KEYMAP, LABS, IWCS, NPOS, NAX,
*                      IPPOS, IPID, TITLE, NAME, STATUS )

*  Description:
*     This routine is equivalent to KPG1_RDTAB except that the values of
*     an arbitrary set of columns (including character-valued columns)
*     can be returned within an AST KeyMap (see KEYMAP).
*
*     See KPG1_RDTAB for further information.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     CURFRM = LOGICAL (Given)
*        If .TRUE. the positions read from the catalogue are Mapped
*        into the Current Frame of the associated FrameSet before being
*        returned. Otherwise, they are returned in the Base Frame.
*     KEYMAP = INTEGER (Given)
*        An AST pointer to an existing KeyMap, or AST__NULL. If a KeyMap
*        is supplied, it should contain a vector valued entry called
*        "COLNAMES" containing the names of one or more catalogue columns
*        to be returned in the KeyMap. On exit, the KeyMap will contain
*        the column values within a set of scalar entries. Each such
*        entry will have a key of the form "<colname>_<row number>"
*        ("_1" for the first row). An error will be reported if the
*        catalogue does not contain the requested columns.
*     LABS = INTEGER (Given and Returned)
*        A GRP identifier for a group containing the values in the LABEL
*        column. If the catalogue contains a LABEL column, then its values
*        are appended to the end of the supplied group. If LABS holds
*        GRP__NOID on entry, then a new GRP group is created and its
*        identifier returned in LABS, but only if the catalogue contains
*        a LABEL column (otherwise the supplied value of GRP__NOID is
*        retained on exit).
*     IWCS = INTEGER (Returned)
*        An AST pointer to the FrameSet read from the catalogue.
*     NPOS = INTEGER (Returned)
*        The number of positions returned.
*     NAX = INTEGER (Returned)
*        The number of axes in the Frame requested by CURFRM.
*     IPPOS = INTEGER (Returned)
*        A pointer to a two-dimensional DOUBLE PRECISION array holding the
*        returned positions. Element (I,J) of this array gives axis J for
*        position I. The first axis will have NPOS elements, and the
*        second will have NAX elements. Should be released using PSX_FREE
*        when no longer needed.
*     IPID = INTEGER (Returned)
*        A pointer to a one-dimensional INTEGER array holding the integer
*        identifiers for the returned positions. The array will have NPOS
*        elements. Should be released using PSX_FREE when no longer needed.
*     TITLE = CHARACTER * ( * ) (Returned)
*        The value of the TITLE parameter in the supplied catalogue.
*        Returned blank if there is no TITLE parameter.
*     NAME = CHARACTER * ( * ) (Returned)
*        The file spec of the catalogue containing the positions list.
*        Not accessed if the declared length is 1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998, 2001, 2004 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     26-OCT-1998 (DSB):
*        Original version.
*     10-DEC-2001 (DSB):
*        Modified to use a default FrameSet if he catalogue does not
*        contain a FrameSet.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL
*     20-NOV-2006 (DSB):
*        Renamed from kpg1_rdlst to kpg1_rdtab, and added argument LABS.
*     4-MAY-2009 (DSB):
*        Renamed from kpg1_rdtab to kpg1_rdcat, and added argument KEYMAP.
*     12-OCT-2009 (DSB):
*        Allow Symbol attribute values to be longer.
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
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER PARAM*(*)
      LOGICAL CURFRM
      INTEGER KEYMAP

*  Arguments Given and Returned:
      INTEGER LABS

*  Arguments Returned:
      INTEGER IWCS
      INTEGER NPOS
      INTEGER NAX
      INTEGER IPPOS
      INTEGER IPID
      CHARACTER TITLE*(*)
      CHARACTER NAME*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER MXDIM              ! Max number of table columns
      PARAMETER ( MXDIM = 50 )

*  Local Variables:
      CHARACTER ATTR*10          ! Attribute name
      CHARACTER CNAME*128        ! Catalogue name
      CHARACTER COLNAM*128       ! Column name
      CHARACTER EPOCH*40         ! Epoch string
      CHARACTER EQN*40           ! Equinox string
      CHARACTER SYM*80           ! Axis symbol
      INTEGER CI                 ! Catalogue identifier
      INTEGER DTYPE              ! Data type identifier
      INTEGER FRM                ! Frame pointer
      INTEGER GAXIS( MXDIM )     ! CAT identifiers for axis columns
      INTEGER GID                ! CAT identifier for PIDENT column
      INTEGER GLAB               ! CAT identifier for LABEL column
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
      INTEGER NCNAM              ! Number of column names
      INTEGER NDIM               ! No of axes
      LOGICAL DONE               ! Have we read enough AST Objects?
      LOGICAL GOTRD              ! Found RA and DEC columns in catalogue?
      LOGICAL GOTXY              ! Found X and Y columns in catalogue?
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

*  If required return the catalogue name.
      IF( LEN( NAME ) .GT. 1 ) NAME = CNAME

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

*  If no FrameSet was found, we may be able to create a suitable FrameSet
*  by guessing at the column names in the catalogue.
      IF( IWCS .EQ. AST__NULL ) THEN

*  Look for a floating point catalogue column with the name "RA".
         GOTRD = .FALSE.
         CALL CAT_TIDNT( CI, 'RA', GAXIS( 1 ), STATUS )
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL CAT_TIQAI( GAXIS( 1 ), 'DTYPE', DTYPE, STATUS )
            IF( DTYPE .EQ. CAT__TYPER .OR. DTYPE .EQ. CAT__TYPED ) THEN
               GOTRD = .TRUE.
            END IF
         ELSE IF( STATUS .EQ. CAT__NOCMP ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF

*  If found, look for a floating point catalogue column with the name "DEC".
         IF( GOTRD ) THEN
            GOTRD = .FALSE.
            CALL CAT_TIDNT( CI, 'DEC', GAXIS( 1 ), STATUS )
            IF( STATUS .EQ. SAI__OK ) THEN
               CALL CAT_TIQAI( GAXIS( 1 ), 'DTYPE', DTYPE, STATUS )
               IF( DTYPE .EQ. CAT__TYPER .OR.
     :             DTYPE .EQ. CAT__TYPED ) THEN
                  GOTRD = .TRUE.
               END IF
            ELSE IF( STATUS .EQ. CAT__NOCMP ) THEN
               CALL ERR_ANNUL( STATUS )
            END IF
         END IF

*  If both RA and DEC columns were found, create a FrameSet holding a
*  single SkyFrame.
         IF( GOTRD ) THEN
            IWCS = AST_FRAMESET( AST_SKYFRAME( ' ', STATUS ), ' ',
     :                           STATUS )

*  Ensure the axis symbols are RA and DEC since this is assumed later on.
            CALL AST_SETC( IWCS, 'SYMBOL(1)', 'RA', STATUS )
            CALL AST_SETC( IWCS, 'SYMBOL(2)', 'DEC', STATUS )

*  If there is an EPOCH parameter in the catalogue, use it to set the
*  Epoch attribute of the SkyFrame.
            CALL CAT_TIDNT( CI, 'EPOCH', GAXIS( 1 ), STATUS )
            IF( STATUS .EQ. SAI__OK ) THEN
               CALL CAT_TIQAC( GAXIS( 1 ), 'VALUE', EPOCH, STATUS )
               CALL AST_SETC( IWCS, 'EPOCH', EPOCH, STATUS )
            ELSE IF( STATUS .EQ. CAT__NOCMP ) THEN
               CALL ERR_ANNUL( STATUS )
            END IF

*  If there is an EQUINOX parameter in the catalogue, use it to set the
*  Equinox attribute of the SkyFrame.
            CALL CAT_TIDNT( CI, 'EQUINOX', GAXIS( 1 ), STATUS )
            IF( STATUS .EQ. SAI__OK ) THEN
               CALL CAT_TIQAC( GAXIS( 1 ), 'VALUE', EQN, STATUS )
               CALL AST_SETC( IWCS, 'EQUINOX', EQN, STATUS )
            ELSE IF( STATUS .EQ. CAT__NOCMP ) THEN
               CALL ERR_ANNUL( STATUS )
            END IF

*  If no RA/DEC columns found, look for X/Y columns and assume they are GRID
*  coords.
         ELSE

*  Look for a floating point catalogue column with the name "X".
            GOTXY = .FALSE.
            CALL CAT_TIDNT( CI, 'X', GAXIS( 1 ), STATUS )
            IF( STATUS .EQ. SAI__OK ) THEN
               CALL CAT_TIQAI( GAXIS( 1 ), 'DTYPE', DTYPE, STATUS )
               IF( DTYPE .EQ. CAT__TYPER .OR.
     :             DTYPE .EQ. CAT__TYPED ) THEN
                  GOTXY = .TRUE.
               END IF
            ELSE IF( STATUS .EQ. CAT__NOCMP ) THEN
               CALL ERR_ANNUL( STATUS )
            END IF

*  If found, look for a floating point catalogue column with the name "Y".
            IF( GOTXY ) THEN
               GOTXY = .FALSE.
               CALL CAT_TIDNT( CI, 'Y', GAXIS( 1 ), STATUS )
               IF( STATUS .EQ. SAI__OK ) THEN
                  CALL CAT_TIQAI( GAXIS( 1 ), 'DTYPE', DTYPE, STATUS )
                  IF( DTYPE .EQ. CAT__TYPER .OR.
     :                DTYPE .EQ. CAT__TYPED ) THEN
                     GOTXY = .TRUE.
                  END IF
               ELSE IF( STATUS .EQ. CAT__NOCMP ) THEN
                  CALL ERR_ANNUL( STATUS )
               END IF
            END IF

*  If both X and Y columns were found, create a FrameSet holding a
*  single Frame.
            IF( GOTXY ) THEN

*  See if thre is a Z column, if so make the Frame three-dimensional.
               NDIM = 2
               CALL CAT_TIDNT( CI, 'Z', GAXIS( 1 ), STATUS )
               IF( STATUS .EQ. SAI__OK ) THEN
                  CALL CAT_TIQAI( GAXIS( 1 ), 'DTYPE', DTYPE, STATUS )
                  IF( DTYPE .EQ. CAT__TYPER .OR.
     :                DTYPE .EQ. CAT__TYPED ) THEN
                     NDIM = 3
                  END IF
               ELSE IF( STATUS .EQ. CAT__NOCMP ) THEN
                  CALL ERR_ANNUL( STATUS )
               END IF

*  Create the Frame with Domain GRID.
               IWCS = AST_FRAMESET( AST_FRAME( NDIM, 'DOMAIN=GRID',
     :                                         STATUS ),
     :                              ' ', STATUS )

*  Set the Symbols to X,Y,Z since this is assumed later on.
               CALL AST_SETC( IWCS, 'SYMBOL(1)', 'X', STATUS )
               CALL AST_SETC( IWCS, 'SYMBOL(2)', 'Y', STATUS )
               IF( NDIM .EQ. 3 ) THEN
                  CALL AST_SETC( IWCS, 'SYMBOL(3)', 'Z', STATUS )
               END IF

            END IF

         END IF

      END IF

*  If no FrameSet was found, report an error.
      IF( IWCS .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'CAT', CNAME )
         CALL ERR_REP( 'KPG1_RDCAT_ERR1', 'Supplied catalogue '//
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
         CALL ERR_REP( 'KPG1_RDCAT_ERR2', 'Could not find columns in '//
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
      CALL KPG1_CTCPD( CI, NAX, GAXIS, NPOS, %VAL( CNF_PVAL( IPPOS ) ),
     :                 STATUS )

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
         CALL AST_TRANN( MAP, NPOS, NAXCAT, NPOS,
     :                   %VAL( CNF_PVAL( IPCAT ) ), .TRUE.,
     :                   NAX, NPOS, %VAL( CNF_PVAL( IPPOS ) ), STATUS )

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
            CALL MSG_OUT( 'KPG1_RDCAT_MSG1', 'WARNING: The position '//
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
      CALL KPG1_CTCPI( CI, 1, GID, NPOS, %VAL( CNF_PVAL( IPID ) ),
     :                 STATUS )

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

*  See if there is column with name "LABEL" in the catalogue. If found, this
*  column is assumed to hold position labels.
      CALL CAT_TIDNT( CI, 'LABEL', GLAB, STATUS )

*  If it exists, append the column values to the supplied GRP group (a new
*  group is created if necessary).
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL KPG1_CTCPC( CI, GLAB, NPOS, LABS, STATUS )

*  If the LABEL component was not found, annul the error.
      ELSE IF( STATUS .EQ. CAT__NOCMP ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  Now retrieve values for any columns specified in the COLNAMES entry
*  of the supplied KeyMap.
      IF( KEYMAP .NE. AST__NULL ) THEN

*  Get CAT column identifiers for all column names listed in the COLNAMES
*  entry.
         NCNAM = MIN( MXDIM, AST_MAPLENGTH( KEYMAP, 'COLNAMES',
     :                                      STATUS ) )
         DO J = 1, NCNAM
            IF( AST_MAPGETELEMC( KEYMAP, 'COLNAMES', J, COLNAM,
     :                           STATUS ) ) THEN
               IF( STATUS .EQ. SAI__OK ) THEN
                  CALL CAT_TIDNT( CI, COLNAM, GAXIS( J ), STATUS )
                  IF( STATUS .EQ. CAT__NOCMP ) THEN
                     CALL ERR_ANNUL( STATUS )
                     STATUS = SAI__ERROR
                     CALL MSG_SETC( 'CAT', CNAME )
                     CALL MSG_SETC( 'COL', COLNAM )
                     CALL ERR_REP( ' ', 'Supplied catalogue ''^CAT'' '//
     :                             'does not contains a column called'//
     :                             ' ^COL.', STATUS )
                     GO TO 999
                  END IF
               END IF
            END IF
         END DO

*  Read the columns into the KeyMap.
         CALL KPG1_CTCKM( CI, NCNAM, GAXIS, NPOS, KEYMAP, STATUS )

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
         IF( LEN( NAME ) .GT. 1 ) NAME = ' '

         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_RDCAT_ERR3', 'Error reading a positions '//
     :                 'list using parameter %^PARAM.', STATUS )

      END IF

*  Release the catalogue identifier.
      CALL CAT_TRLSE( CI, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
