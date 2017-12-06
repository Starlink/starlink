      SUBROUTINE KPS1_LSHPL( IPLOT, NPOS, NAX, POS, PLOT, KEYMAP, GEO,
     :                       IMARK, CLOSE, LABTYP, IGRP, IGRP2, JUST,
     :                       IDS, WORK, STATUS )
*+
*  Name:
*     KPS1_LSHFM

*  Purpose:
*     Plot the positions selected by LISTSHOW.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LSHPL( IPLOT, NPOS, NAX, POS, PLOT, KEYMAP, GEO, IMARK,
*                      CLOSE, LABTYP, IGRP, IGRP2, JUST, IDS, WORK,
*                      STATUS )

*  Description:
*     This routine plots the supplied positions on the currently
*     opened PGPLOT device, using one of several different methods.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        An AST Pointer to a Plot to perform the plotting.
*     NPOS = INTEGER (Given)
*        The number of supplied positions.
*     NAX = INTEGER (Given)
*        The number of axes for the supplied positions.
*     POS( NPOS, NAX ) = DOUBLE PRECISION (Given)
*        The supplied positions (in the Current Frame of IPLOT).
*     PLOT = CHARACTER * ( * ) (Given)
*        The type of plotting required (see KPG1_MKPOS).
*     KEYMAP = INTEGER (Given)
*        An AST KeyMap holding STC-S descriptions to be plotted, or
*        AST__NULL if we are not plotting STC-S shapes.
*     GEO = LOGICAL (Given)
*        Should geodesic polygons be drawn?
*     IMARK = INTEGER (Given)
*        PGPLOT marker type.
*     CLOSE = LOGICAL (Given)
*        Should polygons be closed?
*     LABTYP = CHARACTER * ( * ) (Given)
*        Type of labels to be produced: 'ID', 'LABEL' or 'NONE'
*     IGRP = INTEGER (Given)
*        A GRP group holding the strings to use if PLOT=TEXT.
*     IGRP2 = INTEGER (Given)
*        A GRP group holding the strings to use if LABTYP=LABEL.
*     JUST = CHARACTER * ( * ) (Given)
*        A string specifying the justification to be used when displaying
*        the text supplied in IGRP (ignored if PLOT is not "Text"). This
*        should be a string of two characters; the first should be "B",
*        "C" or "T", meaning bottom, centre or top. The second should be
*        "L", "C" or "R", meaning left, centre or right. The text is
*        displayed so that the position supplied in POS is at the
*        specified point within the displayed text string.
*     IDS( NPOS ) = INTEGER (Given)
*        Array of position identifiers.
*     WORK( NPOS, 2 ) = DOUBLE PRECISION (Given and Returned)
*        Work space.
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-SEP-1998 (DSB):
*        Original version.
*     11-NOV-2005 (DSB):
*        Allow up to 50 axes (this allows tables such as those produced by
*        CUPID:CLUMPS which have more than NDF__MXDIM columns to be
*        displayed).
*     21-NOV-2006 (DSB):
*        Added LABTYP and IGRP2 arguments.
*     3-MAY-2009 (DSB):
*        Added KEYMAP argument.
*     6-DEC-2017 (DSB):
*        Correct selection of STCS shape to display for a given index.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Global Variables:
      CHARACTER STCS*500         ! Used to pass info to kpg1_lshsc
      COMMON /KPG1_LSHPL/ STCS

*  Arguments Given:
      INTEGER IPLOT
      INTEGER NPOS
      INTEGER NAX
      DOUBLE PRECISION POS( NPOS, NAX )
      CHARACTER PLOT*(*)
      INTEGER KEYMAP
      LOGICAL GEO
      INTEGER IMARK
      LOGICAL CLOSE
      CHARACTER LABTYP*(*)
      INTEGER IGRP
      INTEGER IGRP2
      CHARACTER JUST*(*)
      INTEGER IDS( NPOS )

*  Arguments Given and Returned:
      DOUBLE PRECISION WORK( NPOS, 2 )

*  Status:
      INTEGER STATUS               ! Global status

*  External References:
      EXTERNAL KPS1_LSHSC
      INTEGER CHR_LEN

*  Local Constants:
      INTEGER MXDIM
      PARAMETER (MXDIM = 50)

*  Local Variables:
      CHARACTER KEY*30             ! Key for STCS description
      CHARACTER TEXT*(GRP__SZNAM)  ! Marker or label text
      DOUBLE PRECISION DX          ! X position offset to label centre
      DOUBLE PRECISION DY          ! Y position offset to label centre
      DOUBLE PRECISION LPOS( MXDIM )! Local copy of a position
      DOUBLE PRECISION NLG1        ! NumLabGap(1)
      DOUBLE PRECISION NLG2        ! NumLabGap(2)
      DOUBLE PRECISION SIZE        ! Size for numerical labels
      DOUBLE PRECISION START( MXDIM )! Start of closing curve
      DOUBLE PRECISION SZ0         ! Original size for strings
      DOUBLE PRECISION WD0         ! Original width for strings
      INTEGER CHAN                 ! StcsChan pointer
      INTEGER CL0                  ! Original colour index for strings
      INTEGER FN0                  ! Original font for strings
      INTEGER I                    ! Loop count
      INTEGER IAT                  ! No. of characters in a string
      INTEGER IAT0                 ! Length of key root string
      INTEGER ICURR                ! Index of new current frame
      INTEGER ICURR0               ! Index of original current frame
      INTEGER J                    ! Axis index
      INTEGER LSTCS                ! Length of STCS string
      INTEGER NSTR                 ! Number of marker strings supplied
      INTEGER REGION               ! STCS Region pointer
      INTEGER ST0                  ! Original style for strings
      REAL UP(2)                   ! Up vector
      REAL X1, X2, Y1, Y2          ! Bounds of PGPLOT window (millimetres)
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Simplify the Plot. This adds a new Current Frame into the Plot, so note
*  the index of the original Current Frame so that it can be re-instated later.
*  This can help to speed up the drawing, and also avoids the possibility
*  of the Mapping going via a Frame in which the positions are undefined.
      ICURR0 = AST_GETI( IPLOT, 'CURRENT', STATUS )
      CALL KPG1_ASSIM( IPLOT, STATUS )

*  Save the index of the new Current Frame.
      ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )

*  Draw labels first so that they do not obscure the marks.
      IF( LABTYP .NE. 'NONE' .AND. STATUS .EQ. SAI__OK ) THEN

*  Map the supplied positions into the GRAPHICS Frame.
         CALL AST_TRANN( IPLOT, NPOS, NAX, NPOS, POS, .FALSE., 2,
     :                   NPOS, WORK, STATUS )

*  Make the GRAPHICS (Base) Frame the Current Frame.
         CALL AST_SETI( IPLOT, 'CURRENT', AST_GETI( IPLOT, 'BASE',
     :                                              STATUS ),
     :                  STATUS )

*  Get the size of the PGPLOT window.
         CALL PGQWIN( X1, X2, Y1, Y2 )

*  Get the value of the NumLabGap value for each axis.
         NLG1 = AST_GETD( IPLOT, 'NUMLABGAP(1)', STATUS )
         NLG2 = AST_GETD( IPLOT, 'NUMLABGAP(2)', STATUS )

*  Get the scale factor for the size of numerical labels.
         SIZE = AST_GETD( IPLOT, 'SIZE(NUMLAB)', STATUS )

*  Find the offset in graphical co-ordinates from each position to the
*  centre of the label.
         DX = ABS( X2 - X1 )*NLG1*SIZE
         DY = ABS( Y2 - Y1 )*NLG2*SIZE

*  Draw text horizontally.
         UP( 1 ) = 0.0
         UP( 2 ) = 1.0

*  Temporarily set the attributes for text strings to be like Numerical
*  Labels.
         SZ0 = AST_GETD( IPLOT, 'SIZE(STRINGS)', STATUS )
         CL0 = AST_GETI( IPLOT, 'COLOUR(STRINGS)', STATUS )
         WD0 = AST_GETD( IPLOT, 'WIDTH(STRINGS)', STATUS )
         ST0 = AST_GETI( IPLOT, 'STYLE(STRINGS)', STATUS )
         FN0 = AST_GETI( IPLOT, 'FONT(STRINGS)', STATUS )

         CALL AST_SETD( IPLOT, 'SIZE(STRINGS)',
     :                  AST_GETD( IPLOT, 'SIZE(NUMLAB)', STATUS ),
     :                  STATUS )
         CALL AST_SETI( IPLOT, 'COLOUR(STRINGS)',
     :                  AST_GETI( IPLOT, 'COLOUR(NUMLAB)', STATUS ),
     :                  STATUS )
         CALL AST_SETD( IPLOT, 'WIDTH(STRINGS)',
     :                  AST_GETD( IPLOT, 'WIDTH(NUMLAB)', STATUS ),
     :                  STATUS )
         CALL AST_SETI( IPLOT, 'STYLE(STRINGS)',
     :                  AST_GETI( IPLOT, 'STYLE(NUMLAB)', STATUS ),
     :                  STATUS )
         CALL AST_SETI( IPLOT, 'FONT(STRINGS)',
     :                  AST_GETI( IPLOT, 'FONT(NUMLAB)', STATUS ),
     :                  STATUS )

*  Loop round each graphical co-ordinate position.
         DO J = 1, NPOS

*  Copy the co-ordinate values into an axis order array.
            DO I = 1, 2
               START( I ) = WORK( J, I )
            END DO

*  If the position is good...
            IF( START( 1 ) .NE. AST__BAD .AND.
     :          START( 2 ) .NE. AST__BAD ) THEN

*  Format the position identifier or get the label text.
               IF( LABTYP .EQ. 'ID' ) THEN
                  TEXT = ' '
                  IAT = 0
                  CALL CHR_PUTI( IDS( J ), TEXT, IAT )
               ELSE
                  CALL GRP_GET( IGRP2, J, 1, TEXT, STATUS )
                  IAT = CHR_LEN( TEXT )
               END IF

*  Determine the position for the centre of the label.
               START( 1 ) = START( 1 ) - DX
               START( 2 ) = START( 2 ) - DY

*  Draw the text.
               CALL AST_TEXT( IPLOT, TEXT( : IAT ), START, UP, 'CC',
     :                        STATUS )

            END IF

         END DO

*  Reinstate the original STRINGS attributes.
         CALL AST_SETD( IPLOT, 'SIZE(STRINGS)', SZ0, STATUS )
         CALL AST_SETI( IPLOT, 'COLOUR(STRINGS)', CL0, STATUS )
         CALL AST_SETD( IPLOT, 'WIDTH(STRINGS)', WD0, STATUS )
         CALL AST_SETI( IPLOT, 'STYLE(STRINGS)', ST0, STATUS )
         CALL AST_SETI( IPLOT, 'FONT(STRINGS)', FN0, STATUS )

*  Re-instate the Current Frame.
         CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )

      END IF


*  Get the number of strings supplied in IGRP group.
      IF( IGRP .NE. GRP__NOID ) THEN
         CALL GRP_GRPSZ( IGRP, NSTR, STATUS )
      ELSE
         NSTR = 0
      END IF

*  If STC-S shapes are being plotted, create an StcsChan to convert the
*  STC-S descriptions into AST Regions. Also get the column name containing
*  the STC-S description. This is used as the root name for the KeyMap
*  entries containing the STC-S descriptions.
      IF( KEYMAP .NE. AST__NULL ) THEN
         CHAN = AST_STCSCHAN( KPS1_LSHSC, AST_NULL, ' ', STATUS )
         IF( .NOT. AST_MAPGETELEMC( KEYMAP, 'COLNAMES', 1, KEY,
     :                              STATUS ) ) KEY = 'SHAPE'
         IAT0 = CHR_LEN( KEY )
         CALL CHR_APPND( '_', KEY, IAT0 )
      ELSE
         CHAN = AST__NULL
      END IF

*  Loop round each position.
      DO I = 1, NPOS

*  Get the axis values at the position.
         DO J = 1, NAX
            LPOS( J ) = POS( I, J )
         END DO

*  If STC-S shapes are being plotted, get the STC-S description for this
*  position form the supplied KeyMap and then convert it to an AST Region
*  using the StcsChan created above.
         IF( CHAN .NE. AST__NULL ) THEN
            IAT = IAT0
            CALL CHR_PUTI( IDS( I ), KEY, IAT )
            CALL CHR_UCASE( KEY( : IAT ) )
            IF( AST_MAPGET0C( KEYMAP, KEY( : IAT ), STCS, LSTCS,
     :                        STATUS ) ) THEN
               REGION = AST_READ( CHAN, STATUS )
            ELSE
               REGION = AST__NULL
            END IF
         ELSE
            REGION = AST__NULL
         END IF

*  If text is being used to mark each position, extract the string for
*  this position from the GRP group. If no group was supplied, or if the
*  group has been exhausted, format the position index.
         IF( PLOT .EQ. 'TEXT' ) THEN
            IF( I .LE. NSTR ) THEN
               CALL GRP_GET( IGRP, I, 1, TEXT, STATUS )
            ELSE
               TEXT = ' '
               IAT = 0
               CALL CHR_PUTI( I, TEXT, IAT )
            END IF
         END IF

*  Draw the position.
         CALL KPG1_MKPOS( NAX, LPOS, IPLOT, .TRUE., PLOT, IMARK, GEO,
     :                    .FALSE., CLOSE, TEXT, JUST, REGION, STATUS )

*  For efficiency, annul any Region pointer.
         IF( REGION .NE. AST__NULL ) CALL AST_ANNUL( REGION, STATUS )
      END DO

*  Complete any polygons.
      CALL KPG1_MKPOS( NAX, LPOS, IPLOT, .TRUE., PLOT, IMARK, GEO,
     :                 .TRUE., CLOSE, TEXT, JUST, AST__NULL, STATUS )

*  Remove the Current Frame added by KPG1_ASSIM and re-instate the original
*  Current Frame.
      CALL AST_REMOVEFRAME( IPLOT, AST__CURRENT, STATUS )
      CALL AST_SETI( IPLOT, 'CURRENT', ICURR0, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END






      SUBROUTINE KPS1_LSHSC( status )

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      CHARACTER STCS*500         ! Used to communicate with kpg1_lshpl
      COMMON /KPG1_LSHPL/ STCS

*  Status:
      INTEGER STATUS               ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      INTEGER L
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the used length of the STCS description. If a previous invocation
*  of this routine has set it blank, use a negative length to tell the
*  StcsChan that there is no more source text.
      L = CHR_LEN( STCS )
      IF( L .EQ. 0 ) L = -1

*  Send the text to the StcsChan.
      CALL AST_PUTLINE( STCS, L, STATUS )

*  Set the text blank to indicate it has been read.
      STCS = ' '

      END


