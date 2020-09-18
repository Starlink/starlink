      SUBROUTINE KPS1_LSHFM( FRM, NPOS, NAX, ID, POS, IGRP1, IGRP2,
     :                       SHOWPV, PIXVALS, STATUS )

*+
*  Name:
*     KPS1_LSHFM

*  Purpose:
*     Format the selected positions to be displayed by LISTSHOW.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LSHFM( FRM, NPOS, NAX, ID, POS, IGRP1, IGRP2,
*                      SHOWPV, PIXVALS, STATUS )

*  Description:
*     This routine formats the supplied positions, identifiers and labels,
*     and stores the resulting strings in the supplied GRP group.

*  Arguments:
*     FRM = INTEGER (Given)
*        An AST Pointer to the Frame in which the positions are defined.
*     NPOS = INTEGER (Given)
*        The number of supplied positions.
*     NAX = INTEGER (Given)
*        The number of axes for the supplied positions.
*     ID( NPOS ) = INTEGER (Given)
*        The supplied position identifiers.
*     POS( NPOS, NAX ) = DOUBLE PRECISION (Given and Returned)
*        The supplied positions. These are normalised using AST_NORM on
*        return.
*     IGRP1 = INTEGER (Given)
*        A GRP identifier for the group containing position labels. Set
*        to GRP__NOID if there are no labels.
*     IGRP2 = INTEGER (Given)
*        A GRP identifier for the group to receive the formatted positions.
*     SHOWPV = LOGICAL (Given)
*        If .TRUE., include the pixel values supplied in the PIXVALS
*        arrays in the returned formatted positions.
*     PIXVALS( NPOS ) = DOUBLE PRECISION (Given)
*        The pixel values at eahc position. Only used if SHOWPV is .TRUE.,
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1998, 2003 Central Laboratory of the Research
*     Councils. Copyright (C) 2005 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

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
*     7-AUG-2003 (DSB):
*        Normalise the supplied positions.
*     11-NOV-2005 (DSB):
*        Allow up to 50 axes (this allows tables such as those produced by
*        CUPID:CLUMPS which have more than NDF__MXDIM columns to be
*        displayed).
*     21-NOV-2006 (DSB):
*        Added IGRP1 argument.
*     18-SEP-2020 (DSB):
*        Added SHOWPV and PIXVALS.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PRM_PAR'          ! VAL constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER FRM
      INTEGER NPOS
      INTEGER NAX
      INTEGER ID( NPOS )
      INTEGER IGRP1
      INTEGER IGRP2
      LOGICAL SHOWPV
      DOUBLE PRECISION PIXVALS( NPOS )

*  Arguments Given and Returned:
      DOUBLE PRECISION POS( NPOS, NAX )

*  Status:
      INTEGER STATUS               ! Global status

*  External References:
      INTEGER CHR_LEN              ! Used length of a string

*  Local Constants:
      INTEGER NSP                  ! No. of spaces between fields
      PARAMETER( NSP = 3 )

      INTEGER MXDIM                ! Max number of axes
      PARAMETER( MXDIM = 50 )

*  Local Variables:
      CHARACTER ATTRIB*15          ! AST attribute name
      CHARACTER LABEL*( GRP__SZNAM )! Label text
      CHARACTER LINE*( GRP__SZNAM )! Buffer for text
      CHARACTER TEXT*40            ! AST attribute value
      DOUBLE PRECISION C( MXDIM )  ! Buffer for a single position
      INTEGER BLEN                 ! Length of each line
      INTEGER I                    ! Axis index
      INTEGER IAT                  ! No. of characters in a string
      INTEGER K                    ! Position index
      INTEGER MXWID( -1 : MXDIM )   ! Field widths
      INTEGER TAB( -1 : MXDIM + 1 ) ! Field starting positions
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the maximum field width on each axis.
*  ==========================================

*  Initialise the maximum field widths.
      DO I = -1, NAX
         MXWID( I ) = 0
      END DO

*  Loop round each position.
      DO K = 1, NPOS

*  Normalise the position.
         DO I = 1, NAX
            C( I ) = POS( K, I )
         END DO
         CALL AST_NORM( FRM, C, STATUS )
         DO I = 1, NAX
            POS( K, I ) = C( I )
         END DO

*  Do each axis.
         DO I = 1, NAX

*  Find the used length of the formatted axis value.
           MXWID( I ) = MAX( MXWID( I ), CHR_LEN(
     :                     AST_FORMAT( FRM, I, POS( K, I ), STATUS ) ) )

         END DO

*  Extend the label field width.
         IF( IGRP1 .NE. GRP__NOID ) THEN
            CALL GRP_GET( IGRP1, K, 1, LABEL, STATUS )
            MXWID( 0 ) = MAX( MXWID( 0 ), CHR_LEN( LABEL ) )
         END IF

      END DO

*  Make sure the field widths are wide enough to hold the axis symbols
*  and units.
      DO I = 1, NAX

         ATTRIB = 'Unit('
         IAT = 5
         CALL CHR_PUTI( I, ATTRIB, IAT )
         CALL CHR_APPND( ')', ATTRIB, IAT )
         TEXT = AST_GETC( FRM, ATTRIB( : IAT ), STATUS )
         CALL KPG1_PGESC( TEXT, STATUS )
         MXWID( I ) = MAX( MXWID( I ), CHR_LEN( TEXT ) )

         ATTRIB = 'Symbol('
         IAT = 7
         CALL CHR_PUTI( I, ATTRIB, IAT )
         CALL CHR_APPND( ')', ATTRIB, IAT )

         TEXT = AST_GETC( FRM, ATTRIB( : IAT ), STATUS )
         CALL KPG1_PGESC( TEXT, STATUS )
         MXWID( I ) = MAX( MXWID( I ), CHR_LEN( TEXT ) )

      END DO

*  Set the tab positions for each column. -1 is for identifiers, 0 for
*  labels. Allow NSP spaces after each column.
      TAB( -1 ) = 1
      MXWID( -1 ) = 10

      IF( IGRP1 .NE. GRP__NOID ) THEN
         TAB( 0 ) = TAB( - 1 ) + MXWID( - 1 ) + NSP
      ELSE
         TAB( 0 ) = TAB( - 1 )
         MXWID( 0 ) = MXWID( -1 )
      END IF

      DO I = 1, NAX + 1
         TAB( I ) = TAB( I - 1 ) + MXWID( I - 1 ) + NSP
      END DO

*  Create the header, putting each word in the middle of the corresponding
*  field...
*  =======================================================================

*  First line: "Position", "Label" (if needed) and axis symbols
      LINE = ' '
      IF( IGRP1. NE. GRP__NOID ) THEN
         IAT = ( TAB( 0 ) + TAB( -1 ) - NSP - 8 )/2
      ELSE
         IAT = ( TAB( 1 ) + TAB( -1 ) - NSP - 8 )/2
      END IF

      LINE( IAT : ) = 'Position'

      IF( IGRP1. NE. GRP__NOID ) THEN
         IAT = ( TAB( 1 ) + TAB( 0 ) - NSP - 5 )/2
         LINE( IAT : ) = 'Label'
      END IF

      DO I = 1, NAX
         ATTRIB = 'Symbol('
         IAT = 7
         CALL CHR_PUTI( I, ATTRIB, IAT )
         CALL CHR_APPND( ')', ATTRIB, IAT )
         TEXT = AST_GETC( FRM, ATTRIB( : IAT ), STATUS )
         CALL KPG1_PGESC( TEXT, STATUS )
         IAT = ( TAB( I + 1 ) + TAB( I ) - NSP - CHR_LEN( TEXT ) )/2
         LINE( IAT : ) = TEXT
      END DO

      IF( SHOWPV ) THEN
         LINE( TAB( NAX + 1 ) : ) = 'Pixel'
      END IF

      CALL GRP_PUT( IGRP2, 1, LINE( : CHR_LEN(LINE) ), 0, STATUS )

*  Second line: "identifier" and axis units (if any).
      LINE = ' '

      IF( IGRP1. NE. GRP__NOID ) THEN
         IAT = ( TAB( 0 ) + TAB( -1 ) - NSP - 10 )/2
      ELSE
         IAT = ( TAB( 1 ) + TAB( -1 ) - NSP - 10 )/2
      END IF

      LINE( IAT : ) = 'identifier'

      DO I = 1, NAX
         ATTRIB = 'Unit('
         IAT = 5
         CALL CHR_PUTI( I, ATTRIB, IAT )
         CALL CHR_APPND( ')', ATTRIB, IAT )
         TEXT = AST_GETC( FRM, ATTRIB( : IAT ), STATUS )
         CALL KPG1_PGESC( TEXT, STATUS )
         IAT = ( TAB( I + 1 ) + TAB( I ) - NSP - CHR_LEN( TEXT ) )/2
         LINE( IAT : ) = TEXT
      END DO

      IF( SHOWPV ) THEN
         LINE( TAB( NAX + 1 ) : ) = 'value'
      END IF

      BLEN = CHR_LEN( LINE )
      CALL GRP_PUT( IGRP2, 1, LINE( : BLEN ), 0, STATUS )

*  Separator lines.
      DO I = 1, BLEN
         LINE( I : I ) = '-'
      END DO
      CALL GRP_PUT( IGRP2, 1, LINE( : BLEN ), 0, STATUS )
      CALL GRP_PUT( IGRP2, 1, ' ', 0, STATUS )

*  Format the identifiers, labels and positions in columns
*  =======================================================

*  Loop round each position.
      DO K = 1, NPOS
         LINE = ' '

*  Put the position identifier into the buffer.
         IAT = TAB( -1 ) + 2
         LINE( IAT : ) = '#'
         CALL CHR_PUTI( ID( K ), LINE, IAT )

*  Put any label into the buffer.
         IF( IGRP1 .NE. GRP__NOID ) THEN
            CALL GRP_GET( IGRP1, K, 1, LABEL, STATUS )
            IAT = TAB( 0 )
            LINE( IAT : ) = LABEL( : MXWID( 0 ) )
         END IF

*  Format the results on each axis. Start each axis value at the tab
*  positions found above.
         DO I = 1, NAX
            LINE( TAB( I ) : ) = AST_FORMAT( FRM, I, POS( K, I ),
     :                                       STATUS )
         END DO

*  Append any pixel value
         IF( SHOWPV ) THEN
            IF( PIXVALS( K ) .NE. VAL__BADD ) THEN
               CALL CHR_DTOC( PIXVALS( K ), TEXT, IAT )
            ELSE
               TEXT = '<BAD>'
            END IF
            LINE( TAB( NAX + 1 ) : ) = TEXT
         END IF

*  Store the buffer in the group.
         CALL GRP_PUT( IGRP2, 1, LINE( : CHR_LEN(LINE) ), 0, STATUS )

      END DO

      END
