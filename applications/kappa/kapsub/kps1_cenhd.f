      SUBROUTINE KPS1_CENHD( CFRM, LOGPOS, FDL, QUIET, NAXC, TITLE,
     :                       STATUS )
*+
*  Name:
*     KPS1_CENHD

*  Purpose:
*     Display a header prior to displaying a list of centroid positions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CENHD( CFRM, LOGPOS, FDL, QUIET, NAXC, TITLE, STATUS )

*  Description:
*     This routine dislays an initial header for CENTROID.

*  Arguments:
*     CFRM = INTEGER (Given)
*        A pointer to the current Frame of the NDF. This must have NAXC
*        axes.
*     LOGPOS = LOGICAL (Given)
*        Should the results be written to a log file?
*     FDL = INTEGER (Given)
*        The file descriptor for the log file. Ignored if LOGPOS is
*        .FALSE.
*     QUIET = INTEGER (Given)
*        If .FALSE., the results are written to standard output. If .TRUE.
*        nothing is written to standard output.
*     NAXC = INTEGER (Given)
*        The number of axes in CFRM.
*     TITLE = CHARACTER * ( * ) (Given)
*        A title to display before the first position.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-JUN-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Arguments Given:
      INTEGER CFRM
      LOGICAL LOGPOS
      INTEGER FDL
      LOGICAL QUIET
      INTEGER NAXC
      CHARACTER TITLE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ATTR*10          ! Name of an AST attribute
      CHARACTER LINE*128         ! Buffer for output text
      CHARACTER ATTVAL*30        ! Attribute value
      CHARACTER STRING*256       ! Strings of attribute values
      INTEGER IAT                ! Number of characters currently in buffer
      INTEGER J                  ! Axis index
      INTEGER LATTR              ! Used length of ATTR
      INTEGER NUN                ! Number of axes with non-blank units
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  A blank line.
      IF( .NOT. QUIET ) CALL MSG_BLANK( STATUS )
      IF( LOGPOS ) CALL FIO_WRITE( FDL, ' ', STATUS )

*  Any title.
      IF( TITLE .NE. ' ' ) THEN
         LINE = 'Title: '
         IAT = 7
         CALL CHR_APPND( TITLE, LINE, IAT )

         IF( .NOT. QUIET ) THEN
            CALL MSG_OUT( ' ', LINE( : IAT ), STATUS )
            CALL MSG_BLANK( STATUS )
         END IF

         IF( LOGPOS ) THEN
            CALL FIO_WRITE( FDL, LINE( : IAT ), STATUS )
            CALL FIO_WRITE( FDL, ' ', STATUS )
         END IF
      END IF

*  We need a string holding a list of the axis symbols and units.
      STRING = ' '
      IAT = 0
      NUN = 0

*  Do each axis.
      DO J = 1, NAXC

*  Form the Symbol attribute name for this axis.
         ATTR = 'SYMBOL('
         LATTR = 7
         CALL CHR_PUTI( J, ATTR, LATTR )
         CALL CHR_APPND( ')', ATTR, LATTR )

*  Get the Symbol value.
         ATTVAL = AST_GETC( CFRM, ATTR( : LATTR ), STATUS )

*  If blank, use "??".
         IF( ATTVAL .EQ. ' ' ) ATTVAL = '??'

*  Append to the string.
         CALL CHR_APPND( ATTVAL, STRING, IAT )

*  Append an opening paranthesis.
         CALL CHR_APPND( '(', STRING, IAT )

*  Form the Unit attribute name for this axis.
         ATTR = 'UNIT('
         LATTR = 5
         CALL CHR_PUTI( J, ATTR, LATTR )
         CALL CHR_APPND( ')', ATTR, LATTR )

*  Get the Unit value.
         ATTVAL = AST_GETC( CFRM, ATTR( : LATTR ), STATUS )

*  If blank, use "??". Count the number of non-blank units.
         IF( ATTVAL .EQ. ' ' ) THEN
            ATTVAL = '??'
         ELSE
            NUN = NUN + 1
         END IF

*  Append it to the list, with a trailing closing parenthesis and space.
         CALL CHR_APPND( ATTVAL, STRING, IAT )
         CALL CHR_APPND( ')', STRING, IAT )
         IAT = IAT + 1

      END DO

*  For the header.
      LINE = '     Centroid positions:'
      IAT = 26

*  If any of the axis had non-blank units, append the list of symbols and
*  units to the header.
      IF( NUN .GT. 0 ) CALL CHR_APPND( STRING, LINE, IAT )

*  Display the header.
      IF( .NOT. QUIET ) THEN
         CALL MSG_OUT( ' ', LINE( : IAT ), STATUS )
         CALL MSG_BLANK( STATUS )
      END IF

      IF( LOGPOS ) THEN
         CALL FIO_WRITE( FDL, LINE( : IAT ), STATUS )
         CALL FIO_WRITE( FDL, ' ', STATUS )
      END IF

      END
