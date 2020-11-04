      SUBROUTINE KPS1_BFHD( CFRM, LOGF, FDL, NAXC, TITLE, STATUS )
*+
*  Name:
*     KPS1_BFHD

*  Purpose:
*     Reports the co-ordinate system the results will appear in.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_BFHD( CFRM, LOGF, FDL, NAXC, TITLE, STATUS )

*  Description:
*     This routine displays an initial header for BEAMFIT.  It reports
*     the co-ordinate system of the centre and FWHM before BEAMFIT
*     displays a list of fitted beams.

*  Arguments:
*     CFRM = INTEGER (Given)
*        A pointer to the current Frame of the NDF.  This must have NAXC
*        axes.
*     LOGF = LOGICAL (Given)
*        Should the results be written to a log file?
*     FDL = INTEGER (Given)
*        The file descriptor for the log file.  It is gnored if LOGPOS
*        is .FALSE.
*     NAXC = INTEGER (Given)
*        The number of axes in CFRM.
*     TITLE = CHARACTER * ( * ) (Given)
*        A title to display before the first beam parameters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Particle Physics & Astronomy Research Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 March 12 (MJC):
*        Original version slightly adapted from DSB's KPS1_CENHD.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'MSG_PAR'          ! Message-system constants

*  Arguments Given:
      INTEGER CFRM
      LOGICAL LOGF
      INTEGER FDL
      INTEGER NAXC
      CHARACTER*(*) TITLE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*10 ATTR          ! Name of an AST attribute
      CHARACTER*30 ATTVAL        ! Attribute value
      INTEGER IAT                ! No. of characters currently in buffer
      INTEGER J                  ! Axis index
      INTEGER LATTR              ! Used length of ATTR
      CHARACTER*128 LINE         ! Buffer for output text
      INTEGER NUN                ! Number of axes with non-blank units
      CHARACTER*256 STRING       ! Strings of attribute values

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Any title.
      IF ( TITLE .NE. ' ' ) THEN
         LINE = 'Title: '
         IAT = 7
         CALL CHR_APPND( TITLE, LINE, IAT )

         CALL MSG_OUTIF( MSG__NORM, ' ', LINE( : IAT ), STATUS )
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

         IF ( LOGF ) THEN
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
         IF ( ATTVAL .EQ. ' ' ) ATTVAL = '??'

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

*  If blank, use "??".  Count the number of non-blank units.
         IF ( ATTVAL .EQ. ' ' ) THEN
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
      LINE = '    Co-ordinates    :'
      IAT = 22

*  If any of the axis had non-blank units, append the list of symbols and
*  units to the header.
      IF ( NUN .GT. 0 ) CALL CHR_APPND( STRING, LINE, IAT )

*  Display the header.
      CALL MSG_OUTIF( MSG__NORM, ' ', LINE( : IAT ), STATUS )
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

      IF ( LOGF ) THEN
         CALL FIO_WRITE( FDL, LINE( : IAT ), STATUS )
         CALL FIO_WRITE( FDL, ' ', STATUS )
      END IF

      END
