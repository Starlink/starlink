      SUBROUTINE ATL1_PRNTN( NP, NAX, AXVAL, PAR, STATUS )
*+
*  Name:
*     ATL1_PRNTN

*  Purpose:
*     Print a list of N-dimensional position to the screen and optionally
*     to an output file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL1_PRNTN( NP, NAX, AXVAL, PAR, STATUS )

*  Description:
*     The screen output is one position per line, x followed y, etc,
*     separated by spaces. Each output text file has one axis value
*     per line.

*  Arguments:
*     NP = INTEGER (Given)
*        The number of positions.
*     NAX = INTEGER (Given)
*        The number of axis values per position.
*     AXVAL( NP, NAX ) = DOUBLE PRECISION (Given)
*        The axis values.
*     PAR = CHARACTER * ( * ) (Given)
*        The parameter name which gives the name of the text file to
*        receive the axis values. If a null (!) value is obtained no output
*        file is created and the error is annulled. If a blank value is
*        supplied for PAR no output file is created.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001-2006 Particle Physics and Astronomy Research
*     Council. All Rights Reserved.

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
*     22-SEP-2006 (DSB):
*        Original version.
*     4-JUN-2007 (DSB):
*        Check for bad values.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Arguments Given:
      INTEGER NP
      INTEGER NAX
      DOUBLE PRECISION AXVAL( NP, NAX )
      CHARACTER PAR*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER FNAME*255, BUF*500
      INTEGER I, J, IAT, FD, NC
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  List the values to the screen
      BUF = ' '
      DO I = 1, NP
         IAT = 2
         DO J = 1, NAX
            IF( AXVAL( I, J ) .NE. AST__BAD ) THEN
               CALL CHR_PUTD( AXVAL( I, J ), BUF, IAT )
            ELSE
               CALL CHR_APPND( '<bad>', BUF, IAT )
            END IF
            IAT = IAT + 1
         END DO
         CALL MSG_OUT( ' ', BUF( :IAT ), STATUS )
      END DO

* If required, write the values to a text file.
      IF( PAR .NE. ' ' ) THEN

*  Get the name of the output file. If non given, annul the error and
*  pass on.
         CALL PAR_GET0C( PAR, FNAME, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE

*  We delete any pre-existing file first.
            CALL ATL_RM( FNAME, STATUS )

*  Open a new file and get an FIO identifier for it.
            CALL FIO_OPEN( FNAME, 'WRITE', 'LIST', 132, FD, STATUS )

*  Write the values to the file.
            DO J = 1, NAX
               DO I = 1, NP

                  IF( AXVAL( I, J ) .NE. AST__BAD ) THEN
                     CALL CHR_DTOC( AXVAL( I, J ), BUF, NC )
                  ELSE
                     BUF = '<bad>'
                     NC = 5
                  END IF
                  CALL FIO_WRITE( FD, BUF( : NC ), STATUS )
               END DO
            END DO

*  Close the file.
            CALL FIO_ANNUL( FD, STATUS )

         END IF

      END IF

      END
