      SUBROUTINE CCD1_TROUT( TR, FRAME, USEFRM, STATUS )
*+
*  Name:
*     CCD1_TROUT

*  Purpose:
*     Output linear transformation coefficients in a tabular form.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_TROUT( TR, FRAME, USEFRM, STATUS )

*  Description:
*     This routine prints the six coefficients submitted in the TR array
*     in a suitable form given that they represent the coefficients of
*     a linear transformation:
*
*        X' = TR( 1 ) + TR( 2 ) * X + TR( 3 ) * Y
*        Y' = TR( 4 ) + TR( 5 ) * X + TR( 6 ) * Y
*
*     If USEFRM is .TRUE. then the format of output is as determined
*     by the AST Frame object contained in the FRAME argument.  Otherwise
*     formatting is in some default format.  This only applies to TR( 1 )
*     and TR( 4 ), since the other elements are dimensionless.
*
*     Output is via the CCDPACK logging mechanism.

*  Arguments:
*     TR( 6 ) = DOUBLE PRECISION (Given)
*        The coefficients to be output.
*     FRAME = INTEGER (Given)
*        AST pointer to a frame to be used for formatting A and D values.
*     USEFRM = LOGICAL (Given)
*        If .TRUE. use the FRAME value for formatting, otherwise format
*        in a default style.
*     STATUS = INTEGER (Given)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor (STARLINK - IoA)
*     {enter_new_authors_here}

*  History:
*     17-MAR-1999 (MBT):
*        Original version.
*     1-NOV-1999 (MBT):
*        Modified to format according to AST Frame object.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! Standard MSG/ERR constants
      INCLUDE 'AST_PAR'          ! Standard AST system constants

*  Arguments Given:
      DOUBLE PRECISION TR( 6 )
      INTEGER FRAME
      LOGICAL USEFRM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( MSG__SZMSG ) BUFFER ! Buffer for output
      CHARACTER * ( MSG__SZMSG ) XSTR ! Formatted X coordinate
      CHARACTER * ( MSG__SZMSG ) YSTR ! Formatted Y coordinate
      INTEGER FRM               ! AST pointer to frame to use for formatting
      INTEGER IAT               ! Character position in buffer
      INTEGER NCHAR             ! Number of characters converted

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get formatted first and fourth elements.
      IF ( USEFRM ) THEN
         FRM = FRAME
      ELSE
         FRM = AST__NULL
      END IF
      CALL CCD1_XYFMT( TR( 1 ), TR( 4 ), FRM, XSTR, YSTR, STATUS )

*  Output the coefficients.
      BUFFER = ' '
      IAT = 4
      BUFFER( IAT: ) = ' A ='
      IAT = IAT + 5
      BUFFER( IAT: ) = XSTR
      IAT = 29
      BUFFER( IAT: ) = ' B ='
      IAT = IAT + 5
      CALL CHR_RTOC( REAL( TR( 2 ) ), BUFFER( IAT: ), NCHAR )
      IAT = 54
      BUFFER( IAT: ) = ' C ='
      IAT = IAT + 5
      CALL CHR_RTOC( REAL( TR( 3 ) ), BUFFER( IAT: ), NCHAR )
      CALL CCD1_MSG( ' ', BUFFER, STATUS )
      BUFFER = ' '
      IAT = 4
      BUFFER( IAT: ) = ' D ='
      IAT = IAT + 5
      BUFFER( IAT: ) = YSTR
      IAT = 29
      BUFFER( IAT: ) = ' E ='
      IAT = IAT + 5
      CALL CHR_RTOC( REAL( TR( 5 ) ), BUFFER( IAT: ), NCHAR )
      IAT = 54
      BUFFER( IAT: ) = ' F ='
      IAT = IAT + 5
      CALL CHR_RTOC( REAL( TR( 6 ) ), BUFFER( IAT: ), NCHAR )
      CALL CCD1_MSG( ' ', BUFFER, STATUS )

      END
* $Id$
