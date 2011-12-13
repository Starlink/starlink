      SUBROUTINE KPG1_CGET( INDF, COMP, VALUE, STATUS )
*+
*  Name:
*     KPG1_CGET

*  Purpose:
*     Obtains an NDF character component, removing escape sequences.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_CGET( INDF, COMP, VALUE, STATUS )

*  Description:
*     The routine obtains the value of the specified character
*     component of an NDF (i.e. the value of the LABEL, TITLE or UNITS
*     component). It is identical to NDF_CGET except that any PGPLOT or
*     AST escape sequences in the string are removed.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the character component whose value is required:
*        'LABEL', 'TITLE' or 'UNITS'.
*     VALUE = CHARACTER * ( * ) (Given and Returned)
*        The component's value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the requested component is in an undefined state, then the
*     VALUE argument will be returned unchanged. A suitable default
*     should therefore be established before calling this routine.
*     -  If the length of the VALUE argument is too short to
*     accommodate the returned result without losing significant
*     (non-blank) trailing characters, then this will be indicated by
*     an appended ellipsis, i.e. '...'. No error will result.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JUL-2006 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! CNF functions and constants
      INCLUDE 'AST_PAR'          ! AST functions and constants

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP

*  Arguments Given and Returned:
      CHARACTER * ( * ) VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BUFLEN
      INTEGER IPBUF
      INTEGER REMESC
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain a buffer twice the length of the supplied string variable.
      BUFLEN = 2*LEN( VALUE )
      CALL PSX_CALLOC( BUFLEN, '_CHAR', IPBUF, STATUS )

*  Use NDF_CGET to get the full value of the requested character component.
      CALL NDF_CGET( INDF, COMP, %VAL( CNF_PVAL( IPBUF ) ), STATUS,
     :               %VAL( CNF_CVAL( LEN( COMP ) ) ),
     :               %VAL( CNF_CVAL( BUFLEN ) ) )

*  Remove AST escape sequences.
      REMESC = AST_ESCAPES( 0, STATUS )
      VALUE = AST_STRIPESCAPES( %VAL( CNF_PVAL( IPBUF ) ), STATUS,
     :                          %VAL( CNF_CVAL( BUFLEN ) ) )
      REMESC = AST_ESCAPES( REMESC, STATUS )

*  Remove PGPLOT escape sequences.
      CALL KPG1_PGESC( VALUE, STATUS )

*  Free the buffer.
      CALL PSX_FREE( IPBUF, STATUS )

      END
