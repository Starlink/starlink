      SUBROUTINE KPG1_SDTRN( LABEL, NDF, PICID, STATUS )
*+
*  Name:
*     KPG1_SDTRN

*  Purpose:
*     Saves the current SGS zone as a data picture and a reference to
*     the data object that was used to create it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_SDTRN( LABEL, NDF, PICID, STATUS )

*  Description:
*     This routine takes the current zone and saves it with the
*     specified label into the graphics database.  A data object that
*     was used to generate the picture is stored by reference in the
*     database, associated with the data picture.

*  Arguments:
*     LABEL = CHARACTER * ( * ) (Given)
*        The label for the data picture.
*     NDF = CHARACTER * ( * ) (Given)
*        The NDF identifier of the data object.
*     PICID = INTEGER (Returned)
*        The picture identifier of the data picture.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  The AGI SGS interface must be active.
*     -  The data object give by the parameter name should still be
*     associated to prevent reprompting.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 February 25 (MJC):
*        Original version.
*     1996 March 18 (MJC):
*        Obtains name of the NDF using NDF identifier rather than an
*        HDS locator.  This lets sections to be passed.  The PNDATA
*        argument was changed to become the identifier of the NDF
*        (usually being the one obtained through the PNDATA parameter).
*        Used modern-style coding.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants

*  Arguments Given:
      CHARACTER * ( * ) LABEL
      INTEGER NDF

*  Arguments Returned:
      INTEGER PICID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NC                 ! Number of characters
      CHARACTER * ( 256 ) NDFNAM ! NDF name

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Record the picture in the database, but still in linear
*  co-ordinates.
      CALL AGS_SZONE( 'DATA', LABEL, PICID, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'KPG1_SDTRN_DBSD',
     :     'Error while storing the data picture in the '/
     :     /'graphics database.', STATUS )
      END IF

*  Record the data object in the database.
*  =======================================
*
*  Obtain the name and section of the NDF as a string, by first making
*  a message token.
      CALL NDF_MSG( 'NDF', NDF )
      CALL MSG_LOAD( 'KPG1_SDTRN', '^NDF', NDFNAM, NC, STATUS )

*  Put the literal reference into the database for the current picture.
      CALL AGI_PTREF( NDFNAM, -1, STATUS )

      END
