      SUBROUTINE CCD1_CKDEB( IDS, NNDF, STATUS )
*+
*  Name:
*     CCD1_CKDEB

*  Purpose:
*     Checks that a list of NDFs have been debiassed.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CKDEB( IDS, NNDF, STATUS )

*  Description:
*     This routine checks for the presence of the CCDPACK extension item
*     "DEBIAS" in the extensions of a list of NDFs. If an NDF has not
*     been debiassed then a warning is issued. If no extension exists
*     then no action is taken.

*  Arguments:
*     IDS( NNDF ) = INTEGER (Given)
*        The input NDF identifiers.
*     NNDF = INTEGER (Given)
*        The number of NDFs given.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - this routine is used as part of the "automated" extensions
*     added to CCDPACK as of version 2.0.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     17-JAN-1994 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters

*  Arguments Given:
      INTEGER NNDF
      INTEGER IDS( NNDF )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR
      EXTERNAL CHR_SIMLR         ! Strings are similar (case
                                 ! independent)

*  Local Variables:
      CHARACTER * ( 30 ) DEBIAS  ! Value of DEBIAS item
      INTEGER I                  ! Loop variable
      LOGICAL OK                 ! Value obtained ok
      LOGICAL THERE              ! CCDPACK Extension exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop looking for the extension item "DEBIAS"
      DO 1 I = 1, NNDF
         CALL NDF_XSTAT( IDS( I ), 'CCDPACK', THERE, STATUS )
         IF ( THERE ) THEN
            CALL CCG1_FCH0C( IDS( I ), 'DEBIAS', DEBIAS, OK, STATUS )
            IF ( .NOT. OK ) THEN

*  NDF has not been debiassed.
               CALL NDF_MSG( 'NDF', IDS( I ) )
               CALL CCD1_MSG( ' ',
     :' Warning - NDF: ^NDF has not been debiassed', STATUS )
            END IF
         END IF
 1    CONTINUE
      END
* $Id$
* $Id$
