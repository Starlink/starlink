      SUBROUTINE CCD1_CKTYP( IDS, NNDF, TYPE, STATUS )
*+
*  Name:
*     CCD1_CKTYP

*  Purpose:
*     Checks the frame types of a list of NDFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CKTYP( IDS, NNDF, TYPE, STATUS )

*  Description:
*     This routine looks for frame types in the input NDFs and checks
*     that each type is the same as the given one. NDFs with a type
*     which is different from that given (case independent) are reported
*     (using the messages system) frames with no type (or extension
*     are not checked).

*  Arguments:
*     IDS( NNDF ) = INTEGER (Given)
*        The input NDF identifiers.
*     NNDF = INTEGER (Given)
*        The number of NDFs given.
*     TYPE = CHARACTER * ( * ) (Given)
*        The expected frame type.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - this routine is used as part of the "automated" extensions
*     added to CCDPACK as of version 2.0.

*  Copyright:
*     Copyright (C) 1993-1994 Science & Engineering Research Council.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     28-SEP-1993 (PDRAPER):
*        Original version.
*     29-SEP-1993 (PDRAPER):
*        Modification of CCD1_CKFLA
*     17-JAN-1994 (PDRAPER):
*        Just performs essential checks now.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters

*  Arguments Given:
      INTEGER NNDF
      INTEGER IDS( NNDF )
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR
      EXTERNAL CHR_SIMLR         ! Strings are similar (case
                                 ! independent)

*  Local Variables:
      CHARACTER * ( CCD1__NMLEN ) FTYPE ! NDF frame types.
      INTEGER I                  ! Loop variable
      LOGICAL OK                 ! Value obtained ok

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop getting the frame types of the NDFs one by one. Do _not_ issue
*  a warning if no extension or frame type is present.
      DO 1 I = 1, NNDF

*  Get the FTYPE value from the CCDPACK extension of the NDF.
         CALL CCG1_FCH0C( IDS( I ), 'FTYPE', FTYPE, OK, STATUS )
         IF ( OK ) THEN

*  Compare it with the given type.
            IF ( .NOT. CHR_SIMLR( TYPE, FTYPE ) ) THEN

*  This may not be a valid frame to use in this application report this
*  to the user.
               CALL MSG_SETC( 'FTYPE', FTYPE )
               CALL MSG_SETC( 'TYPE', TYPE )
               CALL NDF_MSG( 'NDF', IDS( I ) )
               CALL CCD1_MSG( ' ', ' Warning - the NDF: ^NDF has'//
     :' an inappropriate frame type (^FTYPE), should be ^TYPE', STATUS)
            END IF
         END IF
 1    CONTINUE
      END
* $Id$
