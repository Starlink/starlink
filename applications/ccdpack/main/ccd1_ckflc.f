      SUBROUTINE CCD1_CKFLC( IDIN, STATUS )
*+
*  Name:
*     CCD1_CKFCL

*  Purpose:
*     Checks that an NDF is ok for processing by FLATCOR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CKFLC( IDIN, STATUS )

*  Description:
*     This routine gets the frame type (FTYPE) of the input NDF and
*     checks that it is sensible to perform a flatfield calibration.
*     This only includes frames whose type is TARGET and frames which
*     have been debiassed and excludes any types.

*  Arguments:
*     IDIN = INTEGER (Given)
*        The NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine only reports an warning if the type is not good.
*     No other action is taken.

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
*     3-OCT-1993 (PDRAPER):
*        Original version.
*     17-JAN-1994 (PDRAPER):
*        Now checks CCD1_TOUCH trails
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters

*  Arguments Given:
      INTEGER IDIN

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! Strings are the same (case insenstive)

*  Local Variables:
      CHARACTER * ( CCD1__NMLEN ) FTYPE ! Frame type
      CHARACTER * ( 30 ) DATE    ! Value of pre-processed object
      LOGICAL OK                 ! Found frame type

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Try to get the frame type from the NDF extension.
      CALL CCG1_FCH0C( IDIN, 'FTYPE', FTYPE, OK , STATUS )
      IF ( OK ) THEN

*  Have located a frame type may proceed. Is this a TARGET frame?
         IF ( .NOT. CHR_SIMLR( 'TARGET', FTYPE ) ) THEN

*  Issue a warning.
            CALL MSG_SETC( 'FTYPE', FTYPE )
            CALL CCD1_MSG( ' ',
     :' Warning - input NDF should have a frame type TARGET (^FTYPE) ',
     :      STATUS )
         END IF

*  Is there a DEBIAS object in the extension?
         CALL CCG1_FCH0C( IDIN, 'DEBIAS', DATE, OK, STATUS )
         IF ( .NOT. OK ) THEN
            CALL CCD1_MSG( ' ', ' Warning - input NDF may not '//
     :      'have been debiassed', STATUS )
         END IF

*  Has the NDF been flatfielded? It shouldn't have been.
         CALL CCG1_FCH0C( IDIN, 'FLATCOR', DATE, OK, STATUS )
         IF ( OK ) THEN
            CALL CCD1_MSG( ' ', ' Warning - input NDF appears to '//
     :'have been flatfielded already', STATUS )
         END IF
      END IF
      END
* $Id$
* $Id$
