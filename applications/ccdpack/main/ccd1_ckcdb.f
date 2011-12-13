      SUBROUTINE CCD1_CKCDB( IDIN, STATUS )
*+
*  Name:
*     CCD1_CKCDB

*  Purpose:
*     Checks that an NDF is ok for debiassing.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CKCDB( IDIN, STATUS )

*  Description:
*     This routine gets the frame type (FTYPE) of the input NDF and
*     checks that it is sensible to debias this frame. This excludes
*     frames whose type is BIAS and frames which have already been
*     debiassed.  This latter case is indicated by the presence of
*     an object "DEBIAS" in the CCDPACK extension.

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
      CHARACTER * ( 30 ) DEBIAS  ! Value of DEBIAS object
      LOGICAL OK                 ! Found frame type
      INTEGER IAT                ! Position of _PROCESSED in string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Try to get the frame type from the NDF extension.
      CALL CCG1_FCH0C( IDIN, 'FTYPE', FTYPE, OK , STATUS )
      IF ( OK ) THEN

*  Have located a frame type may proceed. First see if it is a BIAS
*  frame.
         IF ( CHR_SIMLR( 'BIAS', FTYPE ) ) THEN

*  Issue a warning should try to debias bias frames.
            CALL CCD1_MSG( ' ', ' Warning - input NDF is a BIAS frame',
     :                     STATUS )
            GO TO 1
         END IF

*  Is there a DEBIAS object in the extension?
         CALL CCG1_FCH0C( IDIN, 'DEBIAS', DEBIAS, OK, STATUS )
         IF ( OK ) THEN
            CALL CCD1_MSG( ' ', ' Warning - input NDF appears to '//
     :      'have already been debiassed', STATUS )
            GO TO 1
         END IF

*  Is this a MASTER of some kind?
         IAT = INDEX( FTYPE, 'MASTER' )
         IF ( IAT .NE. 0 ) THEN
            CALL NDF_MSG( 'NDF', IDIN )
            CALL CCD1_MSG( ' ', ' Warning - NDF appears to '//
     :      'be a master calibration (^MASTER) frame', STATUS )
            GO TO 1
         END IF
      END IF
 1    CONTINUE
      END
* $Id$
