      SUBROUTINE HDR_OUT( PARAM, XNAME, ITEM, COMMEN, VALUE, STATUS )
*+
*  Name:
*    HDR_OUT

*  Purpose:
*     Writes a header item.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HDR_OUT( PARAM, XNAME, ITEM, COMMEN, VALUE, STATUS )

*  Description:
*     This routine writes the value of a character header item. Header
*     items include both FITS header records and package specific
*     extension information. The values of FITS header records are
*     written by setting the XNAME argument to the value 'FITS' (or '
*     ').

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name of the image (case insensitive).
*     XNAME = CHARACTER * ( * ) (Given)
*        Name of the extension ('FITS' or ' ' for FITS).
*     ITEM = CHARACTER * ( * ) (Given)
*        Name of the header item.
*     COMMEN = CHARACTER * ( * ) (Given)
*        If XNAME is 'FITS' then this is used as a comment to enter
*        with the record. Otherwise this is not used.
*     VALUE = CHARACTER * ( * ) (Given)
*        The value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - Item names for any extension type may be hierarchical
*     (i.e. ING.DETHEAD writes the FITS header "ING DETHEAD";
*     BOUNDS.MAXX the value of the MAXX component of the BOUNDS
*     structure in a non-FITS extension). Writing hierarchical records
*     in FITS records is strongly discouraged.
*
*     - This routine may be used to write the value of items in
*     the same extension of more than one image dataset at a time by
*     using multiple parameter names. Multiple parameter names are
*     provided as a comma separated list (i.e. 'IN1,IN2,IN3'). Note
*     that the argument VALUE must be declared as a dimension of size
*     at least the number of parameters in the list, if this option is
*     used.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     2-SEP-1994 (PDRAPER):
*        Original version.
*     15-NOV-1994 (PDRAPER):
*        Now writes character values as default. This matches the
*        behaviour of the non-generic HDR_IN.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) XNAME
      CHARACTER * ( * ) ITEM
      CHARACTER * ( * ) COMMEN
      CHARACTER * ( * ) VALUE( * )

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Just call the correct generic form of this routine.
      CALL HDR_OUTC( PARAM, XNAME, ITEM, COMMEN, VALUE, STATUS )
      END
* $Id$
