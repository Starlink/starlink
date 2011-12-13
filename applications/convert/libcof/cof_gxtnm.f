      SUBROUTINE COF_GXTNM( NHDU, COMP, EXTNAM, STATUS )
*+
*  Name:
*     COF_GXTNM

*  Purpose:
*     Get the name of an extension given an EXTABLE component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_GXTNM( NHDU, COMP, EXTNAM, STATUS )

*  Description:
*     This routine returns the required extension name give an EXTABLE
*     component name of the form EXTNi.name, where i and .name are
*     optional.
*     i is ignored if present.
*     if .name is present the returned name is 'name'.
*     if name is omitted the returned name is 'FITS_EXT_n' where n is
*     the character representaion of NHDU

*  Arguments:
*     NHDU = INTEGER (Given)
*        The number to append to the default name.
*     COMP = CHARACTER*(*) (Given)
*        The EXTABLE component name.
*     EXTNAM = CHARACTER*(*) (Returned)
*        The name of the NDF extension.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
*     AJC: Alan J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*      5-SEP-2000 (AJC):
*        Original
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NHDU
      CHARACTER * ( * ) COMP
      CHARACTER * ( * ) EXTNAM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:

*  Global Variables:

*  Local Variables:
      INTEGER NPOS
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If it's an extension, create the NDF extension name
      IF( COMP(1:4) .EQ. 'EXTN' ) THEN
         NPOS = INDEX( COMP, '.' )
         IF ( ( NPOS .EQ. 0 )
     :      .OR. ( NPOS .EQ. CHR_LEN( COMP ) ) )THEN
*  Default NDF extension name
            EXTNAM = 'FITS_EXT_'
            NPOS = 9
            CALL CHR_PUTI( NHDU, EXTNAM, NPOS )

         ELSE
            EXTNAM = COMP(NPOS+1:)

         END IF
      END IF

      END
