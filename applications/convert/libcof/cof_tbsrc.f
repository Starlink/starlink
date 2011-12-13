      SUBROUTINE COF_TBSRC( FC, EXTNAM, EXTVER, EXTLEVEL, STATUS )
*+
*  Name:
*     COF_TBSRC

*  Purpose:
*     Reads a FITS binary table and store it in a FitsChan.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_TBSRC( FC, EXTNAM, EXTVER, EXTLEVEL, STATUS )

*  Description:
*     This function is a service routine for the AST_READ function.
*     It is invoked when AST_READ needs to read WCS information from a
*     binary table of a FITS file. This happens if one or more of the
*     CTYPE keywords in the header uses the "-TAB" algorithm code
*     (defined in FITS-WCS Paper III). It reads a binary table from the
*     named extension of the FITS file, copies the data into a newly
*     created AstFitsTable structure, and stores this AstFitsTable in
*     the supplied FitsChan. If the required extension cannot be found,
*     this routine returns without action (no error is reported).
*
*     The identifier for the FITS file is passed in a common block.

*  Arguments:
*     FC = INTEGER (Given)
*        An AST pointer to the FitsChan in which to store the table
*        data.
*     EXTNAM = CHARACTER * ( * ) (Given)
*        The name of the FITS extension containing the required binary
*        table.
*     EXTVER = INTEGER (Given)
*        The value of the EXTVER keyword in the required binary table
*        HDU.
*     EXTLEVEL = INTEGER (Given)
*        The value of the EXTLEVEL keyword in the required binary table
*        HDU.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     13-JAN-2011 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST_ public constants

*  Global Variables:
      INCLUDE 'F2NDF3_CMN'
*        INTEGER FUNIT (Read)
*           The Fortran unit number associated with the FITS file

*  Arguments Given:
      INTEGER FC
      CHARACTER EXTNAM*(*)
      INTEGER EXTVER
      INTEGER EXTLEVEL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER TABLE              ! The FitsTable to add to the FitsChan
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a FitsTable holding the contents of the binary table in the
*  named extension.
      CALL COF_BT2FT( FUNIT, EXTNAM, EXTVER, EXTLEVEL, TABLE, STATUS )

*  If a table was created, add it to the supplied FitsChan, and then
*  annul the local pointer to it.
      IF( TABLE .NE. AST__NULL ) THEN
         CALL AST_PUTTABLE( FC, TABLE, EXTNAM, STATUS )
         CALL AST_ANNUL( TABLE, STATUS )
      END IF

      END
