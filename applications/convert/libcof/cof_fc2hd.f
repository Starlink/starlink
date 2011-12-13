      SUBROUTINE COF_FC2HD( FC, FUNIT, STATUS )
*+
*  Name:
*     COF_FC2HD

*  Purpose:
*     Copies all headers from a FitsChan to the current HDU.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_FC2HD( FC, FUNIT, STATUS )

*  Description:
*     This routine extracts all headers from the supplied FitsChan and
*     stores them in the current HDU of the supplied FITS file. The HDU
*     header is first emptied.

*  Arguments:
*     FC = INTEGER (Given)
*        Pointer to the FitsChan.
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The FITS file must already be opened with the FITSIO library.

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
*     18-JAN-2011 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Arguments Given:
      INTEGER FC
      INTEGER FUNIT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

      INTEGER HEDLEN             ! FITS header length
      PARAMETER( HEDLEN = 80 )

*  Local Variables:
      CHARACTER * ( HEDLEN ) HEADER ! A FITS header
      INTEGER FSTAT              ! FITSIO status
      INTEGER IHEAD              ! Loop counter for headers
      INTEGER KEYADD             ! Number of headers that can be added
      INTEGER NHEAD              ! Number of FITS headers

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  See how many cards there are in the supplied FITSIO header.
      CALL FTGHPS( FUNIT, NHEAD, KEYADD, FSTAT )

*  Empty the FITSIO header. For efficiency, delete from the end of the
*  list to the start of the list. If an error occurs, reset the status,
*  clear the message stack, and continue.
      DO IHEAD = NHEAD, 1, -1
         CALL FTDREC( FUNIT, IHEAD, FSTAT )
         IF( FSTAT .NE. FITSOK ) THEN
            FSTAT = FITSOK
            CALL FTCMSG
         END IF
      END DO

* Now copy the contents of the FitsChan into the empty FITSIO header.
      CALL AST_CLEAR( FC, 'Card', STATUS )
      DO WHILE( AST_FINDFITS( FC, '%f', HEADER, .TRUE., STATUS ) )
         CALL FTPREC( FUNIT, HEADER, FSTAT )
         IF( FSTAT .NE. FITSOK ) THEN
            FSTAT = FITSOK
            CALL FTCMSG
         END IF
      END DO

      END
