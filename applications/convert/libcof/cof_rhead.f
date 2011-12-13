      SUBROUTINE COF_RHEAD( FUNIT, FILE, NHEAD, MXHEAD, HEADER, STATUS )
*+
*  Name:
*     COF_RHEAD

*  Purpose:
*     Reads the current FITS header into an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_RHEAD( FUNIT, FILE, NHEAD, MXHEAD, HEADER, STATUS )

*  Description:
*     This reads the FITSIO current headers of a FITS file into an
*     array.  This is largely intended for dealing with datasets using
*     the Green Bank convention, where the global information in the
*     primary headers needs to be merged with columns in a binary table.
*     of a series of observations.
*
*     Following these, but before the END card, come FITS header cards
*     containing the values of columns of the nominated row of the
*     binary table that have been unused to form the NDF.  These
*     additional headers use the TTYPEn values as their keywords.
*     If the primary header array supplied already contains such
*     keywords, the existing values are overwritten.  The headers are
*     then copied verbatim to the extension.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the FITS file or tape device to appear in the
*        error messages.
*     NHEAD = INTEGER (Given)
*        Number of headers from the HDU.
*     MXHEAD = INTEGER (Given)
*        Maximum number of headers.
*     HEADER( MXHEAD ) = CHARACTER * ( 80 ) (Returned)
*        The NHEAD headers from the HDU, but excluding the END card.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The FITS files must already be opened with the FITSIO library.

*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 June 19 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FUNIT
      CHARACTER * ( * ) FILE
      INTEGER NHEAD
      INTEGER MXHEAD

*  Arguments Returned:
      CHARACTER * ( 80 ) HEADER( MXHEAD )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a string less trailing
                                 ! blanks

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      CHARACTER * ( 256 ) BUFFER ! Used to form error messages
      INTEGER FSTAT              ! FITSIO status
      INTEGER IHEAD              ! Loop counter for headers
      INTEGER NCF                ! Number of characters in the FITS file
                                 ! name

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Get the length of the filename.
      NCF = CHR_LEN( FILE )

*  Loop through the headers excluding the END card.
      DO IHEAD = 1, NHEAD

*  Obtain the header verbatim and store in the output array.
         CALL FTGREC( FUNIT, IHEAD, HEADER( IHEAD ), FSTAT )
      END DO

*  Report an contextual error message if something went wrong.
      IF ( FSTAT .NE. FITSOK ) THEN
         BUFFER = 'Error reading the headers of the FITS file '/
     :            /FILE( :NCF )//'.'
         CALL COF_FIOER( FSTAT, 'COF_RHEAD_READ', 'FTGREC',
     :                   BUFFER, STATUS )

*  Restore the bad status when something went wrong with the FITSIO
*  calls.
         STATUS = SAI__ERROR
      END IF

  999 CONTINUE

      END
