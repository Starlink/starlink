      SUBROUTINE COF_HD2FC( FUNIT, FC, STATUS )
*+
*  Name:
*     COF_HD2FC

*  Purpose:
*     Copies all headers from the current HDU into a FitsChan.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_HD2FC( FUNIT, FC, STATUS )

*  Description:
*     This routine extracts all headers from the current HDU and stores
*     them in the supplied FitsChan. The FitsChan is first emptied.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     FC = INTEGER (Given)
*        Pointer to the FitsChan.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JAN-2011 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Arguments Given:
      INTEGER FUNIT
      INTEGER FC

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
      INTEGER NBAD               ! Number of bad header cards
      INTEGER NHEAD              ! Number of FITS headers

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Find the number of headers (not including the final END).
      CALL FTGHSP( FUNIT, NHEAD, KEYADD, FSTAT )
      IF( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_HD2FC_NHEAD', 'FTGHSP',
     :                   'Error obtaining the number of header cards.',
     :                   STATUS )
         GO TO 999
      END IF

*  Initialise the number of bad header cards.
      NBAD = 0

*  Empty the FitsChan.
      CALL AST_EMPTYFITS( FC, STATUS )

*  Loop through the headers.
      DO IHEAD = 1, NHEAD

*  Obtain the header. If an error occurred getting the header, flush
*  the FITSIO error stack, increment the number of bad headers, but
*  carry on to process any remaining headers.
         HEADER = ' '
         CALL FTGREC( FUNIT, IHEAD, HEADER, FSTAT )
         IF( FSTAT .NE. FITSOK ) THEN
            FSTAT = FITSOK
            CALL FTCMSG
            NBAD = NBAD + 1

*  Add this header into the FitsChan. If an error occurs, annul the
*  error, increment the number of bad headers, and continue to process
*  any remaining headers.
         ELSE
            CALL AST_PUTFITS( FC, HEADER, 1, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
               NBAD = NBAD + 1
            END IF
         END IF

      END DO

*  Issue a warning if any bad header cards were encountered.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NBAD .NE. 0 ) ) THEN
         CALL ERR_MARK
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NBAD', NBAD )
         CALL ERR_REP( 'COF_HD2FC_WARN', 'WARNING: ^NBAD FITS header '//
     :                 'cards could not be read and were not used.',
     :                 STATUS )
         CALL ERR_FLUSH( STATUS )
         CALL ERR_RLSE
      END IF

*  Rewind the FitsChan by clearing the Card attribute so that the first
*  header card will be read first.
      CALL AST_CLEAR( FC, 'Card', STATUS )

*  Jump to here if an error occurs.
  999 CONTINUE

      END
