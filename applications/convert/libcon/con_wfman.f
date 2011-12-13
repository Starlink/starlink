      SUBROUTINE CON_WFMAN( EL, NDIM, DIMS, BITPIX, FITSAR, STATUS )
*+
*  Name:
*     CON_WFMAN

*  Purpose:
*     Writes the mandatory FITS header card images to the start of a
*     character array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_WFMAN( EL, NDIM, DIMS, BITPIX, FITSAR, STATUS )

*  Description:
*     This routine writes the mandatory FITS header cards to an
*     array (FITSAR) of 80-character elements.  These comprise
*     the SIMPLE=T, BITPIX, NAXIS, and NAXISn keywords in that order.

*  Arguments:
*     EL = INTEGER (Given)
*        The size of FITSAR.  It must be at least NDIM + 4.
*     NDIM = INTEGER (Given)
*        The number of axes to be written to the NAXIS keyword.
*     DIMS( NDIM ) = INTEGER (Given)
*        Array containing the lengths of each of the NDIM dimensions.
*     BITPIX = INTEGER (Given)
*        The number of bits per data value to write to the BITPIX
*        keyword.  Allowed values are 8, 16, 32, -32, or -64.
*     FITSAR( EL ) = CHARACTER * ( 80 ) (Returned)
*        The array to contain the mandatory FITS headers.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1997-1998 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     RAHM: Rhys Morris (STARLINK, University of Wales, Cardiff)
*     MJC: Malcolm J. Currie (STARLINK)
*     AJC: Alan J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 July 22 (MJC):
*        Original version based upon RAHM's PREFITS.
*     1997 November 23 (MJC):
*        Some tidying.  Removed obsolete CHR_MOVE code and used standard
*        CON_FKEYx routines to create the FITS headers.
*     1998 August 4 (AJC):
*        Write DIMS(n) not n for NAXISn value.
*     2009 June 29 (MJC):
*        Replace cloned CON_FKEYx with KAPLIBS FTS1_WKEYx.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG constants

*  Arguments Given:
      INTEGER EL
      INTEGER NDIM
      INTEGER DIMS( NDIM )
      INTEGER BITPIX

*  Arguments Given and Returned:
      CHARACTER * ( * ) FITSAR( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER CARDLE             ! Length of each FITS header card-image
      PARAMETER ( CARDLE = 80 )

*  Local Variables:
      CHARACTER * ( 1 ) BUFDIM   ! Buffer for dimension number
      CHARACTER * ( 11) BUFFER   ! Buffer for text manipulation
      INTEGER IDIM               ! Loop counter
      CHARACTER * ( 8 ) KEYWRD   ! FITS keyword
      INTEGER LINENO             ! Line number
      INTEGER NCHARS             ! Dummy to absorb unwanted number
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the size of the array.
      IF ( EL .LT. MAX( 1, NDIM ) + 4 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'EL', EL )
         CALL MSG_SETI( 'MEL', MAX( 1, NDIM ) + 4 )
         CALL ERR_REP( 'CON_WFMAN_DIMS',
     :     'The FITS header array has only ^EL elements.  It must '/
     :     /'have at least ^MEL.', STATUS )
         GOTO 999
      END IF

*  Write the SIMPLE header.
*  ========================

*  Write the 'SIMPLE  = T' in the first line of the FITS headers.
      CALL FTS1_WKEYL( 'SIMPLE', .TRUE., '/',  'Standard FITS',
     :                 FITSAR( 1 ), STATUS )

*  Report the value in verbose mode.
      CALL MSG_OUTIF( MSG__VERB, ' ', FITSAR( 1 ), STATUS )

*  Write the BITPIX header.
*  ========================

*  Write the BITPIX keyword.
      CALL FTS1_WKEYI( 'BITPIX', BITPIX, '/',
     :                 'Data type (bits per element)', FITSAR( 2 ),
     :                 STATUS )

*  Report the value in verbose mode.
      CALL MSG_OUTIF( MSG__VERB, ' ', FITSAR( 2 ), STATUS )

*  Write the NAXIS header.
*  =======================

*  Write the NAXIS keyword.
      CALL FTS1_WKEYI( 'NAXIS', NDIM, '/', 'Number of dimensions',
     :                 FITSAR( 3 ), STATUS )

*  Report the value in verbose mode.
      CALL MSG_OUTIF( MSG__VERB, ' ', FITSAR( 3 ), STATUS )

*  Write the NAXISn headers.
*  =========================

*  Loop for the NDIM dimensions starting at the next (the fourth) card
*  image.
      DO IDIM = 1, NDIM
         LINENO = IDIM + 3

*  Form the NAXISn keyword.
         CALL CHR_ITOC( IDIM, BUFDIM, NCHARS )
         KEYWRD = 'NAXIS'//BUFDIM
         BUFFER = 'Dimension '//BUFDIM

*  Write the NAXISn header.
         CALL FTS1_WKEYI( KEYWRD, DIMS( IDIM ), '/', BUFFER,
     :                    FITSAR( LINENO ), STATUS )

*  Report the value in verbose mode.
         CALL MSG_OUTIF( MSG__VERB, ' ', FITSAR( LINENO ), STATUS )
      END DO

  999 CONTINUE

      END
