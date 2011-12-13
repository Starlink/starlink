	SUBROUTINE CON_AFHIR( NCARDS, HEADER, IMDES, STATUS )
*+
*  Name:
*     CON_AFHIR

*  Purpose:
*     Adds a FITS header to an IRAF image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_AFHIR( NCARDS, HEADER, IMDES, STATUS )

*  Description:
*     The subroutine loops over the number of FITS lines or card images
*     adding each one to the 'user area' of the IRAF image. This cannot
*     be done except by using the SPP routine adline() as the file
*     containing the IRAF header information is a binary one.
*
*     Once the keyword 'END' is encountered, the subroutine returns to
*     the calling routine.

*  Arguments:
*     NCARDS = INTEGER (Given)
*        The number of 80-character card images to be copied.
*     HEADER( NCARDS ) = CHARACTER * ( 80 ) (Given)
*        The array of FITS headers to be parsed
*     IMDES = INTEGER (Given)
*        This is the image descriptor obtained in the calling routine
*        and required by the IRAF header-writing routines.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     ADLINE() is a routine written in SPP, the IRAF programming
*     language, which does the job of adding the header lines to the
*     image. The SPP routine uses the facilities available to the imfort
*     subroutine library, which is a subset of the facilities available
*     within the whole IRAF system.
*
*     This routine acts as a buffer for passing the array to the SPP
*     routine.
*
*        adline()
*     SPP program from NOAO.

*  Prior Requirements:
*     The IRAF image file must be open for write access.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     {enter_new_authors_here}

*  History:
*     28-SEP-1992 (RAHM):
*        Original version.
*     1992 September 29 (MJC):
*        Renamed from ADDFITS to be moved to the CONVERT library.  Also
*        made to adhere to SGP/16 rules.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NCARDS
      CHARACTER * ( 80 ) HEADER( NCARDS )
      INTEGER IMDES

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_EQUAL          ! CHR routine to test whether two
                                 ! strings are equal or not.

*  Local Variables:
      INTEGER J                  ! Loop variable 1 to NCARDS

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over the number of lines in the array.
      DO 100 J = 1, NCARDS

*  Check for the 'END' keyword, when we get to it, exit the loop.

         IF ( CHR_EQUAL( HEADER(J)(1:4), 'END ' ) ) GOTO 100

*  Copy the line to the IRAF header file.
         CALL ADLINE( IMDES, HEADER(J) )

 100  CONTINUE

      END

