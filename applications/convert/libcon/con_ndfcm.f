      SUBROUTINE CON_NDFCM( NCARD, HEADER, SCARD, FITSEX, NDF, STATUS )
*+
*  Name:
*     CON_NDFCM

*  Purpose:
*     Create the title, label, units, axes, and FITS extension in an NDF
*     from the FITS headers.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_NDFCM( NCARD, HEADER, SCARD, FITSEX, NDF, STATUS )

*  Description:
*     This routine adds the character components, axis structure and
*     FITS extension to an NDF.  It searches a buffer containing the
*     FITS header card images for the TITLE keyword whose value
*     becomes the NDF title if present.  Similarly BUNIT is mapped to
*     the NDF units.  The supplied header structure is copied to the
*     FITS extension if requested.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of cards in the array of headers, from the start of
*        the first header section to the end of the current one.
*     HEADER( NCARD ) = CHARACTER * 80 (Given)
*        The buffer containing the header card images.
*     SCARD = INTEGER (Given)
*        The number of the card from where searches will begin, and
*        copying of the headers to the FITS extension.   Therefore
*        NCARD - SCARD + 1 headers will appear in the extension. This
*        argument is needed because the headers make contain a dummy
*        header prior to an extension.
*     FITSEX = LOGICAL (Given)
*        If true, the headers are written to the FITS extension.
*     NDF = INTEGER (Given)
*        Identifier of the NDF to which to write the additional
*        components and the FITS extension.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     AJC: Alan J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 September 17 (MJC):
*        Original version based on FTS1_NDFCM.
*     2001 August 30 (AJC):
*        Correct CON_GKEYC arguments.
*     2009 June 29 (MJC):
*        Replace cloned CON_GKEYx with KAPLIBS FTS1_GKEYx.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT  NONE           ! no default typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'       ! SSE global definitions
      INCLUDE  'DAT_PAR'       ! Data-sytem constants

*  Arguments Given:
      INTEGER NCARD            ! Total number of header cards
      CHARACTER HEADER( NCARD ) * 80 ! Header cards
      INTEGER SCARD            ! Search-start card number
      LOGICAL FITSEX           ! Write FITS extension?
      INTEGER NDF              ! NDF identifier

*  Status:
      INTEGER STATUS           ! Global status

*  External References:
      INTEGER CHR_LEN          ! Number of characters in a string
                               ! ignoring trailing blanks

*  Local Variables:
      INTEGER I                ! Loop counter
      INTEGER J                ! Loop counter
      INTEGER NC               ! Number of characters in a component
      INTEGER NKC              ! Header number containing the named
                               ! keyword

      LOGICAL THERE            ! True if named keyword is present in
                               ! the header

      CHARACTER * ( 80 ) COM   ! Header Comment
      CHARACTER * ( 70 ) LABEL ! NDF label
      CHARACTER * ( 70 ) TITLE ! NDF title
      CHARACTER * ( 70 ) UNITS ! NDF units

      CHARACTER * (DAT__SZLOC) FLOC ! Locators for FITS extension
      CHARACTER * (DAT__SZLOC) HD1LOC ! Locators for a cell of the FITS
                               ! extension
*.

*  Check for an error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain other top-level components.
*  ==================================
*
*  First obtain the title from the TITLE keyword in the header.
      CALL FTS1_GKEYC( NCARD, HEADER, SCARD, 'TITLE', 1, THERE, TITLE,
     :                 COM, NKC, STATUS )

*  If it is present set the NDF title, truncating unnecessary blanks.
      IF ( THERE .AND. TITLE .NE. ' ' ) THEN
         NC = CHR_LEN( TITLE )
         CALL NDF_CPUT( TITLE( :NC ), NDF, 'Title', STATUS )
      END IF

*  Second obtain the units from the BUNIT keyword in the header.
      CALL FTS1_GKEYC( NCARD, HEADER, SCARD, 'BUNIT', 1, THERE, UNITS,
     :                 COM, NKC, STATUS )

*  If it is present set the NDF units, truncating unnecessary blanks.
      IF ( THERE .AND. UNITS .NE. ' ' ) THEN
         NC = CHR_LEN( UNITS )
         CALL NDF_CPUT( UNITS( :NC ), NDF, 'Units', STATUS )
      END IF

*  Third obtain the label from the LABEL keyword in the header.
      CALL FTS1_GKEYC( NCARD, HEADER, SCARD, 'LABEL', 1, THERE, LABEL,
     :                 COM, NKC, STATUS )

*  If it is present set the NDF units, truncating unnecessary blanks.
      IF ( THERE .AND. LABEL .NE. ' ' ) THEN
         NC = CHR_LEN( LABEL )
         CALL NDF_CPUT( LABEL( :NC ), NDF, 'Label', STATUS )
      END IF

*  Make the axes.
*  ==============
      CALL CON_FITAX( NCARD, HEADER, SCARD, NDF, STATUS )

*  Make the FITS extension.
*  ========================
      IF ( FITSEX ) THEN

*  Create the FITS extension---an array structure and get a locator
*  to it.
         CALL NDF_XNEW( NDF, 'FITS', '_CHAR*80', 1, NCARD - SCARD + 1,
     :                  FLOC, STATUS )

*  For all the headers in the relevant section.
         J = 0
         DO  I = SCARD, NCARD - SCARD + 1

*  Increment number of the header in the FITS extension.
            J = J + 1

*  Copy the headers to the FITS extension.
            CALL DAT_CELL( FLOC, 1, J, HD1LOC, STATUS )
            CALL DAT_PUT0C( HD1LOC, HEADER( I ), STATUS )
            CALL DAT_ANNUL ( HD1LOC, STATUS )
         END DO

*  Tidy the locator to the extension.
         CALL DAT_ANNUL( FLOC, STATUS )
      END IF

      END
