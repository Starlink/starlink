      SUBROUTINE FTS1_NDFCM( NCARD, HEADER, SCARD, NDF, NENCOD, ENCODS,
     :                       STATUS )
*+
*  Name:
*     FTS1_NDFCM

*  Purpose:
*     Creates the title, units, axes, WCS, and FITS extension in an NDF
*     from the FITS headers.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_NDFCM( NCARD, HEADER, SCARD, NDF, NENCOD, ENCODS,
*                      STATUS )

*  Description:
*     This routine adds the character components, axis structure, WCS
*     component and FITS extension to an NDF.  It searches a buffer
*     containing the FITS header card images for the OBJECT keyword
*     whose value, if present, becomes the NDF title.  Similarly BUNIT
*     is mapped to the NDF units.  The supplied header structure is
*     copied to the FITS extension.

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
*     NDF = INTEGER (Given)
*        Identifier of the NDF to which to write the additional
*        components and the FITS extension.
*     NENCOD = INTEGER (Given)
*        The number of AST encodings supplied in ENCODS.
*     ENCODS( NENCOD ) = CHARACTER * ( * ) (Given)
*        The user's preferred AST encodings.  If NENCOD is zero, then
*        this is ignored, and an intelligent guess is made as to which
*        encoding to use.  The encoding determines which FITS headers
*        are used to create the NDF WCS component.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
*     Copyright (C) 1996, 1998 Central Laboratory of the Research
*                   Councils.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 November 26 (MJC):
*        Original version.
*     1991 February 28 (MJC):
*        Added the NCARD argument for revised FTS1_GKEYx and FTS1_AXIS
*        calls.
*     1996 November 24 (MJC):
*        Modern style.  Revised FTS1_GKEYx calls.
*     9-JUN-1998 (DSB):
*        Added support for WCS component.
*     9-DEC-1998 (DSB):
*        Flush errors reported while creating the WCS component.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE             ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'DAT_PAR'          ! Data-system constants

*  Arguments Given:
      INTEGER NCARD              ! Total number of header cards
      CHARACTER * ( 80 ) HEADER( NCARD ) ! Header cards
      INTEGER SCARD              ! Search-start card number
      INTEGER NDF                ! NDF identifier
      INTEGER NENCOD
      CHARACTER ENCODS( NENCOD )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Number of characters in a string
                                 ! ignoring trailing blanks

*  Local Variables:
      CHARACTER * ( 48 ) COMENT  ! Comment from card (not used)
      CHARACTER * ( DAT__SZLOC ) FLOC ! Locator for FITS extension
      CHARACTER * ( DAT__SZLOC ) HD1LOC ! Locator for a cell of the FITS
                                 ! extension
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter
      INTEGER NC                 ! Number of characters in a component
      INTEGER NKC                ! Header number containing the named
                                 ! keyword
      LOGICAL THERE              ! Named keyword is present in the
                                 ! header?
      CHARACTER * ( 70 ) TITLE   ! NDF title
      CHARACTER * ( 70 ) UNITS   ! NDF units

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain other top-level components.
*  ==================================
*
*  First obtain the title from the OBJECT keyword in the header.
      CALL FTS1_GKEYC( NCARD, HEADER, SCARD, 'OBJECT', 1, THERE, TITLE,
     :                 COMENT, NKC, STATUS )

*  If it is present set the NDF title, truncating unnecessary blanks.
      IF ( THERE .AND. TITLE .NE. ' ' ) THEN
         NC = CHR_LEN( TITLE )
         CALL NDF_CPUT( TITLE( :NC ), NDF, 'Title', STATUS )
      END IF

*  Second obtain the units from the BUNIT keyword in the header.
      CALL FTS1_GKEYC( NCARD, HEADER, SCARD, 'BUNIT', 1, THERE, UNITS,
     :                 COMENT, NKC, STATUS )

*  If it is present set the NDF units, truncating unnecessary blanks.
      IF ( THERE .AND. UNITS .NE. ' ' ) THEN
         NC = CHR_LEN( UNITS )
         CALL NDF_CPUT( UNITS( :NC ), NDF, 'Units', STATUS )
      END IF

*  Make the axes.
*  ==============
      CALL FTS1_AXIS( NCARD, HEADER, SCARD, NDF, STATUS )

*  Make the WCS component.
*  =======================
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL FTS1_FTWCS( NCARD, HEADER, SCARD, NDF, NENCOD, ENCODS,
     :                    STATUS )

*  Flush any errors which occurred while creating the WCS component
*  since failure to read WCS is not usually fatal.
         IF( STATUS .NE. SAI__OK ) THEN

            CALL ERR_REP( 'FTS1_FTWCS_ERR1', 'Error importing World '//
     :                    'Coordinate information from a FITS header.',
     :                     STATUS )
            CALL ERR_REP( 'FTS1_FTWCS_ERR2', 'The output NDF will not'//
     :                    ' contain any WCS information.', STATUS )

            CALL ERR_FLUSH( STATUS )

         END IF

      END IF

*  Make the FITS extension.
*  ========================

*  Create the FITS extension---an array structure and get a locator to
*  it.
       CALL NDF_XNEW( NDF, 'FITS', '_CHAR*80', 1, NCARD - SCARD + 1,
     :                FLOC, STATUS )

*    For all the headers in the relevant section.
      J = 0
      DO  I = SCARD, NCARD - SCARD + 1

*  Increment number of the header in the FITS extension.
         J = J + 1

*  Copy the headers to the FITS extension.
         CALL DAT_CELL( FLOC, 1, J, HD1LOC, STATUS )
         CALL DAT_PUT0C( HD1LOC, HEADER( I ), STATUS )
         CALL DAT_ANNUL ( HD1LOC, STATUS )
      END DO

      CALL DAT_ANNUL( FLOC, STATUS )

      END
