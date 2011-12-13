      SUBROUTINE COF_WFEXT( FUNIT, NDF, GROUP, PCOUNT, FILE, STATUS )
*+
*  Name:
*     COF_WFEXT

*  Purpose:
*     Creates the FITS extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_WFEXT( FUNIT, NDF, GROUP, PCOUNT, FILE, STATUS )

*  Description:
*     This creates the FITS extension or `airlock' from a previously
*     opened FITS file.  The FITS headers are copied verbatim to the
*     extension.
*
*     For a FITS random-group file, the routine will insert the group
*     parameters in the FITS extension in the form of pseudo-headers
*     situated immediately before the END card.  The scale and offset
*     is applied before the value is written as a header.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     NDF = INTEGER (Given)
*        The identifier for the NDF which is to have a new FITS
*        extension.
*     GROUP = INTEGER (Given)
*        The number of the FITS random-group.  This is ignored if
*        PCOUNT is zero, or has a non-positive value.
*     PCOUNT = INTEGER (Given)
*        The number of FITS random-group parameters.  This is normally
*        zero.
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the FITS file or tape device to appear in the
*        error messages.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The FITS file must already be opened with the FITSIO library.
*
*     [optional_subroutine_items]...

*  Copyright:
*     Copyright (C) 1996, 1998 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     1996 January 19 (MJC):
*        Original version.
*     1996 April 15 (MJC):
*        Added PCOUNT argument.
*     1998 April 22 (MJC):
*        Bug fix: increment the header count when writing the END card.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants

*  Arguments Given:
      INTEGER FUNIT
      INTEGER NDF
      INTEGER GROUP
      INTEGER PCOUNT
      CHARACTER * ( * ) FILE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a string less trailing
                                 ! blanks

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

      INTEGER HEDLEN             ! FITS header length
      PARAMETER( HEDLEN = 80 )

*  Local Variables:
      CHARACTER * ( 256 ) BUFFER ! Used to form error messages
      CHARACTER * ( 48 ) COMENT  ! Keyword comment
      INTEGER EL                 ! Number of FITS-extension elements
      CHARACTER * ( DAT__SZLOC ) FLOC ! Locator to the FITS extension
      INTEGER FSTAT              ! FITSIO status
      DOUBLE PRECISION GPARAM    ! Group parameter value
      CHARACTER * ( HEDLEN ) HEADER ! A FITS header
      CHARACTER * ( DAT__SZLOC ) HLOC ! Locator to a cell of the FITS
                                 ! extension
      INTEGER I                  ! Loop counter for group parameters
      INTEGER IHEAD              ! Loop counter for headers
      INTEGER KEYADD             ! Number of headers that can be added
      CHARACTER * ( 8 ) KEYWRD   ! FITS keyword for PTYPEn
      INTEGER NCF                ! Number of characters in the FITS file
                                 ! name
      INTEGER NHEAD              ! Number of FITS headers
      CHARACTER * ( 20 ) PAVAL   ! Random-group value in G20.12 format
      DOUBLE PRECISION PSCALE    ! Group parameter scale factor
      CHARACTER * ( 8 ) PTYPE    ! PTYPEn keyword
      DOUBLE PRECISION PZERO     ! Group parameter offset
      LOGICAL THERE              ! PTYPEn is present



*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Get the length of the filename.
      NCF = CHR_LEN( FILE )

*  Find the number of headers.
      CALL FTGHSP( FUNIT, NHEAD, KEYADD, FSTAT )
      IF ( FSTAT .NE. FITSOK ) THEN
         BUFFER = 'Error obtaining the number of header cards from '/
     :            /'the FITS file '//FILE( :NCF )//'.'
         CALL COF_FIOER( FSTAT, 'COF_WFEXT_NHEAD', 'FTGHSP', BUFFER,
     :                   STATUS )
         GOTO 999
      END IF

*  Allow for the group parameters and two comment cards, and the END
*  card (FTGHSP excludes it from its count), to find the number of
*  elements in the FITS extension.
      IF ( GROUP .GT. 0 .AND. PCOUNT .GT. 0 ) THEN
         EL = NHEAD + PCOUNT + 3
      ELSE
         EL = NHEAD + 1
      END IF

*  Create the FITS extension of the appropriate length.
      CALL NDF_XNEW( NDF, 'FITS', '_CHAR*80', 1, EL, FLOC, STATUS )

*  Loop through the headers excluding the END card.
      DO IHEAD = 1, NHEAD

*  Obtain the header.
         CALL FTGREC( FUNIT, IHEAD, HEADER, FSTAT )

*  Report if anything went wrong.  Go to the point before the end where
*  the END is written and the extension is tidied.
         IF ( FSTAT .NE. FITSOK ) THEN
            CALL MSG_SETI( 'NH', NHEAD + 1 )
            CALL MSG_SETI( 'IH', IHEAD )
            CALL MSG_SETC( 'FILE', FILE( :NCF ) )

            CALL COF_FIOER( FSTAT, 'COF_WFEXT_GHEAD', 'FTGREC',
     :        'Error obtaining a FITS header (^IH of ^NH) from '/
     :        /'FITS file ^FILE.', STATUS )
            CALL DAT_ANNUL( FLOC, STATUS )
            GOTO 980
         END IF

*  Obtain a cell into the extension.  Put the header into the cell,
*  and tidy the temporary locator.
         CALL DAT_CELL( FLOC, 1, IHEAD, HLOC, STATUS )
         CALL DAT_PUT0C( HLOC, HEADER, STATUS )
         CALL DAT_ANNUL( HLOC, STATUS )
      END DO

*  Insert the group parameters in the form of FITS-header cards.
      IF ( GROUP .GT. 0 .AND. PCOUNT .GT. 0 ) THEN

*  Write a blank card to the header.  Obtain a cell into the extension.
*  Put the header into the cell, and tidy the temporary locator.
         CALL DAT_CELL( FLOC, 1, NHEAD + 1, HLOC, STATUS )
         CALL DAT_PUT0C( HLOC, ' ', STATUS )
         CALL DAT_ANNUL( HLOC, STATUS )

*  Write a blank card to the header.  Obtain a cell into the extension.
*  Put the header into the cell, and tidy the temporary locator.
         CALL DAT_CELL( FLOC, 1, NHEAD + 2, HLOC, STATUS )
         CALL DAT_PUT0C( HLOC, 'COMMENT  Group Parameters: ', STATUS )
         CALL DAT_ANNUL( HLOC, STATUS )

*  Shift the index to the current extension element.
         IHEAD = NHEAD + 2

*  Loop for each group parameter.
         DO I = 1, PCOUNT

*  Get the group-parameter scales and offsets, using double precision
*  to avoid loss of precision, though it may create a false precision
*  in the output data structure.
            CALL FTKEYN( 'PSCAL', I, KEYWRD, FSTAT )
            CALL COF_GKEYD( FUNIT, KEYWRD, THERE, PSCALE, COMENT,
     :                      STATUS )
            IF ( .NOT. THERE ) PSCALE = 1.0D0

            CALL FTKEYN( 'PZERO', I, KEYWRD, FSTAT )
            CALL COF_GKEYD( FUNIT, KEYWRD, THERE, PZERO, COMENT,
     :                      STATUS )
            IF ( .NOT. THERE ) PZERO = 0.0

*  Find the element number for the group parameter.
            IHEAD = IHEAD + 1

*  Obtain the current parameter in double precision.
            CALL FTGGPD( FUNIT, GROUP, I, 1, GPARAM, FSTAT )

*  Abort if something went wrong, but write the
            IF ( FSTAT .NE. FITSOK ) THEN
               CALL MSG_SETI( 'PC', PCOUNT )
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETC( 'FILE', FILE( :NCF ) )

               CALL COF_FIOER( FSTAT, 'COF_WFEXT_GHEAD', 'FTGGPD',
     :           'Error obtaining a random-group parameter (^I of '/
     :           /'^PC) from FITS file ^FILE.', STATUS )
               CALL DAT_ANNUL( FLOC, STATUS )
               GOTO 980
            END IF

*  Apply the scale and offset to get the true parameter value.
            GPARAM = GPARAM * PSCALE + PZERO

*  Write the value to a string that will fit into the normal 20
*  characters for a numerical value in a FITS header.
            WRITE( PAVAL, '(G20.12)' ) GPARAM

*  Obtain the value of the PTYPEn keyword, which will become the keyword
*  of the group parameter in the pseudo header card.
            CALL FTKEYN( 'PTYPE', I, KEYWRD, FSTAT )
            CALL COF_GKEYC( FUNIT, KEYWRD, THERE, PTYPE, COMENT,
     :                      STATUS )
            IF ( THERE ) THEN

*  Create the Pseudo-FITS cards for the parameters.
               HEADER = PTYPE//'= '//PAVAL
            ELSE
               HEADER = 'COMMENT  '//PAVAL
            END IF

*  Obtain a cell into the extension.  Put the header into the cell,
*  and tidy the temporary locator.
            CALL DAT_CELL( FLOC, 1, IHEAD, HLOC, STATUS )
            CALL DAT_PUT0C( HLOC, HEADER, STATUS )
            CALL DAT_ANNUL( HLOC, STATUS )
         END DO

*  Just to be on the safe side set the IHEAD counter and not assume
*  that it has retained the last value of the DO loop through the
*  headers.
      ELSE
         IHEAD = NHEAD

      END IF

*  Use a temporary status if something has gone wrong using FITSIO.
*  This lets the END card be written, and other tidying operations.
  980 CONTINUE
      IF ( FSTAT .GT. FITSOK ) STATUS = SAI__OK

*  Append the END card.  Obtain a cell into the extension.  Put the
*  END card into the cell, and tidy the temporary locator.
      CALL DAT_CELL( FLOC, 1, IHEAD + 1, HLOC, STATUS )
      CALL DAT_PUT0C( HLOC, 'END', STATUS )
      CALL DAT_ANNUL( HLOC, STATUS )

*  Tidy the locator to the extension.
      CALL DAT_ANNUL( FLOC, STATUS )

*  Restore the bad status when something went wrong with the FITSIO
*  calls.
      IF ( FSTAT .GT. FITSOK ) STATUS = SAI__ERROR

  999 CONTINUE

      END
