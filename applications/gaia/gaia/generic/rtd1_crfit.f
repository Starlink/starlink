      SUBROUTINE RTD1_CRFIT( NDF, BITPIX, NHEAD, IPHEAD, AVAIL, STATUS )
*+
*  Name:
*     RTD1_CRFIT

*  Purpose:
*     Writes the FITS header derived from an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RTD1_CRFIT( NDF, BITPIX, NHEAD, IPHEAD, AVAIL, STATUS )

*  Description:
*     This routine creates the header section of the primary array or
*     IMAGE extension of an output FITS file based upon information in
*     an NDF.

*     There are two stages:
*     a) Inquire of the NDF its shape, type, character components,
*     and write these to the header.
*     b) Look for a FITS extension if requested to do so.  If one is
*     present append the headers contained therein to the FITS header
*     section, but not replacing any of the headers created in stage
*     a).

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF.
*     BITPIX = INTEGER (Given)
*        The FITS data type.
*     NHEAD = INTEGER (Returned)
*        On exit this is the number of cards used in HEADER.
*     IPHEAD = INTEGER (Returned)
*        Pointer to a character array that contains the final FITS
*        headers.
*     AVAIL = INTEGER (Returned)
*        The number of header cards actually allocated on exit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     Here are details of the processing of standard items from the
*     NDF into the FITS header:
*        SIMPLE, EXTEND, PCOUNT, GCOUNT --- all take their default
*          values.
*        BITPIX, NAXIS, NAXISn --- are derived directly from the NDF
*          data array;
*        OBJECT, LABEL, BUNIT --- the values held in NDF TITLE, LABEL,
*          and UNITS respectively are used if present, otherwise any
*          s found in the FITS extension are used.
*        ORIGIN and DATE --- are created automatically.  However the
*          former may be overriden by an ORIGIN card in the NDF
*          FITS extension.
*        BSCALE, BZERO, BLANK and END --- are not propagated
*          from the extension.  The first will be set for any extension.
*          BSCALE and BZERO are 1 and 0.  BLANK is set to a character
*          string of the BAD value for this data.

*  Copyright:
*     Copyright (C) 1998-2001 Central Laboratory of the Research Councils
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     PWD: Peter W. Draper (STARLINK, Durham University)
*     {enter_new_authors_here}

*  History:
*     1994 May 31 (MJC):
*        Original version.
*     1995 November 16 (MJC):
*        Fixed a bug that could occur in some circumstances when the a
*        rare CROTAn card is present.
*     1996 February 8 (MJC):
*        Added checks not to propagate the FITSIO banner and Starlink
*        ORIGIN card in the NDF's FITS extension to the output FITS
*        file.
*     1996 November 22 (PWD):
*        Converted to provide GAIA/RTD NDF import.
*     1998 May 15 (PWD):
*        Added NDF WCS support (moved here to make sure NDF WCS is seen
*        before any that are present in the FITS headers).
*     2000 Nov 23 (PWD):
*        Changed to only add the Starlink ORIGIN header, if an existing
*        FITS header block doesn't have an ORIGIN card. Previously
*        always overwrote with Starlink header.
*     2000 Dec 08 (PWD):
*        Now adds default NDF WCS if no actual NDF WCS component
*        exists. The FITS headers still override this by precedence in the
*        header list. Removed axis encoding, this is passed with the NDF
*        WCS.
*     2001 Apr 26 (PWD):
*        LBOUND cards are no longer imported from the existing header
*        (these are written as part of the standard headers). Removed
*        HDUCLAS cards are these are not needed and can cause problems
*        with saved images that are converted to FITS.
*     2004 Sep 02 (PWD):
*        Converted to use CNF_PVAL for pointers.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'         ! Data system constants
      INCLUDE 'NDF_PAR'         ! NDF_ public constants
      INCLUDE 'PRM_PAR'         ! PRIMDAT public constants
      INCLUDE 'AST_PAR'         ! AST parameters
      INCLUDE 'CNF_PAR'         ! CNF functions

*  Arguments Given:
      INTEGER NDF               ! NDF identifier
      INTEGER BITPIX            ! Bits per pixel

*  Arguments Returned:
      INTEGER IPHEAD
      INTEGER NHEAD
      INTEGER AVAIL

*  Status:
      INTEGER STATUS            ! Global status

*  Local Constants:
      INTEGER   FITSOK          ! Good status for FITSIO library
      PARAMETER( FITSOK = 0 )

      INTEGER   NFLAGS          ! Number of flags to indicate
                                ! presence special NDF components
      PARAMETER( NFLAGS = 3 )

      INTEGER   SZKEY           ! Length of keyword names
      PARAMETER( SZKEY = 8 )    ! Columns 1 to 8

      INTEGER   SZFITS          ! Length of FITS string
      PARAMETER( SZFITS = 80 )

      INTEGER   SZNVAL          ! Character length of numeric values
      PARAMETER( SZNVAL = 20 )  ! Columns 11 to 30

      INTEGER   SZVAL           ! Length of keyword string values
      PARAMETER( SZVAL = 70 )   ! Columns 11 to 80

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) FTLOC ! Locator to NDF FITS extension
      CHARACTER * ( DAT__SZLOC ) FTLOCI ! Locator to element of NDF FITS extension
      CHARACTER * ( SZFITS ) FITSTR ! FITS string
      CHARACTER * ( SZKEY ) KEYWRD ! Accommodates keyword name
      CHARACTER * ( SZVAL ) VALUE ! Accommodates keyword value
      INTEGER DIMS( NDF__MXDIM ) ! NDF dimensions (axis length)
      INTEGER I                 ! Loop variable
      INTEGER ICARD             ! Position of END card
      INTEGER IWCS              ! NDF WCS identifier
      INTEGER NCOMP             ! No. of components
      INTEGER NDIM              ! Number of dimensions
      INTEGER NOCCUR            ! Dummy
      LOGICAL BANNER            ! Part of the FITSIO banner header?
      LOGICAL CMPFND( NFLAGS )  ! True if certain special NDF components are present
      LOGICAL EXISTS            ! NDF component exists
      LOGICAL FITSPR            ! True if FITS extension is present
      LOGICAL LABFND            ! True if NDF LABEL found
      LOGICAL MANDAT            ! Not a mandatory header?
      LOGICAL ORIGIN            ! Starlink origin header present?
      LOGICAL THERE             ! END keyword present
      LOGICAL TITFND            ! True if NDF TITLE found
      LOGICAL UNTFND            ! True if NDF UNITS found

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Now create an initial character array.
      AVAIL = 1024
      NHEAD = 0
      NCOMP = 0
      CALL PSX_MALLOC( AVAIL * SZFITS, IPHEAD, STATUS )
      CALL RTD1_WRFTC( 'END', ' ', ' ', IPHEAD, NHEAD, AVAIL,
     :                 STATUS )

*  Write special header cards.
*  ===========================

*  Write out the few special headers irrespective of the presence or
*  otherwise of a FITS extension in the NDF.  These are:
*    SIMPLE, EXTEND, PCOUNT, GCOUNT --- all take their default values.
*    BITPIX, NAXIS, NAXISn --- are derived directly from the NDF data
*      array;
*    OBJECT, LABEL, BUNIT --- the values held in NDF TITLE, LABEL,
*      and UNITS respectively are used if present, otherwise any values
*      found in the FITS extension are used.
*    DATE --- is created automatically.
*    BLANK --- is created from the bad value (includes floating point!).
*
      CALL RTD1_WNDFH( NDF, IPHEAD, NHEAD, AVAIL, BITPIX, CMPFND,
     :                 STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Write the NDF identifier into the header (this is only a temporary
*  FITS structure so this shouldn't have any other effects).
      CALL RTD1_WRFTI( 'NDFID', NDF, 'NDF identifier (volatile)',
     :                 IPHEAD, NHEAD, AVAIL, STATUS )

*  If available encode any NDF WCS components into the headers. This
*  should be performed before the FITS headers as we want to make sure
*  that this is always used in preference to any native encodings
*  already in the header.

*  If the WCS is corrupt for some reason just ignore it (this can happen
*  with incompatible encodings generated by new-v-old versions of AST).
*  The error is flushed so that some indication of the problem is seen.
      EXISTS = .FALSE.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ERR_MARK
         CALL NDF_STATE( NDF, 'WCS', EXISTS, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( STATUS )
            EXISTS = .FALSE.
         END IF
         CALL ERR_RLSE
      END IF
      IF ( EXISTS ) THEN
         CALL ERR_MARK
         CALL NDF_GTWCS( NDF, IWCS, STATUS )
         CALL RTD1_ENWCS( IWCS, IPHEAD, NHEAD, AVAIL, STATUS )
         CALL AST_ANNUL( IWCS, STATUS )

*  Error in the AST description (-INF values, or formatting).
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            EXISTS = .FALSE.
         END IF
         CALL ERR_RLSE
      END IF

*  Deal with the FITS extension that is present.
*  =============================================

*  Check for presence of NDF FITS extension.
      CALL NDF_XSTAT( NDF, 'FITS', FITSPR, STATUS )
      ORIGIN = .FALSE.
      IF ( FITSPR ) THEN

*  Proceed to merge the headers in the FITS extension into the
*  FITS-file header.  Some items should be ignored including those
*  already set above except:
*    Any existing ORIGIN cards are kept (the Starlink one is added
*    otherwise).
*  In addition:
*    XTENSION, BLANK and END --- are not propagated from the extension.
*      The first will be set for any extension.  The second may be
*      written as required dependent on the data type and any scaling.
*      The END card is written by FITSIO automatically once the header
*      is closed.

*  Use more obvious flags to indicate the certain items have been
*  written to the keywords already.
         TITFND = CMPFND( 1 )
         LABFND = CMPFND( 2 )
         UNTFND = CMPFND( 3 )

*  Obtain the number of dimensions of the NDF.
         CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  Deal with the items in the NDF FITS extension one by one.
         CALL NDF_XLOC( NDF, 'FITS', 'READ', FTLOC, STATUS )
         CALL DAT_SIZE( FTLOC, NCOMP, STATUS )

*  Locate the END card so we know where to start appending. If search
*  fails should be positioned at the end of the block.
         NOCCUR = 1
         THERE = .FALSE.
         ICARD = 0
         CALL RTD1_GKEYC( NHEAD, %VAL( CNF_PVAL( IPHEAD ) ), 0, 'END ',
     :                    NOCCUR, THERE, VALUE, ICARD, STATUS,
     :                    %VAL( CNF_CVAL( 80 ) ) )

*  Loop for each header in the NDF FITS extension.
         DO I = 1, NCOMP

*  Get a locator to successive elements in the FITS extension.
            CALL DAT_CELL( FTLOC, 1, I, FTLOCI, STATUS )

*  Read the FITS string, and extract the keyword and value.
            CALL DAT_GET0C( FTLOCI, FITSTR, STATUS )

*  Assume that it is a valid FITS header card.  Extract the keyword
*  and value.
            KEYWRD = FITSTR( 1:SZKEY )
            VALUE = FITSTR( 11:SZFITS )

*  Leave out SIMPLE, XTENSION, BITPIX, EXTEND, PCOUNT, GCOUNT, NAXIS,
*  NAXISn and possibly, OBJECT, LABEL, BUNIT, DATE, BLANK, and
*  END as described above. To avoid duplicate FITSIO banners these are
*  also omitted, as they are written when FITSIO creates the primary
*  headers.  Use an intermediate variable to reduce the number of
*  continuation lines in the test.  This combines tests for the
*  mandatory headers.
            MANDAT = ( KEYWRD .NE. 'SIMPLE' ) .AND.
     :               ( KEYWRD .NE. 'BITPIX' ) .AND.
     :               ( KEYWRD .NE. 'EXTEND' ) .AND.
     :               ( KEYWRD .NE. 'XTENSION' ) .AND.
     :               ( KEYWRD .NE. 'GCOUNT' ) .AND.
     :               ( KEYWRD .NE. 'PCOUNT' ) .AND.
     :               ( KEYWRD( 1:5 ) .NE. 'NAXIS' ) .AND.
     :               ( KEYWRD .NE. 'END' )

*  Use an intermediate variable to reduce the number of continuation
*  lines in the test.  This combines tests for the FITSIO FITS banner.
            BANNER = ( KEYWRD .EQ. 'COMMENT' ) .AND. (
     :               ( VALUE( 1:14 ) .EQ. 'FITS (Flexible' ) .OR.
     :               ( VALUE( 1:40 ) .EQ. 'Astrophysics Supplement '/
     :                 /'Series v44/p363,' ) .OR.
     :               ( VALUE( 1:31 ) .EQ. 'Contact the NASA Science '/
     :                 /'Office' ) .OR.
     :               ( VALUE( 1:39 ) .EQ. 'FITS Definition document '/
     :                 /'#100 and other' ) )

*  Test for an ORIGIN card, if this present then it is kept.
            IF ( KEYWRD .EQ. 'ORIGIN' ) ORIGIN = .TRUE.

*  Do the test whether to copy the FITS extension header into the output
*  FITS file's header.
            IF ( MANDAT .AND. .NOT. BANNER .AND.
     :        ( KEYWRD .NE. 'DATE' ) .AND.
     :        ( KEYWRD .NE. 'BLANK' ) .AND.
     :        ( KEYWRD .NE. 'BSCALE' ) .AND.
     :        ( KEYWRD .NE. 'BZERO' ) .AND.
     :        ( KEYWRD .NE. 'EXTNAME' ) .AND.
     :        ( KEYWRD .NE. 'LABEL' .OR. .NOT. LABFND ) .AND.
     :        ( KEYWRD .NE. 'BUNIT' .OR. .NOT. UNTFND ) .AND.
     :        ( KEYWRD .NE. 'OBJECT' .OR. .NOT. TITFND ) .AND.
     :        ( KEYWRD( 1:6 ) .NE. 'LBOUND' ) )
     :      THEN

*  Write the header card, replacing any non-printing characters
*  with blanks (yes people do this).
               CALL CHR_CLEAN( FITSTR )
               CALL GAI1_INCRD( FITSTR, IPHEAD, NHEAD, ICARD, AVAIL,
     :                          STATUS )
               ICARD = ICARD + 1
            END IF
            CALL DAT_ANNUL( FTLOCI, STATUS )
         END DO

*  Append an 'END' card (overwrites existing if not headers added).
         FITSTR = 'END'
         CALL GAI1_INCRD( FITSTR, IPHEAD, NHEAD, ICARD, AVAIL,
     :                    STATUS )

         CALL DAT_ANNUL( FTLOC, STATUS )
      END IF

*  If the NDF WCS component doesn't exist (if it does we know there's a
*  WCS encoded so we need to do nothing more), then check the FITS
*  headers for a WCS. If that isn't found, then we just pick up the
*  default NDF WCS (GRID, AXIS and PIXEL) and encode that.
      IF ( .NOT. EXISTS .AND. STATUS .EQ. SAI__OK ) THEN

*  Read the FITS headers looking for a WCS. If the headers are bad for
*  any reason this can fail, so trap any errors.
         CALL RTD1_DEWCS( %VAL( CNF_PVAL( IPHEAD ) ), NHEAD, .FALSE.,
     :                    IWCS, STATUS, %VAL( CNF_CVAL( 80 ) ) )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF

*  Fall-back to basic NDF WCS.
         IF ( IWCS .EQ. AST__NULL ) THEN
            CALL ERR_MARK
            CALL NDF_GTWCS( NDF, IWCS, STATUS )
            CALL RTD1_ENWCS( IWCS, IPHEAD, NHEAD, AVAIL, STATUS )

*  Trap errors reading the NDF WCS. Happens reading incompatible AST
*  versions. In this case we have no WCS.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
               EXISTS = .FALSE.
            END IF
            CALL ERR_RLSE
         END IF

*  Free the WCS. May be in a poor state after all the above, so trap any
*  errors.
         CALL ERR_MARK
         CALL AST_ANNUL( IWCS, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF
         CALL ERR_RLSE
      END IF

*  If no FITS headers or no existing ORIGIN card then add the Starlink
*  ORIGIN card.
      IF ( .NOT. ORIGIN ) THEN
         CALL RTD1_WRFTC( 'ORIGIN', 'Starlink Project, U.K.',
     :               'Origin of this FITS file', IPHEAD, NHEAD,
     :               AVAIL, STATUS )
      END IF

 999  CONTINUE

      END
