      SUBROUTINE RTD1_WNDFH( NDF, IPHEAD, NHEAD, AVAIL, BITPIX,
     :                       CMPTHE, STATUS )
*+
*  Name:
*     RTD1_WNDFH

*  Purpose:
*     Writes out the mandatory and axis FITS header cards derived from
*     an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RTD1_WNDFH( NDF, IPHEAD, NHEAD, AVAIL, BITPIX, CMPTHE, STATUS )

*  Description:
*     This routine writes the mandatory and the optional but reserved
*     header cards into a primary header and data unit.  The values for
*     the keywords of the header cards are determined from the supplied
*     NDF.  The routine records which of the optional headers cards are
*     defined.

*     The keywords are:
*        o  NAXIS, and NAXISn are derived from the dimensions of
*           the NDF data array.
*        o  For an NDF whose origin is not the default LBOUNDn
*           cards are written. (These are not part of the standard.)
*        o  The OBJECT, LABEL, and BUNIT keywords are derived from
*           the TITLE, LABEL, and UNITS NDF components.
*        o  The CDELTn, CRPIXn, CRVALn keywords are derived from a
*           set of linear NDF AXIS structures.  Whenever the NDF AXIS
*           structure contains units or a label CUNITn and CTYPEn
*           keywords are created repsectively.
*        o  DATE card is written.
*        o  A BLANK card is written using the standard bad value
*           corresponding to the type of the DATA array, but
*           only if BAD pixels are present.
*        o  Dummy BSCALE and BZERO cards are written with values 1.0 and
*           0.0 respectively.
*        o  The standard order of the FITS keywords is preserved.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF.
*     IPHEAD = INTEGER (Given and Returned)
*        Pointer to the FITS block.
*     NHEAD = INTEGER (Given and Returned)
*        The number of header cards used.
*     AVAIL = INTEGER (Given and Returned)
*        The number of header cards available.
*     BITPIX = INTEGER (Given)
*        The BITPIX value for the output FITS file.
*     CMPTHE( NFLAGS ) = LOGICAL (Returned)
*        The flags when set to true indicate that certain optional NDF
*        components have been used to write descriptors to the NDF.
*        In order they are 1) TITLE, 2) LABEL, and 3) UNITS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998-2000 Central Laboratory of the Research Councils
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
*     AJC: Alan J. Chipperfield (STARLINK)
*     PWD: Peter W. Draper (STARLINK, Durham University)
*     {enter_new_authors_here}

*  History:
*     1994 May 31 (MJC):
*        Original version.
*     1995 Nov 21 (AJC):
*        Remove unused NBYTES
*     1996 Nov 22 (PWD):
*        Converted for GAIA/RTD.
*     2000 Nov 23 (PWD):
*        Removed ORIGIN card, this is now added later, if an existing
*        one isn't found.
*     2000 Dec 08 (PWD):
*        Removed axis encoding. This is now part of standard NDF WCS
*        component.
*     {enter_any_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE               ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard SAE constants
      INCLUDE 'NDF_PAR'           ! NDF_ public constants
      INCLUDE 'PRM_PAR'           ! PRIMDAT public constants

*  Arguments Given:
      INTEGER   NDF               ! NDF identifier
      INTEGER   IPHEAD            ! Logical-unit number of FITS file
      INTEGER   NHEAD             ! Number of header cards used
      INTEGER   AVAIL             ! Number of header cards available
      INTEGER   BITPIX            ! Bits per pixel

*  Arguments Returned:
      LOGICAL CMPTHE( 3 )

*  Status:
      INTEGER STATUS              ! Global status

*  Local Constants:
      LOGICAL EXTEND              ! May contain FITS extensions
      PARAMETER( EXTEND = .TRUE. )

      INTEGER   FITSOK            ! Good status for FITSIO library
      PARAMETER( FITSOK = 0 )

      INTEGER   GCOUNT            ! Value of FITS GCOUNT keyword
      PARAMETER( GCOUNT = 1 )

      INTEGER   PCOUNT            ! Value of FITS PCOUNT keyword
      PARAMETER( PCOUNT = 0 )

      LOGICAL SIMPLE              ! Standard FITS used
      PARAMETER( SIMPLE = .TRUE. )

      INTEGER   SZKEY             ! Length of keywords
      PARAMETER( SZKEY = 8 )

      INTEGER   SZVAL             ! Length of header values
      PARAMETER( SZVAL = 68 )

*  Local Variables:
      CHARACTER C*1             ! Accommodates character string
      CHARACTER KEYWRD * ( SZKEY ) ! Accommodates keyword name
      CHARACTER VALUE * ( SZVAL ) ! Accommodates keyword value
      INTEGER DIMS( NDF__MXDIM ) ! NDF dimensions (axis length)
      INTEGER I                 ! Loop variable
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER NCHAR             ! Length of a character string
      INTEGER NDIM              ! Number of dimensions
      INTEGER NSIG              ! Number of significant dimensions
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      LOGICAL BAD               ! Data array has BAD pixels
      LOGICAL DEFORG            ! True if NDF pixel origins are all 1
      LOGICAL LABFND            ! True if NDF LABEL found
      LOGICAL THERE             ! True if NDF has FITS extension
      LOGICAL TITFND            ! True if NDF TITLE found
      LOGICAL UNTFND            ! True if NDF UNITS found

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise output flags.
*  ========================
      TITFND = .FALSE.
      LABFND = .FALSE.
      UNTFND = .FALSE.

*  Inquire the NDF's shape.
*  ========================
*
*  Obtain the NDF bounds.
      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Write the mandatory headers.
*  ============================
      CALL NDF_BAD( NDF, 'DATA', .FALSE., BAD, STATUS )
      CALL RTD1_SNDFH( NDF, BITPIX, BAD, NHEAD, IPHEAD, AVAIL, STATUS )

*  Write cards for the pixel origin.
*  =================================
*
*  First see if the origin of all the axes are pixel 1.
      DEFORG = .TRUE.
      DO I = 1, NDIM
         DEFORG = DEFORG .AND. LBND( I ) .EQ. 1
      END DO

*  Write cards when the array is not at the default origin.
      IF ( .NOT. DEFORG ) THEN
         DO I = 1, NDIM

*  Form the keyword name.  Note this is not a standard keyword.  If a
*  convention or de facto keyword name becomes ccommon for this
*  parameter, this keyword should be changed accordingly.
            KEYWRD = ' '
            CALL CHR_ITOC( I, C, NCHAR )
            KEYWRD = 'LBOUND'//C( 1:NCHAR )

*  Write the actual card.
            CALL RTD1_WRFTI( KEYWRD, LBND( I ),
     :           'Pixel origin along axis '//C, IPHEAD, NHEAD, AVAIL,
     :           STATUS )
         END DO

*  The origins that are actually applicable may differ from these
*  values, if the NDF isn't 2D. In that case we need to record the
*  origins of the significant dimensions. Use the "ELBOUND" keyword.
         IF ( NDIM .GT. 2 ) THEN
            CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )
            NSIG = 0
            DO I = 1, NDIM
               IF ( DIMS( I ) .GT. 1 ) THEN
                  KEYWRD = ' '
                  IF ( NSIG .EQ. 0 ) THEN
                     KEYWRD = 'ELBOUND1'
                     CALL RTD1_WRFTI( KEYWRD, LBND( I ),
     :                                'Origin along effective axis 1',
     :                                IPHEAD, NHEAD, AVAIL, STATUS )
                  ELSE IF ( NSIG .EQ. 1 ) THEN
                     KEYWRD = 'ELBOUND2'
                     CALL RTD1_WRFTI( KEYWRD, LBND( I ),
     :                                'Origin along effective axis 2',
     :                                IPHEAD, NHEAD, AVAIL, STATUS )
                  END IF
                  NSIG = NSIG + 1
               END IF
            END DO
         END IF
      END IF

*  Process the title.
*  ==================
*
*  Determine whether or not there is a title present in the NDF.
      CALL NDF_STATE( NDF, 'TITLE', THERE, STATUS )

*  If an NDF title is found, this is copied to the FITS header card
*  with keyword OBJECT.
      IF ( THERE ) THEN
         CALL NDF_CGET( NDF, 'TITLE', VALUE, STATUS )
         CALL NDF_CLEN( NDF, 'TITLE', NCHAR, STATUS )
         KEYWRD = 'OBJECT  '

*  Remove unprintable characters that are not permitted in FITS.
         CALL CHR_CLEAN( VALUE )

*  Write the TITLE card to the FITS header.  68 is the maximum number of
*  characters that can be accommodated in a header card.
         CALL RTD1_WRFTC( KEYWRD, VALUE( :MIN( SZVAL, NCHAR ) ),
     :        'Title of the dataset', IPHEAD, NHEAD, AVAIL, STATUS )

*  Record the fact that the title has been written.
         TITFND = .TRUE.
      END IF

*  Process the label.
*  ==================
*
*  Determine whether or not there is a label present in the NDF.
      CALL NDF_STATE( NDF, 'LABEL', THERE, STATUS )

*  If an NDF label is found, this is copied to the FITS header card
*  with keyword LABEL.
      IF ( THERE ) THEN
         CALL NDF_CGET( NDF, 'LABEL', VALUE, STATUS )
         CALL NDF_CLEN( NDF, 'LABEL', NCHAR, STATUS )
         KEYWRD = 'LABEL   '

*  Remove unprintable characters that are not permitted in FITS.
         CALL CHR_CLEAN( VALUE )

*  Write the LABEL card to the FITS header.  68 is the maximum number of
*  characters that can be accommodated in a header card.
         CALL RTD1_WRFTC( KEYWRD, VALUE( :MIN( SZVAL, NCHAR ) ),
     :        'Label of the primary array', IPHEAD, NHEAD, AVAIL,
     :        STATUS )

*  Record the fact that the label has been written.
         LABFND = .TRUE.
      END IF

*  Process the units.
*  ==================
*
*  Determine whether or not there is a label present in the NDF.
      CALL NDF_STATE( NDF, 'UNITS', THERE, STATUS )

*  If an NDF label is found, this is copied to the FITS header card
*  with keyword BUNIT.
      IF ( THERE ) THEN
         CALL NDF_CGET( NDF, 'UNITS', VALUE, STATUS )
         CALL NDF_CLEN( NDF, 'UNITS', NCHAR, STATUS )
         KEYWRD = 'BUNIT   '

*  Remove unprintable characters that are not permitted in FITS.
         CALL CHR_CLEAN( VALUE )

*  Write the BUNIT card to the FITS header.  68 is the maximum number
*  of characters that can be accommodated in a header card.
         CALL RTD1_WRFTC( KEYWRD, VALUE( :MIN( SZVAL, NCHAR ) ),
     :        'Units of the primary array', IPHEAD, NHEAD, AVAIL,
     :        STATUS )

*  Record the fact that the title has been written.
         UNTFND = .TRUE.
      END IF

*  Write scale and offset cards.
*  ===================================

      CALL RTD1_WRFTD( 'BSCALE', 1.0D0,
     :     'True_value = BSCALE * FITS_value + BZERO', IPHEAD, NHEAD,
     :     AVAIL, STATUS )

      CALL RTD1_WRFTD( 'BZERO', 0.0D0,
     :     'True_value = BSCALE * FITS_value + BZERO', IPHEAD, NHEAD,
     :     AVAIL, STATUS )

      CMPTHE( 1 ) = TITFND
      CMPTHE( 2 ) = LABFND
      CMPTHE( 3 ) = UNTFND

 99   CONTINUE
      END
