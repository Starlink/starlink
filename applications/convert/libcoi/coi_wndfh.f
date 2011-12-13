      SUBROUTINE COI_WNDFH( NDF, COMP, IMDESC, NFLAGS, BITPIX, CMPTHE,
     :                      STATUS )
*+
*  Name:
*     COI_WNDFH

*  Purpose:
*     Writes character and axis IRAF headers derived from standard
*     components of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COI_WNDFH( NDF, COMP, IMDESC, NFLAGS, BITPIX, CMPTHE,
*                     STATUS )

*  Description:
*     This routine writes headers to an OIF file.  The values for the
*     headers are derived from standard components in the supplied NDF,
*     including the axis centres and character components like the
*     units.  The routine records which of the optional headers cards
*     are defined.
*
*     The keywords are:
*        o  For an NDF whose origin is not 1 along each axis, LBOUNDn
*           cards are written. (These are not part of the standard.)
*        o  The TITLE, LABEL, and BUNIT keywords are derived from
*           the title, label, and units NDF components.
*        o  The IRAF mini world co-ordinate system (MWCS) is used to
*           record axis information whenever one of the following
*           criteria is satisfied:
*
*           1) the dataset has some linear axes (system=world);
*
*           2) the dataset is one-dimensional with a non-linear axis or
*              is two-dimensional with the first axis non-linear and the
*              second being some aperture number or index
*              (system=multispec);
*
*           3) the dataset has a linear spectral/dispersion axis along
*              the first dimension and all other dimensions are pixel
*              indices (system=equispec).
*
*           See routine COI_AXEXP for more details.
*        o  For integer DATA types a BLANK card is written using the
*           standard bad value corresponding to the type of the FITS
*           array (except for the QUALITY array).

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF.
*     COMP = CHARACTER * ( * ) (Given)
*        The array component to write to the HDU.
*     IMDESC = INTEGER (Given)
*        The IRAF IMFORT image descriptor.
*     NFLAGS = INTEGER (Given)
*        The number of flags used to indicate that certain NDF
*        components have been used to write header cards to the IRAF
*        header file.  It should be set to 6.
*     BITPIX = INTEGER (Given)
*        The BITPIX value for the output IRAF header file.
*     CMPTHE( NFLAGS ) = LOGICAL (Returned)
*        The flags when set to true indicate that certain optional NDF
*        components have been used to write descriptors to the NDF.
*        In order they are 1) CRVARn, CDELTn, and CRPIXn 2) CUNITn,
*        3) CTYPEn, 4) TITLE, 5) LABEL, and 6) UNITS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
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
*     {enter_new_authors_here}

*  History:
*     1997 March 25 (MJC):
*        Original version based upon COI_WNDFH.
*     2009 June 29 (MJC):
*        Replace cloned CON_FKEYx with KAPLIBS FTS1_WKEYx.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants

*  Arguments Given:
      INTEGER NDF                ! NDF identifier
      CHARACTER * ( * ) COMP     ! The array component
      INTEGER IMDESC             ! Logical-unit number of FITS file
      INTEGER NFLAGS             ! Number of flags to indicate
                                 ! presence of certain components
      INTEGER BITPIX             ! Bits per pixel

*  Arguments Returned:
      LOGICAL CMPTHE( NFLAGS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      LOGICAL EXTEND             ! May contain FITS extensions
      PARAMETER( EXTEND = .TRUE. )

      INTEGER GCOUNT             ! Value of FITS GCOUNT keyword
      PARAMETER( GCOUNT = 1 )

      INTEGER IMOK               ! Good status for IMFORT
      PARAMETER( IMOK = 0 )

      INTEGER PCOUNT             ! Value of FITS PCOUNT keyword
      PARAMETER( PCOUNT = 0 )

      LOGICAL SIMPLE             ! Standard FITS used
      PARAMETER( SIMPLE = .TRUE. )

      INTEGER SZKEY              ! Length of keywords
      PARAMETER( SZKEY = 8 )

      INTEGER SZVAL              ! Length of header values
      PARAMETER( SZVAL = 68 )

*  Local Variables:
      LOGICAL AXIFND             ! NDF contains linear axis centres?
      LOGICAL AXIPRE             ! NDF has axis structure?
      LOGICAL AXLFND             ! NDF contains axis label?
      LOGICAL AXUFND             ! NDF contains axis units?
      INTEGER BLANK              ! Data blank value
      CHARACTER C*1              ! Accommodates character string
      LOGICAL DEFORG             ! NDF pixel origins are all 1?
      INTEGER DIMS( NDF__MXDIM ) ! NDF dimensions (axis length)
      CHARACTER HEADER * ( 80 )  ! FITS-like header
      INTEGER I                  ! Loop variable
      INTEGER ISTAT              ! IMFORT status
      CHARACTER KEYWRD * ( SZKEY ) ! Accommodates keyword name
      LOGICAL LABFND             ! NDF LABEL found?
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER NCHAR              ! Length of a character string
      INTEGER NDIM               ! Number of dimensions
      CHARACTER * ( 9 ) SYSTEM   ! MWCS system used
      LOGICAL THERE              ! NDF has FITS extension?
      LOGICAL TITFND             ! NDF TITLE found?
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      LOGICAL UNTFND             ! NDF UNITS found?
      CHARACTER VALUE * ( SZVAL ) ! Accommodates keyword value

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise output flags.
*  ========================
      AXIFND = .FALSE.
      AXLFND = .FALSE.
      AXUFND = .FALSE.
      TITFND = .FALSE.
      LABFND = .FALSE.
      UNTFND = .FALSE.

*  Inquire the NDF's shape.
*  ========================
*
*  Obtain the NDF dimensions.
      CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  Obtain the NDF bounds.
      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Handle axis header cards.
*  =========================
      CALL NDF_STATE( NDF, 'Axis', AXIPRE, STATUS )
      IF ( AXIPRE ) CALL COI_AXEXP( NDF, IMDESC, SYSTEM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Record whether or not an axis system was found.
      IF ( SYSTEM .NE. 'NONE' ) THEN
         AXIFND = .TRUE.
         AXLFND = .TRUE.
         AXUFND = .TRUE.
      END IF

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
            CALL IMAKWI( IMDESC, KEYWRD, LBND( I ),
     :                   'Pixel origin along axis '//C, ISTAT )
         END DO

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( ISTAT .NE. IMOK ) THEN
            CALL COI_FIOER( ISTAT, 'COI_WNDFH_ERR2', 'IMAKWI',
     :                      'Error writing LBOUNDn header cards.',
     :                      STATUS )
            GOTO 999
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

*  Write the TITLE card to the OIF header.  68 is the maximum number of
*  characters that can be accommodated in a header card.  We need to
*  use the IMPKWC() routine to set an already existing keyword and
*  standard header keywords.
         CALL IMPKWC( IMDESC, 'i_title',
     :                VALUE( :MIN( SZVAL, NCHAR ) ), ISTAT )

*  Record the fact that the title has been written.
         TITFND = .TRUE.
      END IF

*  Process the label.
*  ==================
*
*  Determine whether or not there is a label present in the NDF.
      CALL NDF_STATE( NDF, 'LABEL', THERE, STATUS )

*  If an NDF label is found, this is copied to the OIF header card
*  with keyword LABEL.
      IF ( THERE ) THEN
         CALL NDF_CGET( NDF, 'LABEL', VALUE, STATUS )
         CALL NDF_CLEN( NDF, 'LABEL', NCHAR, STATUS )
         KEYWRD = 'LABEL   '

*  Remove unprintable characters that are not permitted in FITS.
         CALL CHR_CLEAN( VALUE )

*  Write the LABEL card to the FITS header.  68 is the maximum number of
*  characters that can be accommodated in a header card.
*  Create the header (there is no comment) and append it to the headers.
         CALL FTS1_WKEYC( KEYWRD, VALUE( :MIN( SZVAL, NCHAR ) ),
     :                    'Label of the primary array', ' ', .FALSE.,
     :                    HEADER, STATUS )
         IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )

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
         KEYWRD = 'BUNIT  '

*  Remove unprintable characters that are not permitted in FITS.
         CALL CHR_CLEAN( VALUE )

*  Write the BUNIT card to the FITS header.  68 is the maximum number
*  of characters that can be accommodated in a header card.
         CALL FTS1_WKEYC( KEYWRD, VALUE( :MIN( SZVAL, NCHAR ) ),
     :                    'Units of the primary array', ' ', .FALSE.,
     :                    HEADER, STATUS )
         IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )

*  Record the fact that the title has been written.
         UNTFND = .TRUE.
      END IF

*  Handle a bad status.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'COI_WNDFH_ERR3',
     :     'Error writing an NDF character component to a header card.',
     :     STATUS )
         GOTO 999
      END IF

*  Write a dummy blank keyword.
*  ============================
      IF ( BITPIX .GT. 0 .AND. COMP .NE. 'QUALITY' ) THEN

*  Write the BLANK card to the OIF header.  The actual value may be
*  changed later.  This just reserves a place in the header.  This
*  assumes that the user has not have set up private bad values in the
*  NDF and set BLANK in the NDF's FITS extension.
         IF ( BITPIX .EQ. 32 ) THEN
            BLANK = VAL__BADI

         ELSE IF ( BITPIX .EQ. 16 ) THEN
            BLANK = VAL__BADW

         ELSE IF ( BITPIX .EQ. 8 ) THEN
            BLANK = VAL__BADUB

         END IF
         CALL FTS1_WKEYI( 'BLANK', BLANK, '/', 'Bad value', HEADER,
     :                    STATUS )
         IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, HEADER )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'COI_WNDFH_ERR6',
     :        'Error writing the BLANK header card.', STATUS )
            GOTO 999
         END IF

      END IF

  999 CONTINUE

*  Set the array of flags indicating the presence or not of certain
*  NDF components.
      CMPTHE( 1 ) = AXIFND
      CMPTHE( 2 ) = AXLFND
      CMPTHE( 3 ) = AXUFND
      CMPTHE( 4 ) = TITFND
      CMPTHE( 5 ) = LABFND
      CMPTHE( 6 ) = UNTFND

      END
