      SUBROUTINE COI_WHEAD( NDF, COMP, IMDESC, BITPIX, PROPEX, STATUS )
*+
*  Name:
*     COI_WHEAD

*  Purpose:
*     Writes the FITS header derived from an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COI_WHEAD( NDF, COMP, IMDESC, BITPIX, PROPEX, STATUS )

*  Description:
*     This routine creates the headers in the output IMFORT OIF file
*     based upon information in an NDF.
*
*     There are two stages:
*     a) Inquire of the NDF its shape, type, character components,
*     and axis components; and write these to the header.
*     b) Look for a FITS extension if requested to do so.  If one is
*     present append the headers contained therein to the OIF header
*     section, but not replacing any of the headers created in stage
*     a).

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF.
*     COMP = CHARACTER * ( * ) (Given)
*        The array component to write to the HDU.
*     IMDESC = INTEGER (Given)
*        The IRAF IMFORT image descriptor.
*     BITPIX = INTEGER (Given)
*        The BITPIX value for the output OIF file.
*     PROPEX = LOGICAL (Given)
*        The NDF FITS extension, if present, is folded into the output
*        FITS header when PROPEX is .TRUE..  When PROPEX is .FALSE.,
*        the FITS extension is ignored.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The rules for the conversion for ancillary information written to
*     the OIF ".imh" header file in FITS-like headers are as follows:
*     -  The IRAF mini world co-ordinate system (MWCS) is used to
*     record axis information whenever one of the following criteria is
*     satisfied:
*
*        1) the dataset has some linear axes (system=world);
*
*        2) the dataset is one-dimensional with a non-linear axis, or is
*           two-dimensional with the first axis non-linear and the
*           second being some aperture number or index
*           (system=multispec);
*
*        3) the dataset has a linear spectral/dispersion axis along the
*           first dimension and all other dimensions are pixel indices
*           (system=equispec).
*
*     -  The NDF title, label, units are written to the header keywords
*     TITLE, OBJECT, and BUNIT respectively if they are defined.
*     Otherwise anys values for these keywords found in the FITS
*     extension are used (provided parameter PROFITS is TRUE).  There
*     is a limit of twenty characters for each.
*     _  The NDF pixel origins are stored in keywords LBOUNDn for the
*     nth dimension when any of the pixel origins is not equal to 1.
*     -  Keywords HDUCLAS1, HDUCLASn are set to "NDF" and the
*     array-component name respectively.
*     -  The BLANK keyword is set to the Starlink standard bad value,
*     but only for the _WORD data type and not for a quality array.  It
*     appears regardless of whether or not there are bad values
*     actually present in the array.
*     -  HISTORY headers are propagated from the FITS extension when
*     PROFITS is TRUE, and from the NDF history component when PROHIS
*     is TRUE.
*     -  If there is a FITS extension in the NDF, then the elements up
*     to the first END keyword of this are added to the `user area' of
*     the IRAF header file when PROFITS=TRUE.  However, certain
*     keywords are excluded: SIMPLE, NAXIS, NAXISn, BITPIX, EXTEND,
*     PCOUNT, GCOUNT, BSCALE, BZERO, END, and any already created from
*     standard components of the NDF listed above.
*     -  All other NDF components are ignored.
*
*     [optional_subroutine_items]...

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
*        Original version based upon COF_WHEAD.
*     1997 November 14 (MJC):
*        Filtered LBOUNDn keywords.
*     2009 June 29 (MJC):
*        Replace cloned CON_FKEYx with KAPLIBS FTS1_WKEYx.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data system constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants

*  Arguments Given:
      INTEGER NDF                ! NDF identifier
      CHARACTER * ( * ) COMP     ! The array component
      INTEGER IMDESC             ! Image descriptor of OIF file
      INTEGER BITPIX             ! Bits per pixel
      LOGICAL PROPEX             ! Propagate FITS extension?

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER IMOK               ! Good status for IMFORT
      PARAMETER( IMOK = 0 )

      INTEGER NFLAGS             ! Number of flags to indicate
                                 ! presence special NDF components
      PARAMETER( NFLAGS = 6 )

      INTEGER SZKEY              ! Length of keyword names
      PARAMETER( SZKEY = 8 )     ! Columns 1 to 8

      INTEGER SZFITS             ! Length of FITS string
      PARAMETER( SZFITS = 80 )

      INTEGER SZNVAL             ! Character length of numeric values
      PARAMETER( SZNVAL = 20 )   ! Columns 11 to 30

      INTEGER SZVAL              ! Length of keyword string values
      PARAMETER( SZVAL = 70 )    ! Columns 11 to 80

*  Local Variables:
      INTEGER ADIM               ! Axis loop counter
      LOGICAL AXIFND             ! NDF contains a linear axis comps?
      LOGICAL AXLFND             ! True if NDF contains axis label
      REAL AXROT                 ! Rotation angle of an axis
      LOGICAL AXUFND             ! NDF contains axis units?
      LOGICAL BANNER             ! Part of the FITSIO banner header?
      CHARACTER C*1              ! Accommodates character string
      CHARACTER CDELT * ( SZKEY ) ! Keyword name of CDELTn
      LOGICAL CMPFND( NFLAGS )   ! Certain special NDF
                                 ! components are present?
      CHARACTER CRPIX * ( SZKEY ) ! Keyword name of CRPIXn
      CHARACTER CRVAL * ( SZKEY ) ! Keyword name of CRVALn
      INTEGER DIMS( NDF__MXDIM ) ! NDF dimensions (axis length)
      LOGICAL FITSPR             ! FITS extension is present?
      CHARACTER FITSTR * ( SZFITS ) ! FITS string
      CHARACTER FTLOC * ( DAT__SZLOC ) ! Locator to NDF FITS extension
      CHARACTER FTLOCI * ( DAT__SZLOC ) ! Locator to element of NDF
                                 ! FITS extension
      INTEGER I                  ! Loop variable
      INTEGER ISTAT              ! IMFORT status
      INTEGER J                  ! Loop variable
      CHARACTER KEYWRD * ( SZKEY ) ! Accommodates keyword name
      LOGICAL LABFND             ! NDF LABEL found?
      LOGICAL MANDAT             ! Not a mandatory header?
      INTEGER NCHAR              ! Length of a character string
      INTEGER NCOMP              ! No. of components
      INTEGER NDIM               ! Number of dimensions
      LOGICAL ORIGIN             ! Starlink origin header present?
      LOGICAL ROTAX( DAT__MXDIM ) ! An axis is rotated in the
                                 ! FITS extension?
      LOGICAL TITFND             ! NDF TITLE found?
      LOGICAL UNTFND             ! NDF UNITS found?
      CHARACTER VALUE * ( SZVAL ) ! Accommodates keyword value

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write special header cards.
*  ===========================

*  Write out the few special headers irrespective of the presence or
*  otherwise of a FITS extension in the NDF.  These are:
*    SIMPLE, EXTEND, PCOUNT, GCOUNT --- all take their default values.
*    BITPIX, NAXIS, NAXISn --- are derived directly from the NDF data
*      array;
*    LBOUNDn --- the pixel lower bounds of an NDF whose origin is
*      not 1 along each axis.  (These are not part of the standard.)
*    CRVALn, CDELTn, CRPIXn, CTYPEn, CUNITn --- are derived from the
*      NDF axis structures if possible.  If no linear NDF axis
*      structures are present, the values in the NDF FITS extension are
*      copied.  If any are non-linear, all FITS axis information is
*      lost.
*    OBJECT, LABEL, BUNIT --- the values held in NDF TITLE, LABEL,
*      and UNITS respectively are used if present, otherwise any values
*      found in the FITS extension are used.
*    ORIGIN and DATE --- are created automatically.  However the former
*      may be overriden by the entry in the NDF extension.
*    BLANK --- is created for integer data types from the bad value.
*
      CALL COI_WNDFH( NDF, COMP, IMDESC, NFLAGS, BITPIX, CMPFND,
     :                STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Write classification and naming headers.
*  ========================================
      CALL FTS1_WKEYC( 'HDUCLAS1', 'NDF', '/',
     :                 'Starlink NDF (hierarchical n-dim format)',
     :                 .FALSE., FITSTR, STATUS )
      IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, FITSTR )

      CALL FTS1_WKEYC( 'HDUCLAS2', COMP, '/',
     :                  'Array component subclass',
     :                  .FALSE., FITSTR, STATUS )
      IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, FITSTR )

      IF ( COMP .NE. 'DATA' ) THEN

*  Write the NDF's component name.
        CALL FTS1_WKEYC( 'EXTNAME', COMP, '/', 'Array component',
     :                   .FALSE., FITSTR, STATUS )
         IF ( STATUS .EQ. SAI__OK ) CALL ADLINE( IMDESC, FITSTR )
      END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'COI_WHEAD_ERR3',
     :     'Error writing an EXTNAME or HDUCLASn header card.', STATUS )
         GOTO 999
      END IF

*  Deal with the FITS extension that is present.
*  =============================================

*  Check for presence of NDF FITS extension.
      CALL NDF_XSTAT( NDF, 'FITS', FITSPR, STATUS )
      IF ( FITSPR .AND. PROPEX ) THEN

*  Proceed to merge the headers in the FITS extension into the
*  OIF-file header.  Some items should be ignored including those
*  already set above except:
*    ORIGIN which may be overriden by the entry in the NDF extension.
*  In addition:
*    XTENSION, BLANK and END --- are not propagated from the extension.
*      The first will be set for any extension.  The second may be
*      written as required dependent on the data type and any scaling.
*      The END card is not written.

*  Use more obvious flags to indicate the certain items have been
*  written to the keywords already.
         AXIFND = CMPFND( 1 )
         AXLFND = CMPFND( 2 )
         AXUFND = CMPFND( 3 )
         TITFND = CMPFND( 4 )
         LABFND = CMPFND( 5 )
         UNTFND = CMPFND( 6 )

*  Obtain the number of dimensions of the NDF.
         CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  Initialise the flags that indicate a rotated axis.
         DO I = 1, NDIM
            ROTAX( I ) = .FALSE.
         END DO

*  Deal with the items in the NDF FITS extension one by one.
         CALL NDF_XLOC( NDF, 'FITS', 'READ', FTLOC, STATUS )
         CALL DAT_SIZE( FTLOC, NCOMP, STATUS )

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
*  NAXISn, and possibly LBOUNDn, CDELTn, CRVALn, CRPIXn, CRTYPEn,
*  CTYPEn, CUNITn, OBJECT, LABEL, BUNIT, DATE, BLANK, HDUCLASn, and END
*  as described above.  Note CROTAn are also excluded.  To avoid
*  duplicate FITSIO banners these are also omitted.
*
*  Use an intermediate variable to reduce the number of continuation
*  lines in the test.  This combines tests for the mandatory headers.
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

*  Use an intermediate variable to reduce the number of continuation
*  lines in the test.  This tests for the Starlink ORIGIN card.
            ORIGIN = KEYWRD .EQ. 'ORIGIN'
*            ORIGIN = ( KEYWRD .EQ. 'ORIGIN' ) .AND.
*     :               ( VALUE( 2:23 ) .EQ. 'Starlink Project, U.K.' )

*  Do the test whether to copy the FITS extension header into the output
*  OIF file's header.
            IF ( MANDAT .AND. .NOT. BANNER .AND. .NOT. ORIGIN .AND.
     :        ( KEYWRD .NE. 'DATE' ) .AND.
     :        ( KEYWRD .NE. 'BLANK' ) .AND.
     :        ( KEYWRD .NE. 'BSCALE' ) .AND.
     :        ( KEYWRD .NE. 'BZERO' ) .AND.
     :        ( KEYWRD .NE. 'EXTNAME' ) .AND.
     :        ( KEYWRD( 1:6 ) .NE. 'LBOUND' ) .AND.
     :        ( KEYWRD( 1:7 ) .NE. 'HDUCLAS' ) .AND.
     :        ( KEYWRD( 1:5 ) .NE. 'CDELT' .OR. .NOT. AXIFND ) .AND.
     :        ( KEYWRD( 1:5 ) .NE. 'CRVAL' .OR. .NOT. AXIFND ) .AND.
     :        ( KEYWRD( 1:5 ) .NE. 'CRPIX' .OR. .NOT. AXIFND ) .AND.
     :        ( KEYWRD( 1:6 ) .NE. 'CRTYPE' .OR. .NOT. AXLFND ) .AND.
     :        ( KEYWRD( 1:5 ) .NE. 'CTYPE' .OR. .NOT. AXLFND ) .AND.
     :        ( KEYWRD( 1:5 ) .NE. 'CUNIT' .OR. .NOT. AXUFND ) .AND.
     :        ( KEYWRD .NE. 'LABEL' .OR. .NOT. LABFND ) .AND.
     :        ( KEYWRD .NE. 'BUNIT' .OR. .NOT. UNTFND ) .AND.
     :        ( KEYWRD .NE. 'OBJECT' .OR. .NOT. TITFND ) ) THEN

*  Look for a rotated axis in the FITS extension (CROTAn is present and
*  non-zero).  If there is one, the NDF AXIS structure will contain
*  pixel co-ordinates, which are probably not the original co-ordinates
*  for the axis.  If there are rotated axes, the keywords defining the
*  axis will be re-written later using the values of CRVALn and CDELTn
*  in the FITS extension.  When there is no AXIS component in the NDF,
*  then the axis keywords can be written immediately to the output
*  OIF headers.
               IF ( INDEX( KEYWRD, 'CROTA' ) .NE. 0 .AND.
     :              AXIFND ) THEN
                  CALL CHR_CTOI( KEYWRD( 6: ), ADIM, STATUS )
                  CALL CHR_CTOR( VALUE( :SZNVAL ), AXROT, STATUS )
                  ROTAX( ADIM ) = ABS( AXROT ) .GT. VAL__EPSR
               ELSE

*  Write the header card.
                  CALL ADLINE( IMDESC, FITSTR )
               END IF

*  Just skip over non-valid cards.  Thus defective cards are not
*  propagated.  There is no warning, but users should check their
*  headers anyway, and we should aim to have header-validation
*  software.  ISAT no longer used, but retain this in case ADLINE is
*  given a status argument.
               IF ( ISTAT .GT. IMOK ) ISTAT = IMOK
            END IF
         END DO

*  Deal with rotated axes.
*  =======================

*  For each dimension check if there are any rotated axes when the
*  the NDF contains an AXIS component.
         DO J = 1, NDIM
            IF ( ROTAX( J ) ) THEN

*  Search through the FITS headers to find the values of CRVALn,
*  CDELTn, and CRPIXn.
               DO I = 1, NCOMP

*  Get locator to successive elements in the FITS extension.
                  CALL DAT_CELL( FTLOC, 1, I, FTLOCI, STATUS )

*  Read the FITS string, and extract the keyword.
                  CALL DAT_GET0C( FTLOCI, FITSTR, STATUS )
                  KEYWRD = FITSTR( 1:SZKEY )

*  Create the keywords being searched.
                  CALL CHR_ITOC( J, C, NCHAR )
                  CDELT = 'CDELT'//C(:1)
                  CRPIX = 'CRPIX'//C(:1)
                  CRVAL = 'CRVAL'//C(:1)

*  Test the current keyword.
                  IF ( ( INDEX( KEYWRD, CDELT ) .NE. 0 ) .OR.
     :                 ( INDEX( KEYWRD, CRPIX ) .NE. 0 ) .OR.
     :                 ( INDEX( KEYWRD, CRVAL ) .NE. 0 ) ) THEN

*  Write the header card.
                     CALL ADLINE( IMDESC, FITSTR )

                  END IF
               END DO
            END IF
         END DO
      END IF

  999 CONTINUE

      END
