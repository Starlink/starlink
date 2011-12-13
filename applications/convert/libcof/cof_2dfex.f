      SUBROUTINE COF_2DFEX( SNAME, LOC, FUNIT, STATUS )
*+
*  Name:
*     COF_2DFEX

*  Purpose:
*     Exports 2dF FIELD extension in an NDF to a FITS binary table.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_2DFEX( SNAME, LOC, FUNIT, STATUS )

*  Description:
*     This routine converts a 2dF FIELD extension witihn an NDF into
*     a binary table within the current FITS file.
*
*     To propagate the OBJECT substructure this routine creates a binary
*     table of constant width (224 bytes) with one row per fibre.  The
*     total number of rows is obtained from component NUM_FIBRES.  If a
*     possible OBJECT component is missing from the NDF, a null column
*     is written for that component.  The columns inherit the data
*     types of the OBJECT structure's components.  Column meanings and
*     units are assigned based upon information in the reference given
*     below.
*
*     The FIELD structure components are converted into additional
*     keywords of the same name in the binary-table header, with the
*     exception that components with names longer than 8 characters
*     have abbreviated keywords: UNALLOCxxx become UNAL-xxx (xxx= OBJ,
*     GUI, or SKY), CONFIGMJD becomes CONFMJD, and xSWITCHOFF become
*     xSWTCHOF (x=X or Y).  If any FIELD component is missing it is
*     ignored.
*
*     Extension-level and name keywords are written.

*  Arguments:
*     SNAME = CHARACTER * ( * ) (Given)
*        The name of structure.  It is used to form the EXTNAME
*        keyword.
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        Locator to the 2dF extension structure whose contents are to
*        be converted to binary tables.
*     FUNIT = INTEGER (Given)
*        The logical unit number of the output FITS file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  References:
*     Bailey, J.A. 1996,97, 2dF Software Report 14, versions 0.3, 0.5.
*
*     [optional_subroutine_items]...

*  Prior Requirements:
*     -  A primary HDU unit exists in the FITS file, and the file is
*     open.

*  Copyright:
*     Copyright (C) 1997-1998, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MNB: Mike N Birchall (AAO)
*     {enter_new_authors_here}

*  History:
*     1997 January 14 (MJC):
*        Original version.
*     1997 November 10 (MJC):
*        Added FILENAME to the FIELD structure.  Used latest version of
*        the OBJECT structure.
*     1998 January 8 (MJC):
*        Corrected null value for double-precision columns to NaN.
*        Set null value (as opposed to TNULLn) keywords for integer
*        columns.  Added TDISP cards for position, magnitude and
*        programme identification columns.
*     1998 August 13 (MJC):
*        No longer writes TNULLn keywords for non-integer columns,
*        so as not to violate the FITS standard.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2006 June 15 (MNB):
*        Added definitions for extra keywords relevent to "FIBRES_IFU"
*        table be included in the conversion.  Also added "FIBRES_IFU"
*        table column definitions.
*     2006 August 1 (MJC):
*        Modified some units strings to conform to the FITS standard.
*     2006 September 10 (MNB):
*        Added WLEN column and additional keyword WLNSUSD for standard
*        FIBRE table.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * ( * ) SNAME
      CHARACTER * ( * ) LOC
      INTEGER FUNIT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Effective string length

*  Local Constants:
      INTEGER   FITSOK           ! Good status for FITSIO library
      PARAMETER( FITSOK = 0 )

      INTEGER MXCSTD             ! Maximum number of components in the
                                 ! OBJECT structure or fields in the
                                 ! binary table in standard mode
      PARAMETER ( MXCSTD = 17 )

      INTEGER MXCIFU             ! Maximum number of components in the
                                 ! OBJECT structure or fields in the
                                 ! binary table associated with the IFU
      PARAMETER ( MXCIFU = 13 )

*  Local Variables:
      CHARACTER * ( 80 ) BUFFER  ! BUFFER for writing error messages
      CHARACTER * ( 80 ) CDUMMY  ! Dummy for reading TFORMn cards
      CHARACTER * ( DAT__SZLOC ) CLOC ! Locator to a structure component
      CHARACTER * ( 3 ) CN       ! Column number
      CHARACTER * ( DAT__SZNAM ) CNAME ! Component name
      CHARACTER * ( DAT__SZNAM ) CTYPE ! Component type
      CHARACTER * ( 8 ) CRDNAM   ! Header-card name
      CHARACTER * 80 CVALUE      ! Character value
      CHARACTER * ( 4 ) DSPFMT   ! TDISPn display format
      DOUBLE PRECISION DVALUE    ! D.p. value
      INTEGER EL                 ! Number of elements in mapped array
      CHARACTER * ( 68 ) EXTNAM  ! Name of the component
      CHARACTER * ( DAT__SZLOC ) FLOC ! Locator to FIELD structure
      CHARACTER * ( 256 ) FILE   ! Name of the HDS file (not used)
      INTEGER FSTAT              ! FITSIO status
      INTEGER I                  ! Loop through fields
      INTEGER IC                 ! Loop counter
      INTEGER IVALUE             ! Integer value
      CHARACTER * ( 8 ) KEYWRD   ! FITS keyword
      INTEGER LEL                ! Loop counter for filling column with
                                 ! null values
      INTEGER MXCOMP             ! Maximum number of components in the
                                 ! OBJECT structure of fields in the
                                 ! binary table
      INTEGER NC                 ! Number of characters
      INTEGER NCOMP              ! Number of components
      INTEGER NFIELD             ! Number of fields in table
      INTEGER NLEV               ! Number of hierarchical levels (not
                                 ! used)
      INTEGER NROW               ! Number of rows in table
      CHARACTER * 1 NULL         ! ASCII null character
      CHARACTER * ( DAT__SZLOC ) OLOC ! Locator to OBJECT structure
      INTEGER OPNTR              ! Pointer to mapped component
      CHARACTER * ( 6 ) ROUTIN   ! Name of the FITSIO routine used to
                                 ! copy data into the binary table
      INTEGER STRLEN             ! Number of characters in component
                                 ! element
      CHARACTER * ( 32 ) TDESC( MXCSTD ) ! Table field descriptions
      CHARACTER * ( 32 ) TDEIFU( MXCIFU ) ! IFU table descriptions
      CHARACTER * ( 3 ) TFORM( MXCSTD ) ! Binary-table field formats
      CHARACTER * ( 3 ) TFOIFU( MXCIFU ) ! IFU table field formats
      LOGICAL THERE              ! Component is present?
      CHARACTER * ( 13 ) TTYPE( MXCSTD ) ! Binary-table field names
      CHARACTER * ( 13 ) TTYIFU( MXCIFU ) ! IFU table field names
      CHARACTER * ( 13 ) TUNIT( MXCSTD ) ! Binary-table field units
      CHARACTER * ( 13 ) TUNIFU( MXCIFU ) ! IFU table field units


*  Local Data:
      DATA TDESC / 'Object name', 'J2000 mean Right Ascension',
     :             'J2000 mean Declination', 'X position', 'Y position',
     :             'X position error', 'Y position error',
     :             'Button angle', 'P (programme object) or S (sky)',
     :             'Pivot number', 'Object magnitude',
     :             'Programme identification', 'Comments',
     :             'Retractor name', 'A or B if beam switching',
     :             'Beam-switch partner fibre',
     :             'Configured wavelength'  /

      DATA TFORM / '80A', '1D', '1D', '1J', '1J', '1I', '1I', '1D',
     :             '1A', '1I', '1D', '1J', '80A', '10A', '1A', '1I',
     :             '1D' /

      DATA TTYPE / 'NAME', 'RA', 'DEC', 'X', 'Y', 'XERR', 'YERR',
     :             'THETA', 'TYPE', 'PIVOT', 'MAGNITUDE', 'PID',
     :             'COMMENT', 'RETRACTOR', 'SWITCHFIELD',
     :             'SWITCHPARTNER', 'WLEN' /

      DATA TUNIT / ' ', 'rad', 'rad', 'um', 'um', 'um', 'um', 'rad',
     :             ' ', ' ', 'mag', ' ', ' ', ' ', ' ', 'bin', 'A' /

      DATA TDEIFU / 'Spectrum Identifier',
     :              'Selection flag',
     :              'Number of Spaxels',
     :              'The length of the spectrum',
     :              'Currently always zero?',
     :              'Sky RA position',
     :              'Always 1',
     :              'Pivot number',
     :              'ID from fibres_ifu.txt',
     :              'X pos. within the spiral grid',
     :              'Y pos. within the spiral grid',
     :              'Type code either U, D, P or S',
     :              'Name' /

      DATA TFOIFU / '1J', '1J', '1J', '1J', '1J', '1D', '1D', '1J',
     :              '16A', '1J', '1J', '1A', '8A' /

      DATA TTYIFU / 'SPEC_ID', 'SELECTED', 'NSPAX', 'SPEC_LEN',
     :              'SPEC_STA', 'XPOS', 'YPOS', 'GROUP_N',
     :              'SPAX_ID', 'XGPOS', 'YGPOS', 'TYPE' , 'NAME' /

      DATA TUNIFU / ' ', ' ', ' ', ' ', ' ', 'arcsec',
     :              'arcsec', ' ', ' ', ' ', ' ', ' ', ' ' /

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Define the null character.
      NULL = CHAR( 0 )

*  Check if this is a IFU fibre table.
*  ===================================
      IF ( SNAME .EQ. 'FIBRES_IFU' ) THEN

*  We use the FIBRES_IFU data
          DO I = 1, MXCIFU
	      TDESC( I ) = TDEIFU( I )
	      TFORM( I ) = TFOIFU( I )
	      TTYPE( I ) = TTYIFU( I )
	      TUNIT( I ) = TUNIFU( I )
	  ENDDO
	  MXCOMP = MXCIFU
      ELSE
          MXCOMP = MXCSTD
      ENDIF

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Define the shape of the binary table.
*  =====================================

*  Get the value of NUM_FIBRES scalar primitive object.
      CALL CMP_GET0I( LOC, 'NUM_FIBRES', NROW, STATUS )

*  Create the binary table.
*  ========================

*  Create new header and data section.
      CALL FTCRHD( FUNIT, FSTAT )

*  Get the structure's path name and assign it to the extension name.
      CALL HDS_TRACE( LOC, NLEV, EXTNAM, FILE, STATUS )

*  Create binary-table header.  There are NROW rows in the table.
*  The "variable-length data area" has length of 0 bytes.
      CALL FTPHBN( FUNIT, NROW, MXCOMP, TTYPE, TFORM, TUNIT, EXTNAM,
     :             0, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_2DFEX_ERR1', 'FTPHBN',
     :                   'Error writing binary-table header.', STATUS )
         GOTO 999
      END IF

*  Replace the default comments with the 2dF descriptions.
*  =======================================================
      DO I = 1, MXCOMP

*  The comments appear on the TTYPEn cards.  Form the TTYPEn keyword,
*  and replace the comment.
         CALL FTKEYN( 'TTYPE', I, CRDNAM, FSTAT )
         CALL FTMCOM( FUNIT, CRDNAM, TDESC( I ), FSTAT )

*  Write the TNULLn cards for an integer column.
*  =============================================

*  Convert the column number into character form.
         CALL CHR_ITOC( I, CN, NC )

*  Form the name of the keyword which will immediately precede the
*  inserted TNULLn card.
         NC = 5
         CRDNAM = 'TFORM'
         CALL CHR_APPND( CN, CRDNAM, NC )

*  FITSIO does not permit cards to be placed after a named card;
*  it requires that we read that named card first.
         CALL FTGCRD( FUNIT, CRDNAM, CDUMMY, FSTAT )

*  Assign the bad/null value for each of the columns.
*  --------------------------------------------------

*  Form the name of the TNULLn keyword.
         NC = 5
         CRDNAM = 'TNULL'
         CALL CHR_APPND( CN, CRDNAM, NC )

*  Process by data type.  Note that the _DOUBLE bad values are replaced
*  by NaNs and don't have TNULLn cards.  Null character values are
*  recognised by the presence of the ASCII null as the first character.
         IF ( TFORM( I ) .EQ. '1J ') THEN

*  Insert the TNULLn card.
            CALL FTIKYJ( FUNIT, CRDNAM, VAL__BADI, 'Null value', FSTAT )

         ELSE IF ( TFORM( I ) .EQ. '1I' ) THEN

*  Insert the TNULLn card.  There is no FTIKYI routine so equate the
*  word bad value to an integer and create an integer-valued card.
            IVALUE = VAL__BADW
            CALL FTIKYJ( FUNIT, CRDNAM, IVALUE, 'Null value', FSTAT )

         END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
         IF ( FSTAT .GT. FITSOK ) THEN
            BUFFER = 'Error writing '//CRDNAM( :NC )//' card for a '/
     :               /'binary table.'
            CALL COF_FIOER( FSTAT, 'COF_2DFEX_ERR3', 'FTIKYx',
     :                      BUFFER, STATUS )
            GOTO 999
         END IF

*  Write the TDISPn cards for table viewers.
*  =========================================

*  In most cases default display formats are likely to be fine.  Only
*  the following certainly look wrong using CURSA.
         IF ( TTYPE( I ) .EQ. 'X' .OR.
     :        TTYPE( I ) .EQ. 'Y' .OR.
     :        TTYPE( I ) .EQ. 'MAGNITUDE' .OR.
     :        TTYPE( I ) .EQ. 'PID' ) THEN

*  Form the name of the keyword which will immediately precede the
*  inserted TNULLn card.
            NC = 5
            CRDNAM = 'TDISP'
            CALL CHR_APPND( CN, CRDNAM, NC )

*  These are the positions in microns.
            IF ( TTYPE( I ) .EQ. 'X' .OR. TTYPE( I ) .EQ. 'Y' ) THEN
               DSPFMT = 'I7'

*  The magnitudes.
            ELSE IF ( TTYPE( I ) .EQ. 'MAGNITUDE' ) THEN
               DSPFMT = 'F6.2'

            ELSE IF ( TTYPE( I ) .EQ. 'PID' ) THEN
               DSPFMT = 'I9'
            END IF

*  Insert the TDISPn card with values appropriate for the columns.
            CALL FTIKYS( FUNIT, CRDNAM, DSPFMT, 'Display format',
     :                   FSTAT )
         END IF
      END DO

*  Write extension-level header card.
*  ==================================

*  Set the extension level.
      CALL FTPKYJ( FUNIT, 'EXTLEVEL', 1, 'Level in the '/
     :             /'hierarchical structure', FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_2DFEX_ERR2', 'FTPKYJ',
     :     'Error writing extension level in header.', STATUS )
         GOTO 999
      END IF

*  Write additional header cards based upon the OBJECT structure.
*  ==============================================================

*  First write a blank link to separate these headers.
      CALL FTPREC( FUNIT, ' ', FSTAT )

*  Get a locator to the OBJECT structure.
      CALL DAT_FIND( LOC, 'OBJECT', OLOC, STATUS )

*  Determine how many components it contains.  This will be the number
*  of fields in the binary table.
      CALL DAT_NCOMP( OLOC, NFIELD, STATUS )

*  FIELD structure
*  ===============
*
*  The scalar primitive components of this structure are to be made
*  into FITS headers located in the binary table, using their names as
*  keywords, where that is possible.
*
*  Get a locator to the FIELD structure.
      CALL DAT_FIND( LOC, 'FIELD', FLOC, STATUS )

*  Determine how many components it contains.
      CALL DAT_NCOMP( FLOC, NCOMP, STATUS )

*  Loop for each component.
      DO IC = 1, NCOMP

*  Get a locator to the object.
         CALL DAT_INDEX( FLOC, IC, CLOC, STATUS )

*  Determine its name.  Define the FITS keyword, shortening long names
*  where necessary.
         CALL DAT_NAME( CLOC, CNAME, STATUS )
         IF ( CNAME( 1:7 ) .EQ. 'UNALLOC' ) THEN
            KEYWRD = 'UNAL-'//CNAME( 8:10 )
         ELSE IF ( CNAME .EQ. 'CONFIGMJD' ) THEN
            KEYWRD = 'CONFMJD'
         ELSE IF ( CNAME( 2:13 ) .EQ. 'SWITCHOFFSET' ) THEN
            KEYWRD = CNAME( 1:1 )//'SWTCHOF'
         ELSE
            KEYWRD = CNAME( 1:8 )
         END IF

*  Obtain the component's value using the documented data type for the
*  keyword; and write out a FITS header card using the keyword, and
*  the value.  As the number of decimal places (for floating-point
*  values) and the comment depends on the component, the code tests for
*  each recognised object.

*  CENRA
         IF ( KEYWRD .EQ. 'CENRA' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Field centre mean Right Ascension (rad)',
     :                   FSTAT )

*  CENDEC
         ELSE IF ( KEYWRD .EQ. 'CENDEC' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Field centre mean Declination (rad)',
     :                   FSTAT )

*  CENEQNX
         ELSE IF ( KEYWRD .EQ. 'CENEQNX' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, 2,
     :                   'Equinox of Field Centre (FK5 Julian)', FSTAT )


*  APPRA
         ELSE IF ( KEYWRD .EQ. 'APPRA' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Apparent Right Ascension (rad)', FSTAT )

*  APPDEC
         ELSE IF ( KEYWRD .EQ. 'APPDEC' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Apparent Declination (rad)', FSTAT )

*  APPEPOCH
         ELSE IF ( KEYWRD .EQ. 'APPEPOCH' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, 6,
     :                   'Epoch of Apparent co-ordinates', FSTAT )

*  CONFIGMJD
         ELSE IF ( KEYWRD .EQ. 'CONFMJD' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, 6,
     :                   'MJD configured for', FSTAT )

*  ACTMJD
         ELSE IF ( KEYWRD .EQ. 'ACTMJD' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, 3,
     :                   'Actual MJD of observation', FSTAT )

*  UNALLOCOBJ
         ELSE IF ( KEYWRD .EQ. 'UNAL-OBJ' ) THEN
            CALL DAT_GET0I( CLOC, IVALUE, STATUS )
            CALL FTPKYJ( FUNIT, KEYWRD, IVALUE,
     :                   'Number of unallocated objects', FSTAT )

*  UNALLOCGUI
         ELSE IF ( KEYWRD .EQ. 'UNAL-GUI' ) THEN
            CALL DAT_GET0I( CLOC, IVALUE, STATUS )
            CALL FTPKYJ( FUNIT, KEYWRD, IVALUE,
     :                   'Number of unallocated guide stars', FSTAT )

*  UNALLOCSKY
         ELSE IF ( KEYWRD .EQ. 'UNAL-SKY' ) THEN
            CALL DAT_GET0I( CLOC, IVALUE, STATUS )
            CALL FTPKYJ( FUNIT, KEYWRD, IVALUE,
     :                   'Number of unallocated skies', FSTAT )

*  ALLOCOBJ
         ELSE IF ( KEYWRD .EQ. 'ALLOCOBJ' ) THEN
            CALL DAT_GET0I( CLOC, IVALUE, STATUS )
            CALL FTPKYJ( FUNIT, KEYWRD, IVALUE,
     :                   'Number of allocated objects', FSTAT )

*  ALLOCGUI
         ELSE IF ( KEYWRD .EQ. 'ALLOCGUI' ) THEN
            CALL DAT_GET0I( CLOC, IVALUE, STATUS )
            CALL FTPKYJ( FUNIT, KEYWRD, IVALUE,
     :                   'Number of allocated guide stars', FSTAT )

*  ALLOCSKY
         ELSE IF ( KEYWRD .EQ. 'ALLOCSKY' ) THEN
            CALL DAT_GET0I( CLOC, IVALUE, STATUS )
            CALL FTPKYJ( FUNIT, KEYWRD, IVALUE,
     :                   'Number of unallocated skies', FSTAT )

*  XSWITCHOFFSET
         ELSE IF ( KEYWRD .EQ. 'XSWTCHOF' ) THEN
            CALL DAT_GET0I( CLOC, IVALUE, STATUS )
            CALL FTPKYJ( FUNIT, KEYWRD, IVALUE,
     :                   'Beam switch offset X (um)', FSTAT )

*  YSWITCHOFFSET
         ELSE IF ( KEYWRD .EQ. 'YSWTCHOF' ) THEN
            CALL DAT_GET0I( CLOC, IVALUE, STATUS )
            CALL FTPKYJ( FUNIT, KEYWRD, IVALUE,
     :                   'Beam switch offset Y (um)', FSTAT )

*  WLNSUSD
         ELSE IF ( KEYWRD .EQ. 'WLNSUSD' ) THEN
            CALL DAT_GET0I( CLOC, IVALUE, STATUS )
            CALL FTPKYJ( FUNIT, KEYWRD, IVALUE,
     :         'No of Fibres using the specific wavelength', FSTAT )

* -------------------------------
* AAOMEGA-IFU Non STRING KEYWORDS
* -------------------------------

*  ATMPRES
         ELSE IF ( KEYWRD .EQ. 'ATMPRES' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Atmospheric pressure (millibars)', FSTAT )

*  ATMHRUM
         ELSE IF ( KEYWRD .EQ. 'ATMHRUM' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Atmospheric relative humidity (percent)',
     :                   FSTAT )

*  ATMTEMP
         ELSE IF ( KEYWRD .EQ. 'ATMTEMP' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Atmospheric temperature (K)', FSTAT )

*  WLEN
         ELSE IF ( KEYWRD .EQ. 'WLEN' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Refraction wavelength (nm)', FSTAT )

*  TELMA
         ELSE IF ( KEYWRD .EQ. 'TELMA' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'AAT CCS MA value', FSTAT )

*  TELME
         ELSE IF ( KEYWRD .EQ. 'TELME' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'AAT CCS ME value', FSTAT )

*  TELNP
         ELSE IF ( KEYWRD .EQ. 'TELNP' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'AAT CCS NP value', FSTAT )

*  TELCH
         ELSE IF ( KEYWRD .EQ. 'TELCH' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'AAT CCS CH value', FSTAT )

*  TELHF
         ELSE IF ( KEYWRD .EQ. 'TELHF' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'AAT CCS HF value', FSTAT )

*  PA
         ELSE IF ( KEYWRD .EQ. 'PA' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Rotator Position Angle (deg)', FSTAT )

*  INST_ROT
         ELSE IF ( KEYWRD .EQ. 'INST_ROT' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Instrument mount rotation (deg)', FSTAT )

*  INSTOFFX
         ELSE IF ( KEYWRD .EQ. 'INSTOFFX' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Offset of instrument centre in X (um)',
     :                   FSTAT )

*  INSTOFFY
         ELSE IF ( KEYWRD .EQ. 'INSTOFFY' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Offset of instrument centre in Y (um)',
     :                   FSTAT )

*  TCRVL6
         ELSE IF ( KEYWRD .EQ. 'TCRVL6' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Reference pixel J2000 Right Ascension (deg)',
     :                   FSTAT )

*  TCRVL7
         ELSE IF ( KEYWRD .EQ. 'TCRVL7' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Reference pixel J2000 Declination (deg)',
     :                   FSTAT )

*  TCRPX6
         ELSE IF ( KEYWRD .EQ. 'TCRPX6' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Reference pixel x', FSTAT )

*  TCRPX7
         ELSE IF ( KEYWRD .EQ. 'TCRPX7' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Reference pixel y', FSTAT )

*  TC6_6
         ELSE IF ( KEYWRD .EQ. 'TC6_6' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Pixel transformation', FSTAT )

*  TC6_7
         ELSE IF ( KEYWRD .EQ. 'TC6_7' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Pixel transformation', FSTAT )

*  TC7_6
         ELSE IF ( KEYWRD .EQ. 'TC7_6' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Pixel transformation ', FSTAT )

*  TC7_7
         ELSE IF ( KEYWRD .EQ. 'TC7_7' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Pixel transformation', FSTAT )

*  TCDLT6
         ELSE IF ( KEYWRD .EQ. 'TCDLT6' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Co-ordinate increment', FSTAT )

*  TCDLT7
         ELSE IF ( KEYWRD .EQ. 'TCDLT7' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Co-ordinate increment', FSTAT )

*  TP66
         ELSE IF ( KEYWRD .EQ. 'TP66' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Transformation matrix', FSTAT )

*  TP67
         ELSE IF ( KEYWRD .EQ. 'TP67' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Transformation matrix', FSTAT )

*  TP76
         ELSE IF ( KEYWRD .EQ. 'TP76' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Transformation matrix', FSTAT )

*  TP77
         ELSE IF ( KEYWRD .EQ. 'TP77' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Transformation matrix', FSTAT )

*  TCRD6
         ELSE IF ( KEYWRD .EQ. 'TCRD6' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Random error in X', FSTAT )

*  TCRD7
         ELSE IF ( KEYWRD .EQ. 'TCRD7' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Random error in Y', FSTAT )

*  TCSY6
         ELSE IF ( KEYWRD .EQ. 'TCSY6' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Systematic error in X', FSTAT )

*  TCSY7
         ELSE IF ( KEYWRD .EQ. 'TCSY7' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Systematic error in Y', FSTAT )



*  The 2dF software seems to put in ASCII null characters perhaps due to
*  some C usage.  These must be replaced with a blank.  Find the
*  effective length up to a maximum that will fit into the header
*  allowing for the comment and standard header syntax for character
*  values.

*  PROGID
         ELSE IF ( KEYWRD .EQ. 'PROGID' ) THEN
            CALL DAT_GET0C( CLOC, CVALUE, STATUS )
            CALL CHR_TRCHR( NULL, ' ', CVALUE, STATUS )
            NC = CHR_LEN( CVALUE( :41 ) )
            CALL FTPKYS( FUNIT, KEYWRD, CVALUE( :NC ),
     :                   'Programme Identification', FSTAT )

*  MODE
         ELSE IF ( KEYWRD .EQ. 'MODE' ) THEN
            CALL DAT_GET0C( CLOC, CVALUE, STATUS )
            CALL CHR_TRCHR( NULL, ' ', CVALUE, STATUS )
            NC = CHR_LEN( CVALUE( :43 ) )
            CALL FTPKYS( FUNIT, KEYWRD, CVALUE( :NC ),
     :                   'Mode (SWITCH or NORMAL)', FSTAT )

*  LABEL
         ELSE IF ( KEYWRD .EQ. 'LABEL' ) THEN
            CALL DAT_GET0C( CLOC, CVALUE, STATUS )
            CALL CHR_TRCHR( NULL, ' ', CVALUE, STATUS )
            NC = CHR_LEN( CVALUE( :64 ) )
            CALL FTPKYS( FUNIT, KEYWRD, CVALUE( :NC ), ' ', FSTAT )

*  FILENAME
         ELSE IF ( KEYWRD .EQ. 'FILENAME' ) THEN
            CALL DAT_GET0C( CLOC, CVALUE, STATUS )
            CALL CHR_TRCHR( NULL, ' ', CVALUE, STATUS )
            NC = CHR_LEN( CVALUE( :68 ) )
            CALL FTPKYS( FUNIT, KEYWRD, CVALUE( :NC ), ' ', FSTAT )

* -------------------------------
* AAOMEGA-IFU STRING KEYWORDS
* -------------------------------

*  ACTUTC
         ELSE IF ( KEYWRD .EQ. 'ACTUTC' ) THEN
            CALL DAT_GET0C( CLOC, CVALUE, STATUS )
            CALL CHR_TRCHR( NULL, ' ', CVALUE, STATUS )
            NC = CHR_LEN( CVALUE( :64 ) )
            CALL FTPKYS( FUNIT, KEYWRD, CVALUE( :NC ),
     :                   'UT of tweak', FSTAT )

*  TOPEND
         ELSE IF ( KEYWRD .EQ. 'TOPEND' ) THEN
            CALL DAT_GET0C( CLOC, CVALUE, STATUS )
            CALL CHR_TRCHR( NULL, ' ', CVALUE, STATUS )
            NC = CHR_LEN( CVALUE( :64 ) )
            CALL FTPKYS( FUNIT, KEYWRD, CVALUE( :NC ),
     :                   'AAT Top-End name', FSTAT )

*  TCTYP6
         ELSE IF ( KEYWRD .EQ. 'TCTYP6' ) THEN
            CALL DAT_GET0C( CLOC, CVALUE, STATUS )
            CALL CHR_TRCHR( NULL, ' ', CVALUE, STATUS )
            NC = CHR_LEN( CVALUE( :64 ) )
            CALL FTPKYS( FUNIT, KEYWRD, CVALUE( :NC ),
     :                   'Unit Type', FSTAT )

*  TCTYP7
         ELSE IF ( KEYWRD .EQ. 'TCTYP7' ) THEN
            CALL DAT_GET0C( CLOC, CVALUE, STATUS )
            CALL CHR_TRCHR( NULL, ' ', CVALUE, STATUS )
            NC = CHR_LEN( CVALUE( :64 ) )
            CALL FTPKYS( FUNIT, KEYWRD, CVALUE( :NC ),
     :                   'Unit Type', FSTAT )

*  TCUNI6
         ELSE IF ( KEYWRD .EQ. 'TCUNI6' ) THEN
            CALL DAT_GET0C( CLOC, CVALUE, STATUS )
            CALL CHR_TRCHR( NULL, ' ', CVALUE, STATUS )
            NC = CHR_LEN( CVALUE( :64 ) )
            CALL FTPKYS( FUNIT, KEYWRD, CVALUE( :NC ), 'Unit', FSTAT )

*  TCUNI7
         ELSE IF ( KEYWRD .EQ. 'TCUNI7' ) THEN
            CALL DAT_GET0C( CLOC, CVALUE, STATUS )
            CALL CHR_TRCHR( NULL, ' ', CVALUE, STATUS )
            NC = CHR_LEN( CVALUE( :64 ) )
            CALL FTPKYS( FUNIT, KEYWRD, CVALUE( :NC ), 'Unit', FSTAT )

*  TWCS6
         ELSE IF ( KEYWRD .EQ. 'TWCS6' ) THEN
            CALL DAT_GET0C( CLOC, CVALUE, STATUS )
            CALL CHR_TRCHR( NULL, ' ', CVALUE, STATUS )
            NC = CHR_LEN( CVALUE( :64 ) )
            CALL FTPKYS( FUNIT, KEYWRD, CVALUE( :NC ),
     :                   'Co-ordinate name', FSTAT )


*  TWCS7
         ELSE IF ( KEYWRD .EQ. 'TWCS7' ) THEN
            CALL DAT_GET0C( CLOC, CVALUE, STATUS )
            CALL CHR_TRCHR( NULL, ' ', CVALUE, STATUS )
            NC = CHR_LEN( CVALUE( :64 ) )
            CALL FTPKYS( FUNIT, KEYWRD, CVALUE( :NC ),
     :                   'Co-ordinate name', FSTAT )

         END IF

*  Tidy the locator to the object.
         CALL DAT_ANNUL( CLOC, STATUS )

      END DO

*  Free the locator to the FIELD structure.
      CALL DAT_ANNUL( FLOC, STATUS )

*  Write components into table columns.
*  ====================================

*  At this point all the headers have been written.  Now it's time
*  to create the columns.  Deal with each column in turn.  If the
*  component is present, map it and transfer the data to the
*  binary-table column; if not, just write a column of null values.
      DO I = 1, MXCOMP

*  Search for the object.
         CALL DAT_THERE( OLOC, TTYPE( I ), THERE, STATUS )

*  Obtain a locator to the object and determine its type.
         IF ( THERE ) THEN
            CALL DAT_FIND( OLOC, TTYPE( I ), CLOC, STATUS )
            CALL DAT_TYPE( CLOC, CTYPE, STATUS )
         END IF

*  Process by data type.  Start with the character arrays.
*  -------------------------------------------------------
         IF ( TFORM( I ) .EQ. '80A'  .OR. TFORM( I ) .EQ. '16A' .OR.
     :        TFORM( I ) .EQ. '10A'  .OR. TFORM( I ) .EQ. '8A' .OR.
     :        TFORM( I ) .EQ. '1A' ) THEN

            IF ( THERE ) THEN

*  Obtain the length in characters of the component and map its values.
                CALL DAT_LEN( CLOC, STRLEN, STATUS )
                CALL DAT_MAPV( CLOC, CTYPE, 'READ', OPNTR, EL, STATUS )
*                CALL CON_TRCHA( NULL, ' ', EL, %VAL(CNF_PVAL( OPNTR )),
*     :                          STATUS, %VAL( STRLEN ) )

*  Transfer the values to the binary-table column.
                CALL FTPCLS( FUNIT, I, 1, 1, EL,
     :                       %VAL( CNF_PVAL( OPNTR ) ),
     :                       FSTAT, %VAL( CNF_CVAL( STRLEN ) ) )

*  Unmap the component.
                CALL DAT_UNMAP( CLOC, STATUS )

*  Binary tables use the ASCII NULL as the first character to
*  indicate a null character value.  Since this is really a fault in
*  the NDF, the user can suffer a little by calling the FITSIO routine
*  for every element, instead of getting workspace and filling it with
*  blank values.
             ELSE
                DO LEL = 1, EL
                   CALL FTPCLS( FUNIT, I, LEL, 1, 1, NULL, FSTAT )
                END DO
             END IF
             ROUTIN = 'FTPCLS'

*  Double-precision components
*  ---------------------------
         ELSE IF ( TFORM( I ) .EQ. '1D' ) THEN

            IF ( THERE ) THEN

*  Map the values of the component.
                CALL DAT_MAPV( CLOC, '_DOUBLE', 'READ', OPNTR, EL,
     :                         STATUS )

*  Transfer the values to the binary-table column.  Replace Starlink bad
*  values with NaNs.
                CALL FTPCND( FUNIT, I, 1, 1, EL,
     :                       %VAL( CNF_PVAL( OPNTR ) ),
     :                       VAL__BADD, FSTAT )

*  Unmap the component.
                CALL DAT_UNMAP( CLOC, STATUS )

*  Need to create a column of null values.  First fill a work array with
*  bad values.
             ELSE
                CALL PSX_CALLOC( EL, '_DOUBLE', OPNTR, STATUS )
                CALL CON_CONSD( VAL__BADD, EL,
     :                          %VAL( CNF_PVAL( OPNTR ) ), STATUS )

*  Transfer the bad values to the binary-table column.  Replace
*  Starlink bad values with NaNs.
                CALL FTPCND( FUNIT, I, 1, 1, EL,
     :                       %VAL( CNF_PVAL( OPNTR ) ),
     :                       VAL__BADD, FSTAT )

*  Release the workspace.
                CALL PSX_FREE( OPNTR, STATUS )
             END IF
             ROUTIN = 'FTPCLD'

*  Integer components
*  ------------------
         ELSE IF ( TFORM( I ) .EQ. '1J' ) THEN

            IF ( THERE ) THEN

*  Map the values of the component.
                CALL DAT_MAPV( CLOC, '_INTEGER', 'READ', OPNTR, EL,
     :                         STATUS )

*  Transfer the values to the binary-table column.
                CALL FTPCLJ( FUNIT, I, 1, 1, EL,
     :                       %VAL( CNF_PVAL( OPNTR ) ), FSTAT )

*  Unmap the component.
                CALL DAT_UNMAP( CLOC, STATUS )

*  Fill the array by calling the FITSIO routine for every element,
*  instead of getting workspace and filling it with a constant.
             ELSE
                DO LEL = 1, EL
                   CALL FTPCLJ( FUNIT, I, LEL, 1, 1, VAL__BADI, FSTAT )
                END DO
             END IF
             ROUTIN = 'FTPCLJ'

*  Word components
*  ---------------
         ELSE IF ( TFORM( I ) .EQ. '1I' ) THEN

            IF ( THERE ) THEN

*  Map the values of the component.
                CALL DAT_MAPV( CLOC, '_WORD', 'READ', OPNTR, EL,
     :                         STATUS )

*  Transfer the values to the binary-table column.
                CALL FTPCLI( FUNIT, I, 1, 1, EL,
     :                       %VAL( CNF_PVAL( OPNTR ) ), FSTAT )

*  Unmap the component.
                CALL DAT_UNMAP( CLOC, STATUS )

*  Fill the array by calling the FITSIO routine for every element,
*  instead of getting workspace and filling it with a constant.
             ELSE
                DO LEL = 1, EL
                   CALL FTPCLI( FUNIT, I, LEL, 1, 1, VAL__BADW, FSTAT )
                END DO
             END IF
             ROUTIN = 'FTPCLI'

         END IF

*  Tidy the locator to the component.
         IF ( THERE ) CALL DAT_ANNUL( CLOC, STATUS )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
         IF ( FSTAT .GT. FITSOK ) THEN
            NC = CHR_LEN( TTYPE( I ) )
            BUFFER = 'Error copying vector component '/
     :               /TTYPE( I )( :NC )//' to the binary table.'
            CALL COF_FIOER( FSTAT, 'COF_2DFEX_ERR4', ROUTIN, BUFFER,
     :                      STATUS )
            GOTO 999
         END IF
      END DO

*  Tidy the locator to the OBJECT structure.
      CALL DAT_ANNUL( OLOC, STATUS )

  999 CONTINUE

      END
