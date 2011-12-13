      SUBROUTINE COF_2DFIM( FUNIT, FILE, NDF, PROFIT, LOGHDR, FDL,
     :                      FMTCNV, STATUS )
*+
*  Name:
*     COF_2DFIM

*  Purpose:
*     Imports a 2dF FITS file into an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_2DFIM( FUNIT, FILE, NDF, PROFIT, LOGHDR, FDL, FMTCNV,
*                     STATUS )

*  Description:
*     This converts a 2dF FITS file created by NDF2FITS (COF_2DFEX in
*     particular) into an NDF.  It creates the 2dF FIBRES extension and
*     its constituent structures, and NDF_CLASS extension.  In addition
*     the variance, axes, and HISTORY records are converted.  It can
*     also cope with non-standard extensions using the same rules as
*     COF_F2NDF.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        Logical-unit number of the FITS file.
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the FITS file or device being converted.  This is
*        only used for error messages.
*     NDF = INTEGER (Given)
*        The NDF identifier of the output NDF.
*     PROFIT = LOGICAL (Given)
*        If .TRUE., the FITS headers are written to the NDF's FITS
*        extension.  It should be set to .TRUE. in normal circumstances.
*     LOGHDR = LOGICAL (Given)
*        If .TRUE., a record of the FITS headers is written to a log
*        file given by descriptor FDL.  If .FALSE., no log is made and
*        argument FDL is ignored.
*     FDL = INTEGER (Given)
*        The file descriptor for the log file.  This is ignored when
*        LOGHDR is .FALSE..
*     FMTCNV = LOGICAL (Given)
*        This specifies whether or not format conversion will occur.
*        The conversion applies the values of the FITS `keywords'
*        BSCALE and BZERO  to the FITS data to generate the "true" data
*        values.  Keywords is in quotes because the scale and offset
*        are actually in columns of the binary table called BSCALE and
*        BZERO.
*
*        If FMTCNV=.FALSE., the HDS type of the data array in the NDF
*        will be the equivalent of the FITS data format (e.g.
*        BITPIX = 16 creates a _WORD array).  If FMTCNV=.TRUE., the
*        data array in the NDF will be converted from the FITS data
*        type on tape to _REAL or _DOUBLE in the NDF.  The selection
*        of the floating-point type is equivalent to the data types
*        of the BSCALE and BZERO values in the header.
*
*        FMTCNV = .FALSE. is recommended, as there should be no
*        conversion to do.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     Details of the conversion are:
*        -  The primary data array becomes the NDF's data array.  Any
*        NaN values present become bad values in the NDF.
*        -  The keywords CRVALn, CDELTn, CRPIXn, CTYPEn, CUNITn are
*        used to create the NDF axis structure.
*        -  The OBJECT, LABEL, BUNITS keywords define the NDF's title,
*        label, and units components respectively, if they are defined.
*        -  HISTORY cards in a special format created by NDF2FITS are
*        converted back into NDF HISTORY records.
*        -  The NDF variance is derived from the data array of an
*        IMAGE extension (usually the first), if present, provided the
*        IMAGE extension headers have an HDUCLAS2 keyword whose value
*        is either 'VARIANCE' or 'ERROR'.
*        -  The NDF_CLASS extension within the NDF is filled using the
*        a FITS binary-table extension whose EXTNAME keyword's value is
*        NDF_CLASS.  Note: no error is reported if this extension does
*        not exist within the FITS file.
*        -  The FIBRES extension is created using another binary table
*        whose EXTNAME keyword's value is FIBRES.  The OBJECT
*        substructure's component names, data types, and values are
*        taken from the binary-table columns themselves, and the
*        components of the FIELD substructure are extracted from
*        recognised keywords in the binary-table's header.  Note:
*        no error is reported if this extension does not exist within
*        the FITS file.
*        -  Other IMAGE and BINTABLE extensions are propagated to the
*        NDF extension.  It uses the extension name and type found in
*        the EXTNAME and EXTTYPE keywords, or names it FITS_EXT_n for
*        the nth FITS extension.
*        -  A FITS airlock in the NDF may be written (PROFIT=.TRUE.)
*        to store the primary data unit's headers.  The airlock
*        will not contain any NDF-style HISTORY headers.

*  References:
*     Bailey, J.A. 1996, 1997 2dF Software Report 14, version 0.3, 0.5.
*
*     [optional_subroutine_items]...

*  Prior Requirements:
*     -  The FITS file should already have been opened by FITSIO, and
*     is in the primary HDU.  The NDF must also exist.
*     -  The HISTORY propagation assumes that the FITS HISTORY cards
*     created by routine COF_WHISR have not been tampered.

*  Copyright:
*     Copyright (C) 1997-2000, 2004 Central Laboratory of the Research
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
*     DSB: David S. Berry (STARLINK)
*     AJC: Alan J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MNB: Mike N Birchall (AAO)
*     {enter_new_authors_here}

*  History:
*     1997 February 28 (MJC):
*        Original version.
*     1997 November 10 (MJC):
*        Allow the variance array to be absent.
*     1997 November 12 (MJC):
*        Large rewrite to cope with non-standard and missing extensions.
*     1997 November 16 (MJC):
*        Filter out NDF-style HISTORY.
*     1998 April 30 (MJC):
*        Increased the maximum CLASS_NAME from 8 to 10 to allow for
*        MFSOBJECT.
*     8-JAN-1999 (DSB):
*        Added FMTCNV to argument list for COF_STYPE call.
*     5-APR-2000 (AJC):
*        Add CHECK argument to COF_SBND.
*     30-AUG-2000 (AJC):
*        Correct description FITS_EXT_n not NDF_EXT_n.  Add separate
*        FITS unit numbers in call to COF_WRTAB.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2006 April 13 (MJC):
*        Remove unused variables.
*     2006 June 15 (MNB):
*        Added that the fibres table conversion be applied to ndf extensions
*        with name "FIBRES_IFU" as well as "FIBRES".
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER FUNIT
      CHARACTER * ( * ) FILE
      INTEGER NDF
      LOGICAL PROFIT
      LOGICAL LOGHDR
      INTEGER FDL
      LOGICAL FMTCNV

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Effective string length
      CHARACTER * ( 2 ) CHR_NTH  ! Ordinal abbreviation
      EXTERNAL CHR_NTH

*  Local Constants:
      INTEGER  FITSOK             ! Good status for FITSIO library
      PARAMETER ( FITSOK = 0 )
      INTEGER  CLASS_LEN          ! Maximum length of observation-class
      PARAMETER ( CLASS_LEN = 10 ) ! string

*  Local Variables:
      LOGICAL BAD                ! Bad values may be present in array?
      INTEGER BITPIX             ! FITS file's BITPIX
      CHARACTER * ( 256 ) BUFFER ! BUFFER for writing error messages
      CHARACTER * ( CLASS_LEN ) CLASS   ! Observation class
      CHARACTER * ( DAT__SZLOC ) CLOC ! Locator to CLASS structure
      CHARACTER * ( DAT__SZLOC ) CLLOC ! Locator to CLASS component
      INTEGER COLEXT             ! Character where extension name begins
      CHARACTER * ( 48 ) COMENT  ! Keyword comment
      CHARACTER * ( 8 ) COMP     ! NDF array component name
      LOGICAL DARRAY             ! Current HDU contains a data array?
      INTEGER DIMS( NDF__MXDIM ) ! NDF dimensions
      INTEGER EL                 ! Number of elements in array
      CHARACTER * ( DAT__SZLOC ) ELOC ! Locator to NDF extension (MORE)
                                 ! structure
      CHARACTER * ( 68 ) EXTNAM  ! Name of the extension/path
      LOGICAL EXNDF              ! FITS file originated from an NDF?
      LOGICAL EXTEND             ! Value of FITS EXTEND keyword
      CHARACTER * ( DAT__SZLOC ) FILOC ! Locator to FIELD structure
      LOGICAL FIRST              ! Processing the first HDU
      CHARACTER * ( DAT__SZLOC ) FLOC ! Locator to FIBRES structure
      INTEGER FSTAT              ! FITSIO error status
      INTEGER GCOUNT             ! Value of FITS GCOUNT keyword
      CHARACTER * ( NDF__SZFTP ) HDUCLA ! Classification of HDU
      LOGICAL HDUPRE             ! HDUCLASn keyword is present?
      INTEGER HDUTYP             ! HDU type (primary, IMAGE, ASCII or
                                 ! binary table)
      CHARACTER * ( NDF__SZTYP ) ITYPE ! NDF implementation data type
      LOGICAL LOOP               ! Loop for another FITS extension?
      INTEGER NC                 ! Number of characters in component
      INTEGER NCF                ! Number of characters in filename
      INTEGER NDFE               ! Identifier of effective NDF
      INTEGER NDIM               ! Number of dimensions
      INTEGER NHDU               ! Count of header and data unit
      INTEGER NHEAD              ! Number of FITS header cards
      CHARACTER * ( DAT__SZLOC ) NLOC ! Locator to NUM_FIBRES component
      INTEGER NOBS               ! Number of rows in table
      LOGICAL NONSDA             ! Current HDU contains a
                                 ! non-standard data array?
      INTEGER NPOS               ! Character position in extension name
      CHARACTER * ( DAT__SZLOC ) OLOC ! Locator to OBJECT structure
      INTEGER PCOUNT             ! Value of FITS PCOUNT keyword
      INTEGER PLACE              ! NDF placeholder for <NDF> extension
      INTEGER PNTR( 1 )          ! Pointer to NDF array
      INTEGER REPNTR             ! Pointer to header-propagation flags
      LOGICAL SIMPLE             ! FITS file is simple?
      LOGICAL THERE              ! Keyword is present?
      CHARACTER * ( DAT__SZNAM ) TYPE ! Data type
      LOGICAL WRTEXT             ! Write NDF FITS extension?
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator to an NDF extension
      CHARACTER * ( 20 ) XTENS   ! Name of FITS extension

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Obtain the length of the filename.
      NCF = CHR_LEN( FILE )

*  Initialise the root name of NDF extensions for FITS extensions.  The
*  number of the FITS extension is appended to create the NDF-extension
*  name.
      EXTNAM = 'FITS_EXT_'

*  Loop for each FITS extension.
*  =============================
      LOOP = .TRUE.
      EXNDF = .FALSE.
      NHDU = 0
      XTENS = ' '

*  Continue looping when there are more extensions that are requested to
*  be converted into the NDF, and nothing has gone wrong thus far.
      DO WHILE ( LOOP .AND. FSTAT .EQ. FITSOK )

*  Increment the count of the header-and-data units.
         NHDU = NHDU + 1
         FIRST = NHDU .EQ. 1

         IF ( .NOT. FIRST ) THEN

*  Skip to the next HDU.
            CALL FTMRHD( FUNIT, 1, HDUTYP, FSTAT )
            IF ( FSTAT .NE. FITSOK ) THEN

*  Report the error, if it is not the expected end of file (error 107)
*  or has some garbage at the end (unrecognisable FITS record, error
*  252).  In the latter case just annul the error and remove the FITSIO
*  error messages from the stack.
               IF ( FSTAT .EQ. 107 .OR. FSTAT .EQ. 252 ) THEN
                  CALL FTCMSG
                  FSTAT = FITSOK
               ELSE
                  BUFFER = 'of the FITS file '//FILE( :NCF )//'.'
                  CALL MSG_SETI( 'N', NHDU - 1 )
                  CALL MSG_SETC( 'NTH', CHR_NTH( NHDU - 1 ) )
                  CALL MSG_SETC( 'BUF', BUFFER )

                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'COF_2DFIM_WREXT',
     :              'Error skipping to the ^N^NTH extension ^BUF',
     :              STATUS )
                  CALL COF_FIOER( FSTAT, 'COF_2DFIM_WREXT', 'FTMRHD',
     :              ' ', STATUS )
               END IF
               GOTO 999
            END IF

*  Obtain the name of the current extension.
            CALL FTGKYS( FUNIT, 'XTENSION', XTENS, COMENT, FSTAT )
            IF ( FSTAT .NE. FITSOK ) THEN
               BUFFER = 'Error obtaining the extension name from '/
     :                  /'FITS file '//FILE( :NCF )//'.'
               CALL COF_FIOER( FSTAT, 'COF_2DFIM_MANDH', 'FTGKYS',
     :                         BUFFER, STATUS )
               GOTO 999
            END IF
         END IF

*  Data scaling.
*  =============

*  Check that if the current HDU is the primary, or that it is an
*  IMAGE, and thus can be processed by the following routine.
         IF ( FIRST. OR. XTENS .EQ. 'IMAGE' ) THEN

*  The FMTCNV flag decides whether or not the data scaling is required.
*  The FITSIO routines that obtain the data array(s) will apply the
*  block floating-point scaling as prescribed by the BSCALE and BZERO
*  keywords.
            IF ( FMTCNV .AND. ( FIRST .OR. XTENS .EQ. 'IMAGE' ) ) THEN

*  Scaling is to be applied.  Find the data type required for the
*  output array based upon the number of significant digits in the
*  BSCALE and BZERO keywords.  If these have values of 1.0D0 and 0.0D0
*  respectively either explicitly, or because one or both are absent,
*  then the data type can be set to the null string.  This instructs
*  later routines like COF_STYPC to use the data type specified by the
*  FITSIO data-type code (based on BITPIX).
               CALL COF_DSTYP( FUNIT, 'BSCALE', 'BZERO', TYPE, STATUS )

*  To prevent scaling, the scale and offset must be set to
*  one and zero respectively.  Note that this does not affect the
*  keywords in the header of the input FITS file.  Note that the values
*  are double precision.
            ELSE
               CALL FTPSCL( FUNIT, 1.0D0, 0.0D0, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
               IF ( FSTAT .GT. FITSOK ) THEN
                  BUFFER = 'Error defaulting the scale and offset for '/
     :                     /'FITS file '//FILE( :NCF )//'.'
                  CALL COF_FIOER( FSTAT, 'COF_2DFIM_SCOF', 'FTPSCL',
     :                            BUFFER, STATUS )
                  GOTO 999
               END IF

*  Set the recommended data type to a null string.  This instructs later
*  routines like COF_STYPC to use the data type specified by the FITSIO
*  data-type code (based on BITPIX).
               TYPE = ' '
            END IF

*  Determine the main properties of the FITS object.
*  =================================================

*  Get the mandatory headers of the primary HDU or an IMAGE extension.
            CALL COF_MANDH( FUNIT, FIRST, NDF__MXDIM, SIMPLE, BITPIX,
     :                      NDIM, DIMS, PCOUNT, GCOUNT, EXTEND,
     :                      DARRAY, NONSDA, EL, STATUS )

*  Report the error context.
            IF ( STATUS .NE. SAI__OK ) THEN
               BUFFER = 'FITS file '//FILE( :NCF )//'.'
               CALL MSG_SETI( 'N', NHDU )
               CALL MSG_SETC( 'NTH', CHR_NTH( NHDU ) )
               CALL MSG_SETC( 'BUF', BUFFER )
               CALL ERR_REP( 'COF_2DFIM_MANDH',
     :           'Error occurred during accessing headers in the '/
     :           /'^N^NTH header and data unit of ^BUF', STATUS )
               GOTO 999
            END IF

*  Cannot processed non-standard files.
            IF ( FIRST .AND. .NOT. SIMPLE ) THEN
               STATUS = SAI__ERROR
               BUFFER = 'The FITS file '//FILE( :NCF )//' is not'
               CALL MSG_SETC( 'BUF', BUFFER )
               CALL ERR_REP( 'COF_2DFIM_NOTSIM',
     :           '^BUF simple and therefore cannot be processed.',
     :           STATUS )
               GOTO 999
            END IF

*  Former NDF?
*  ===========
*
*  First see if the primary data or IMAGE extension data originated in
*  an NDF array component.  If it is, we want to replace it rather than
*  create an extension.  A bad status can be ignored, as the presence
*  flag will be false.
            CALL ERR_MARK
            CALL COF_GKEYC( FUNIT, 'HDUCLAS1', HDUPRE, HDUCLA,
     :                      COMENT, STATUS )
            IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
            CALL ERR_RLSE

*  Define whether or not the extension came from an NDF.
            EXNDF = HDUPRE .AND. HDUCLA .EQ. 'NDF'

         END IF

*  Select type of extension.
*  =========================
*
*  There is support for basic FITS, which creates a typical NDF; the
*  IMAGE extension, which creates an NDF within an extension of the
*  original NDF; 2dF standard BINTABLEs (NDF_CLASS and FIBRES) generate
*  standard 2dF extensions of the same name within the NDF; arbitrary
*  BINTABLE and TABLE create <TABLE> type extension within the NDF,
*  unless they were produced by NDF2FITS, whereupon they recreate the
*  original NDF structures.

*  Test for a primary HDU or an IMAGE extension.  Note that this
*  includes random groups as these must be defined in the primary HDU.
         IF ( FIRST. OR. XTENS .EQ. 'IMAGE' ) THEN

*  Copy the NDF identifier to the temporary clone.
            IF ( FIRST ) THEN
               NDFE = NDF

*  By definition the NDF must have a Data component.
               COMP = 'Data'

*  Do we need to create an extension which is an NDF?
*  ==================================================
            ELSE IF ( XTENS .EQ. 'IMAGE' ) THEN

*  First see if the IMAGE extension data originated in an NDF array
*  component.  If it is, we want to replace it rather than create an
*  extension.
               IF ( EXNDF ) THEN

*  Obtain the component.
                  CALL ERR_MARK
                  CALL COF_GKEYC( FUNIT, 'HDUCLAS2', HDUPRE,
     :                            COMP, COMENT, STATUS )
                  IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
                  CALL ERR_RLSE

*  Provided HDUCLAS2 keyword is present and has one of the
*  array-component names, make assignments to create the component
*  within the current NDF rather than as the data array in an NDF
*  extension.
                  IF ( HDUPRE .AND.
     :                 ( COMP .EQ. 'QUALITY' .OR.
     :                   COMP .EQ. 'VARIANCE' .OR.
     :                   COMP .EQ. 'ERROR' ) ) THEN
                     NDFE = NDF
                  ELSE

*  By definition the extension NDF must have a Data component.
                     COMP ='Data'
                  END IF
               ELSE
                  COMP ='Data'
               END IF

               IF ( COMP .EQ. 'Data' ) THEN

*  Generate the name of the extension.  NPOS is updated so cannot be
*  defined outside the FITS_extension loop.
                  NPOS = 9
                  CALL CHR_PUTI( NHDU - 1, EXTNAM, NPOS )

*  Create an extension of type NDF.
                  CALL NDF_XNEW( NDF, EXTNAM, 'NDF', 0, 0, XLOC,
     :                           STATUS )

*  Find the parent structure (i.e. .MORE).
                  CALL DAT_PAREN( XLOC, ELOC, STATUS )

*  Create a new NDF in the extension via an NDF placeholder.  The data
*  type and bounds will be changed below once they are known.
                  CALL NDF_PLACE( ELOC, EXTNAM, PLACE, STATUS )
                  CALL NDF_NEW( '_UBYTE', 1, 1, 1, PLACE, NDFE,
     :                          STATUS )
               END IF
            END IF

*  Report the full set of headers and/or write to NDF's FITS extension.
*  ====================================================================
*
*  This is slightly less efficient than combining the two operations,
*  but re-using subroutines does make the code easier to follow.  There
*  is another factor.  This stage incorporates the recreation of NDF
*  HISTORY records, which should not be propagated to the FITS airlock
*  too.  This intertwined steps requires a set of flags, but the logging
*  does not.

*  Decide whether or not to save the headers in an extension.
            WRTEXT = ( ( XTENS .EQ. 'IMAGE' .AND. .NOT. EXNDF ) .OR.
     :                 FIRST ) .AND. PROFIT

*  Decide when to access the headers.
            IF ( WRTEXT .OR. ( EXNDF .AND. COMP .EQ. 'Data' ) ) THEN

*  Instruct that the FITS airlock is not to use all the headers
*  regardless whether or not they are NDF-style HISTORY records.  This
*  uses a logical work array to flag whether or not to propagate the
*  header to the airlock.

*  Find the number of FITS headers.  Check that nothing has gone wrong
*  before using the the number to create workspace.
               CALL COF_NHEAD( FUNIT, FILE, NHEAD, STATUS )
               IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Create the mask and assign .TRUE. to all of its elements.
               CALL PSX_CALLOC( NHEAD, '_LOGICAL', REPNTR, STATUS )
               CALL CON_CONSL( .TRUE., NHEAD,
     :                         %VAL( CNF_PVAL( REPNTR ) ), STATUS )
            END IF

*  Deal with the NDF-style history records in the headers, assuming the
*  history records have not been tampered.  Search for such records,
*  transfer their information back into NDF HISTORY records, and flag
*  that these headers should not to be propagated to the FITS airlock.
*  This is to avoid growing duplication of potentially bulky text, if
*  using FITS files with Starlink tasks.
            IF ( EXNDF .AND. COMP .EQ. 'Data' ) THEN
               CALL COF_CHISR( FUNIT, NDFE, NHEAD,
     :                         %VAL( CNF_PVAL( REPNTR ) ),
     :                         STATUS )
            END IF

*  Read the main header into the FITS extension of the NDF.  The FITS
*  headers for the random groups will appear in each group NDF.
            IF ( WRTEXT ) THEN
               CALL COF_WFEXF( FUNIT, NDFE, 0, 0, FILE, NHEAD,
     :                         %VAL( CNF_PVAL( REPNTR ) ), STATUS )
            END IF

*  Free the work space.
            IF ( WRTEXT .OR. ( EXNDF .AND. COMP .EQ. 'Data' ) )
     :        CALL PSX_FREE( REPNTR, STATUS )

*  Write out the headers to a logfile, if desired.
            IF ( LOGHDR )
     :        CALL COF_HDLOG( FUNIT, FDL, FILE, NHDU, STATUS )

*  Modify the shape and type of the NDF, now that it is known.
*  ===========================================================

*  Test whether or not there is a data array present.  The zero'th group
*  is a simple array and is merely filled with dummy data, so test for
*  this.
            IF ( DARRAY .AND. .NOT. NONSDA ) THEN
               CALL COF_STYPE( NDFE, COMP, TYPE, BITPIX, FMTCNV, ITYPE,
     :                         STATUS )

*  Specify the bounds of the NDF array component.
               CALL COF_SBND( FUNIT, NDFE, 'LBOUND', .FALSE., STATUS )

*  Copy the data values into the array component.
*  ==============================================

*  First map the input array component with the desired data type.
*  Any type conversion will be performed by the FITSIO array-reading
*  routine.
               CALL NDF_MAP( NDFE, COMP, ITYPE, 'WRITE', PNTR, EL,
     :                       STATUS )
               IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Call the appropriate routine for the data type of the created array.
*  The group is 0 for simple FITS or the group identification
*  otherwise.  We always start at the first element.  The arrays may
*  have bad pixels.
               IF ( ITYPE .EQ. '_UBYTE' ) THEN
                  CALL FTGPVB( FUNIT, 0, 1, EL, VAL__BADUB,
     :                         %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                         BAD, FSTAT )

               ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                  CALL FTGPVI( FUNIT, 0, 1, EL, VAL__BADW,
     :                         %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                         BAD, FSTAT )

               ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                  CALL FTGPVJ( FUNIT, 0, 1, EL, VAL__BADI,
     :                         %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                         BAD, FSTAT )

               ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
                  CALL FTGPVE( FUNIT, 0, 1, EL, VAL__BADR,
     :                         %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                         BAD, FSTAT )

               ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                  CALL FTGPVD( FUNIT, 0, 1, EL, VAL__BADD,
     :                         %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                         BAD, FSTAT )

               END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
               IF ( FSTAT .GT. FITSOK ) THEN
                  NC = CHR_LEN( COMP )
                  BUFFER = 'Error reading the '//COMP( :NC )/
     :                     /' array in FITS file '//FILE( :NCF )//'.'
                  CALL COF_FIOER( FSTAT, 'COF_2DFIM_READ',
     :                            'FTGPVx', BUFFER, STATUS )
                  CALL NDF_UNMAP( NDFE, COMP, STATUS )
                  GOTO 999
               END IF

*  Set the bad-pixel flag.
               IF ( COMP .NE. 'QUALITY' )
     :           CALL NDF_SBAD( BAD, NDFE, COMP, STATUS )

*  The header is only a dummy, so fill the array with bad values by
*  mapping with the appropriate initialisation.  The values will be
*  returned to the output NDF when the array component is unmapped.
            ELSE
               CALL NDF_MAP( NDFE, COMP, '_REAL', 'WRITE/BAD',
     :                       PNTR, EL, STATUS )

            END IF

*  Tidy the array.
*  ===============
*  Unmap the array.
            CALL NDF_UNMAP( NDFE, COMP, STATUS )

*  Other components.
*  =================

*  Only need to define the other components once in an NDF.
            IF ( COMP .EQ. 'Data' ) THEN

*  Create the NDF character components from the FITS headers.
               CALL COF_NDFCC( FUNIT, NDFE, STATUS )

*  Create the NDF AXIS structure from the FITS headers.
               CALL COF_NDFAX( FUNIT, NDFE, STATUS )

*  Tidy the extension NDF and the locators used to create it.
               IF ( XTENS .EQ. 'IMAGE' ) THEN
                  CALL NDF_ANNUL( NDFE, STATUS )
                  CALL DAT_ANNUL( XLOC, STATUS )
                  CALL DAT_ANNUL( ELOC, STATUS )
               END IF
            END IF

*  Binary or ASCII Table.
*  ======================
         ELSE IF ( XTENS .EQ. 'BINTABLE' .OR.
     :             XTENS .EQ. 'TABLE' ) THEN

*  Obtain and validate the extension's name.
*  =========================================

*  Obtain the full component name.
            CALL COF_GKEYC( FUNIT, 'EXTNAME', THERE, EXTNAM, COMENT,
     :                      STATUS )

*  Extract the extension name.  This assumes that it was originally an
*  NDF extension.
            COLEXT = INDEX( EXTNAM, 'MORE.' ) + 5
            EXTNAM = EXTNAM( COLEXT: )

*  Create the FIBRES extension in the NDF.
*  =======================================
            IF ( EXTNAM .EQ. 'FIBRES' .OR.
     :           EXTNAM .EQ. 'FIBRES_IFU' ) THEN

*  Create the structure to hold the BINTABLE data.
               CALL NDF_XNEW( NDF, EXTNAM, 'FIBRE_EXT', 0, 0, FLOC,
     :                        STATUS )

*  Obtain the number of elements.
               CALL COF_GKEYI( FUNIT, 'NAXIS2', THERE, NOBS, COMENT,
     :                         STATUS )

*  Create a NUM_FIBRES component, and assign the number of fibres to it
*  using a locator.
               CALL DAT_NEW0I( FLOC, 'NUM_FIBRES', STATUS )
               CALL DAT_FIND( FLOC, 'NUM_FIBRES', NLOC, STATUS )
               CALL DAT_PUT0I( NLOC, NOBS, STATUS )
               CALL DAT_ANNUL( NLOC, STATUS )

*  Create the FIELD sub-structure, and obtain a locator to it.
               CALL DAT_NEW( FLOC, 'FIELD', 'FIB_FIELD', 0, 0, STATUS )
               CALL DAT_FIND( FLOC, 'FIELD', FILOC, STATUS )

*  Transfer any FIELD components from the FITS headers of the FIBRES
*  BINTABLE to the FIELD structure.
               CALL COF_2DFIF( FUNIT, FILOC, STATUS )
               CALL DAT_ANNUL( FILOC, STATUS )

*  Create the OBJECT sub-structure.
               CALL DAT_NEW( FLOC, 'OBJECT', 'FIB_OBJECT', 0, 0,
     :                       STATUS )
               CALL DAT_FIND( FLOC, 'OBJECT', OLOC, STATUS )

*  Fill the OBJECT structure using the BINTABLE columns.
               CALL COF_T2HDS( FUNIT, OLOC, STATUS )
               CALL DAT_ANNUL( OLOC, STATUS )

*  Free the locator to the FIBRES extension.
               CALL DAT_ANNUL( FLOC, STATUS )

*  Create the NDF_CLASS extension in the NDF.
*  ==========================================
            ELSE IF ( EXTNAM .EQ. 'NDF_CLASS' ) THEN

*  Create the structure to hold the BINTABLE data.
               CALL NDF_XNEW( NDF, 'NDF_CLASS', 'CLASS', 0, 0, CLOC,
     :                        STATUS )
*  Obtain the class.
               CALL FTGCVS( FUNIT, 1, 1, 1, 1, ' ', CLASS, BAD, FSTAT )

               IF ( FSTAT .NE. FITSOK ) THEN
                  BUFFER = 'Error obtain the class of the observation '/
     :                     /'in file '//FILE( :NCF )//'.'
                  CALL COF_FIOER( FSTAT, 'COF_2DFIM_GTCLASS', 'FTGCVS',
     :                            BUFFER, STATUS )
                  GOTO 999
               END IF

*  Create a NAME component, and assign the class to it via a locator.
               CALL DAT_NEWC( CLOC, 'NAME', CLASS_LEN, 0, 0, STATUS )
               CALL DAT_FIND( CLOC, 'NAME', CLLOC, STATUS )
               CALL DAT_PUT0C( CLLOC, CLASS, STATUS )
               CALL DAT_ANNUL( CLLOC, STATUS )

*  Free the locator to the NDF_CLASS extension.
               CALL DAT_ANNUL( CLOC, STATUS )

*  Some non-standard table extension.
*  ==================================
            ELSE

*  Test whether the FITS file came from an NDF.  If it does, propagate
*  the FITS table to the NDF extension.
               IF ( EXNDF ) THEN
                  CALL COF_FT2NE( FUNIT, NDF, STATUS )

*  Generate the name of the extension.  NPOS is updated so cannot be
*  defined outside the FITS_extension loop.
               ELSE
                  NPOS = 9
                  CALL CHR_PUTI( NHDU - 1, EXTNAM, NPOS )

*  Create a table extension.
                  CALL NDF_XNEW( NDF, EXTNAM, 'TABLE', 0, 0, XLOC,
     :                           STATUS )

*  Call routine to create the <TABLE> structure from the FITS binary
*  or ASCII table.  Header and Data FUNITs are the same (no
*  inheritance).
                  CALL COF_WRTAB( FUNIT, FUNIT, XLOC, STATUS )

*  Tidy the locator to the extension.
                  CALL DAT_ANNUL( XLOC, STATUS )
               END IF

            END IF

         END IF

      END DO

  999 CONTINUE

      END
