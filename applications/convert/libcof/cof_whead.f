      SUBROUTINE COF_WHEAD( NDF, COMP, FUNIT, BITPIX, PROPEX, STATUS )
*+
*  Name:
*     COF_WHEAD

*  Purpose:
*     Writes the FITS header derived from an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_WHEAD( NDF, COMP, FUNIT, BITPIX, PROPEX, STATUS )

*  Description:
*     This routine creates the header section of the primary array or
*     IMAGE extension of an output FITS file based upon information in
*     an NDF.  The IMAGE extension is written when the current header
*     and data unit (CHDU) is not the first.

*     There are two stages:
*     a) Inquire of the NDF its shape, type, character components,
*     and axis components; and write these to the header.
*     b) Look for a FITS extension if requested to do so.  If one is
*     present append the headers contained therein to the FITS header
*     section, but not replacing any of the headers created in stage
*     a).

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF.
*     COMP = CHARACTER * ( * ) (Given)
*        The array component to write to the HDU.
*     FUNIT = INTEGER (Given)
*        The logical unit number of the output FITS file.
*     BITPIX = INTEGER (Given)
*        The BITPIX value for the output FITS file.
*     PROPEX = LOGICAL (Given)
*        The NDF FITS extension, if present, is folded into the output
*        FITS header when PROPEX is .TRUE..  When PROPEX is .FALSE.,
*        the FITS extension is ignored.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     Here are details of the processing of standard items from the
*     NDF into the FITS header:
*        SIMPLE, EXTEND, PCOUNT, GCOUNT --- all take their default
*          values.
*        BITPIX, NAXIS, NAXISn --- are derived directly from the NDF
*          data array;
*        CRVALn, CDELTn, CRPIXn, CTYPEn, CUNITn --- are derived from
*          the NDF axis structures if possible.  If no linear NDF axis
*          structures are present, the values in the NDF FITS extension
*          are copied.  If any are non-linear, all FITS axis
*          information is lost.  When any non-zero CROTAn card is
*          present in the FITS extension, the extension axis
*          information is propagated, and not that of the NDF axis
*          structure.  [This rule is to enable rotated axis (not
*          supported in the NDF) to be retained in the cycle from FITS
*          to NDF and back to FITS.]
*        OBJECT, LABEL, BUNITS --- the values held in NDF TITLE, LABEL,
*          and UNITS respectively are used if present, otherwise any
*          s found in the FITS extension are used.
*        ORIGIN and DATE --- are created automatically.  However the
*          former may be overriden by an ORIGIN card in the NDF
*          extension.
*        EXTNAME --- is the component name of the object from the COMP
*          argument.
*        HDUCLAS1, HDUCLASn --- "NDF" and the value of COMP
*          respectively.
*        XTENSION, BSCALE, BZERO, BLANK and END --- are not propagated
*          from the extension.  The first will be set for any
*          extension.  BSCALE and BZERO will be defined based on the
*          chosen output data type in comparison with the NDF array's
*          type, but cards with values 1.0 and 0.0 respectively are
*          written to reserve places in the header section.  Likewise a
*          dummy BLANK card is written for the integer types.  The
*          BLANK value may be written as required, dependent on the
*          data type and any scaling.  These `reservation' cards are
*          for efficiency and they can always be deleted later.  The
*          END card is written by FITSIO automatically once the header
*          is closed.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1994 May 31 (MJC):
*        Original version.
*     1995 November 16 (MJC):
*        Fixed a bug that could occur in some circumstances when the
*        a rare CROTAn card is present.
*     1996 February 8 (MJC):
*        Added checks not to propagate the FITSIO banner and Starlink
*        ORIGIN card in the NDF's FITS extension to the output FITS
*        file.
*     1996 September 16 (MJC):
*        Corrected usage of CTYPEn (was CRTYPEn) and introduced CUNITn
*        for axis units.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data system constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants

*  Arguments Given:
      INTEGER   NDF              ! NDF identifier
      CHARACTER * ( * ) COMP     ! The array component
      INTEGER   FUNIT            ! Logical-unit number of FITS file
      INTEGER   BITPIX           ! Bits per pixel
      LOGICAL   PROPEX           ! Propagate FITS extension, when true

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER   FITSOK           ! Good status for FITSIO library
      PARAMETER( FITSOK = 0 )

      INTEGER   NFLAGS           ! Number of flags to indicate
                                 ! presence special NDF components
      PARAMETER( NFLAGS = 6 )

      INTEGER   SZKEY            ! Length of keyword names
      PARAMETER( SZKEY = 8 )     ! Columns 1 to 8

      INTEGER   SZFITS           ! Length of FITS string
      PARAMETER( SZFITS = 80 )

      INTEGER   SZNVAL           ! Character length of numeric values
      PARAMETER( SZNVAL = 20 )   ! Columns 11 to 30

      INTEGER   SZVAL            ! Length of keyword string values
      PARAMETER( SZVAL = 70 )    ! Columns 11 to 80

*  Local Variables:
      INTEGER   ADIM             ! Axis loop counter
      LOGICAL   AXIFND           ! True if NDF contains a linear axis
                                 ! comps.
      LOGICAL   AXLFND           ! True if NDF contains axis label
      REAL      AXROT            ! Rotation angle of an axis
      LOGICAL   AXUFND           ! True if NDF contains axis units
      LOGICAL   BANNER           ! Part of the FITSIO banner header?
      CHARACTER C*1              ! Accommodates character string
      CHARACTER CDELT * ( SZKEY ) ! Keyword name of CDELTn
      LOGICAL   CMPFND( NFLAGS ) ! True if certain special NDF
                                 ! components are present
      CHARACTER CRPIX * ( SZKEY ) ! Keyword name of CRPIXn
      CHARACTER CRVAL * ( SZKEY ) ! Keyword name of CRVALn
      CHARACTER CVALUE * ( SZVAL ) ! Accommodates keyword value
      INTEGER   DIMS( NDF__MXDIM ) ! NDF dimensions (axis length)
      LOGICAL   FITSPR           ! True if FITS extension is present
      CHARACTER FITSTR * ( SZFITS ) ! FITS string
      INTEGER   FSTAT            ! FITSIO status
      CHARACTER FTLOC * ( DAT__SZLOC ) ! Locator to NDF FITS extension
      CHARACTER FTLOCI * ( DAT__SZLOC ) ! Locator to element of NDF
                                 ! FITS extension
      INTEGER   I                ! Loop variable
      INTEGER   J                ! Loop variable
      CHARACTER KEYWRD * ( SZKEY ) ! Accommodates keyword name
      LOGICAL   LABFND           ! True if NDF LABEL found
      LOGICAL   MANDAT           ! Not a mandatory header?
      INTEGER   NCHAR            ! Length of a character string
      INTEGER   NCOMP            ! No. of components
      INTEGER   NDIM             ! Number of dimensions
      LOGICAL   ORIGIN           ! Starlink origin header present?
      LOGICAL   ROTAX( DAT__MXDIM ) ! True if an axis is rotated in the
                                 ! FITS extension
      LOGICAL   TITFND           ! True if NDF TITLE found
      LOGICAL   UNTFND           ! True if NDF UNITS found
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
*    CRVALn, CDELTn, CRPIXn, CTYPEn, CUNITn --- are derived from the
*      NDF axis structures if possible.  If no linear NDF axis
*      structures are present, the values in the NDF FITS extension are
*      copied.  If any are non-linear, all FITS axis information is
*      lost.
*    OBJECT, LABEL, BUNITS --- the values held in NDF TITLE, LABEL,
*      and UNITS respectively are used if present, otherwise any values
*      found in the FITS extension are used.
*    ORIGIN and DATE --- are created automatically.  However the former
*      may be overriden by the entry in the NDF extension.
*    BLANK --- is created for integer data types from the bad value.
*
      CALL COF_WNDFH( NDF, COMP, FUNIT, NFLAGS, BITPIX, CMPFND, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Write classification and naming headers.
*  ========================================
      CALL FTPKYS( FUNIT, 'HDUCLAS1', 'NDF', 'Starlink NDF '/
     :             /'(hierarchical n-dim format)', FSTAT )

      CALL FTPKYS( FUNIT, 'HDUCLAS2', COMP, 'Array component subclass',
     :             FSTAT )

      IF ( COMP .NE. 'DATA' ) THEN

*  Obtain the NDF's name.
         CALL FTPKYS( FUNIT, 'EXTNAME', COMP, 'Array component',
     :                FSTAT )
      END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_WHEAD_ERR3', 'FTPKYS',
     :                    'Error writing an EXTNAME or HDUCLASn '/
     :                    /'header card.', STATUS )
         GOTO 999
      END IF

*  Deal with the FITS extension that is present.
*  =============================================

*  Check for presence of NDF FITS extension.
      CALL NDF_XSTAT( NDF, 'FITS', FITSPR, STATUS )
      IF ( FITSPR .AND. PROPEX ) THEN

*  Proceed to merge the headers in the FITS extension into the
*  FITS-file header.  Some items should be ignored including those
*  already set above except:
*    ORIGIN which may be overriden by the entry in the NDF extension.
*  In addition:
*    XTENSION, BLANK and END --- are not propagated from the extension.
*      The first will be set for any extension.  The second may be
*      written as required dependent on the data type and any scaling.
*      The END card is written by FITSIO automatically once the header
*      is closed.

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
*  NAXISn, and possibly CDELTn, CRVALn, CRPIXn, CRTYPEn, CTYPEn,
*  CUNITn, OBJECT, LABEL, BUNITS, DATE, BLANK, HDUCLASn, and END as
*  described above.  Note CROTAn are also excluded.  To avoid duplicate
*  FITSIO banners these are also omitted, as they are written when
*  FITSIO creates the primary headers.
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
            ORIGIN = ( KEYWRD .EQ. 'ORIGIN' ) .AND. 
     :               ( VALUE( 2:23 ) .EQ. 'Starlink Project, U.K.' )

*  Do the test whether to copy the FITS extension header into the output
*  FITS file's header.
            IF ( MANDAT .AND. .NOT. BANNER .AND. .NOT. ORIGIN .AND.
     :        ( KEYWRD .NE. 'DATE' ) .AND.
     :        ( KEYWRD .NE. 'BLANK' ) .AND.
     :        ( KEYWRD .NE. 'BSCALE' ) .AND.
     :        ( KEYWRD .NE. 'BZERO' ) .AND.
     :        ( KEYWRD .NE. 'EXTNAME' ) .AND.
     :        ( KEYWRD( 1:7 ) .NE. 'HDUCLAS' ) .AND.
     :        ( KEYWRD( 1:5 ) .NE. 'CDELT' .OR. .NOT. AXIFND ) .AND.
     :        ( KEYWRD( 1:5 ) .NE. 'CRVAL' .OR. .NOT. AXIFND ) .AND.
     :        ( KEYWRD( 1:5 ) .NE. 'CRPIX' .OR. .NOT. AXIFND ) .AND.
     :        ( KEYWRD( 1:6 ) .NE. 'CRTYPE' .OR. .NOT. AXLFND ) .AND.
     :        ( KEYWRD( 1:5 ) .NE. 'CTYPE' .OR. .NOT. AXLFND ) .AND.
     :        ( KEYWRD( 1:5 ) .NE. 'CUNIT' .OR. .NOT. AXUFND ) .AND.
     :        ( KEYWRD .NE. 'LABEL' .OR. .NOT. LABFND ) .AND.
     :        ( KEYWRD .NE. 'BUNITS' .OR. .NOT. UNTFND ) .AND.
     :        ( KEYWRD .NE. 'OBJECT' .OR. .NOT. TITFND ) ) THEN

*  Look for a rotated axis in the FITS extension (CROTAn is present and
*  non-zero).  If there is one, the NDF AXIS structure will contain
*  pixel co-ordinates, which are probably not the original co-ordinates
*  for the axis.  If there are rotated axes, the keywords defining the
*  axis will be re-written later using the values of CRVALn and CDELTn
*  in the FITS extension.  When there is no AXIS component in the NDF,
*  then the axis keywords can be written immediately to the output
*  FITS header.
               IF ( INDEX( KEYWRD, 'CROTA' ) .NE. 0 .AND.
     :              AXIFND ) THEN
                  CALL CHR_CTOI( KEYWRD( 6: ), ADIM, STATUS )
                  CALL CHR_CTOR( VALUE( :SZNVAL ), AXROT, STATUS )
                  ROTAX( ADIM ) = ABS( AXROT ) .GT. VAL__EPSR
               ELSE

*  Write the header card.
                  CALL FTPREC( FUNIT, FITSTR, FSTAT )
               END IF

*  Just skip over non-valid cards.  Thus defective cards are not
*  propagated.  There is no warning, but users should check their
*  headers anyway, and we should aim to have header-validation
*  software.
               IF ( FSTAT .GT. FITSOK ) FSTAT = FITSOK
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
                     CALL FTPREC( FUNIT, FITSTR, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
                     IF ( FSTAT .GT. FITSOK ) THEN
                        CALL COF_FIOER( FSTAT, 'COF_WHEAD_ERR2',
     :                    'FTPREC', 'Error copying FITS axis-rotation '/
     :                    /'header card to the FITS file.', STATUS )
                        GOTO 999
                     END IF
                  END IF
               END DO
            END IF
         END DO      
      END IF

  999 CONTINUE

      END
