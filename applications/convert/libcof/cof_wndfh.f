      SUBROUTINE COF_WNDFH( NDF, COMP, FUNIT, NFLAGS, BITPIX, CMPTHE,
     :                      STATUS )
*+
*  Name:
*     COF_WNDFH

*  Purpose:
*     Writes out the mandatory and axis FITS header cards derived from
*     an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_WNDFH( NDF, COMP, FUNIT, NFLAGS, BITPIX, CMPTHE, STATUS )

*  Description:
*     This routine writes the mandatory and the optional but reserved
*     header cards into a primary header and data unit.  The values for
*     the keywords of the header cards are determined from the supplied
*     NDF.  The routine records which of the optional headers cards are
*     defined.

*     The keywords are:
*        o  NAXIS, and NAXISn are derived from the dimensions of 
*           the NDF data array.
*        o  For an NDF whose origin is not 1 along each axis, LBOUNDn
*           cards are written. (These are not part of the standard.)
*        o  The OBJECT, LABEL, and BUNITS keywords are derived from
*           the TITLE, LABEL, and UNITS NDF components.
*        o  The CDELTn, CRVALn, CRTYPEn and CTYPEn keywords are
*           derived from a set of linear NDF AXIS structures.
*        o  DATE and ORIGIN cards are written.
*        o  For integer DATA types a BLANK card is written using the
*           standard bad value corresponding to the type of the FITS
*           array (except for the QUALITY array).
*        o  Dummy BSCALE and BZERO cards are written with values 1.0 and
*           0.0 respectively.
*        o  The standard order of the FITS keywords is preserved.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF.
*     COMP = CHARACTER * ( * ) (Given)
*        The array component to write to the HDU.
*     FUNIT = INTEGER (Given)
*        The logical unit number of the output FITS file.
*     NFLAGS = INTEGER (Given)
*        The number of flags used to indicate that certain NDF
*        components have been used to write header cards to the FITS
*        file.  It should be set to 6.
*     BITPIX = INTEGER (Given)
*        The BITPIX value for the output FITS file.
*     CMPTHE( NFLAGS ) = LOGICAL (Returned)
*        The flags when set to true indicate that certain optional NDF
*        components have been used to write descriptors to the BDF.
*        In order they are 1) CRVARn and CDELTn, 2) CRTYPEn, 3) CTYPEn,
*        4) TITLE, 5) LABEL, and 6) UNITS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1994 May 31 (MJC):
*        Original version.
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
      CHARACTER * ( * ) COMP      ! The array component
      INTEGER   FUNIT             ! Logical-unit number of FITS file
      INTEGER   NFLAGS            ! Number of flags to indicate
                                  ! presence of certain components
      INTEGER   BITPIX            ! Bits per pixel

*  Arguments Returned:
      LOGICAL   CMPTHE( NFLAGS )

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
      INTEGER   APNTR( NDF__MXDIM ) ! Pointers to NDF axis arrays
      CHARACTER * ( NDF__SZTYP ) ATYPE ! Data type of the axis centres
      LOGICAL   AXIFND            ! True if NDF contains a linear axis
                                  ! comps.
      LOGICAL   AXLFND            ! True if NDF contains axis label
      LOGICAL   AXUFND            ! True if NDF contains axis units
      CHARACTER C*1               ! Accommodates character string
      LOGICAL   DEFORG            ! True if NDF pixel origins are all 1
      DOUBLE PRECISION DEND       ! End value for an axis-centre array
      INTEGER   DIMS( NDF__MXDIM ) ! NDF dimensions (axis length)
      DOUBLE PRECISION DINCRE     ! Incremental value for axis array
      DOUBLE PRECISION DSTART     ! Start value for an axis-centre array
      REAL      END               ! End value for an axis-centre array
      INTEGER   FSTAT             ! FITSIO status
      INTEGER   I                 ! Loop variable
      REAL      INCREM            ! Incremental value for axis array
      CHARACTER KEYWRD * ( SZKEY ) ! Accommodates keyword name
      LOGICAL   LABFND            ! True if NDF LABEL found
      INTEGER   LBND( NDF__MXDIM ) ! NDF lower bounds
      LOGICAL   LINEAR            ! True if an axis is linear
      INTEGER   NCHAR             ! Length of a character string
      INTEGER   NDIM              ! Number of dimensions
      INTEGER   NELM              ! Number of elements
      REAL      START             ! Start value for an axis structure
      LOGICAL   THERE             ! True if NDF has FITS extension
      LOGICAL   TITFND            ! True if NDF TITLE found
      INTEGER   UBND( NDF__MXDIM ) ! NDF upper bounds
      LOGICAL   UNTFND            ! True if NDF UNITS found
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

*  Write the mandatory headers.
*  ============================
      CALL FTPHPR( FUNIT, SIMPLE, BITPIX, NDIM, DIMS, PCOUNT, GCOUNT,
     :             EXTEND, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_WNDFH_ERR1', 'FTPHPR',
     :                    'Error writing mandatory header cards.',
     :                    STATUS )
         GOTO 999
      END IF

*  Handle axis header cards.
*  =========================
*  -------------------------
*      
*  For any axis structure present, the routine checks to see if each
*  axis data array is linear. If it is, the start value and incremental
*  value are written to the appropriate CRVALn and CDELTn keywords, as
*  are the label and units, if present, to CRTYPEn and CTYPEn
*  respectively.  This is rather crude, as it deals with the axis
*  system as a whole, and that the flags to indicate presence of
*  components are for any of the axes.
      DO I = 1, NDIM 
         CALL NDF_ASTAT( NDF, 'Centre', I, THERE, STATUS )

         IF ( THERE ) THEN

*  Determine the data type of the axis array.
            CALL NDF_ATYPE( NDF, 'Centre', I, ATYPE, STATUS )

*  The axis structure is found, so map it using an appropriate data
*  type.  Use _REAL for all but double-precision centres.  See if the
*  axis is linear.  Derive the increment between values.
            IF ( ATYPE .EQ. '_DOUBLE' ) THEN
               CALL NDF_AMAP( NDF, 'Centre', I, '_DOUBLE', 'READ', 
     :                        APNTR( I ), NELM, STATUS )

               CALL CON_AXLID( NELM, %VAL( APNTR( I ) ), DSTART,
     :                          DEND, LINEAR, STATUS )

               IF ( LINEAR ) THEN
                  DINCRE = ( DEND - DSTART ) / DBLE( NELM - 1 )
               END IF

*  Repeat for all other axis-centre data types mapped as real.
            ELSE
               CALL NDF_AMAP( NDF, 'Centre', I, '_REAL', 'READ', 
     :                        APNTR( I ), NELM, STATUS )

               CALL CON_AXLIR( NELM, %VAL( APNTR( I ) ), START,
     :                          END, LINEAR, STATUS )

               IF ( LINEAR ) THEN
                  INCREM = ( END - START ) / REAL( NELM - 1 )
               END IF
            END IF

*  Unmap the array.
            CALL NDF_AUNMP( NDF, 'Centre', I, STATUS )

            IF ( LINEAR ) THEN

*  It is linear.  Record the fact to prevent copying axis information
*  from the FITS extension.
               AXIFND = .TRUE.

*  Write the start value to keyword CRVALn.
*  ========================================

*  Form the keyword name.
               KEYWRD = ' '
               CALL CHR_ITOC( I, C, NCHAR )
               KEYWRD = 'CRVAL'//C( 1:NCHAR )

*  Write the CRVALn card to the FITS header.  Allow space in the value
*  for the maximum number of decimal places required for the
*  appropriate data type.
               IF ( ATYPE .EQ. '_DOUBLE' ) THEN
                  CALL FTPKYD( FUNIT, KEYWRD, DSTART, VAL__SZD - 7,
     :                         'Co-ordinate value of axis '//C, FSTAT )
               ELSE
                  CALL FTPKYE( FUNIT, KEYWRD, START, VAL__SZR - 7,
     :                         'Co-ordinate value of axis '//C, FSTAT )
               END IF

*  Write the incremental value to keyword CDELTn.
*  ===============================================

*  Form the keyword name.
               KEYWRD = 'CDELT'//C

*  Write the CDELTn card to the FITS header.  Allow space in the value
*  for the maximum number of decimal places required for the
*  appropriate data type.
               IF ( ATYPE .EQ. '_DOUBLE' ) THEN
                  CALL FTPKYD( FUNIT, KEYWRD, DINCRE, VAL__SZD - 7,
     :                         'Co-ordinate increment along axis '//C,
     :                         FSTAT )
               ELSE
                  CALL FTPKYE( FUNIT, KEYWRD, INCREM, VAL__SZR - 7,
     :                         'Co-ordinate increment along axis '//C,
     :                         FSTAT )
               END IF

*  Write the label value to keyword CRTYPEn.
*  =========================================

*  See whether an axis label is present or not.
               AXLFND = .FALSE.
               CALL NDF_ASTAT( NDF, 'Label', I, AXLFND, STATUS )
               IF ( AXLFND ) THEN

*  Form the keyword name.
                  KEYWRD = 'CRTYPE'//C

*  Obtain the label's value and length.
                  CALL NDF_ACGET( NDF, 'Label', I, VALUE, STATUS )
                  CALL NDF_ACLEN( NDF, 'Label', I, NCHAR, STATUS )

*  Remove unprintable characters that are not permitted in FITS.
                  CALL CHR_CLEAN( VALUE )

*  Write the CRTYPEn card to the FITS header.
                  CALL FTPKYS( FUNIT, KEYWRD,
     :                         VALUE( :MIN( SZVAL, NCHAR ) ),
     :                         'Label for axis '//C, FSTAT )
               END IF

*  Write the units value to keyword CTYPEn.
*  ========================================

*  See whether an axis units is present or not.
               AXUFND = .FALSE.
               CALL NDF_ASTAT( NDF, 'Units', I, AXUFND, STATUS )
               IF ( AXUFND ) THEN

*  Form the keyword name.
                  KEYWRD = 'CTYPE'//C

*  Obtain the units' value and length.
                  CALL NDF_ACGET( NDF, 'Units', I, VALUE, STATUS )
                  CALL NDF_ACLEN( NDF, 'Units', I, NCHAR, STATUS )

*  Remove unprintable characters that are not permitted in FITS.
                  CALL CHR_CLEAN( VALUE )

*  Write the CTYPEn card to the FITS header.
                  CALL FTPKYS( FUNIT, KEYWRD,
     :                         VALUE( :MIN( SZVAL, NCHAR ) ),
     :                         'Units for axis '//C, FSTAT )
               
               END IF

            END IF
         END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( FSTAT .GT. FITSOK ) THEN
            CALL COF_FIOER( FSTAT, 'COF_WNDFH_ERR2', 'FTPKYx',
     :                       'Error writing axis'//C//' header cards.',
     :                      STATUS )
            GOTO 999
         END IF
         
      END DO

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
            CALL FTPKYJ( FUNIT, KEYWRD, LBND( I ), 
     :                         'Pixel origin along axis '//C, FSTAT )
         END DO

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( FSTAT .GT. FITSOK ) THEN
            CALL COF_FIOER( FSTAT, 'COF_WNDFH_ERR2', 'FTPKYJ',
     :                       'Error writing LBOUNDn header cards.',
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

*  Write the TITLE card to the FITS header.  68 is the maximum number of
*  characters that can be accommodated in a header card.
         CALL FTPKYS( FUNIT, KEYWRD, VALUE( :MIN( SZVAL, NCHAR ) ),
     :                'Title of the dataset', FSTAT )

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
         CALL FTPKYS( FUNIT, KEYWRD, VALUE( :MIN( SZVAL, NCHAR ) ),
     :                'Label of the primary array', FSTAT )

*  Record the fact that the label has been written.
         LABFND = .TRUE.
      END IF

*  Process the units.
*  ==================
*   
*  Determine whether or not there is a label present in the NDF.
      CALL NDF_STATE( NDF, 'UNITS', THERE, STATUS )

*  If an NDF label is found, this is copied to the FITS header card
*  with keyword BUNITS.
      IF ( THERE ) THEN
         CALL NDF_CGET( NDF, 'UNITS', VALUE, STATUS )
         CALL NDF_CLEN( NDF, 'UNITS', NCHAR, STATUS )
         KEYWRD = 'BUNITS  '

*  Remove unprintable characters that are not permitted in FITS.
         CALL CHR_CLEAN( VALUE )

*  Write the BUNITS card to the FITS header.  68 is the maximum number
*  of characters that can be accommodated in a header card.
         CALL FTPKYS( FUNIT, KEYWRD, VALUE( :MIN( SZVAL, NCHAR ) ),
     :                'Units of the primary array', FSTAT )

*  Record the fact that the title has been written.
         UNTFND = .TRUE.
      END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_WNDFH_ERR3', 'FTPKYS',
     :                    'Error writing an NDF character component '/
     :                    /'to a header card. ', STATUS )
         GOTO 999
      END IF

*  Write the DATE card in reverse format.
*  ======================================
      CALL FTPDAT( FUNIT, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_WNDFH_ERR4', 'FTPDAT',
     :                    'Error writing the DATE header card.',
     :                    STATUS )
         GOTO 999
      END IF

*  Write a default ORIGIN card.
*  ============================
      CALL FTPKYS( FUNIT, 'ORIGIN', 'Starlink Project, U.K.',
     :             'Origin of this FITS file', FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_WNDFH_ERR4', 'FTPKYS',
     :                    'Error writing the ORIGIN header card.',
     :                    STATUS )
         GOTO 999
      END IF

*  Write dummy scale and offset cards.
*  ===================================

      CALL FTPKYD( FUNIT, 'BSCALE', 1.0D0, 1,
     :             'True_value = BSCALE * FITS_value + BZERO', FSTAT )

      CALL FTPKYD( FUNIT, 'BZERO', 0.0D0, 1,
     :             'True_value = BSCALE * FITS_value + BZERO', FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_WNDFH_ERR5', 'FTPKYD',
     :                    'Error writing the BSCALE or BZERO '/
     :                    /'header card.', STATUS )
         GOTO 999
      END IF


*  Write a dummy blank keyword.
*  ============================

      IF ( BITPIX .GT. 0 .AND. COMP .NE. 'QUALITY' ) THEN

*  Write the BLANK card to the FITS header.  The actual value may be
*  changed later.  This just reserves a place in the header.  This
*  assumes that the user has not have set up private bad values in the
*  NDF and set BLANK in the NDF's FITS extension.
         IF ( BITPIX .EQ. 32 ) THEN
            CALL FTPKYJ( FUNIT, 'BLANK', VAL__BADI, 'Bad value', FSTAT )

         ELSE IF ( BITPIX .EQ. 16 ) THEN
            CALL FTPKYJ( FUNIT, 'BLANK', VAL__BADW, 'Bad value', FSTAT )

         ELSE IF ( BITPIX .EQ. 8 ) THEN
            CALL FTPKYJ( FUNIT, 'BLANK', VAL__BADUB, 'Bad value',
     :                   FSTAT )
         END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( FSTAT .GT. FITSOK ) THEN
            CALL COF_FIOER( FSTAT, 'COF_WNDFH_ERR6', 'FTPKYJ',
     :                       'Error writing the BLANK header card.',
     :                       STATUS )
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
