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
*        o  For an NDF whose origin is not 1 along each axis, LBOUNDn
*           cards are written. (These are not part of the standard.)
*        o  The OBJECT, LABEL, and BUNIT keywords are derived from
*           the TITLE, LABEL, and UNITS NDF components.
*        o  The CDELTn, CRPIXn, CRVALn keywords are derived from a
*           set of linear NDF AXIS structures.  Whenever the NDF AXIS
*           structure contains units or a label CUNITn and CTYPEn
*           keywords are created repsectively.
*        o  DATE and ORIGIN cards are written.
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
*        In order they are 1) CRVARn, CDELTn, and CRPIXn 2) CUNITn,
*        3) CTYPEn, 4) TITLE, 5) LABEL, and 6) UNITS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

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
*     1996 Nov 22 (PDRAPER):
*        Converted for GAIA/RTD.
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
      LOGICAL CMPTHE( 6 )

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
      CHARACTER * ( NDF__SZTYP ) ATYPE ! Data type of the axis centres
      CHARACTER C*1             ! Accommodates character string
      CHARACTER KEYWRD * ( SZKEY ) ! Accommodates keyword name
      CHARACTER VALUE * ( SZVAL ) ! Accommodates keyword value
      DOUBLE PRECISION DEND     ! End value for an axis-centre array
      DOUBLE PRECISION DINCRE   ! Incremental value for axis array
      DOUBLE PRECISION DREFVL   ! Value of reference pixel
      DOUBLE PRECISION DSTART   ! Start value for an axis-centre array
      INTEGER   APNTR( NDF__MXDIM ) ! Pointers to NDF axis arrays
      INTEGER   DIMS( NDF__MXDIM ) ! NDF dimensions (axis length)
      INTEGER   I               ! Loop variable
      INTEGER   LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER   NCHAR           ! Length of a character string
      INTEGER   NDIM            ! Number of dimensions
      INTEGER   NELM            ! Number of elements
      INTEGER   UBND( NDF__MXDIM ) ! NDF upper bounds
      INTEGER REFPIX            ! Position of reference pixel
      LOGICAL AXIFND            ! True if NDF contains a linear axis comps.
      LOGICAL AXLFND            ! True if NDF contains axis label
      LOGICAL AXUFND            ! True if NDF contains axis units
      LOGICAL DEFORG            ! True if NDF pixel origins are all 1
      LOGICAL LABFND            ! True if NDF LABEL found
      LOGICAL LINEAR            ! True if an axis is linear
      LOGICAL THERE             ! True if NDF has FITS extension
      LOGICAL TITFND            ! True if NDF TITLE found
      LOGICAL UNTFND            ! True if NDF UNITS found
      LOGICAL BAD               ! Data array has BAD pixels
      REAL END                  ! End value for an axis-centre array
      REAL INCREM               ! Incremental value for axis array
      REAL START                ! Start value for an axis structure
      REAL REFVAL                 ! Value of reference pixel

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
      CALL NDF_BAD( NDF, 'DATA', .FALSE., BAD, STATUS )
      CALL RTD1_SNDFH( NDF, BITPIX, BAD, NHEAD, IPHEAD, AVAIL, STATUS )

*  Handle axis header cards.
*  =========================
*  -------------------------
*      
*  For any axis structure present, the routine checks to see if each
*  axis data array is linear.  If it is, the start value and incremental
*  value are written to the appropriate CRVALn and CDELTn keywords
*  defined at a reference pixel 1.0 written to the CRPIXn keyword,
*  as are the label and units, if present, to CTYPEn and CUNITn
*  respectively.  This is rather crude, as it deals with the axis
*  system as a whole, and that the flags to indicate presence of
*  components are for any of the axes. None of this is a fatal error so
*  just silently give up if any errors occur.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL ERR_MARK
      DO I = 1, NDIM 
         CALL NDF_ASTAT( NDF, 'Centre', I, THERE, STATUS )

         IF ( THERE ) THEN

*  Determine the data type of the axis array.
            CALL NDF_ATYPE( NDF, 'Centre', I, ATYPE, STATUS )

*  The axis structure is found, so map it using an appropriate data
*  type.  Use _REAL for all but double-precision centres.  See if the
*  axis is linear.
            IF ( ATYPE .EQ. '_DOUBLE' ) THEN
               CALL NDF_AMAP( NDF, 'Centre', I, '_DOUBLE', 'READ', 
     :                        APNTR( I ), NELM, STATUS )

               CALL RTD_AXLID( NELM, %VAL( APNTR( I ) ), DSTART,
     :                         DEND, REFPIX, DREFVL, LINEAR, STATUS )

*  Derive the increment between values.
               IF ( LINEAR ) THEN
                  DINCRE = ( DEND - DSTART ) / DBLE( NELM - 1 )
               END IF

*  Repeat for all other axis-centre data types mapped as real.
            ELSE
               CALL NDF_AMAP( NDF, 'Centre', I, '_REAL', 'READ', 
     :                        APNTR( I ), NELM, STATUS )

               CALL RTD_AXLIR( NELM, %VAL( APNTR( I ) ), START,
     :                         END, REFPIX, REFVAL, LINEAR, STATUS )

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
                  CALL RTD1_WRFTD( KEYWRD, DREFVL, 
     :                 'Co-ordinate value of axis '//C, IPHEAD, NHEAD, 
     :                 AVAIL, STATUS)
               ELSE
                  CALL RTD1_WRFTR( KEYWRD, REFVAL, 
     :                 'Co-ordinate value of axis '//C, IPHEAD, NHEAD, 
     :                 AVAIL, STATUS)
               END IF

*  Write the incremental value to keyword CDELTn.
*  ===============================================

*  Form the keyword name.
               KEYWRD = 'CDELT'//C

*  Write the CDELTn card to the FITS header.  Allow space in the value
*  for the maximum number of decimal places required for the
*  appropriate data type.
               IF ( ATYPE .EQ. '_DOUBLE' ) THEN
                  CALL RTD1_WRFTD( KEYWRD, DINCRE,
     :                 'Co-ordinate increment along axis '//C, IPHEAD, 
     :                 NHEAD, AVAIL, STATUS )
               ELSE
                  CALL RTD1_WRFTR( KEYWRD, INCREM,
     :                 'Co-ordinate increment along axis '//C, IPHEAD, 
     :                 NHEAD, AVAIL, STATUS )
               END IF

*  Write the reference pixel to keyword CRPIXn.
*  ============================================

*  Form the keyword name.
               KEYWRD = 'CRPIX'//C

*  Write the CRPIXn card to the FITS header.  Allow space in the value
*  for the maximum number of decimal places required for the
*  appropriate data type.
               IF ( ATYPE .EQ. '_DOUBLE' ) THEN
                  CALL RTD1_WRFTD( KEYWRD, DBLE(REFPIX),
     :                 'Reference pixel along axis '//C, IPHEAD, 
     :                 NHEAD, AVAIL, STATUS )
               ELSE
                  CALL RTD1_WRFTR( KEYWRD, REAL(REFPIX),
     :                 'Reference pixel along axis '//C, IPHEAD, 
     :                 NHEAD, AVAIL, STATUS )
               END IF

*  Write the label value to keyword CTYPEn.
*  ========================================

*  See whether an axis label is present or not.
               AXLFND = .FALSE.
               CALL NDF_ASTAT( NDF, 'Label', I, AXLFND, STATUS )
               IF ( AXLFND ) THEN

*  Form the keyword name.
                  KEYWRD = 'CTYPE'//C

*  Obtain the label's value and length.
                  CALL NDF_ACGET( NDF, 'Label', I, VALUE, STATUS )
                  CALL NDF_ACLEN( NDF, 'Label', I, NCHAR, STATUS )

*  Remove unprintable characters that are not permitted in FITS.
                  CALL CHR_CLEAN( VALUE )

*  Write the CRTYPEn card to the FITS header.
                  CALL RTD1_WRFTC( KEYWRD, 
     :                 VALUE( :MIN( SZVAL, NCHAR )),
     :                 'Label for axis '//C, IPHEAD, NHEAD, AVAIL, 
     :                 STATUS )
               END IF

*  Write the units value to keyword CUNITn.
*  ========================================

*  See whether an axis units is present or not.
               AXUFND = .FALSE.
               CALL NDF_ASTAT( NDF, 'Units', I, AXUFND, STATUS )
               IF ( AXUFND ) THEN

*  Form the keyword name.
                  KEYWRD = 'CUNIT'//C

*  Obtain the units' value and length.
                  CALL NDF_ACGET( NDF, 'Units', I, VALUE, STATUS )
                  CALL NDF_ACLEN( NDF, 'Units', I, NCHAR, STATUS )

*  Remove unprintable characters that are not permitted in FITS.
                  CALL CHR_CLEAN( VALUE )

*  Write the CTYPEn card to the FITS header.
                  CALL RTD1_WRFTC( KEYWRD, 
     :                 VALUE( :MIN( SZVAL, NCHAR )),
     :                 'Units for axis '//C, IPHEAD, NHEAD, AVAIL,
     :                  STATUS )
               END IF

            END IF
         END IF
      END DO
      IF ( STATUS .NE. SAI__OK ) THEN 
         CALL ERR_FLUSH( STATUS )
      END IF
      CALL ERR_RLSE

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

*  Write a default ORIGIN card.
*  ============================
      CALL RTD1_WRFTC( 'ORIGIN', 'Starlink Project, U.K.',
     :     'Origin of this FITS file', IPHEAD, NHEAD, AVAIL, STATUS )

*  Write scale and offset cards.
*  ===================================

      CALL RTD1_WRFTD( 'BSCALE', 1.0D0,
     :     'True_value = BSCALE * FITS_value + BZERO', IPHEAD, NHEAD, 
     :     AVAIL, STATUS )

      CALL RTD1_WRFTD( 'BZERO', 0.0D0,
     :     'True_value = BSCALE * FITS_value + BZERO', IPHEAD, NHEAD, 
     :     AVAIL, STATUS )

      CMPTHE( 1 ) = AXIFND 
      CMPTHE( 2 ) = AXLFND
      CMPTHE( 3 ) = AXUFND
      CMPTHE( 4 ) = TITFND
      CMPTHE( 5 ) = LABFND
      CMPTHE( 6 ) = UNTFND

 99   CONTINUE
      END
