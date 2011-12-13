      SUBROUTINE COI_MULAX( IMDESC, NDF, BUFFER, VALUE, STATUS )
*+
*  Name:
*     COI_MULAX

*  Purpose:
*     Creates axis centres within an NDF from Multispec axis information
*     in an IRAF header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COI_MULAX( IMDESC, NDF, BUFFER, VALUE, STATUS )

*  Description:
*     The routine searches the IRAF headers for the keywords that
*     describe an axis structure stored using the IRAF Mini World
*     Co-ordinate System (MWCS) using the Multispec format.
*
*     The data type of the output axis-centre array is double precision.

*  Arguments:
*     IMDESC = INTEGER (Given)
*        The IMFORT file descriptor.
*     NDF = INTEGER (Given)
*        The identifier for the NDF to contain the axis structure.
*     BUFFER = CHARACTER * ( * ) (Returned)
*        Workspace to store concatenated value from multi-line headers
*        WATd_nnn.  It should be at least 68 characters long times the
*        maximum number of lines per WATd_ header.  The safest approach
*        is to find the total number of header lines, subtract say 10
*        for the other headers and multiply by 68.
*     VALUE = CHARACTER * ( * ) (Returned)
*        Workspace to store concatenated value from multi-line headers
*        WATd_nnn.  It should be at least 68 characters long times the
*        maximum number of lines per WATd_ header.  The safest approach
*        is to find the total number of header lines, subtract say 10
*        for the other headers and multiply by 68.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The allowed variants of multispec functions are:
*        -  Linear
*        -  Log-linear
*        -  Chebyshev polynomial
*        -  Legendre polynomial
*        -  Linear spline
*        -  Cubic spline
*        -  Explicit array of values
*
*     Oonly the first (spec1) axis co-ordinates are transferred to the
*     NDF AXIS centres.  Any spec2...specn co-ordinates, present when
*     the data array is not one-dimensional or multiple fits have been
*     stored, are ignored.  The weights for multiple fits are thus also
*     ignored.  The data type of the axis centres is _REAL or _DOUBLE
*     depending on the number of significant digits in the co-ordinates
*     or coefficients.
*
*     The axis labels and units are also propagated, where present, to
*     the NDF AXIS structure.  These components originate in the label
*     and units parameters.
*
*     The redshift correction, when present, is applied to the axis
*     co-ordinates.

*  Deficiencies:
*     Does not handle all of the ftypes.

*  Copyright:
*     Copyright (C) 1997, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council. Copyright (C) 2008 Science & Technology
*     Facilities Council. All Rights Reserved.

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
*     {enter_new_authors_here}

*  History:
*     1997 July 24 (MJC):
*        Original version.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2006 April 13 (MJC):
*        Remove unused variables.
*     2008 March 15 (MJC):
*        Use KAPLIBS routine instead of its cloned CON equivalent.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER IMDESC
      INTEGER NDF

*  Arguments Returned:
      CHARACTER * ( * ) BUFFER
      CHARACTER * ( * ) VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a character string less
                                 ! trailing blanks

*  Local Constants:
      INTEGER IMOK               ! IMFORT OK status
      PARAMETER( IMOK = 0 )

      INTEGER MAXWRD             ! Maximum number of words
      PARAMETER( MAXWRD = 12 )

      INTEGER MXCOEF             ! Maximum number of polynomial
                                 ! coefficents
      PARAMETER( MXCOEF = 15 )

*  Local Variables:
      INTEGER BUFLEN             ! Character length of the multi-line
                                 ! buffer for WATd_nnn headers
      REAL COEF( MXCOEF )        ! Polynomial coeeficients
      INTEGER CPOS               ! CUrrent column position
      INTEGER CSTAT              ! CHR status
      DOUBLE PRECISION DCOEF( MXCOEF ) ! Polynomial coeeficients
      DOUBLE PRECISION DELT      ! The co-ordinate increment between
                                 ! pixels
      INTEGER DIMS( NDF__MXDIM ) ! The dimensions of the NDF
      DOUBLE PRECISION DPMAX     ! Upper scaling limit for polynomials
      DOUBLE PRECISION DPMIN     ! Lower scaling limit for polynomials
      INTEGER DTYPE              ! Data linearity code
      INTEGER EL                 ! Dimension of the current axis
      INTEGER END( MAXWRD )      ! End column of each word
      INTEGER FTYPE              ! Non-linear-axis storage variant
      INTEGER I                  ! Loop counter
      CHARACTER * ( DAT__SZTYP ) ITYPE ! Data type of the axis centres
      CHARACTER * ( 70 ) LABEL   ! Axis label
      INTEGER NCL                ! Number of characters in label
      INTEGER NCU                ! Number of characters in units
      INTEGER NDIM               ! Number of dimensions of the NDF
      INTEGER NPIECE             ! Number of pieces of the NDF
      INTEGER NSDIG              ! Number of significant digits
      INTEGER NVALUE             ! Number of values in the header
      INTEGER NWORD              ! Number of words
      DOUBLE PRECISION OFFSET    ! Offset after allowing for the
                                 ! position of reference pixel
      INTEGER ORDER              ! Number of polynomial coefficients
      REAL PMAX                  ! Upper scaling limit for polynomials
      REAL PMIN                  ! Lower scaling limit for polynomials
      INTEGER PNTR( 1 )          ! Pointer the mapped CENTRE component
      INTEGER START( MAXWRD )    ! Start column of each word
      LOGICAL THERE              ! The nominated FITS keyword is present
                                 ! in the header?
      CHARACTER * ( VAL__SZD ) WORDS( MAXWRD ) ! spec1 sub parameters
      CHARACTER * ( 70 ) UNITS   ! Axis units
      DOUBLE PRECISION Z         ! Redshift

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the dimensions of the NDF.
      CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  Create a default axis system in case only some of the FITS axes are
*  defined in the headers.
      CALL NDF_ACRE( NDF, STATUS )

*  Ensure that the axis-centre data type is double precision.
      CALL NDF_ASTYP( '_DOUBLE', NDF, 'Centre', 1, STATUS )

*  Obtain the axis label and units from IRAF MWCS headers.
*  =======================================================

*  Concatenate the WAT<dim>_nnn header, which may contain the axis label
*  and units.
      CALL ERR_MARK
      CALL COI_WCCAT( IMDESC, 1, BUFFER, BUFLEN, STATUS )

*  Extract the label.  Watch for a null string.
      IF ( STATUS .EQ. SAI__OK .AND. BUFLEN .GT. 1 ) THEN

*  Extract the label.
         CALL COI_WCWRD( BUFFER( :BUFLEN ), 'label', LABEL,
     :                   THERE, STATUS )

*  If a label is present, write it to the NDF AXIS structure for the
*  current axis.  Pass only the used length not to waste space in the
*  NDF.
         IF ( THERE ) THEN
            NCL = CHR_LEN( LABEL )
            CALL NDF_ACPUT( LABEL( :NCL ), NDF, 'Label', 1, STATUS )
         END IF

*  Extract the units.
         CALL COI_WCWRD( BUFFER( :BUFLEN ), 'units', UNITS, THERE,
     :                   STATUS )

*  If units value is present, write it to the NDF AXIS structure for
*  the current axis.  Pass only the used length not to waste space in
*  the NDF.
         IF ( THERE ) THEN
            NCU = CHR_LEN( UNITS )
            CALL NDF_ACPUT( UNITS( :NCU ), NDF, 'Units', 1, STATUS )
         END IF

*  It is not serious if we cannot obtain the header card, as the
*  character components of the axis may not be present.
      ELSE
         CALL ERR_ANNUL( STATUS )
      END IF
      CALL ERR_RLSE

*  Obtain the spec1 from the IRAF MWCS headers.
*  ============================================

*  Concatenate the WAT2_nnn headers, which should contain the spec1
*  parameter.  NDF cannot cope with n-dimensional centres along an axis,
*  so specN values will be given the same wavelength scale.
      CALL ERR_MARK
      CALL COI_WCCAT( IMDESC, 2, BUFFER, BUFLEN, STATUS )

*  Extract the spec1 parameter's value.
      CALL COI_WCWRD( BUFFER( :BUFLEN ), 'spec1', VALUE, THERE, STATUS )

*  Only proceed if the spec1 parameter was obained successfully.
      IF ( .NOT. THERE ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'COI_MULAX_NOSPEC1',
     :     'COI_MULAX: spec1 parameter missing from WAT2_nnn header. '/
     :     /'It is needed for multispec axes.', STATUS )
      END IF

*  Split the SpecN sub-parameters into words.  Length is at least 8
*  characters fewer than the concatenated header value.  Use a local
*  status as we expect more words in the buffer than elements of the
*  WORDS array.
      CALL CHR_DCWRD( VALUE( :BUFLEN-8 ), MAXWRD, NWORD, START, END,
     :                WORDS, CSTAT )

*  The linearity is defined by word 3.  0 is linear, 1 is log-linear,
*  2 is non-linear.
      CALL CHR_CTOI( WORDS( 3 ), DTYPE, STATUS )

*  The seventh is the redshift.
      CALL CHR_CTOD( WORDS( 7 ), Z, STATUS )

*  Linear case and log-linear cases:
*  =================================
      IF ( DTYPE .EQ. 0 .OR. DTYPE .EQ. 1 ) THEN

*  It's linear.  The fourth and fifth words given the start wavelength
*  and offset.
         CALL CHR_CTOD( WORDS( 4 ), OFFSET, STATUS )
         CALL CHR_CTOD( WORDS( 5 ), DELT, STATUS )

*  Correct the start and increments for the redshift.
         DELT = DELT / ( 1.0D0 + Z )
         OFFSET = OFFSET / ( 1.0D0 + Z )

*  Map the centre array in the axis structure.
         CALL NDF_AMAP( NDF, 'Centre', 1, '_DOUBLE', 'WRITE',
     :                  PNTR, EL, STATUS )

*  Test status before accessing the pointer.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL KPG1_SSAZD( EL, DELT, OFFSET,
     :                       %VAL( CNF_PVAL( PNTR( 1 ) ) ), STATUS )

*  Exponentiate a log-linear axis.
            IF ( DTYPE .EQ. 1 )
     :        CALL COI_ALOGR( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                        STATUS )

*  Unmap the axis array.
            CALL NDF_AUNMP( NDF, 'Centre', 1, STATUS )

         END IF

*  Non-linear case:
*  ================
      ELSE

*  The type of function is defined by word 12.
         CALL CHR_CTOI( WORDS( 12 ), FTYPE, STATUS )

*  Listed centres
*  ==============
         IF ( FTYPE .EQ. 5 ) THEN

*  Read the next word from the last word, to find the number of values
*  to read.  Use a local status as we expect more words in the buffer
*  than elements of the WORDS array.
            CALL CHR_DCWRD( VALUE( END( MAXWRD )+1:BUFLEN-8 ), 1, NWORD,
     :                      START, END, WORDS, CSTAT )
            CALL CHR_CTOI( WORDS( 1 ), NVALUE, STATUS )
            CPOS = END( MAXWRD ) + END( 1 ) + 1

*  Map the centre array in the axis structure.
            CALL NDF_AMAP( NDF, 'Centre', 1, '_DOUBLE', 'WRITE',
     :                     PNTR, EL, STATUS )

*  Extract the array values and place them in the axis centres for the
*  first axis.
            CALL COI_FILAX( MIN( EL, NVALUE ), VALUE( CPOS:BUFLEN-8 ),
     :                      Z, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                      STATUS )

*  Unmap the axis array.
            CALL NDF_AUNMP( NDF, 'Centre', 1, STATUS )

*  Chebyshev or Legendre Polynomial
*  ================================
         ELSE IF ( FTYPE .EQ. 1 .OR. FTYPE .EQ. 2 ) THEN

*  Shift the text-search origin.
            CPOS = END( MAXWRD ) + 1

*  Read the next three words from the last word, to find the number of
*  coefficients, the scaling limits, and the first coefficient.  Use a
*  local status as we expect more words in the buffer than elements of
*  the WORDS array.
            CALL CHR_DCWRD( VALUE( CPOS:BUFLEN-8 ), 4, NWORD,
     :                      START, END, WORDS, CSTAT )
            CALL CHR_CTOI( WORDS( 1 ), ORDER, STATUS )

*  Determine the data type of the axis centres.  Find the number of
*  significant digits in the first coefficient.
            CALL KPG1_SGDIG( WORDS( 4 ), NSDIG, STATUS )

*  Determine the appropriate type by comparing the number of
*  significant digits present with the maximum number of significant
*  digits afforded by a real number.
            IF ( NSDIG .LE. -INT( LOG10( VAL__EPSR ) ) ) THEN
               ITYPE = '_REAL'
            ELSE
               ITYPE = '_DOUBLE'
            END IF

*  Process using the required precision.  First double precision.
*  --------------------------------------------------------------
            IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL CHR_CTOD( WORDS( 2 ), DPMIN, STATUS )
               CALL CHR_CTOD( WORDS( 3 ), DPMAX, STATUS )

*  Shift the origin.
               CPOS = CPOS + END( 3 )

*  Read the coefficients from the next words.  Use a local status as we
*  expect more words in the buffer than elements of the WORDS array.
               CALL CHR_DCWRD( VALUE( CPOS:BUFLEN-8 ), ORDER, NWORD,
     :                         START, END, WORDS, CSTAT )

*  Convert from strings to numeric values.
               DO I = 1, ORDER
                  CALL CHR_CTOD( WORDS( I ), DCOEF( I ), STATUS )
               END DO

*  Map the centre array in the axis structure.
               CALL NDF_AMAP( NDF, 'Centre', 1, '_DOUBLE', 'WRITE',
     :                        PNTR, EL, STATUS )

*  Evaluate the Chebyshev-polynomial at each axis centre.
               IF ( FTYPE .EQ. 1 ) THEN
                  CALL COI_CHEVD( ORDER, DCOEF, DPMIN, DPMAX, Z, EL,
     :                            %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                            STATUS )

               ELSE IF ( FTYPE .EQ. 2 ) THEN

*  Evaluate the Legendre polynomial at each axis centre.
                  CALL COI_LEGED( ORDER, DCOEF, DPMIN, DPMAX, Z, EL,
     :                            %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                            STATUS )
               END IF

*  Unmap the axis array.
               CALL NDF_AUNMP( NDF, 'Centre', 1, STATUS )

*  Process the single-precision axis.
*  ----------------------------------
            ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL CHR_CTOR( WORDS( 2 ), PMIN, STATUS )
               CALL CHR_CTOR( WORDS( 3 ), PMAX, STATUS )

*  Shift the origin.
               CPOS = CPOS + END( 3 )

*  Read the coefficients from the next words.  Use a local status as we
*  expect more words in the buffer than elements of the WORDS array.
               CALL CHR_DCWRD( VALUE( CPOS:BUFLEN-8 ), ORDER, NWORD,
     :                         START, END, WORDS, CSTAT )

*  Convert from strings to numeric values.
               DO I = 1, ORDER
                  CALL CHR_CTOR( WORDS( I ), COEF( I ), STATUS )
               END DO

*  Map the centre array in the axis structure.
               CALL NDF_AMAP( NDF, 'Centre', 1, '_REAL', 'WRITE',
     :                        PNTR, EL, STATUS )

*  Evaluate the Chebyshev-polynomial at each axis centre.
               IF ( FTYPE .EQ. 1 ) THEN
                  CALL COI_CHEVR( ORDER, COEF, PMIN, PMAX, REAL( Z ),
     :                            EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                            STATUS )

               ELSE IF ( FTYPE .EQ. 2 ) THEN

*  Evaluate the Legendre polynomial at each axis centre.
                  CALL COI_LEGER( ORDER, COEF, PMIN, PMAX, REAL( Z ),
     :                            EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                            STATUS )
               END IF

*  Unmap the axis array.
               CALL NDF_AUNMP( NDF, 'Centre', 1, STATUS )
            END IF

*  Linear or Cubic Spline
*  ======================
         ELSE IF ( FTYPE .EQ. 3 .OR. FTYPE .EQ. 4 ) THEN

*  Shift the text-search origin.
            CPOS = END( MAXWRD ) + 1

*  Read the next three words from the last word, to find the number of
*  pieces, the scaling limits, and the first coefficient.  Use a
*  local status as we expect more words in the buffer than elements of
*  the WORDS array.
            CALL CHR_DCWRD( VALUE( CPOS:BUFLEN-8 ), 4, NWORD,
     :                      START, END, WORDS, CSTAT )
            CALL CHR_CTOI( WORDS( 1 ), NPIECE, STATUS )

*  Find the number of coeffients.  This is the number of pieces plus
*  the order of the spline.
            IF ( FTYPE .EQ. 3 ) THEN
               ORDER = NPIECE + 3
            ELSE IF ( FTYPE .EQ. 4 ) THEN
               ORDER = NPIECE + 1
            END IF

*  Determine the data type of the axis centres.  Find the number of
*  significant digits in the first coefficient.
            CALL KPG1_SGDIG( WORDS( 4 ), NSDIG, STATUS )

*  Determine the appropriate type by comparing the number of
*  significant digits present with the maximum number of significant
*  digits afforded by a real number.
            IF ( NSDIG .LE. -INT( LOG10( VAL__EPSR ) ) ) THEN
               ITYPE = '_REAL'
            ELSE
               ITYPE = '_DOUBLE'
            END IF

*  Process using the required precision.  First double precision.
*  --------------------------------------------------------------
            IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL CHR_CTOD( WORDS( 2 ), DPMIN, STATUS )
               CALL CHR_CTOD( WORDS( 3 ), DPMAX, STATUS )

*  Shift the origin.
               CPOS = CPOS + END( 3 )

*  Read the coefficients from the next words.  Use a local status as we
*  expect more words in the buffer than elements of the WORDS array.
               CALL CHR_DCWRD( VALUE( CPOS:BUFLEN-8 ), ORDER, NWORD,
     :                         START, END, WORDS, CSTAT )

*  Convert from strings to numeric values.
               DO I = 1, ORDER
                  CALL CHR_CTOD( WORDS( I ), DCOEF( I ), STATUS )
               END DO

*  Map the centre array in the axis structure.
               CALL NDF_AMAP( NDF, 'Centre', 1, '_DOUBLE', 'WRITE',
     :                        PNTR, EL, STATUS )

*  Evaluate the cubic spline at each axis centre.
               IF ( FTYPE .EQ. 3 ) THEN
                  CALL COI_SP3ED( NPIECE, DCOEF, DPMIN, DPMAX, Z, EL,
     :                            %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                            STATUS )

               ELSE IF ( FTYPE .EQ. 4 ) THEN

*  Evaluate the linear spline at each axis centre.
                  CALL COI_SP1ED( NPIECE, DCOEF, DPMIN, DPMAX, Z, EL,
     :                            %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                            STATUS )
               END IF

*  Unmap the axis array.
               CALL NDF_AUNMP( NDF, 'Centre', 1, STATUS )

*  Process the single-precision axis.
*  ----------------------------------
            ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL CHR_CTOR( WORDS( 2 ), PMIN, STATUS )
               CALL CHR_CTOR( WORDS( 3 ), PMAX, STATUS )

*  Shift the origin.
               CPOS = CPOS + END( 3 )

*  Read the coefficients from the next words.  Use a local status as we
*  expect more words in the buffer than elements of the WORDS array.
               CALL CHR_DCWRD( VALUE( CPOS:BUFLEN-8 ), ORDER, NWORD,
     :                         START, END, WORDS, CSTAT )

*  Convert from strings to numeric values.
               DO I = 1, ORDER
                  CALL CHR_CTOR( WORDS( I ), COEF( I ), STATUS )
               END DO

*  Map the centre array in the axis structure.
               CALL NDF_AMAP( NDF, 'Centre', 1, '_REAL', 'WRITE',
     :                        PNTR, EL, STATUS )

*  Evaluate the cubic spline at each axis centre.
               IF ( FTYPE .EQ. 3 ) THEN
                  CALL COI_SP3ER( NPIECE, COEF, PMIN, PMAX, REAL( Z ),
     :                            EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                            STATUS )

               ELSE IF ( FTYPE .EQ. 4 ) THEN

*  Evaluate the linear spline at each axis centre.
                  CALL COI_SP1ER( NPIECE, COEF, PMIN, PMAX, REAL( Z ),
     :                            EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                            STATUS )
               END IF

*  Unmap the axis array.
               CALL NDF_AUNMP( NDF, 'Centre', 1, STATUS )
            END IF
         END IF
      END IF

  999 CONTINUE

      END
