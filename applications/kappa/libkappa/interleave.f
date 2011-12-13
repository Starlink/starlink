      SUBROUTINE INTERLEAVE ( STATUS )
*+
*  Name:
*     INTERLEAVE

*  Purpose:
*     Forms a higher-resolution NDF by interleaving a set of NDFs.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL INTERLEAVE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine performs interleaving, also known as interlacing,
*     in order to restore resolution where the pixel dimension
*     undersamples data.  Resolution may be improved by integer
*     factors along one or more dimensions.  For an N-fold increase in
*     resolution along a dimension, INTERLEAVE demands N NDF
*     structures that are displaced from each other by i/N pixels,
*     where i is an integer from 1 to N-1.  It creates an NDF whose
*     dimensions are enlarged by N along that dimension.
*
*     The supplied NDFs should have the same dimensionality.

*  Usage:
*     interleave in out expand

*  ADAM Parameters:
*     EXPAND() = _INTEGER (Read)
*        Linear expansion factors to be used to create the new data
*        array.  The number of factors should equal the number of
*        dimensions in the input NDF.  If fewer are supplied the last
*        value in the list of expansion factors is given to the
*        remaining dimensions.  Thus if a uniform expansion is required
*        in all dimensions, just one value need be entered.  If the net
*        expansion is one, an error results.  The suggested default is
*        the current value.
*     FILL = LITERAL (Read)
*        Specifies the value to use where the interleaving does not
*        fill the array, say because the shapes of the input NDFs are
*        not the same, or have additional shifts of origin.  Allowed
*        values are "Bad" or "Zero".  ["Bad"]
*     IN = NDF (Read)
*        A group of input NDFs to be interweaved.  They may have
*        different shapes, but must all have the same number of
*        dimensions.  This should be given as a comma-separated list,
*        in which each list element can be:
*
*        - an NDF name, optionally containing wild-cards and/or regular
*        expressions ("*", "?", "[a-z]" etc.).
*
*        - the name of a text file, preceded by an up-arrow character
*        "^".  Each line in the text file should contain a
*        comma-separated list of elements, each of which can in turn
*        be an NDF name (with optional wild-cards, etc.), or another
*        file specification (preceded by a caret).  Comments can be
*        included in the file by commencing lines with a hash
*        character "#".
*
*        If the value supplied for this parameter ends with a hyphen
*        "-", then you are re-prompted for further input until
*        a value is given which does not end with a hyphen.  All
*        the datasets given in this way are concatenated into a single
*        group.
*     OUT = NDF (Write)
*        Output NDF structure.
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the input NDF to the output NDF.  [!]
*     TRIM = _LOGICAL (Read)
*        This parameter controls the shape of the output NDF before the
*        application of the expansion.  If TRIM=TRUE, then the output
*        NDF reflects the shape of the intersection of all the input
*        NDFs, i.e. only pixels which appear in all the input arrays
*        will be represented in the output.  If TRIM=FALSE, the output
*        reflects shape of the union of the inputs, i.e. every pixel
*        which appears in the input arrays will be represented in the
*        output.  [TRUE]

*  Examples:
*     interleave "vector1,vector2" weave 2
*        This interleaves the 1one-dimensional NDFs called vector1 and
*        vector2 and stores the result in NDF weave.  Only the
*        intersection of the two input NDFs is used.
*     interleave 'image*' weave [3,2] title="Interlaced image"
*        This interleaves the two-dimensional NDFs with names beginning
*        with "image" into an NDF called weave.  The interleaving has
*        three datasets along the first dimension and two along the
*        second.  Therefore there should be six input NDFs.  The output
*        NDF has title "Interlaced image".
*     interleave in='image*' out=weave expand=[3,2] notrim
*        As above except the title is not set and the union of the
*        bounds of the input NDFs is expanded to form the shape of the
*        weave NDF.
*     interleave ^frames.lis finer 2
*        This interleaves the NDFs listed in the text file frames.lis
*        to form an enlarged NDF called finer.  The interleaving is
*        twofold along each axis of those NDFs.

*  Related Applications:
*     KAPPA: PIXDUPE; CCDPACK: DRIZZLE.

*  Implementation Status:
*     -  This routine processes the AXIS, DATA, QUALITY, and VARIANCE
*     from the all input NDF data structures.  It also processes the
*     WCS, LABEL, TITLE, UNITS, and HISTORY components of the primary
*     NDF data structure, and propagates all of its extensions.
*     -  The AXIS centre values along each axis are formed by
*     interleaving the corresponding centres from the first NDF, and
*     linearly interpolating between those to complete the array.
*     -  The AXIS width and variance values in the output are formed by
*     interleaving the corresponding input AXIS values.  Each array
*     element is assigned from the first applicable NDF.  For example,
*     for a two-dimensional array with expansion factors of 2 and 3
*     respectively, the first two NDFs would be used to define the
*     array elements for the first axis.  The second axis's elements
*     come from the first, third, and fifth NDFs.
*     -  All non-complex numeric data types can be handled.
*     -  Any number of NDF dimensions is supported.

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics & Astronomy Research
*     Council.
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     2005 August 11 (MJC):
*        Original version.
*     2005 August 25 (MJC):
*        Complete axis interleaving.
*     2005 August 17 & 25 (MJC):
*        Use only axis centres from the primary NDF and linearly
*        interpolate between these in the output NDF.  This is to
*        avoid non-monotonic axes.
*     2006 April 12 (MJC):
*        Remove unused variable.
*     2010 August 25 (MJC):
*        Used KPG_DIMLS instead of old DIMLST.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SAE definitions
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ACTVAL             ! Actual number of expansion factors
      INTEGER AENDF( NDF__MXDIM ) ! Axis expansion indices for current
                                 ! NDF
      INTEGER AEXPND( NDF__MXDIM ) ! Axis expansion factors
      INTEGER AFIRST( NDF__MXDIM )! Indices in output axis array of the
                                 ! first interleave-axis-array element
      INTEGER AFLBND( NDF__MXDIM ) ! Minimum indices in output axis
                                 ! array of the first interleave-array
                                 ! axis element
      CHARACTER * ( 8 ) ALIST( 3 ) ! Names of array components
      INTEGER AIDIMS( NDF__MXDIM ) ! Axis expansion dimensions of input
      INTEGER AODIMS( NDF__MXDIM ) ! Axis expansion dimensions of output
      LOGICAL AVAR               ! Axis variance is present?
      LOGICAL AXIS               ! Axis structure is present?
      INTEGER CNDF               ! Loop counter for current NDF
      CHARACTER * ( 80 ) DIMSTR  ! List of the output dimensions
      INTEGER EXPAND( NDF__MXDIM ) ! Expansion factors
      INTEGER EXPMAX( NDF__MXDIM ) ! Maximum expansion factors
      INTEGER EXPMIN( NDF__MXDIM ) ! Minimum expansion factors
      INTEGER EL                 ! Number of elements in mapped array
      INTEGER ELO                ! Number of elements in mapped output
                                 ! array
      CHARACTER FILL * ( 4 )     ! Initialize data values: bad or zero
      LOGICAL FILLAX             ! Fill axis array for the current
                                 ! NDF and axis?
      INTEGER FIRST( NDF__MXDIM )! Indices in output array of the
                                 ! first interleave-array element
      INTEGER FLBND( NDF__MXDIM )! Minimum indices in output array of
                                 ! the first interleave-array element
      INTEGER I                  ! Loop counter for the dimensions
      INTEGER IAXIS              ! Loop counter for the axis-array
                                 ! components
      INTEGER IDIMS( NDF__MXDIM )! Dimensions of input NDF
      INTEGER IGRP               ! GRP identifier for the group of
                                 ! input NDFs
      INTEGER IPNDF( NDF__MXDIM )! Pointer to input NDF identifiers
      CHARACTER ITYPE * ( NDF__SZTYP ) ! Numeric type for processing
      INTEGER J                  ! Loop counter for the dimensions
      INTEGER LARR               ! Loop counter for array components
      INTEGER LBNDI( NDF__MXDIM ) ! Lower bounds of input NDF
      INTEGER LBNDO( NDF__MXDIM ) ! Lower bounds of output NDF
      DOUBLE PRECISION MATRIX( NDF__MXDIM*NDF__MXDIM )! Matrix
                                 ! component of linear mapping
      INTEGER NARR               ! Number of array components
      INTEGER NCD                ! No. of characters in dimension list
      INTEGER NDFI               ! Identifier to the input NDF
      INTEGER NDFO               ! Identifier to the output NDF
      INTEGER NDFP               ! Identifier to the first input NDF
      INTEGER NDFS               ! Identifier to the section of the
                                 ! input NDF
      INTEGER NDIM               ! Dimensionality of the NDF
      INTEGER NNDF               ! Number of input NDFs
      INTEGER ODIMS( NDF__MXDIM )! Dimensions of output array
      DOUBLE PRECISION OFFSET( NDF__MXDIM ) ! Translation component of
                                 ! linear mapping
      INTEGER PNTRI( 1 )         ! Pointer to input array component(s)
      INTEGER PNTRO( 1 )         ! Pointer to output array component(s)
      LOGICAL QUAL               ! Quality is present?
      CHARACTER STRIM * ( 8 )    ! Trim parameter to pass to NDF_MBNDN
      INTEGER TOTEXP             ! Total expansion factor
      LOGICAL TRIM               ! Trim or pad arrays?
      INTEGER UBNDI( NDF__MXDIM )! Upper bounds of input NDF
      INTEGER UBNDO( NDF__MXDIM )! Upper bounds of output NDF
      LOGICAL VAR                ! Variance is present?
      CHARACTER * ( 6 ) WACCES   ! Access to mapped output array
      LOGICAL WIDTH              ! Axis width is present?

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get parameters.
*  ===============

*  Obtain the filling value.
      CALL PAR_CHOIC( 'FILL', 'Bad', 'Bad,Zero', .TRUE., FILL, STATUS )

*  See whether the output NDF will be the union or intersection of the
*  inputs.
      CALL PAR_GET0L( 'TRIM', TRIM, STATUS )
      IF ( TRIM ) THEN
         STRIM = 'TRIM'
      ELSE
         STRIM = 'PAD'
      END IF

*  Start an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDFs.
*  ======================
*
*  Get a group containing the names of the NDFs to be processed.
      NNDF = 0
      CALL KPG1_RGNDF( 'IN', 0, 1, '  Give more NDFs...', IGRP,
     :                 NNDF, STATUS )

*  Allocate some work space for the NDF identifiers.
      CALL PSX_CALLOC( NNDF, '_INTEGER', IPNDF, STATUS )

*  Check that the NDFs have the same dimensionality, and obtain their
*  NDF identifiers.  The NDFs of the identifiers have the same bounds.
      CALL KPS1_INMAT( IGRP, NNDF, STRIM, %VAL( CNF_PVAL( IPNDF ) ),
     :                 NDIM, STATUS )

*  Obtain the primary NDF's identifier.
      CALL KPG1_RETRI( NNDF, 1, %VAL( CNF_PVAL( IPNDF ) ), NDFP,
     :                 STATUS )

*  Obtain the dimensions of the primary NDF.
      CALL NDF_DIM( NDFP, NDF__MXDIM, IDIMS, NDIM, STATUS )

*  Obtain the expansion factors.
*  =============================
*
*  Set the acceptable range of values from no expansion to expansion
*  of a single element in a dimension.  Initialise values in case of an
*  error to prevent a possible divide-by-zero catastrophe.
      DO I = 1, NDIM
         EXPMIN( I ) = 1
         EXPMAX( I ) = VAL__MAXI / IDIMS( I )
         EXPAND( I ) = 1
      END DO

*  Get the expansion factors.
      CALL PAR_GRMVI( 'EXPAND', NDIM, EXPMIN, EXPMAX, EXPAND, ACTVAL,
     :                STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Should less values be entered than is required copy the last value
*  to higher dimensions.
      IF ( ACTVAL .LT. NDIM ) THEN
         DO I = ACTVAL + 1, NDIM
            EXPAND( I ) = EXPAND( ACTVAL )
         END DO
      END IF

*  Pad out the expansion factors and dimensions to the maximum number
*  of dimensions.  The subroutine uses them all to avoid a complex
*  piece of coding.
      IF ( NDIM .LT. NDF__MXDIM ) THEN
         DO I = NDIM + 1, NDF__MXDIM
            EXPAND( I ) = 1
            FIRST( I ) = 1
            FLBND( I ) = 1
            IDIMS( I ) = 1
            ODIMS( I ) = 1
         END DO
      END IF

* Set the lower bound of the first pixel indices in the output array.
      DO I = 1, NDIM
         FLBND( I ) = 1
      END DO

*  Check there is going to be a expansion.
*  =======================================

*  Find total expansion.
      TOTEXP = 1
      DO I = 1, NDIM
         TOTEXP = TOTEXP * EXPAND( I )
      END DO

*  Report and abort if there is no expansion.
      IF ( TOTEXP .EQ. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ERR_INTERLEAVE_NOEXPR',
     :     'INTERLEAVE: There is no expansion to be made.', STATUS )
         GOTO 999
      END IF

*  Compute the output NDF's dimensions.
*  ====================================

*  Obtain the dimensions of the primary NDF.
      CALL NDF_BOUND( NDFP, NDF__MXDIM, LBNDI, UBNDI, NDIM, STATUS )

*  Work out the size of the output array from the input array
*  dimensions and the expansion factor.  Also derive bounds for the
*  output array.  These retain origin information by imagining the
*  expansion takes place about the origin of pixel coordinates.
      DO I = 1, NDIM
         ODIMS( I ) = IDIMS( I ) * EXPAND( I )
         LBNDO( I ) = ( LBNDI( I ) - 1 ) * EXPAND( I ) + 1
         UBNDO( I ) = LBNDO( I ) + ODIMS( I ) - 1
      END DO

*  Report the new dimensions.
      CALL KPG_DIMLS( NDIM, ODIMS, NCD, DIMSTR, STATUS )
      CALL MSG_OUTIF( MSG__VERB, 'OUTPUT_DIMS', ' The output NDF will '/
     :  /'have ' // DIMSTR( :NCD ) // ' pixels.', STATUS )

*  Create the output NDF.
*  ======================
*
*  Take a shortcut to propagate ancillary data from the input NDF.
*  Create a section from the input NDF of the size of the required NDF.
      CALL NDF_SECT( NDFP, NDIM, LBNDO, UBNDO, NDFS, STATUS )

*  Create the output NDF based on the sub-section.  The array
*  components and axes will be processed individually, but this enables
*  the LABEL, HISTORY, AXIS character components, and extensions to be
*  propagated.  The axis arrays will be changed later.
      CALL LPG_PROP( NDFS, 'Axis,Units', 'OUT', NDFO, STATUS )
      CALL NDF_ANNUL( NDFS, STATUS )

*  Obtain a title and assign it to the output NDF.  A null results in
*  the output title being the same as the input title.
      CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Test for VARIANCE & QUALITY.
*  ============================

*  Determine if variance or quality are present in the primary input
*  NDF.
      CALL NDF_STATE( NDFP, 'Variance', VAR, STATUS )
      CALL NDF_STATE( NDFP, 'Quality', QUAL, STATUS )

*  See whether or not there is an axis system.
      CALL NDF_STATE( NDFP, 'Axis', AXIS, STATUS )

*  Interleave each input NDF.
*  ==========================
      DO CNDF = 1, NNDF

*  Obtain the identifier for the current NDF.
         CALL KPG1_RETRI( NNDF, CNDF, %VAL( CNF_PVAL( IPNDF ) ), NDFI,
     :                    STATUS )

*  Form list of array components.
*  ==============================
         NARR = 1
         ALIST( NARR ) = 'Data'

*  Test for VARIANCE & QUALITY.
         CALL NDF_STATE( NDFI, 'Variance', VAR, STATUS )
         CALL NDF_STATE( NDFI, 'Quality', QUAL, STATUS )

*  Append to list of array components as needed.
         IF ( VAR ) THEN
            NARR = NARR + 1
            ALIST( NARR ) = 'Variance'
         END IF

         IF ( QUAL ) THEN
            NARR = NARR + 1
            ALIST( NARR ) = 'Quality'
         END IF

*  Values and quality are merely duplicated, so there is no need to test
*  for bad values.  Hence we can switch off automatic quality masking
*  too.
         CALL NDF_SQMF( .FALSE., NDFI, STATUS )

*  As a memory-saving measure, only map the potentially large output
*  arrays as needed.  For the second and subsequent arrays, the
*  partially interleaved should only be updated.
         IF ( CNDF .EQ. 1 ) THEN
            WACCES = 'WRITE'
         ELSE
            WACCES = 'UPDATE'
         END IF

*  Determine the axis `origins' for the current NDF.
*  =================================================

*  The first pixel for the first array starts at index 1 in the output
*  array.  The origin is shifted by one along the first axis up to
*  EXPAND( 1 ).  Then this is reset to one and the index along the
*  second dimension is incremented by one up to EXPAND( 2 ), as so on.
*  Thus the increments are in Fortran order, with the lowest dimension
*  incrementing fastest as we process each input NDF.
         CALL KPG1_VEC2N( 1, CNDF, NDIM, FLBND, EXPAND, FIRST, STATUS )

*  Interleave each input array separately.
*  =======================================

*  Normally it would be more efficient to map the variance at the same
*  time as the data array as quality masking need only be applied once.
*  However, masking has been switched off, and in this case a lack of
*  memory is more important, as the output arrays could be large, so we
*  process each of the array components in turn.
         DO LARR = 1, NARR

*  Find the data type of the array in the primary NDF.
            CALL NDF_TYPE( NDFP, ALIST( LARR ), ITYPE, STATUS )

*  Map the full input and output arrays.
            CALL KPG1_MAP( NDFI, ALIST( NARR ), ITYPE, 'READ', PNTRI,
     :                     EL, STATUS )
            CALL KPG1_MAP( NDFO, ALIST( NARR ), ITYPE, WACCES, PNTRO,
     :                     ELO, STATUS )

*  Interleave the array, using the routine approriate for the data
*  type.
            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPS1_INLER( EXPAND, FIRST, IDIMS, EL,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ), ODIMS,
     :                          ELO, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                          STATUS )

            ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
               CALL KPS1_INLEB( EXPAND, FIRST, IDIMS, EL,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ), ODIMS,
     :                          ELO, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                          STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPS1_INLED( EXPAND, FIRST, IDIMS, EL,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ), ODIMS,
     :                          ELO, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                          STATUS )

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
               CALL KPS1_INLEI( EXPAND, FIRST, IDIMS, EL,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ), ODIMS,
     :                          ELO, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                          STATUS )

            ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
               CALL KPS1_INLEUB( EXPAND, FIRST, IDIMS, EL,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ), ODIMS,
     :                          ELO, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                          STATUS )

            ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
               CALL KPS1_INLEUW( EXPAND, FIRST, IDIMS, EL,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ), ODIMS,
     :                          ELO, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                          STATUS )

            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
               CALL KPS1_INLEW( EXPAND, FIRST, IDIMS, EL,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ), ODIMS,
     :                          ELO, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                          STATUS )
            END IF

*  Tidy the data arrays.
            CALL NDF_UNMAP( NDFI, ALIST( LARR ), STATUS )
            CALL NDF_UNMAP( NDFO, ALIST( LARR ), STATUS )
         END DO

*  Expand the axis-centres array.
*  ==============================
         IF ( AXIS ) THEN

*  Form list of array components.
*  ==============================
            NARR = 1
            ALIST( NARR ) = 'Centre'

*  Test for Axis VARIANCE and WIDTH.
            CALL NDF_ASTAT( NDFI, 'Variance', 1, AVAR, STATUS )
            CALL NDF_ASTAT( NDFI, 'Width', 1, WIDTH, STATUS )

*  Append to list of array components as needed.
            IF ( AVAR ) THEN
               NARR = NARR + 1
               ALIST( NARR ) = 'Variance'
            END IF

            IF ( WIDTH ) THEN
               NARR = NARR + 1
               ALIST( NARR ) = 'Width'
            END IF

*  Set the expansion factors.  Pad out the expansion factors to the
*  maximum number of dimensions.  Also set the dummy input and output
*  dimensions.
            DO I = 2, NDF__MXDIM
               AEXPND( I ) = 1
               AFLBND( I ) = 1
               AFIRST( I ) = 1
               AIDIMS( I ) = 1
               AODIMS( I ) = 1
               AENDF( I ) = 1
            END DO

*  Process each dimension separately.
            DO IAXIS = 1, NDIM

*  Set the axis expansion factor, and the dimensions.
               AEXPND( 1 ) = EXPAND( IAXIS )
               AFLBND( 1 ) = 1
               AIDIMS( 1 ) = IDIMS( IAXIS )
               AODIMS( 1 ) = ODIMS( IAXIS )
               AFIRST( 1 ) = FIRST( IAXIS )

*  Determine the axis-array `origin' for the current NDF.
*  ======================================================

*  We want to determine the n-dimensional array indices within a box
*  with dimensions of the expansion factors, and corresponding
*  to the current NDF.  Recall that the interleaving is in Fortran
*  order.
               CALL KPG1_VEC2N( 1, CNDF, NDIM, AFLBND,
     :                          AEXPND, AENDF, STATUS )

*  The axis arrays are filled by the first NDF that applies.
*  The first NDF always fills the axis arrays along each axis.
*  For others, the axis array is only filled when its indices are
*  all one except along the current (IAXIS) axis.
               FILLAX = CNDF .EQ. 1
               IF ( .NOT. FILLAX ) THEN
                  FILLAX = .TRUE.
                  DO J = 1, NDIM
                     IF ( J .NE. IAXIS ) THEN
                        FILLAX = FILLAX .AND. AENDF( J ) .EQ. 1
                     END IF
                  END DO
               END IF

*  Fill the axis where needed.
               IF ( FILLAX ) THEN
                  DO LARR = 1, NARR

*  Find the data type of the array in the primary NDF.
                     CALL NDF_ATYPE( NDFP, ALIST( LARR ), IAXIS, ITYPE,
     :                               STATUS )

*  Map the full input, and output variance arrays.
                     CALL NDF_AMAP( NDFI, ALIST( LARR ), IAXIS, ITYPE,
     :                              'READ', PNTRI, EL, STATUS )
                     CALL NDF_AMAP( NDFO, ALIST( LARR ), IAXIS, ITYPE,
     :                              WACCES, PNTRO, ELO, STATUS )

*  Interleave the first, i.e. of the primary input NDF, axis-centre
*  array and interpolate between the values, calling the appropriate
*  routine for the data type.
                     IF ( ALIST( LARR ) .EQ. 'Centre' .AND.
     :                    CNDF .EQ. 1 ) THEN
                        IF ( ITYPE .EQ. '_REAL' ) THEN
                           CALL KPS1_INLIR( AEXPND( 1 ), AFIRST( 1 ),
     :                              EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              ELO, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                              STATUS )

                        ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
                           CALL KPS1_INLIB( AEXPND( 1 ), AFIRST( 1 ),
     :                              EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              ELO, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                              STATUS )

                        ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                           CALL KPS1_INLID( AEXPND( 1 ), AFIRST( 1 ),
     :                              EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              ELO, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                              STATUS )

                        ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                           CALL KPS1_INLII( AEXPND( 1 ), AFIRST( 1 ),
     :                              EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              ELO, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                              STATUS )

                        ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
                           CALL KPS1_INLIUB( AEXPND( 1 ), AFIRST( 1 ),
     :                              EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              ELO, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                              STATUS )

                        ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
                           CALL KPS1_INLIUW( AEXPND( 1 ), AFIRST( 1 ),
     :                              EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              ELO, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                              STATUS )

                        ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                           CALL KPS1_INLIW( AEXPND( 1 ), AFIRST( 1 ),
     :                              EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              ELO, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                              STATUS )
                        END IF

* Just interleave the axis variances, although there is something to
* be said for interpolating like the centres, and the widths.
                     ELSE IF ( ALIST( LARR ) .EQ. 'Variance' .OR.
     :                         ALIST( LARR ) .EQ. 'Width' ) THEN
                        IF ( ITYPE .EQ. '_REAL' ) THEN
                           CALL KPS1_INLER( AEXPND, AFIRST, AIDIMS, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   AODIMS, ELO,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                        ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
                           CALL KPS1_INLEB( AEXPND, AFIRST, AIDIMS, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   AODIMS, ELO,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                        ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                           CALL KPS1_INLED( AEXPND, AFIRST, AIDIMS, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   AODIMS, ELO,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                        ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                           CALL KPS1_INLEI( AEXPND, AFIRST, AIDIMS, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   AODIMS, ELO,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                        ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
                           CALL KPS1_INLEUB( AEXPND, AFIRST, AIDIMS, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   AODIMS, ELO,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                        ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
                           CALL KPS1_INLEUW( AEXPND, AFIRST, AIDIMS, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   AODIMS, ELO,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )

                        ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                           CALL KPS1_INLEW( AEXPND, AFIRST, AIDIMS, EL,
     :                                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                                   AODIMS, ELO,
     :                                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                                   STATUS )
                        END IF
                     END IF

*  Tidy the axis arrays.
                     CALL NDF_AUNMP( NDFI, ALIST( LARR ), IAXIS,
     :                               STATUS )
                     CALL NDF_AUNMP( NDFO, ALIST( LARR ), IAXIS,
     :                               STATUS )
                  END DO
               END IF
            END DO
         END IF
      END DO

*  Propagate the WCS component, incorporating a linear mapping between
*  pixel co-ordinates. This mapping is described by a matrix and an
*  offset vector.  Set these up.
      DO I = 1, NDIM * NDIM
         MATRIX( I ) = 0.0
      END DO

      DO I = 1, NDIM
         OFFSET( I ) = DBLE( LBNDO( I ) - 1 ) - EXPAND( I ) *
     :                 DBLE( LBNDI( I ) - 1 )
         MATRIX( NDIM * ( I - 1 ) + I ) = DBLE( EXPAND( I ) )
      END DO

*  Propagate the WCS component from the primary input NDF.
      CALL KPG1_ASPRP( NDIM, NDFP, NDFO, MATRIX, OFFSET, STATUS )

*  Come here if something has gone wrong.
  999 CONTINUE

*  Release GRP resources.
      CALL GRP_DELET( IGRP, STATUS )

*  Tidy the NDF system.
      CALL NDF_END( STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'INTERLEAVE_ERR',
     :     'INTERLEAVE: Unable to interleave NDFs.', STATUS )
      END IF

      END
