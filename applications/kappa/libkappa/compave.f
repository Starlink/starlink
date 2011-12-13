      SUBROUTINE COMPAVE( STATUS )
*+
*  Name:
*     COMPAVE

*  Purpose:
*     Reduces the size of an NDF by averaging values in rectangular
*     boxes.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL COMPAVE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application takes an NDF data structure and reduces it in
*     size by integer factors along each dimension.  The compression
*     is achieved by averaging the input NDF within non-overlapping
*     `rectangular' boxes whose dimensions are the compression factors.
*     The averages may be weighted when there is a variance array
*     present. The exact placement of the boxes can be controlled using
*     parameter ALIGN.

*  Usage:
*     compave in out compress [wlim]

*  ADAM Parameters:
*     ALIGN = LITERAL (Read)
*        This parameter controls the placement of the compression boxes
*        within the input NDF (also see parameter TRIM).  It can take
*        any of the following values:
*
*        - "ORIGIN" --- The compression boxes are placed so that the
*        origin of the pixel co-ordinate Frame (i.e. pixel co-ordinates
*        (0,0)) in  the input NDF corresponds to a corner of a
*        compression box.  This results in the pixel origin being
*        retained in the output NDF.  For instance, if a pair of
*        two-dimensional images which have previously been aligned in
*        pixel co-ordinates are compressed, then using this option
*        ensures that the compressed images will also be aligned in
*        pixel co-ordinates.
*
*        - "FIRST" --- The compression boxes are placed so that the
*        first pixel in the input NDF (for instance, the bottom-left
*        pixel in a two-dimensional image) corresponds to the first
*        pixel in a compression box.  This can result in the pixel
*        origin being shifted by up to one compression box in the output
*        image.  Thus, images which were previously aligned in pixel
*        co-ordinates may not be aligned after compression.  You may
*        want to use this option if you are using a very large box to
*        reduce the number of dimensions in the data (for instance
*        averaging across the entire width of an image to produce a
*        one-dimensional array).
*
*        - "LAST" --- The compression boxes are placed so that the
*        last pixel in the input NDF (for instance, the top-right
*        pixel in a two-dimensional image) corresponds to the last pixel
*        in a compression box.  See the "FIRST" option above for further
*        comments.
*                                                             ["ORIGIN"]
*     AXWEIGHT = _LOGICAL (Read)
*        When there is an AXIS variance array present in the NDF and
*        AXWEIGHT=TRUE the application forms weighted averages of the
*        axis centres using the variance.  For all other conditions
*        the non-bad axis centres are given equal weight during the
*        averaging to form the output axis centres. [FALSE]
*     COMPRESS( ) = _INTEGER (Read)
*        Linear compression factors to be used to create the output
*        NDF.  There should be one for each dimension of the NDF.  If
*        fewer are supplied the last value in the list of compression
*        factors is given to the remaining dimensions.  Thus if a
*        uniform compression is required in all dimensions, just one
*        value need be entered.  The suggested default is the current
*        value.
*     IN  = NDF (Read)
*        The NDF structure to be reduced in size.
*     OUT = NDF (Write)
*        NDF structure to contain compressed version of the input NDF.
*     PRESERVE = _LOGICAL (Read)
*        If the input data type is to be preserved on output then this
*        parameter should be set true.   However, this will probably
*        result in a loss of precision.  If this parameter is set false
*        then the output data type will be one of _REAL or _DOUBLE,
*        depending on the input type. [FALSE]
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the input NDF to the output NDF. [!]
*     TRIM = _LOGICAL (Read)
*        If parameter TRIM is set TRUE, the output NDF only contains
*        data for compression boxes which are entirely contained within
*        the input NDF.  Any pixels around the edge of the input NDF
*        that are not contained within a compression box are ignored.
*        If TRIM is set FALSE, the output NDF contains data for all
*        compression boxes which have any overlap with the input NDF.
*        All pixels outside the bounds of the NDF are assumed to be bad.
*        That is, any boxes which extend beyond the bounds of the input
*        NDF are padded with bad pixels.  See also parameter ALIGN.
*        [current value]
*     WEIGHT = _LOGICAL (Read)
*        When there is a variance array present in the NDF and
*        WEIGHT=TRUE the application forms weighted averages of the
*        data array using the variance.  For all other conditions
*        the non-bad pixels are given equal weight during averaging.
*        [FALSE]
*     WLIM = _REAL (Read)
*        If the input NDF contains bad pixels, then this parameter
*        may be used to determine the number of good pixels which must
*        be present within the averaging box before a valid output
*        pixel is generated.  It can be used, for example, to prevent
*        output pixels from being generated in regions where there are
*        relatively few good pixels to contribute to the smoothed
*        result.
*
*        WLIM specifies the minimum fraction of good pixels which must
*        be present in the averaging box in order to generate a good
*        output pixel.  If this specified minimum fraction of good
*        input pixels is not present, then a bad output pixel will
*        result, otherwise an averaged output value will be calculated.
*        The value of this parameter should lie between 0.0 and 1.0
*        (the actual number used will be rounded up if necessary to
*        correspond to at least 1 pixel). [0.3]

*  Examples:
*     compave cosmos galaxy 4
*        This compresses the NDF called cosmos averaging four times in
*        each dimension, and stores the reduced data in the NDF called
*        galaxy.  Thus if cosmos is two-dimensional, this command
*        would result in a sixteen-fold reduction in the array
*        components.
*     compave cosmos profile [10000,1] wlim=0 align=first trim=no
*        This compresses the two-dimensional NDF called cosmos to
*        produce a one-dimensional NDF called profile.  This is done
*        using a compression box which is one pixel high, but which is
*        wider than the whole input image.  Each pixel in the output NDF
*        thus corresponds to the average of the corresponding row in the
*        input image. WLIM is set to zero to ensure that bad pixels
*        are ignored.  ALIGN is set to FIRST so that each compression
*        box is flush with the left edge of the input image.  TRIM is
*        set to NO so that compression boxes which extend outside the
*        bounds of the input image (which will be all of them if the
*        input image is narrower than 10000 pixels) are retained in the
*        output NDF.
*     compave cosmos galaxy 4 wlim=1.0
*        This compresses the NDF called cosmos averaging four times in
*        each dimension, and stores the reduced data in the NDF called
*        galaxy.  Thus if cosmos is two-dimensional, this command
*        would result in a sixteen-fold reduction in the array
*        components.  If an averaging box contains any bad pixels, the
*        output pixel is set to bad.
*     compave cosmos galaxy 4 0.0 preserve
*        As above except that an averaging box need only contains a
*        single non-bad pixels for the output pixel to be good, and
*        galaxy's array components will have the same as those in
*        cosmos.
*     compave cosmos galaxy [4,3] weight title="COSMOS compressed"
*        This compresses the NDF called cosmos averaging four times in
*        the first dimension and three times in higher dimensions, and
*        stores the reduced data in the NDF called galaxy.  Thus if
*        cosmos is two-dimensional, this command would result in a
*        twelve-fold reduction in the array components.  Also, if there
*        is a variance array present it is used to form weighted means
*        of the data array.   The title of the output NDF is "COSMOS
*        compressed".
*     compave in=arp244 compress=[1,1,3] out=arp244cs
*        Suppose arp244 is a huge NDF storing a spectral-line data
*        cube, with the third dimension being the spectral axis.
*        This command compresses arp244 in the spectral dimension,
*        averaging every three pixels to form the NDF called arp244cs.

*  Notes:
*     -  The axis centres and variances are averaged, whilst the widths
*     are summed and always normalised for bad values.

*  Related Applications:
*     KAPPA: BLOCK, COMPADD, COMPICK, PIXDUPE, SQORST, RESAMPLE;
*     Figaro: ISTRETCH.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, VARIANCE,
*     LABEL, TITLE, UNITS, WCS, and HISTORY components of the input NDF
*     and propagates all extensions.  QUALITY is not processed since it
*     is a series of flags, not numerical values.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  Any number of NDF dimensions is supported.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998-2000, 2004 Central Laboratory of the
*     Research Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street,,Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1991 November 27 (MJC):
*        Original version.
*     1995 January 11 (MJC):
*        Made TITLE propagate from the input NDF.  Used PSX for
*        workspace.
*     27-FEB-1998 (DSB):
*        Type of local variable AXWT corrected from INTEGER to LOGICAL.
*     10-JUN-1998 (DSB):
*        Propagate WCS component.  Ensure each output dimension is at
*        least one pixel long.
*     12-OCT-1998 (DSB):
*        Changed the way in which the bounds of the output image are
*        determined so that pixel origin information is retained.
*     1998 October 23 (MJC):
*        Fixed bug for 1-dimensional data with axes.  It now uses the
*        the actual dimensionality as opposed to 2, as needed by the
*        resampling tasks.
*     9-DEC-1999 (DSB):
*        Corrected propagation of WCS (an erroneous shift was previously
*        introduced if the lower bounds of the input NDF were not
*        (1,1)).
*     25-APR-2000 (DSB):
*        Added parameters TRIM and ALIGN.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No default typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'       ! Global SSE definitions
      INCLUDE  'PAR_ERR'       ! Parameter-system errors
      INCLUDE  'NDF_PAR'       ! NDF_ public constants
      INCLUDE  'PRM_PAR'       ! Primdat public constants
      INCLUDE  'CNF_PAR'       ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER KPG1_FLOOR       ! Most positive integer .LE. a given real
      INTEGER KPG1_CEIL        ! Most negative integer .GE. a given real

*  Local Variables:
      CHARACTER
     :  ALIGN * ( 6 ),         ! Alignment option
     :  COMP * ( 15 ),         ! List of components to process
     :  CTYPE * ( NDF__SZTYP ),! Numeric type for counting workspace
     :  DTYPE * ( NDF__SZFTP ),! Numeric type for output arrays
     :  ITYPE * ( NDF__SZTYP ),! Numeric type for processing
     :  TYPE * ( NDF__SZTYP )  ! Data type of an array component

      DOUBLE PRECISION
     :  MATRIX( NDF__MXDIM*NDF__MXDIM ),! Matrix component of linear
                               ! mapping
     :  OFFSET( NDF__MXDIM )   ! Translation component of linear mapping

      INTEGER
     :  ACTVAL,                ! Actual number of compression factors
     :  ACOMPR( 2 ),           ! 1-d compression for axis (2nd factor
                               ! is dummy for subroutines)
     :  ADIMS( 2 ),            ! Axis dimension (2nd dimension is dummy
                               ! for subroutines).
     :  CMPMAX( NDF__MXDIM ),  ! Maximum compression factors
     :  CMPMIN( NDF__MXDIM ),  ! Minimum compression factors
     :  COMPRS( NDF__MXDIM ),  ! Compression factors
     :  D,                     ! No. o/p pixels from ref. to i/p pixel 1
     :  EL,                    ! Number of elements in mapped array
     :  ELA,                   ! Number of elements in input axis array
     :  ELWS1,                 ! Number of elements in summation
                               ! workspace
     :  ELWS2,                 ! Number of elements in counting
                               ! workspace
     :  I, J,                  ! Loop counter for the dimensions
     :  IAXIS,                 ! Loop counter for the axis-array
                               ! components
     :  IDIMS( NDF__MXDIM ),   ! Dimensions of input NDF
     :  LBND( NDF__MXDIM ),    ! Lower bounds of input NDF
     :  LBNDO( NDF__MXDIM )    ! Lower bounds of output NDF

      INTEGER
     :  NDFI,                  ! Identifier to the whole input NDF
     :  NDFIS,                 ! Identifier to used section of input NDF
     :  NDFO,                  ! Identifier to the output NDF
     :  NDFS,                  ! Identifier to the section of input NDF
     :  NDIM,                  ! Dimensionality of the NDF
     :  NDIMI,                 ! Actual dimensionality of the NDF
     :  NLIM,                  ! Minimum number of elements in input
                               ! averaging box to form good output value
     :  ODIMS( NDF__MXDIM ),   ! Dimensions of output array
     :  PNTRI( 2 ),            ! Pointer to input array component(s)
     :  PNTRO( 2 ),            ! Pointer to output array component(s)
     :  REF( NDF__MXDIM ),     ! I/p pixel co-ords at bottom left of a
                               ! comp. box
     :  TOTCMP,                ! Total compression factor
     :  UBND( NDF__MXDIM ),    ! Upper bounds of input NDF
     :  UBNDO( NDF__MXDIM ),   ! Upper bounds of output NDF
     :  WPNTR1,                ! Pointer to summation workspace
     :  WPNTR2                 ! Pointer to counting workspace

      LOGICAL                  ! True if:
     :  AVAR,                  ! Axis variance is present
     :  AXIS,                  ! Axis structure is present
     :  AXWT,                  ! Axis weighted averages
     :  PRESTY,                ! Preserve the input array's data type in
                               ! the output arrays
     :  TRIM,                  ! Trim i/p image to a whole number of
                               ! boxes?
     :  VAR,                   ! Variance is present
     :  WEIGHT,                ! Weighting is required if variance is
                               ! present
     :  WIDTH                  ! Axis width is present

      REAL
     :  WLIM                   ! Fraction of good pixels required

*.

*  Check the global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Inquire the bounds and dimensions of the NDF.
      CALL NDF_BOUND( NDFI, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      CALL NDF_DIM( NDFI, NDF__MXDIM, IDIMS, NDIM, STATUS )

*  The subroutines require that there must be at least two dimensions
*  in the arrays, but higher ones may be dummies.
      NDIMI = NDIM
      NDIM = MAX( NDIM, 2 )

*  Obtain the compression factors.
*  ===============================
*
*  Set the acceptable range of values. Initialise values in case of an
*  error to prevent a possible divide-by-zero catastrophe.
      DO I = 1, NDIM
         CMPMIN( I ) = 1
         CMPMAX( I ) = VAL__MAXI
         COMPRS( I ) = 1
      END DO

*  Get the compression factors.
      CALL PAR_GRMVI( 'COMPRESS', NDIM, CMPMIN, CMPMAX, COMPRS, ACTVAL,
     :                STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Should less values be entered than is required copy the last value to
*  higher dimensions.
      IF ( ACTVAL .LT. NDIM ) THEN
         DO I = ACTVAL + 1, NDIM
            COMPRS( I ) = COMPRS( ACTVAL )
         END DO
      END IF

*  Limit all values to be no greater than the corresponding dimension.
      DO I = 1, NDIM
         COMPRS( I ) = MIN( IDIMS( I ), COMPRS( I ) )
      END DO

*  Check there is going to be a compression.
*  =========================================

*  Find total compression.
      TOTCMP = 1
      DO I = 1, NDIM
         TOTCMP = TOTCMP * COMPRS( I )
      END DO

*  Report and abort if there is no compression.
      IF ( TOTCMP .EQ. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ERR_COMPAVE_NOCMPR',
     :     'COMPAVE: There is no compression to be made.', STATUS )
         GOTO 999
      END IF

*  Obtain the minimum fraction of good pixels which should be used to
*  calculate an output pixel value.
      CALL PAR_GDR0R( 'WLIM', 0.3, 0.0, 1.0, .FALSE., WLIM, STATUS )

*  Derive the minimum number of pixels, using at least one.
      NLIM = MAX( 1, NINT( REAL( TOTCMP ) * WLIM ) )

*  Determine if a variance component is present and derive a list of
*  the components to be processed together.  The variance component
*  cannot be processed alone, since it needs to know about bad pixels
*  in the data array, so only the variances of good data pixels are
*  used to find a resultant variance.  The bad-pixel flag could be
*  tested, but since we have to call the routine when there may be bad
*  pixels that would only serve to lengthen this routine.
      CALL NDF_STATE( NDFI, 'Variance', VAR, STATUS )
      IF ( VAR ) THEN
         COMP = 'Data,Variance'

*  Obtain the the flag to decide whether or not the variance is used
*  to weight the average.
         CALL PAR_GTD0L( 'WEIGHT', .FALSE., .FALSE., WEIGHT, STATUS )
      ELSE
         COMP = 'Data'
      END IF

*  Obtain the the flag to decide whether or not to preserve the input
*  type though this may lose precision.
      CALL PAR_GTD0L( 'PRESERVE', .FALSE., .FALSE., PRESTY, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Compute the output NDF's dimensions.
*  ====================================

*  See how the placement of the compression boxes is to be chosen.
*  "ORIGIN" results in boxes being placed so that the origin of pixel
*  co-ordinates in the input NDF is co-incident with the bottom-left
*  corner of a compression box.  "First" results in the first pixel
*  (i.e. the bottom left pixel in a two-dimensional array) being the
*  first pixel in the first compression box.  "Last" results in the last
*  pixel (i.e. the top-right pixel in a two-dimensional array) being the
*  last pixel in the last compression box.
      CALL PAR_CHOIC( 'ALIGN', 'Origin', 'Origin,First,Last', .TRUE.,
     :                ALIGN, STATUS )

*  Store the input pixel co-ordinates of the bottom left corner of a
*  compression box.
      IF( ALIGN .EQ. 'ORIGIN' ) THEN
         DO I = 1, NDIM
            REF( I ) = 0
         END DO

      ELSE IF( ALIGN .EQ. 'FIRST' ) THEN
         DO I = 1, NDIM
            REF( I ) = LBND( I ) - 1
         END DO

      ELSE IF( ALIGN .EQ. 'LAST' ) THEN
         DO I = 1, NDIM
            REF( I ) = UBND( I )
         END DO

      END IF

*  See if the input image is to padded or trimmed in order to make it a
*  whole number of compression boxes.
      CALL PAR_GET0L( 'TRIM', TRIM, STATUS )

*  Work out the bounds for the output array and the size of the output
*  array from the input array dimensions, compression factor and
*  alignment.  Also modify the input bounds so that they correspond to
*  the section of the input image which is actually used.  Trim or pad
*  the input image to make it a whole number of compression boxes, as
*  required by parameter TRIM.
      DO I = 1, NDIM
         D = KPG1_CEIL( REAL( 1 - REF( I ) )/REAL( COMPRS( I ) ) ) - 1

         IF( TRIM ) THEN
            LBNDO( I ) = KPG1_CEIL( REAL( LBND( I ) - 1 - REF( I ) )
     :                              / REAL( COMPRS( I ) ) ) - D + 1
            UBNDO( I ) = MAX( LBNDO( I ),
     :                        KPG1_FLOOR( REAL( UBND( I ) - REF( I ) )
     :                                    / REAL( COMPRS( I ) ) ) - D )
         ELSE
            LBNDO( I ) = KPG1_FLOOR( REAL( LBND( I ) - 1 - REF( I ) )
     :                              / REAL( COMPRS( I ) ) ) - D + 1
            UBNDO( I ) = MAX( LBNDO( I ),
     :                        KPG1_CEIL( REAL( UBND( I ) - REF( I ) )
     :                                    / REAL( COMPRS( I ) ) ) - D )
         END IF

         ODIMS( I ) = UBNDO( I ) - LBNDO( I ) + 1

         LBND( I ) = 1 + REF( I ) + COMPRS( I )*( LBNDO( I ) - 1 + D )
         UBND( I ) = REF( I ) + COMPRS( I )*( UBNDO( I ) + D )
         IDIMS( I ) = UBND( I ) - LBND( I ) + 1

      END DO

*  Create a section of the input NDF containing the region will actually
*  be used (i.e. excluding any pixels which lie over the edge of the
*  output image).
      CALL NDF_SECT( NDFI, NDIMI, LBND, UBND, NDFIS, STATUS )

*  Annul the original input NDF and use the section created above
*  instead.
      CALL NDF_ANNUL( NDFI, STATUS )
      NDFI = NDFIS

*  Create the output NDF.
*  ======================
*
*  Take a shortcut to propagate ancillary data from the input NDF.
*  Create a section from the input NDF of the size of the required NDF.
      CALL NDF_SECT( NDFI, NDIMI, LBNDO, UBNDO, NDFS, STATUS )

*  Create the output NDF based on the sub-section.  The array components
*  and axes will be processed individually, but this enables the LABEL,
*  UNITS, HISTORY, AXIS character components, and extensions to be
*  propagated.
      CALL LPG_PROP( NDFS, 'Axis,Units', 'OUT', NDFO, STATUS )

*  Set its bounds to those found above.
      CALL NDF_SBND( NDIMI, LBNDO, UBNDO, NDFO, STATUS )

*  Obtain a title and assign it to the output NDF.
*  ===============================================

*  A null results in the output title being the same as the input
*  title.
      CALL KPG1_CCPRO( 'TITLE', 'TITLE', NDFI, NDFO, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Compress the data array.
*  ========================
*
*  Determine the processing type and the data type of the output data
*  array and variance.

*  Determine the numeric type to be used for processing the input
*  data and variance (if any) arrays.  Since the subroutines that
*  perform the averaging need the data and variance arrays in the same
*  data type, the component list is used.  This application supports
*  single- and double-precision floating-point processing.
      CALL NDF_MTYPE( '_REAL,_DOUBLE', NDFI, NDFI, COMP, ITYPE, DTYPE,
     :                STATUS )

*  When the types of the input NDF's arrays are to be preserved, just
*  inquire what it is and set the output NDF type accordingly.  Notice
*  the data and variance arrays may be different, and so are handled
*  separately.  Otherwise the output arrays will have the mapped data
*  type upon unmapping.
      IF ( PRESTY ) THEN
         CALL NDF_TYPE( NDFI, 'Data', TYPE, STATUS )
         CALL NDF_STYPE( TYPE, NDFO, 'Data', STATUS )
         IF ( VAR ) THEN
            CALL NDF_TYPE( NDFI, 'Variance', TYPE, STATUS )
            CALL NDF_STYPE( TYPE, NDFO, 'Variance', STATUS )
         END IF
      END IF

*  Set the quantity of workspace required for averaging.  In order to
*  save time by not annulling and then re-creating slightly
*  different-sized summation work arrays, the first is made
*  sufficiently large to to satisfy all requests.  Also set the type of
*  the space for counting required by the appropriate averaging
*  subroutine.
      ELWS1 = 1
      DO I = 1, NDIM
         ELWS1 = MAX( ELWS1, IDIMS( I ) )
      END DO
      ELWS1 = ELWS1 * 2
      IF ( VAR ) THEN
         ELWS2 = IDIMS( 1 ) * 2
         CTYPE = ITYPE
      ELSE
         ELWS2 = IDIMS( 1 )
         CTYPE = '_INTEGER'
      END IF

*  Obtain some workspace for the averaging and map them.  First find
*  the quantity and the type of the counting space required.
      CALL PSX_CALLOC( ELWS1, ITYPE, WPNTR1, STATUS )
      CALL PSX_CALLOC( ELWS2, CTYPE, WPNTR2, STATUS )

*  Map the full input, and output data arrays.
      CALL KPG1_MAP( NDFI, COMP, ITYPE, 'READ', PNTRI, EL, STATUS )
      CALL KPG1_MAP( NDFO, COMP, ITYPE, 'WRITE', PNTRO, EL, STATUS )

*  Compress the input array to make the output array by averaging in
*  non-overlapping boxes.  Call the appropriate routine depending on
*  whether there is variance or not, and the implementation type.
      IF ( VAR ) THEN
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_CMVVR( NDIM, IDIMS,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       %VAL( CNF_PVAL( PNTRI( 2 ) ) ),
     :                       COMPRS, NLIM,
     :                       WEIGHT, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       %VAL( CNF_PVAL( PNTRO( 2 ) ) ),
     :                       %VAL( CNF_PVAL( WPNTR1 ) ),
     :                       %VAL( CNF_PVAL( WPNTR2 ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_CMVVD( NDIM, IDIMS,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       %VAL( CNF_PVAL( PNTRI( 2 ) ) ),
     :                       COMPRS, NLIM,
     :                       WEIGHT, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       %VAL( CNF_PVAL( PNTRO( 2 ) ) ),
     :                       %VAL( CNF_PVAL( WPNTR1 ) ),
     :                       %VAL( CNF_PVAL( WPNTR2 ) ), STATUS )
         END IF
      ELSE
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_CMAVR( NDIM, IDIMS,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       COMPRS, NLIM,
     :                       %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       %VAL( CNF_PVAL( WPNTR1 ) ),
     :                       %VAL( CNF_PVAL( WPNTR2 ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_CMAVD( NDIM, IDIMS,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       COMPRS, NLIM,
     :                       %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       %VAL( CNF_PVAL( WPNTR1 ) ),
     :                       %VAL( CNF_PVAL( WPNTR2 ) ), STATUS )
         END IF
      END IF

*  Tidy the data and variance arrays.
      CALL NDF_UNMAP( NDFI, COMP, STATUS )
      CALL NDF_UNMAP( NDFO, COMP, STATUS )

*  Tidy the second workspace array, since its type may not be
*  constant when processing the axis arrays---it could be the
*  implementation type or _INTEGER.
      CALL PSX_FREE( WPNTR2, STATUS )

*  Compress AXIS centre and variance arrays.
*  =========================================
*
*  First see whether or not there is an AXIS structure to compress.
      CALL NDF_STATE( NDFI, 'Axis', AXIS, STATUS )

      IF ( AXIS ) THEN

*  Obtain the the flag to decide whether or not the axis variance, if
*  present is used to weight the centres.
         CALL PAR_GTD0L( 'AXWEIGHT', .FALSE., .FALSE., AXWT, STATUS )

*  Define dummy compression factor and dimension.
         ACOMPR( 2 ) = 1
         ADIMS( 2 ) = 1

*  Loop for all axes.
         DO IAXIS = 1, NDIMI

*  Inquire whether or not there is a variance array and derive a list
*  of the axis array components to be processed together.  The variance
*  component cannot be processed alone, since it needs to know about
*  bad values in the centre array, so only the variances of good centre
*  values are used to find a resultant variance.  Also define the size
*  and type of the counting workspace.  Note this assumes that the
*  processing type of the main data and variance is going to be used
*  for the axis centre and variance arrays (see below for the reason
*  why).
            AVAR = .FALSE.
            CALL NDF_ASTAT( NDFI, 'Variance', IAXIS, AVAR, STATUS )
            IF ( AVAR ) THEN
               COMP = 'Centre,Variance'
               CTYPE = ITYPE
               ELWS2 = IDIMS( IAXIS ) * 2
            ELSE
               COMP = 'Centre'
               CTYPE = '_INTEGER'
               ELWS2 = IDIMS( IAXIS )
            END IF

*  When the types of the input NDF's arrays are to be preserved, just
*  inquire what it is and set the output NDF type accordingly.  Notice
*  the data and variance arrays may be different, and so are handled
*  separately.  Otherwise the output axis arrays will have the mapped
*  data type upon unmapping.
            IF ( PRESTY ) THEN
               CALL NDF_ATYPE( NDFI, 'Centre', IAXIS, TYPE, STATUS )
               CALL NDF_ASTYP( TYPE, NDFO, 'Centre', IAXIS, STATUS )
               IF ( AVAR ) THEN
                  CALL NDF_ATYPE( NDFI, 'Varianc', IAXIS, TYPE, STATUS )
                  CALL NDF_ASTYP( TYPE, NDFO, 'Varianc', IAXIS, STATUS )
               END IF
            END IF

*  Obtain the workspace for counting.
            CALL PSX_CALLOC( ELWS2, CTYPE, WPNTR2, STATUS )

*  Since there is no NDF_AMTYP for matching types, we'll have to assume
*  the implementation type of the data and/or main variance arrays
*  is satisfactory.  Map the input and output axis arrays.
            CALL NDF_AMAP( NDFI, COMP, IAXIS, ITYPE, 'READ',
     :                     PNTRI, ELA, STATUS )
            CALL NDF_AMAP( NDFO, 'Centre', IAXIS, ITYPE, 'WRITE',
     :                     PNTRO, EL, STATUS )

*  Fill in the actual compression factor and the dimension for the axis.
            ACOMPR( 1 ) = COMPRS( IAXIS )
            ADIMS( 1 ) = IDIMS( IAXIS )

*  Compress the input array to make the output array by averaging in
*  non-overlapping boxes.  Note that each axis array is one-dimensional,
*  so pass just the required dimension, and compression factor.  Call
*  the appropriate routine depending on whether there is variance or
*  not, and the implementation type.  Since the original averaging
*  workspace is of sufficient length, is initialised each time it is
*  used, and also because we are using the same implementation type as
*  the data and main variance arrays we can re-cycle it.  If an
*  NDF_AMTYP is written then this will have to change.
            IF ( AVAR ) THEN
               IF ( ITYPE .EQ. '_REAL' ) THEN
                  CALL KPG1_CMVVR( 2, ADIMS,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             %VAL( CNF_PVAL( PNTRI( 2 ) ) ),
     :                             ACOMPR,
     :                             NLIM, AXWT,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             %VAL( CNF_PVAL( PNTRO( 2 ) ) ),
     :                             %VAL( CNF_PVAL( WPNTR1 ) ),
     :                             %VAL( CNF_PVAL( WPNTR2 ) ), STATUS )

               ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                  CALL KPG1_CMVVD( 2, ADIMS,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             %VAL( CNF_PVAL( PNTRI( 2 ) ) ),
     :                             ACOMPR,
     :                             NLIM, AXWT,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             %VAL( CNF_PVAL( PNTRO( 2 ) ) ),
     :                             %VAL( CNF_PVAL( WPNTR1 ) ),
     :                             %VAL( CNF_PVAL( WPNTR2 ) ), STATUS )
               END IF
            ELSE
               IF ( ITYPE .EQ. '_REAL' ) THEN
                  CALL KPG1_CMAVR( 2, ADIMS,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             ACOMPR, NLIM,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             %VAL( CNF_PVAL( WPNTR1 ) ),
     :                             %VAL( CNF_PVAL( WPNTR2 ) ),
     :                             STATUS )

               ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                  CALL KPG1_CMAVD( 2, ADIMS,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             ACOMPR, NLIM,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             %VAL( CNF_PVAL( WPNTR1 ) ),
     :                             %VAL( CNF_PVAL( WPNTR2 ) ),
     :                             STATUS )
               END IF
            END IF

*  Tidy the centre and variance arrays.
            CALL NDF_AUNMP( NDFI, COMP, IAXIS, STATUS )
            CALL NDF_AUNMP( NDFO, COMP, IAXIS, STATUS )

*  Tidy the counting workspace.
            CALL PSX_FREE( WPNTR2, STATUS )
         END DO

*  Extend the AXIS width arrays.
*  =============================

*  Loop for all axes.
         DO IAXIS = 1, NDIMI

*  Inquire whether or not there is a width array.
            WIDTH = .FALSE.
            CALL NDF_ASTAT( NDFI, 'Width', IAXIS, WIDTH, STATUS )

            IF ( WIDTH ) THEN

*  When the type of the input NDF's axis width array is to be preserved,
*  just inquire what it is and set the output type accordingly.
               IF ( PRESTY ) THEN
                  CALL NDF_ATYPE( NDFI, 'Width', IAXIS, TYPE, STATUS )
                  CALL NDF_ASTYP( TYPE, NDFO, 'Width', IAXIS, STATUS )
               END IF

*  Obtain the workspace for counting.
               CALL PSX_CALLOC( IDIMS( IAXIS ), '_INTEGER', WPNTR2,
     :                          STATUS )

*  Since there is no NDF_AMTYP for matching types, we'll have to assume
*  the implementation type of the data and/or main variance arrays
*  is satisfactory.  Map the input and output width arrays.
               CALL NDF_AMAP( NDFI, 'Width', IAXIS, ITYPE, 'READ',
     :                        PNTRI, ELA, STATUS )
               CALL NDF_AMAP( NDFO, 'Width', IAXIS, ITYPE, 'WRITE',
     :                        PNTRO, EL, STATUS )

*  Fill in the actual compression factor and the dimension for the axis.
               ACOMPR( 1 ) = COMPRS( IAXIS )
               ADIMS( 1 ) = IDIMS( IAXIS )

*  Compress the input width array to make the output array by SUMMING
*  in non-overlapping boxes.  Note that each axis array is
*  one-dimensional, so pass just the required dimension, and compression
*  factor.  Call the appropriate routine depending on the
*  implementation type.  Since the original workspace is sufficiently
*  large, initialised each time it is used we can re-cycle just one
*  pair of arrays, and also because we are using the same
*  implementation type as the data and main variance arrays.  If an
*  NDF_AMTYP is written then this will have to change.  The widths are
*  normalised in case there are undefined widths present.
               IF ( ITYPE .EQ. '_REAL' ) THEN
                  CALL KPG1_CMADR( 2, ADIMS,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             ACOMPR, NLIM, .TRUE.,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             %VAL( CNF_PVAL( WPNTR1 ) ),
     :                             %VAL( CNF_PVAL( WPNTR2 ) ), STATUS )

               ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                  CALL KPG1_CMADD( 2, ADIMS,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             ACOMPR, NLIM, .TRUE.,
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             %VAL( CNF_PVAL( WPNTR1 ) ),
     :                             %VAL( CNF_PVAL( WPNTR2 ) ), STATUS )
               END IF

*  Tidy the axis-width arrays.
               CALL NDF_AUNMP( NDFI, 'Width', IAXIS, STATUS )
               CALL NDF_AUNMP( NDFO, 'Width', IAXIS, STATUS )

*  Tidy the counting workspace.
               CALL PSX_FREE( WPNTR2, STATUS )
            END IF
         END DO
      END IF

*  Propagate the WCS component, incorporating a linear mapping between
*  pixel co-ordinates.  This mapping is described by a matrix and an
*  offset vector.  Set these up.
      DO I = 1, NDIMI * NDIMI
         MATRIX( I ) = 0.0D0
      END DO

      DO J = 1, NDIMI
         OFFSET( J ) = 0.0
         MATRIX( NDIMI * ( J - 1 ) + J ) = 1.0D0 / DBLE( COMPRS( J ) )
      END DO

*  Propagate the WCS component.
      CALL KPG1_ASPRP( NDIMI, NDFI, NDFO, MATRIX, OFFSET, STATUS )

*  Tidy the counting workspace.
      CALL PSX_FREE( WPNTR1, STATUS )

*  Come here if something has gone wrong.
  999 CONTINUE

*  Tidy the NDF system.
      CALL NDF_END( STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'COMPAVE_ERR',
     :     'COMPAVE: Unable to compress an NDF by averaging '/
     :     /'neighbouring values.', STATUS )
      END IF

      END
