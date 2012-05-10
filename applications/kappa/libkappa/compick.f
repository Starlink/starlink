      SUBROUTINE COMPICK( STATUS )
*+
*  Name:
*     COMPICK

*  Purpose:
*     Reduces the size of an NDF by picking equally spaced pixels.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL COMPICK( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application takes an NDF data structure and reduces it in
*     size by integer factors along each dimension.  The input NDF is
*     sampled at these constant compression factors or intervals along
*     each dimension, starting from a defined origin, to form an output
*     NDF structure.  The compression factors may be different in each
*     dimension.

*  Usage:
*     compick in out compress [origin]

*  ADAM Parameters:
*     COMPRESS( ) = _INTEGER (Read)
*        Linear compression factors to be used to create the output
*        NDF.  There should be one for each dimension of the NDF.  If
*        fewer are supplied the last value in the list of compression
*        factors is given to the remaining dimensions.  Thus if a
*        uniform compression is required in all dimensions, just one
*        value need be entered. The suggested default is the current value.
*     IN  = NDF (Read)
*        The NDF structure to be reduced in size.
*     ORIGIN( ) = _INTEGER (Read)
*        The pixel indices of the first pixel to be selected.
*        Thereafter the selected pixels will be spaced equally by
*        COMPRESS() pixels.  The origin must lie within the first
*        selection intervals, therefore the ith origin must be in the
*        range LBND(i) to LBND(i)+COMPRESS(i)-1, where LBND(i) is the
*        lower bound of the ith dimension.  If a null (!) value is
*        supplied, the first array element is used. [!]
*     OUT = NDF (Write)
*        NDF structure to contain compressed version of the input NDF.
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the input NDF to the output NDF. [!]

*  Examples:
*     compick cosmos galaxy 4
*        This compresses the NDF called cosmos selecting every fourth
*        array element along each dimension, starting from the first
*        element in the NDF, and stores the reduced data in the NDF
*        called galaxy.
*     compick cosmos galaxy 4 [3,2]
*        This compresses the two-dimensional NDF called cosmos
*        selecting every fourth array element along each dimension,
*        starting from the pixel index (3,2), and stores the
*        reduced data in the NDF called galaxy.
*     compick in=arp244 compress=[1,1,3] out=arp244cs
*        Suppose arp244 is a huge NDF storing a spectral-line data
*        cube, with the third dimension being the spectral axis.
*        This command compresses arp244 in the spectral dimension,
*        sampling every third pixel, starting from the first wavelength
*        at each image position, to form the NDF called arp244cs.

*  Notes:
*     -  The compression is centred on the origin of the pixel co-ordinate
*     Frame. That is, if a position has a value p(i) on the i'th pixel
*     co-ordinate axis of the input NDF, then it will have position
*     p(i)/COMPRESS(i) on the corresponding axis of the output NDF. The
*     pixel index bounds of the output NDF are chosen accordingly.

*  Algorithm:
*     -  Obtain the input NDF. Inquire its bounds and dimensions.
*     -  Obtain the compression factors with acceptable ranges.  Abort
*     if there is no compression.  Obtain the origin element, within the
*     first selection intervals and convert to offsets within the output
*     array.  Compute the dimensions of the output NDF.
*     -  Create the output NDF via a section of the input array whose
*     bounds are those of the output array in order to propagate
*     character components, AXIS, HISTORY, and extensions.
*     -  Compress the data array, and if present the variance, quality,
*     axis centre, axis width and variance.  This is done by mapping
*     the input and output arrays and calling the selection subroutine
*     of the appropriate type (_UBYTE for QUALITY).  For the AXIS
*     arrays each axis array is processed one-dimensionally.
*     -  Tidy the NDF system.

*  Related Applications:
*     KAPPA: BLOCK, COMPADD, COMPAVE, PIXDUPE, SQORST, RESAMPLE;
*     Figaro: ISTRETCH.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of the
*     input NDF and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  Any number of NDF dimensions is supported.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998, 2004 Central Laboratory of the Research
*     Councils.  Copyright (C) 2012 Science & Facilities Research
*     Council.  All Rights Reserved.

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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1991 November 27 (MJC):
*        Original version.
*     1995 January 11 (MJC):
*        Made TITLE propagate from the input NDF.
*     10-JUN-1998 (DSB):
*        Propagate WCS component. Ensure each output dimension is at least
*        one pixel long.
*     12-OCT-1998 (DSB):
*        Changed the way in which the bounds of the output image are
*        determined so that pixel origin information is retained.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2012 May 8 (MJC):
*        Add _INT64 support.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No default typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'       ! Global SSE definitions
      INCLUDE  'PAR_ERR'       ! Parameter-system errors
      INCLUDE  'NDF_PAR'       ! NDF_ public constants
      INCLUDE  'PRM_PAR'       ! VAL__ constants
      INCLUDE  'CNF_PAR'       ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER KPG1_FLOOR       ! Most positive integer .LE. a given real
      INTEGER KPG1_CEIL        ! Most negative integer .GE. a given real

*  Local Variables:
      CHARACTER
     :  TYPE * ( NDF__SZTYP )  ! Data type of an array component

      DOUBLE PRECISION
     :  MATRIX( NDF__MXDIM*NDF__MXDIM ),! Matrix component of linear mapping
     :  OFFSET( NDF__MXDIM )   ! Translation component of linear mapping

      INTEGER
     :  ACTVAL,                ! Actual number of compression factors
     :  CMPMAX( NDF__MXDIM ),  ! Maximum compression factors
     :  CMPMIN( NDF__MXDIM ),  ! Minimum compression factors
     :  COMPRS( NDF__MXDIM ),  ! Compression factors
     :  EL,                    ! Number of elements in mapped array
     :  ELA,                   ! Number of elements in input axis array
     :  I, J,                  ! Loop counter for the dimensions
     :  IAXIS,                 ! Loop counter for the axis-array
                               ! components
     :  IDIMS( NDF__MXDIM ),   ! Dimensions of input NDF
     :  LBND( NDF__MXDIM ),    ! Lower bounds of input NDF
     :  LBNDO( NDF__MXDIM ),   ! Lower bounds of output NDF
     :  NDFI,                  ! Identifier to the input NDF
     :  NDFIS,                 ! Identifier to the used section of the input NDF
     :  NDFO,                  ! Identifier to the output NDF
     :  NDFS,                  ! Identifier to the section of the input
                               ! NDF
     :  NDIM                   ! Dimensionality of the NDF

      INTEGER
     :  ODIMS( NDF__MXDIM ),   ! Dimensions of output array
     :  ORGDEF( NDF__MXDIM ),  ! Default origin indices
     :  ORGMAX( NDF__MXDIM ),  ! Maximum origin indices
     :  ORIGIN( NDF__MXDIM ),  ! Compression origin indices (1-based)
     :  ORIGN0( NDF__MXDIM ),  ! Compression origin indices (as given)
     :  PNTRI( 1 ),            ! Pointer to an input array component
     :  PNTRO( 1 ),            ! Pointer to an output array component
     :  TOTCMP,                ! Total compression factor
     :  UBND( NDF__MXDIM ),    ! Upper bounds of input NDF
     :  UBNDO( NDF__MXDIM )    ! Upper bounds of output NDF

      LOGICAL                  ! True if:
     :  AVAR,                  ! Axis variance is present
     :  AXIS,                  ! Axis structure is present
     :  QUAL,                  ! Quality is present
     :  VAR,                   ! Variance is present
     :  WIDTH                  ! Axis width is present

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
         CALL ERR_REP( 'ERR_COMPICK_NOCMPR',
     :     'COMPICK: There is no compression to be made.', STATUS )
         GOTO 999
      END IF

*  Obtain the origin element to be selected.
*  =========================================
*
*  Specify the upper extreme permitted values for the origin indices.
*  The lower limits are just the lower bounds.  Also set the default
*  to be the first element.
      DO I = 1, NDIM
         ORGMAX( I ) = LBND( I ) + COMPRS( I ) - 1
         ORGDEF( I ) = LBND( I )
      END DO

*  Get the origin indices.  All must be given.
      CALL PAR_GRM1I( 'ORIGIN', NDIM, ORGDEF, LBND, ORGMAX, .TRUE.,
     :                ORIGIN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Convert the origin to offsets within the array as this is required
*  by the subroutine that will perform the selections.
      DO I = 1, NDIM
         ORIGN0( I ) = ORIGIN( I )
         ORIGIN( I ) = ORIGIN( I ) - LBND( I ) + 1
      END DO

*  Compute the output NDF's dimensions.
*  ====================================

*  Work out the bounds for the output array and the size of the output
*  array from the input array dimensions and the compression factor.
*  The pixel origin is retained. Also modify the input bounds so that
*  they correspond to the section of the input image which is actually
*  used.
      DO I = 1, NDIM
         LBNDO( I ) = 1 + KPG1_CEIL( REAL( LBND( I ) - 1 )/
     :                               REAL( COMPRS( I ) ) )

         UBNDO( I ) = MAX( LBNDO( I ), KPG1_FLOOR( REAL( UBND( I ) )/
     :                                           REAL( COMPRS( I ) ) ) )
         ODIMS( I ) = UBNDO( I ) - LBNDO( I ) + 1

         LBND( I ) = 1 + COMPRS( I )*( LBNDO( I ) - 1 )
         UBND( I ) = COMPRS( I ) * UBNDO( I )
         IDIMS( I ) = UBND( I ) - LBND( I ) + 1
      END DO

*  Create a section of the input NDF containing the region will actually
*  be used (i.e. excluding any pixels which lie over the edge of the
*  output image).
      CALL NDF_SECT( NDFI, NDIM, LBND, UBND, NDFIS, STATUS )

*  Annul the original input NDF and use the section create above instead.
      CALL NDF_ANNUL( NDFI, STATUS )
      NDFI = NDFIS

*  Create the output NDF.
*  ======================
*
*  Take a shortcut to propagate ancillary data from the input NDF.
*  Create a section from the input NDF of the size of the required NDF.
      CALL NDF_SECT( NDFI, NDIM, LBNDO, UBNDO, NDFS, STATUS )

*  Create the output NDF based on the sub-section.  The array components
*  and axes will be processed individually, but this enables the LABEL,
*  UNITS, HISTORY, AXIS character components, and extensions to be
*  propagated.
      CALL LPG_PROP( NDFS, 'Axis,Units', 'OUT', NDFO, STATUS )

*  Obtain a title and assign it to the output NDF.
*  ===============================================

*  A null results in the output title being the same as the input
*  title.
      CALL KPG1_CCPRO( 'TITLE', 'TITLE', NDFI, NDFO, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Compress the data array.
*  ========================
*
*  Inquire the data-array's type.
      CALL NDF_TYPE( NDFI, 'Data', TYPE, STATUS )

*  Map the full input, and output data arrays.
      CALL KPG1_MAP( NDFI, 'Data', TYPE, 'READ', PNTRI, EL, STATUS )
      CALL KPG1_MAP( NDFO, 'Data', TYPE, 'WRITE', PNTRO, EL, STATUS )

*  Compress the input array to make the output array by selecting
*  elements at equal intervals.
      IF ( TYPE .EQ. '_REAL' ) THEN
         CALL KPG1_CMPKR( NDIM, IDIMS, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    COMPRS,
     :                    ORIGIN, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_CMPKB( NDIM, IDIMS, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    COMPRS,
     :                    ORIGIN, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_CMPKD( NDIM, IDIMS, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    COMPRS,
     :                    ORIGIN, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_CMPKI( NDIM, IDIMS, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    COMPRS,
     :                    ORIGIN, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         CALL KPG1_CMPKK( NDIM, IDIMS, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    COMPRS,
     :                    ORIGIN, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_CMPKUB( NDIM, IDIMS, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     COMPRS,
     :                     ORIGIN, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                     STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_CMPKUW( NDIM, IDIMS, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     COMPRS,
     :                     ORIGIN, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                     STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL KPG1_CMPKW( NDIM, IDIMS, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    COMPRS,
     :                    ORIGIN, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    STATUS )
      END IF

*    Tidy the data arrays.
      CALL NDF_UNMAP( NDFI, 'Data', STATUS )
      CALL NDF_UNMAP( NDFO, 'Data', STATUS )

*  Compress the variance array.
*  ============================
*
*  First see whether or not there is a variance array to compress.
      CALL NDF_STATE( NDFI, 'Variance', VAR, STATUS )

      IF ( VAR ) THEN

*  Inquire the variance-array's type.
         CALL NDF_TYPE( NDFI, 'Variance', TYPE, STATUS )

*  Map the full input, and output variance arrays.
         CALL KPG1_MAP( NDFI, 'Variance', TYPE, 'READ', PNTRI, EL,
     :                 STATUS )
         CALL KPG1_MAP( NDFO, 'Variance', TYPE, 'WRITE', PNTRO, EL,
     :                 STATUS )

*  Compress the input array to make the output array by selecting
*  elements at equal intervals.
         IF ( TYPE .EQ. '_REAL' ) THEN
            CALL KPG1_CMPKR( NDIM, IDIMS,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ), COMPRS,
     :                       ORIGIN, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       STATUS )

         ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
            CALL KPG1_CMPKB( NDIM, IDIMS,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ), COMPRS,
     :                       ORIGIN, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       STATUS )

         ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_CMPKD( NDIM, IDIMS,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ), COMPRS,
     :                       ORIGIN, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       STATUS )

         ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
            CALL KPG1_CMPKI( NDIM, IDIMS,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ), COMPRS,
     :                       ORIGIN, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       STATUS )

         ELSE IF ( TYPE .EQ. '_INT64' ) THEN
            CALL KPG1_CMPKK( NDIM, IDIMS,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ), COMPRS,
     :                       ORIGIN, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       STATUS )

         ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
            CALL KPG1_CMPKUB( NDIM, IDIMS,
     :                        %VAL( CNF_PVAL( PNTRI( 1 ) ) ), COMPRS,
     :                        ORIGIN, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                        STATUS )

         ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
            CALL KPG1_CMPKUW( NDIM, IDIMS,
     :                        %VAL( CNF_PVAL( PNTRI( 1 ) ) ), COMPRS,
     :                        ORIGIN, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                        STATUS )

         ELSE IF ( TYPE .EQ. '_WORD' ) THEN
            CALL KPG1_CMPKW( NDIM, IDIMS,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ), COMPRS,
     :                       ORIGIN, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       STATUS )
         END IF

*    Tidy the variance arrays.
         CALL NDF_UNMAP( NDFI, 'Variance', STATUS )
         CALL NDF_UNMAP( NDFO, 'Variance', STATUS )
      END IF

*  Compress the quality array.
*  ===========================
*
*  First see whether or not there is a quality array to compress.
      CALL NDF_STATE( NDFI, 'QUALITY', QUAL, STATUS )

      IF ( QUAL ) THEN

*  Map the full input, and output quality arrays.
         CALL KPG1_MAP( NDFI, 'Quality', '_UBYTE', 'READ', PNTRI, EL,
     :                 STATUS )
         CALL KPG1_MAP( NDFO, 'Quality', '_UBYTE', 'WRITE', PNTRO, EL,
     :                 STATUS )

*  Compress the input array to make the output array by selecting
*  elements at equal intervals.
         CALL KPG1_CMPKUB( NDIM, IDIMS, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     COMPRS,
     :                     ORIGIN, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                     STATUS )

*    Tidy the quality arrays.
         CALL NDF_UNMAP( NDFI, 'Quality', STATUS )
         CALL NDF_UNMAP( NDFO, 'Quality', STATUS )
      END IF

*  Compress AXIS centre arrays.
*  ============================
*
*  First see whether or not there is an AXIS structure to compress.
      CALL NDF_STATE( NDFI, 'Axis', AXIS, STATUS )

      IF ( AXIS ) THEN

*  Loop for all axes.
         DO IAXIS = 1, NDIM

*  Inquire the axis-centre array's type.
            CALL NDF_ATYPE( NDFI, 'Centre', IAXIS, TYPE, STATUS )

*  Map the input and output centre arrays.
            CALL NDF_AMAP( NDFI, 'Centre', IAXIS, TYPE, 'READ',
     :                     PNTRI, ELA, STATUS )
            CALL NDF_AMAP( NDFO, 'Centre', IAXIS, TYPE, 'WRITE',
     :                     PNTRO, EL, STATUS )

*  Compress the input array to make the output array by selecting
*  elements at equal intervals.  Note that each axis array is
*  1-dimensional, so pass just the required dimension, compression
*  factor and origin.
            IF ( TYPE .EQ. '_REAL' ) THEN
               CALL KPG1_CMPKR( 1, ELA, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
               CALL KPG1_CMPKB( 1, ELA, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
               CALL KPG1_CMPKD( 1, ELA, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
               CALL KPG1_CMPKI( 1, ELA, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_INT64' ) THEN
               CALL KPG1_CMPKK( 1, ELA, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
               CALL KPG1_CMPKUB( 1, ELA, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                           COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                           %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                           STATUS )

            ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
               CALL KPG1_CMPKUW( 1, ELA, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                           COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                           %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                           STATUS )

            ELSE IF ( TYPE .EQ. '_WORD' ) THEN
               CALL KPG1_CMPKW( 1, ELA, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                          %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )
            END IF

*    Tidy the centre arrays.
            CALL NDF_AUNMP( NDFI, 'Centre', IAXIS, STATUS )
            CALL NDF_AUNMP( NDFO, 'Centre', IAXIS, STATUS )
         END DO

*  Compress the AXIS width arrays.
*  ===============================

*  Loop for all axes.
         DO IAXIS = 1, NDIM

*  Inquire whether or not there is a width array.
            WIDTH = .FALSE.
            CALL NDF_ASTAT( NDFI, 'Width', IAXIS, WIDTH, STATUS )

            IF ( WIDTH ) THEN

*  Inquire the axis-width array's type.
               CALL NDF_ATYPE( NDFI, 'Width', IAXIS, TYPE, STATUS )

*  Map the input and output axis-width arrays.
               CALL NDF_AMAP( NDFI, 'Width', IAXIS, TYPE, 'READ',
     :                        PNTRI, ELA, STATUS )
               CALL NDF_AMAP( NDFO, 'Width', IAXIS, TYPE, 'WRITE',
     :                        PNTRO, EL, STATUS )

*  Compress the input array to make the output array by selecting
*  elements at equal intervals.  Note that each axis array is
*  1-dimensional, so pass just the required dimension, compression
*  factor and origin.
               IF ( TYPE .EQ. '_REAL' ) THEN
                  CALL KPG1_CMPKR( 1, ELA,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
                  CALL KPG1_CMPKB( 1, ELA,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                  CALL KPG1_CMPKD( 1, ELA,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                  CALL KPG1_CMPKI( 1, ELA,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( TYPE .EQ. '_INT64' ) THEN
                  CALL KPG1_CMPKK( 1, ELA,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                  CALL KPG1_CMPKUB( 1, ELA,
     :                              %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                              %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                              STATUS )

               ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                  CALL KPG1_CMPKUW( 1, ELA,
     :                              %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                              %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                              STATUS )

               ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                  CALL KPG1_CMPKW( 1, ELA,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             STATUS )

               END IF

*  Tidy the width arrays.
               CALL NDF_AUNMP( NDFI, 'Width', IAXIS, STATUS )
               CALL NDF_AUNMP( NDFO, 'Width', IAXIS, STATUS )
            END IF
         END DO

*  Compress the AXIS variance arrays.
*  ==================================

*  Loop for all axes.
         DO IAXIS = 1, NDIM

*  Inquire whether or not there is a variance array.
            AVAR = .FALSE.
            CALL NDF_ASTAT( NDFI, 'Variance', IAXIS, AVAR, STATUS )

            IF ( AVAR ) THEN

*  Inquire the axis-variance array's type.
               CALL NDF_ATYPE( NDFI, 'Variance', IAXIS, TYPE, STATUS )

*  Map the input and output axis-variance arrays.
               CALL NDF_AMAP( NDFI, 'Variance', IAXIS, TYPE, 'READ',
     :                        PNTRI, ELA, STATUS )
               CALL NDF_AMAP( NDFO, 'Variance', IAXIS, TYPE, 'WRITE',
     :                        PNTRO, EL, STATUS )

*  Compress the input array to make the output array by selecting
*  elements at equal intervals.  Note that each axis array is
*  1-dimensional, so pass just the required dimension, compression
*  factor and origin.
               IF ( TYPE .EQ. '_REAL' ) THEN
                  CALL KPG1_CMPKR( 1, ELA,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
                  CALL KPG1_CMPKB( 1, ELA,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                  CALL KPG1_CMPKD( 1, ELA,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                  CALL KPG1_CMPKI( 1, ELA,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( TYPE .EQ. '_INT64' ) THEN
                  CALL KPG1_CMPKK( 1, ELA,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                  CALL KPG1_CMPKUB( 1, ELA,
     :                              %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                              %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                              STATUS )

               ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                  CALL KPG1_CMPKUW( 1, ELA,
     :                              %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                              %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                              STATUS )

               ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                  CALL KPG1_CMPKW( 1, ELA,
     :                             %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                             COMPRS( IAXIS ), ORIGIN( IAXIS ),
     :                             %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                             STATUS )
               END IF

*  Tidy the variance arrays.
               CALL NDF_AUNMP( NDFI, 'Variance', IAXIS, STATUS )
               CALL NDF_AUNMP( NDFO, 'Variance', IAXIS, STATUS )
            END IF
         END DO
      END IF

*  Propagate the WCS component, incorporating a linear mapping between
*  pixel coordinates. This mapping is described by a matrix and an offset
*  vector. Set these up.
      DO I = 1, NDIM*NDIM
         MATRIX( I ) = 0.0
      END DO

      DO J = 1, NDIM
         OFFSET( J ) = DBLE( LBNDO( J ) - 1 ) -
     :                 DBLE( ORIGN0( J ) - 1 )/DBLE( COMPRS( J ) )
         MATRIX( NDIM*( J - 1 ) + J ) = 1.0D0/DBLE( COMPRS( J ) )
      END DO

*  Propagate the WCS component.
      CALL KPG1_ASPRP( NDIM, NDFI, NDFO, MATRIX, OFFSET, STATUS )

*  Come here if something has gone wrong.
  999 CONTINUE

*  Tidy the NDF system.
      CALL NDF_END( STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'COMPICK_ERR',
     :     'COMPICK: Unable to compress an NDF by selecting equally '/
     :     /'spaced pixels.', STATUS )
      END IF

      END
