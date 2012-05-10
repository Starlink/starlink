      SUBROUTINE FLIP( STATUS )
*+
*  Name:
*     FLIP

*  Purpose:
*     Reverses an NDF's pixels along a specified dimension.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FLIP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application reverses the order of an NDF's pixels along a
*     specified dimension, leaving all other aspects of the data
*     structure unchanged.

*  Usage:
*     flip in out dim

*  ADAM Parameters:
*     AXIS = _LOGICAL (Read)
*        If a TRUE value is given for this parameter (the default),
*        then any axis values and WCS information associated with the
*        NDF dimension being reversed will also be reversed in the same
*        way.  If a FALSE value is given, then all axis values and WCS
*        information will be left unchanged. [TRUE]
*     DIM = _INTEGER (Read)
*        The number of the dimension along which the NDF's pixels
*        should be reversed.  The value should lie between 1 and the
*        total number of NDF dimensions.  If the NDF has only a single
*        dimension, then this parameter is not used, a value of 1 being
*        assumed.
*     IN = NDF (Read)
*        The input NDF data structure whose pixel order is to be
*        reversed.
*     OUT = NDF (Write)
*        The output NDF data structure.
*     TITLE = LITERAL (Read)
*        A title for the output NDF.  A null value will cause the title
*        of the NDF supplied for parameter IN to be used instead. [!]

*  Examples:
*     flip a b 2
*        Reverses the pixels in the NDF called a along its second
*        dimension to create the new NDF called b.
*     flip specin specout
*        If specin is a 1-dimensional spectrum, then this example
*        reverses the order of its pixels to create a new spectrum
*        specout.  Note that no value for the DIM parameter need be
*        supplied in this case.
*     flip in=cube out=newcube dim=2 noaxis
*        Reverses the order of the pixels along dimension 2 of the NDF
*        called cube to give newcube, but leaves the associated axis
*        values in their original order.

*  Notes:
*     The pixel-index bounds of the NDF are unchanged by this routine.

*  Related Applications:
*     KAPPA: ROTATE, RESAMPLE; Figaro: IREVX, IREVY, IROT90.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of the
*     input NDF and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  The data
*     type of the input pixels is preserved in the output NDF.

*  Copyright:
*     Copyright (C) 1991-1992 Science & Engineering Research Council.
*     Copyright (C) 1998, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     13-MAR-1991 (RFWS):
*        Original version.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     11-JUN-1998 (DSB):
*        Added propagation of the NDF WCS component.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2012 May 8 (MJC):
*        Add _INT64 support.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      BYTE BB                    ! Quality bad-bits value
      CHARACTER * ( 8 ) ACOMP( 3 ) ! Axis array components to process
      CHARACTER * ( 8 ) COMP( 3 ) ! Array components to process
      CHARACTER * ( NDF__SZFRM ) FORM ! Form of the NDF array
      CHARACTER * ( NDF__SZTYP ) TYPE ! Array component numeric type
      DOUBLE PRECISION MATRIX( NDF__MXDIM*NDF__MXDIM )! Matrix component of linear mapping
      DOUBLE PRECISION OFFSET( NDF__MXDIM ) ! Translation component of linear mapping
      INTEGER DIM( NDF__MXDIM )  ! NDF dimension sizes
      INTEGER EL                 ! Number of elements mapped
      INTEGER I                  ! Axis index
      INTEGER ICOMP              ! Loop counter for array components
      INTEGER IDIM               ! Dimension to reverse pixels along
      INTEGER LBND( NDF__MXDIM ) ! Lower pixel index bounds
      INTEGER NDF1               ! Input NDF identifier
      INTEGER NDF2               ! Output NDF identifier
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER PNTR1( 1 )         ! Pointer to mapped input array
      INTEGER PNTR2( 1 )         ! Pointer to mapped output array
      INTEGER UBND( NDF__MXDIM ) ! Upper pixel index bounds
      LOGICAL AXIS               ! Reverse axis arrays?
      LOGICAL BAD                ! Bad-pixel flag
      LOGICAL THERE              ! Whether component is defined

*  Local Data:
      DATA COMP / 'Data', 'Variance', 'Quality' /
      DATA ACOMP / 'Centre', 'Width', 'Variance' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF and determine its dimension sizes.
      CALL LPG_ASSOC( 'IN', 'READ', NDF1, STATUS )
      CALL NDF_DIM( NDF1, NDF__MXDIM, DIM, NDIM, STATUS )

*  Create the output NDF, propagating the axis and units values (some of
*  the propagated axis values and WCS component may later be over-written).
      CALL LPG_PROP( NDF1, 'Axis,Units,Wcs', 'OUT', NDF2, STATUS )

*  Determine which NDF dimension the pixels are to be reversed along.
*  Only do this if there is a choice.
      IF ( NDIM .EQ. 1 ) THEN
         IDIM = 1
      ELSE
         CALL PAR_GDR0I( 'DIM', 1, 1, NDIM, .FALSE., IDIM, STATUS )
      END IF

*  See whether the associated axis arrays are also to be reversed.
      CALL PAR_GET0L( 'AXIS', AXIS, STATUS )

*  Process the main NDF array components.
*  =====================================

*  Disable automatic quality masking, since the quality array (if
*  present) will be handled explicitly.
      CALL NDF_SQMF( .FALSE., NDF1, STATUS )

*  Loop to process the data, variance and quality arrays in turn.
      DO 1 ICOMP = 1, 3

*  Determine if the input array is defined.
         CALL NDF_STATE( NDF1, COMP( ICOMP ), THERE, STATUS )

*  If so, then determine its numeric type and map the input and output
*  arrays for access using this type.
         IF ( THERE ) THEN
            CALL NDF_TYPE( NDF1, COMP( ICOMP ), TYPE, STATUS )
            CALL KPG1_MAP( NDF1, COMP( ICOMP ), TYPE, 'READ', PNTR1,
     :                     EL, STATUS )
            CALL KPG1_MAP( NDF2, COMP( ICOMP ), TYPE, 'WRITE', PNTR2,
     :                     EL, STATUS )

*  Call the appropriate routine to process the array, depending on its
*  numeric type.
            IF ( TYPE .EQ. '_BYTE' ) THEN
               CALL KPG1_FLIPB( NDIM, DIM,
     :                          %VAL( CNF_PVAL( PNTR1( 1 ) ) ), IDIM,
     :                          %VAL( CNF_PVAL( PNTR2( 1 ) ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
               CALL KPG1_FLIPUB( NDIM, DIM,
     :                           %VAL( CNF_PVAL( PNTR1( 1 ) ) ), IDIM,
     :                           %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                           STATUS )

            ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
               CALL KPG1_FLIPD( NDIM, DIM,
     :                          %VAL( CNF_PVAL( PNTR1( 1 ) ) ), IDIM,
     :                          %VAL( CNF_PVAL( PNTR2( 1 ) ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
               CALL KPG1_FLIPI( NDIM, DIM,
     :                          %VAL( CNF_PVAL( PNTR1( 1 ) ) ), IDIM,
     :                          %VAL( CNF_PVAL( PNTR2( 1 ) ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_INT64' ) THEN
               CALL KPG1_FLIPK( NDIM, DIM,
     :                          %VAL( CNF_PVAL( PNTR1( 1 ) ) ), IDIM,
     :                          %VAL( CNF_PVAL( PNTR2( 1 ) ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_REAL' ) THEN
               CALL KPG1_FLIPR( NDIM, DIM,
     :                          %VAL( CNF_PVAL( PNTR1( 1 ) ) ), IDIM,
     :                          %VAL( CNF_PVAL( PNTR2( 1 ) ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_WORD' ) THEN
               CALL KPG1_FLIPW( NDIM, DIM,
     :                          %VAL( CNF_PVAL( PNTR1( 1 ) ) ), IDIM,
     :                          %VAL( CNF_PVAL( PNTR2( 1 ) ) ), STATUS )

            ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
               CALL KPG1_FLIPUW( NDIM, DIM,
     :                           %VAL( CNF_PVAL( PNTR1( 1 ) ) ), IDIM,
     :                           %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                           STATUS )
            END IF

*  If a quality array is being processed, then transfer the quality
*  component's bad-bits value.
            IF ( COMP( ICOMP ) .EQ. 'Quality' ) THEN
               CALL NDF_BB( NDF1, BB, STATUS )
               CALL NDF_SBB( BB, NDF2, STATUS )

*  Otherwise, transfer the bad-pixel flag from the input to the output
*  array unless its storage format is primitive.
            ELSE
               CALL NDF_FORM( NDF2, COMP( ICOMP ), FORM, STATUS )
               CALL NDF_BAD( NDF1, COMP( ICOMP ), .FALSE., BAD, STATUS )
               IF ( FORM .NE. 'PRIMITIVE' ) THEN
                  CALL NDF_SBAD( BAD, NDF2, COMP( ICOMP ), STATUS )
               END IF
            END IF

*  Unmap the input and output arrays.
            CALL NDF_UNMAP( NDF1, COMP( ICOMP ), STATUS )
            CALL NDF_UNMAP( NDF2, COMP( ICOMP ), STATUS )
         END IF
 1    CONTINUE

*  Process the axis arrays.
*  =======================

*  If axis information for the reversed dimension is also to be
*  reversed, then loop to process the axis centre, width and variance
*  arrays.
      IF ( AXIS ) THEN
         DO 2 ICOMP = 1, 3

*  Determine if the input axis array is defined.
            CALL NDF_ASTAT( NDF1, ACOMP( ICOMP ), IDIM, THERE, STATUS )

*  If so, then determine its numeric type and map the input and output
*  axis arrays for access using this type.
            IF ( THERE ) THEN
               CALL NDF_ATYPE( NDF1, ACOMP( ICOMP ), IDIM, TYPE,
     :                         STATUS )
               CALL NDF_AMAP( NDF1, ACOMP( ICOMP ), IDIM, TYPE, 'READ',
     :                        PNTR1, EL, STATUS )
               CALL NDF_AMAP( NDF2, ACOMP( ICOMP ), IDIM, TYPE, 'WRITE',
     :                        PNTR2, EL, STATUS )

*  Call the appropriate routine to process the array, depending on its
*  numeric type.
               IF ( TYPE .EQ. '_BYTE' ) THEN
                  CALL KPG1_FLIPB( 1, DIM( IDIM ),
     :                             %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                             1, %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                  CALL KPG1_FLIPUB( 1, DIM( IDIM ),
     :                              %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                              1, %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                              STATUS )

               ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                  CALL KPG1_FLIPD( 1, DIM( IDIM ),
     :                             %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                             1, %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                  CALL KPG1_FLIPI( 1, DIM( IDIM ),
     :                             %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                             1, %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( TYPE .EQ. '_INT64' ) THEN
                  CALL KPG1_FLIPK( 1, DIM( IDIM ),
     :                             %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                             1, %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( TYPE .EQ. '_REAL' ) THEN
                  CALL KPG1_FLIPR( 1, DIM( IDIM ),
     :                             %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                             1, %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                  CALL KPG1_FLIPW( 1, DIM( IDIM ),
     :                             %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                             1, %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                  CALL KPG1_FLIPUW( 1, DIM( IDIM ),
     :                              %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                              1, %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                              STATUS )
               END IF

*  Unmap the input and output axis arrays.
               CALL NDF_AUNMP( NDF1, ACOMP( ICOMP ), IDIM, STATUS )
               CALL NDF_AUNMP( NDF2, ACOMP( ICOMP ), IDIM, STATUS )
            END IF
 2       CONTINUE
      END IF

*  Propagate the WCS component.
*  ============================
      IF( AXIS ) THEN

*  Set up a matrix and offset vector describing the linear mapping from
*  input pixel coordinates to output pixel coordinates. First of all
*  set the matrix and vector to a unit transformation.
         DO I = 1, NDIM*NDIM
            MATRIX( I ) = 0.0D0
         END DO

         DO I = 1, NDIM
            OFFSET( I ) = 0.0D0
            MATRIX( NDIM*( I - 1 ) + I ) = 1.0D0
         END DO

*  Now change the scale factor for the flipped axis to -1.0
         MATRIX( NDIM*( IDIM - 1 ) + IDIM ) = -1.0D0

*  Set the offset for the flipped axis.
         CALL NDF_BOUND( NDF1, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
         OFFSET( IDIM ) = DBLE( UBND( IDIM ) + LBND( IDIM ) - 1 )

*  Propagate the WCS component.
         CALL KPG1_ASPRP( NDIM, NDF1, NDF2, MATRIX, OFFSET, STATUS )

      END IF

*  Obtain a new title for the output NDF.
      CALL NDF_CINP( 'TITLE', NDF2, 'Title', STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FLIP_ERR',
     :     'FLIP: Error reversing an NDF''s pixels along a dimension.',
     :     STATUS )
      END IF

      END
