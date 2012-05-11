      SUBROUTINE RESHAPE( STATUS )
*+
*  Name:
*     RESHAPE

*  Purpose:
*     Reshapes an NDF, treating its arrays as vectors.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL RESHAPE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application reshapes an NDF to create another NDF by copying
*     array values.  The array components in the input NDF are treated
*     as vectors.  Each output array is filled in order with values from
*     the input vector, until it is full or the input vector is
*     exhausted.  Output data and variance pixels not filled are set to
*     the bad value; unfilled quality pixels are set to zero.  The
*     filling is in Fortran order, namely the first dimension, followed
*     by the second dimension,... to the highest dimension.
*
*     It is possible to form a vectorized NDF using parameter VECTORIZE
*     without having to specify the shape.

*  Usage:
*     reshape in out shape=?

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input NDF to be reshaped.
*     OUT = NDF (Read)
*        The NDF after reshaping.
*     SHAPE( ) = _INTEGER (Read)
*        The shape of the output NDF.  For example, [50,30,20] would
*        create 50 columns by 30 lines by 20 bands.  It is only
*        accessed when VECTORIZE = FALSE.
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the base NDF to the output NDF. [!]
*     VECTORIZE = _LOGICAL (Read)
*        If TRUE, the output NDF is the vectorized form of the input
*        NDF.  If FALSE, parameter SHAPE is used to specify the new
*        shape.  [FALSE]

*  Examples:
*     reshape shear normal shape=[511,512]
*        This reshapes the NDF called shear to form NDF normal, whose
*        shape is 511 x 512 pixels.  One example is where the original
*        image has 512 x 512 pixels but one pixel was omitted from each
*        line during some data capture, causing the image to be sheared
*        between lines.
*     reshape cube cube1d vectorize
*        This vectorizes the NDF called cube to form NDF cube1d.  This
*        could be used for a task that only permits one-dimensional
*        data.

*  Related Applications:
*     KAPPA: CHAIN, PASTE, RESHAPE.

*  Implementation Status:
*     -  This routine correctly processes the DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, and HISTORY, components of an NDF
*     data structure and propagates all extensions.  WCS and AXIS information
*     is lost.
*     -  All non-complex numeric data types can be handled.
*     -  Any number of NDF dimensions is supported.

*  Copyright:
*     Copyright (C) 1997-1998, 2004 Central Laboratory of the Research
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
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1997 June 15 (MJC):
*        Original version.
*     8-DEC-1998 (DSB):
*        Corrected the name of the output NDF component to receive the
*        QUALITY values from 'variance' to 'quality'.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     20-MAY-2010 (DSB):
*        Map output Quality array with type "_UBYTE", not ITYPE.
*     2012 May 10 (MJC):
*        Add _INT64 support.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ PRIMDAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER EL                 ! Number of elements copied
      INTEGER ELIN               ! Number of elements in a mapped input
                                 ! array
      INTEGER ELOUT              ! Number of elements in an output array
      INTEGER I                  ! Loop counter
      INTEGER IERR               ! Index of first copy error (not used)
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Processing type of the data
                                 ! array
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of output array
      INTEGER NDFI               ! Identifier for input NDF
      INTEGER NDFO               ! Identifier for output NDF
      INTEGER NDIMS              ! Number of dimensions in output array
      INTEGER NERR               ! Number of copy errors (not used)
      INTEGER PNTRI( 1 )         ! Pointer to input array component
      INTEGER PNTRO( 1 )         ! Pointer to output array component
      LOGICAL QUAPRS             ! Quality is present in the NDF?
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of output array
      LOGICAL VARPRS             ! Variance is present in the NDF?
      LOGICAL VECTOR             ! Vectorize the input NDF?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the NDF.
*  ===============

*  Start an NDF context.
      CALL NDF_BEGIN

*  Get the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Determine which array components are present.
      CALL NDF_STATE( NDFI, 'Variance', VARPRS, STATUS )
      CALL NDF_STATE( NDFI, 'Quality', QUAPRS, STATUS )

*  Determine the shape of the output NDF.
*  ======================================

*  Get the size of the input NDF.
      CALL NDF_SIZE( NDFI, ELIN, STATUS )

*  Do we want to vectorize the the array.
      CALL PAR_GET0L( 'VECTORIZE', VECTOR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Specify the vector's dimensionality and bounds.
      IF ( VECTOR ) THEN
         NDIMS = 1
         LBND( 1 ) = 1
         UBND( 1 ) = ELIN

*  Get the SHAPE
      ELSE

*  Get the shape of the output NDF.
         CALL PAR_GDRVI( 'SHAPE', NDF__MXDIM, 1, VAL__MAXI, UBND,
     :                   NDIMS, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Set the lower bounds.
         DO I = 1, NDIMS
            LBND( I ) = 1
         END DO

      END IF

*  Create the output NDF.
*  ======================
      CALL LPG_PROP( NDFI, 'UNITS', 'OUT', NDFO, STATUS )

*  Change to the desired shape.
      CALL NDF_SBND( NDIMS, LBND, UBND, NDFO, STATUS )

*  Obtain a title and assign it to the output NDF.
*  ===============================================

*  A null results in the output title being the same as the input
*  title because the title was already propagated by LPG_PROP above.
      CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )

*  Reshape the data component.
*  ===========================

*  Get the data type of the input data component.
      CALL NDF_TYPE( NDFI, 'Data', ITYPE, STATUS )

*  As values are merely passed verbatim, the automatic quality masking
*  is switched off.
      CALL NDF_SQMF( .FALSE., NDFI, STATUS )

*  Map the two data arrays.  Since the output array may be larger than
*  the input, first initialise it with the bad value.
      CALL NDF_MAP( NDFI, 'Data', ITYPE, 'READ', PNTRI, ELIN, STATUS )
      CALL NDF_MAP( NDFO, 'Data', ITYPE, 'WRITE/BAD', PNTRO, ELOUT,
     :              STATUS )

*  Find the number of values to copy.
      EL = MIN( ELIN, ELOUT )

*  Copy the data from the input to the output array, using the
*  appropriate routine for the data type.
      IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL VEC_RTOR( .TRUE., EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL VEC_BTOB( .TRUE., EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL VEC_DTOD( .TRUE., EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL VEC_ITOI( .TRUE., EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
         CALL VEC_KTOK( .TRUE., EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL VEC_UBTOUB( .TRUE., EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL VEC_UWTOUW( .TRUE., EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL VEC_WTOW( .TRUE., EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                  IERR, NERR, STATUS )
      END IF

*  Unmap the data arrays.
      CALL NDF_UNMAP( NDFI, 'Data', STATUS )
      CALL NDF_UNMAP( NDFO, 'Data', STATUS )

*  Reshape the variance array.
*  ===========================
      IF ( VARPRS ) THEN

*  As values are merely passed verbatim, the automatic quality masking
*  is switched off.  It was reset by NDF_UNMAP.
         CALL NDF_SQMF( .FALSE., NDFI, STATUS )

*  Map the two variance arrays.  Since the output array may be larger
*  than the input, first initialise it with the bad value.
         CALL NDF_MAP( NDFI, 'Variance', ITYPE, 'READ', PNTRI, ELIN,
     :                 STATUS )
         CALL NDF_MAP( NDFO, 'Variance', ITYPE, 'WRITE/BAD', PNTRO,
     :                 ELOUT, STATUS )

*  Copy the variance from the input to the output array, using the
*  appropriate routine for the data type.
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL VEC_RTOR( .TRUE., EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL VEC_BTOB( .TRUE., EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL VEC_DTOD( .TRUE., EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL VEC_ITOI( .TRUE., EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
            CALL VEC_KTOK( .TRUE., EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL VEC_UBTOUB( .TRUE., EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL VEC_UWTOUW( .TRUE., EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                        %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL VEC_WTOW( .TRUE., EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                     IERR, NERR, STATUS )
         END IF

*  Unmap the variance arrays.
         CALL NDF_UNMAP( NDFI, 'Variance', STATUS )
         CALL NDF_UNMAP( NDFO, 'Variance', STATUS )
      END IF

*  Reshape the quality array.
*  ==========================
      IF ( QUAPRS ) THEN

*  Map the quality arrays, which have type unsigned byte.  Since the output array may be larger
*  than the input, first initialise it with zero.
         CALL NDF_MAP( NDFI, 'Quality', '_UBYTE', 'READ', PNTRI, ELIN,
     :                 STATUS )
         CALL NDF_MAP( NDFO, 'Quality', '_UBYTE', 'WRITE/ZERO', PNTRO,
     :                 ELOUT, STATUS )

         CALL VEC_UBTOUB( .TRUE., EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    IERR, NERR, STATUS )

*  Unmap the quality arrays.
         CALL NDF_UNMAP( NDFI, 'Quality', STATUS )
         CALL NDF_UNMAP( NDFO, 'Quality', STATUS )
      END IF

  999 CONTINUE

*  Tidy the NDF system.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'RESHAPE_ERR',
     :     'RESHAPE: Error reshaping an NDF.', STATUS )
      END IF

      END
