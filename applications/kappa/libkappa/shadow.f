      SUBROUTINE SHADOW( STATUS )
*+
*  Name:
*     SHADOW

*  Purpose:
*     Enhances edges in a 2-dimensional NDF using a shadow effect.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SHADOW( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine enhances a 2-dimensional NDF by creating a
*     bas-relief or shadow effect, that causes features in an array to
*     appear as though they have been illuminated from the side by some
*     imaginary light source.  The enhancement is useful in locating
*     edges and fine detail in an array.

*  Usage:
*     shadow in out

*  ADAM Parameters:
*     IN = NDF (Read)
*        The 2-dimensional NDF to be enhanced.
*     OUT = NDF (Write)
*        The output NDF containing the enhanced image.
*     SHIFT( 2 )  =  _INTEGER (Given)
*         The shift in x and y pixel indices to be used in the
*         enhancement.  If the x shift is positive, positive features
*         in the original array will appear to be lit from the positive
*         x direction, i.e. from the right.  Similarly, if the y shift
*         is positive, the light source will appear to be shining from
*         the top of the array.  A one- or two-pixel shift is normally
*         adequate. [1,1]
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the NDF supplied for parameter IN to be used
*        instead. [!]

*  Examples:
*     shadow horse horse_bas
*        This enhances the NDF called horse by making it appear to be
*        illuminated from the top right, and stores the result in the
*        NDF called horse_bas.
*     shadow out=aash in=aa [-1,-1] title="Bas relief"
*        This enhances the NDF called aa by making it appear to be
*        illuminated from the bottom left, and stores the result in the
*        NDF called aash, which has the title "Bas relief".

*  Related Applications:
*     KAPPA: LAPLACE, MEDIAN; Figaro: ICONV3.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, AXIS and HISTORY components of an NDF data
*     structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  The output NDF will be trimmed compared with the input NDF
*     by the shifts applied.

*  Copyright:
*     Copyright (C) 1995, 1998, 2004 Central Laboratory of the Research
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
*     1995 April 28 (MJC):
*        Original NDF version.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2012 May 9 (MJC):
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

*  Local Constants:
      INTEGER NDIM               ! Required dimensionality
      PARAMETER ( NDIM = 2 )

*  Local Variables:
      LOGICAL BAD                ! Need to check for bad pixels?
      INTEGER DIMS( NDIM )       ! Dimensions of significant axes
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Data type for output components
      INTEGER EL                 ! Number of mapped elements
      INTEGER IERR               ! Position of first error (dummy)
      CHARACTER * ( NDF__SZFRM ) FORM ! Form of the NDF
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Data type for processing
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of input NDF
      INTEGER LIML( NDIM )       ! Lower limits for the shifts
      INTEGER MDIM               ! Total no. of dimensions in input NDF
      INTEGER NDFI               ! Identifier for input NDF
      INTEGER NDFO               ! Identifier for output NDF
      INTEGER NDFS               ! Identifier for shifted input NDF
      INTEGER NERR               ! Number of errors
      INTEGER PNTRI( 1 )         ! Pointer to input mapped array
      INTEGER PNTRO( 1 )         ! Pointer to output mapped array
      INTEGER PNTRS( 1 )         ! Pointer to shifted input mapped array
      INTEGER SDIM( NDIM )       ! Indices of significant axes
      INTEGER SHIDEF( NDIM )     ! Dynamic default shifts
      INTEGER SHIFTS( NDIM )     ! Pixel shifts
      INTEGER SLBND( NDIM )      ! Lower bounds of significant axes
      INTEGER SUBND( NDIM )      ! Upper bounds of significant axes
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of input NDF
      LOGICAL VAR                ! Variance component in input NDF?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get an input NDF with exactly two significant dimensions.
      CALL KPG1_GTNDF( 'IN', 2, .TRUE., 'READ', NDFI, SDIM, SLBND,
     :                 SUBND, STATUS )

*  Find the dimensions of the input image.
      DIMS( 1 ) = SUBND( 1 ) - SLBND( 1 ) + 1
      DIMS( 2 ) = SUBND( 2 ) - SLBND( 2 ) + 1

*  Obtain the shifts, within the dimensions of the input array so that
*  there will be some overlap.
      SHIDEF( 1 ) = 1
      SHIDEF( 2 ) = 1
      LIML( 1 ) = -DIMS( 1 )
      LIML( 2 ) = -DIMS( 2 )
      CALL PAR_GRM1I( 'SHIFT', NDIM, SHIDEF, LIML, DIMS, .FALSE.,
     :                SHIFTS, STATUS )

*  Create a section of the input NDF.
      CALL NDF_BOUND( NDFI, NDF__MXDIM, LBND, UBND, MDIM, STATUS )
      CALL NDF_SECT( NDFI, MDIM, LBND, UBND, NDFS, STATUS )

*  Shift the origin of the cloned NDF by the shifts.
      CALL NDF_SHIFT( NDIM, SHIFTS, NDFS, STATUS )

*  Trim the bounds of the input and shifted NDFs' bounds to match.
      CALL NDF_MBND( 'TRIM', NDFI, NDFS, STATUS )

*  Create a new output NDF based on the first input NDF.  Propagate the
*  axis, quality, WCS and units components.
      CALL LPG_PROP( NDFI, 'Axis,Quality,Units,WCS', 'OUT', NDFO,
     :               STATUS )

*  Determine which data type to use to process the input data/variance
*  arrays and set an appropriate data type for these components in the
*  output NDF.
      CALL NDF_MTYPE( '_BYTE,_WORD,_UBYTE,_UWORD,_INTEGER,_INT64,'/
     :                /'_REAL,_DOUBLE', NDFI, NDFS, 'Data,Variance',
     :                ITYPE, DTYPE, STATUS )
      CALL NDF_STYPE( DTYPE, NDFO, 'Data,Variance', STATUS )

*  Map the input and output data arrays.
      CALL KPG1_MAP( NDFI, 'Data', ITYPE, 'READ', PNTRI, EL, STATUS )
      CALL KPG1_MAP( NDFS, 'Data', ITYPE, 'READ', PNTRS, EL, STATUS )
      CALL KPG1_MAP( NDFO, 'Data', ITYPE, 'WRITE', PNTRO, EL, STATUS )

*  Merge the bad pixel flag values for the input data arrays to see if
*  checks for bad pixels are needed.
      CALL NDF_MBAD( .TRUE., NDFI, NDFS, 'Data', .FALSE., BAD, STATUS )

*  Select the appropriate routine for the data type being processed and
*  subtract the data arrays.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL VEC_SUBB( BAD, EL, %VAL( CNF_PVAL( PNTRS( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL VEC_SUBUB( BAD, EL, %VAL( CNF_PVAL( PNTRS( 1 ) ) ),
     :                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                   IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL VEC_SUBD( BAD, EL, %VAL( CNF_PVAL( PNTRS( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL VEC_SUBI( BAD, EL, %VAL( CNF_PVAL( PNTRS( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
         CALL VEC_SUBK( BAD, EL, %VAL( CNF_PVAL( PNTRS( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL VEC_SUBR( BAD, EL, %VAL( CNF_PVAL( PNTRS( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL VEC_SUBW( BAD, EL, %VAL( CNF_PVAL( PNTRS( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL VEC_SUBUW( BAD, EL, %VAL( CNF_PVAL( PNTRS( 1 ) ) ),
     :                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                   IERR, NERR, STATUS )
      END IF

*  See if there may be bad pixels in the output data array and set the
*  output bad-pixel flag value accordingly unless the output NDF is
*  primitive.
      BAD = BAD .OR. ( NERR .NE. 0 )
      CALL NDF_FORM( NDFO, 'Data', FORM, STATUS )

      IF ( FORM .NE. 'PRIMITIVE' ) THEN
         CALL NDF_SBAD( BAD, NDFO, 'Data', STATUS )
      END IF

*  Unmap the data arrays.
      CALL NDF_UNMAP( NDFI, 'Data', STATUS )
      CALL NDF_UNMAP( NDFS, 'Data', STATUS )
      CALL NDF_UNMAP( NDFO, 'Data', STATUS )

*  If the input NDF has a variance component, then map the input and
*  output variance arrays.
      CALL NDF_STATE( NDFI, 'Variance', VAR, STATUS )
      IF ( VAR ) THEN
         CALL KPG1_MAP( NDFI, 'Variance', ITYPE, 'READ', PNTRI, EL,
     :                 STATUS )
         CALL KPG1_MAP( NDFS, 'Variance', ITYPE, 'READ', PNTRS, EL,
     :                 STATUS )
         CALL KPG1_MAP( NDFO, 'Variance', ITYPE, 'WRITE', PNTRO, EL,
     :                 STATUS )

*  See if checks for bad pixels are necessary.
         CALL NDF_MBAD( .TRUE., NDFI, NDFS, 'Variance', .FALSE., BAD,
     :                  STATUS )

*  Select the appropriate routine to add the input variance arrays.
         IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL VEC_ADDB( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRS( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL VEC_ADDUB( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                      %VAL( CNF_PVAL( PNTRS( 1 ) ) ),
     :                      %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                      IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL VEC_ADDD( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRS( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL VEC_ADDI( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRS( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
            CALL VEC_ADDK( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRS( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL VEC_ADDR( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRS( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL VEC_ADDW( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRS( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL VEC_ADDUW( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                      %VAL( CNF_PVAL( PNTRS( 1 ) ) ),
     :                      %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                      IERR, NERR, STATUS )
         END IF

*  See if bad pixels may be present in the output variance array and
*  set the output bad pixel flag value accordingly unless the output
*  NDF is primitive.
         BAD = BAD .OR. ( NERR .NE. 0 )
         CALL NDF_FORM( NDFO, 'Variance', FORM, STATUS )

         IF ( FORM .NE. 'PRIMITIVE' ) THEN
            CALL NDF_SBAD( BAD, NDFO, 'Variance', STATUS )
         END IF
      END IF

*  Obtain the output title and insert it into the output NDF.
      CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SHADOW_ERR',
     :     'SHADOW: Error enhancing an NDF data structure.',
     :   STATUS )
      END IF

      END
