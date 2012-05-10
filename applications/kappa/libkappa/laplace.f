      SUBROUTINE LAPLACE( STATUS )
*+
*  Name:
*     LAPLACE

*  Purpose:
*     Performs a Laplacian convolution as an edge detector in a 2-d NDF

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LAPLACE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine calculates the Laplacian of the supplied 2-d NDF, and
*     subtracts it from the original array to create the output NDF. The
*     subtractions can be done a specified integer number of times.
*     This operation can be approximated by a convolution with the kernel:
*
*                           -N   -N   -N
*                           -N   +8N  -N
*                           -N   -N   -N
*
*     where N is the integer number of times the Laplacian is
*     subtracted.  This convolution is used as a uni-directional edge
*     detector.  Areas where the input data array is flat become zero
*     in the output data array.

*  Usage:
*     laplace in number out title

*  ADAM Parameters:
*     IN = NDF (Read)
*        Input NDF.
*     NUMBER = _INTEGER (Read)
*        Number of Laplacians to remove. [1]
*     OUT = NDF (Write)
*        Output NDF.
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the NDF supplied for parameter IN to be used
*        instead. [!]

*  Examples:
*     laplace a 10 b
*        This subtracts 10 Laplacians from the NDF called a, to make the
*        NDF called b. NDF b inherits its title from a.

*  Related Applications:
*     KAPPA: SHADOW, MEDIAN; Figaro: ICONV3.

*  Implementation Status:
*     -  This routine correctly processes the WCS, AXIS, DATA, and VARIANCE
*     components of an NDF data structure. QUALITY is propagated.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.

*  Copyright:
*     Copyright (C) 2001, 2004 Central Laboratory of the Research
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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     26-NOV-2001 (DSB):
*        Original NDF version.
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
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIM               ! Maximum dimensionality
      PARAMETER( NDIM = 2 )

*  Local Variables:
      CHARACTER COMP*13          ! Component list
      CHARACTER DTYPE*( NDF__SZFTP )! Output data type
      CHARACTER FORM*( NDF__SZFRM ) ! Form of the NDF array
      CHARACTER ITYPE*( NDF__SZTYP )! Data type for processing
      INTEGER N                  ! Number of Laplacians to subtract
      INTEGER EL                 ! Number of mapped elements
      INTEGER DIM( NDF__MXDIM )  ! NDF dimensions
      INTEGER NBAD               ! Number of bad pixels in result array
      INTEGER INDF1              ! Identifier for 1st NDF (input)
      INTEGER INDF2              ! Identifier for 2nd NDF (output)
      INTEGER IPIN( 2 )          ! Pointers to input NDF mapped arrays
      INTEGER IPOUT( 2 )         ! Pointers to output NDF mapped arrays
      INTEGER NDIMS              ! Total number of NDF dimensions
      INTEGER SDIM( NDF__MXDIM ) ! Indices of significant NDF dimensions
      LOGICAL BAD                ! Need to check for bad pixels?
      LOGICAL VAR                ! Variance component present?

*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', INDF1, STATUS )

*  Find whether or not there are no more than two significant
*  dimensions and which ones they are.
      CALL KPG1_SDIMP( INDF1, NDIM, SDIM, STATUS )

*  Exit if an error occurred.  This is needed because the significant
*  dimensions are used as array indices.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Determine its dimensions.  Note that only two dimensions can be
*  accommodated, but of these only one need be siginifcant.  Then
*  ignore non-significant dimensions.
      CALL NDF_DIM( INDF1, NDF__MXDIM, DIM, NDIMS, STATUS )
      DIM( 1 ) = DIM( SDIM( 1 ) )
      DIM( 2 ) = DIM( SDIM( 2 ) )

*  Obtain the number of Laplacians to add.
      CALL PAR_GET0I( 'NUMBER', N, STATUS )

*  Create a new output NDF based on the input NDF.  Propagate the WCS, axis,
*  quality and units components.
      CALL LPG_PROP( INDF1, 'WCS,Axis,Quality,Units', 'OUT', INDF2,
     :               STATUS )

*  See if the input NDF has a variance component and set the list of
*  components to process accordingly.
      CALL NDF_STATE( INDF1, 'Variance', VAR, STATUS )
      IF( VAR ) THEN
         COMP = 'Data,Variance'
      ELSE
         COMP = 'Data'
      END IF

*  Determine the data type to use for processing and set the output data
*  type accordingly.
      CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_INT64,'//
     :                '_REAL,_DOUBLE', INDF1, INDF1, COMP, ITYPE, DTYPE,
     :                STATUS )
      CALL NDF_STYPE( DTYPE, INDF2, COMP, STATUS )

*  Map the input and output arrays.
      CALL NDF_MAP( INDF1, COMP, ITYPE, 'READ', IPIN, EL, STATUS )
      CALL NDF_MAP( INDF2, COMP, ITYPE, 'WRITE', IPOUT, EL, STATUS )

*  See if checks for bad pixels are needed when processing the NDF's
*  data array.
      CALL NDF_BAD( INDF1, 'Data', .FALSE., BAD, STATUS )

*  Select the appropriate routine for the data type being processed and
*  multiply the data array by the constant.
      IF( ITYPE .EQ. '_BYTE' ) THEN
         CALL KPS1_LAPLB( .FALSE., BAD, DIM( 1 ), DIM( 2 ),
     :                    %VAL( CNF_PVAL( IPIN( 1 ) ) ), N,
     :                    %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                    NBAD, STATUS )

      ELSE IF( ITYPE .EQ. '_UBYTE' ) THEN
         CALL KPS1_LAPLUB( .FALSE., BAD, DIM( 1 ), DIM( 2 ),
     :                     %VAL( CNF_PVAL( IPIN( 1 ) ) ), N,
     :                     %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                     NBAD, STATUS )

      ELSE IF( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPS1_LAPLD( .FALSE., BAD, DIM( 1 ), DIM( 2 ),
     :                    %VAL( CNF_PVAL( IPIN( 1 ) ) ), N,
     :                    %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                    NBAD, STATUS )

      ELSE IF( ITYPE .EQ. '_INTEGER' ) THEN
         CALL KPS1_LAPLI( .FALSE., BAD, DIM( 1 ), DIM( 2 ),
     :                    %VAL( CNF_PVAL( IPIN( 1 ) ) ), N,
     :                    %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                    NBAD, STATUS )

      ELSE IF( ITYPE .EQ. '_INT64' ) THEN
         CALL KPS1_LAPLK( .FALSE., BAD, DIM( 1 ), DIM( 2 ),
     :                    %VAL( CNF_PVAL( IPIN( 1 ) ) ), N,
     :                    %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                    NBAD, STATUS )

      ELSE IF( ITYPE .EQ. '_REAL' ) THEN
         CALL KPS1_LAPLR( .FALSE., BAD, DIM( 1 ), DIM( 2 ),
     :                    %VAL( CNF_PVAL( IPIN( 1 ) ) ), N,
     :                    %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                    NBAD, STATUS )

      ELSE IF( ITYPE .EQ. '_WORD' ) THEN
         CALL KPS1_LAPLW( .FALSE., BAD, DIM( 1 ), DIM( 2 ),
     :                    %VAL( CNF_PVAL( IPIN( 1 ) ) ), N,
     :                    %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                    NBAD, STATUS )

      ELSE IF( ITYPE .EQ. '_UWORD' ) THEN
         CALL KPS1_LAPLUW( .FALSE., BAD, DIM( 1 ), DIM( 2 ),
     :                    %VAL( CNF_PVAL( IPIN( 1 ) ) ), N,
     :                    %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                    NBAD, STATUS )

      END IF

*  Set the output bad pixel flag value unless the NDF is primitive.
      CALL NDF_FORM( INDF2, 'Data', FORM, STATUS )
      IF( FORM .NE. 'PRIMITIVE' ) THEN
         CALL NDF_SBAD( ( NBAD .NE. 0 ), INDF2, 'Data', STATUS )
      END IF

*  Now process any variance component.
      IF( VAR ) THEN

*  See if checks for bad pixels are needed when processing the NDF's
*  variance array.
         CALL NDF_BAD( INDF1, 'Variance', .FALSE., BAD, STATUS )

*  Select the appropriate routine for the data type being processed and
*  multiply the variance array by the squared constant.
         IF( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPS1_LAPLB( .TRUE., BAD, DIM( 1 ), DIM( 2 ),
     :                       %VAL( CNF_PVAL( IPIN( 2 ) ) ), N,
     :                       %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                       NBAD, STATUS )

         ELSE IF( ITYPE .EQ. '_UBYTE' ) THEN
            CALL KPS1_LAPLUB( .TRUE., BAD, DIM( 1 ), DIM( 2 ),
     :                        %VAL( CNF_PVAL( IPIN( 2 ) ) ), N,
     :                        %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                        NBAD, STATUS )

         ELSE IF( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_LAPLD( .TRUE., BAD, DIM( 1 ), DIM( 2 ),
     :                       %VAL( CNF_PVAL( IPIN( 2 ) ) ), N,
     :                       %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                       NBAD, STATUS )

         ELSE IF( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPS1_LAPLI( .TRUE., BAD, DIM( 1 ), DIM( 2 ),
     :                       %VAL( CNF_PVAL( IPIN( 2 ) ) ), N,
     :                       %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                       NBAD, STATUS )

         ELSE IF( ITYPE .EQ. '_INT64' ) THEN
            CALL KPS1_LAPLK( .TRUE., BAD, DIM( 1 ), DIM( 2 ),
     :                       %VAL( CNF_PVAL( IPIN( 2 ) ) ), N,
     :                       %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                       NBAD, STATUS )

         ELSE IF( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_LAPLR( .TRUE., BAD, DIM( 1 ), DIM( 2 ),
     :                       %VAL( CNF_PVAL( IPIN( 2 ) ) ), N,
     :                       %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                       NBAD, STATUS )

         ELSE IF( ITYPE .EQ. '_WORD' ) THEN
            CALL KPS1_LAPLW( .TRUE., BAD, DIM( 1 ), DIM( 2 ),
     :                       %VAL( CNF_PVAL( IPIN( 2 ) ) ), N,
     :                       %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                       NBAD, STATUS )

         ELSE IF( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPS1_LAPLUW( .TRUE., BAD, DIM( 1 ), DIM( 2 ),
     :                        %VAL( CNF_PVAL( IPIN( 2 ) ) ), N,
     :                        %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                        NBAD, STATUS )

         END IF

*  Set the output bad pixel flag value unless the NDF is primitive.
         CALL NDF_FORM( INDF2, 'Variance', FORM, STATUS )
         IF( FORM .NE. 'PRIMITIVE' ) THEN
            CALL NDF_SBAD( ( NBAD .NE. 0 ), INDF2, 'Variance', STATUS )
         END IF

      END IF

*  Obtain a new title for the output NDF.
      CALL NDF_CINP( 'TITLE', INDF2, 'Title', STATUS )

*  Tidy up.
 999  CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report context information.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'LAPLACE_ERR', 'LAPLACE: Error applying a '//
     :                 'Laplacian convolution to an NDF.',
     :                 STATUS )
      END IF

      END
