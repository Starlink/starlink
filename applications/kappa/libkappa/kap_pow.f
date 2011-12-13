      SUBROUTINE KAP_POW( STATUS )
*+
*  Name:
*     KAP_POW

*  Purpose:
*     Takes the specified power of each pixel of an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL KAP_POW( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine copies the supplied input NDF, raising each value in
*     the DATA array to the specified power. The VARIANCE component, if
*     present, is modified appropriately. Negative data values will only
*     generate good output pixels when the power is an integer.

*  Usage:
*     pow in power out

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input NDF structure.
*     OUT = NDF (Write)
*        The output NDF structure.
*     POWER = _DOUBLE (Read)
*        The power.
*     TITLE = LITERAL (Read)
*        A title for the output NDF.  A null value will cause the title
*        of the NDF supplied for parameter IN to be used instead.
*        [!]

*  Examples:
*     pow m51 2 m51sq
*        Square all values in the NDF called m51, and store the results in the
*        NDF called m51sq.

*  Related Applications:
*     KAPPA: ADD, CADD, CMULT, CDIV, CSUB, DIV, MATHS, MULT, SUB.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     LABEL, TITLE, UNITS, HISTORY, WCS and VARIANCE components of an NDF
*     data structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using single-precision floating point, or double
*     precision, if appropriate, but the numeric type of the input pixels
*     is preserved in the output NDF.

*  Copyright:
*     Copyright (C) 2001, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     30-NOV-2001 (DSB):
*        Original NDF version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
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
      CHARACTER COMP*13          ! Component list
      CHARACTER DTYPE*( NDF__SZFTP ) ! Output data type
      CHARACTER FORM*( NDF__SZFRM )  ! Form of the NDF array
      CHARACTER ITYPE*( NDF__SZTYP ) ! Data type for processing
      DOUBLE PRECISION POWER     ! Constant for multiplication
      INTEGER EL                 ! Number of mapped elements
      INTEGER NBAD( 2 )          ! Number of bad pixels in results arrays
      INTEGER INDF1              ! Identifier for 1st NDF (input)
      INTEGER INDF2              ! Identifier for 2nd NDF (output)
      INTEGER IPIN( 2 )          ! Pointers to 1st NDF mapped arrays
      INTEGER IPOUT( 2 )         ! Pointers to 2nd NDF mapped arrays
      LOGICAL BAD                ! Need to check for bad pixels?
      LOGICAL VAR                ! Variance component present?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', INDF1, STATUS )

*  Obtain the power.
      CALL PAR_GET0D( 'POWER', POWER, STATUS )

*  Create a new output NDF based on the input NDF.  Propagate the WCS, axis,
*  quality and units components.
      CALL LPG_PROP( INDF1, 'WCS,Axis,Quality,Units', 'OUT', INDF2,
     :               STATUS )

*  See if the input NDF has a variance component and set the list of
*  components to process accordingly.
      CALL NDF_STATE( INDF1, 'Variance', VAR, STATUS )
      IF ( VAR ) THEN
         COMP = 'Data,Variance'
      ELSE
         COMP = 'Data'
      END IF

*  Determine the data type to use for processing and set the output data
*  type accordingly.
      CALL NDF_MTYPE( '_REAL,_DOUBLE', INDF1, INDF1, COMP, ITYPE, DTYPE,
     :                STATUS )
      CALL NDF_STYPE( DTYPE, INDF2, COMP, STATUS )

*  Map the input and output arrays.
      CALL NDF_MAP( INDF1, COMP, ITYPE, 'READ', IPIN, EL, STATUS )
      CALL NDF_MAP( INDF2, COMP, ITYPE, 'WRITE', IPOUT, EL, STATUS )

*  See if checks for bad pixels are needed when processing the NDF's
*  data or variance array.
      CALL NDF_BAD( INDF1, COMP, .FALSE., BAD, STATUS )

*  Select the appropriate routine for the data type being processed and
*  create the output values.
      IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_POWD( BAD, VAR, POWER, EL,
     :                   %VAL( CNF_PVAL( IPIN( 1 ) ) ),
     :                   %VAL( CNF_PVAL( IPIN( 2 ) ) ),
     :                   %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                   %VAL( CNF_PVAL( IPOUT( 2 ) ) ), NBAD, STATUS )

      ELSE
         CALL KPG1_POWR( BAD, VAR, POWER, EL,
     :                   %VAL( CNF_PVAL( IPIN( 1 ) ) ),
     :                   %VAL( CNF_PVAL( IPIN( 2 ) ) ),
     :                   %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                   %VAL( CNF_PVAL( IPOUT( 2 ) ) ), NBAD, STATUS )

      END IF

*  Set the output bad pixel flag value unless the NDF is primitive.
      CALL NDF_FORM( INDF2, 'Data', FORM, STATUS )
      IF ( FORM .NE. 'PRIMITIVE' ) THEN
         CALL NDF_SBAD( ( NBAD( 1 ) .NE. 0 ), INDF2, 'Data', STATUS )
      END IF

      IF( VAR ) THEN
         CALL NDF_FORM( INDF2, 'Variance', FORM, STATUS )
         IF ( FORM .NE. 'PRIMITIVE' ) THEN
            CALL NDF_SBAD( ( NBAD( 2 ) .NE. 0 ), INDF2, 'Variance',
     :                     STATUS )
         END IF
      END IF


*  Obtain a new title for the output NDF.
      CALL NDF_CINP( 'TITLE', INDF2, 'Title', STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POW_ERR', 'POW: Error raising the pixel '//
     :                 'values in an NDF to a given power.', STATUS )
      END IF

      END
