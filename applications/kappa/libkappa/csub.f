      SUBROUTINE CSUB( STATUS )
*+
*  Name:
*     CSUB

*  Purpose:
*     Subtracts a scalar from an NDF data structure.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CSUB( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The routine subtracts a scalar (i.e. constant) value from each
*     pixel of an NDF's data array to produce a new NDF data structure.

*  Usage:
*     csub in scalar out

*  ADAM Parameters:
*     IN = NDF (Read)
*        Input NDF data structure, from which the value is to be
*        subtracted.
*     OUT = NDF (Write)
*        Output NDF data structure.
*     SCALAR = _DOUBLE (Read)
*        The value to be subtracted from the NDF's data array.
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the NDF supplied for parameter IN to be used
*        instead. [!]

*  Examples:
*     csub a 10 b
*        This subtracts ten from the NDF called a, to make the NDF
*        called b.  NDF b inherits its title from a.
*     csub title="HD123456" out=b in=a scalar=21.9
*        This subtracts 21.9 from the NDF called a, to make the NDF
*        called b.  NDF b has the title "HD123456".

*  Related Applications:
*     KAPPA: ADD, CADD, CDIV, CMULT, DIV, MATHS, MULT, SUB.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     LABEL, TITLE, UNITS, HISTORY, WCS and VARIANCE components of an NDF
*     data structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.

*  Copyright:
*     Copyright (C) 1990, 1992 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     12-APR-1990 (RFWS):
*        Original version.
*     1992 January 15 (MJC):
*        Added Usage and Examples items.
*     1995 September 12 (MJC):
*        Title inherited by default.  Usage and examples to lowercase.
*        Added Related Applications.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
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
      CHARACTER * ( NDF__SZFRM ) FORM ! Form of the NDF array
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Data type for processing
      DOUBLE PRECISION CONST     ! Constant to be subtracted
      INTEGER EL                 ! Number of mapped elements
      INTEGER NDF1               ! Identifier for 1st NDF (input)
      INTEGER NDF2               ! Identifier for 2nd NDF (output)
      INTEGER NERR               ! Number of errors
      INTEGER PNTR1( 1 )         ! Pointer to 1st NDF mapped array
      INTEGER PNTR2( 1 )         ! Pointer to 2nd NDF mapped array
      LOGICAL BAD                ! Need to check for bad pixels?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', NDF1, STATUS )

*  Obtain the scalar value to be subtracted.
      CALL PAR_GET0D( 'SCALAR', CONST, STATUS )

*  Create a new output NDF based on the input NDF. Propagate the WCS, axis,
*  quality, units and variance components.
      CALL LPG_PROP( NDF1, 'WCS,Axis,Quality,Units,Variance', 'OUT',
     :               NDF2, STATUS )

*  Determine which data type to use to process the input data array.
      CALL NDF_TYPE( NDF1, 'Data', ITYPE, STATUS )

*  Map the input and output data arrays.
      CALL KPG1_MAP( NDF1, 'Data', ITYPE, 'READ', PNTR1, EL, STATUS )
      CALL KPG1_MAP( NDF2, 'Data', ITYPE, 'WRITE', PNTR2, EL, STATUS )

*  See if checks for bad pixels are needed.
      CALL NDF_BAD( NDF1, 'Data', .FALSE., BAD, STATUS )

*  Select the appropriate routine for the data type being processed and
*  subtract the constant from the data array.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_CSUBB( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                    CONST,
     :                    %VAL( CNF_PVAL( PNTR2( 1 ) ) ), NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_CSUBUB( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                     CONST,
     :                     %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                     NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_CSUBD( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                    CONST,
     :                    %VAL( CNF_PVAL( PNTR2( 1 ) ) ), NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_CSUBI( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                    CONST,
     :                    %VAL( CNF_PVAL( PNTR2( 1 ) ) ), NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
         CALL KPG1_CSUBK( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                    CONST,
     :                    %VAL( CNF_PVAL( PNTR2( 1 ) ) ), NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPG1_CSUBR( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                    CONST,
     :                    %VAL( CNF_PVAL( PNTR2( 1 ) ) ), NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL KPG1_CSUBW( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                    CONST,
     :                    %VAL( CNF_PVAL( PNTR2( 1 ) ) ), NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_CSUBUW( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                     CONST,
     :                     %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                     NERR, STATUS )
      END IF

*  See if there may be bad pixels in the output data array and set the
*  output bad pixel flag value accordingly unless the output NDF is
*  primitive.
      BAD = BAD .OR. ( NERR .NE. 0 )
      CALL NDF_FORM( NDF2, 'Data', FORM, STATUS )

      IF ( FORM .NE. 'PRIMITIVE' ) THEN
         CALL NDF_SBAD( BAD, NDF2, 'Data', STATUS )
      END IF

*  Obtain a new title for the output NDF.
      CALL NDF_CINP( 'TITLE', NDF2, 'Title', STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CSUB_ERR',
     :   'CSUB: Error subtracting a scalar value from an NDF data ' //
     :   'structure.', STATUS )
      END IF

      END
