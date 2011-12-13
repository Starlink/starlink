      SUBROUTINE ADD( STATUS )
*+
*  Name:
*     ADD

*  Purpose:
*     Adds two NDF data structures.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ADD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The routine adds two NDF data structures pixel-by-pixel to produce
*     a new NDF.

*  Usage:
*     add in1 in2 out

*  ADAM Parameters:
*     IN1 = NDF (Read)
*        First NDF to be added.
*     IN2 = NDF (Read)
*        Second NDF to be added.
*     OUT = NDF (Write)
*        Output NDF to contain the sum of the two input NDFs.
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the NDF supplied for parameter IN1 to be used
*        instead. [!]

*  Examples:
*     add a b c
*        This adds the NDF called b to the NDF called a, to make the
*        NDF called c.  NDF c inherits its title from a.
*     add out=c in1=a in2=b title="Co-added image"
*        This adds the NDF called b to the NDF called a, to make the
*        NDF called c.  NDF c has the title "Co-added image".

*  Notes:
*     - The output NDF contains the simple sum of the input NDFs.
*     Inparticularly, the input variances are not used as weights. For a
*     weighted co-addition, use CCDPACK:MAKEMOS.
*     - If the two input NDFs have different pixel-index bounds, then
*     they will be trimmed to match before being added.  An error will
*     result if they have no pixels in common.

*  Related Applications:
*     KAPPA: CADD, CDIV, CMULT, CSUB, DIV, MATHS, MULT, SUB.

*  Implementation Status:
*     -  This routine correctly processes the WCS, AXIS, DATA, QUALITY,
*     LABEL, TITLE, HISTORY, and VARIANCE components of an NDF data
*     structure and propagates all extensions.
*     -  The UNITS component is propagated only if it has the same value
*     in both input NDFs.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.

*  Copyright:
*     Copyright (C) 1990, 1992 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2007 Science & Technology Facilities Council.  All
*     Rights Reserved.

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
*     4-APR-1990 (RFWS):
*        Original version, derived from the previous non-NDF routine of
*        the same name.
*     1992 January 15 (MJC):
*        Added Usage and Examples items.
*     1995 September 12 (MJC):
*        Title inherited by default.  Usage and examples to lowercase.
*        Added Related Applications.
*     5-JUN-1998 (DSB):
*        Added propagation of WCS component.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     14-SEP-2004 (DSB):
*        Mods to prologue to note that variances are not used as weights.
*     5-MAY-2007 (DSB):
*        Propagate the Unit value if it is the same in both NDFs.
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
      CHARACTER * ( NDF__SZFRM ) FORM ! Form of the ARRAY
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Data type for processing
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Data type for output components
      CHARACTER CLIST*30         ! List of NDF components to copy
      CHARACTER UNIT1*30         ! Units string from NDF1
      CHARACTER UNIT2*30         ! Units string from NDF2
      INTEGER EL                 ! Number of mapped elements
      INTEGER IERR               ! Position of first error (dummy)
      INTEGER NDF1               ! Identifier for 1st NDF (input)
      INTEGER NDF2               ! Identifier for 2nd NDF (input)
      INTEGER NDF3               ! Identifier for 3rd NDF (output)
      INTEGER NERR               ! Number of errors
      INTEGER PNTR1( 1 )         ! Pointer to 1st NDF mapped array
      INTEGER PNTR2( 1 )         ! Pointer to 2nd NDF mapped array
      INTEGER PNTR3( 1 )         ! Pointer to 3rd NDF mapped array
      LOGICAL BAD                ! Need to check for bad pixels?
      LOGICAL VAR1               ! Variance component in 1st input NDF?
      LOGICAL VAR2               ! Variance component in 2nd input NDF?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain identifiers for the two input NDFs.
      CALL LPG_ASSOC( 'IN1', 'READ', NDF1, STATUS )
      CALL LPG_ASSOC( 'IN2', 'READ', NDF2, STATUS )

*  Trim the input pixel-index bounds to match.
      CALL NDF_MBND( 'TRIM', NDF1, NDF2, STATUS )

*  See if the Units are the same.
      UNIT1 = ' '
      CALL NDF_CGET( NDF1, 'Unit', UNIT1, STATUS )
      UNIT2 = ' '
      CALL NDF_CGET( NDF2, 'Unit', UNIT2, STATUS )

*  Determine the list of components to be propagated from NDF1. We only
*  propagate the Unit component if it is the same in both input NDFs.
      IF( UNIT1 .EQ. UNIT2 .AND. UNIT1 .NE. ' ' ) THEN
         CLIST = 'WCS,Axis,Quality,Unit'
      ELSE
         CLIST = 'WCS,Axis,Quality'
      END IF

*  Create a new output NDF based on the first input NDF. Propagate the
*  WCS, axis and quality components.
      CALL LPG_PROP( NDF1, CLIST, 'OUT', NDF3, STATUS )

*  Determine which data type to use to process the input data/variance
*  arrays and set an appropriate data type for these components in the
*  output NDF.
      CALL NDF_MTYPE(
     : '_BYTE,_WORD,_UBYTE,_UWORD,_INTEGER,_REAL,_DOUBLE',
     :                NDF1, NDF2, 'Data,Variance', ITYPE, DTYPE,
     :                STATUS )
      CALL NDF_STYPE( DTYPE, NDF3, 'Data,Variance', STATUS )

*  Map the input and output data arrays.
      CALL KPG1_MAP( NDF1, 'Data', ITYPE, 'READ', PNTR1, EL, STATUS )
      CALL KPG1_MAP( NDF2, 'Data', ITYPE, 'READ', PNTR2, EL, STATUS )
      CALL KPG1_MAP( NDF3, 'Data', ITYPE, 'WRITE', PNTR3, EL, STATUS )

*  Merge the bad pixel flag values for the input data arrays to see if
*  checks for bad pixels are needed.
      CALL NDF_MBAD( .TRUE., NDF1, NDF2, 'Data', .FALSE., BAD, STATUS )

*  Select the appropriate routine for the data type being processed and
*  add the data arrays.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL VEC_ADDB( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTR3( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL VEC_ADDUB( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                   %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                   %VAL( CNF_PVAL( PNTR3( 1 ) ) ),
     :                   IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL VEC_ADDD( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTR3( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL VEC_ADDI( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTR3( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL VEC_ADDR( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTR3( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL VEC_ADDW( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                  %VAL( CNF_PVAL( PNTR3( 1 ) ) ),
     :                  IERR, NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL VEC_ADDUW( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                   %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                   %VAL( CNF_PVAL( PNTR3( 1 ) ) ),
     :                   IERR, NERR, STATUS )
      END IF

*  See if there may be bad pixels in the output data array and set the
*  output bad pixel flag value accordingly unless the output NDF is
*  primitive.
      BAD = BAD .OR. ( NERR .NE. 0 )
      CALL NDF_FORM( NDF3, 'Data', FORM, STATUS )

      IF ( FORM .NE. 'PRIMITIVE' ) THEN
         CALL NDF_SBAD( BAD, NDF3, 'Data', STATUS )
      END IF

*  Unmap the data arrays.
      CALL NDF_UNMAP( NDF1, 'Data', STATUS )
      CALL NDF_UNMAP( NDF2, 'Data', STATUS )
      CALL NDF_UNMAP( NDF3, 'Data', STATUS )

*  If both input NDFs have a variance component, then map the input and
*  output variance arrays.
      CALL NDF_STATE( NDF1, 'Variance', VAR1, STATUS )
      CALL NDF_STATE( NDF2, 'Variance', VAR2, STATUS )
      IF ( VAR1 .AND. VAR2 ) THEN
         CALL KPG1_MAP( NDF1, 'Variance', ITYPE, 'READ', PNTR1, EL,
     :                 STATUS )
         CALL KPG1_MAP( NDF2, 'Variance', ITYPE, 'READ', PNTR2, EL,
     :                 STATUS )
         CALL KPG1_MAP( NDF3, 'Variance', ITYPE, 'WRITE', PNTR3, EL,
     :                 STATUS )

*  See if checks for bad pixels are necessary.
         CALL NDF_MBAD( .TRUE., NDF1, NDF2, 'Variance', .FALSE., BAD,
     :                  STATUS )

*  Select the appropriate routine to add the input variance arrays.
         IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL VEC_ADDB( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTR3( 1 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL VEC_ADDUB( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                      %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                      %VAL( CNF_PVAL( PNTR3( 1 ) ) ),
     :                      IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL VEC_ADDD( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTR3( 1 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL VEC_ADDI( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTR3( 1 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL VEC_ADDR( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTR3( 1 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL VEC_ADDW( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                     %VAL( CNF_PVAL( PNTR3( 1 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL VEC_ADDUW( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                      %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                      %VAL( CNF_PVAL( PNTR3( 1 ) ) ),
     :                      IERR, NERR, STATUS )
         END IF

*  See if bad pixels may be present in the output variance array and
*  set the output bad pixel flag value accordingly unless the output
*  NDF is primitive.
         BAD = BAD .OR. ( NERR .NE. 0 )
         CALL NDF_FORM( NDF3, 'Variance', FORM, STATUS )

         IF ( FORM .NE. 'PRIMITIVE' ) THEN
            CALL NDF_SBAD( BAD, NDF3, 'Variance', STATUS )
         END IF
      END IF

*  Obtain the output title and insert it into the output NDF.
      CALL NDF_CINP( 'TITLE', NDF3, 'Title', STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ADD_ERR',
     :   'ADD: Error adding two NDF data structures.', STATUS )
      END IF

      END
