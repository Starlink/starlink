      SUBROUTINE CMULT( STATUS )
*+
*  Name:
*     CMULT

*  Purpose:
*     Multiplies an NDF by a scalar.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CMULT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application multiplies each pixel of an NDF by a scalar
*     (constant) value to produce a new NDF.

*  Usage:
*     cmult in scalar out

*  ADAM Parameters:
*     IN = NDF (Read)
*        Input NDF structure whose pixels are to be multiplied by a
*        scalar.
*     OUT = NDF (Write)
*        Output NDF structure.
*     SCALAR = _DOUBLE (Read)
*        The value by which the NDF's pixels are to be multiplied.
*     TITLE = LITERAL (Read)
*        A title for the output NDF.  A null value will cause the title
*        of the NDF supplied for Parameter IN to be used instead.
*        [!]

*  Examples:
*     cmult a 12.5 b
*        Multiplies all the pixels in the NDF called a by the constant
*        value 12.5 to produce a new NDF called b.
*     cmult in=rawdata out=newdata scalar=-19
*        Multiplies all the pixels in the NDF called rawdata by -19 to
*        give newdata.

*  Related Applications:
*     KAPPA: ADD, CADD, CDIV, CSUB, DIV, MATHS, MULT, SUB.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     LABEL, TITLE, UNITS, HISTORY, WCS and VARIANCE components of an NDF
*     data structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is carried out using the appropriate floating-point type, but the
*     numeric type of the input pixels is preserved in the output NDF.

*  Copyright:
*     Copyright (C) 1990-1991 Science & Engineering Research Council.
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
*     17-APR-1990 (RFWS):
*        Original version.
*     11-MAR-1991 (RFWS):
*        Finished writing the new prologue and fixed a minor typo in
*        the code.
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
      CHARACTER * ( 13 ) COMP    ! Component list
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Output data type
      CHARACTER * ( NDF__SZFRM ) FORM ! Form of the NDF array
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Data type for processing
      DOUBLE PRECISION CONST     ! Constant for multiplication
      INTEGER EL                 ! Number of mapped elements
      INTEGER NBAD               ! Number of bad pixels in result array
      INTEGER NDF1               ! Identifier for 1st NDF (input)
      INTEGER NDF2               ! Identifier for 2nd NDF (output)
      INTEGER PNTR1( 2 )         ! Pointers to 1st NDF mapped arrays
      INTEGER PNTR2( 2 )         ! Pointers to 2nd NDF mapped arrays
      LOGICAL BAD                ! Need to check for bad pixels?
      LOGICAL VAR                ! Variance component present?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', NDF1, STATUS )

*  Obtain the scalar value for multiplication.
      CALL PAR_GET0D( 'SCALAR', CONST, STATUS )

*  Create a new output NDF based on the input NDF.  Propagate the WCS, axis,
*  quality and units components.
      CALL LPG_PROP( NDF1, 'WCS,Axis,Quality,Units', 'OUT', NDF2,
     :               STATUS )

*  See if the input NDF has a variance component and set the list of
*  components to process accordingly.
      CALL NDF_STATE( NDF1, 'Variance', VAR, STATUS )
      IF ( VAR ) THEN
         COMP = 'Data,Variance'
      ELSE
         COMP = 'Data'
      END IF

*  Determine the data type to use for processing and set the output data
*  type accordingly.
      CALL NDF_MTYPE(
     :   '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_INT64,_REAL,_DOUBLE',
     :                NDF1, NDF1, COMP, ITYPE, DTYPE, STATUS )
      CALL NDF_STYPE( DTYPE, NDF2, COMP, STATUS )

*  Map the input and output arrays.
      CALL KPG1_MAP( NDF1, COMP, ITYPE, 'READ', PNTR1, EL, STATUS )
      CALL KPG1_MAP( NDF2, COMP, ITYPE, 'WRITE', PNTR2, EL, STATUS )

*  See if checks for bad pixels are needed when processing the NDF's
*  data array.
      CALL NDF_BAD( NDF1, 'Data', .FALSE., BAD, STATUS )

*  Select the appropriate routine for the data type being processed and
*  multiply the data array by the constant.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_CMULB( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                    CONST,
     :                    %VAL( CNF_PVAL( PNTR2( 1 ) ) ), NBAD, STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_CMULUB( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                     CONST,
     :                     %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                     NBAD, STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_CMULD( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                    CONST,
     :                    %VAL( CNF_PVAL( PNTR2( 1 ) ) ), NBAD, STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_CMULI( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                    CONST,
     :                    %VAL( CNF_PVAL( PNTR2( 1 ) ) ), NBAD, STATUS )

      ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
         CALL KPG1_CMULK( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                    CONST,
     :                    %VAL( CNF_PVAL( PNTR2( 1 ) ) ), NBAD, STATUS )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPG1_CMULR( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                    CONST,
     :                    %VAL( CNF_PVAL( PNTR2( 1 ) ) ), NBAD, STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL KPG1_CMULW( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                    CONST,
     :                    %VAL( CNF_PVAL( PNTR2( 1 ) ) ), NBAD, STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_CMULUW( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                     CONST,
     :                     %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                     NBAD, STATUS )

      END IF

*  Set the output bad pixel flag value unless the NDF is primitive.
      CALL NDF_FORM( NDF2, 'Data', FORM, STATUS )

      IF ( FORM .NE. 'PRIMITIVE' ) THEN
         CALL NDF_SBAD( ( NBAD .NE. 0 ), NDF2, 'Data', STATUS )
      END IF

*  If there is a variance component to be processed, then square the
*  constant to be used for multiplication.
      IF ( VAR ) THEN
         CONST = CONST * CONST

*  See if checks for bad pixels are needed when processing the NDF's
*  variance array.
         CALL NDF_BAD( NDF1, 'Variance', .FALSE., BAD, STATUS )

*  Select the appropriate routine for the data type being processed and
*  multiply the variance array by the squared constant.
         IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPG1_CMULB( BAD, EL, %VAL( CNF_PVAL( PNTR1( 2 ) ) ),
     :                       CONST,
     :                       %VAL( CNF_PVAL( PNTR2( 2 ) ) ),
     :                       NBAD, STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL KPG1_CMULUB( BAD, EL, %VAL( CNF_PVAL( PNTR1( 2 ) ) ),
     :                        CONST,
     :                        %VAL( CNF_PVAL( PNTR2( 2 ) ) ),
     :                        NBAD, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_CMULD( BAD, EL, %VAL( CNF_PVAL( PNTR1( 2 ) ) ),
     :                       CONST,
     :                       %VAL( CNF_PVAL( PNTR2( 2 ) ) ),
     :                       NBAD, STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPG1_CMULI( BAD, EL, %VAL( CNF_PVAL( PNTR1( 2 ) ) ),
     :                       CONST,
     :                       %VAL( CNF_PVAL( PNTR2( 2 ) ) ),
     :                       NBAD, STATUS )

         ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
            CALL KPG1_CMULK( BAD, EL, %VAL( CNF_PVAL( PNTR1( 2 ) ) ),
     :                       CONST,
     :                       %VAL( CNF_PVAL( PNTR2( 2 ) ) ),
     :                       NBAD, STATUS )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_CMULR( BAD, EL, %VAL( CNF_PVAL( PNTR1( 2 ) ) ),
     :                       CONST,
     :                       %VAL( CNF_PVAL( PNTR2( 2 ) ) ),
     :                       NBAD, STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL KPG1_CMULW( BAD, EL, %VAL( CNF_PVAL( PNTR1( 2 ) ) ),
     :                       CONST,
     :                       %VAL( CNF_PVAL( PNTR2( 2 ) ) ),
     :                       NBAD, STATUS )

         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPG1_CMULUW( BAD, EL, %VAL( CNF_PVAL( PNTR1( 2 ) ) ),
     :                        CONST,
     :                        %VAL( CNF_PVAL( PNTR2( 2 ) ) ),
     :                        NBAD, STATUS )

         END IF

*  Set the output bad pixel flag value unless the NDF is primitive.
         CALL NDF_FORM( NDF2, 'Variance', FORM, STATUS )

         IF ( FORM .NE. 'PRIMITIVE' ) THEN
            CALL NDF_SBAD( ( NBAD .NE. 0 ), NDF2, 'Variance', STATUS )
         END IF
      END IF

*  Obtain a new title for the output NDF.
      CALL NDF_CINP( 'TITLE', NDF2, 'Title', STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CMULT_ERR',
     :   'CMULT: Error multiplying an NDF data structure by a scalar.',
     :   STATUS )
      END IF

      END
