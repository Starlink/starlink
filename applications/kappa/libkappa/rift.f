      SUBROUTINE RIFT( STATUS )
*+
*  Name:
*     RIFT

*  Purpose:
*     Adds a scalar to a section of an NDF data structure to correct
*     rift-valley defects.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL RIFT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The routine adds a scalar (i.e. constant) value to each pixel of
*     an NDF's data array within a sub-section to produce a new NDF
*     data structure.

*  Usage:
*     rift in scalar out section

*  ADAM Parameters:
*     IN = NDF (Read)
*        Input NDF data structure, to which the value is to be added.
*     OUT = NDF (Write)
*        Output NDF data structure.
*     SCALAR = _DOUBLE (Read)
*        The value to be added to the NDF's data array within the
*        section.
*     SECTION = LITERAL (Read)
*        The pixels to which a scalar is to be added.  This is defined
*        as an NDF section, so that ranges can be defined along any
*        axis, and be given as pixel indices or axis (data)
*        co-ordinates.  So for example "3,4,5" would select the pixel
*        at (3,4,5); "3:5," would select all elements in columns 3 to
*        5; ",4" selects line 4.  See "NDF Sections" in SUN/95, or the
*        online documentation for details.
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the NDF supplied for parameter IN to be used
*        instead. [!]

*  Examples:
*     rift aa 10.7 bb "100:105" 20
*        This adds 10 in the columns 100 to 105 in the data array of
*        the NDF called aa and stores the result in the NDF called bb.
*        In other respects bb is a copy of aa.
*     rift cubein -100 cubeout ",,4"
*        This adds -100 to all values in the fourth plane of the data
*        array of the NDF called cubein and stores the result in the
*        NDF called cubeout.  In other respects cubeout is a copy of
*        cubeout.
*     rift in=aa scalar=2 out=bb section="-10:5,200~9"
*        This adds 2 to the rectangular section between columns -10 to
*        5 and lines 196 to 204 of the data array of the NDF called aa
*        and stores the result in the NDF called bb.  In other respects
*        bb is a copy of aa.

*  Notes:
*     For similar operations performed on a subset, use the appropriate
*     application to process the relevant section and then run PASTE to
*     paste the result back into the full array.

*  Related Applications:
*     KAPPA: CADD, CHPIX, GLITCH, PASTE, SEGMENT, ZAPLIN; Figaro: CSET,
*     ICSET, NCSET, TIPPEX.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of an NDF
*     data structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  The bad-pixel flag is set to TRUE if undefined values are
*     created during the arithmetic.
*     -  All non-complex numeric data types can be handled.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     1991 October 31 (MJC):
*        Original version based on RFWS's CADD.
*     1995 April 28 (MJC):
*        Used the SECTION parameter instead of pixel bounds.  Made Usage
*        and Examples Lowercase.  Added Related Applications, Examples,
*        and Implementation Status.
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
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      LOGICAL BAD                ! Need to check for bad pixels?
      DOUBLE PRECISION CONST     ! Constant to be added
      INTEGER EL                 ! Number of mapped elements
      CHARACTER * ( NDF__SZFRM ) FORM ! Form of the ARRAY
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Data type for processing
      CHARACTER * ( DAT__SZLOC ) LOCI ! Locator to the input NDF
      CHARACTER * ( DAT__SZLOC ) LOCO ! Locator to the output NDF
      INTEGER NDFI               ! Identifier for 1st NDF (input)
      INTEGER NDFO               ! Identifier for 2nd NDF (output)
      INTEGER NDFSI              ! Identifier for input NDF section
      INTEGER NDFSO              ! Identifier for input NDF section
      INTEGER NCSECT             ! Number of characters in section
      INTEGER NERR               ! Number of errors
      INTEGER PNTR1( 1 )         ! Pointer to 1st NDF mapped array
      INTEGER PNTR2( 1 )         ! Pointer to 2nd NDF mapped array
      CHARACTER * ( 80 ) SECT    ! Section specifier

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the NDFs.
*  ================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Obtain the scalar value to be added.
      CALL PAR_GET0D( 'SCALAR', CONST, STATUS )

*  Create a new output NDF based on the input NDF. Propagate the data,
*  axis, quality, units, WCS and variance components.
      CALL LPG_PROP( NDFI, 'Data,Axis,Quality,Units,Variance,WCS',
     :               'OUT', NDFO, STATUS )

*  Determine which data type to use to process the input data array.
      CALL NDF_TYPE( NDFI, 'Data', ITYPE, STATUS )

*  Create and map new sections containing the 'rift valley'.
*  =========================================================

*  Obtain the section specification.
      CALL PAR_GET0C( 'SECTION', SECT, STATUS )
      NCSECT = CHR_LEN( SECT )

*  Obtain locators to the NDFs.
      CALL NDF_LOC( NDFI, 'Read', LOCI, STATUS )
      CALL NDF_LOC( NDFO, 'Write', LOCO, STATUS )

*  Create the section in the NDFs.
      CALL NDF_FIND( LOCI, '(' // SECT( :NCSECT ) // ')', NDFSI,
     :               STATUS )
      CALL NDF_FIND( LOCO, '(' // SECT( :NCSECT ) // ')', NDFSO,
     :               STATUS )

*  Map the input and output data arrays.
      CALL KPG1_MAP( NDFSI, 'Data', ITYPE, 'READ', PNTR1, EL, STATUS )
      CALL KPG1_MAP( NDFSO, 'Data', ITYPE, 'WRITE', PNTR2, EL, STATUS )

*  Add the constant.
*  =================

*  See if checks for bad pixels are needed.
      CALL NDF_BAD( NDFI, 'Data', .FALSE., BAD, STATUS )

*  Select the appropriate routine for the data type being processed and
*  add the constant to the data array.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_CADDB( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                    CONST,
     :                    %VAL( CNF_PVAL( PNTR2( 1 ) ) ), NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_CADDUB( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                     CONST,
     :                     %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                     NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_CADDD( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                    CONST,
     :                    %VAL( CNF_PVAL( PNTR2( 1 ) ) ), NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_CADDI( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                    CONST,
     :                    %VAL( CNF_PVAL( PNTR2( 1 ) ) ), NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
         CALL KPG1_CADDK( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                    CONST,
     :                    %VAL( CNF_PVAL( PNTR2( 1 ) ) ), NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPG1_CADDR( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                    CONST,
     :                    %VAL( CNF_PVAL( PNTR2( 1 ) ) ), NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL KPG1_CADDW( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                    CONST,
     :                    %VAL( CNF_PVAL( PNTR2( 1 ) ) ), NERR, STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_CADDUW( BAD, EL, %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                     CONST,
     :                     %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                     NERR, STATUS )
      END IF

*  See if there may be bad pixels in the output data array and set the
*  output bad pixel flag value accordingly unless the output NDF is
*  primitive.
      BAD = BAD .OR. ( NERR .NE. 0 )
      CALL NDF_FORM( NDFO, 'Data', FORM, STATUS )

      IF ( FORM .NE. 'PRIMITIVE' ) THEN
         CALL NDF_SBAD( BAD, NDFO, 'Data', STATUS )
      END IF

*  Obtain a new title for the output NDF.
      CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'RIFT_ERR',
     :     'RIFT: Error adding a scalar value to a subset of an NDF '/
     :     /'data structure.', STATUS )
      END IF

      END
