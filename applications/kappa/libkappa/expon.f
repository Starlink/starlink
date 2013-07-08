      SUBROUTINE EXPON( STATUS )
*+
*  Name:
*     EXPON

*  Purpose:
*     Takes the exponential (specified base) of an NDF data structure.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL EXPON( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine takes the exponential to a specified base of each
*     pixel of a NDF to produce a new NDF data structure.

*  Usage:
*     expon in out base

*  ADAM Parameters:
*     BASE = LITERAL (Read)
*        The base of the exponential to be applied.  A special value
*        "Natural" gives natural (base-e) exponentiation.
*     IN = NDF (Read)
*        Input NDF data structure.
*     OUT = NDF (Write)
*        Output NDF data structure being the exponential of the input
*        NDF.
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the NDF supplied for parameter IN to be used
*        instead. [!]

*  Examples:
*     expon a b 10
*        This takes exponentials to base ten of the pixels in the NDF
*        called a, to make the NDF called b.  NDF b inherits its title
*        from a.
*     expon base=8 title="HD123456" out=b in=a
*        This takes exponential to base eight of the pixels in the NDF
*        called a, to make the NDF called b.  NDF b has the title
*        "HD123456".

*  Related Applications:
*     KAPPA: LOG10, LOGAR, LOGE, EXP10, EXPE, POW; Figaro: IALOG, ILOG,
*     IPOWER.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     LABEL, TITLE, UNITS, HISTORY, WCS, and VARIANCE components of an
*     NDF data structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.

*  Copyright:
*     Copyright (C) 1997, 1999, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2012 Science & Technology facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the free Software foundation; either Version 2 of
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
*     1997 November 5 (MJC):
*        Original NDF version.
*     1-OCT-1999 (DSB):
*        Changed to use LPG.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2012 May 8 (MJC):
*        Add _INT64 support.
*     2013-07-08 (TIMJ):
*        Restore _REAL support.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! VAL_ public constants
      INCLUDE 'CNF_PAR'          ! for CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION MXBASE    ! Maximum base allowed for exponential
      PARAMETER( MXBASE = 1000 )

      DOUBLE PRECISION MNBASE    ! Minimum base allowed for exponential
      PARAMETER( MNBASE = 0.001 )

*  Local Variables:
      LOGICAL BAD                ! Need to check for bad pixels?
      DOUBLE PRECISION BASE      ! Base of exponential to be taken of
                                 ! each pixel of input array
      CHARACTER * ( VAL__SZD ) CBASE ! Base of exponential to be taken
                                 ! (value of parameter BASE)
      CHARACTER * ( 13 ) COMPS   ! Array components to map
      INTEGER EL                 ! Number of mapped elements
      CHARACTER * ( NDF__SZFRM ) FORM ! Form of the ARRAY
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Data type for processing
      INTEGER NDFI               ! Identifier for 1st NDF (input)
      INTEGER NDFO               ! Identifier for 2nd NDF (output)
      INTEGER NERR               ! Number of errors generated
      INTEGER NERRV              ! Number of errors generated (var)
      INTEGER PNTRI( 2 )         ! Pointer to input NDF mapped array
      INTEGER PNTRO( 2 )         ! Pointer to output NDF mapped array
      LOGICAL VAR                ! Variance array to process?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Obtain the scalar value to be added.
      CALL PAR_MIX0D( 'BASE', 'Natural', MNBASE, MXBASE, 'Natural',
     :                .FALSE., CBASE, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Convert the string to a value.  PAR_MIX0D will give an error if the
*  value is neither numeric nor "Natural" so no need to check again.
      IF ( CBASE .EQ. 'NATURAL' ) THEN
         BASE = 2.718281828459
      ELSE
         CALL CHR_CTOD( CBASE, BASE, STATUS )
      END IF

*  Create a new output NDF based on the input NDF. Propagate the axis,
*  quality, and world co-ordinate system, in addition to the defaulted
*  components.
      CALL LPG_PROP( NDFI, 'Axis,Quality,WCS', 'OUT', NDFO, STATUS )

*  See if variance is present.
      CALL NDF_STATE( NDFI, 'Variance', VAR, STATUS )
      IF ( VAR ) THEN
         COMPS = 'Data,Variance'
      ELSE
         COMPS = 'Data'
      END IF

*  Determine which data type to use to process the input data array.
      CALL NDF_TYPE( NDFI, COMPS, ITYPE, STATUS )

*  Map the input and output arrays.
      CALL NDF_MAP( NDFI, COMPS, ITYPE, 'READ', PNTRI, EL, STATUS )
      CALL NDF_MAP( NDFO, COMPS, ITYPE, 'WRITE', PNTRO, EL, STATUS )

*  See if checks for bad pixels are needed.
      CALL NDF_BAD( NDFI, COMPS, .FALSE., BAD, STATUS )

*  Select the appropriate routine for the data type being processed and
*  add the constant to the data array.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_EXPOB( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ), VAR,
     :                    %VAL( CNF_PVAL( PNTRI( 2 ) ) ), BASE,
     :                    %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    %VAL( CNF_PVAL( PNTRO( 2 ) ) ),
     :                    NERR, NERRV, STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_EXPOUB( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ), VAR,
     :                    %VAL( CNF_PVAL( PNTRI( 2 ) ) ), BASE,
     :                    %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    %VAL( CNF_PVAL( PNTRO( 2 ) ) ),
     :                    NERR, NERRV, STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_EXPOD( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ), VAR,
     :                    %VAL( CNF_PVAL( PNTRI( 2 ) ) ), BASE,
     :                    %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    %VAL( CNF_PVAL( PNTRO( 2 ) ) ),
     :                    NERR, NERRV, STATUS )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPG1_EXPOR( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ), VAR,
     :                    %VAL( CNF_PVAL( PNTRI( 2 ) ) ), BASE,
     :                    %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    %VAL( CNF_PVAL( PNTRO( 2 ) ) ),
     :                    NERR, NERRV, STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_EXPOI( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ), VAR,
     :                    %VAL( CNF_PVAL( PNTRI( 2 ) ) ), BASE,
     :                    %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    %VAL( CNF_PVAL( PNTRO( 2 ) ) ),
     :                    NERR, NERRV, STATUS )

      ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
         CALL KPG1_EXPOK( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ), VAR,
     :                    %VAL( CNF_PVAL( PNTRI( 2 ) ) ), BASE,
     :                    %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    %VAL( CNF_PVAL( PNTRO( 2 ) ) ),
     :                    NERR, NERRV, STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL KPG1_EXPOW( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ), VAR,
     :                    %VAL( CNF_PVAL( PNTRI( 2 ) ) ), BASE,
     :                    %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    %VAL( CNF_PVAL( PNTRO( 2 ) ) ),
     :                    NERR, NERRV, STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_EXPOUW( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ), VAR,
     :                    %VAL( CNF_PVAL( PNTRI( 2 ) ) ), BASE,
     :                    %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                    %VAL( CNF_PVAL( PNTRO( 2 ) ) ),
     :                    NERR, NERRV, STATUS )
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC('T', ITYPE)
         CALL ERR_REP( ' ', 'Unrecognized data type ^T. '//
     :        '(possible programming error)', STATUS )
      END IF

*  See if there may be bad pixels in the output data array and set the
*  output bad pixel flag value accordingly unless the output NDF is
*  primitive.
      CALL NDF_FORM( NDFO, 'Data', FORM, STATUS )

      IF ( FORM .NE. 'PRIMITIVE' ) THEN
         CALL NDF_SBAD( BAD .OR. ( NERR .NE. 0 ), NDFO, 'Data', STATUS )
      END If

*  See if there may be bad pixels in the output variance array and set
*  the output bad pixel flag value accordingly unless the output NDF is
*  primitive.
      IF ( VAR ) THEN
         CALL NDF_FORM( NDFO, 'Variance', FORM, STATUS )

         IF ( FORM .NE. 'PRIMITIVE' ) THEN
            CALL NDF_SBAD( BAD .OR. ( NERRV .NE. 0 ), NDFO, 'Variance',
     :                     STATUS )
         END IF
      END IF

*  Obtain a new title for the output NDF.
      CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )

  999 CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'EXPON_ERR',
     :      'EXPON: Error taking the exponential of an NDF data '/
     :      /'structure.', STATUS )
      END IF

      END
