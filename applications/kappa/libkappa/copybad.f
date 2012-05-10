      SUBROUTINE COPYBAD( STATUS )
*+
*  Name:
*     COPYBAD

*  Purpose:
*     Copies bad pixels from one NDF file to another.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL COPYBAD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application copies bad pixels from one NDF file to another. It
*     takes in two NDFs (parameters IN and REF), and creates a third
*     (parameter OUT) which is a copy of IN, except that any pixel which
*     is set bad in the DATA array of REF, is also set bad in the DATA
*     and VARIANCE (if available) arrays in OUT.
*
*     By setting the INVERT parameter TRUE, the opposite effect can be
*     produced (i.e. any pixel which is not set bad in the DATA array of
*     REF, is set bad in OUT and the others are left unchanged).

*  Usage:
*     copybad in ref out title

*  ADAM Parameters:
*     IN = NDF (Read)
*        NDF containing the data to be copied to OUT.
*     INVERT = _LOGICAL (Read)
*        If TRUE, then the bad and good pixels within the reference NDF
*        specified by parameter REF are inverted before being used (that
*        is, good pixels are treated as bad and bad pixels are treated as
*        good). [FALSE]
*     REF = NDF (Read)
*        NDF containing the bad pixels to be copied to OUT.
*     OUT = NDF (Write)
*        The output NDF.
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the NDF supplied for parameter IN to be used
*        instead. [!]

*  Examples:
*     copybad in=a ref=b out=c title="New image"
*        This creates a NDF called c, which is a copy of the NDF called a.
*        Any bad pixels present in the NDF called b are copied into the
*        corresponding positions in c (non-bad pixels in b are ignored).
*        The title of c is "New image".

*  Notes:
*     - If the two input NDFs have different pixel-index bounds, then
*     they will be trimmed to match before being processed.  An error
*     will result if they have no pixels in common.

*  Related Applications:
*     KAPPA: SUBSTITUTE, NOMAGIC, FILLBAD, PASTE, GLITCH.

*  Implementation Status:
*     -  This routine correctly processes the WCS, AXIS, DATA, QUALITY,
*     LABEL, TITLE, UNITS, HISTORY, and VARIANCE components of an NDF
*     data structure and propagates all extensions.
*     -  All non-complex numeric data types can be handled.

*  Copyright:
*     Copyright (C) 1998, 2000, 2003-2004 Central Laboratory of the
*     Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2008, 2009, 2012 Science and Technology Facilities
*     Council.
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
*     TDCA: Tim D.C. Ash(STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     BEC: Brad Cavanagh (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-OCT-1998 (TDCA):
*        Original version, partially based on the KAPPA routine ADD
*     13-OCT-1998 (DSB):
*        - Tidied up the prologue and code a touch.
*        - Corrected distinction between the data type in which values are
*          processed, and the data type in which values are stored in the
*          output NDF.
*        - Use ERR_REP in place of MSG_OUT to report an error if an
*          unrecognised data type is obtained. Also reformat the error
*          report to include the unrecognised data type, and a friendly
*          message to explain to the user that the problem is not of their
*          doing.
*        - Changes the KPS routine names to use a consistent root
*          ("kps1_cpb").
*        - Replaced the message which reports the number of bad pixels
*          copied, with a message reporting the number of bad pixels in
*          the output NDF. This is easier for a user to understand.
*        - Removed un-required initialisation of NBAD.
*        - Make the grammar of the NBAD message dependant on the value of
*          NBAD, and include the name of the output NDF.
*        - Sort local variable declarations into alphabetical order.
*     20-SEP-2000 (DSB):
*        - Correct mapping mode for output NDF from WRITE to UPDATE.
*     20-MAY-2003 (DSB):
*        - Set BAD-PIXEL flag in output NDF.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     27-APR-2006 (DSB):
*        Added parameter INVERT.
*     2008 July 14 (BEC)
*        Propagate UNITS component to output NDF.
*     24-FEB-2009 (BEC):
*        - Correct mapping mode for output NDF's VARIANCE component
*          from WRITE to UPDATE.
*     2009-11-25 (TIMJ):
*        Do not force the type of OUT to be the type of REF, it should
*        be the type of IN.
*     26-NOV-2009 (DSB):
*        Initialise TY_IN.
*     2012 May 8 (MJC):
*        Add _INT64 support.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants
      INCLUDE 'NDF_PAR'                 ! NDF_ public constants
      INCLUDE 'CNF_PAR'                 ! For CNF_PVAL function

*  Status:
      INTEGER STATUS                    ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZTYP ) TY_IN  ! Data type for processing
      INTEGER NEL                       ! Number of mapped elements
      INTEGER IN                        ! Identifier for IN (input)
      INTEGER NBAD                      ! Number of bad pixels in output
      INTEGER OUT                       ! Identifier for OUT (output)
      INTEGER P_OUT                     ! Pointer to OUT's data array
      INTEGER P_OUTV                    ! Pointer to OUT's variance array
      INTEGER P_REF                     ! Pointer to REF's data array
      INTEGER REF                       ! Identifier for REF (input)
      LOGICAL VAR                       ! Varience array present in IN ?
      LOGICAL INVERT                    ! Invert the good/bad status?

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain identifiers for the two input NDFs.
      CALL LPG_ASSOC( 'IN', 'READ', IN, STATUS )
      CALL LPG_ASSOC( 'REF', 'READ', REF, STATUS )

*  Trim the input pixel-index bounds to match.
      CALL NDF_MBND( 'TRIM', IN, REF, STATUS )

*  Create a new output NDF based on the first input NDF. Propagate the
*  data, WCS, axis, and unit components. (Use if neither variance nor
*  quality arrays are present.)
      CALL LPG_PROP( IN, 'Data,WCS,Axis,Variance,Quality,Units', 'OUT',
     :               OUT, STATUS )

* Map the NDF DATA arrays using the type from IN.
      CALL NDF_TYPE( IN, 'Data', TY_IN, STATUS )
      CALL NDF_MAP( REF, 'Data', TY_IN, 'READ', P_REF, NEL, STATUS )
      CALL NDF_MAP( OUT, 'Data', TY_IN, 'UPDATE', P_OUT, NEL, STATUS )

*  If required, map the VARIANCE array.
      CALL NDF_STATE( IN, 'Variance', VAR, STATUS )
      IF( VAR ) CALL NDF_MAP( OUT, 'Variance', TY_IN, 'UPDATE', P_OUTV,
     :                        NEL, STATUS )

*  See if the operation is to be inverted.
      CALL PAR_GET0L( 'INVERT', INVERT, STATUS )

*  Select the appropriate routine to copy the bad pixels.
      IF ( TY_IN .EQ. '_INTEGER' ) THEN
         CALL KPS1_CPBI( NEL, INVERT, VAR, %VAL( CNF_PVAL( P_REF ) ),
     :                   %VAL( CNF_PVAL( P_OUT ) ),
     :                   %VAL( CNF_PVAL( P_OUTV ) ), NBAD, STATUS )

      ELSE IF ( TY_IN .EQ. '_REAL' ) THEN
         CALL KPS1_CPBR( NEL, INVERT, VAR, %VAL( CNF_PVAL( P_REF ) ),
     :                   %VAL( CNF_PVAL( P_OUT ) ),
     :                   %VAL( CNF_PVAL( P_OUTV ) ), NBAD, STATUS )

      ELSE IF ( TY_IN .EQ. '_DOUBLE' ) THEN
         CALL KPS1_CPBD( NEL, INVERT, VAR, %VAL( CNF_PVAL( P_REF ) ),
     :                   %VAL( CNF_PVAL( P_OUT ) ),
     :                   %VAL( CNF_PVAL( P_OUTV ) ), NBAD, STATUS )

      ELSE IF ( TY_IN .EQ. '_INT64' ) THEN
         CALL KPS1_CPBK( NEL, INVERT, VAR, %VAL( CNF_PVAL( P_REF ) ),
     :                   %VAL( CNF_PVAL( P_OUT ) ),
     :                   %VAL( CNF_PVAL( P_OUTV ) ), NBAD, STATUS )

      ELSE IF ( TY_IN .EQ. '_BYTE' ) THEN
         CALL KPS1_CPBB( NEL, INVERT, VAR, %VAL( CNF_PVAL( P_REF ) ),
     :                   %VAL( CNF_PVAL( P_OUT ) ),
     :                   %VAL( CNF_PVAL( P_OUTV ) ), NBAD, STATUS )

      ELSE IF ( TY_IN .EQ. '_UBYTE' ) THEN
         CALL KPS1_CPBUB( NEL, INVERT, VAR, %VAL( CNF_PVAL( P_REF ) ),
     :                    %VAL( CNF_PVAL( P_OUT ) ),
     :                    %VAL( CNF_PVAL( P_OUTV ) ), NBAD, STATUS )

      ELSE IF ( TY_IN .EQ. '_WORD' ) THEN
         CALL KPS1_CPBW( NEL, INVERT, VAR, %VAL( CNF_PVAL( P_REF ) ),
     :                   %VAL( CNF_PVAL( P_OUT ) ),
     :                   %VAL( CNF_PVAL( P_OUTV ) ), NBAD, STATUS )

      ELSE IF ( TY_IN .EQ. '_UWORD' ) THEN
         CALL KPS1_CPBUW( NEL, INVERT, VAR, %VAL( CNF_PVAL( P_REF ) ),
     :                    %VAL( CNF_PVAL( P_OUT ) ),
     :                    %VAL( CNF_PVAL( P_OUTV ) ), NBAD, STATUS )

      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TY', TY_IN )
         CALL ERR_REP( 'COPYBAD_UTYP', 'Unsupported data type '//
     :                 '''^TY'' (programming error).', STATUS )
      END IF

*  Display a blank line to highlight the following message.
      CALL MSG_BLANK( STATUS )

*  Report the number of pixels in the output NDF.
      CALL NDF_MSG( 'NDF', OUT )
      IF ( NBAD .EQ. 0 ) THEN
         CALL MSG_OUT( 'COPYBAD_NBAD', '  There are no bad pixels in '//
     :                 'the output NDF ''^NDF''.', STATUS)

      ELSE IF ( NBAD .EQ. 1 ) THEN
         CALL MSG_OUT( 'COPYBAD_NBAD', '  There is 1 bad pixel in the'//
     :                 ' output NDF ''^NDF''.', STATUS)

      ELSE
         CALL MSG_SETI( 'NBAD', NBAD )
         CALL MSG_OUT( 'COPYBAD_NBAD', '  There are ^NBAD bad pixels '//
     :                 'in the output NDF ''^NDF''.', STATUS)
      END IF

*  Display a blank line to highlight the previous message.
      CALL MSG_BLANK( STATUS )

*  Obtain the output title and insert it into the output NDF.
      CALL NDF_CINP( 'TITLE', OUT, 'Title', STATUS )

*  Set the BAD-PIXEL flag in the output NDF.
      CALL NDF_SBAD( ( NBAD .GT. 0 ), OUT, 'Data,Variance', STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'COPYBAD_ERR', 'COPYBAD: Error copying bad '//
     :                 'pixels.', STATUS )
      END IF

      END
