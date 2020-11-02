      SUBROUTINE ASTTRAN2( STATUS )
*+
*  Name:
*     ASTTRAN2

*  Purpose:
*     Use a Mapping to transform a set of position in two dimensions.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTTRAN2( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application applies a Mapping to transform the coordinates of a
*     set of points in two dimensions. The input positions may be supplied
*     either directly or in a text file. The output positions are listed
*     on the screen ( an X value and a Y value on each line, separated by
*     a space) and may optionally be stored in output text files.

*  Usage:
*     asttran2 this xin yin forward xout yout

*  ADAM Parameters:
*     THIS = LITERAL (Read)
*        An NDF, FITS file or text file holding the Mapping. If an NDF is
*        supplied, the Mapping from the base Frame of the WCS FrameSet to the
*        current Frame will be used. If a FITS file is supplied, the Mapping
*        from the pixel grid coordinates to the primary axis descriptions
*        will be used.
*     XIN = GROUP (Read)
*        A comma-separated list of floating point values to be used as the
*        input X axis value. A text file may be specified by preceeding the
*        name of the file with an up arrow character "^". If the supplied value
*        ends with a minus sign, the user is re-prompted for additional
*        values.
*     YIN = GROUP (Read)
*        A comma-separated list of floating point values to be used as the
*        input Y axis value. A text file may be specified by preceeding the
*        name of the file with an up arrow character "^". If the supplied value
*        ends with a minus sign, the user is re-prompted for additional
*        values.
*     FORWARD = _LOGICAL (Read)
*        If this value is TRUE, then the Mapping's forward
*        transformation will be used to transform the input positions.
*        Otherwise, its inverse transformation will be used.
*     XOUT = LITERAL (Read)
*        The name of a text file in which to put the transformed X axis
*        values. No file is produced if a null (!) value is supplied. One
*        axis value is stored on each line of the file. [!]
*     XVAL = _DOUBLE (Write)
*        An output parameter that is left holding the final transformed
*        output X value.
*     YOUT = LITERAL (Read)
*        The name of a text file in which to put the transformed Y axis
*        values. No file is produced if a null (!) value is supplied. One
*        axis value is stored on each line of the file. [!]
*     YVAL = _DOUBLE (Write)
*        An output parameter that is left holding the final transformed
*        output Y value.

*  Copyright:
*     Copyright (C) 2003-2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2013 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     6-JUN-2003 (DSB):
*        Original version.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL
*     3-DEC-2013 (DSB):
*        Added parameters XVAL and YVAL.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  External References:
      EXTERNAL AST_ISAMAPPING

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER THIS, IGRP1, IGRP2, IPXIN, IPYIN, IPXOUT, IPYOUT
      INTEGER NP, NP2
      LOGICAL FORWRD

*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Mapping.
      CALL KPG1_GTOBJ( 'THIS', 'Mapping', AST_ISAMAPPING, THIS,
     :                 STATUS )

*  Check that the the Nin and Nout attributes of the Mapping are both 2.
      NP = AST_GETI( THIS, 'Nin', STATUS)
      IF( NP .NE. 2 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NI', NP )
         CALL ERR_REP( 'ASTTRAN2_ERR1', 'The supplied Mapping has '//
     :                 '^NI input axes (should be 2).', STATUS)
      END IF

      NP = AST_GETI( THIS, 'Nout', STATUS)
      IF( NP .NE. 2 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NI', NP )
         CALL ERR_REP( 'ASTTRAN2_ERR2', 'The supplied Mapping has '//
     :                 '^NI output axes (should be 2).', STATUS)
      END IF

*  Get the FORWARD parameter.
      CALL PAR_GET0L( 'FORWARD', FORWRD, STATUS )

*  Get two groups holding the input axis values.
      IGRP1 = GRP__NOID
      CALL KPG1_GTGRP( 'XIN', IGRP1, NP, STATUS )
      IGRP2 = GRP__NOID
      CALL KPG1_GTGRP( 'YIN', IGRP2, NP2, STATUS )

      IF( NP .NE. NP2 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NX', NP )
         CALL MSG_SETI( 'NY', NP2 )
         CALL ERR_REP( 'ASTTRAN2_ERR3', ' Number of input X positions'//
     :                 ' (^NX) and Y positions (^NY) differ.', STATUS )
      END IF

*  Allocate memory for the input positions.
      CALL PSX_CALLOC( NP, '_DOUBLE', IPXIN, STATUS )
      CALL PSX_CALLOC( NP, '_DOUBLE', IPYIN, STATUS )

*  Allocate memory for the output positions.
      CALL PSX_CALLOC( NP, '_DOUBLE', IPXOUT, STATUS )
      CALL PSX_CALLOC( NP, '_DOUBLE', IPYOUT, STATUS )

*  Read the values from the group into the memory.
      CALL ATL1_GTOFL( IGRP1, NP, 1, %VAL( CNF_PVAL( IPXIN ) ), STATUS )
      CALL ATL1_GTOFL( IGRP2, NP, 1, %VAL( CNF_PVAL( IPYIN ) ), STATUS )

* Transform the positions.
      CALL AST_TRAN2( THIS, NP, %VAL( CNF_PVAL( IPXIN ) ),
     :                %VAL( CNF_PVAL( IPYIN ) ), FORWRD,
     :                %VAL( CNF_PVAL( IPXOUT ) ),
     :                %VAL( CNF_PVAL( IPYOUT ) ), STATUS )

*  Output the results.
      CALL ATL1_PRNT2( NP, %VAL( CNF_PVAL( IPXOUT ) ),
     :                 %VAL( CNF_PVAL( IPYOUT ) ), 'XOUT',
     :                 'YOUT', 'XVAL', 'YVAL', STATUS )

*  Free resources.
      CALL GRP_DELET( IGRP1, STATUS )
      CALL GRP_DELET( IGRP2, STATUS )
      CALL PSX_FREE( IPXIN, STATUS )
      CALL PSX_FREE( IPYIN, STATUS )
      CALL PSX_FREE( IPXOUT, STATUS )
      CALL PSX_FREE( IPYOUT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTTRAN2_ERR', 'Error transforming 2-'//
     :                 'dimensional positions.', STATUS )
      END IF

      END
