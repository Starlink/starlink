      SUBROUTINE ASTTRAN1( STATUS )
*+
*  Name:
*     ASTTRAN1

*  Purpose:
*     Use a Mapping to transform a set of position in one dimension.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTTRAN1( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application applies a Mapping to transform the coordinates of a
*     set of points in one dimension. The input positions may be supplied
*     either directly or in a text file. The output positions are listed
*     on the screen and may optionally be stored in output text files.

*  Usage:
*     asttran1 this xin forward xout

*  ADAM Parameters:
*     THIS = LITERAL (Read)
*        An NDF, FITS file or text file holding the Mapping. If an NDF is
*        supplied, the Mapping from the base Frame of the WCS FrameSet to the
*        current Frame will be used. If a FITS file is supplied, the Mapping
*        from the pixel grid coordinates to the primary axis descriptions
*        will be used.
*     XIN = GROUP (Read)
*        A comma-separated list of floating point values to be used as the
*        input axis value. A text file may be specified by preceeding the
*        name of the file with an up arrow character "^". If the supplied value
*        ends with a minus sign, the user is re-prompted for additional
*        values.
*     FORWARD = _LOGICAL (Read)
*        If this value is TRUE, then the Mapping's forward
*        transformation will be used to transform the input positions.
*        Otherwise, its inverse transformation will be used.
*     XOUT = LITERAL (Read)
*        The name of a text file in which to put the transformed axis
*        values. No file is produced if a null (!) value is supplied. One
*        axis value is stored on each line of the file. [!]

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     19-AUG-2004 (DSB):
*        Original version.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL
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
      INTEGER THIS, IGRP1, IPXIN, IPXOUT
      INTEGER NP
      LOGICAL FORWRD

*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Mapping.
      CALL KPG1_GTOBJ( 'THIS', 'Mapping', AST_ISAMAPPING, THIS,
     :                 STATUS )

*  Check that the the Nin and Nout attributes of the Mapping are both 1.
      NP = AST_GETI( THIS, 'Nin', STATUS)
      IF( NP .NE. 1 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NI', NP )
         CALL ERR_REP( 'ASTTRAN1_ERR1', 'The supplied Mapping has '//
     :                 '^NI input axes (should be 1).', STATUS)
      END IF

      NP = AST_GETI( THIS, 'Nout', STATUS)
      IF( NP .NE. 1 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NI', NP )
         CALL ERR_REP( 'ASTTRAN1_ERR2', 'The supplied Mapping has '//
     :                 '^NI output axes (should be 1).', STATUS)
      END IF

*  Get the FORWARD parameter.
      CALL PAR_GET0L( 'FORWARD', FORWRD, STATUS )

*  Get a group holding the input axis values.
      IGRP1 = GRP__NOID
      CALL KPG1_GTGRP( 'XIN', IGRP1, NP, STATUS )

*  Allocate memory for the input positions.
      CALL PSX_CALLOC( NP, '_DOUBLE', IPXIN, STATUS )

*  Allocate memory for the output positions.
      CALL PSX_CALLOC( NP, '_DOUBLE', IPXOUT, STATUS )

*  Read the values from the group into the memory.
      CALL ATL1_GTOFL( IGRP1, NP, 1, %VAL( CNF_PVAL( IPXIN ) ), STATUS )

* Transform the positions.
      CALL AST_TRAN1( THIS, NP, %VAL( CNF_PVAL( IPXIN ) ), FORWRD,
     :                %VAL( CNF_PVAL( IPXOUT ) ), STATUS )

*  Output the results.
      CALL ATL1_PRNT1( NP, %VAL( CNF_PVAL( IPXOUT ) ), 'XOUT', STATUS )

*  Free resources.
      CALL GRP_DELET( IGRP1, STATUS )
      CALL PSX_FREE( IPXIN, STATUS )
      CALL PSX_FREE( IPXOUT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTTRAN1_ERR', 'Error transforming 1-'//
     :                 'dimensional positions.', STATUS )
      END IF

      END
