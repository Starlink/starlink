      SUBROUTINE ASTRATEMAP( STATUS )
*+
*  Name:
*     ASTRATEMAP

*  Purpose:
*     Create a RateMap.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTRATEMAP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new RateMap and optionally initialises
*     its attributes.
*
*     A RateMap is a Mapping which represents a single element of the
*     Jacobian matrix of another Mapping. The Mapping for which the
*     Jacobian is required is specified when the new RateMap is created,
*     and is referred to as the "encapsulated Mapping" below.
*
*     The number of inputs to a RateMap is the same as the number of inputs
*     to its encapsulated Mapping. The number of outputs from a RateMap
*     is always one. This one output equals the rate of change of a
*     specified output of the encapsulated Mapping with respect to a
*     specified input of the encapsulated Mapping (the input and output
*     to use are specified when the RateMap is created).
*
*     A RateMap which has not been inverted does not define an inverse
*     transformation. If a RateMap has been inverted then it will define
*     an inverse transformation but not a forward transformation.

*  Usage:
*     astratemap map ax1 ax2 options result

*  ADAM Parameters:
*     AX1 = _INTEGER (Read)
*        Index of the output from the encapsulated Mapping for which the
*        rate of change is required. This corresponds to the delta
*        quantity forming the numerator of the required element of the
*        Jacobian matrix. The first axis has index 1.
*     AX2 = _INTEGER (Read)
*        Index of the input to the encapsulated Mapping which is to be
*        varied. This corresponds to the delta quantity forming the
*        denominator of the required element of the Jacobian matrix.
*        The first axis has index 1.
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     MAP = LITERAL (Read)
*        An NDF or text file holding the encapsulated Mapping. If an NDF is
*        supplied, the Mapping from the base Frame of the WCS FrameSet to
*        the current Frame will be used.
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute
*        assignments to be used for initialising the new RateMap.
*     RESULT = LITERAL (Read)
*        A text file to receive the new RateMap.

*  Copyright:
*     Copyright (C) 2011 Central Laboratory of the Research Councils.
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
*     {enter_new_authors_here}

*  History:
*     16-MAR-2011 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  External References:
      EXTERNAL AST_ISAMAPPING

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER AX1
      INTEGER AX2
      INTEGER MAP
      INTEGER NIN
      INTEGER NOUT
      INTEGER RESULT
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Mapping.
      CALL KPG1_GTOBJ( 'MAP', 'Mapping', AST_ISAMAPPING, MAP,
     :                 STATUS )

*  Determine the Nin and Nout attributes of the Mapping
      NIN = AST_GETI( MAP, 'Nin', STATUS)
      NOUT = AST_GETI( MAP, 'Nout', STATUS)

*  Get the other parameters.
      CALL PAR_GDR0I( 'AX1', 1, 1, NOUT, .FALSE., AX1,
     :                STATUS )
      CALL PAR_GDR0I( 'AX2', 1, 1, NIN, .FALSE., AX2,
     :                STATUS )

*  Get the required simplified Mapping.
      RESULT = AST_RATEMAP( MAP, AX1, AX2, ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write this Mapping out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTRATEMAP_ERR', 'Error creating a new '//
     :                 'RateMap.', STATUS )
      END IF

      END
