      SUBROUTINE ASTRATE( STATUS )
*+
*  Name:
*     ASTRATE

*  Purpose:
*     Calculate the rate of change of a Mapping output

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTRATE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application evaluates the rate of change of a specified output
*     of the supplied Mapping with respect to a specified input, at a
*     specified input position.
*
*     The result is estimated by interpolating the function using a fourth
*     order polynomial in the neighbourhood of the specified position. The
*     size of the neighbourhood used is chosen to minimise the RMS
*     residual per unit length between the interpolating polynomial and
*     the supplied Mapping function. This method produces good accuracy
*     but can involve evaluating the Mapping one hundred times or more.

*  Usage:
*     astrate this at ax1 ax2

*  ADAM Parameters:
*     RATE = _DOUBLE (Write)
*        A scale in which to store the rate of change of Mapping output
*        AX1 with respect to input AX2, evaluated at AT.
*     THIS = LITERAL (Read)
*        An NDF or text file holding the Mapping. If an NDF is supplied,
*        the Mapping from the base Frame of the WCS FrameSet to the
*        current Frame will be used.
*     AT() = _DOUBLE (Read)
*        An array holding the axis values at the position at which the
*        rate of change is to be evaluated. The number of elements in
*	 this array should equal the number of inputs to the Mapping.
*     AX1 = _INTEGER (Read)
*        The index of the Mapping output for which the rate of change is
*        to be found (output numbering starts at 1 for the first output).
*     AX2 = _INTEGER (Read)
*        The index of the Mapping input which is to be varied in order to
*        find the rate of change (input numbering starts at 1 for the first
*        input).

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council
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
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     15-JUN-2009 (DSB):
*        Original version.
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

*  External References:
      EXTERNAL AST_ISAMAPPING

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER THIS, NIN, NOUT, I, AX1, AX2
      DOUBLE PRECISION AT( NDF__MXDIM ), RATE
*.

*  Check inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Mapping.
      CALL KPG1_GTOBJ( 'THIS', 'Mapping', AST_ISAMAPPING, THIS,
     :                 STATUS )

*  Determine the Nin and Nout attributes of the Mapping
      NIN = AST_GETI( THIS, 'Nin', STATUS)
      NOUT = AST_GETI( THIS, 'Nout', STATUS)

      IF ( ( NIN .GT. NDF__MXDIM .OR. NOUT .GT. NDF__MXDIM )
     :    .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NI', NIN )
         CALL MSG_SETI( 'NO', NOUT )
         CALL MSG_SETI( 'NC', NDF__MXDIM )
         CALL ERR_REP( 'ASTRATE_ERR1', 'The supplied '//
     :                 'Mapping has ^NI input axes and ^NO output'//
     :                 'axes, but the maximum allowed number of '//
     :                 'axes is ^NC', STATUS)
      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the position at which the rate is to be evaluated.
      CALL PAR_EXACD( 'AT', NIN, AT, STATUS )

*  Get the index of the Mapping output for which the rate of change is to
*  be found.
      CALL PAR_GDR0I( 'AX1', 1, 1, NOUT, .FALSE., AX1, STATUS )

*  Get the index of the Mapping input which is to be varied in order to
*  find the rate of change.
      CALL PAR_GDR0I( 'AX2', 1, 1, NIN, .FALSE., AX2, STATUS )

*  Find the required rate of change.
      RATE = AST_RATE( THIS, AT, AX1, AX2, STATUS )

*  Display it an write it to the output parameter.
      CALL MSG_SETI( 'AX1', AX1 )
      CALL MSG_SETI( 'AX2', AX2 )
      CALL MSG_SETD( 'AT', AT( 1 ) )
      DO I = 2, NIN
         CALL MSG_SETC( 'AT', ',' )
         CALL MSG_SETD( 'AT', AT( I ) )
      END DO

      IF( RATE .NE. AST__BAD ) THEN
         CALL MSG_SETD( 'R', RATE )
         CALL MSG_OUT( ' ', 'Rate of change of output ^AX1 with '//
     :                 'respect to input ^AX2 at (^AT) is ^R.',
     :                 STATUS )
      ELSE
         CALL MSG_OUT( ' ', 'Rate of change of output ^AX1 with '//
     :                 'respect to input ^AX2 at (^AT) is undefined.',
     :                 STATUS )
      END IF

      CALL PAR_PUT0D( 'RATE', RATE, STATUS )

*  Tidy up.
 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTRATE_ERR', 'Error finding the rate of '//
     :                 'change of a Mapping output.', STATUS )
      END IF

      END

