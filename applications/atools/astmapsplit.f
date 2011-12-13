      SUBROUTINE ASTMAPSPLIT( STATUS )
*+
*  Name:
*     ASTMAPSPLIT

*  Purpose:
*     Split a Mapping up into parallel component Mappings.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTMAPSPLIT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application returns a Mapping that will convert coordinates
*     between the coordinate systems represented by two Frames in a
*     FrameSet.
*
*     This application creates a new Mapping which connects specified inputs
*     within a supplied Mapping to the corresponding outputs of the supplied
*     Mapping.  This is only possible if the specified inputs correspond to
*     some subset of the Mapping outputs. That is, there must exist a subset
*     of the Mapping outputs for which each output depends only on the
*     selected Mapping inputs, and not on any of the inputs which have not
*     been  selected. If this condition is not met by the supplied Mapping,
*     then an error is reported.

*  Usage:
*     astmapsplit this in out map

*  ADAM Parameters:
*     IN() = _INTEGER (Read)
*        A vector of integers which are the indices within the supplied
*        Mapping (THIS) of the inputs which are to be picked from the Mapping
*        (the first Mapping input has index 1).
*     MAP = LITERAL (Read)
*        An text file to receive the output Mapping. The number of inputs
*        to this Mapping will be the same as the number of values
*        supplied for the IN parameter (the number of outputs may be
*        different).
*     OUT() = _INTEGER (Write)
*        An output parameter to which is written a vector of integers which
*        are the indices of the outputs of the supplied Mapping fed by the
*        picked inputs. A value of one is used to refer to the first Mapping
*        output. The number of values stored in the array on exit will equal
*        the number of outputs in the returned Mapping. The i'th element in
*        the returned array holds the index within the supplied Mapping which
*        corresponds to the i'th output of the returned Mapping.
*     THIS = LITERAL (Read)
*        An NDF or text file holding the input Mapping. If an NDF is supplied,
*        the base to current Mapping within the WCS FrameSet will be used.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     2-DEC-2005 (DSB):
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
      INCLUDE 'NDF_PAR'          ! NDF constants

*  External References:
      EXTERNAL AST_ISAMAPPING

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I
      INTEGER IN( NDF__MXDIM )
      INTEGER MAP
      INTEGER NIN
      INTEGER NOUT
      INTEGER OUT( NDF__MXDIM )
      INTEGER THIS
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Mapping.
      CALL KPG1_GTOBJ( 'THIS', 'Mapping', AST_ISAMAPPING, THIS,
     :                 STATUS )

*  Get the indices of the inputs to select.
      CALL PAR_GET1I( 'IN', NDF__MXDIM, IN, NIN, STATUS )

*  Split the supplied Mapping.
      CALL AST_MAPSPLIT( THIS, NIN, IN, OUT, MAP, STATUS )

*  Report an error if the Mapping could not be split.
      IF( MAP .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ASTMAPSPLIT_ERR1', 'There is no subset of '//
     :                 'Mapping outputs which depend only on the '//
     :                 'specified Mapping inputs.', STATUS )

*  Otherwise, display the indices of the output axes.
      ELSE
         CALL MSG_BLANK( STATUS )
         NOUT = AST_GETI( map, 'Nout', STATUS )

         CALL MSG_SETI( 'NIN', NIN )
         CALL MSG_OUT( 'ASTMAPSPLIT_MSG1', 'The specified ^NIN inputs:',
     :                 status )

         DO I = 1, NIN
            CALL MSG_SETI( 'IN', IN( I ) )
            IF( I .NE. NIN ) CALL MSG_SETC( 'IN', ' ,' )
         END DO
         CALL MSG_OUT( 'ASTMAPSPLIT_MSG2', '   ^IN', STATUS )

         CALL MSG_SETI( 'NOUT', NOUT )
         CALL MSG_OUT( 'ASTMAPSPLIT_MSG3', 'feed the following '//
     :                 '^NOUT outputs:', status )

         DO I = 1, NOUT
            CALL MSG_SETI( 'OUT', OUT( I ) )
            IF( I .NE. NOUT ) CALL MSG_SETC( 'OUT', ' ,' )
         END DO

         CALL MSG_OUT( 'ASTMAPSPLIT_MSG4', '   ^OUT', STATUS )
         CALL MSG_BLANK( STATUS )

         CALL PAR_PUT1I( 'OUT', NOUT, OUT, STATUS )

*  Write the Mapping out to a text file.
         CALL ATL1_PTOBJ( 'MAP', ' ', MAP, STATUS )

      END IF

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTMAPSPLIT_ERR', 'Error splitting a Mapping '//
     :                 'up into parallel component Mappings.', STATUS )
      END IF

      END
