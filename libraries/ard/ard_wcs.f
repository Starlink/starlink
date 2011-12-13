      SUBROUTINE ARD_WCS( IWCS, DOMAIN, STATUS )
*+
*  Name:
*     ARD_WCS

*  Purpose:
*     Specify WCS information to be used in future calls to ARD_WORK.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD_WCS( IWCS, DOMAIN, STATUS )

*  Description:
*     This routine can be used to specify the cooordinate systems
*     which can be used in subsequent calls to ARD_WORK. ARD descriptions
*     passed to subsequent calls to ARD_WORK can include positions in any
*     of the Frames included in the supplied FrameSet. The ARD description
*     should include suitable COFRAME or WCS statements to indicate which
*     coordinate system is being used. If no COFRAME or WCS statements
*     are included in the ARD description, then it is assumed that
*     positions within the ARD description are given in the current Frame
*     of the supplied FrameSet, IWCS.
*
*     If this routine is not called prior to ARD_WORK (or if it is
*     called with IWCS set AST__NULL), then the ARD description must
*     provide (either directly or through a WCS statement) positions in
*     pixel coordinates.
*
*     The FrameSet pointer supplied is simply stored by this routine.
*     If any changes are subsequently made to the FrameSet by the calling
*     routine, then these changes will be visible within ARD_WORK. In
*     particular, if the calling routine annulls the FrameSet pointer,
*     then ARD_WORK will fail.
*
*     The supplied FrameSet will be used by all subsequent calls to ARD_WORK
*     until a new FrameSet is specified by calling ARD_WCS again.

*  Arguments:
*     IWCS = INTEGER (Given)
*        An AST pointer to a FrameSet, or AST__NULL.
*     DOMAIN = CHARACTER * ( * ) (Given)
*        The Domain name corresponding to pixel co-ordinates within the
*        mask array passed to routine ARD_WORK. If a blank value is
*        supplied, "PIXEL" will be used. The IWCS FrameSet (if supplied)
*        must contain a Frame with this Domain. If the supplied string is
*        longer than 40 characters, the trailing characters are ignored.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
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
*     17-JUL-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARD_ERR'          ! ARD error constants
      INCLUDE 'ARD_CONST'        ! ARD private constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Global Constants:
      INCLUDE 'ARD_COM'          ! ARD common blocks
*        CMN_AWCS = INTEGER (Write)
*           A pointer to the application FrameSet.
*        CMN_ADOM = CHARACTER*40 (Write)
*           The Domain name associated with pixel
*           coordinates in the mask array.

*  Arguments Given:
      INTEGER IWCS
      CHARACTER DOMAIN*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL ARD1_INIT         ! Initialise ARD common blocks
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      LOGICAL OK                 ! Is the pointer usable?
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Start an new error reporting context.
      CALL ERR_MARK

*  If the supplied value is not AST__NULL or a FrameSet, set a flag.
      IF( IWCS .NE. AST__NULL ) THEN
         OK = AST_ISAFRAMESET( IWCS, STATUS )
      ELSE
         OK = .TRUE.
      END IF

*  If an error occurred above, annull it.
      IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

*  End the error reporting context.
      CALL ERR_RLSE

*  If a valid FrameSet pointer (or AST__NULL) was supplied, store it,
*  together with the pixel Domain name.
      IF( OK ) THEN
         CMN_AWCS = IWCS
         IF( CHR_LEN( DOMAIN ) .GT. 0 ) THEN
            CMN_ADOM = DOMAIN
         ELSE
            CMN_ADOM = 'PIXEL'
         END IF

*  Otherwise, report an error.
      ELSE
         STATUS = ARD__BADAR
         CALL MSG_SETI( 'I', IWCS )
         CALL ERR_REP( 'ARD_WCS_ERR', 'ARD_WCS: The supplied value '//
     :                 '(^I) is not a valid FrameSet pointer '//
     :                 '(programming error).', STATUS )
      END IF

      END
