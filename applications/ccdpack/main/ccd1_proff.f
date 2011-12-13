      SUBROUTINE CCD1_PROFF( NN, USE, XOFF, YOFF, FRAMES, USEFRM,
     :                       STATUS )
*+
*  Name:
*     CCD1_PROFF

*  Purpose:
*     Outputs X and Y offsets through the logging system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_PROFF( NN, USE, XOFF, YOFF, FRAMES, USEFRM, STATUS )

*  Description:
*     This routine prints out a set of X and Y offsets.  Output is only
*     made for the pairs for which the logical array USE is true.  If
*     USEFRM is set TRUE then output will be in a format determined by
*     frame in question (using AST_FORMAT, so determined by the
*     Format(axis) attributes of the frame).  If the USEFRM
*     argument is .FALSE. then output will simply be in pixels.

*  Arguments:
*     NN = INTEGER (Given)
*        The number of offset pairs.
*     USE( NN ) = LOGICAL (Given)
*        Whether to use the offset pair of this index.
*     XOFF( NN ) = DOUBLE PRECISION (Given)
*        The X offsets to be output.
*     YOFF( NN ) = DOUBLE PRECISION (Given)
*        The Y offsets to be output.
*     FRAMES( NN ) = INTEGER (Given)
*        AST pointers to the Frame in which the offsets are given (not
*        accessed if USEFRM is FALSE).
*     USEFRM = LOGICAL (Given)
*        If TRUE, output coordinates as per FRAMES, otherwise output in
*        pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - this routine writes information using the CCDPACK logfile
*     system, so this must be opened first.

*  Copyright:
*     Copyright (C) 1999, 2001 Central Laboratory of the Research
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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-NOV-1999 (MBT):
*        Original version.
*     25-JAN-2001 (MBT):
*        Modified message to say that it's an approximate offset, to
*        reflect the fact that it's not been properly best fit yet,
*        and in FINDOFF may contain the effect of duplicate points.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'AST_PAR'          ! AST system constants
      INCLUDE 'MSG_PAR'          ! Message system constants

*  Arguments Given:
      INTEGER NN
      LOGICAL USE( NN )
      DOUBLE PRECISION XOFF( NN )
      DOUBLE PRECISION YOFF( NN )
      INTEGER FRAMES( NN )
      LOGICAL USEFRM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( MSG__SZMSG ) BUFFER ! Output message string
      CHARACTER * ( MSG__SZMSG ) XSTR ! Formatted X coordinate
      CHARACTER * ( MSG__SZMSG ) YSTR ! Formatted Y coordinate
      INTEGER FRM                ! AST frame to use for formatting
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Position in string
      INTEGER XS                 ! X-offset position in o/p string
      INTEGER YS                 ! Y-offset position in o/p string
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write a header about the units.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      IF ( USEFRM ) THEN
         CALL CCD1_MSG( ' ',
     :'  Approximate offsets in image Current coordinates:', STATUS )
      ELSE
         CALL CCD1_MSG( ' ',
     :'  Approximate offsets in pixels:', STATUS )
      END IF

*  Construct a header string.
      BUFFER = '  List'
      IAT = VAL__SZI + 4
      BUFFER( IAT: ) = '     X-offset '
      XS = IAT + 2
      IAT = IAT + VAL__SZD + 4
      BUFFER( IAT: ) = '     Y-offset '
      YS = IAT + 2
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', BUFFER, STATUS )

*  Now write out the offsets for the selected pairs.
       DO I = 1, NN
         IF ( USE( I ) )  THEN
            BUFFER = ' '
            CALL CHR_ITOC( I, BUFFER( 5: ), IAT )
            IAT = 5 + IAT
            BUFFER( IAT : IAT ) = ')'
            IF ( USEFRM ) THEN
               FRM = FRAMES( I )
            ELSE
               FRM = AST__NULL
            END IF
            CALL CCD1_XYFMT( XOFF( I ), YOFF( I ), FRM, XSTR, YSTR,
     :                       STATUS )
            BUFFER( XS: ) = XSTR
            BUFFER( YS: ) = YSTR
            CALL CCD1_MSG( ' ', BUFFER, STATUS )
         END IF
      END DO

      END
* $Id$
