      SUBROUTINE CCD1_DMPRG( FSET, DOMAIN, REPORT, JKEEP, STATUS )
*+
*  Name:
*     CCD1_DMPRG

*  Purpose:
*     Purge a frameset of frames in a given domain.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_DMPRG( FSET, DOMAIN, REPORT, JKEEP, STATUS )

*  Description:
*     This routine removes all frames from a frameset which have the
*     domain name supplied.  Optionally, the index of one domain to
*     exempt from this process may be supplied.  For each domain
*     removed, a message may be logged through the CCDPACK logging
*     system.
*
*     Normally the Current and Base frames are not disturbed by
*     this routine.  However, if one of these is deleted, a
*     new Current or Base frame will be assigned using normal AST
*     rules.  The exception to this is that if a Current or Base
*     frame is deleted, and JKEEP is a valid frame index, then JKEEP
*     will become the new one.

*  Arguments:
*     FSET = INTEGER (Given)
*        AST pointer to the frameset.  The frameset may be modified on
*        exit, but the Current frame will not be altered, except in the
*        case that the Current frame is one of the ones removed.
*     DOMAIN = CHARACTER * ( * ) (Given)
*        Name of the domain to purge.  Spaces and case-sensitivity are
*        ignored.
*     REPORT = LOGICAL (Given)
*        If REPORT is set .TRUE. then for each domain removed, a message
*        is logged through the CCDPACK logging system.  Otherwise
*        operation is silent.
*     JKEEP = INTEGER (Given and Returned)
*        Any frame with this index will not be deleted, even if it has
*        a Domain of DOMAIN.  On exit, this value will refer to the same
*        frame as on entry, although if frames with indices below its
*        value have been removed, its numerical value will have changed.
*
*        If no frames are to be exempted, this should be set to a
*        non-positive (e.g. AST__NOFRAME).  In this case, it is allowed
*        to supply a constant rather than a variable for this parameter.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

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
*     16-APR-1999 (MBT):
*        Original version.
*     21-SEP-1999 (MBT):
*        Added REPORT argument.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants

*  Arguments Given:
      INTEGER FSET
      CHARACTER * ( * ) DOMAIN
      LOGICAL REPORT
      INTEGER JKEEP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( AST__SZCHR ) DMN1 ! Case folded and despaced domain name
      INTEGER FRM                ! AST pointer to frame under consideration
      INTEGER J                  ! Frame under consideration
      INTEGER NFRM               ! Number of frames in frameset
      LOGICAL ISBAS              ! Is this the Base frame?
      LOGICAL ISCUR              ! Is this the Current frame?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get case folded and despaced domain name.
      DMN1 = DOMAIN
      CALL CHR_UCASE( DMN1 )
      CALL CHR_RMBLK( DMN1 )

*  Begin new AST context.
      CALL AST_BEGIN( STATUS )

*  Get the number of frames in the frameset.
      NFRM = AST_GETI( FSET, 'Nframe', STATUS )

*  Go through frameset removing frames.  It is necessary to work backwards
*  so that removing a frame doesn't mess up the numbering.
      DO 1 J = NFRM, 1, -1

*  See if we should remove this frame.
         IF ( J .NE. JKEEP ) THEN
            FRM = AST_GETFRAME( FSET, J, STATUS )
            IF ( AST_GETC( FRM, 'Domain', STATUS ) .EQ. DMN1 ) THEN

*  Check if we are about to remove the Current or Base frame.
               ISCUR = J .EQ. AST_GETI( FSET, 'Current', STATUS )
               ISBAS = J .EQ. AST_GETI( FSET, 'Base', STATUS )

*  Remove the frame.
               CALL AST_REMOVEFRAME( FSET, J, STATUS )

*  Log to user if required.
               CALL MSG_SETC( 'DMN', DMN1 )
               IF ( REPORT )
     :            CALL CCD1_MSG( ' ',
     :         '      Removing existing frame in domain ^DMN', STATUS )

*  Keep track of the position of the retained frame.
               IF ( J .LT. JKEEP ) JKEEP = JKEEP - 1

*  If we have a retained frame and we have just removed the Current or
*  Base frame, set them to it.
               IF ( JKEEP .GT. 0 ) THEN
                  IF ( ISCUR ) CALL AST_SETI( FSET, 'Current', JKEEP,
     :                                        STATUS )
                  IF ( ISBAS ) CALL AST_SETI( FSET, 'Base', JKEEP,
     :                                        STATUS )
               END IF
            END IF
         END IF
 1    CONTINUE

*  End AST context.
      CALL AST_END( STATUS )

      END
* $Id$
