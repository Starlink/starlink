      SUBROUTINE CCD1_DMPRG( FSET, DOMAIN, JKEEP, STATUS )
*+
*  Name:
*     CCD1_DMPRG

*  Purpose:
*     Purge a frameset of frames in a given domain.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_DMPRG( FSET, DOMAIN, JKEEP, STATUS )

*  Description:
*     This routine removes all frames from a frameset which have the
*     domain name supplied.  Optionally, the index of one domain to
*     exempt from this process may be supplied.  For each domain 
*     removed, a message is logged through the CCD logging system.

*  Arguments:
*     FSET = INTEGER (Given)
*        AST pointer to the frameset.  The frameset may be modified on
*        exit, but the Current frame will not be altered, except in the
*        case that the Current frame is one of the ones removed.
*     DOMAIN = CHARACTER * ( * ) (Given)
*        Name of the domain to purge.  Spaces and case-sensitivity are
*        ignored.
*     JKEEP = INTEGER (Given and Returned)
*        Any frame with this index will not be deleted, even if it has
*        a Domain of DOMAIN.  On exit, this value will refer to the same
*        frame as on entry, although if frames with indices below its
*        value have been removed, its numerical value will have changed.
*
*        If no frames are to be exempted, this should be set to a 
*        non-positive integer.  In this case, it is allowed to supply
*        a constant rather than a variable for this parameter.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-APR-1999 (MBT):
*        Original version.
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
      INTEGER JKEEP
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( AST__SZCHR ) DMN1 ! Case folded and despaced domain name
      INTEGER FRM                ! AST pointer to frame under consideration
      INTEGER J                  ! Frame under consideration
      INTEGER NFRM               ! Number of frames in frameset
      
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
         IF ( J .NE. JKEEP ) THEN
            FRM = AST_GETFRAME( FSET, J, STATUS )
            IF ( AST_GETC( FRM, 'Domain', STATUS ) .EQ. DMN1 ) THEN
               CALL AST_REMOVEFRAME( FSET, J, STATUS )
               CALL MSG_SETC( 'DMN', DMN1 )
               CALL CCD1_MSG( ' ', 
     :         '      Removing existing frame in domain ^DMN', STATUS )
               IF ( J .LT. JKEEP ) JKEEP = JKEEP - 1
            END IF
         END IF
 1    CONTINUE

*  End AST context.
      CALL AST_END( STATUS )

      END
* $Id$
