      SUBROUTINE CCD1_PLSTY( PLOT, PARAM, STATUS )
*+
*  Name:
*     CCD1_PLSTY

*  Purpose:
*     Apply style settings to an AST Plot object.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_PLSTY( PLOT, PARAM, STATUS )

*  Description:
*     This routine applies default (CCDPACK-style) and, optionally, 
*     environment-specified style settings to an AST Plot object.
*     It picks up the default settings from a file in the 
*     CCDPACK_DIR directory, and gets the environment-specified ones
*     from an ADAM parameter given by the PARAM argument.  If PARAM
*     is an empty string, then only the CCDPACK default style settings
*     are applied.

*  Arguments:
*     PLOT = INTEGER (Given)
*        The AST identifier for a Plot object whose style settings are to
*        be modified.
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter from which additional style 
*        settings are to be taken.  This will be treated as a GRP 
*        group, so the normal GRP facilities such as continuation lines 
*        and file indirection are available.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This scheme follows, but is much simpler than, the one used in 
*     KAPPA.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-2001 (MBT):
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
      INCLUDE 'GRP_PAR'          ! Standard GRP constants
      
*  Arguments Given:
      INTEGER PLOT
      CHARACTER * ( * ) PARAM
      
*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      INTEGER GID( 2 )           ! GRP group identifiers
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable
      INTEGER NADD               ! Number of elements added to group (dummy)
      INTEGER NEL                ! Number of elements in a group
      INTEGER NGRP               ! Number of groups with style elements
      LOGICAL FLAG               ! Continuation character supplied? (dummy)
      CHARACTER * ( 256 ) CCDDIR ! Value of CCDPACK_DIR environment variable
      CHARACTER * ( 256 ) DEFNAM ! Default style file location
      CHARACTER * ( GRP__SZNAM ) STYEL ! Style element string
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      NGRP = 0

*  Check that the AST object is indeed a plot.
      IF ( .NOT. AST_ISAPLOT( PLOT, STATUS ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_PLSTY_NOPLOT', 'CCD1_PLSTY: ' //
     :   'Object passed is not an AST Plot (programming error).',
     :   STATUS )
         GO TO 99
      END IF
      
*  Read elements from the CCDPACK default file into a group.  An error
*  here is not fatal.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL ERR_MARK
      CALL PSX_GETENV( 'CCDPACK_DIR', CCDDIR, STATUS )
      DEFNAM = CCDDIR( :CHR_LEN( CCDDIR ) ) // '/ccdpack_style.def'
      NGRP = NGRP + 1
      CALL GRP_NEW( 'CCDPACK:STYLE', GID( NGRP ), STATUS )
      CALL GRP_GRPEX( '^' // DEFNAM, GRP__NOID, GID( NGRP ), NEL, 
     :                NADD, FLAG, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         NGRP = NGRP - 1
         CALL CCD1_MSG( ' ', '  Failed to load plot style defaults.',
     :                  STATUS )
      END IF
      CALL ERR_RLSE

*  If required, read elements from the ADAM parameter into a group.
      IF ( PARAM .NE. ' ' ) THEN
         NGRP = NGRP + 1
         CALL CCD1_STRGR( PARAM, GRP__NOID, 0, 999, GID( NGRP ), NEL,
     :                    STATUS )
         IF ( NEL .LT. 1 ) NGRP = NGRP - 1
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  For each group in turn, apply the style settings to the plot.
      DO I = 1, NGRP
         CALL GRP_GRPSZ( GID( I ), NEL, STATUS )
         DO J = 1, NEL
            CALL GRP_GET( GID( I ), J, 1, STYEL, STATUS )
            IF ( STYEL .NE. ' ' ) THEN
               CALL AST_SET( PLOT, STYEL( 1 : CHR_LEN( STYEL ) ), 
     :                       STATUS )
            END IF
         END DO
      END DO

*  Error exit label.
 99   CONTINUE

*  Release group resources.
      CALL CCD1_GRDEL( GID( 1 ), STATUS )
      CALL CCD1_GRDEL( GID( 2 ), STATUS )

      END
* $Id$
