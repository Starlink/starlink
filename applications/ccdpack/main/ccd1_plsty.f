      SUBROUTINE CCD1_PLSTY( PLOT, SETING, PARAM, STATUS )
*+
*  Name:
*     CCD1_PLSTY

*  Purpose:
*     Apply style settings to an AST Plot object.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_PLSTY( PLOT, SETING, PARAM, STATUS )

*  Description:
*     This routine applies in order default (CCDPACK-style), caller-
*     specified (in the argument SETING), and environment-specified
*     (from the parameter PARAM) style settings to an AST Plot object.
*     It picks up the default settings from a file in the
*     CCDPACK_DIR directory.

*  Arguments:
*     PLOT = INTEGER (Given)
*        The AST identifier for a Plot object whose style settings are to
*        be modified.
*     SETING = CHARACTER * ( * ) (Given)
*        A string passed to AST_SET to apply settings.
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter from which additional style
*        settings are to be taken.  This will be treated as a GRP
*        group, so the normal GRP facilities such as continuation lines
*        and file indirection are available.  If set to an empty
*        string, no attempt will be made to interrogate the parameter
*        system.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This scheme follows, but is much simpler than, the one used in
*     KAPPA.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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
      CHARACTER * ( * ) SETING
      CHARACTER * ( * ) PARAM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      INTEGER GID( 3 )           ! GRP group identifiers
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

*  If required, read elements from the SETING variable into a group.
      IF ( SETING .NE. ' ' ) THEN
         NGRP = NGRP + 1
         CALL GRP_NEW( 'CCDPACK:STYLE', GID( NGRP ), STATUS )
         CALL GRP_GRPEX( SETING, GRP__NOID, GID( NGRP ), NEL,
     :                   NADD, FLAG, STATUS )
      END IF

*  If required, read elements from the ADAM parameter into a group.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      IF ( PARAM .NE. ' ' ) THEN
         NGRP = NGRP + 1
         CALL ERR_MARK
         CALL CCD1_STRGR( PARAM, GRP__NOID, 0, 999, GID( NGRP ), NEL,
     :                    STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            NEL = 0
            CALL ERR_ANNUL( STATUS )
         END IF
         CALL ERR_RLSE
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
      CALL CCD1_GRDEL( GID( 3 ), STATUS )

      END
* $Id$
