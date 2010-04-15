      SUBROUTINE POL1_CLCAT( IWCS, CI, STATUS )
*+
*  Name:
*     POL1_CLCAT

*  Purpose:
*     Write coordinate information to a catalogue and close it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CLCAT( IWCS, CI, STATUS )

*  Description:
*     This routine write the supplied AST FrameSet in the textual
*     information associated with the supplied catalogue, and then closes
*     the catalogue. NB, at the moment, the CAT library reports errors if
*     you try to store textual information in a FITS file before any rows
*     have been written to the catalogue. For this reason, the WCS
*     information is stored after all rows have been written, just before
*     the catalogue is closed.

*  Arguments:
*     IWCS = INTEGER (Given)
*        An AST pointer to a FrameSet. This should have been obtained using
*        the KPG1_GTWCS subroutine.
*     CI = INTEGER (Given and Returned)
*        A CAT identifier for the catalogue. Returned equal to CAT__NOID.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-JAN-1998 (DSB):
*        Original version.
*     12-NOV-1998 (DSB):
*        Do not store a GRID Frame in the catalogue, since no data grid
*        is defined in a catalogue.
*     17-JUL-2000 (DSB):
*        Reinstate the GRID Frame in the catalogue, to facilitiate
*        alignment with systems that do not know about PIXEL co-ords
*        (eg GAIA).
*     7-FEB-2001 (DSB):
*        Add support for 3D data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER IWCS

*  Arguments Given and Returned:
      INTEGER CI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FRM                ! Pointer to Base Frame
      INTEGER LWCS               ! Pointer to local copy of the FrameSet
*.

*  Copy the supplied FrameSet into the textual information associated
*  with the catalogue (if supplied), and if no error has already occurred ...
      IF( STATUS .EQ. SAI__OK .AND. IWCS .NE. AST__NULL ) THEN

*  Copy the supplied FrameSet so that we do not alter the supplied
*  FrameSet.
         LWCS = AST_COPY( IWCS, STATUS )

*  Routine POL1_MKCAT creates the X, Y (and Z) catalogue columns with names
*  X, Y (and Z). Applications which access the WCS information in the
*  catalogue use the routine POL1_GTCTA to look for a Frame spanned by
*  axes with Symbol attributes equal to the catalogue column names. In
*  order for this to succeed, we ensure that he symbols on axes 1, 2 (and
*  3) of the Base Frame correspond to the names of the catalogue columns
*  (i.e. "X", "Y" (and "Z") ).
         FRM = AST_GETFRAME( LWCS, AST__BASE, STATUS )
         CALL AST_SETC( FRM, 'Symbol(1)', 'X', STATUS )
         CALL AST_SETC( FRM, 'Symbol(2)', 'Y', STATUS )
         IF( AST_GETI( FRM, 'NAXES', STATUS ) .EQ. 3 ) THEN
            CALL AST_SETC( FRM, 'Symbol(3)', 'Z', STATUS )
         END IF
         CALL AST_ANNUL( FRM, STATUS )

*  Add a header to the textual information.
         CALL CAT_PUTXT( CI, 'COMMENT', ' ', STATUS )
         CALL CAT_PUTXT( CI, 'COMMENT', '      Coordinate system '//
     :                   'information follows, stored ', STATUS )
         CALL CAT_PUTXT( CI, 'COMMENT', '      as an AST FrameSet '//
     :                   '(see Starlink User Note 210).', STATUS )
         CALL CAT_PUTXT( CI, 'COMMENT', '      The coordinates '//
     :                   'stored in columns X and Y of the', STATUS )
         CALL CAT_PUTXT( CI, 'COMMENT', '      table refer to the '//
     :                   'PIXEL Frame within this FrameSet.', STATUS )
         CALL CAT_PUTXT( CI, 'COMMENT', ' ', STATUS )

*  Write out the WCS information.
         CALL KPG1_WCATW( LWCS, CI, STATUS )

*  Annul the local copy of the FrameSet.
         CALL AST_ANNUL( LWCS, STATUS )

      END IF

*  Release the catalogue. Do this in a new error reporting context
*  since CAT_TRLSE doesn't check the inherited status before reporting
*  new ones.
      CALL ERR_BEGIN( STATUS )
      CALL CAT_TRLSE( CI, STATUS )
      CALL ERR_END( STATUS )

      END
