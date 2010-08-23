      SUBROUTINE KPG1_PLOTN( IWCS, DOMAIN, IPICD, QUIET, IPLOT, NFRM,
     :                       STATUS )
*+
*  Name:
*     KPG1_PLOTN

*  Purpose:
*     Create a new DATA picture and ensure there is an AST Plot for it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PLOTN( IWCS, DOMAIN, IPICD, IPLOT, NFRM, STATUS )

*  Description:
*     This routine createa an AST Plot describing a given DATA picture.
*     If a FrameSet is supplied (IWCS), then it is merged with the
*     Plot. An error is reported if the FrameSet and Plot cannot be
*     aligned.
*
*     On exit, the current PGPLOT viewport corresponds to area occupied by
*     the DATA picture. The bounds of the PGPLOT window produce a world
*     co-ordinate system within the viewport corresponding to the Base
*     Frame in the returned Plot (i.e. millimetres from the bottom left
*     corner of the view surface - NOT pixel co-ordinates). Note, this is
*     different to the world co-ordinate system stored in the AGI database
*     with the DATA picture.

*  Arguments:
*     IWCS = INTEGER (Given)
*        An AST pointer to a FrameSet. This may be AST__NULL. The Current
*        and Base Frames are unchanged on exit.
*     DOMAIN = CHARACTER * ( * ) (Given)
*        The Domain in which AGI world co-ordinates within the specified
*        DATA picture live. Alignment of the Plot and FrameSet will be
*        attampted in this Domain if it is not possible in the Current
*        Frame of the FrameSet.
*     IPICD = INTEGER (Given)
*        An AGI identifier for the DATA picture.
*     QUIET = LOGICAL (Given)
*        Suppress message identifying the Domain in which alignment occurs?
*     IPLOT = INTEGER (Returned)
*        An AST pointer to the new Plot. Returned equal to AST__NULL if
*        an error occurs.
*     NFRM = INTEGER (Returned)
*        A  Frame with index I in the supplied FrameSet (IWCS) will have index
*        ( I + NFRM ) in the returned Plot (IPLOT). Returned equal to zero
*        if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1998 (DSB):
*        Original version.
*     {enter_changes_here}

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
      CHARACTER DOMAIN*(*)
      INTEGER IPICD
      LOGICAL QUIET

*  Arguments Returned:
      INTEGER IPLOT
      INTEGER NFRM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      INTEGER FRM                ! Pointer to AGI world co-ordinates Frame
      INTEGER ICURR              ! Index of Current Frame in FrameSet
      INTEGER IFRM               ! Index of AGI world co-ordinates Frame
*.

*  Initialise returned values.
      IPLOT = AST__NULL
      NFRM = 0

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the DATA picture does not have a Plot stored with it, KPG1_GDGET
*  will create a default Plot containing a GRAPHIS Frame, and a Frame
*  corresponding to AGI world co-ordinates. The AGI world co-ordinate
*  Frame is normally just a default two-dimensional Frame with Domain AGI_WORLD, but
*  we can over-ride this by supplying an alternative Frame containing
*  more appropriate attribute settings. If we do not know the Domain of the
*  AGI world co-ordinate system, just use the default Frame. This is
*  indicated by setting the FRM pointer to AST__NULL.
      FRM = AST__NULL
      IFRM = AST__NOFRAME
      IF( DOMAIN .NE. ' ' ) THEN

*  If we have a Domain, look for a Frame with the specified Domain within
*  any supplied FrameSet.
         IF( IWCS .NE. AST__NULL ) THEN
            CALL KPG1_ASFFR( IWCS, DOMAIN, IFRM, STATUS )

*  If one was found, use it to represent AGI world co-ordinates.
            IF( IFRM .NE. AST__NOFRAME ) THEN
               FRM = AST_GETFRAME( IWCS, IFRM, STATUS )
            END IF

         END IF

*  If we have a Domain name, but no Frame with this Domain was supplied,
*  use a default two-dimensional Frame with the specified Domain name.
         IF( FRM .EQ. AST__NULL ) THEN
            FRM = AST_FRAME( 2, ' ', STATUS )
            CALL AST_SETC( FRM, 'DOMAIN', DOMAIN( : CHR_LEN( DOMAIN ) ),
     :                     STATUS )
         END IF

      END IF

*  Get the Plot associated with the DATA picture. The above Frame is used
*  to represent AGI world co-ordinates if there is no Plot stored with the
*  picture in the AGI database.
      CALL KPG1_GDGET( IPICD, FRM, .FALSE., IPLOT, STATUS )

*  Save the number of Frames in the Plot before any supplied FrameSet is
*  added into it.
      NFRM = AST_GETI( IPLOT, 'NFRAME', STATUS )

*  Merge any supplied FrameSet into the Plot.
      IF( IWCS .NE. AST__NULL ) THEN

*  First set the Current Frame in the FrameSet to the AGI world
*  co-ordinate Frame so that alignment will occur in this Frame by prefernce.
         IF( IFRM .NE. AST__NOFRAME ) THEN
            ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )
            CALL AST_SETI( IWCS, 'CURRENT', IFRM, STATUS )
         END IF

*  Merge the FrameSet and Plot.
         CALL KPG1_ASMRG( IPLOT, IWCS, DOMAIN, QUIET, 0, STATUS )

*  Re-instate the original Current Frame in the FrameSet and Plot.
         IF( IFRM .NE. AST__NOFRAME ) THEN
            CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )
            CALL AST_SETI( IPLOT, 'CURRENT', ICURR + NFRM, STATUS )
         END IF

      END IF

*  Annul the default AGI world co-ordinate Frame pointer.
      IF( FRM .NE. AST__NULL ) CALL AST_ANNUL( FRM, STATUS )

*  Annul the returned Plot if an error has occurred.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL AST_ANNUL( IPLOT, STATUS )
         NFRM = 0
      END IF

      END
