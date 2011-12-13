      SUBROUTINE PICVIS( STATUS )
*+
*  Name:
*     PICVIS

*  Purpose:
*     Finds the first unobscured FRAME picture in the graphics database.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PICVIS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application selects the first, i.e. oldest, unobstructed
*     FRAME picture in the graphics database for a graphics device.
*     Unobstructed means that there is no younger picture overlying it
*     either wholly or in part.

*  Usage:
*     picvis [device]

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The graphics workstation. [The current graphics device]

*  Examples:
*     picvis
*        This selects the first unobscured FRAME picture for the
*        current graphics device.
*     picvis xwindows
*        This selects the first unobscured FRAME picture for the
*        xwindows graphics device.

*  Notes:
*     -  An error is returned if there is no unobscured FRAME picture,
*     and the current picture remains unchanged.
*     -  This routine cannot know whether or a picture has been cleared,
*     and hence is safe to reuse, as such information is not stored in
*     the graphics database.

*  Related Applications:
*     KAPPA: PICEMPTY, PICENTIRE, PICGRID, PICLAST, PICLIST, PICSEL.

*  Timing:
*     The execution time is approximately proportional to a linear
*     combination of the number of pictures in the database before the
*     unobstructed FRAME picture is found, and the square of the number
*     of pictures in the database.

*  Copyright:
*     Copyright (C) 1995, 2004 Central Laboratory of the Research
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 October 29 (MJC):
*        Original version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AGI_ERR'          ! AGI system errors

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL OBSCUR             ! Search for an unobscured FRAME
                                 ! picture not completed?
      INTEGER PICIDB             ! Base-picture identifier
      INTEGER PICIDC             ! Graphics' database identifier on
                                 ! input
      INTEGER PICIDE             ! Current picture identifier on exit
      INTEGER PICIDF             ! Current FRAME picture identifier
      INTEGER PICIDS             ! Picture identifier to search from
      INTEGER PICNO              ! Picture number

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the graphics database for the required device.
      CALL AGI_ASSOC( 'DEVICE', 'READ', PICIDC, STATUS )

*  Start a new AGI context.
      CALL AGI_BEGIN

*  Ensure that the current picture on entrance to the application is
*  the current picture on exit, unless a new current picture is
*  selected.
      PICIDE = -1

*  Inquire the base picture for current workstation.
      CALL AGI_IBASE( PICIDB, STATUS )

*  Select this as the start picture for the search.
      PICIDS = PICIDB

*  Initialise picture counter and search flag.
      PICNO = 1
      OBSCUR = .TRUE.

      DO WHILE ( STATUS .EQ. SAI__OK .AND. OBSCUR )

*  Search for the next picture (except for the first occurrence where
*  it is known).  We must first select the base picture so the search
*  includes all pictures.
         CALL AGI_SELP( PICIDB, STATUS )

*  Recall the next FRAME picture, starting the search from the last
*  FRAME picture recalled (or the base picture for the first search).
         CALL AGI_RCS( 'FRAME', PICIDS, PICIDF, STATUS )

*  Handle the case where there is no FRAME picture, or no further frame
*  pictures.  Write a supplementary error message when there is no
*  FRAME picture.
         IF ( STATUS .EQ. AGI__NONAM .OR. STATUS .EQ. AGI__PICNF ) THEN
            IF ( PICNO .EQ. 1 ) THEN
               CALL ERR_REP( 'PICVIS_NOFRAME',
     :          'There are no FRAME pictures in the database for the '/
     :          /'$DEVICE device.', STATUS )
            END IF

         ELSE

*  Increment the picture count.
            IF ( STATUS .EQ. SAI__OK ) PICNO = PICNO + 1

*  Find whether or not this FRAME picture is obscured.  Minus one means
*  compare all subsequent pictures with the current picture.
            CALL AGI_IPOBS( -1, OBSCUR, STATUS )

*  If the picture is unobscured, set the output picture identifier to
*  this FRAME.
            IF ( .NOT. OBSCUR ) THEN
               PICIDE = PICIDF

            ELSE

*  Avoid exhausting the available picture identifiers by annulling each
*  one as we are finished with it unless it is the base picture.
               IF ( PICNO .GT. 2 ) CALL AGI_ANNUL( PICIDS, STATUS )

*  Reset the start of the search to the FRAME picture last recalled.
               PICIDS = PICIDF

            END IF

         END IF

      END DO

*  Close the AGI context.  Make the chosen picture current.
      CALL AGI_END( PICIDE, STATUS )

*  Close the database.
      CALL AGI_ANNUL( PICIDC, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PICVIS_ERR',
     :     'PICVIS: Unable to locate an unobscured FRAME picture in '/
     :     /'the database for the $DEVICE device.', STATUS )
      END IF

      END
