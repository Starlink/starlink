      SUBROUTINE KPG1_ASTTL( IPLOT, IWCS, INDF, STATUS )
*+
*  Name:
*     KPG1_ASTTL

*  Purpose:
*     Sets the Title attribute of a Plot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASTTL( IPLOT, IWCS, INDF, STATUS )

*  Description:
*     This routine sets the Title attribute of an AST Plot. The Title
*     string can be obtained from a variety of places, and the following
*     priority order is used:
*
*     The value of the Title attribute supplied by the user via the
*     application's STYLE parameter is given top priority. If no Title
*     was supplied then the Title component of the NDF is used. If this
*     is blank, then the TItle attribute stored in the current Frame of
*     the NDF's WCS FrameSet is used, if it has been set explicitly (i.e.
*     is not just a default value). Otherwise, the name of the NDF is used.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        An AST pointer for the Plot. The Title attribute of the current
*        Frame will be set on exit.
*     IWCS = INTEGER (Given)
*        An AST pointer for the WCS FrameSet obtained from the NDF.
*     INDF = INTEGER (Given)
*        An NDF identifier for the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
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
*     6-FEB-2006 (DSB):
*        Original version.
*     17-FEB-2006 (DSB):
*        Ensure AST graphical escape sequences are retained in the Title.
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
      INTEGER IPLOT
      INTEGER IWCS
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      LOGICAL KPG1_GETASTFIT     ! Include NDF path in title?

*  Local Variables:
      CHARACTER TTL*80           ! Title string
      CHARACTER PTH*150          ! NDF path
      CHARACTER NEWTTL*255       ! Title string including NDF path
      INTEGER TTLLEN             ! Used length of TTL
      INTEGER OLDESC             ! What to do about escape sequences
      LOGICAL PATH               ! Is the NDF path being used as the title?
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure graphical escape sequences are not removed from the strings
*  returned by AST_GETC.
      OLDESC = AST_ESCAPES( 1, STATUS )

*  Indicate the NDF path has not been used as the Title.
      PATH = .FALSE.

*  First priority is given to values explicitly supplied by the user via
*  the application's STYLE parameter. If such a value was supplied it
*  will currently be assigned to the Title attribute of the Plot. However,
*  simply testing the state of the attribute using AST_TEST will not
*  distinguish between titles supplied via the STYLE parameter a title
*  stored in the  NDF's WCS FrameSet. We assume that if the Title in the
*  Plot is the same as the Title in the WCS FrameSet, then the user has
*  not supplied the Title via the STYLE parameter. If these two titles
*  differ, then return with the Plot unchanged since we will then assume
*  that the Plot's Title was supplied via the STYLE parameter (if the
*  user has explicitly supplied a blank title, then we find a better value).
      TTL = AST_GETC( IPLOT, 'TITLE', STATUS )
      IF( TTL .EQ. ' ' .OR.
     :    TTL .EQ. AST_GETC( IWCS, 'TITLE', STATUS ) .OR.
     :    .NOT. AST_TEST( IPLOT, 'TITLE', STATUS ) ) THEN

*  If the user has not supplied a title via the STYLE parameter, or has
*  supplied a blank title, then the next priority is to use the NDF's Title
*  component, if set.
         TTL = ' '
         CALL NDF_CGET( INDF, 'TITLE', TTL, STATUS )

*  If the NDF has no Title component, use the WCS FrameSet Title if it has
*  been set explicitly (i.e. is not a default value).
         IF( TTL .EQ. ' ' .AND. AST_TEST( IWCS, 'TITLE', STATUS ) ) THEN
            TTL = AST_GETC( IWCS, 'TITLE', STATUS )
         END IF

*  If we still have no title, use the NDF path. Ensure we know the length
*  of the title string.
         IF( TTL .EQ. ' ' ) THEN
            CALL KPG1_NDFNM( INDF, TTL, TTLLEN, STATUS )
            PATH = .TRUE.
         ELSE
            TTLLEN = CHR_LEN( TTL )
         END IF

*  Set the Plot Title.
         CALL AST_SETC( IPLOT, 'TITLE', TTL( : TTLLEN ), STATUS )

      END IF

*  If the NDF path has not been used as the title, see if the NDF path
*  should be added to the title. The user can control this by including
*  the kappa pseudo-attribute FileInTitle within the value supplied for
*  STYLE.
      IF( .NOT. PATH ) THEN
         IF( KPG1_GETASTFIT() ) THEN

*  Get the NDF path.
            CALL KPG1_NDFNM( INDF, PTH, TTLLEN, STATUS )

*  Construct a new title which includes the original title and the NDF
*  path, with suitable AST graphical escape sequences to ensure the
*  NDF path comes out as a second line in the title, left justified with
*  the first line.
            NEWTTL = '%h+'
            TTLLEN = 3
            CALL CHR_APPND( TTL, NEWTTL, TTLLEN )
            CALL CHR_APPND( '%v100+%g+%s70+[', NEWTTL, TTLLEN )
            TTLLEN = TTLLEN + 1
            CALL CHR_APPND( PTH, NEWTTL, TTLLEN )
            CALL CHR_APPND( ' ]%s+', NEWTTL, TTLLEN )

*  Set the NEW Plot Title.
            CALL AST_SETC( IPLOT, 'TITLE', NEWTTL( : TTLLEN ), STATUS )

         END IF
      END IF

*  Reinstate the original behaviour of AST with regard to graphical escape
*  sequences in the strings returned by AST_GETC.
      OLDESC = AST_ESCAPES( OLDESC, STATUS )

      END
