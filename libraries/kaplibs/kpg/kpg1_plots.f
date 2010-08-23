      SUBROUTINE KPG1_PLOTS( IPLOT, IPICD, DATREF, ICURR, WDOM, DDOM,
     :                       STATUS )
*+
*  Name:
*     KPG1_PLOTS

*  Purpose:
*     Saves an AST Plot with an AGI DATA picture.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PLOTS( IPLOT, IPICD, DATREF, ICURR, WDOM, DDOM, STATUS )

*  Description:
*     This routine saves a specified Plot and data reference in the AGI
*     database with a specified DATA picture. A specified Frame can be
*     made current before saving the Plot.
*
*     If the supplied Plot contains a "AGI Data" Frame with the
*     Domain given by DDOM in which the axes are scaled and shifted
*     versions of the axes of the AGI world co-ordinate Frame
*     (specified by argument WDOM), then a TRANSFORM structure defining
*     AGI Data co-ordinates is stored with the DATA picture. This is purely
*     for the benefit of non-AST based applications which may use AGI Data
*     co-ordinates (AST-based applications should always use the Plot
*     stored with the picture in preference to the TRANSFORM structure
*     stored in the AGI database).

*  Arguments:
*     IPLOT = INTEGER (Given)
*        An AST pointer to the Plot. The current Frame is unchanged on
*        exit (even if a Frame is specified using ICURR).
*     IPICD = INTEGER (Given)
*        An AGI identifier for the DATA picture.
*     DATREF = CHARACTER * ( * ) (Given)
*        A data reference to store with the picture.
*     ICURR = INTEGER (Returned)
*        The index of a Frame to make current before storing the Plot. The
*        supplied current Frame is used if ICURR is AST__NOFRAME.
*     WDOM = CHARACTER * ( * ) (Given)
*        The Domain name of the Frame correspondoing to AGI world
*        co-ordinates.
*     DDOM = CHARACTER * ( * ) (Given)
*        Domain name of the co-ordinate Frame within IPLOT corresponding
*        to AGI data co-ordinates. "AXIS" is used if a blank value
*        is supplied.
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
*     24-SEP-1998 (DSB):
*        Original version.
*     4-DEC-1998 (DSB):
*        Added facilities for storing a TRANFORM structure with the
*        picture for the benefit of non-AST applications which need access
*        to AGI Data co-ordinates.
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
      INTEGER IPICD
      CHARACTER DATREF*(*)
      INTEGER ICURR
      CHARACTER WDOM*(*)
      CHARACTER DDOM*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ICURR0              ! Index of original current Frame
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If a non-blank data reference string was supplied, put it into the
*  AGI database for the DATA picture.
      IF( DATREF .NE. ' ' ) CALL AGI_PTREF( DATREF, IPICD, STATUS )

*  Save the Plot with the DATA picture in the database. If a Frame has
*  been specified, save the index of the Current Frame in the Plot, and
*  temporarily make the specified Frame the Current Frame.
      IF( ICURR .NE. AST__NOFRAME ) THEN
         ICURR0 = AST_GETI( IPLOT, 'CURRENT', STATUS )
         CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )
      END IF

*  Save the Plot in the AGI database.
      CALL KPG1_GDPUT( IPICD, WDOM, DDOM, IPLOT, STATUS )

*  Re-instate the original Current Frame in the Plot, if required.
      IF( ICURR .NE. AST__NOFRAME ) CALL AST_SETI( IPLOT, 'CURRENT',
     :                                              ICURR0, STATUS )

      END
