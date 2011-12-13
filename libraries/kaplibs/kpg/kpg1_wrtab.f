      SUBROUTINE KPG1_WRTAB( PARAM, ARRDIM, NPOS, NAX, POS, IFRM, IWCS,
     :                       TITLE, ID0, IDENTS, LABS, HIST, NULL,
     :                       STATUS )
*+
*  Name:
*     KPG1_WRTAB

*  Purpose:
*     Writes a set of positions to a text file as a CAT catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_WRTAB( PARAM, ARRDIM, NPOS, NAX, POS, IFRM, IWCS, TITLE,
*                      ID0, IDENTS, LABS, HIST, NULL, STATUS )

*  Description:
*     This routine is equivalent to KPG1_WRLST, except that it provides
*     the option of storing a textual label with each position, via
*     the extra argument LABS.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     ARRDIM = INTEGER (Given)
*        The size of the first dimension of the positions array. This must
*        be larger than or equal to NPOS.
*     NPOS = INTEGER (Given)
*        The number of positions to store in the file.
*     NAX = INTEGER (Given)
*        The number of axes in the Frame specified by IFRM.
*     POS( ARRDIM, NAX ) = DOUBLE PRECISION (Given)
*        The positions to store in the file. POS( I, J ) should give the
*        axis J value for position I.
*     IFRM = INTEGER (Given)
*        The index of the Frame within IWCS to which the supplied
*        positions relate. Can be AST__BASE or AST__CURRENT.
*     IWCS = INTEGER (Given)
*        A pointer to an AST FrameSet to store with the positions.
*     TITLE = CHARACTER * ( * ) (Given)
*        A title to store at the top of the text file. Ignored if blank.
*     ID0 = INTEGER (Given)
*        The integer identifier value to associate with the first
*        supplied position. Identifiers for subsequent positions increase
*        by 1 for each position. If this is supplied less than or equal
*        to zero, then its value is ignored and the identifiers supplied
*        in array IDENTS are used instead.
*     IDENTS( NPOS ) = INTEGER (Given)
*        The individual integer identifiers to associate with each
*        position. Only accessed if ID0 is less than or equal to zero.
*     LABS = INTEGER (Given)
*        A GRP group identifier containing the labels to be associated
*        with the positions. The number of elements in this group should
*        be equal to NPOS. If GRP__NOID is supplied, no label column will
*        be created.
*     HIST = INTEGER (Given)
*        A GRP group identifier containing history text to store with the
*        catalogue.  If GRP__NOID is supplied, no history information
*        will be stored with the catalogue.
*     NULL = LOGICAL (Given)
*        Is the user allowed to supply a null value? If so, the error
*        status will be annulled before returning.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998, 2001, 2002, 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-NOV-2006 (DSB):
*        Original version.
*     25-JAN-2007 (DSB):
*        Added argument HIST.
*     27-APR-2009 (DSB):
*        Call KPG1_WRCAT to do the work.
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
      CHARACTER PARAM*(*)
      INTEGER ARRDIM
      INTEGER NPOS
      INTEGER NAX
      DOUBLE PRECISION POS( ARRDIM, NAX )
      INTEGER IFRM
      INTEGER IWCS
      CHARACTER TITLE*(*)
      LOGICAL NULL
      INTEGER ID0
      INTEGER IDENTS( NPOS )
      INTEGER LABS
      INTEGER HIST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IBASE              ! Index of base Frame
      INTEGER ICURR              ! Index of current Frame
      INTEGER IDEF               ! Index of default catalogue Frame
      INTEGER IPW                ! Pointer to work space
      INTEGER MAP                ! AST Pointer to Mapping
      INTEGER NBAX               ! No. of axes in BASE FRAME
*.

*  Call KPG1_WRCAT to do the work, suppling a null value for KEYMAP.
      CALL KPG1_WRCAT( PARAM, ARRDIM, NPOS, NAX, POS, IFRM, IWCS, TITLE,
     :                 ID0, IDENTS, AST__NULL, LABS, HIST, NULL,
     :                 STATUS )

      END
