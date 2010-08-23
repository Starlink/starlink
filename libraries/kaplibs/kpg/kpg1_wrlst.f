      SUBROUTINE KPG1_WRLST( PARAM, ARRDIM, NPOS, NAX, POS, IFRM, IWCS,
     :                       TITLE, ID0, IDENTS, NULL, STATUS )
*+
*  Name:
*     KPG1_WRLST

*  Purpose:
*     Writes a set of positions to a text file as a CAT catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_WRLST( PARAM, ARRDIM, NPOS, NAX, POS, IFRM, IWCS, TITLE,
*                      ID0, IDENTS, NULL, STATUS )

*  Description:
*     This routine saves a set of positions in a text file as a CAT
*     catalogue (see SUN/181). Information describing associated co-ordinate
*     Frames can also be stored in the file as textual information, allowing
*     subsequent applications to interpret the positions. Files written with
*     this routine can be read using KPG1_RDLST (and also XCATVIEW etc.).
*
*     The positions are stored in the file in a Frame selected by the
*     user using hardwired parameters CATFRAME and CATEPOCH. This Frame
*     defaults to a SKY Frame if present, otherwise a PIXEL Frame if present,
*     otherwise the original Base Frame within the supplied FrameSet. The
*     positions can be supplied within any of the Frames in the FrameSet
*     and will be Mapped into the required Frame if necessary.
*
*     If the ID atttribute of the FrameSet is set to "FIXED_BASE", then
*     the user is not allowed to change the base Frame using parameters
*     CATFRAME and CATEPOCH.
*
*     See also KPG1_WRTAB, which is like this routine but allows a
*     textual label to be associated with each position.

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
*     NULL = LOGICAL (Given)
*        Is the user allowed to supply a null value? If so, the error
*        status will be annulled before returning.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998, 2001, 2002, 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     15-SEP-1998 (DSB):
*        Original version.
*     13-DEC-2001 (DSB):
*        Added facility to specify the Frame in which to store positions
*        in the catalogue.
*     11-JUN-2002 (DSB):
*        Remove code which pre-opened the output catalogue since CAT does
*        not allow a catalogue to be opened more than once. This gave
*        problems when using ICL due to FITS files not being closed
*        properly.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL.
*     1-NOV-2005 (DSB):
*        Allow the calling application to supress the use of the CATEPOCH
*        and CATFRAME parameters by setting the FrameSet ID attribute to
*        "FIXED_BASE".
*     20-NOV-2006 (DSB)
*        Re-implement by calling KPG1_WRTAB.
*     25-JAN-2007 (DSB)
*        Supply a value for argument HIST when calling KPG1_WRTA2.
*     15-FEB-2007 (DSB)
*        Correct order of arguments passed to KPG1_WRTAB.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants

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

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Call KPG1_WRTAB to do the work, specifiying GRP__NOID for the groups
*  containing the position labels and history text.
      CALL KPG1_WRTAB( PARAM, ARRDIM, NPOS, NAX, POS, IFRM, IWCS,
     :                 TITLE, ID0, IDENTS, GRP__NOID, GRP__NOID, NULL,
     :                 STATUS )

      END
