      SUBROUTINE KPG1_RDTAB( PARAM, CURFRM, LABS, IWCS, NPOS, NAX,
     :                       IPPOS, IPID, TITLE, NAME, STATUS )
*+
*  Name:
*     KPG1_RDTAB

*  Purpose:
*     Reads a set of positions with labels from a CAT catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_RDTAB( PARAM, CURFRM, LABS, IWCS, NPOS, NAX, IPPOS,
*                      IPID, TITLE, NAME, STATUS )

*  Description:
*     This routine is equivalent to KPG1_RDLST except that any labels
*     stored with the positions in the catalogue are returned in a GRP
*     group (see LABS). The labels must be stored in a column called
*     "LABEL". Catalogues containing such labels are written by KPG1_WRTAB.
*
*     See KPG1_RDLST for further information.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     CURFRM = LOGICAL (Given)
*        If .TRUE. the positions read from the catalogue are Mapped
*        into the Current Frame of the associated FrameSet before being
*        returned. Otherwise, they are returned in the Base Frame.
*     LABS = INTEGER (Given and Returned)
*        A GRP identifier for a group containing the values in the LABEL
*        column. If the catalogue contains a LABEL column, then its values
*        are appended to the end of the supplied group. If LABS holds
*        GRP__NOID on entry, then a new GRP group is created and its
*        identifier returned in LABS, but only if the catalogue contains
*        a LABEL column (otherwise the supplied value of GRP__NOID is
*        retained on exit).
*     IWCS = INTEGER (Returned)
*        An AST pointer to the FrameSet read from the catalogue.
*     NPOS = INTEGER (Returned)
*        The number of positions returned.
*     NAX = INTEGER (Returned)
*        The number of axes in the Frame requested by CURFRM.
*     IPPOS = INTEGER (Returned)
*        A pointer to a two-dimensional DOUBLE PRECISION array holding the
*        returned positions. Element (I,J) of this array gives axis J for
*        position I. The first axis will have NPOS elements, and the
*        second will have NAX elements. Should be released using PSX_FREE
*        when no longer needed.
*     IPID = INTEGER (Returned)
*        A pointer to a one-dimensional INTEGER array holding the integer
*        identifiers for the returned positions. The array will have NPOS
*        elements. Should be released using PSX_FREE when no longer needed.
*     TITLE = CHARACTER * ( * ) (Returned)
*        The value of the TITLE parameter in the supplied catalogue.
*        Returned blank if there is no TITLE parameter.
*     NAME = CHARACTER * ( * ) (Returned)
*        The file spec of the catalogue containing the positions list.
*        Not accessed if the declared length is 1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998, 2001, 2004 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     26-OCT-1998 (DSB):
*        Original version.
*     10-DEC-2001 (DSB):
*        Modified to use a default FrameSet if he catalogue does not
*        contain a FrameSet.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL
*     20-NOV-2006 (DSB):
*        Renamed from kpg1_rdlst to kpg1_rdtab, and added argument LABS.
*     4-MAY-2009 (DSB):
*        Call KPG1_RDCAT to do the work.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Arguments Given:
      CHARACTER PARAM*(*)
      LOGICAL CURFRM

*  Arguments Given and Returned:
      INTEGER LABS

*  Arguments Returned:
      INTEGER IWCS
      INTEGER NPOS
      INTEGER NAX
      INTEGER IPPOS
      INTEGER IPID
      CHARACTER TITLE*(*)
      CHARACTER NAME*(*)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Call KPG1_RDCAT to do the work, supplying a NULL KeyMap pointer.
      CALL KPG1_RDCAT( PARAM, CURFRM, AST__NULL, LABS, IWCS, NPOS, NAX,
     :                 IPPOS, IPID, TITLE, NAME, STATUS )


      END
