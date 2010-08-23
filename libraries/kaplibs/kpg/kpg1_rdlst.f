      SUBROUTINE KPG1_RDLST( PARAM, CURFRM, IWCS, NPOS, NAX, IPPOS,
     :                       IPID, TITLE, NAME, STATUS )
*+
*  Name:
*     KPG1_RDLST

*  Purpose:
*     Reads a set of positions from a CAT catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_RDLST( PARAM, CURFRM, IWCS, NPOS, NAX, IPPOS, IPID, TITLE,
*                      NAME, STATUS )

*  Description:
*     This routine reads a FrameSet, and a set of positions with
*     associated integer identifiers from a CAT catalogue. The FrameSet
*     should be stored as an AST Dump in the textual information associated
*     with the catalogue. Such catalogues can be created using KPG1_WRLST.
*     If the catalogue does not contain a FrameSet, then a default
*     FrameSet will be used if possible. If the catalogue contains
*     floating point columns named RA and DEC, then the default FrameSet
*     contains a single SkyFrame (Epoch and Equinox are set from the
*     EPOCH and EQUINOX catalogue parameters - if they exist). Otherwise,
*     if the catalogue contains floating point columns named X and Y, then
*     the default FrameSet contains a single two-dimensional Frame with axis symbols X
*     and Y, and Domain GRID. If there is also a column named Z, then
*     the Frame will be three-dimensional, with a Z axis.
*
*     However the FrameSet is obtained, it is assumed that the columns
*     containing the axis values have CAT names equal to the Symbol
*     attribute of the corresponding AST Axis. The catalogue columns from
*     which to read the axis values are chosen by matching column names
*     with Axis Symbols (only columns containing floating point values are
*     considered). Frames are checked in the following order: the Base
*     Frame, the Current Frame, all other Frames in order of increasing
*     Frame index. An error is reported if no Frame has a set of
*     corresponding columns.
*
*     It is assumed that position identifiers are stored in an integer column
*     with name PIDENT. If no such column is found, the returned position
*     identifiers start at 1 and increase monotonically.
*
*     The routine KPG1_RDTAB is like KPG1_RDLST, but provides an extra
*     option to return a group of position labels read from a LABEL column
*     in the catalogue.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     CURFRM = LOGICAL (Given)
*        If .TRUE. the positions read from the catalogue are Mapped
*        into the Current Frame of the associated FrameSet before being
*        returned. Otherwise, they are returned in the Base Frame.
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
*        Re-implemented to call KPG1_RDTAB.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      LOGICAL CURFRM

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

*  Local Variables:
      INTEGER LABS               ! GRP group for position labels
*.

*  Call KPG1_RDTAB to do the work.
      LABS = GRP__NOID
      CALL KPG1_RDTAB( PARAM, CURFRM, LABS, IWCS, NPOS, NAX, IPPOS,
     :                 IPID, TITLE, NAME, STATUS )

*  Delete any unwanted labels group.
      IF( LABS .NE. GRP__NOID ) CALL GRP_DELET( LABS, STATUS )

      END
