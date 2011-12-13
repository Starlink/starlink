      INTEGER FUNCTION ARD1_GATTR(  GRFCON, ATT, VAL, OLDVAL, PRIM )
*+
*  Name:
*     ARD1_GATTR

*  Purpose:
*     Set/Get a ARD "graphics attribute".

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = ARD1_GATTR(  GRFCON, ATT, VAL, OLDVAL, PRIM )

*  Description:
*     This is a dummy routine to allow ARD to use the graphics
*     facilities of the AST Plot class for "drawing" curves into integer
*     pixel arrays.

*  Arguments:
*     GRFCON = INTEGER (Given)
*        An AST Object stored using astSetGrfContext, or AST__NULL.
*     ATT = INTEGER (Given)
*        An integer identifying the required attribute.
*     VAL = DOUBLE PRECISION (Given)
*        A new value to store for the attribute. If this is AST__BAD
*        no value is stored.
*     OLDVAL = DOUBLE PRECISION (Returned)
*       Returned holding the attribute value.
*     PRIM = INTEGER (Given)
*       The sort of graphics primitive to be drawn with the new attribute.

*  Returned Value:
*     One for success. Zero for failure.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JUN-2001 (DSB):
*        Original version.
*     21-JUN-2007 (DSB):
*        Add GRFCON argument.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER GRFCON
      INTEGER ATT
      DOUBLE PRECISION VAL
      INTEGER PRIM

*  Arguments Returned:
      DOUBLE PRECISION OLDVAL

*  Initialize the returned value to indicate success.
      ARD1_GATTR = 1

      END
