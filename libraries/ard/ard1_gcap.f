      INTEGER FUNCTION ARD1_GCAP(  GRFCON, CAP, VALUE )
*+
*  Name:
*     ARD1_GCAP

*  Purpose:
*     Indicates the capabilities of the ARD grf module

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = ARD1_GCAP(  GRFCON, CAP, VALUE )

*  Description:
*     This is a dummy routine to allow ARD to use the graphics
*     facilities of the AST Plot class for "drawing" curves into integer
*     pixel arrays.

*  Arguments:
*     GRFCON = INTEGER (Given)
*        An AST Object stored using astSetGrfContext, or AST__NULL.
*     CAP = INTEGER (Given)
*        The capability being inquired about.
*     VALUE = INTEGER (Given)
*       A qualifying parameter for the specified capability.

*  Returned Value:
*     A flag indicating if the requested capability is implemented by the
*     ARD grf module.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     15-OCT-2011 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER GRFCON
      INTEGER CAP
      INTEGER VALUE

*  Currently ARD provides none of the additional capabilities that can be
*  used by AST.
      ARD1_GCAP = 0

      END
