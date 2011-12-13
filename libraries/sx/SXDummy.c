#include <dx/dx.h>

Error m_SXDummy( Object *in, Object*out){
/*
*+
*  Name:
*     SXDummy

*  Purpose:
*     a dummy module which can be used to force a macro input to
*     have a particular type

*  Language:
*     ANSI C

*  Syntax:
*     SXDummy( field, group, vector, scalar, integer, vectorlist,
*                       scalarlist, integerlist, string, camera );

*  Classification:
*     Special

*  Description:
*     The SXDummy module is a dummy moudle which does nothing.
*     It may be attached to an "input" module within a macro definition
*     to force the macro input to have the type of the selected
*     SXDummy input.

*  Parameters:
*     field = field (Given)
*        an input field
*     group = group (Given)
*        an input group
*     vector = vector (Given)
*        an input vector
*     scalar = scalar (Given)
*        an input scalar
*     integer =  integer (Given)
*        an input integer
*     vectorlist = vector list (Given)
*        an input vector list
*     scalarlist = scalar list (Given)
*        an input scalar list
*     integerlist =  integer list (Given)
*        an input integer list
*     string =  string (Given)
*        an input string
*     camera =  camera (Given)
*        an input camera

*  Returned Value:
*     OK

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     6-OCT-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

      return( OK );

}
