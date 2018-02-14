/*
*+
*  Name:
*     dat1CcopyXtoY

*  Purpose:
*     Copy one structure level from version X locator to version Y locator

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     dat1CcopyXtoY(const HDSLoc *locator1X, const HDSLoc *locator2Y, const char *name,
*                   HDSLoc **locator3Y, int *status );

*  Arguments:
*     locator1X = const HDSLoc * (Given)
*        Object locator to copy. In version X.
*     locator2Y = const HDSLoc * (Given)
*        Locator of structure to receive copy of object. Structure is in
*        a version Y file.
*     name = const char * (Given)
*        Name of object when copied into structure.
*     locator3Y = HDSLoc ** (Returned)
*        Locator of newly copied component. Will be in version Y.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Copy an object into a structure and give the new component the
*     specified name. If the source object is a structure, a new structure
*     of the same type and shape is created but the content of the
*     original structure is not copied.

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  Notes:
*     - Use datCcopy directly when the source and target locator
*       are from the same version.
*     - When copying primitive types the data will be mapped and
*       copied from one location to another.
*     - In general this routine will be no less efficient than
*       calling datCcopy between locators of the same data type.

*  History:
*     2014-10-29 (TIMJ):
*        Initial version. Logic copied from HDSv4 datccopy.c
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2014 Cornell University
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program.  If not, see <http://www.gnu.org/licenses/>.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "sae_par.h"
#include "dat1.h"
#include "hds.h"

int
dat1CcopyXtoY(const HDSLoc *locator1X, const HDSLoc *locator2Y, const char *name,
             HDSLoc **locator3Y, int *status ) {
  if (*status != SAI__OK) return *status;
  *locator3Y = dat1CcopyLocXtoY( locator1X, locator2Y, name, NULL, status );
  return *status;
}
