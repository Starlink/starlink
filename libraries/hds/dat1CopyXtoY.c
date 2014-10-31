/*
*+
*  Name:
*     dat1CopyXtoY

*  Purpose:
*     Copy structures and data from version X locator to version Y locator

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     dat1CopyXtoY(const HDSLoc *locatorX, const HDSLoc *locatorY, const char *name_c, int *status);

*  Arguments:
*     locatorX = const HDSLoc * (Given)
*        Locator of object to copy. In version X.
*     locatorY = const HDSLoc * (Given)
*        Locator of structure to receive the copy.
*        Structure is in a version Y file.
*     name = const char * (Given)
*        Name of newly copied object.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Recursively copy an object from a version X locator to a version
*     Y structure locator. The complete object is copied one item at
*     a time. This is far less efficient than using the native HDSv4
*     or HDSv5 datCopy implementations and should only be used when
*     the two input locators are associated with different HDS versions.

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  Notes:
*     - This is not an efficient copy. The components are traversed
*       one at a time and created in the target. Primitives will be mapped
*       and copied one primitive at a time.

*  History:
*     2014-10-28 (TIMJ):
*        Initial version
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

#include "dat1.h"
#include "sae_par.h"
#include "hds.h"
#include "dat_err.h"
#include "hds_types.h"
#include "ems.h"

int
dat1CopyXtoY(const HDSLoc *locatorX, const HDSLoc *locatorY, const char *name_c, int *status) {
  hdsbool_t isstruc;
  HDSLoc * outloc = NULL;
  if (*status != SAI__OK) return *status;

  /* First simply create the next level down */
  outloc = dat1CcopyLocXtoY( locatorX, locatorY, name_c, &isstruc, status );

  /* Primitives will have been copied already, just need to copy structures */
  if (isstruc) dat1CopyStrucXtoY( locatorX, outloc, status );
  if (outloc) datAnnul(&outloc, status);
  return *status;
}

