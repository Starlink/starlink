#include "f77.h"
#include "ast.h"
#include "star/grp.h"
#include "kaplibs_private.h"

F77_SUBROUTINE(kpg1_kygrp)( INTEGER(KEYMAP), INTEGER(IGRP), INTEGER(STATUS) ) {
/*
*+
*  Name:
*     KPG1_KYGRP

*  Purpose:
*     Creates an GRP group holding keyword/value pairs read from an AST
*     KeyMap.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL KPG1_KYGRP( KEYMAP, IGRP, STATUS )

*  Description:
*     This function is the inverse of KPG1_KYMAP. It extracts the values
*     from the supplied AST KeyMap and creates a set of "name=value" strings
*     which it appends to a supplied group (or creates a new group). If
*     the KeyMap contains nested KeyMaps, then the "name" associated with
*     each primitive value stored in the returned group is a hierarchical
*     list of component names separated by dots.

*  Arguments:
*     KEYMAP = INTEGER (Given)
*        An AST pointer to the existing KeyMap. Numerical entries which
*        have bad values (VAL__BADI for integer entries or VAL__BADD for
*        floating point entries) are not copied into the group.
*     IGRP = INTEGER (Returned)
*        A GRP identifier for the group to which to append the "name=value"
*        strings read from the KeyMap. A new group is created if GRP__NOID
*        is supplied.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     7-NOV-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/
   GENPTR_INTEGER(KEYMAP)
   GENPTR_INTEGER(IGRP)
   GENPTR_INTEGER(STATUS)

   AstKeyMap *keymap;
   Grp *grp;
   int status;

   F77_IMPORT_INTEGER( *STATUS, status );

   keymap = astI2P( *KEYMAP );
   grp = (Grp *) grpF2C( *IGRP, &status );

   kpg1Kygp1( keymap, &grp, NULL, &status );

   grp = grpFree( grp, &status );

   F77_EXPORT_INTEGER( grpC2F( grp, &status ), *IGRP );
   F77_EXPORT_INTEGER( status, *STATUS );

}
