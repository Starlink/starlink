/*
*+
*  Name:
*     dat1.h

*  Purpose:
*     Private header file for HDS wrapper

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C Header File

*  Invocation:
*     #include "dat1.h"

*  Description:
*     Private header file for the HDS wrapper library.

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

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

#if !defined( DAT_1_H_INCLUDED ) /* dat1.h already included? */
#define DAT_1_H_INCLUDED 1

#include "dat_par.h"

/* Wrapper definition of an HDS locator struct. The first element */
/* must be present in both v4 and v5 implementations.             */
/* We can not use an opaque struct internally but we can also not */
/* use a full definition of the struct. Just enough for the       */
/* compiler to be able to know how to read the version.           */
#define HDS_USE_INTERNAL_STRUCT 1
typedef struct LOC { int hds_version; } HDSLoc;
#include "hds_types.h"

HDSLoc *
dat1_import_floc ( const char flocator[DAT__SZLOC], int loc_length, int * status);

int
dat1CopyXtoY(const HDSLoc *locatorX, const HDSLoc *locatorY, const char *name_c, int *status);

int
dat1CcopyXtoY(const HDSLoc *locator1X, const HDSLoc *locator2Y, const char *name,
              HDSLoc **locator3Y, int *status );

HDSLoc *
dat1CcopyLocXtoY(const HDSLoc *locatorX, const HDSLoc *locatorY, const char *name,
                 hdsbool_t * struc, int *status );

int
dat1CopyPrimXtoY( const HDSLoc *locatorX, HDSLoc *locatorY, int *status );

int
dat1CopyStrucXtoY( const HDSLoc *locatorX, const HDSLoc *locatorY,
                   int *status );

int hds1TuneWrapper ( const char * param_str, int value, int *status );

int
hds1GtuneWrapper(const char *param_str, int *value, int *status);

hdsbool_t hds1UseVersion5();
hdsbool_t hds1V4LockError();

void dat1GetEnv( const char *varname, int def, int *val );

#endif

