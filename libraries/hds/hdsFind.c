/*+
 *  Name:
 *     hdsFind

 *  Purpose:
 *     C-wrapper for the Fortran HDS_FIND routine.

 *  Language:
 *     C

 *  Copyright:
 *     Copyright (C) 2007 Science and Technology Facilities Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *   Authors:
 *      PWD: Peter W. Draper, JAC - University of Durham

 *   History:
 *      17-APR-2007 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif


#include "hds.h"
#include "hds_fortran.h"
#include "f77.h"

F77_SUBROUTINE( hds_find )( CHARACTER( floc1 ),
                            CHARACTER( fname ),
                            CHARACTER( fmode ),
                            CHARACTER( floc2 ),
                            INTEGER( fstatus )
                            TRAIL( floc1 )
                            TRAIL( fname )
                            TRAIL( fmode )
                            TRAIL( floc2 ) );

/*
 *  Name:
 *     hdsFind
 *
 *  Purpose:
 *     Obtain a locator to a named component, the component name may be a
 *     structure (name.component1.component2 etc.).
 *
 *  Params:
 *     loc1 = structure locator
 *     name = component name
 *     mode = access mode (READ, WRITE, UPDATE).
 *     loc2 = component locator
 *     status = global status
 *
 */
void hdsFind( const HDSLoc *loc1, const char *name, const char *mode,
              HDSLoc **loc2, int *status )
{
    DECLARE_CHARACTER(floc1,DAT__SZLOC);
    DECLARE_CHARACTER(floc2,DAT__SZLOC);
    DECLARE_CHARACTER_DYN(fname);
    DECLARE_CHARACTER_DYN(fmode);
    DECLARE_INTEGER(fstatus);

    HDS_EXPORT_CLOCATOR( loc1, floc1, status );
    F77_CREATE_CHARACTER( fname, strlen( name ) );
    F77_EXPORT_CHARACTER( name, fname, fname_length );
    F77_CREATE_CHARACTER( fmode, strlen( mode ) );
    F77_EXPORT_CHARACTER( mode, fmode, fmode_length );
    F77_EXPORT_INTEGER( *status, fstatus );

    F77_LOCK( F77_CALL( hds_find )( CHARACTER_ARG( floc1 ),
                          CHARACTER_ARG( fname ),
                          CHARACTER_ARG( fmode ),
                          CHARACTER_ARG( floc2 ),
                          INTEGER_ARG( &fstatus )
                          TRAIL_ARG( floc1 )
                          TRAIL_ARG( fname )
                          TRAIL_ARG( fmode )
                          TRAIL_ARG( floc2 ) ); )

    F77_FREE_CHARACTER( fname );
    F77_FREE_CHARACTER( fmode );
    HDS_IMPORT_FLOCATOR( floc2, loc2, status );
    F77_IMPORT_INTEGER( fstatus, *status );
    return;
}
