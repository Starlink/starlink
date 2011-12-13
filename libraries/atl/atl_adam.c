/*
*  Name:
*     atl_adam.c

*  Purpose:
*     Implement the C interface to the ADAM atl library.

*  Description:
*     This module implements C-callable wrappers for the public ADAM
*     routines in the atl library. The interface to these wrappers
*     is defined in atl.h.

*  Authors:
*     DSB: David S Berry
*     {enter_new_authors_here}

*  History:
*     27-NOV-2009 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*/

/* Header files. */
/* ============= */
#include "f77.h"
#include "sae_par.h"
#include "par_par.h"
#include "ast.h"
#include "atl.h"

/* Wrapper function implementations. */
/* ================================= */


F77_SUBROUTINE(atl_creat)( CHARACTER(PARAM), INTEGER(IAST), INTEGER(STATUS) TRAIL(PARAM) );

void atlCreat( const char *param, AstObject *iast, int *status ){
   DECLARE_CHARACTER(PARAM,PAR__SZNAM);
   DECLARE_INTEGER(IAST);
   DECLARE_INTEGER(STATUS);

   if ( !astOK ) return;

   F77_EXPORT_CHARACTER( param, PARAM, PAR__SZNAM );
   F77_EXPORT_INTEGER( astP2I( iast ), IAST );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(atl_creat)( CHARACTER_ARG(PARAM), INTEGER_ARG(&IAST),
                        INTEGER_ARG(&STATUS) TRAIL_ARG(PARAM) ); )

   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}


