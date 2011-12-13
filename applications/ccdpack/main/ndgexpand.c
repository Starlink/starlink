/*
*+
*  Name:
*     ndgexpand

*  Purpose:
*     Expands an NDG group expression to a list of names.

*  Language:
*     ANSI C.

*  Type of Module:
*     C extension to Tcl.

*  Description:
*     An NDG group expression is expanded to give a list of the NDF
*     structures to which it refers.  Either the NDF name alone or
*     the full list of NDG supplementary data can be returned
*     according to the absence or presence of the -sup flag.

*  Arguments:
*     group = string
*        A group expression representing an NDG-type query.

*  Usage:
*     ndgexpand ?-sup? group-expression

*  Flags:
*     -sup
*        If the -sup flag is absent, then the return value is a list of
*        the fully qualified NDF names found in the group expression.
*
*        If the -sup flag is present, then for each NDF a list containing
*        the NDG supplementary information is returned instead.
*        This is the data returned from NDG_GTSUP; for each NDF the
*        list contains six elements:
*           0 - NDF slice specification (if any)
*           1 - HDS path (if any)
*           2 - File type
*           3 - Base file name
*           4 - Directory path
*           5 - Full NDF specification
*
*        Note that the final element of this list is the same as the
*        single value returned with no -sup flag.

*  Return Value:
*     A list of strings, one per (possible) NDF structure in the
*     NDG-expanded version of group.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     DSB: David S Berry (JAC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     14-JUN-2001 (MBT):
*        Original version.
*     15-AUG-2006 (DSB):
*        Changed to use C interface for GRP and NDG.
*     15-JUL-2008 (TIMJ):
*        Tweak NDG interace.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include <stdio.h>
#include <string.h>
#include "sae_par.h"
#include "star/grp.h"
#include "star/ndg.h"
#include "tcl.h"
#include "cnf.h"
#include "ccdaux.h"

/**********************************************************************/
   int NdgexpandCmd( ClientData clientData, Tcl_Interp *interp, int objc,
                     Tcl_Obj *CONST objv[] ) {
/**********************************************************************/

/* Local constants. */
#define NFIELD 6                    /* Number of fields returned by NDG_GTSUP */

/* Local variables. */
      Grp *outgrp;                  /* GRP group for output group */
      Tcl_Obj *ob;                  /* Element of result list */
      Tcl_Obj *result;              /* Result returned to Tcl caller */
      char *grpex;                  /* Input group expression */
      char *pc;                     /* Pointer to character */
      char const *ffields[ NFIELD ];/* Pointers to supplementary data */
      char *pname;                  /* Pointer to GRP element buffer */
      char name[ GRP__SZNAM + 1 ];  /* Name of an expanded item */
      char supdat[ NFIELD ][ GRP__SZNAM + 1 ]; /* Supplementary data */
      int flag;                     /* NDG trailing character flag */
      int gtsup;                    /* Get supplementary information? */
      int i;                        /* Loop variable */
      int j;                        /* Loop variable */
      int nflag;                    /* Number of flags supplied */
      size_t size;                     /* Number of items in output group */
      int unblank;                  /* Pattern string is not empty */

/* Process flags. */
      gtsup = 0;
      nflag = 0;
      if ( objc > 1 + nflag &&
           ! strncmp( Tcl_GetString( objv[ 1 + nflag ] ), "-sup", 3 ) ) {
         gtsup = 1;
         nflag++;
      }

/* Check syntax. */
      if ( objc != 2 + nflag ) {
         Tcl_WrongNumArgs( interp, 1, objv, "?-sup? group-expression" );
         return TCL_ERROR;
      }

/* Get arguments. */
      grpex = Tcl_GetString( objv[ 1 + nflag ] );

/* Check for empty string (ndg_asexp does not like being handed one). */
      unblank = 0;
      for ( pc = grpex; !unblank && (*pc != '\0'); pc++ ) {
         if ( *pc != ' ' ) unblank = 1;
      }

/* If we do not have an empty string, invoke NDG. */
      if ( unblank ) {

/* Prepare arguments for passing to NDG. */
         outgrp = NULL;

/* Generate a new group to put the results into. */
         STARCALL(
            ndgAsexp( grpex, 0, NULL, &outgrp, &size, &flag, status );
         )
      }

/* If the string was blank, fake an empty result. */
      else {
         outgrp = NULL;
         size = 0;
      }

/* Initialise the result object. */
      result = Tcl_NewListObj( 0, (Tcl_Obj **) NULL );

/* Unpack the names from the group into the Tcl result object. */
      for ( i = 1; i <= size; i++ ) {

/* Only the name is required. */
         if ( ! gtsup ) {
            pname = name;
            STARCALL(
               grpGet( outgrp, i, 1, &pname, GRP__SZNAM, status );
            )
            ob = Tcl_NewStringObj( name, -1 );
         }

/* Supplementary information is required. */
         else {
            for( j = 0; j < 6; j++ ) ffields[ j ] = supdat[ j ];
            STARCALL(
               ndgGtsup( outgrp, i, ffields, GRP__SZNAM, status );
            )
            ob = Tcl_NewListObj( 0, (Tcl_Obj **) NULL );
            for ( j = 0; j < NFIELD; j++ ) {
               Tcl_ListObjAppendElement( interp, ob,
                                         Tcl_NewStringObj( ffields[ j ], -1 ) );
            }
         }

/* Append this element of the answer to the result object. */
         Tcl_ListObjAppendElement( interp, result, ob );
      }

/* Annul the group. */
      if ( outgrp ) {
         STARCALL(
            grpDelet( &outgrp, status );
         )
      }

/* Set result and exit successfully. */
      Tcl_SetObjResult( interp, result );
      return TCL_OK;
   }

/* $Id$ */
