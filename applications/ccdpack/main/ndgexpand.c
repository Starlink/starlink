/*
*+
*  Name:
*     ndgexpand

*  Type of Module:
*     C extension to Tcl.

*  Language:
*     ANSI C.

*  Purpose:
*     Expands an NDG group expression to a list of names.

*  Usage:
*     ndgexpand group

*  Description:
*     An NDG group expression is expanded to give a list of the NDF 
*     structures to which it refers.

*  Arguments:
*     group = string
*        A group expression representing an NDG-type query.

*  Return Value:
*     A list of strings, one per (possible) NDF structure in the 
*     NDG-expanded version of group.

*  Authors:
*     MBT: Mark Taylor (STARLINK)

*  History:
*     14-JUN-2001 (MBT):
*        Original version.

*-
*/

#include <stdio.h>
#include <string.h>
#include "sae_par.h"
#include "grp_par.h"
#include "tcl.h"
#include "cnf.h"
#include "ccdaux.h"

/**********************************************************************/
   int NdgexpandCmd( ClientData clientData, Tcl_Interp *interp, int objc,
                     Tcl_Obj *CONST objv[] ) {
/**********************************************************************/

/* Local variables. */
      char *grpex;                  /* Input group expression */
      char name[ GRP__SZNAM + 1 ];  /* Name of an expanded item */
      int i;                        /* Loop variable */
      Tcl_Obj *result;              /* Result returned to Tcl caller */
      F77_INTEGER_TYPE errgid;      /* GRP identifier for inaccessible group */
      F77_INTEGER_TYPE outgid;      /* GRP identifier for output group */       
      F77_INTEGER_TYPE size;        /* Number of items in output group */
      F77_LOGICAL_TYPE flag;        /* NDG trailing character flag */
      DECLARE_CHARACTER( fgrpex, GRP__SZNAM ); /* Fortran version of grpex */
      DECLARE_CHARACTER( fname, GRP__SZNAM); /* Fortran version of name */

/* Local constants. */
      const F77_INTEGER_TYPE one = 1; /* Unity */
      const F77_LOGICAL_TYPE false = F77_FALSE; /* Logical false */

/* Check syntax. */
      if ( objc != 2 ) {
         Tcl_WrongNumArgs( interp, 1, objv, "group-expression" );
         return TCL_ERROR;
      }

/* Get arguments. */
      grpex = Tcl_GetString( objv[ 1 ] );

/* Prepare arguments for passing to NDG. */
      cnfExprt( grpex, fgrpex, fgrpex_length );
      errgid = GRP__NOID;
      outgid = GRP__NOID;

/* Generate a new group to put the results into. */
      STARCALL(
         F77_CALL(ndg_asexp)( CHARACTER_ARG(fgrpex), LOGICAL_ARG(&false),
                              INTEGER_ARG(&errgid), INTEGER_ARG(&outgid), 
                              INTEGER_ARG(&size), LOGICAL_ARG(&flag), 
                              INTEGER_ARG(status)
                              TRAIL_ARG(fgrpex) );
      )

/* Initialise the result object. */
      result = Tcl_NewListObj( 0, (Tcl_Obj **) NULL );

/* Unpack the names from the group into the Tcl result object. */
      STARCALL(
         for ( i = 1; i <= size; i++ ) {
            F77_CALL(grp_get)( INTEGER_ARG(&outgid), INTEGER_ARG(&i),
                               INTEGER_ARG(&one), CHARACTER_ARG(fname),
                               INTEGER_ARG(status)
                               TRAIL_ARG(fname) );
            cnfImprt( fname, fname_length, name );
            Tcl_ListObjAppendElement( interp, result, 
                                      Tcl_NewStringObj( name, -1 ) );
         }

/* Annul the group. */
         F77_CALL(grp_delet)( INTEGER_ARG(&outgid), INTEGER_ARG(status) );
      )

/* Set result and exit successfully. */
      Tcl_SetObjResult( interp, result );
      return TCL_OK;
   }

/* $Id$ */
