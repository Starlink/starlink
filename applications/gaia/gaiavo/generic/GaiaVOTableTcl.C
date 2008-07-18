/*+
 *   Name:
 *      GaiaVOTableTcl

 *   Purpose:
 *      Tcl interface to VOTable facilities.

 *   Language:
 *      C++

 *   Authors:
 *      PWD: Peter W. Draper, JAC - University of Durham

 *  Copyright:
 *     Copyright (C) 2008 Science and Technology Facilities Council.
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
 *     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
 *     02111-1307, USA

 *   History:
 *     18-JUL-2008 (PWD):
 *        Original version.
 *     {enter_changes_here}
 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <tcl.h>
#include <VOTable.h>

using namespace gaia;

/* Local prototypes */
/* ================ */
static int GaiaVOTableTclOpen( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] );

/* Register all the commands (local entry point). */
/* ============================================== */
int GaiaVOTable_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "gaiavotable::open", GaiaVOTableTclOpen,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );
    return TCL_OK;
}


/**
 * Import a VOTable back from a Tcl_Obj. Returns TCL_ERROR if fails.
 */
static int importVOTableHandle( Tcl_Interp *interp, Tcl_Obj *obj, 
                         VOTable *& table  )
{
    Tcl_Obj *resultObj;
    long adr;
    
    if ( Tcl_GetLongFromObj( interp, obj, &adr ) != TCL_OK ) {
        
        /* Replace result with our message */
        resultObj = Tcl_GetObjResult( interp );
        Tcl_SetStringObj( resultObj, Tcl_GetString( obj ), -1 );
        Tcl_AppendStringsToObj( resultObj, " : is not a VOTable" ,
                                (char *)NULL );
        return TCL_ERROR;
    }
    
    /* Cast to a VOTable. */
    table = reinterpret_cast<VOTable *>( adr );
    return TCL_OK;
}

/**
 * Export a VOTable to an address.
 */
static long exportVOTableHandle( VOTable *table )
{
    return reinterpret_cast<long>( table );
}

/**
 * Open a VOTable. The result is the address of the object.
 */
static int GaiaVOTableTclOpen( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *resultObj;
    char *name;
    VOTable *table;
    
    /*  Check arguments, only allow one, the name of the VOTable. */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "votable" );
        return TCL_ERROR;
    }
    name = Tcl_GetString( objv[1] );
    
    /*  Create instance of VOTable and open the table. */
    table = new gaia::VOTable();
    if ( table->open( name ) ) {
        resultObj = Tcl_GetObjResult( interp );
        Tcl_SetLongObj( resultObj, exportVOTableHandle( table ) );
        return TCL_OK;
    }
    Tcl_SetResult( interp, const_cast<char *>( "Failed to open VOTable" ), 
                   TCL_VOLATILE );
    return TCL_ERROR;
}
