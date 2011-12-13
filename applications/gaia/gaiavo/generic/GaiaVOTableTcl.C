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
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

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
static int GaiaVOTableTclClose( ClientData clientData, Tcl_Interp *interp,
                                int objc, Tcl_Obj *CONST objv[] );
static int GaiaVOTableTclNumTables( ClientData clientData, Tcl_Interp *interp,
                                    int objc, Tcl_Obj *CONST objv[] );
static int GaiaVOTableTclList( ClientData clientData, Tcl_Interp *interp,
                                int objc, Tcl_Obj *CONST objv[] );
static int GaiaVOTableTclListHeadings( ClientData clientData, Tcl_Interp *interp,
                                       int objc, Tcl_Obj *CONST objv[] );
static int GaiaVOTableTclOpen( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] );
static int GaiaVOTableTclInfo( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] );
static int GaiaVOTableTclRead( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] );
static int GaiaVOTableTclSave( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] );

/* Register all the commands (local entry point). */
/* ============================================== */
int GaiaVOTable_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "gaiavotable::close", GaiaVOTableTclClose,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );
    Tcl_CreateObjCommand( interp, "gaiavotable::numtables",
                          GaiaVOTableTclNumTables, (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );
    Tcl_CreateObjCommand( interp, "gaiavotable::list", GaiaVOTableTclList,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );
    Tcl_CreateObjCommand( interp, "gaiavotable::listheadings",
                          GaiaVOTableTclListHeadings,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );
    Tcl_CreateObjCommand( interp, "gaiavotable::info", GaiaVOTableTclInfo,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );
    Tcl_CreateObjCommand( interp, "gaiavotable::open", GaiaVOTableTclOpen,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );
    Tcl_CreateObjCommand( interp, "gaiavotable::read", GaiaVOTableTclRead,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );
    Tcl_CreateObjCommand( interp, "gaiavotable::save", GaiaVOTableTclSave,
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

    if ( Tcl_GetLongFromObj( interp, obj, &adr ) != TCL_OK ||
         adr == 0 ) {

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

/**
 * Read a VOTable from a string. The result is the address of the object.
 */
static int GaiaVOTableTclRead( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *resultObj;
    char *content;
    VOTable *table;

    /*  Check arguments, only allow one, the VOTable content. */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "votable_string" );
        return TCL_ERROR;
    }
    content = Tcl_GetString( objv[1] );

    /*  Create instance of VOTable and open the table. */
    table = new gaia::VOTable();
    if ( table->read( content ) ) {
        resultObj = Tcl_GetObjResult( interp );
        Tcl_SetLongObj( resultObj, exportVOTableHandle( table ) );
        return TCL_OK;
    }
    Tcl_SetResult( interp, const_cast<char *>( "Failed to read VOTable" ),
                   TCL_VOLATILE );
    return TCL_ERROR;
}

/**
 * Close a VOTable.
 */
static int GaiaVOTableTclClose( ClientData clientData, Tcl_Interp *interp,
                                int objc, Tcl_Obj *CONST objv[] )
{
    VOTable *table;

    /*  Check arguments, only allow one, the address of the VOTable. */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "votable_reference" );
        return TCL_ERROR;
    }

    /*  Import the table. */
    if ( importVOTableHandle( interp, objv[1], table ) == TCL_OK ) {

        /*  Close by deleting the object. */
        delete table;
        return TCL_OK;
    }

    Tcl_SetResult( interp, const_cast<char *>( "Failed to close VOTable" ),
                   TCL_VOLATILE );
    return TCL_ERROR;
}

/**
 * Output a list of the names of the various TABLEs in the VOTABLE.
 */
static int GaiaVOTableTclList( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] )
{
    VOTable *table;
    ostringstream str;

    /*  Check arguments, only allow one, the address of the VOTable. */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "votable_reference" );
        return TCL_ERROR;
    }

    /*  Import the table. */
    if ( importVOTableHandle( interp, objv[1], table ) == TCL_OK ) {
        table->list( str );
        Tcl_SetResult( interp, const_cast<char *>( str.str().c_str() ),
                       TCL_VOLATILE );
        return TCL_OK;
    }
    return TCL_ERROR;
}

/**
 * Output a list of the headings of the data that will be returned by
 * the "list" command.
 */
static int GaiaVOTableTclListHeadings( ClientData clientData, Tcl_Interp *interp,
                                       int objc, Tcl_Obj *CONST objv[] )
{
    char *headings =
        const_cast<char *>( "Table Name Description ID Rows(est) Columns" );
    Tcl_SetResult( interp, headings, TCL_VOLATILE );
    return TCL_OK;
}

/**
 * Return number of tables in a VOTable.
 */
static int GaiaVOTableTclNumTables( ClientData clientData, Tcl_Interp *interp,
                                    int objc, Tcl_Obj *CONST objv[] )
{
    VOTable *table;

    /*  Check arguments, only allow one, the address of the VOTable. */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "votable_reference" );
        return TCL_ERROR;
    }

    /*  Import the table. */
    if ( importVOTableHandle( interp, objv[1], table ) == TCL_OK ) {
        Tcl_SetObjResult( interp, Tcl_NewIntObj( table->nTable() ) );
        return TCL_OK;
    }
    return TCL_ERROR;
}

/**
 * Save a TABLE to a Skycat tst. The table is selected as an index into all
 * the TABLEs in the VOTable.
 */
static int GaiaVOTableTclSave( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] )
{
    VOTable *table;
    const char *file;
    int ntable = 0;

    /*  Check arguments, need three, the address of the VOTable, the
     *  index of the table to extract and the file to save into. */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "votable_reference index file" );
        return TCL_ERROR;
    }

    /*  Import the table. */
    if ( importVOTableHandle( interp, objv[1], table ) == TCL_OK ) {
        if ( Tcl_GetIntFromObj( interp, objv[2], &ntable ) == TCL_OK ) {
            file = Tcl_GetString( objv[3] );
            if ( table->saveAsTST( ntable, file ) ) {
                return TCL_OK;
            }
            Tcl_SetResult( interp, const_cast<char *>( "Failed to save TABLE" ),
                           TCL_VOLATILE );
        }
    }
    return TCL_ERROR;
}

/**
 *  Get an INFO value from a VOTable. The INFO is identified by it's name.
 *  (e.q. "QUERY_STATUS" for <INFO name="QUERY_STATUS">). Returns the
 *  value attribute and any text content.
 */
static int GaiaVOTableTclInfo( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] )
{
    VOTable *table;
    string value;
    string content;

    /*  Check arguments, only allow two, the address of the VOTable and the
     *  name of the INFO element. */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "votable_reference INFO_name" );
        return TCL_ERROR;
    }

    /*  Import the table. */
    if ( importVOTableHandle( interp, objv[1], table ) == TCL_OK ) {

        /*  Get the values. */
        if ( table->infoValue( Tcl_GetString( objv[2] ), value, content ) ) {
            Tcl_AppendElement( interp,
                               const_cast<char *>( value.c_str() ) );
            Tcl_AppendElement( interp,
                               const_cast<char *>( content.c_str() ) );
            return TCL_OK;
        }
        else {
            Tcl_SetResult( interp,
                           const_cast<char *>( "Failed to locate INFO value" ),
                           TCL_VOLATILE );
        }
    }
    return TCL_ERROR;
}
