#include "sae_par.h"		 /* Standard SAE constants */
#include "ndf_err.h"		 /* NDF error constants */
#include "ems.h"		 /* EMS_ error reporting routines */
#include "ems_par.h"		 /* EMS_ constants */
#include "f77.h"		 /* Fortran 77 <=> C interface macros */
#include "ast.h"		 /* AST definitions */

/* A type for a pointer to a function that can be called to handle an
   NDF event. */
typedef void (* NdfEventHandler)( CHARACTER(EVNAME), CHARACTER(TEXT), INTEGER( STATUS )
                                  TRAIL(EVNAME) TRAIL(TEXT) );

/* The number of different event types */
#define NTYPE 5

/* The names of the different event types. */
static const char *types[ NTYPE ] = { "OPEN_NEW_NDF", 
                                      "READ_EXISTING_NDF",
                                      "WRITE_EXISTING_NDF", 
                                      "UPDATE_EXISTING_NDF",
                                      "CLOSE_NDF" };

/* An array of pointers to the currently registered event handlers. */
static NdfEventHandler *handlers[ NTYPE ] = { NULL, NULL, NULL, NULL, NULL };

/* The number of handlers registered for each event type. */
static int nhandlers[ NTYPE ] = { 0, 0, 0, 0, 0 };

/* Local Prototypes */
static int CheckType( const char *, int * );







F77_SUBROUTINE(ndf_hndlr)( CHARACTER(EVNAME),
                           NdfEventHandler HANDLR,
                           LOGICAL(SET),
                           INTEGER(STATUS)
                           TRAIL(EVNAME) ) {
/*
*+
*  Name:
*     NDF1_HNDLR

*  Purpose:
*     Register a handler for a class of NDF event.

*  Language:
*     ANSI C

*  Invocation:
*     CALL NDF1_HNDLR( EVNAME, HANDLR, SET, STATUS )

*  Description:
*     The routine can be called by an application to register 
*     interest in a particular class of NDF event. When an event 
*     of the specified type occurs within the NDF library, the 
*     supplied subroutine is called.
*
*     This is currently an undocumented experimental facility.

*  Arguments:
*     EVNAME = CHARACTER * ( * ) (Given)
*        A case-insensitive string indicating the sort of event about 
*        which the application wishes to be notified. See "Event Types:" 
*        below.
*     HANDLR = SUBROUTINE (Given)
*        A routine that should be called when an event of the specified
*        type occurs within the the NDF library. It should be declared
*        EXTERNAL in the calling routine. It should take three arguments,
*        the first two are character variables which will be filled before 
*        the handler is invoked with the name of the event, and any
*        additional descriptive information associated with the event. The 
*        third argument is the usual inherited STATUS argument. 
*     SET = LOGICAL (Given)
*        If .TRUE., then the specified handler is registered to be called
*        when the specified event type occurs. If .FALSE., then the
*        specified handler is unregistered so that it will no longer be
*        called when the specified event occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Event Types:
*     The following event types are currently defined:
*
*     - READ_EXISTING_NDF: Occurs after an existing NDF has been opened for 
*     READ access. The "descriptive information" is the path to the
*     NDF that was opened.
*
*     - WRITE_EXISTING_NDF: Occurs after an existing NDF has been opened for 
*     WRITE access. The "descriptive information" is the path to the
*     NDF that was opened.
*
*     - UPDATE_EXISTING_NDF: Occurs after an existing NDF has been opened for
*     UPDATE access. The "descriptive information" is the path 
*     to the NDF that was opened.
*
*     - OPEN_NEW_NDF: Occurs after a new NDF has been opened. The "descriptive 
*     information" is message token will contain the path to the NDF that was opened.
*
*     - CLOSE_NDF: Occurs before an NDF is closed. The "descriptive information" is 
*     the path to the NDF that is about to be closed.

*  Notes:
*     - Any number of handlers can be registered with a given event type.
*     They will each be invoked when an event occurs (the order in
*     which they were invoked is unspecified).
*     -  This routine is intended to be callable from Fortran.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {@enter_new_authors_here@}

*  History:
*     31-OCT-2007 (DSB):
*        Original version.
*     {@enter_changes_here@}

*  Bugs:
*     {@note_any_bugs_here@}

*-
*/
   GENPTR_CHARACTER(EVNAME)
   GENPTR_INTEGER(STATUS)
   GENPTR_LOGICAL(STATUS)
   char *evname;
   int ihandler;
   NdfEventHandler *handler_list;
   int i;

/* Check the inherited global status. */
   if ( *STATUS != SAI__OK ) return;

/* Get a pointer to dynamic memory holding a null-terminated copy of the supplied 
   EVNAME string. */
   evname = astString( EVNAME, EVNAME_length );

/* Verify the supplied event type and get its index within the types and
   handlers arrays. */
   ihandler = CheckType( evname, STATUS );

/* If OK, add or remove the pointer from the appropriate array. */
   if( *STATUS == SAI__OK ) {
      handler_list = handlers[ ihandler ];

/* If adding the handler to the list of registered handlers... */
      if( F77_ISTRUE( *SET ) ) {

/* Find an unused element in the array of handlers. */
         for( i = 0; i < nhandlers[ ihandler ]; i++ ) {
            if( ! handler_list[ i ] ) break;
         }

/* If no unused element was found, extend the array. */
         if( i == nhandlers[ ihandler ] ) {
            astBeginPM;
            handlers[ ihandler ] = astGrow( handler_list, ++nhandlers[ ihandler ],
                                            sizeof( NdfEventHandler ) );
            handler_list = handlers[ ihandler ];
            astEndPM;
         }

/* If OK, store the pointer. */
         if( astOK ) handler_list[ i ] =  HANDLR;

/* If the handler is being unregistered, search for it in the list of
   currently registered handlers, and nullify the array element. */
      } else {
         for( i = 0; i < nhandlers[ ihandler ]; i++ ) {
            if( handler_list[ i ] == HANDLR ) {
               handler_list[ i ] = NULL;
            }
         }
      } 
   }

/* Free resources */
   evname = astFree( evname );
}


F77_SUBROUTINE(ndf1_event)( CHARACTER(EVNAME),
                            INTEGER(STATUS)
                            TRAIL(EVNAME) ) {
/*
*+
*  Name:
*     NDF1_EVENT

*  Purpose:
*     Called to indicate that an NDF event has occurred.

*  Language:
*     ANSI C

*  Invocation:
*     CALL NDF1_EVENT( EVNAME, STATUS )

*  Description:
*     The routine should be called to indicate that an event that may be
*     of significance to the calling application has occurred. Each such
*     event has a class name (EVNAME) and associated descriptive
*     information, which is assumed to have been assigned to the MSG token
*     "NDF_EVENT" prior to calling this routine.

*  Arguments:
*     EVNAME = CHARACTER * ( * ) (Given)
*        A string indicating the sort of event that has occurred.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine is intended to be callable from Fortran.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {@enter_new_authors_here@}

*  History:
*     31-OCT-2007 (DSB):
*        Original version.
*     {@enter_changes_here@}

*  Bugs:
*     {@note_any_bugs_here@}

*-
*/

   GENPTR_CHARACTER(EVNAME)
   GENPTR_INTEGER(STATUS)
   DECLARE_CHARACTER(TEXT,EMS__SZMSG+1);
   char *evname;
   int ihandler;
   int i;
   int expanded;
   NdfEventHandler *handler_list;

/* Check the inherited global status. */
   if ( *STATUS != SAI__OK ) return;

/* Get a pointer to dynamic memory holding a null-terminated copy of the supplied 
   EVNAME string. */
   evname = astString( EVNAME, EVNAME_length );

/* Verify the supplied event type nad get its index within the types and
   handlers arrays. */
   ihandler = CheckType( evname, STATUS );

/* If OK, invoke any defined handlers. */
   if( *STATUS == SAI__OK  && handlers[ ihandler ] ) {
      expanded = 0;
      handler_list = handlers[ ihandler ];
      for( i = 0; i < nhandlers[ ihandler ]; i++ ) {
         if( handler_list[ i ] ) {

/* If not already done, expand the NDF_EVENT message token. */
            if( !expanded ) {
               emsMload( "", "^NDF_EVENT", TEXT, &TEXT_length, STATUS );
               expanded = 1;
            }

/* Invoke the handler. */
            ( *handler_list[ i ] )( CHARACTER_ARG(EVNAME),
                                    CHARACTER_ARG(TEXT),
                                    INTEGER_ARG(STATUS) 
                                    TRAIL_ARG(EVNAME)
                                    TRAIL_ARG(TEXT) );
         }
      }
   }

/* Free resources */
   evname = astFree( evname );
}





/* Verify the supplied event type and return the corresponding index
   within the "handlers" and "types" arrays. */

static int CheckType( const char *evname, int *status ) {
   int ihandler;

   if( *status != SAI__OK ) return 0;

   for( ihandler = 0; ihandler < NTYPE; ihandler++ ) {
      if( astChrMatch( evname, types[ ihandler ] ) ) break;
   }         

   if( ihandler == NTYPE ) {
       *status = NDF__NAMIN;
       emsSetc( "EV", evname );
       emsRep(" ", "Unknown NDF event type '^EV' supplied.", status );
   }

   return ihandler;
}
