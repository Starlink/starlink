#include "sae_par.h"
#include "ndf_err.h"
#include "ems.h"
#include "ems_par.h"
#include "ndf.h"
#include "ndf_ast.h"
#include "mers.h"
#include "ndf1.h"
#include <pthread.h>

/* The number of different event types */
#define NTYPE 9

/* The names of the different event types. */
static const char *types[ NTYPE ] = { "OPEN_NEW_NDF",
                                      "READ_EXISTING_NDF",
                                      "WRITE_EXISTING_NDF",
                                      "UPDATE_EXISTING_NDF",
                                      "CLOSE_NDF",
                                      "READ_DATA",
                                      "WRITE_DATA",
                                      "UPDATE_DATA",
                                      "DEF_HISTORY" };

/* An array of pointers to the currently registered event handlers. */
static NdfEventHandler *handlers[ NTYPE ] = { NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };

/* The number of handlers registered for each event type. */
static int nhandlers[ NTYPE ] = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };

/* Local Prototypes */
static int CheckType( const char *, int * );

/* Mutex to serialise invocations access to static variables. */
static pthread_mutex_t Mutex = PTHREAD_MUTEX_INITIALIZER;






void ndfHndlr_( const char *evname, NdfEventHandler handlr, int set,
                int *status ){
/*
*+
*  Name:
*     ndfHndlr

*  Purpose:
*     Register a handler for a class of NDF event.

*  Synopsis:
*     void ndfHndlr( const char *evname, NdfEventHandler handlr, int set,
*                    int *status )

*  Description:
*     This runction can be called by an application to register
*     interest in a particular class of NDF event. When an event
*     of the specified type occurs within the NDF library, the
*     supplied handler function is called.
*
*     This is currently an undocumented experimental facility.

*  Arguments:
*     evname
*        A null-terminated case-insensitive string indicating the sort
*        of event about which the application wishes to be notified. See
*        "Event Types:" below.
*     handlr
*        Pointer to a function that should be called when an event of the
*        specified type occurs within the the NDF library. It should
*        match the following prototype:
*
*        void handler( const char *evname, const char *text, int *status );
*
*        The first two arguments ("evname" and "text") will be set before
*        the handler is invoked to the name of the event, and any additional
*        descriptive information associated with the event.
*     set
*        If non-zero, then the specified handler is registered to be called
*        when the specified event type occurs. If zero, then the specified
*        handler is unregistered so that it will no longer be called when
*        the specified event occurs.
*     *status
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
*     information" is the path to the NDF that was opened.
*
*     - CLOSE_NDF: Occurs before an NDF is closed. The "descriptive information" is
*     the path to the NDF that is about to be closed.
*
*     - READ_DATA: Occurs when the DATA component of an NDF is mapped for
*     read access. The "descriptive information" is the path to the NDF that
*     is mapped.
*
*     - WRITE_DATA: Occurs when the DATA component of an NDF is mapped for
*     write access. The "descriptive information" is the path to the NDF that
*     is mapped.
*
*     - UPDATE_DATA: Occurs when the DATA component of an NDF is mapped for
*     update access. The "descriptive information" is the path to the NDF that
*     is mapped.
*
*     - DEF_HISTORY: Occurs immediately after a default history record has
*     been writtent to an NDF. The "descriptive information" is the path to
*     the NDF.

*  Notes:
*     - In the above event descriptions, the "path to the NDF" will be
*     extended with the string "::<code>" if the NDF is a foreign format
*     file, where "<code>" is replaced by the name of the foreign data
*     format.
*     - Any number of handlers can be registered with a given event type.
*     They will each be invoked when an event occurs (the order in
*     which they were invoked is unspecified).
*     - Handlers should probably avoid making any calls back to the NDF
*     library to avoid possible deadlock if any internal mutexes are locked
*     when the event occurs.

*  Copyright:
*     Copyright (C) 2018 East Asian Obvservatory
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
*     DSB: David S. Berry (EAO)
*     {@enter_new_authors_here@}

*  History:
*     31-OCT-2007 (DSB):
*        Original version.
*     1-SEP-2008 (DSB):
*        Added READ_DATA, WRITE_DATA and UPDATE_DATA event types.
*     16-OCT-2009 (DSB):
*        Added DEF_HISTORY event type.
*     19-JUL-2018 (DSB):
*        Modified to be callable from C and renamed as ndfHndlr.
*     {@enter_changes_here@}

*  Bugs:
*     {@note_any_bugs_here@}

*-
*/
   NdfEventHandler *handler_list;
   int i;
   int ihandler;
   int there;
   int unused;
   int *old_status;

/* Check the inherited global status. */
   if ( *status != SAI__OK ) return;

/* All threads share the same event handlers. Lock the module mutex in
   order to ensure static variables are not used simultaneously by more
   than one thread.  */
   pthread_mutex_lock( &Mutex );

/* Make AST use the supplied status variable. */
   old_status = astWatch( status );

/* Verify the supplied event type and get its index within the types and
   handlers arrays. */
   ihandler = CheckType( evname, status );

/* If OK, add or remove the pointer from the appropriate array. */
   if( *status == SAI__OK ) {
      handler_list = handlers[ ihandler ];

/* If adding the handler to the list of registered handlers... */
      if( set ) {

/* Find an unused element in the array of handlers and check to see if
   the handler is already registered. */
         there = 0;
         unused = -1;
         for( i = 0; i < nhandlers[ ihandler ]; i++ ) {
            if( ! handler_list[ i ] ){
               if( unused == -1 ) unused = i;
            } else if(  handler_list[ i ] == handlr ) {
               there = 1;
            }
         }

/* Do nothing more if the handler is already registered. */
         if( ! there ) {

/* If no unused element was found, extend the array. */
            if( unused == -1 ) {
               astBeginPM;
               unused = nhandlers[ ihandler ];
               handlers[ ihandler ] = astGrow( handler_list,
                                               ++nhandlers[ ihandler ],
                                               sizeof( NdfEventHandler ) );
               handler_list = handlers[ ihandler ];
               astEndPM;
            }

/* If OK, store the pointer. */
            if( astOK ) handler_list[ unused ] =  handlr;
         }

/* If the handler is being unregistered, search for it in the list of
   currently registered handlers, and nullify the array element. */
      } else {
         for( i = 0; i < nhandlers[ ihandler ]; i++ ) {
            if( handler_list[ i ] == handlr ) {
               handler_list[ i ] = NULL;
            }
         }
      }
   }

/* Make AST use the original status variable. */
   astWatch( old_status );

/* Unlock the module mutex. */
   pthread_mutex_unlock( &Mutex );

/* Report a context error message. */
   if( *status != SAI__OK ) {
      if( set ) {
         errRep( "", "ndfHndlr: Failed to register a new NDF event handler",
                 status );
      } else {
         errRep( "", "ndfHndlr: Failed to clear an NDF event handler",
                 status );
      }
   }
}


void ndf1Event( const char *evname, int *status ){
/*
*+
*  Name:
*     ndf1Event

*  Purpose:
*     Called to indicate that an NDF event has occurred.

*  Synopsis:
*     void ndf1Event( const char *evname, int *status )

*  Description:
*     This function should be called to indicate that an event that may be
*     of significance to the calling application has occurred. Each such
*     event has a class name (EVNAME) and associated descriptive
*     information, which is assumed to have been assigned to the MSG token
*     "NDF_EVENT" prior to calling this routine.

*  Arguments:
*     evname
*        A null=-terminated string indicating the sort of event that
*        has occurred.
*     *status
*        The global status.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
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
*     7-NOV-2007 (DSB):
*        Avoid modifying TEXT_length by copying the required sub-string
*        to a secondary, dynamically allocated, text string (SHORTTEXT).
*     8-MAR-2011 (DSB):
*        Ensure the NDF_EVENT message token is always cleared before
*        leaving this function.
*     19-JUL-2018 (DSB):
*        Modified to be callable from C and renamed to ndf1Event.
*     {@enter_changes_here@}

*  Bugs:
*     {@note_any_bugs_here@}

*-
*/


   char text[EMS__SZMSG+1];
   int ihandler;
   int i;
   int tlen;
   NdfEventHandler *handler_list;
   int *old_status;

/* Check the inherited global status. */
   if ( *status != SAI__OK ) return;

/* All threads share the same event handlers. Lock the module mutex in
   order to ensure static variables are not used simultaneously by more
   than one thread.  */
   pthread_mutex_lock( &Mutex );

/* Make AST use the supplied status variable. */
   old_status = astWatch( status );

/* Verify the supplied event type and get its index within the types and
   handlers arrays. */
   ihandler = CheckType( evname, status );

/* Expand the NDF_EVENT message token. */
   emsMload( "", "^NDF_EVENT", text, &tlen, status );

/* If OK, invoke any defined handlers. */
   if( *status == SAI__OK  && handlers[ ihandler ] ) {
      handler_list = handlers[ ihandler ];
      for( i = 0; i < nhandlers[ ihandler ]; i++ ) {
         if( handler_list[ i ] ) {

/* Invoke the handler via an F77 wrapper function. */
            ndf1InvokeF77Handler( handler_list[ i ], evname,
                                  text, status );
         }
      }
   }

/* Make AST use the original status variable. */
   astWatch( old_status );

/* Unlock the module mutex. */
   pthread_mutex_unlock( &Mutex );

/* Report a context error message. */
   if( *status != SAI__OK ) {
      errRep( "", "ndf1Event: Failed to raise an NDF event (internal NDF "
              "library error).", status );
   }
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
