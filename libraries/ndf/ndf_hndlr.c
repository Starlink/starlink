#include "sae_par.h"		 /* Standard SAE constants */
#include "ndf_err.h"		 /* NDF error constants */
#include "ems.h"		 /* EMS_ error reporting routines */
#include "ems_par.h"		 /* EMS_ constants */
#include "f77.h"		 /* Fortran 77 <=> C interface macros */
#include "ast.h"		 /* AST definitions */
#include "mers.h"

/* A type for a pointer to a function that can be called to handle an
   NDF event. */
typedef void (* NdfEventHandler)( CHARACTER(EVNAME), CHARACTER(TEXT), INTEGER( STATUS )
                                  TRAIL(EVNAME) TRAIL(TEXT) );

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







F77_SUBROUTINE(ndf_hndlr)( CHARACTER(EVNAME),
                           NdfEventHandler HANDLR,
                           LOGICAL(SET),
                           INTEGER(STATUS)
                           TRAIL(EVNAME) ) {
/*
*+
*  Name:
*     NDF_HNDLR

*  Purpose:
*     Register a handler for a class of NDF event.

*  Language:
*     ANSI C

*  Invocation:
*     CALL NDF_HNDLR( EVNAME, HANDLR, SET, STATUS )

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
*     -  This routine is intended to be callable from Fortran.

*  Copyright:
*     Copyright (C) 2007-2010 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (STARLINK)
*     {@enter_new_authors_here@}

*  History:
*     31-OCT-2007 (DSB):
*        Original version.
*     1-SEP-2008 (DSB):
*        Added READ_DATA, WRITE_DATA and UPDATE_DATA event types.
*     16-OCT-2009 (DSB):
*        Added DEF_HISTORY event type.
*     {@enter_changes_here@}

*  Bugs:
*     {@note_any_bugs_here@}

*-
*/
   GENPTR_CHARACTER(EVNAME)
   GENPTR_INTEGER(STATUS)
   GENPTR_LOGICAL(STATUS)
   NdfEventHandler *handler_list;
   char *evname;
   int i;
   int ihandler;
   int there;
   int unused;
   int *old_status;

/* Check the inherited global status. */
   if ( *STATUS != SAI__OK ) return;

/* Make AST use the supplied status variable. */
   old_status = astWatch( STATUS );

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

/* Find an unused element in the array of handlers and check to see if
   the handler is already registered. */
         there = 0;
         unused = -1;
         for( i = 0; i < nhandlers[ ihandler ]; i++ ) {
            if( ! handler_list[ i ] ){
               if( unused == -1 ) unused = i;
            } else if(  handler_list[ i ] == HANDLR ) {
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
            if( astOK ) handler_list[ unused ] =  HANDLR;
         }

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

/* Make AST use the original status variable. */
   astWatch( old_status );

/* Report a context error message. */
   if( *STATUS != SAI__OK ) {
      if( F77_ISTRUE( *SET ) ) {
         errRep( "", "NDF_HDNLR: Failed to register a new NDF event handler",
                 STATUS );
      } else {
         errRep( "", "NDF_HDNLR: Failed to clear an NDF event handler",
                 STATUS );
      }
   }
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
*     7-NOV-2007 (DSB):
*        Avoid modifying TEXT_length by copying the required sub-string
*        to a secondary, dynamically allocated, text string (SHORTTEXT).
*     8-MAR-2011 (DSB):
*        Ensure the NDF_EVENT message token is always cleared before
*        leaving this function.
*     {@enter_changes_here@}

*  Bugs:
*     {@note_any_bugs_here@}

*-
*/

   GENPTR_CHARACTER(EVNAME)
   GENPTR_INTEGER(STATUS)
   DECLARE_CHARACTER(TEXT,EMS__SZMSG+1);
   DECLARE_CHARACTER_DYN(SHORTTEXT);
   char *evname;
   int ihandler;
   int i;
   int tlen;
   NdfEventHandler *handler_list;
   int *old_status;

/* Check the inherited global status. */
   if ( *STATUS != SAI__OK ) return;

/* Make AST use the supplied status variable. */
   old_status = astWatch( STATUS );

/* Get a pointer to dynamic memory holding a null-terminated copy of the supplied
   EVNAME string. */
   evname = astString( EVNAME, EVNAME_length );

/* Verify the supplied event type and get its index within the types and
   handlers arrays. */
   ihandler = CheckType( evname, STATUS );

/* Expand the NDF_EVENT message token. */
   emsMload( "", "^NDF_EVENT", TEXT, &tlen, STATUS );
   F77_CREATE_CHARACTER(SHORTTEXT,tlen);
   memcpy( SHORTTEXT, TEXT, tlen );

/* If OK, invoke any defined handlers. */
   if( *STATUS == SAI__OK  && handlers[ ihandler ] ) {
      handler_list = handlers[ ihandler ];
      for( i = 0; i < nhandlers[ ihandler ]; i++ ) {
         if( handler_list[ i ] ) {

/* Invoke the handler. */
            ( *handler_list[ i ] )( CHARACTER_ARG(EVNAME),
                                    CHARACTER_ARG(SHORTTEXT),
                                    INTEGER_ARG(STATUS)
                                    TRAIL_ARG(EVNAME)
                                    TRAIL_ARG(SHORTTEXT) );
         }
      }
   }

/* Free resources */
   F77_FREE_CHARACTER(SHORTTEXT);
   evname = astFree( evname );

/* Make AST use the original status variable. */
   astWatch( old_status );

/* Report a context error message. */
   if( *STATUS != SAI__OK ) {
      errRep( "", "NDF1_EVENT: Failed to raise an NDF event (internal NDF "
              "library error).", STATUS );
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
