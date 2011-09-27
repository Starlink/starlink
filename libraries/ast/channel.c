/*
*class++
*  Name:
*     Channel

*  Purpose:
*     Basic (textual) I/O channel.

*  Constructor Function:
c     astChannel
f     AST_CHANNEL

*  Description:
*     The Channel class implements low-level input/output for the AST
*     library.  Writing an Object to a Channel will generate a textual
*     representation of that Object, and reading from a Channel will
*     create a new Object from its textual representation.
*
*     Normally, when you use a Channel, you should provide "source"
c     and "sink" functions which connect it to an external data store
f     and "sink" routines which connect it to an external data store
*     by reading and writing the resulting text. By default, however,
*     a Channel will read from standard input and write to standard
*     output. Alternatively, a Channel can be told to read or write from
*     specific text files using the SinkFile and SourceFile attributes,
*     in which case no sink or source function need be supplied.

*  Inheritance:
*     The Channel class inherits from the Object class.

*  Attributes:
*     In addition to those attributes common to all Objects, every
*     Channel also has the following attributes:
*
*     - Comment: Include textual comments in output?
*     - Full: Set level of output detail
*     - Indent: Indentation increment between objects
*     - ReportLevel: Selects the level of error reporting
*     - SinkFile: The path to a file to which the Channel should write
*     - Skip: Skip irrelevant data?
*     - SourceFile: The path to a file from which the Channel should read
*     - Strict: Generate errors instead of warnings?

*  Functions:
c     In addition to those functions applicable to all Objects, the
c     following functions may also be applied to all Channels:
f     In addition to those routines applicable to all Objects, the
f     following routines may also be applied to all Channels:
*
c     - astWarnings: Return warnings from the previous read or write
c     - astPutChannelData: Store data to pass to source or sink functions
c     - astRead: Read an Object from a Channel
c     - astWrite: Write an Object to a Channel
f     - AST_WARNINGS: Return warnings from the previous read or write
f     - AST_READ: Read an Object from a Channel
f     - AST_WRITE: Write an Object to a Channel

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.
*     Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     12-AUG-1996 (RFWS):
*        Original version.
*     6-SEP-1996:
*        Finished initial implementation.
*     11-DEC-1996 (RFWS):
*        Added support for foreign language source and sink functions.
*     28-APR-1997 (RFWS):
*        Prevent "-0" being written (use "0" instead).
*     27-NOV-2002 (DSB):
*        Added astWriteInvocations.
*     8-JAN-2003 (DSB):
*        - Changed private InitVtab method to protected astInitChannelVtab
*        method.
*        - Modified to use protected Vtab initialisation methods when
*        loading an Object.
*     1-NOV-2003 (DSB):
*        Change the initialiser so that it accepts source and sink
*        wrapper functions as arguments (for use by derived classes).
*     16-AUG-2006 (DSB):
*        - Document non-destructive nature of unsuccessful astRead calls
*        on a FitsChan.
*     3-OCT-2008 (DSB):
*        Added "Strict" attribute.
*     11-DEC-2008 (DSB):
*        Added astPutChannelData and astChannelData functions.
*     16-JAN-2009 (DSB):
*        Added astAddWarning and astWarnings.
*     11-JUN-2009 (DSB):
*        Enable astChannelData to be used from within astRead.
*     7-DEC-2009 (DSB):
*        Added Indent attribute.
*     12-FEB-2010 (DSB):
*        Represent AST__BAD externally using the string "<bad>".
*     23-JUN-2011 (DSB):
*        Added attributes SinkFile and SourceFile.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS Channel

/* Define a string containing the maximum length of keywords used to
   identify values in the external representation of data. This is
   deliberately kept small so as to simplify integration with
   standards such as FITS. */
#define MAX_NAME "8"

/* Max length of string returned by GetAttrib */
#define GETATTRIB_BUFF_LEN 50

/* String used to represent AST__BAD externally. */
#define BAD_STRING "<bad>"

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "channel.h"             /* Interface definition for this class */
#include "loader.h"              /* Interface to the global loader */
#include "keymap.h"              /* Storing arbitrary data in an AST Object */
#include "pointset.h"            /* For AST__BAD */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <limits.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static const char *(* parent_getattrib)( AstObject *, const char *, int * );
static int (* parent_testattrib)( AstObject *, const char *, int * );
static void (* parent_clearattrib)( AstObject *, const char *, int * );
static void (* parent_setattrib)( AstObject *, const char *, int * );

/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0; \
   globals->AstReadClassData_Msg = 0; \
   globals->GetAttrib_Buff[ 0 ] = 0; \
   globals->Items_Written = 0; \
   globals->Current_Indent = 0; \
   globals->Nest = -1; \
   globals->Nwrite_Invoc = 0; \
   globals->Object_Class = NULL; \
   globals->Values_List = NULL; \
   globals->Values_Class = NULL; \
   globals->Values_OK = NULL; \
   globals->End_Of_Object = NULL; \
   globals->Channel_Data  = NULL;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(Channel)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(Channel,Class_Init)
#define class_vtab astGLOBAL(Channel,Class_Vtab)
#define astreadclassdata_msg astGLOBAL(Channel,AstReadClassData_Msg)
#define getattrib_buff astGLOBAL(Channel,GetAttrib_Buff)
#define items_written  astGLOBAL(Channel,Items_Written)
#define current_indent astGLOBAL(Channel,Current_Indent)
#define nest           astGLOBAL(Channel,Nest)
#define nwrite_invoc   astGLOBAL(Channel,Nwrite_Invoc)
#define object_class   astGLOBAL(Channel,Object_Class)
#define values_list    astGLOBAL(Channel,Values_List)
#define values_class   astGLOBAL(Channel,Values_Class)
#define values_ok      astGLOBAL(Channel,Values_OK)
#define end_of_object  astGLOBAL(Channel,End_Of_Object)
#define channel_data   astGLOBAL(Channel,Channel_Data)



static pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX2 pthread_mutex_lock( &mutex2 );
#define UNLOCK_MUTEX2 pthread_mutex_unlock( &mutex2 );

static pthread_mutex_t mutex3 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX3 pthread_mutex_lock( &mutex3 );
#define UNLOCK_MUTEX3 pthread_mutex_unlock( &mutex3 );

/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

/* Contextual error message reported in astReadClassData? */
static int astreadclassdata_msg = 0;

/* Buffer returned by GetAttrib. */
static char getattrib_buff[ GETATTRIB_BUFF_LEN + 1 ];

/* Count of the number of output items written since the last "Begin"
   or "IsA" item. */
static int items_written = 0;

/* Amount of indentation to be applied to the next output item. */
static int current_indent = 0;

/* Nesting level, used to keep track of data associated with building
   Objects when they contain other Objects. */
static int nest = -1;

/* The number of times astWrite has been invoked. */
static int nwrite_invoc = 0;

/* Pointer to a user-supplied block of memory to be made available to
   source or sink functions via the astChannelData function. */
static void *channel_data = NULL;

/***
   The following items are all pointers to dynamically allocated
   arrays (stacks) that grow as necessary to accommodate one element
   for each level of nesting (one more than the value of "nest").
***/

/* Stack of pointers to null-terminated character strings giving the
   names of the classes of the Objects being built at each nesting
   level. */
static char **object_class = NULL;

/* Stack of pointers to the elements designated as the "heads" of
   circular, doubly linked lists of name-value associations. */
static AstChannelValue **values_list = NULL;

/* Stack of pointers to null-terminated character strings giving the
   names of the classes for which the values held in the values lists
   are intended. */
static char **values_class = NULL;

/* Stack of flags indicating whether the values held in the values
   lists are intended for the class loaders currently executing to
   build Objects at each nesting level. */
static int *values_ok = NULL;

/* Stack of flags indicating whether "End" items have been read for
   the Objects being built at each nesting level. */
static int *end_of_object = NULL;


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstChannelVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */
#define LOCK_MUTEX2
#define UNLOCK_MUTEX2
#define LOCK_MUTEX3
#define UNLOCK_MUTEX3

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstChannel *astChannelForId_( const char *(*)( void ),
                              char *(*)( const char *(*)( void ), int * ),
                              void (*)( const char * ),
                              void (*)( void (*)( const char * ),
                                        const char *, int * ),
                              const char *, ... );
AstChannel *astChannelId_( const char *(*)( void ), void (*)( const char * ), const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstObject *Read( AstChannel *, int * );
static AstObject *ReadObject( AstChannel *, const char *, AstObject *, int * );
static AstChannelValue *FreeValue( AstChannelValue *, int * );
static AstChannelValue *LookupValue( const char *, int * );
static AstKeyMap *Warnings( AstChannel *, int * );
static char *GetNextText( AstChannel *, int * );
static char *InputTextItem( AstChannel *, int * );
static char *ReadString( AstChannel *, const char *, const char *, int * );
static char *SourceWrap( const char *(*)( void ), int * );
static const char *GetAttrib( AstObject *, const char *, int * );
static double ReadDouble( AstChannel *, const char *, double, int * );
static int GetComment( AstChannel *, int * );
static int GetFull( AstChannel *, int * );
static int GetSkip( AstChannel *, int * );
static int GetStrict( AstChannel *, int * );
static int ReadInt( AstChannel *, const char *, int, int * );
static int TestAttrib( AstObject *, const char *, int * );
static int TestComment( AstChannel *, int * );
static int TestFull( AstChannel *, int * );
static int TestSkip( AstChannel *, int * );
static int TestStrict( AstChannel *, int * );
static int Use( AstChannel *, int, int, int * );
static int Write( AstChannel *, AstObject *, int * );
static void AddWarning( AstChannel *, int, const char *, const char *, int * );
static void AppendValue( AstChannelValue *, AstChannelValue **, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void ClearComment( AstChannel *, int * );
static void ClearFull( AstChannel *, int * );
static void ClearSkip( AstChannel *, int * );
static void ClearStrict( AstChannel *, int * );
static void ClearValues( AstChannel *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void GetNextData( AstChannel *, int, char **, char **, int * );
static void OutputTextItem( AstChannel *, const char *, int * );
static void PutChannelData( AstChannel *, void *, int * );
static void PutNextText( AstChannel *, const char *, int * );
static void ReadClassData( AstChannel *, const char *, int * );
static void RemoveValue( AstChannelValue *, AstChannelValue **, int * );
static void SetAttrib( AstObject *, const char *, int * );
static void SetComment( AstChannel *, int, int * );
static void SetFull( AstChannel *, int, int * );
static void SetSkip( AstChannel *, int, int * );
static void SetStrict( AstChannel *, int, int * );
static void SinkWrap( void (*)( const char * ), const char *, int * );
static void Unquote( AstChannel *, char *, int * );
static void WriteBegin( AstChannel *, const char *, const char *, int * );
static void WriteDouble( AstChannel *, const char *, int, int, double, const char *, int * );
static void WriteEnd( AstChannel *, const char *, int * );
static void WriteInt( AstChannel *, const char *, int, int, int, const char *, int * );
static void WriteIsA( AstChannel *, const char *, const char *, int * );
static void WriteObject( AstChannel *, const char *, int, int, AstObject *, const char *, int * );
static void WriteString( AstChannel *, const char *, int, int, const char *, const char *, int * );

static int GetReportLevel( AstChannel *, int * );
static int TestReportLevel( AstChannel *, int * );
static void ClearReportLevel( AstChannel *, int * );
static void SetReportLevel( AstChannel *, int, int * );

static int GetIndent( AstChannel *, int * );
static int TestIndent( AstChannel *, int * );
static void ClearIndent( AstChannel *, int * );
static void SetIndent( AstChannel *, int, int * );

static const char *GetSourceFile( AstChannel *, int * );
static int TestSourceFile( AstChannel *, int * );
static void ClearSourceFile( AstChannel *, int * );
static void SetSourceFile( AstChannel *, const char *, int * );

static const char *GetSinkFile( AstChannel *, int * );
static int TestSinkFile( AstChannel *, int * );
static void ClearSinkFile( AstChannel *, int * );
static void SetSinkFile( AstChannel *, const char *, int * );

/* Member functions. */
/* ================= */
static void AddWarning( AstChannel *this, int level, const char *msg,
                        const char *method, int *status ) {
/*
*+
*  Name:
*     astAddWarning

*  Purpose:
*     Add a warning to a Channel.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "channel.h"
*     void astAddWarning( AstChannel *this, int level, const char *msg,
*                         const char *method, int status, ... )

*  Class Membership:
*     Channel method.

*  Description:
*     This function stores a warning message inside a Channel. These
*     messages can be retirieved using astWarnings.

*  Parameters:
*     this
*        Pointer to the Channel.
*     level
*        Ignore the warning if the ReportLevel attribute value is less
*        than "level".
*     msg
*        The wanting message to store. It may contain printf format
*        specifiers. If a NULL pointer is supplied, all warnings
*        currently stored in the Channel are removed.
*     method
*        The method name.
*     status
*        Inherited status value.
*     ...
*        Extra values to substitute into the message string as
*        replacements for the printf format specifiers.
*-

*  Note: The expansion of the printf format specifiers is done in the
*     astAddWarning_ wrapper function. The AddWarning functions defined by
*     each class receives the fully expanded message and does not have a
*     variable argument list. The variable argument list is included in the
*     above prologue in order to document the wrapper function.

*/

/* Local Variables: */
   int i;          /* Message index */
   char *a;        /* Pointer to copy of message */

/* If a NULL pointer was supplied, free all warnings currently in the
   Channel. Do this before checking the inherited status so that it works
   even if an error has occurred. */
   if( !msg ) {
      for( i = 0; i < this->nwarn; i++ ) {
         (this->warnings)[ i ] = astFree( (this->warnings)[ i ] );
      }
      this->warnings = astFree( this->warnings );
      this->nwarn = 0;
      return;
   }

/* Check the global error status. */
   if ( !astOK ) return;

/* Only proceed if the message level is sufficiently important. */
   if( astGetReportLevel( this ) >= level ) {

/* If we are being strict, issue an error rather than a warning. */
      if( astGetStrict( this ) ) {
         if( astOK ) {
            astError( AST__BADIN, "%s(%s): %s", status, method,
                      astGetClass( this ), msg );
         }

/* Otherwise, we store a copy of the message in the Channel. */
      } else {

/* Allocate memory and store a copy of th supplied string in it. */
         a = astStore( NULL, msg, strlen( msg ) + 1 );

/* Expand the array of warning pointers in ther Channel structure. */
         this->warnings = astGrow( this->warnings, this->nwarn + 1,
                                   sizeof( char * ) );

/* If all is OK so far, store the new warning pointer, and increment the
   number of warnings in the Channel. */
         if( astOK ) {
            (this->warnings)[ (this->nwarn)++ ] = a;

/* Otherwise, attempt to free the memory holding the copy of the warning. */
         } else {
            a = astFree( a );
         }
      }
   }
}

static void AppendValue( AstChannelValue *value, AstChannelValue **head, int *status ) {
/*
*  Name:
*     AppendValue

*  Purpose:
*     Append a Value structure to a list.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     void AppendValue( AstChannelValue *value, AstChannelValue **head, int *status )

*  Class Membership:
*     Channel member function.

*  Description:
*     This function appends a Value structure to a doubly linked
*     circular list of such structures. The new list element is
*     inserted just in front of the element occupying the "head of
*     list" position (i.e. it becomes the new last element in the
*     list).

*  Parameters:
*     value
*        Pointer to the new element. This must not already be in the
*        list.
*     head
*        Address of a pointer to the element at the head of the list
*        (this pointer should be NULL if the list is initially
*        empty). This pointer will only be updated if a new element is
*        being added to an empty list.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - This function does not perform error chacking and does not
*     generate errors.
*/

/* If the list is initially empty, the sole new element points at
   itself. */
   if ( !*head ) {
      value->flink = value;
      value->blink = value;

/* Update the list head to identify the new element. */
      *head = value;

/* Otherwise, insert the new element in front of the element at the
   head of the list. */
   } else {
      value->flink = *head;
      value->blink = ( *head )->blink;
      ( *head )->blink = value;
      value->blink->flink = value;
   }
}

void *astChannelData_( void ) {
/*
c++
*  Name:
*     astChannelData

*  Purpose:
*     Return a pointer to user-supplied data stored with a Channel.

*  Type:
*     Public macro.

*  Synopsis:
*     #include "channel.h"
*     void *astChannelData

*  Class Membership:
*     Channel macro.

*  Description:
*     This macro is intended to be used within the source or sink
*     functions associated with a Channel. It returns any pointer
*     previously stored in the Channel (that is, the Channel that has
*     invoked the source or sink function) using astPutChannelData.
*
*     This mechanism is a thread-safe alternative to passing file
*     descriptors, etc, via static global variables.

*  Returned Value:
*     astChannelData
*        The pointer previously stored with the Channel using
*        astPutChannelData. A NULL pointer will be returned if no such
*        pointer has been stored with the Channel.

*  Applicability:
*     Channel
*        This macro applies to all Channels.

*  Notes:
*     - This routine is not available in the Fortran 77 interface to
*     the AST library.
c--
*/
   astDECLARE_GLOBALS;
   astGET_GLOBALS(NULL);
   return channel_data;
}

static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a Channel.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     void ClearAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Channel member function (over-rides the astClearAttrib protected
*     method inherited from the Object class).

*  Description:
*     This function clears the value of a specified attribute for a
*     Channel, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the Channel.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstChannel *this;              /* Pointer to the Channel structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Channel structure. */
   this = (AstChannel *) this_object;

/* Check the attribute name and clear the appropriate attribute. */

/* Comment. */
/* -------- */
   if ( !strcmp( attrib, "comment" ) ) {
      astClearComment( this );

/* Full. */
/* ----- */
   } else if ( !strcmp( attrib, "full" ) ) {
      astClearFull( this );

/* Indent. */
/* ------- */
   } else if ( !strcmp( attrib, "indent" ) ) {
      astClearIndent( this );

/* ReportLevel. */
/* ------------ */
   } else if ( !strcmp( attrib, "reportlevel" ) ) {
      astClearReportLevel( this );

/* Skip. */
/* ----- */
   } else if ( !strcmp( attrib, "skip" ) ) {
      astClearSkip( this );

/* SourceFile. */
/* ----------- */
   } else if ( !strcmp( attrib, "sourcefile" ) ) {
      astClearSourceFile( this );

/* SinkFile. */
/* --------- */
   } else if ( !strcmp( attrib, "sinkfile" ) ) {
      astClearSinkFile( this );

/* Strict. */
/* ------- */
   } else if ( !strcmp( attrib, "strict" ) ) {
      astClearStrict( this );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib, status );
   }
}

static void ClearValues( AstChannel *this, int *status ) {
/*
*  Name:
*     ClearValues

*  Purpose:
*     Clear the current values list.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     void ClearValues( AstChannel *this, int *status )

*  Class Membership:
*     Channel member function.

*  Description:
*     This function clears any (un-read) Value structures remaining in
*     the current values list (i.e. at the current nesting level). It
*     should be invoked after all required values have been read.
*
*     If the values list has not been read, or if any remaining values
*     are found (i.e. the list is not empty) then this indicates an
*     unrecognised input class or an input value that has not been
*     read by a class loader. This implies an error in the loader, or
*     bad input data, so an error is reported.
*
*     All resources used by any remaining Value structures are freed
*     and the values list is left in an empty state.

*  Parameters:
*     this
*        Pointer to the Channel being read. This is only used for
*        constructing error messages. It must not be NULL.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - This function attempts to execute even if the global error
*     status is set on entry, although no further error report will be
*     made if it subsequently fails under these circumstances.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   AstChannelValue **head;       /* Address of pointer to values list */
   AstChannelValue *value;       /* Pointer to value list element */

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* If "values_class" is non-NULL, then the values list has previously
   been filled with Values for a class. */
   if ( values_class[ nest ] ) {

/* If "values_ok" is zero, however, then these Values have not yet
   been read by a class loader. This must be due to a bad class name
   associated with them or because the class data are not available in
   the correct order. If we are using strict error reporting, then report
   an error (unless the error status is already set). */
      if ( astGetStrict( this ) && !values_ok[ nest ] && astOK ) {
         astError( AST__BADIN,
                   "astRead(%s): Invalid class structure in input data.", status,
                   astGetClass( this ) );
         astError( AST__BADIN,
                   "Class \"%s\" is invalid or out of order within a %s.", status,
                   values_class[ nest ], object_class[ nest ] );
      }

/* Free the memory holding the class string. */
      values_class[ nest ] = astFree( values_class[ nest ] );
   }

/* Reset the "values_ok" flag. */
   values_ok[ nest ] = 0;

/* Now clear any Values remaining in the values list. Obtain the
   address of the pointer to the head of this list (at the current
   nesting level) and loop to remove Values from the list while it is
   not empty. */
   head = values_list + nest;
   while ( *head ) {

/* Obtain a pointer to the first element. */
      value = *head;

/* Issue a warning. */
      if ( value->is_object ) {
         astAddWarning( this, 1, "The Object \"%s = <%s>\" was "
                        "not recognised as valid input.", "astRead", status,
                        value->name, astGetClass( value->ptr.object ) );
      } else {
         astAddWarning( this, 1, "The value \"%s = %s\" was not "
                        "recognised as valid input.", "astRead", status,
                        value->name, value->ptr.string );
      }

/* Remove the Value structure from the list (which updates the head of
   list pointer) and free its resources. */
      RemoveValue( value, head, status );
      value = FreeValue( value, status );
   }
}

static AstChannelValue *FreeValue( AstChannelValue *value, int *status ) {
/*
*  Name:
*     FreeValue

*  Purpose:
*     Free a dynamically allocated Value structure.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     AstChannelValue *FreeValue( AstChannelValue *value, int *status )

*  Class Membership:
*     Channel member function.

*  Description:
*     This function frees a dynamically allocated Value structure,
*     releasing all resources used by it. The structure contents must
*     have been correctly initialised.

*  Parameters:
*     value
*        Pointer to the Value structure to be freed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A NULL pointer is always returned.

*  Notes:
*     - This function attempts to execute even if the global error
*     status is set on entry, although no further error report will be
*     made if it subsequently fails under these circumstances.
*/

/* Check that a non-NULL pointer has been supplied. */
   if ( value ) {

/* If the "name" component has been allocated, then free it. */
      if ( value->name ) value->name = astFree( value->name );

/* If the "ptr" component identifies an Object, then annul the Object
   pointer. */
      if ( value->is_object ) {
         if ( value->ptr.object ) {
            value->ptr.object = astAnnul( value->ptr.object );
         }

/* Otherwise, if it identifies a string, then free the string. */
      } else {
         if ( value->ptr.string ) {
            value->ptr.string = astFree( value->ptr.string );
         }
      }

/* Free the Value structure itself. */
      value = astFree( value );
   }

/* Return a NULL pointer. */
   return NULL;
}

static const char *GetAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a Channel.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Channel member function (over-rides the protected astGetAttrib
*     method inherited from the Object class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a Channel, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the Channel.
*     attrib
*        Pointer to a null terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to a null terminated string containing the attribute
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the Channel, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the Channel. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   AstChannel *this;             /* Pointer to the Channel structure */
   const char *result;           /* Pointer value to return */
   int comment;                  /* Comment attribute value */
   int full;                     /* Full attribute value */
   int indent;                   /* Indent attribute value */
   int report_level;             /* ReportLevel attribute value */
   int skip;                     /* Skip attribute value */
   int strict;                   /* Report errors insead of warnings? */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the Channel structure. */
   this = (AstChannel *) this_object;

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "getattrib_buff" as a null terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* Comment. */
/* -------- */
   if ( !strcmp( attrib, "comment" ) ) {
      comment = astGetComment( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", comment );
         result = getattrib_buff;
      }

/* Full. */
/* ----- */
   } else if ( !strcmp( attrib, "full" ) ) {
      full = astGetFull( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", full );
         result = getattrib_buff;
      }

/* Indent. */
/* ------- */
   } else if ( !strcmp( attrib, "indent" ) ) {
      indent = astGetIndent( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", indent );
         result = getattrib_buff;
      }

/* ReportLevel. */
/* ------------ */
   } else if ( !strcmp( attrib, "reportlevel" ) ) {
      report_level = astGetReportLevel( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", report_level );
         result = getattrib_buff;
      }

/* Skip. */
/* ----- */
   } else if ( !strcmp( attrib, "skip" ) ) {
      skip = astGetSkip( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", skip );
         result = getattrib_buff;
      }

/* SourceFile. */
/* ----------- */
   } else if ( !strcmp( attrib, "sourcefile" ) ) {
      result = astGetSourceFile( this );

/* SinkFile. */
/* --------- */
   } else if ( !strcmp( attrib, "sinkfile" ) ) {
      result = astGetSinkFile( this );

/* Strict. */
/* ------- */
   } else if ( !strcmp( attrib, "strict" ) ) {
      strict = astGetStrict( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", strict );
         result = getattrib_buff;
      }

/* If the attribute name was not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_getattrib)( this_object, attrib, status );
   }

/* Return the result. */
   return result;

}

static void GetNextData( AstChannel *this, int skip, char **name,
                         char **val, int *status ) {
/*
*+
*  Name:
*     astGetNextData

*  Purpose:
*     Read the next item of data from a data source.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "channel.h"
*     void astGetNextData( AstChannel *this, int skip, char **name,
*                          char **val )

*  Class Membership:
*     Channel method.

*  Description:
*     This function reads the next item of input data from a data
*     source associated with a Channel and returns the result.
*
*     It is layered conceptually on the astGetNextText method, but
*     instead of returning the raw input text, it decodes it and
*     returns name/value pairs ready for use. Note that in some
*     derived classes, where the data are not stored as text, this
*     function may not actually use astGetNextText, but will access
*     the data directly.

*  Parameters:
*     this
*        Pointer to the Channel.
*     skip
*        A non-zero value indicates that a new Object is to be read,
*        and that all input data up to the next "Begin" item are to be
*        skipped in order to locate it. This is useful if the data
*        source contains AST objects interspersed with other data (but
*        note that these other data cannot appear inside AST Objects,
*        only between them).
*
*        A zero value indicates that all input data are significant
*        and the next item will therefore be read and an attempt made
*        to interpret it whatever it contains. Any other data
*        inter-mixed with AST Objects will then result in an error.
*     name
*        An address at which to store a pointer to a null-terminated
*        dynamically allocated string containing the name of the next
*        item in the input data stream. This name will be in lower
*        case with no surrounding white space.  It is the callers
*        responsibilty to free the memory holding this string (using
*        astFree) when it is no longer required.
*
*        A NULL pointer value will be returned (without error) to
*        indicate when there are no further input data items to be
*        read.
*     val
*        An address at which to store a pointer to a null-terminated
*        dynamically allocated string containing the value associated
*        with the next item in the input data stream. No case
*        conversion is performed on this string and all white space is
*        potentially significant.  It is the callers responsibilty to
*        free the memory holding this string (using astFree) when it
*        is no longer required.
*
*        The returned pointer will be NULL if an Object data item is
*        read (see the "Data Representation" section).

*  Data Representation:
*     The returned data items fall into the following categories:
*
*     - Begin: Identified by the name string "begin", this indicates
*     the start of an Object definition. The associated value string
*     gives the class name of the Object being defined.
*
*     - IsA: Identified by the name string "isa", this indicates the
*     end of the data associated with a particular class structure
*     within the definiton of a larger Object. The associated value
*     string gives the name of the class whose data have just been
*     read.
*
*     - End: Identified by the name string "end", this indicates the
*     end of the data associated with a complete Object
*     definition. The associated value string gives the class name of
*     the Object whose definition is being ended.
*
*     - Non-Object: Identified by any other name string plus a
*     non-NULL "val" pointer, this gives the value of a non-Object
*     structure component (instance variable). The name identifies
*     which instance variable it is (within the context of the class
*     whose data are being read) and the value is encoded as a string.
*
*     - Object: Identified by any other name string plus a NULL "val"
*     pointer, this identifies the value of an Object structure
*     component (instance variable).  The name identifies which
*     instance variable it is (within the context of the class whose
*     data are being read) and the value is given by subsequent data
*     items (so the next item should be a "Begin" item).

*  Notes:
*     - NULL pointer values will be returned if this function is
*     invoked with the global error status set, or if it should fail
*     for any reason.
*     - This method is provided primarily so that derived classes may
*     over-ride it in order to read from alternative data sources. It
*     provides a higher-level interface than astGetNextText, so is
*     suitable for classes that either need to read textual data in a
*     different format, or to read from non-textual data sources.
*-
*/

/* Local Variables: */
   char *line;                   /* Pointer to input text line */
   int done;                     /* Data item read? */
   int i;                        /* Loop counter for string characters */
   int len;                      /* Length of input text line */
   int nc1;                      /* Offset to start of first field */
   int nc2;                      /* Offset to end of first field */
   int nc3;                      /* Offset to start of second field */
   int nc;                       /* Number of charaters read by "astSscanf" */

/* Initialise the returned values. */
   *name = NULL;
   *val = NULL;

/* Check the global error status. */
   if ( !astOK ) return;

/* Read the next input line as text (the loop is needed to allow
   initial lines to be skipped if the "skip" flag is set). */
   done = 0;
   while ( !done && ( line = InputTextItem( this, status ) ) && astOK ) {

/* If OK, determine the line length. */
      len = strlen( line );

/* Non-Object value. */
/* ----------------- */
/* Test for lines of the form " name = value" (or similar), where the
   name is no more than MAX_NAME characters long (the presence of a
   value on the right hand side indicates that this is a non-Object
   value, encoded as a string). Ignore these lines if the "skip" flag
   is set. */
      if ( nc = 0,
           ( !skip
             && ( 0 == astSscanf( line,
                               " %n%*" MAX_NAME "[^ \t=]%n = %n%*[^\n]%n",
                               &nc1, &nc2, &nc3, &nc ) )
             && ( nc >= len ) ) ) {

/* Note we have found a data item. */
         done = 1;

/* Extract the name and value fields. */
         *name = astString( line + nc1, nc2 - nc1 );
         *val = astString( line + nc3, len - nc3 );

/* If OK, truncate the value to remove any trailing white space. */
         if ( astOK ) {
            i = len - nc3 - 1;
            while ( ( i >= 0 ) && isspace( ( *val )[ i ] ) ) i--;
            ( *val )[ i + 1 ] = '\0';

/* Also remove any quotes from the string. */
            Unquote( this, *val, status );
         }

/* Object value. */
/* ------------- */
/* Test for lines of the form " name = " (or similar), where the name
   is no more than MAX_NAME characters long (the absence of a value on
   the right hand side indicates that this is an Object, whose
   definition follows on subsequent input lines). Ignore these lines
   if the "skip" flag is set. */
      } else if ( nc = 0,
                  ( !skip
                    && ( 0 == astSscanf( line,
                                      " %n%*" MAX_NAME "[^ \t=]%n = %n",
                                      &nc1, &nc2, &nc ) )
                    && ( nc >= len ) ) ) {

/* Note we have found a data item. */
         done = 1;

/* Extract the name field but leave the value pointer as NULL. */
         *name = astString( line + nc1, nc2 - nc1 );

/* Begin. */
/* ------ */
/* Test for lines of the form " Begin Class " (or similar). */
      } else if ( nc = 0,
             ( ( 0 == astSscanf( line,
                             " %*1[Bb]%*1[Ee]%*1[Gg]%*1[Ii]%*1[Nn] %n%*s%n %n",
                              &nc1, &nc2, &nc ) )
               && ( nc >= len ) ) ) {

/* Note we have found a data item. */
         done = 1;

/* Set the returned name to "begin" and extract the associated class
   name for the value. Store both of these in dynamically allocated
   strings. */
         *name = astString( "begin", 5 );
         *val = astString( line + nc1, nc2 - nc1 );

/* IsA. */
/* ---- */
/* Test for lines of the form " IsA Class " (or similar). Ignore these
   lies if the "skip" flag is set. */
      } else if ( nc = 0,
                  ( !skip
                    && ( 0 == astSscanf( line,
                                      " %*1[Ii]%*1[Ss]%*1[Aa] %n%*s%n %n",
                                      &nc1, &nc2, &nc ) )
                    && ( nc >= len ) ) ) {

/* Note we have found a data item. */
         done = 1;

/* Set the returned name to "isa" and extract the associated class
   name for the value. */
         *name = astString( "isa", 3 );
         *val = astString( line + nc1, nc2 - nc1 );

/* End. */
/* ---- */
/* Test for lines of the form " End Class " (or similar). Ignore these
   lines if the "skip" flag is set. */
      } else if ( nc = 0,
                  ( !skip
                    && ( 0 == astSscanf( line,
                                      " %*1[Ee]%*1[Nn]%*1[Dd] %n%*s%n %n",
                                      &nc1, &nc2, &nc ) )
                    && ( nc >= len ) ) ) {

/* Note we have found a data item. */
         done = 1;

/* If found, set the returned name to "end" and extract the associated
   class name for the value. */
         *name = astString( "end", 3 );
         *val = astString( line + nc1, nc2 - nc1 );

/* If the input line didn't match any of the above and the "skip" flag
   is not set, then report an error. */
      } else if ( !skip ) {
         astError( AST__BADIN,
                   "astRead(%s): Cannot interpret the input data: \"%s\".", status,
                   astGetClass( this ), line );
      }

/* Free the memory holding the input data as text. */
      line = astFree( line );
   }

/* If successful, convert the name to lower case. */
   if ( astOK && *name ) {
      for ( i = 0; ( *name )[ i ]; i++ ) {
         ( *name )[ i ] = tolower( ( *name )[ i ] );
      }
   }

/* If an error occurred, ensure that any memory allocated is freed and
   that NULL pointer values are returned. */
   if ( !astOK ) {
      *name = astFree( *name );
      *val = astFree( *val );
   }
}

static char *GetNextText( AstChannel *this, int *status ) {
/*
*+
*  Name:
*     GetNextText

*  Purpose:
*     Read the next line of input text from a data source.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "channel.h"
*     char *astGetNextText( AstChannel *this )

*  Class Membership:
*     Channel method.

*  Description:
*     This function reads the next "raw" input line of text from the
*     data source associated with a Channel.
*
*     Each line is returned as a pointer to a null-terminated string
*     held in dynamic memory, and it is the caller's responsibility to
*     free this memory (using astFree) when it is no longer
*     required. A NULL pointer is returned if there are no more input
*     lines to be read.

*  Parameters:
*     this
*        Pointer to the Channel.

*  Returned Value:
*     Pointer to a null-terminated string containing the input line
*     (held in dynamically allocated memory, which must be freed by
*     the caller when no longer required). A NULL pointer is returned
*     if there are no more input lines to be read.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*     - This method is provided primarily so that derived classes may
*     over-ride it in order to read from alternative (textual) data
*     sources.
*-
*/

/* Local Constants: */
#define MIN_CHARS 81             /* Initial size for allocating memory */
#define ERRBUF_LEN 80

/* Local Variables: */
   FILE *fd;                     /* Input file descriptor */
   char *errstat;                /* Pointer for system error message */
   char *line;                   /* Pointer to line data to be returned */
   char errbuf[ ERRBUF_LEN ];    /* Buffer for system error message */
   const char *sink_file;        /* Path to output sink file */
   const char *source_file;      /* Path to source file */
   int c;                        /* Input character */
   int len;                      /* Length of input line */
   int readstat;                 /* "errno" value set by "getchar" */
   int size;                     /* Size of allocated memory */

/* Initialise. */
   line = NULL;

/* Check the global error status. */
   if ( !astOK ) return line;

/* If the SourceFile attribute of the Channel specifies an input file,
   but no input file has yet been opened, open it now. Report an error if
   it is the same as the sink file. */
   if( astTestSourceFile( this ) && !this->fd_in ) {
      source_file = astGetSourceFile( this );

      if( this->fd_out ) {
         sink_file = astGetSinkFile( this );
         if( astOK && !strcmp( sink_file, source_file ) ) {
            astError( AST__RDERR, "astRead(%s): Failed to open input "
                      "SourceFile '%s' - the file is currently being used "
                      "as the output SinkFile.", status, astGetClass( this ),
                      source_file );
         }
      }

      if( astOK ) {
         this->fd_in = fopen( source_file, "r" );
         if( !this->fd_in ) {
            if ( errno ) {
#if HAVE_STRERROR_R
               strerror_r( errno, errbuf, ERRBUF_LEN );
               errstat = errbuf;
#else
               errstat = strerror( errno );
#endif
               astError( AST__RDERR, "astRead(%s): Failed to open input "
                         "SourceFile '%s' - %s.", status, astGetClass( this ),
                         source_file, errstat );
            } else {
               astError( AST__RDERR, "astRead(%s): Failed to open input "
                         "SourceFile '%s'.", status, astGetClass( this ),
                         source_file );
            }
         }

      }
   }

/* Source function defined, but no input file. */
/* ------------------------------------------- */
/* If no active input file descriptor is stored in the Channel, but
   a source function (and its wrapper function) is defined for the
   Channel, use the wrapper function to invoke the source function to
   read a line of input text. This is returned in a dynamically
   allocated string. */
   if ( !this->fd_in && this->source && this->source_wrap ) {

/* About to call an externally supplied function which may not be
   thread-safe, so lock a mutex first. Also store the channel data
   pointer in a global variable so that it can be accessed in the source
   function using macro astChannelData. */
      astStoreChannelData( this );
      LOCK_MUTEX3;
      line = ( *this->source_wrap )( this->source, status );
      UNLOCK_MUTEX3;

/* Input file defined, or no source function. */
/* ------------------------------------------ */
/* Read the line from the input file or from standard input. */
   } else if( astOK ) {
      c = '\0';
      len = 0;
      size = 0;

/* Choose the file descriptor to use. */
      fd = this->fd_in ? this->fd_in : stdin;

/* Loop to read input characters, saving any "errno" value that may be
   set by "getchar" if an error occurs. Quit if an end of file (or
   error) occurs or if a newline character is read. */
      while ( errno = 0, c = getc( fd ), readstat = errno,
              ( c != EOF ) && ( c != '\n' ) ) {

/* If no memory has yet been allocated to hold the line, allocate some
   now, using MIN_CHARS as the initial line length. */
         if ( !line ) {
            line = astMalloc( sizeof( char ) * (size_t) MIN_CHARS );
            size = MIN_CHARS;

/* If memory has previously been allocated, extend it when necessary
   to hold the new input character (plus a terminating null) and note
   the new size. */
         } else if ( ( len + 2 ) > size ) {
            line = astGrow( line, len + 2, sizeof( char ) );
            if ( !astOK ) break;
            size = (int) astSizeOf( line );
         }

/* Store the character just read. */
         line[ len++ ] = c;
      }

/* If the above loop completed without setting the global error
   status, check the last character read and use "ferror" to see if a
   read error occurred. If so, report the error, using the saved
   "errno" value (but only if one was set). */
      if ( astOK && ( c == EOF ) && ferror( fd ) ) {
         if ( readstat ) {
#if HAVE_STRERROR_R
            strerror_r( readstat, errbuf, ERRBUF_LEN );
            errstat = errbuf;
#else
            errstat = strerror( readstat );
#endif
            astError( AST__RDERR,
                      "astRead(%s): Read error on standard input - %s.", status,
                      astGetClass( this ), errstat );
         } else {
            astError( AST__RDERR,
                      "astRead(%s): Read error on standard input.", status,
                      astGetClass( this ) );
         }
      }

/* If an empty line has been read, allocate memory to hold an empty
   string. */
      if ( !line && ( c == '\n' ) ) {
         line = astMalloc( sizeof( char ) );
      }

/* If memory has been allocated and there has been no error,
   null-terminate the string of input characters. */
      if ( line ) {
         if ( astOK ) {
            line[ len ] = '\0';

/* If there has been an error, free the allocated memory. */
         } else {
            line = astFree( line );
         }
      }
   }


/* Return the result pointer. */
   return line;

/* Undefine macros local to this function. */
#undef MIN_CHARS
#undef ERRBUF_LEN
}

static AstKeyMap *Warnings( AstChannel *this, int *status ){
/*
*++
*  Name:
c     astWarnings
f     AST_WARNINGS

*  Purpose:
*     Returns any warnings issued by the previous read or write operation.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "channel.h"
c     AstKeyMap *astWarnings( AstChannel *this )
f     RESULT = AST_WARNINGS( THIS, STATUS )

*  Class Membership:
*     Channel member function.

*  Description:
*     This function returns an AST KeyMap object holding the text of any
*     warnings issued as a result of the previous invocation of the
c     astRead or astWrite
f     AST_READ or AST_WRITE
*     function on the Channel. If no warnings were issued, a
c     a NULL value
f     AST__NULL
*     will be returned.
*
*     Such warnings are non-fatal and will not prevent the
*     read or write operation succeeding. However, the converted object
*     may not be identical to the original object in all respects.
*     Differences which would usually be deemed as insignificant in most
*     usual cases will generate a warning, whereas more significant
*     differences will generate an error.
*
*     The "Strict" attribute allows this warning facility to be switched
*     off, so that a fatal error is always reported for any conversion
*     error.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Channel.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astWarnings()
f     AST_WARNINGS = INTEGER
*        A pointer to the KeyMap holding the warning messages, or
c        NULL
f        AST__NULL
*        if no warnings were issued during the previous read operation.

*  Applicability:
*     Channel
*        The basic Channel class generates a warning when ever an
*        un-recognised item is encountered whilst reading an Object from
*        an external data source. If Strict is zero (the default), then
*        unexpected items in the Object description are simply ignored,
*        and any remaining items are used to construct the returned
*        Object. If Strict is non-zero, an error will be reported and a
*        NULL Object pointer returned if any unexpected items are
*        encountered.
*
*        As AST continues to be developed, new attributes are added
*        occasionally to selected classes. If an older version of AST is
*        used to read external Object descriptions created by a more
*        recent version of AST, then the Channel class will, by default,
*        ignore the new attributes, using the remaining attributes to
*        construct the Object. This is usually a good thing. However,
*        since external Object descriptions are often stored in plain
*        text, it is possible to edit them using a text editor. This
*        gives rise to the possibility of genuine errors in the
*        description due to finger-slips, typos, or simple
*        mis-understanding. Such inappropriate attributes will be ignored
*        if Strict is left at its default zero value. This will cause the
*        mis-spelled attribute to revert to its default value,
*        potentially causing subtle changes in the behaviour of
*        application software. If such an effect is suspected, the Strict
*        attribute can be set non-zero, resulting in the erroneous
*        attribute being identified in an error message.
*     FitsChan
*        The returned KeyMap will contain warnings for all conditions
*        listed in the Warnings attribute.
*     XmlChan
*        Reports conversion errors that result in what are usally
*        insignificant  changes.

*  Notes:
*     - The returned KeyMap uses keys of the form "Warning_1",
*     "Warning_2", etc.
*     - A value of
c     NULL will be returned if this function is invoked with the AST
c     error status set,
f     AST__NULL will be returned if this function is invoked with STATUS
f     set to an error value,
*     or if it should fail for any reason.
*--
*/

/* Local Variables: */
   AstKeyMap *result;
   char key[ 20 ];
   int i;

/* Check the global status, and supplied keyword name. */
   result = NULL;
   if( !astOK ) return result;

/* Check there are some warnings to return. */
   if( this->nwarn && this->warnings ) {

/* Create the KeyMap. */
      result = astKeyMap( "", status );

/* Loop round all warnings, adding them into the KeyMap. */
      for( i = 0; i < this->nwarn; i++ ){
         sprintf( key, "Warning_%d", i + 1 );
         astMapPut0C( result, key, (this->warnings)[ i ], " " );
      }
   }

/* Return the KeyMap. */
   return result;
}

AstChannel *astInitChannel_( void *mem, size_t size, int init,
                             AstChannelVtab *vtab, const char *name,
                             const char *(* source)( void ),
                             char *(* source_wrap)( const char *(*)( void ), int * ),
                             void (* sink)( const char * ),
                             void (* sink_wrap)( void (*)( const char * ),
                                                 const char *, int * ), int *status ) {
/*
*+
*  Name:
*     astInitChannel

*  Purpose:
*     Initialise a Channel.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "channel.h"
*     AstChannel *astInitChannel( void *mem, size_t size, int init,
*                                 AstChannelVtab *vtab, const char *name,
*                                 const char *(* source)( void ),
*                                 char *(* source_wrap)( const char *(*)( void ), int * ),
*                                 void (* sink)( const char * ),
*                                 void (* sink_wrap)( void (*)( const char * ),
*                                                     const char *, int * ) )

*  Class Membership:
*     Channel initialiser.

*  Description:
*     This function is provided for use by class implementations to
*     initialise a new Channel object. It allocates memory (if
*     necessary) to accommodate the Channel plus any additional data
*     associated with the derived class.  It then initialises a
*     Channel structure at the start of this memory. If the "init"
*     flag is set, it also initialises the contents of a virtual
*     function table for a Channel at the start of the memory passed
*     via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the Channel is to be
*        initialised.  This must be of sufficient size to accommodate
*        the Channel data (sizeof(Channel)) plus any data used by the
*        derived class. If a value of NULL is given, this function
*        will allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the Channel (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the Channel structure, so a valid value must be
*        supplied even if not required for allocating memory.
*     init
*        A boolean flag indicating if the Channel's virtual function
*        table is to be initialised. If this value is non-zero, the
*        virtual function table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new Channel.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*     source
*        Pointer to a "source" function which will be used to obtain
*        lines of input text. Generally, this will be obtained by
*        casting a pointer to a source function which is compatible
*        with the "source_wrap" wrapper function (below). The pointer
*        should later be cast back to its original type by the
*        "source_wrap" function before the function is invoked.
*
*        If "source" is NULL, the Channel will read from standard
*        input instead.
*     source_wrap
*        Pointer to a function which can be used to invoke the
*        "source" function supplied (above). This wrapper function is
*        necessary in order to hide variations in the nature of the
*        source function, such as may arise when it is supplied by a
*        foreign (non-C) language interface.
*
*        The single parameter of the "source_wrap" function is a
*        pointer to the "source" function, and it should cast this
*        function pointer (as necessary) and invoke the function with
*        appropriate arguments to obtain the next line of input
*        text. The "source_wrap" function should then return a pointer
*        to a dynamically allocated, null terminated string containing
*        the text that was read. The string will be freed (using
*        astFree) when no longer required and the "source_wrap"
*        function need not concern itself with this. A NULL pointer
*        should be returned if there is no more input to read.
*
*        If "source_wrap" is NULL, the Channel will read from standard
*        input instead.
*     sink
*        Pointer to a "sink" function which will be used to deliver
*        lines of output text. Generally, this will be obtained by
*        casting a pointer to a sink function which is compatible with
*        the "sink_wrap" wrapper function (below). The pointer should
*        later be cast back to its original type by the "sink_wrap"
*        function before the function is invoked.
*
*        If "sink" is NULL, the Channel will write to standard output
*        instead.
*     sink_wrap
*        Pointer to a function which can be used to invoke the "sink"
*        function supplied (above). This wrapper function is necessary
*        in order to hide variations in the nature of the sink
*        function, such as may arise when it is supplied by a foreign
*        (non-C) language interface.
*
*        The first parameter of the "sink_wrap" function is a pointer
*        to the "sink" function, and the second parameter is a pointer
*        to a const, null-terminated character string containing the
*        text to be written.  The "sink_wrap" function should cast the
*        "sink" function pointer (as necessary) and invoke the
*        function with appropriate arguments to deliver the line of
*        output text. The "sink_wrap" function then returns void.
*
*        If "sink_wrap" is NULL, the Channel will write to standard
*        output instead.

*  Returned Value:
*     A pointer to the new Channel.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstChannel *new;              /* Pointer to new Channel */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitChannelVtab( vtab, name );

/* Initialise an Object structure (the parent class) as the first
   component within the Channel structure, allocating memory if
   necessary. */
   new = (AstChannel *) astInitObject( mem, size, 0,
                                       (AstObjectVtab *) vtab, name );

   if ( astOK ) {

/* Initialise the Channel data. */
/* ---------------------------- */
/* Save the pointers to the source and sink functions and the wrapper
   functions that invoke them. */
      new->source = source;
      new->source_wrap = source_wrap;
      new->sink = sink;
      new->sink_wrap = sink_wrap;

/* Indicate no input or output files have been associated with the
   Channel. */
      new->fd_in = NULL;
      new->fn_in = NULL;
      new->fd_out = NULL;
      new->fn_out = NULL;

/* Set all attributes to their undefined values. */
      new->comment = -INT_MAX;
      new->full = -INT_MAX;
      new->indent = -INT_MAX;
      new->report_level = -INT_MAX;
      new->skip = -INT_MAX;
      new->strict = -INT_MAX;
      new->data = NULL;
      new->warnings = NULL;
      new->nwarn = 0;

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new object. */
   return new;
}

void astInitChannelVtab_(  AstChannelVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitChannelVtab

*  Purpose:
*     Initialise a virtual function table for a Channel.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "channel.h"
*     void astInitChannelVtab( AstChannelVtab *vtab, const char *name )

*  Class Membership:
*     Channel vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the Channel class.

*  Parameters:
*     vtab
*        Pointer to the virtual function table. The components used by
*        all ancestral classes will be initialised if they have not already
*        been initialised.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the virtual function table belongs (it
*        is this pointer value that will subsequently be returned by the Object
*        astClass function).
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitObjectVtab( (AstObjectVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAChannel) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstObjectVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that
   provide virtual methods for this class. */
   vtab->AddWarning = AddWarning;
   vtab->ClearComment = ClearComment;
   vtab->ClearFull = ClearFull;
   vtab->ClearSkip = ClearSkip;
   vtab->ClearStrict = ClearStrict;
   vtab->GetComment = GetComment;
   vtab->GetFull = GetFull;
   vtab->GetNextData = GetNextData;
   vtab->GetNextText = GetNextText;
   vtab->GetSkip = GetSkip;
   vtab->GetStrict = GetStrict;
   vtab->Warnings = Warnings;
   vtab->PutNextText = PutNextText;
   vtab->Read = Read;
   vtab->ReadClassData = ReadClassData;
   vtab->ReadDouble = ReadDouble;
   vtab->ReadInt = ReadInt;
   vtab->ReadObject = ReadObject;
   vtab->ReadString = ReadString;
   vtab->SetComment = SetComment;
   vtab->SetFull = SetFull;
   vtab->SetSkip = SetSkip;
   vtab->SetStrict = SetStrict;
   vtab->TestComment = TestComment;
   vtab->TestFull = TestFull;
   vtab->TestSkip = TestSkip;
   vtab->TestStrict = TestStrict;
   vtab->Write = Write;
   vtab->WriteBegin = WriteBegin;
   vtab->WriteDouble = WriteDouble;
   vtab->WriteEnd = WriteEnd;
   vtab->WriteInt = WriteInt;
   vtab->WriteIsA = WriteIsA;
   vtab->WriteObject = WriteObject;
   vtab->WriteString = WriteString;
   vtab->PutChannelData = PutChannelData;

   vtab->ClearReportLevel = ClearReportLevel;
   vtab->GetReportLevel = GetReportLevel;
   vtab->SetReportLevel = SetReportLevel;
   vtab->TestReportLevel = TestReportLevel;

   vtab->ClearIndent = ClearIndent;
   vtab->GetIndent = GetIndent;
   vtab->SetIndent = SetIndent;
   vtab->TestIndent = TestIndent;

   vtab->ClearSourceFile = ClearSourceFile;
   vtab->GetSourceFile = GetSourceFile;
   vtab->SetSourceFile = SetSourceFile;
   vtab->TestSourceFile = TestSourceFile;

   vtab->ClearSinkFile = ClearSinkFile;
   vtab->GetSinkFile = GetSinkFile;
   vtab->SetSinkFile = SetSinkFile;
   vtab->TestSinkFile = TestSinkFile;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;

   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;
   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;
   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;
   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;

/* Declare the destructor and copy constructor. */
   astSetDelete( (AstObjectVtab *) vtab, Delete );
   astSetCopy( (AstObjectVtab *) vtab, Copy );

/* Declare the Dump function for this class. There is no destructor or
   copy constructor. */
   astSetDump( vtab, Dump, "Channel", "Basic I/O Channel" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static char *InputTextItem( AstChannel *this, int *status ) {
/*
*  Name:
*     InputTextItem

*  Purpose:
*     Read the next item from a data source as text.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     char *InputTextItem( AstChannel *this )

*  Class Membership:
*     Channel member function.

*  Description:
*     This function reads the next input data item as text from the
*     data source associated with a Channel. It is similar to the
*     astGetNextText method (which it invokes), except that it strips
*     off any comments along with leading and trailing white
*     space. Input lines which are empty or do not contain significant
*     characters (e.g. all comment) are skipped, so that only
*     significant lines are returned.
*
*     Each line is returned as a pointer to a null-terminated string
*     held in dynamic memory, and it is the caller's responsibility to
*     free this memory (using astFree) when it is no longer
*     required. A NULL pointer is returned if there are no more input
*     lines to be read.

*  Parameters:
*     this
*        Pointer to the Channel.

*  Returned Value:
*     Pointer to a null-terminated string containing the input line
*     (held in dynamically allocated memory, which must be freed by
*     the caller when no longer required). A NULL pointer is returned
*     if there are no more input lines to be read.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   char *line;                   /* Pointer to line data to be returned */
   int i;                        /* Loop counter for line characters */
   int j;                        /* Counter for characters */
   int len;                      /* Length of result line */
   int nonspace;                 /* Non-space character encountered? */
   int quoted;                   /* Character is inside quotes? */

/* Initialise. */
   line = NULL;

/* Check the global error status. */
   if ( !astOK ) return line;

/* Loop to read input lines until one is found which contains useful
   characters or end of file is reached (or a read error occurs). */
   while ( !line && ( line = astGetNextText( this ) ) && astOK ) {

/* Loop to remove comments and leading and trailing white space. */
      len = 0;
      nonspace = 0;
      quoted = 0;
      for ( i = j = 0; line[ i ]; i++ ) {

/* Note quote characters and ignore all text after the first unquoted
   comment character. */
         if ( line[ i ] == '"' ) quoted = !quoted;
         if ( ( line[ i ] == '#' ) && !quoted ) break;

/* Note the first non-space character and ignore everything before
   it. */
         if ( ( nonspace = nonspace || !isspace( line[ i ] ) ) ) {

/* Move each character to its new position in the string. */
            line[ j++ ] = line[ i ];

/* Note the final length of the string (ignoring trailing spaces). */
            if ( !isspace( line[ i ] ) ) len = j;
         }
      }

/* If the string is not empty, terminate it. */
     if ( len ) {
        line[ len ] = '\0';

/* Otherwise, free the memory used for the string so that another
   input line will be read. */
      } else {
         line = astFree( line );
      }
   }

/* Return the result pointer. */
   return line;

/* Undefine macros local to this function. */
#undef MIN_CHARS
}

static AstChannelValue *LookupValue( const char *name, int *status ) {
/*
*  Name:
*     LookupValue

*  Purpose:
*     Look up a Value structure by name.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     AstChannelValue *LookupValue( const char *name )

*  Class Membership:
*     Channel member function.

*  Description:
*     This function searches the current values list (i.e. at the
*     current nesting level) to identify a Value structure with a
*     specified name. If one is found, it is removed from the list and
*     a pointer to it is returned. If no suitable Value can be found,
*     a NULL pointer is returned instead.

*  Parameters:
*     name
*        Pointer to a constant null-terminated character string
*        containing the name of the required Value. This must be in
*        lower case with no surrounding white space. Note that names
*        longer than NAME_MAX characters will not match any Value.

*  Returned value:
*     Pointer to the required Value structure, or NULL if no suitable
*     Value exists.

*  Notes:
*     - The returned pointer refers to a dynamically allocated
*     structure and it is the callers responsibility to free this when
*     no longer required. The FreeValue function must be used for this
*     purpose.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   AstChannelValue **head;       /* Address of head of list pointer */
   AstChannelValue *result;      /* Pointer value to return */
   AstChannelValue *value;       /* Pointer to list element */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(NULL);

/* Check that the "values_ok" flag is set. If not, the Values in the
   values list belong to a different class to that of the current
   class loader, so we cannot return any Value. */
   if ( values_ok[ nest ] ) {

/* Obtain the address of the current "head of list" pointer for the
   values list (at the current nesting level). */
      head = values_list + nest;

/* Obtain the head of list pointer itself and check the list is not
   empty. */
      if ( ( value = *head ) ) {

/* Loop to inspect each list element. */
         while ( 1 ) {

/* If a name match is found, remove the element from the list, return
   a pointer to it and quit searching. */
            if ( !strcmp( name, value->name ) ) {
               RemoveValue( value, head, status );
               result = value;
               break;
            }

/* Follow the list until we return to the head. */
            value = value->flink;
            if ( value == *head ) break;
         }
      }
   }

/* Return the result. */
   return result;
}

static void OutputTextItem( AstChannel *this, const char *line, int *status ) {
/*
*  Name:
*     OutputTextItem

*  Purpose:
*     Output a data item formatted as text.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     void OutputTextItem( AstChannel *this, const char *line, int *status )

*  Class Membership:
*     Channel member function.

*  Description:
*     This function outputs a data item formatted as a text string to
*     a data sink associated with a Channel. It keeps track of the
*     number of items written.

*  Parameters:
*     this
*        Pointer to the Channel.
*     line
*        Pointer to a constant null-terminated string containing the
*        data item to be output (no newline character should be
*        appended).
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* Write out the line of text using the astPutNextText method (which
   may be over-ridden). */
   astPutNextText( this, line );

/* If successful, increment the count of items written. */
   if ( astOK ) items_written++;
}

static void PutChannelData( AstChannel *this, void *data, int *status ) {
/*
c++
*  Name:
*     astPutChannelData

*  Purpose:
*     Store arbitrary data to be passed to a source or sink function.

*  Type:
*     Public function.

*  Synopsis:
*     #include "channel.h"
*     void astPutChannelData( AstChannel *this, void *data )

*  Class Membership:
*     Channel method.

*  Description:
*     This function stores a supplied arbitrary pointer in the Channel.
*     When a source or sink function is invoked by the Channel, the
*     invoked function can use the astChannelData macro to retrieve the
*     pointer. This provides a thread-safe alternative to passing file
*     descriptors, etc, via global static variables.

*  Parameters:
*     this
*        Pointer to the Channel.
*     data
*        A pointer to be made available to the source and sink functions
*        via the astChannelData macro. May be NULL.

*  Applicability:
*     Channel
*        All Channels have this function.

*  Notes:
*     - This routine is not available in the Fortran 77 interface to
*     the AST library.
c--
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Store the pointer. */
   this->data = data;
}

static void PutNextText( AstChannel *this, const char *line, int *status ) {
/*
*+
*  Name:
*     astPutNextText

*  Purpose:
*     Write a line of output text to a data sink.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "channel.h"
*     void astPutNextText( AstChannel *this, const char *line )

*  Class Membership:
*     Channel method.

*  Description:
*     This function writes an output line of text to a data sink
*     associated with a Channel.

*  Parameters:
*     this
*        Pointer to the Channel.
*     line
*        Pointer to a constant null-terminated string containing the
*        line of output text to be written (no newline character
*        should be appended).

*  Notes:
*     - This method is provided primarily so that derived classes may
*     over-ride it in order to write to alternative (textual) data
*     sinks.
*-
*/

/* Local Constants: */
#define ERRBUF_LEN 80

/* Local Variables: */
   char *errstat;                /* Pointer for system error message */
   char errbuf[ ERRBUF_LEN ];    /* Buffer for system error message */
   const char *sink_file;        /* Path to output sink file */
   const char *source_file;      /* Path to output source file */

/* Check the global error status. */
   if ( !astOK ) return;

/* If the SinkFile attribute of the Channel specifies an output file,
   but no output file has yet been opened, open it now. Report an error
   if it is the same as the source file. */
   if( astTestSinkFile( this ) && !this->fd_out ) {
      sink_file = astGetSinkFile( this );

      if( this->fd_out ) {
         source_file = astGetSourceFile( this );
         if( astOK && !strcmp( sink_file, source_file ) ) {
            astError( AST__WRERR, "astWrite(%s): Failed to open output "
                      "SinkFile '%s' - the file is currently being used "
                      "as the input SourceFile.", status, astGetClass( this ),
                      sink_file );
         }
      }

      if( astOK ) {
         this->fd_out = fopen( sink_file, "w" );
         if( !this->fd_out ) {
            if ( errno ) {
#if HAVE_STRERROR_R
               strerror_r( errno, errbuf, ERRBUF_LEN );
               errstat = errbuf;
#else
               errstat = strerror( errno );
#endif
               astError( AST__WRERR, "astWrite(%s): Failed to open output "
                         "SinkFile '%s' - %s.", status, astGetClass( this ),
                         sink_file, errstat );
            } else {
               astError( AST__WRERR, "astWrite(%s): Failed to open output "
                         "SinkFile '%s'.", status, astGetClass( this ),
                         sink_file );
            }
         }
      }
   }

/* Check no error occurred above. */
   if( astOK ) {

/* If an active output file descriptor is stored in the channel, write
   the text to it, with a newline appended. */
      if( this->fd_out ) {
         (void) fprintf( this->fd_out, "%s\n", line );

/* Otherwise, if a sink function (and its wrapper function) is defined for
   the Channel, use the wrapper function to invoke the sink function to
   output the text line. Since we are about to call an externally supplied
   function which may not be thread-safe, lock a mutex first. Also store
   the channel data pointer in a global variable so that it can be accessed
   in the source function using macro astChannelData. */
      } else if ( this->sink && this->sink_wrap ) {
         astStoreChannelData( this );
         LOCK_MUTEX2;
         ( *this->sink_wrap )( *this->sink, line, status );
         UNLOCK_MUTEX2;

/* Otherwise, simply write the text to standard output with a newline
   appended. */
      } else {
         (void) printf( "%s\n", line );
      }
   }
}

static AstObject *Read( AstChannel *this, int *status ) {
/*
*++
*  Name:
c     astRead
f     AST_READ

*  Purpose:
*     Read an Object from a Channel.

*  Type:
*     Public function.

*  Synopsis:
c     #include "channel.h"
c     AstObject *astRead( AstChannel *this )
f     RESULT = AST_READ( THIS, STATUS )

*  Class Membership:
*     Channel method.

*  Description:
*     This function reads the next Object from a Channel and returns a
*     pointer to the new Object.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Channel.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astRead()
f     AST_READ = INTEGER
*        A pointer to the new Object. The class to which this will
*        belong is determined by the input data, so is not known in
*        advance.

*  Applicability:
*     FitsChan
c        All successful use of astRead on a FitsChan is destructive, so that
f        All successful use of AST_READ on a FitsChan is destructive, so that
*        FITS header cards are consumed in the process of reading an Object,
*        and are removed from the FitsChan (this deletion can be prevented
*        for specific cards by calling the FitsChan
c        astRetainFits function).
f        AST_RETAINFITS routine).
*        An unsuccessful call of
c        astRead
f        AST_READ
*        (for instance, caused by the FitsChan not containing the necessary
*        FITS headers cards needed to create an Object) results in the
*        contents of the FitsChan being left unchanged.
*     StcsChan
*        The AST Object returned by a successful use of
c        astRead
f        AST_READ
*        on an StcsChan, will be either a Region or a KeyMap, depending
*        on the values of the StcsArea, StcsCoords and StcsProps
*        attributes. See the documentation for these attributes for further
*        information.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned, without
*     error, if the Channel contains no further Objects to be read.
*     - A null Object pointer will also be returned if this function
c     is invoked with the AST error status set, or if it should fail
f     is invoked with STATUS set to an error value, or if it should fail
*     for any reason.
*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   AstLoaderType *loader;        /* Pointer to loader for Object */
   AstObject *new;               /* Pointer to new Object */
   char *class;                  /* Pointer to Object class name string */
   char *name;                   /* Pointer to data item name */
   int skip;                     /* Skip non-AST data? */
   int top;                      /* Reading top-level Object definition? */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* Determine if we are reading a top-level (i.e. user-level) Object
   definition, as opposed to the definition of an Object contained
   within another Object. This is indicated by the current nesting
   level. */
   top = ( nest == -1 );

/* If reading a top-level object, determine if data lying in between
   Object definitions in the input data stream are to be skipped. */
   skip = ( top && astGetSkip( this ) );

/* Read the next input data item. If we are reading a top-level Object
   definition, skip any unrelated data beforehand. Otherwise read the
   data strictly as it comes (there should be no unrelated data
   embedded within Object definitions themselves). */
   astGetNextData( this, skip, &name, &class );

/* If no suitable data item was found (and no error occurred), we have
   reached the end of data. For a top-level Object a NULL Object
   pointer is simply returned, but for a nested Object this indicates
   that part of the Object definition is missing, so report an
   error. */
   if ( astOK ) {
      if ( !name ) {
         if ( !top ) {
            astError( AST__EOCHN,
                      "astRead(%s): End of input encountered while trying to "
                      "read an AST Object.", status, astGetClass( this ) );
         }

/* If a data item was found, check it is a "Begin" item. If not, there
   is a data item missing, so report an error and free all memory. */
      } else if ( strcmp( name, "begin" ) ) {
         astError( AST__BADIN,
                   "astRead(%s): Missing \"Begin\" when expecting an Object.", status,
                   astGetClass( this ) );
         name = astFree( name );
         if ( class ) class = astFree( class );

/* If the required "Begin" item was found, free the memory used for the
   name string. */
      } else {
         name = astFree( name );

/* Use the associated class name to locate the loader for that
   class. This function will then be used to build the Object. */
         loader = astGetLoader( class, status );

/* Extend all necessary stack arrays to accommodate entries for the
   next nesting level (this allocates space if none has yet been
   allocated). */
         end_of_object = astGrow( end_of_object, nest + 2, sizeof( int ) );
         object_class = astGrow( object_class, nest + 2, sizeof( char * ) );
         values_class = astGrow( values_class, nest + 2, sizeof( char * ) );
         values_list = astGrow( values_list, nest + 2, sizeof( AstChannelValue * ) );
         values_ok = astGrow( values_ok, nest + 2, sizeof( int ) );

/* If an error occurred, free the memory used by the class string,
   which will not now be used. */
         if ( !astOK ) {
            class = astFree( class );

/* Otherwise, increment the nesting level and initialise the new stack
   elements for this new level. This includes clearing the
   "end_of_object" flag so that ReadClassData can read more data, and
   storing the class name of the object we are about to read. */
         } else {
            nest++;
            end_of_object[ nest ] = 0;
            object_class[ nest ] = class;
            values_class[ nest ] = NULL;
            values_list[ nest ] = NULL;
            values_ok[ nest ] = 0;

/* Invoke the loader, which reads the Object definition from the input
   data stream and builds the Object. Supply NULL/zero values to the
   loader so that it will substitute values appropriate to its own
   class. */
            new = (*loader)( NULL, (size_t) 0, NULL, NULL, this, status );

/* Clear the values list for the current nesting level. If the list
   has not been read or any Values remain in it, an error will
   result. */
            ClearValues( this, status );

/* If no error has yet occurred, check that the "end_of_object" flag
   has been set. If not, the input data were not correctly terminated,
   so report an error. */
            if ( astOK && !end_of_object[ nest ] ) {
               astError( AST__BADIN,
                         "astRead(%s): Unexpected end of input (missing end "
                         "of %s).", status,
                         astGetClass( this ), object_class[ nest ] );
            }

/* If an error occurred, report contextual information. Only do this
   for top-level Objects to avoid multple messages. */
            if ( !astOK && top ) {
               astError( astStatus, "Error while reading a %s from a %s.", status,
                         class, astGetClass( this ) );
            }

/* Clear the Object's class string, freeing the associated memory
   (note this is the memory allocated for the "class" string
   earlier). */
            object_class[ nest ] = astFree( object_class[ nest ] );

/* Restore the previous nesting level. */
            nest--;
         }

/* Once the top-level Object has been built, free the memory used by
   the stack arrays. */
         if ( top ) {
            end_of_object = astFree( end_of_object );
            object_class = astFree( object_class );
            values_class = astFree( values_class );
            values_list = astFree( values_list );
            values_ok = astFree( values_ok );
         }
      }
   }

/* If an error occurred, clean up by deleting the new Object and
   return a NULL pointer. */
   if ( !astOK ) new = astDelete( new );

/* Return the pointer to the new Object. */
   return new;
}

static void ReadClassData( AstChannel *this, const char *class, int *status ) {
/*
*+
*  Name:
*     astReadClassData

*  Purpose:
*     Read values from a data source for a class loader.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "channel.h"
*     void astReadClassData( AstChannel *this, const char *class )

*  Class Membership:
*     Channel method.

*  Description:
*     This function reads the data for a class from the data source
*     associated with a Channel, so as to provide values for
*     initialising the instance variables of that class as part of
*     building a complete Object. This function should be invoked by
*     the loader for each class immediately before it attempts to read
*     these values.
*
*     The values read are placed into the current values list by this
*     function. They may then be read from this list by the class
*     loader making calls to astReadDouble, astReadInt, astReadObject
*     and astReadString. The order in which values are read by the
*     loader is unimportant (although using the same order for reading
*     as for writing will usually be more efficient) and values are
*     removed from the list as they are read.

*  Parameters:
*     this
*        Pointer to the Channel.
*     class
*        A pointer to a constant null-terminated string containing the
*        name of the class whose loader is requesting the data (note
*        this is not usually the same as the class name of the Object
*        being built). This value allows the class structure of the
*        input data to be validated.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   AstObject *object;            /* Pointer to new Object */
   AstChannelValue *value;       /* Pointer to Value structure */
   char *name;                   /* Pointer to data item name string */
   char *val;                    /* Pointer to data item value string */
   int done;                     /* All class data read? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* If the "values_ok" flag is set, this indicates that the values list
   (at the current nesting level) has been filled by a previous
   invocation of this function and has then been read by the
   appropriate class loader. In this case, clear any entries which may
   remain in the current values list. If any such entries are found,
   they represent input data that were not read, so an error will
   result. */
   if ( values_ok[ nest ] ) ClearValues( this, status );

/* If "values_class" is non-NULL, this indicates that the values list
   (at the current nesting level) has been filled by a previous
   invocation of this function, but that the values belong to a class
   whose loader has not yet tried to read them. In this case, we must
   continue to keep the values until they are needed, so we do not
   read any more input data this time. */
   if ( values_class[ nest ] ) {

/* If the class to which the previously saved values belong matches
   the class we now want values for, set the "values_ok" flag. This
   then allows the values to be accessed (by LookupValue). */
      values_ok[ nest ] = !strcmp( values_class[ nest ], class );

/* If the current values list is empty, we must read in values for the
   next class that appears in the input data. However, first check
   that the "end_of_object" flag has not been set. If it has, we have
   already reached the end of this Object's data, so there is some
   kind of problem with the order in which class loaders have been
   invoked. This will probably never happen, but report an error if
   necessary. */
   } else if ( end_of_object[ nest ] ) {
      astError( AST__LDERR,
                "astRead(%s): Invalid attempt to read further %s data "
                "following an end of %s.", status,
                astGetClass( this ), class, object_class[ nest ] );
      astError( AST__LDERR,
                "Perhaps the wrong class loader has been invoked?" , status);

/* If we need new values, loop to read input data items until the end
   of the data for a class is reached. */
   } else {
      done = 0;
      while ( astOK && !done ) {

/* Read the next input data item. */
         astGetNextData( this, 0, &name, &val );
         if ( astOK ) {

/* Unexpected end of input. */
/* ------------------------ */
/* If no "name" value is returned, we have reached the end of the
   input data stream without finding the required end of class
   terminator, so report an error. */
            if ( !name ) {
               astError( AST__EOCHN,
                         "astRead(%s): Unexpected end of input (missing end "
                         "of %s).", status,
                         astGetClass( this ), object_class[ nest ] );

/* "IsA" item. */
/* ----------- */
/* Otherwise, if an "IsA" item was read, it indicates the end of the
   data for a class. Store the pointer to the name of this class in
   "values_class" and note whether this is the class whose data we
   wanted in "values_ok". If the data we have read do not belong to
   the class we wanted, they will simply be kept until the right class
   comes looking for them. */
            } else if ( !strcmp( name, "isa" ) ) {
               values_class[ nest ] = val;
               values_ok[ nest ] = !strcmp( val, class );

/* Free the memory holding the name string. */
               name = astFree( name );

/* Note we have finished reading class data. */
               done = 1;

/* "End" item. */
/* ----------- */
/* If an "End" item was read, it indicates the end of the data both
   for a class and for the current Object definition as a whole. Set
   the "end_of_object" flag (for the current nesting level) which
   prevents any further data being read for this Object. This flag is
   also used (by Read) to check that an "End" item was actually
   read. */
            } else if ( !strcmp( name, "end" ) ) {
               end_of_object[ nest ] = 1;

/* Check that the class name in the "End" item matches that of the
   Object being built. If so, store the pointer to the name of this
   class in "values_class" and note whether this is the class whose
   data we wanted in "values_ok". If the data we have read do not
   belong to the class we wanted, they will simply be kept until the
   right class comes looking for them. */
               if ( !strcmp( val, object_class[ nest ] ) ) {
                  values_class[ nest ] = val;
                  values_ok[ nest ] = !strcmp( class, val );

/* If the "End" item contains the wrong class name (i.e. not matching
   the corresponding "Begin" item), then report an error. */
               } else {
                  astError( AST__BADIN,
                            "astRead(%s): Bad class structure in input data.", status,
                            astGetClass( this ) );
                  astError( AST__BADIN,
                            "End of %s read when expecting end of %s.", status,
                            val, object_class[ nest ] );

/* Free the memory used by the class string, which will not now be
   used. */
                  val = astFree( val );
               }

/* Free the memory holding the name string. */
               name = astFree( name );

/* Note we have finished reading class data. */
               done = 1;

/* String value. */
/* ------------- */
/* If any other name is obtained and "val" is not NULL, we have read a
   non-Object value, encoded as a string. Allocate memory for a Value
   structure to describe it. */
            } else if ( val ) {
               value = astMalloc( sizeof( AstChannelValue ) );
               if ( astOK ) {

/* Store pointers to the name and value string in the Value structure
   and note this is not an Object value. */
                  value->name = name;
                  value->ptr.string = val;
                  value->is_object = 0;

/* Append the Value structure to the values list for the current
   nesting level. */
                  AppendValue( value, values_list + nest, status );

/* If an error occurred, free the memory holding the "name" and "val"
   strings. */
               } else {
                  name = astFree( name );
                  val = astFree( val );
               }

/* Object value. */
/* ------------- */
/* If "val" is NULL, we have read an Object item, and the Object
   definition should follow. Allocate memory for a Value structure to
   describe it. */
            } else {
               value = astMalloc( sizeof( AstChannelValue ) );

/* Invoke astRead to read the Object definition from subsequent data
   items and to build the Object, returning a pointer to it. This will
   result in recursive calls to the current function, but as these
   will use higher nesting levels they will not interfere with the
   current invocation. */
               astreadclassdata_msg = 0;
               object = astRead( this );
               if ( astOK ) {

/* Store pointers to the name and Object in the Value structure and
   note this is an Object value. */
                  value->name = name;
                  value->ptr.object = object;
                  value->is_object = 1;

/* Append the Value structure to the values list for the current
   nesting level. */
                  AppendValue( value, values_list + nest, status );

/* If an error occurred, report a contextual error maessage and set
   the "astreadclassdata_msg" flag (this prevents multiple messages if this function is
   invoked recursively to deal with nested Objects). */
               } else {
                  if ( !astreadclassdata_msg ) {
                     astError( astStatus,
                               "Failed to read the \"%s\" Object value.", status,
                               name );
                     astreadclassdata_msg = 1;
                  }

/* Free the memory holding the "name" string and any Value structure
   that was allocated. */
                  name = astFree( name );
                  value = astFree( value );
               }
            }
         }
      }
   }
}

static double ReadDouble( AstChannel *this, const char *name, double def, int *status ) {
/*
*+
*  Name:
*     astReadDouble

*  Purpose:
*     Read a double value as part of loading a class.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "channel.h"
*     double astReadDouble( AstChannel *this, const char *name, double def )

*  Class Membership:
*     Channel method.

*  Description:
*     This function searches the current values list of a Channel to
*     identify a double value with a specified name. If such a value
*     is found, it is returned, otherwise a default value is returned
*     instead.
*
*     This function should only be invoked from within the loader
*     function associated with a class, in order to return a double
*     value to be assigned to an instance variable. It must be
*     preceded by a call to the astReadClassData function, which loads
*     the values associated with the class into the current values
*     list from the input data source.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated character string
*        containing the name of the required value. This must be in
*        lower case with no surrounding white space. Note that names
*        longer than 6 characters will not match any value.
*     def
*        If no suitable value can be found (e.g. it is absent from the
*        data stream being read), then this value will be returned
*        instead.

*  Returned Value:
*     The required value, or the default if the value was not found.

*  Notes:
*     - A value of 0.0 will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstChannelValue *value;       /* Pointer to required Value structure */
   double result;                /* Value to be returned */
   int nc;                       /* Number of characters read by astSscanf */

/* Initialise. */
   result = 0.0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Search for a Value structure with the required name in the current
   values list.*/
   value = LookupValue( name, status );
   if ( astOK ) {

/* If a Value was found, check that it describes a string (as opposed
   to an Object). */
      if ( value ) {
         if ( !value->is_object ) {

/* If so, then attempt to decode the string to give a double value,
   checking that the entire string is read (and checking for the magic string
   used to represent bad values). If this fails, then the wrong name has
   probably been given, or the input data are corrupt, so report an error. */
            nc = 0;
            if ( ( 0 == astSscanf( value->ptr.string, " " BAD_STRING " %n",
                                                      &nc ) )
                    && ( nc >= (int) strlen( value->ptr.string ) ) ) {
               result = AST__BAD;

            } else if ( !( ( 1 == astSscanf( value->ptr.string, " %lf %n",
                                                      &result, &nc ) )
                    && ( nc >= (int) strlen( value->ptr.string ) ) ) ) {
               astError( AST__BADIN,
                         "astRead(%s): The value \"%s = %s\" cannot "
                         "be read as a double precision floating point "
                         "number.", status, astGetClass( this ),
                         value->name, value->ptr.string );
            }

/* Report a similar error if the Value does not describe a string. */
         } else {
            astError( AST__BADIN,
                      "astRead(%s): The Object \"%s = <%s>\" cannot "
                      "be read as a double precision floating point number.", status,
                      astGetClass( this ),
                      value->name, astGetClass( value->ptr.object ) );
         }

/* Free the Value structure and the resources it points at. */
         value = FreeValue( value, status );

/* If no suitable Value structure was found, then use the default
   value instead. */
      } else {
         result = def;
      }
   }

/* Return the result. */
   return result;
}

static int ReadInt( AstChannel *this, const char *name, int def, int *status ) {
/*
*+
*  Name:
*     astReadInt

*  Purpose:
*     Read an int value as part of loading a class.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "channel.h"
*     int astReadInt( AstChannel *this, const char *name, int def )

*  Class Membership:
*     Channel method.

*  Description:
*     This function searches the current values list of a Channel to
*     identify an int value with a specified name. If such a value is
*     found, it is returned, otherwise a default value is returned
*     instead.
*
*     This function should only be invoked from within the loader
*     function associated with a class, in order to return an int
*     value to be assigned to an instance variable. It must be
*     preceded by a call to the astReadClassData function, which loads
*     the values associated with the class into the current values
*     list from the input data source.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated character string
*        containing the name of the required value. This must be in
*        lower case with no surrounding white space. Note that names
*        longer than 6 characters will not match any value.
*     def
*        If no suitable value can be found (e.g. it is absent from the
*        data stream being read), then this value will be returned
*        instead.

*  Returned Value:
*     The required value, or the default if the value was not found.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstChannelValue *value;       /* Pointer to required Value structure */
   int nc;                       /* Number of characters read by astSscanf */
   int result;                   /* Value to be returned */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Search for a Value structure with the required name in the current
   values list.*/
   value = LookupValue( name, status );
   if ( astOK ) {

/* If a Value was found, check that it describes a string (as opposed
   to an Object). */
      if ( value ) {
         if ( !value->is_object ) {

/* If so, then attempt to decode the string to give an int value,
   checking that the entire string is read. If this fails, then the
   wrong name has probably been given, or the input data are corrupt,
   so report an error. */
            nc = 0;
            if ( !( ( 1 == astSscanf( value->ptr.string, " %d %n",
                                                      &result, &nc ) )
                    && ( nc >= (int) strlen( value->ptr.string ) ) ) ) {
               astError( AST__BADIN,
                         "astRead(%s): The value \"%s = %s\" cannot "
                         "be read as an integer.", status, astGetClass( this ),
                         value->name, value->ptr.string );
            }

/* Report a similar error if the Value does not describe a string. */
         } else {
            astError( AST__BADIN,
                      "astRead(%s): The Object \"%s = <%s>\" cannot "
                      "be read as an integer.", status, astGetClass( this ),
                      value->name, astGetClass( value->ptr.object ) );
         }

/* Free the Value structure and the resources it points at. */
         value = FreeValue( value, status );

/* If no suitable Value structure was found, then use the default
   value instead. */
      } else {
         result = def;
      }
   }

/* Return the result. */
   return result;
}

static AstObject *ReadObject( AstChannel *this, const char *name,
                              AstObject *def, int *status ) {
/*
*+
*  Name:
*     astReadObject

*  Purpose:
*     Read a (sub)Object as part of loading a class.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "channel.h"
*     AstObject *astReadObject( AstChannel *this, const char *name,
*                               AstObject *def )

*  Class Membership:
*     Channel method.

*  Description:
*     This function searches the current values list of a Channel to
*     identify an Object with a specified name. If such an Object is
*     found, a pointer to it is returned, otherwise a default pointer
*     is returned instead.
*
*     This function should only be invoked from within the loader
*     function associated with a class, in order to return an Object
*     pointer value to be assigned to an instance variable. It must be
*     preceded by a call to the astReadClassData function, which loads
*     the values associated with the class into the current values
*     list from the input data source.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated character string
*        containing the name of the required Object. This must be in
*        lower case with no surrounding white space. Note that names
*        longer than 6 characters will not match any Object.
*     def
*        If no suitable Object can be found (e.g. the Object is absent
*        from the data stream being read), then a clone of this
*        default Object pointer will be returned instead (or NULL if
*        this default pointer is NULL).

*  Returned Value:
*     A pointer to the Object, or a clone of the default pointer if
*     the Object was not found.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstObject *result;            /* Pointer value to return */
   AstChannelValue *value;       /* Pointer to required Value structure */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Search for a Value structure with the required name in the current
   values list.*/
   value = LookupValue( name, status );
   if ( astOK ) {

/* If a Value was found, check that it describes an Object (as opposed to
   a string). */
      if ( value ) {
         if ( value->is_object ) {

/* If so, then extract the Object pointer, replacing it with NULL. */
            result = value->ptr.object;
            value->ptr.object = NULL;

/* If the Value does not describe an Object, then the wrong name has
   probably been given, or the input data are corrupt, so report an
   error. */
         } else {
            astError( AST__BADIN,
                      "astRead(%s): The value \"%s = %s\" cannot be "
                      "read as an Object.", status, astGetClass( this ),
                      value->name, value->ptr.string );
         }

/* Free the Value structure and the resources it points at. */
         value = FreeValue( value, status );

/* If no suitable Value structure was found, clone the default
   pointer, if given. */
      } else if ( def ) {
         result = astClone( def );
      }
   }

/* Return the result. */
   return result;
}

static char *ReadString( AstChannel *this, const char *name,
                         const char *def, int *status ) {
/*
*+
*  Name:
*     astReadString

*  Purpose:
*     Read a string value as part of loading a class.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "channel.h"
*     char *astReadString( AstChannel *this, const char *name,
*                          const char *def )

*  Class Membership:
*     Channel method.

*  Description:
*     This function searches the current values list of a Channel to
*     identify a string value with a specified name. If such a value
*     is found, a pointer to the string is returned, otherwise a
*     pointer to a copy of a default string is returned instead.
*
*     This function should only be invoked from within the loader
*     function associated with a class, in order to return a string
*     pointer value to be assigned to an instance variable. It must be
*     preceded by a call to the astReadClassData function, which loads
*     the values associated with the class into the current values
*     list from the input data source.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated character string
*        containing the name of the required value. This must be in
*        lower case with no surrounding white space. Note that names
*        longer than 6 characters will not match any value.
*     def
*        If no suitable string can be found (e.g. the value is absent
*        from the data stream being read), then a dynamically
*        allocated copy of the null-terminated string pointed at by
*        "def" will be made, and a pointer to this copy will be
*        returned instead (or NULL if this default pointer is NULL).

*  Returned Value:
*     A pointer to a dynamically allocated null-terminated string
*     containing the value required, or to a copy of the default
*     string if the value was not found (or NULL if the "def" pointer
*     was NULL).

*  Notes:
*     - It is the caller's responsibility to arrange for the memory
*     holding the returned string to be freed (using astFree) when it
*     is no longer required.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstChannelValue *value;       /* Pointer to required Value structure */
   char *result;                 /* Pointer value to return */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Search for a Value structure with the required name in the current
   values list.*/
   value = LookupValue( name, status );
   if ( astOK ) {

/* If a Value was found, check that it describes a string (as opposed
   to an Object). */
      if ( value ) {
         if ( !value->is_object ) {

/* If so, then extract the string pointer, replacing it with NULL. */
            result = value->ptr.string;
            value->ptr.string = NULL;

/* If the Value does not describe a string, then the wrong name has
   probably been given, or the input data are corrupt, so report an
   error. */
         } else {
            astError( AST__BADIN,
                      "astRead(%s): The Object \"%s = <%s>\" cannot "
                      "be read as a string.", status, astGetClass( this ),
                      value->name, astGetClass( value->ptr.object ) );
         }

/* Free the Value structure and the resources it points at. */
         value = FreeValue( value, status );

/* If no suitable Value structure was found, then make a dynamic copy
   of the default string (if given) and return a pointer to this. */
      } else if ( def ) {
         result = astStore( NULL, def, strlen( def ) + (size_t) 1 );
      }
   }

/* Return the result. */
   return result;
}

static void RemoveValue( AstChannelValue *value, AstChannelValue **head, int *status ) {
/*
*  Name:
*     RemoveValue

*  Purpose:
*     Remove a Value structure from a circular linked list.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     void RemoveValue( AstChannelValue *value, AstChannelValue **head, int *status );

*  Class Membership:
*     Channel member function.

*  Description:
*     This function removes a Value structure from a doubly linked
*     circular list of such structures. The "head of list" pointer is
*     updated to point at the element following the one removed.

*  Parameters:
*     value
*        Pointer to the structure to be removed (note that this must
*        actually be in the list, although this function does not
*        check).
*     head
*        Address of a pointer to the element at the head of the
*        list. This pointer will be updated to point at the list
*        element that follows the one removed. If the list becomes
*        empty, the pointer will be set to NULL.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - This function does not perform error chacking and does not
*     generate errors.
*/

/* Remove the Value structure from the list by re-establishing links
   between the elements on either side of it. */
   value->blink->flink = value->flink;
   value->flink->blink = value->blink;

/* Update the head of list pointer to identify the following
   element. */
   *head = value->flink;

/* If the head of list identifies the removed element, then note that
   the list is now empty. */
   if ( *head == value ) *head = NULL;

/* Make the removed element point at itself. */
   value->flink = value;
   value->blink = value;
}

static void SetAttrib( AstObject *this_object, const char *setting, int *status ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a Channel.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     void SetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     Channel member function (over-rides the astSetAttrib protected
*     method inherited from the Object class).

*  Description:
*     This function assigns an attribute value for a Channel, the
*     attribute and its value being specified by means of a string of
*     the form:
*
*        "attribute= value "
*
*     Here, "attribute" specifies the attribute name and should be in
*     lower case with no white space present. The value to the right
*     of the "=" should be a suitable textual representation of the
*     value to be assigned and this will be interpreted according to
*     the attribute's data type.  White space surrounding the value is
*     only significant for string attributes.

*  Parameters:
*     this
*        Pointer to the Channel.
*     setting
*        Pointer to a null terminated string specifying the new attribute
*        value.
*/

/* Local Variables: */
   AstChannel *this;             /* Pointer to the Channel structure */
   int comment;                  /* Comment attribute value */
   int full;                     /* Full attribute value */
   int indent;                   /* Indent attribute value */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by "astSscanf" */
   int report_level;             /* Skip attribute value */
   int skip;                     /* Skip attribute value */
   int sourcefile;               /* Offset of SourceFile string */
   int sinkfile;                 /* Offset of SinkFile string */
   int strict;                   /* Report errors instead of warnings? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Channel structure. */
   this = (AstChannel *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */

/* Comment. */
/* ---------*/
   if ( nc = 0,
        ( 1 == astSscanf( setting, "comment= %d %n", &comment, &nc ) )
        && ( nc >= len ) ) {
      astSetComment( this, comment );

/* Full. */
/* ----- */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "full= %d %n", &full, &nc ) )
               && ( nc >= len ) ) {
      astSetFull( this, full );

/* Indent. */
/* ------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "indent= %d %n", &indent, &nc ) )
               && ( nc >= len ) ) {
      astSetIndent( this, indent );

/* ReportLavel. */
/* ------------ */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "reportlevel= %d %n", &report_level, &nc ) )
               && ( nc >= len ) ) {
      astSetReportLevel( this, report_level );

/* Skip. */
/* ----- */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "skip= %d %n", &skip, &nc ) )
               && ( nc >= len ) ) {
      astSetSkip( this, skip );

/* SinkFile. */
/* --------- */
   } else if ( nc = 0,
               ( 0 == astSscanf( setting, "sinkfile=%n%*[^\n]%n", &sinkfile, &nc ) )
               && ( nc >= len ) ) {
      astSetSinkFile( this, setting + sinkfile );

/* SourceFile. */
/* ----------- */
   } else if ( nc = 0,
               ( 0 == astSscanf( setting, "sourcefile=%n%*[^\n]%n", &sourcefile, &nc ) )
               && ( nc >= len ) ) {
      astSetSourceFile( this, setting + sourcefile );

/* Strict. */
/* ------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "strict= %d %n", &strict, &nc ) )
               && ( nc >= len ) ) {
      astSetStrict( this, strict );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting, status );
   }
}

static void SinkWrap( void (* sink)( const char * ), const char *line, int *status ) {
/*
*  Name:
*     SinkWrap

*  Purpose:
*     Wrapper function to invoke a C Channel sink function.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     void SinkWrap( void (* sink)( const char * ), const char *line, int *status )

*  Class Membership:
*     Channel member function.

*  Description:
*     This function invokes the sink function whose pointer is
*     supplied in order to write an output line to an external data
*     store.

*  Parameters:
*     sink
*        Pointer to a sink function, whose single parameter is a
*        pointer to a const, null-terminated string containing the
*        text to be written, and which returns void. This is the form
*        of Channel sink function employed by the C language interface
*        to the AST library.
*     status
*        Pointer to the inherited status variable.
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Invoke the sink function. */
   ( *sink )( line );
}

static char *SourceWrap( const char *(* source)( void ), int *status ) {
/*
*  Name:
*     SourceWrap

*  Purpose:
*     Wrapper function to invoke a C Channel source function.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     char *SourceWrap( const char *, int *status(* source)( void ) )

*  Class Membership:
*     Channel member function.

*  Description:
*     This function invokes the source function whose pointer is
*     supplied in order to read the next input line from an external
*     data store. It then returns a pointer to a dynamic string
*     containing a copy of the text that was read.

*  Parameters:
*     source
*        Pointer to a source function, with no parameters, that
*        returns a pointer to a const, null-terminated string
*        containing the text that it read. This is the form of Channel
*        source function employed by the C language interface to the
*        AST library.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated, null terminated string
*     containing a copy of the text that was read. This string must be
*     freed by the caller (using astFree) when no longer required.
*
*     A NULL pointer will be returned if there is no more input text
*     to read.

*  Notes:
*     - A NULL pointer value will be returned if this function is
*     invoked with the global error status set or if it should fail
*     for any reason.
*/

/* Local Variables: */
   char *result;                 /* Pointer value to return */
   const char *line;             /* Pointer to input line */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Invoke the source function to read the next input line and return a
   pointer to the resulting string. */
   line = ( *source )();

/* If a string was obtained, make a dynamic copy of it and save the
   resulting pointer. */
   if ( line ) result = astString( line, (int) strlen( line ) );

/* Return the result. */
   return result;
}

void astStoreChannelData_( AstChannel *this, int *status ) {
/*
*+
*  Name:
*     astStoreChannelData

*  Purpose:
*     Store the Channel's channel-data pointer in a thread-specific
*     global variable.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "channel.h"
*     astStoreChannelData( AstChannel *this )

*  Class Membership:
*     Channel method.

*  Description:
*     This function stores the Channel's channel-data pointer (if any)
*     established by the previous call to astPutChannelData, in a
*     thread-specific global variable from where the astChannelData macro
*     can access it.

*  Parameters:
*     this
*        Pointer to the Channel.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* Store the pointer int he global variable. */
   channel_data = this->data;
}

static int TestAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a Channel.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Channel member function (over-rides the astTestAttrib protected
*     method inherited from the Object class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a Channel's attributes.

*  Parameters:
*     this
*        Pointer to the Channel.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstChannel *this;             /* Pointer to the Channel structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the Channel structure. */
   this = (AstChannel *) this_object;

/* Check the attribute name and test the appropriate attribute. */

/* Comment. */
/* -------- */
   if ( !strcmp( attrib, "comment" ) ) {
      result = astTestComment( this );

/* Full. */
/* ----- */
   } else if ( !strcmp( attrib, "full" ) ) {
      result = astTestFull( this );

/* Indent. */
/* ------- */
   } else if ( !strcmp( attrib, "indent" ) ) {
      result = astTestIndent( this );

/* ReportLevel. */
/* ------------ */
   } else if ( !strcmp( attrib, "reportlevel" ) ) {
      result = astTestReportLevel( this );

/* Skip. */
/* ----- */
   } else if ( !strcmp( attrib, "skip" ) ) {
      result = astTestSkip( this );

/* SourceFile. */
/* ----------- */
   } else if ( !strcmp( attrib, "sourcefile" ) ) {
      result = astTestSourceFile( this );

/* SinkFile. */
/* ----------- */
   } else if ( !strcmp( attrib, "sinkfile" ) ) {
      result = astTestSinkFile( this );

/* Strict. */
/* ------- */
   } else if ( !strcmp( attrib, "strict" ) ) {
      result = astTestStrict( this );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib, status );
   }

/* Return the result, */
   return result;
}

static void Unquote( AstChannel *this, char *str, int *status ) {
/*
*  Name:
*     Unquote

*  Purpose:
*     Remove quotes from a (possibly) quoted string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     void Unquote( AstChannel *this, char *str, int *status )

*  Class Membership:
*     Channel member function.

*  Description:
*     This function removes one layer of quote characters (") from a
*     string which is possibly quoted. Any quotes within quotes (which
*     should have been doubled when the string was originally quoted)
*     are also converted back to single quotes again.
*
*     The quotes need not start or end at the ends of the string, and
*     there may be any number of quoted sections within the string. No
*     error results if the string does not contain any quotes at all
*     (it is simply returned unchanged), but an error results if any
*     unmatched quotes are found.

*  Parameters:
*     this
*        Pointer to a Channel. This is only used for constructing error
*        messages and has no influence on the string processing.
*     str
*        Pointer to the null-terminated string to be processed. This
*        is modified in place. The new string starts at the same
*        location as the original but has a new null character
*        appended if necessary (it will usually be shorter than the
*        original).
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   int i;                        /* Loop counter for "input" characters */
   int j;                        /* Counter for "output" characters */
   int quoted;                   /* Inside a quoted string? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Loop to inspect each character in the string. */
   quoted = 0;
   for ( i = j = 0; str[ i ]; i++ ) {

/* Non-quote characters are simply copied to their new location in the
   string. */
      if ( str[ i ] != '"' ) {
         str[ j++ ] = str[ i ];

/* If a quote character '"' is encountered and we are not already in a
   quoted string, then note the start of a quoted string (and discard
   the quote). */
      } else if ( !quoted ) {
         quoted = 1;

/* If a quote character is encountered inside a quoted string, then
   check if the next character is also a quote. If so, convert this
   double quote to a single one. */
      } else if ( str[ i + 1 ] == '"' ) {
         str[ j++ ] = '"';
         i++;

/* If a single quote character is encountered inside a quoted string,
   then note the end of the quoted string (and discard the quote). */
      } else {
         quoted = 0;
      }
   }

/* Append a null to terminate the processed string. */
   str[ j ] = '\0';

/* If the "quoted" flag is still set, then there were unmatched
   quotes, so report an error. */
   if ( quoted ) {
      astError( AST__UNMQT,
                "astRead(%s): Unmatched quotes in input data: %s.", status,
                astGetClass( this ), str );
   }
}

static int Use( AstChannel *this, int set, int helpful, int *status ) {
/*
*  Name:
*     Use

*  Purpose:
*     Decide whether to write a value to a data sink.

*  Type:
*     Private function.

*  Synopsis:
*     #include "channel.h"
*     int Use( AstChannel *this, int set, int helpful, int *status )

*  Class Membership:
*     Channel member function.

*  Description:
*     This function decides whether a value supplied by a class "Dump"
*     function, via a call to one of the astWrite... protected
*     methods, should actually be written to the data sink associated
*     with a Channel.
*
*     This decision is based on the settings of the "set" and
*     "helpful" flags supplied to the astWrite... method, plus the
*     attribute settings of the Channel.

*  Parameters:
*     this
*        A pointer to the Channel.
*     set
*        The "set" flag supplied.
*     helpful
*        The "helpful" value supplied.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the value should be written out, otherwise zero.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set or if it should fail for any
*     reason.
*/

/* Local Variables: */
   int full;                     /* Full attribute value */
   int result;                   /* Result value to be returned */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* If "set" is non-zero, then so is the result ("set" values must
   always be written out). */
   result = ( set != 0 );

/* Otherwise, obtain the value of the Channel's Full attribute. */
   if ( !set ) {
      full = astGetFull( this );

/* If Full is positive, display all values, if zero, display only
   "helpful" values, if negative, display no (un-"set") values. */
      if ( astOK ) result = ( ( helpful && ( full > -1 ) ) || ( full > 0 ) );
   }

/* Return the result. */
   return result;
}

static int Write( AstChannel *this, AstObject *object, int *status ) {
/*
*++
*  Name:
c     astWrite
f     AST_WRITE

*  Purpose:
*     Write an Object to a Channel.

*  Type:
*     Public function.

*  Synopsis:
c     #include "channel.h"
c     int astWrite( AstChannel *this, AstObject *object )
f     RESULT = AST_WRITE( THIS, OBJECT, STATUS )

*  Class Membership:
*     Channel method.

*  Description:
*     This function writes an Object to a Channel, appending it to any
*     previous Objects written to that Channel.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Channel.
c     object
f     OBJECT = INTEGER (Given)
*        Pointer to the Object which is to be written.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astWrite()
f     AST_WRITE = INTEGER
*        The number of Objects written to the Channel by this
c        invocation of astWrite (normally, this will be one).
f        invocation of AST_WRITE (normally, this will be one).

*  Applicability:
*     FitsChan
*        If the FitsChan uses a foreign encoding (e.g. FITS-WCS) rather
*        than the native AST encoding, then storing values in the
*        FitsChan for keywords NAXIS1, NAXIS2, etc., before invoking
c        astWrite
f        AST_WRITE
*        can help to produce a successful write.

*  Notes:
*     - A value of zero will be returned if this function is invoked
c     with the AST error status set, or if it should fail for any
f     with STATUS set to an error value, or if it should fail for any
*     reason.
*     - Invoking this function will usually cause the sink function
*     associated with the channel to be called in order to transfer a
*     textual description of the supplied object to some external data
*     store. However, the FitsChan class behaves differently. Invoking
*     this function on a FitsChan causes new FITS header cards to be
*     added to an internal buffer (the sink function is not invoked).
*     This buffer is written out through the sink function only when the
*     FitsChan is deleted.
*--
*/

/* Check the global error status. */
   if ( !astOK ) return 0;

/* The work of this function is actually performed by the protected
   astDump method of the Object. The fact that this is further
   encapsulated within the astWrite method (which belongs to the
   Channel) is simply a trick to allow it to be over-ridden either by
   a derived Channel, or a derived Object (or both), and hence to
   adapt to the nature of either argument. */
   astDump( object, this );

/* Return the number of Objects written. */
   return astOK ? 1 : 0;
}

static void WriteBegin( AstChannel *this, const char *class,
                        const char *comment, int *status ) {
/*
*+
*  Name:
*     astWriteBegin

*  Purpose:
*     Write a "Begin" data item to a data sink.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "channel.h"
*     void astWriteBegin( AstChannel *this, const char *class,
*                         const char *comment )

*  Class Membership:
*     Channel method.

*  Description:
*     This function writes a "Begin" data item to the data sink
*     associated with a Channel, so as to begin the output of a new
*     Object definition.

*  Parameters:
*     this
*        Pointer to the Channel.
*     class
*        Pointer to a constant null-terminated string containing the
*        name of the class to which the Object belongs.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the "Begin"
*        item. Normally, this will describe the purpose of the Object.

*  Notes:
*     - The comment supplied may not actually be used, depending on
*     the nature of the Channel supplied.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   char *line;                   /* Pointer to dynamic output string */
   int i;                        /* Loop counter for indentation characters */
   int nc;                       /* Number of output characters */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* Start building a dynamic string with an initial space. Then add
   further spaces to suit the current indentation level. */
   line = astAppendString( NULL, &nc, " " );
   for ( i = 0; i < current_indent; i++ ) {
      line = astAppendString( line, &nc, " " );
   }

/* Append the "Begin" keyword followed by the class name. */
   line = astAppendString( line, &nc, "Begin " );
   line = astAppendString( line, &nc, class );

/* If required, also append the comment. */
   if ( astGetComment( this ) && *comment ) {
      line = astAppendString( line, &nc, " \t# " );
      line = astAppendString( line, &nc, comment );
   }

/* Write out the resulting line of text. */
   OutputTextItem( this, line, status );

/* Free the dynamic string. */
   line = astFree( line );

/* Increment the indentation level and clear the count of items written
   for this Object. */
   current_indent += astGetIndent( this );
   items_written = 0;
}

static void WriteDouble( AstChannel *this, const char *name,
                         int set, int helpful,
                         double value, const char *comment, int *status ) {
/*
*+
*  Name:
*     astWriteDouble

*  Purpose:
*     Write a double value to a data sink.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "channel.h"
*     void astWriteDouble( AstChannel *this, const char *name,
*                          int set, int helpful,
*                          double value, const char *comment )

*  Class Membership:
*     Channel method.

*  Description:
*     This function writes a named double value, representing the
*     value of a class instance variable, to the data sink associated
*     with a Channel. It is intended for use by class "Dump" functions
*     when writing out class information which will subsequently be
*     re-read.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated string containing the
*        name to be used to identify the value in the external
*        representation. This will form the key for identifying it
*        again when it is re-read. The name supplied should be unique
*        within its class.
*
*        Mixed case may be used and will be preserved in the external
*        representation (where possible) for cosmetic effect. However,
*        case is not significant when re-reading values.
*
*        It is recommended that a maximum of 6 alphanumeric characters
*        (starting with an alphabetic character) be used. This permits
*        maximum flexibility in adapting to standard external data
*        representations (e.g. FITS).
*     set
*        If this is zero, it indicates that the value being written is
*        a default value (or can be re-generated from other values) so
*        need not necessarily be written out. Such values will
*        typically be included in the external representation with
*        (e.g.) a comment character so that they are available to
*        human readers but will be ignored when re-read. They may also
*        be completely omitted in some circumstances.
*
*        If "set" is non-zero, the value will always be explicitly
*        included in the external representation so that it can be
*        re-read.
*     helpful
*        This flag provides a hint about whether a value whose "set"
*        flag is zero (above) should actually appear at all in the
*        external representaton.
*
*        If the external representation allows values to be "commented
*        out" then, by default, values will be included in this form
*        only if their "helpful" flag is non-zero. Otherwise, they
*        will be omitted entirely. When possible, omitting the more
*        obscure values associated with a class is recommended in
*        order to improve readability.
*
*        This default behaviour may be further modified if the
*        Channel's Full attribute is set - either to permit all values
*        to be shown, or to suppress non-essential information
*        entirely.
*     value
*        The value to be written.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the value.
*
*        Note that this comment may not actually be used, depending on
*        the nature of the Channel supplied and the setting of its
*        Comment attribute.
*-
*/

/* Local Constants: */
#define BUFF_LEN 100             /* Size of local formatting buffer */

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   char *line;                   /* Pointer to dynamic output string */
   char buff[ BUFF_LEN + 1 ];    /* Local formatting buffer */
   int i;                        /* Loop counter for indentation characters */
   int nc;                       /* Number of output characters */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* Use the "set" and "helpful" flags, along with the Channel's
   attributes to decide whether this value should actually be
   written. */
   if ( Use( this, set, helpful, status ) ) {

/* Start building a dynamic string with an initial space, or a comment
   character if "set" is zero. Then add further spaces to suit the
   current indentation level. */
      line = astAppendString( NULL, &nc, set ? " " : "#" );
      for ( i = 0; i < current_indent; i++ ) {
         line = astAppendString( line, &nc, " " );
      }

/* Append the name string followed by " = ". */
      line = astAppendString( line, &nc, name );
      line = astAppendString( line, &nc, " = " );

/* Format the value as a string and append this. Make sure "-0" isn't
   produced. Use a magic string to represent bad values. */
      if( value != AST__BAD ) {
         (void) sprintf( buff, "%.*g", DBL_DIG, value );
         if ( !strcmp( buff, "-0" ) ) {
            buff[ 0 ] = '0';
            buff[ 1 ] = '\0';
         }
      } else {
         strcpy( buff, BAD_STRING );
      }
      line = astAppendString( line, &nc, buff );

/* If required, also append the comment. */
      if ( astGetComment( this ) && *comment ) {
         line = astAppendString( line, &nc, " \t# " );
         line = astAppendString( line, &nc, comment );
      }

/* Write out the resulting line of text. */
      OutputTextItem( this, line, status );

/* Free the dynamic string. */
      line = astFree( line );
   }

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

static void WriteEnd( AstChannel *this, const char *class, int *status ) {
/*
*+
*  Name:
*     astWriteEnd

*  Purpose:
*     Write an "End" data item to a data sink.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "channel.h"
*     void astWriteEnd( AstChannel *this, const char *class )

*  Class Membership:
*     Channel method.

*  Description:
*     This function writes an "End" data item to the data sink
*     associated with a Channel. This item delimits the end of an
*     Object definition.

*  Parameters:
*     this
*        Pointer to the Channel.
*     class
*        Pointer to a constant null-terminated string containing the
*        class name of the Object whose definition is being terminated
*        by the "End" item.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   char *line;                   /* Pointer to dynamic output string */
   int i;                        /* Loop counter for indentation characters */
   int nc;                       /* Number of output characters */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* Decrement the indentation level so that the "End" item matches the
   corresponding "Begin" item. */
   current_indent -= astGetIndent( this );

/* Start building a dynamic string with an initial space. Then add
   further spaces to suit the current indentation level. */
   line = astAppendString( NULL, &nc, " " );
   for ( i = 0; i < current_indent; i++ ) {
      line = astAppendString( line, &nc, " " );
   }

/* Append the "End" keyword followed by the class name. */
   line = astAppendString( line, &nc, "End " );
   line = astAppendString( line, &nc, class );

/* Write out the resulting line of text. */
   OutputTextItem( this, line, status );

/* Free the dynamic string. */
   line = astFree( line );
}

static void WriteInt( AstChannel *this, const char *name, int set, int helpful,
                      int value, const char *comment, int *status ) {
/*
*+
*  Name:
*     astWriteInt

*  Purpose:
*     Write an integer value to a data sink.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "channel.h"
*     void astWriteInt( AstChannel *this, const char *name,
*                       int set, int helpful,
*                       int value, const char *comment )

*  Class Membership:
*     Channel method.

*  Description:
*     This function writes a named integer value, representing the
*     value of a class instance variable, to the data sink associated
*     with a Channel. It is intended for use by class "Dump" functions
*     when writing out class information which will subsequently be
*     re-read.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated string containing the
*        name to be used to identify the value in the external
*        representation. This will form the key for identifying it
*        again when it is re-read. The name supplied should be unique
*        within its class.
*
*        Mixed case may be used and will be preserved in the external
*        representation (where possible) for cosmetic effect. However,
*        case is not significant when re-reading values.
*
*        It is recommended that a maximum of 6 alphanumeric characters
*        (starting with an alphabetic character) be used. This permits
*        maximum flexibility in adapting to standard external data
*        representations (e.g. FITS).
*     set
*        If this is zero, it indicates that the value being written is
*        a default value (or can be re-generated from other values) so
*        need not necessarily be written out. Such values will
*        typically be included in the external representation with
*        (e.g.) a comment character so that they are available to
*        human readers but will be ignored when re-read. They may also
*        be completely omitted in some circumstances.
*
*        If "set" is non-zero, the value will always be explicitly
*        included in the external representation so that it can be
*        re-read.
*     helpful
*        This flag provides a hint about whether a value whose "set"
*        flag is zero (above) should actually appear at all in the
*        external representaton.
*
*        If the external representation allows values to be "commented
*        out" then, by default, values will be included in this form
*        only if their "helpful" flag is non-zero. Otherwise, they
*        will be omitted entirely. When possible, omitting the more
*        obscure values associated with a class is recommended in
*        order to improve readability.
*
*        This default behaviour may be further modified if the
*        Channel's Full attribute is set - either to permit all values
*        to be shown, or to suppress non-essential information
*        entirely.
*     value
*        The value to be written.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the value.
*
*        Note that this comment may not actually be used, depending on
*        the nature of the Channel supplied and the setting of its
*        Comment attribute.
*-
*/

/* Local Constants: */
#define BUFF_LEN 50              /* Size of local formatting buffer */

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   char *line;                   /* Pointer to dynamic output string */
   char buff[ BUFF_LEN + 1 ];    /* Local formatting buffer */
   int i;                        /* Loop counter for indentation characters */
   int nc;                       /* Number of output characters */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* Use the "set" and "helpful" flags, along with the Channel's
   attributes to decide whether this value should actually be
   written. */
   if ( Use( this, set, helpful, status ) ) {

/* Start building a dynamic string with an initial space, or a comment
   character if "set" is zero. Then add further spaces to suit the
   current indentation level. */
      line = astAppendString( NULL, &nc, set ? " " : "#" );
      for ( i = 0; i < current_indent; i++ ) {
         line = astAppendString( line, &nc, " " );
      }

/* Append the name string followed by " = ". */
      line = astAppendString( line, &nc, name );
      line = astAppendString( line, &nc, " = " );

/* Format the value as a decimal string and append this. */
      (void) sprintf( buff, "%d", value );
      line = astAppendString( line, &nc, buff );

/* If required, also append the comment. */
      if ( astGetComment( this ) && *comment ) {
         line = astAppendString( line, &nc, " \t# " );
         line = astAppendString( line, &nc, comment );
      }

/* Write out the resulting line of text. */
      OutputTextItem( this, line, status );

/* Free the dynamic string. */
      line = astFree( line );
   }

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

int astWriteInvocations_( int *status ){
/*
*+
*  Name:
*     astWriteInvocations

*  Purpose:
*     Returns the number of invocations of the astWrite method.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "channel.h"
*     int astWriteInvocations

*  Class Membership:
*     Channel method.

*  Description:
*     This function returns the number of invocations of astWrite which
*     have been made so far, excluding those made from within the
*     astWriteObject method. An example of its use is to allow a Dump
*     function to determine if a sub-object has already been dumped
*     during the current invocation of astWrite. See the Dump method for
*     the AstUnit class as an example.
*-
*/
   astDECLARE_GLOBALS;
   astGET_GLOBALS(NULL);
   return nwrite_invoc;
}

static void WriteIsA( AstChannel *this, const char *class,
                      const char *comment, int *status ) {
/*
*+
*  Name:
*     astWriteIsA

*  Purpose:
*     Write an "IsA" data item to a data sink.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "channel.h"
*     void astWriteIsA( AstChannel *this, const char *class,
*                       const char *comment )

*  Class Membership:
*     Channel method.

*  Description:
*     This function writes an "IsA" data item to the data sink
*     associated with a Channel. This item delimits the end of the
*     data associated with the instance variables of a class, as part
*     of an overall Object definition.

*  Parameters:
*     this
*        Pointer to the Channel.
*     class
*        Pointer to a constant null-terminated string containing the
*        name of the class whose data are terminated by the "IsA"
*        item.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the "IsA"
*        item. Normally, this will describe the purpose of the class
*        whose data are being terminated.

*  Notes:
*     - The comment supplied may not actually be used, depending on
*     the nature of the Channel supplied.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   char *line;                   /* Pointer to dynamic output string */
   int i;                        /* Loop counter for indentation characters */
   int indent_inc;               /* Indentation increment */
   int nc;                       /* Number of output characters */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* Output an "IsA" item only if there has been at least one item
   written since the last "Begin" or "IsA" item, or if the Full
   attribute for the Channel is greater than zero (requesting maximum
   information). */
   if ( items_written || astGetFull( this ) > 0 ) {

/* Start building a dynamic string with an initial space. Then add
   further spaces to suit the current indentation level, but reduced
   by one to allow the "IsA" item to match the "Begin" and "End" items
   which enclose it. */
      indent_inc = astGetIndent( this );
      line = astAppendString( NULL, &nc, " " );
      for ( i = 0; i < ( current_indent - indent_inc ); i++ ) {
         line = astAppendString( line, &nc, " " );
      }

/* Append the "IsA" keyword followed by the class name. */
      line = astAppendString( line, &nc, "IsA " );
      line = astAppendString( line, &nc, class );

/* If required, also append the comment. */
      if ( astGetComment( this ) && *comment ) {
         line = astAppendString( line, &nc, " \t# " );
         line = astAppendString( line, &nc, comment );
      }

/* Write out the resulting line of text. */
      OutputTextItem( this, line, status );

/* Free the dynamic string. */
      line = astFree( line );

/* Clear the count of items written for this class. */
      items_written = 0;
   }
}

static void WriteObject( AstChannel *this, const char *name,
                         int set, int helpful,
                         AstObject *value, const char *comment, int *status ) {
/*
*+
*  Name:
*     astWriteObject

*  Purpose:
*     Write an Object as a value to a data sink.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "channel.h"
*     void astWriteObject( AstChannel *this, const char *name,
*                          int set, int helpful,
*                          AstObject *value, const char *comment )

*  Class Membership:
*     Channel method.

*  Description:
*     This function writes an Object as a named value, representing
*     the value of a class instance variable, to the data sink
*     associated with a Channel. It is intended for use by class
*     "Dump" functions when writing out class information which will
*     subsequently be re-read.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated string containing the
*        name to be used to identify the value in the external
*        representation. This will form the key for identifying it
*        again when it is re-read. The name supplied should be unique
*        within its class.
*
*        Mixed case may be used and will be preserved in the external
*        representation (where possible) for cosmetic effect. However,
*        case is not significant when re-reading values.
*
*        It is recommended that a maximum of 6 alphanumeric characters
*        (starting with an alphabetic character) be used. This permits
*        maximum flexibility in adapting to standard external data
*        representations (e.g. FITS).
*     set
*        If this is zero, it indicates that the value being written is
*        a default value (or can be re-generated from other values) so
*        need not necessarily be written out. Such values will
*        typically be included in the external representation with
*        (e.g.) a comment character so that they are available to
*        human readers but will be ignored when re-read. They may also
*        be completely omitted in some circumstances.
*
*        If "set" is non-zero, the value will always be explicitly
*        included in the external representation so that it can be
*        re-read.
*     helpful
*        This flag provides a hint about whether a value whose "set"
*        flag is zero (above) should actually appear at all in the
*        external representaton.
*
*        If the external representation allows values to be "commented
*        out" then, by default, values will be included in this form
*        only if their "helpful" flag is non-zero. Otherwise, they
*        will be omitted entirely. When possible, omitting the more
*        obscure values associated with a class is recommended in
*        order to improve readability.
*
*        This default behaviour may be further modified if the
*        Channel's Full attribute is set - either to permit all values
*        to be shown, or to suppress non-essential information
*        entirely.
*     value
*        A Pointer to the Object to be written.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the value.
*
*        Note that this comment may not actually be used, depending on
*        the nature of the Channel supplied and the setting of its
*        Comment attribute.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   char *line;                   /* Pointer to dynamic output string */
   int i;                        /* Loop counter for indentation characters */
   int indent_inc;               /* Indentation increment */
   int nc;                       /* Number of output characters */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* Use the "set" and "helpful" flags, along with the Channel's
   attributes to decide whether this value should actually be
   written. */
   if ( Use( this, set, helpful, status ) ) {

/* Start building a dynamic string with an initial space, or a comment
   character if "set" is zero. Then add further spaces to suit the
   current indentation level. */
      line = astAppendString( NULL, &nc, set ? " " : "#" );
      for ( i = 0; i < current_indent; i++ ) {
         line = astAppendString( line, &nc, " " );
      }

/* Append the name string followed by " =". The absence of a value on
   the right hand side indicates an Object value, whose definition
   follows. */
      line = astAppendString( line, &nc, name );
      line = astAppendString( line, &nc, " =" );

/* If required, also append the comment. */
      if ( astGetComment( this ) && *comment ) {
         line = astAppendString( line, &nc, " \t# " );
         line = astAppendString( line, &nc, comment );
      }

/* Write out the resulting line of text. */
      OutputTextItem( this, line, status );

/* Free the dynamic string. */
      line = astFree( line );

/* If the value is not a default, write the Object to the Channel as
   well, suitably indented (this is omitted if the value is commented
   out). */
      if ( set ) {
         indent_inc = astGetIndent( this );
         current_indent += indent_inc;
         (void) astWrite( this, value );
         current_indent -= indent_inc;
      }
   }
}

static void WriteString( AstChannel *this, const char *name,
                         int set, int helpful,
                         const char *value, const char *comment, int *status ) {
/*
*+
*  Name:
*     astWriteString

*  Purpose:
*     Write a string value to a data sink.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "channel.h"
*     void astWriteString( AstChannel *this, const char *name,
*                          int set, int helpful,
*                          const char *value, const char *comment )

*  Class Membership:
*     Channel method.

*  Description:
*     This function writes a named string value, representing the
*     value of a class instance variable, to the data sink associated
*     with a Channel. It is intended for use by class "Dump" functions
*     when writing out class information which will subsequently be
*     re-read.

*  Parameters:
*     this
*        Pointer to the Channel.
*     name
*        Pointer to a constant null-terminated string containing the
*        name to be used to identify the value in the external
*        representation. This will form the key for identifying it
*        again when it is re-read. The name supplied should be unique
*        within its class.
*
*        Mixed case may be used and will be preserved in the external
*        representation (where possible) for cosmetic effect. However,
*        case is not significant when re-reading values.
*
*        It is recommended that a maximum of 6 alphanumeric characters
*        (starting with an alphabetic character) be used. This permits
*        maximum flexibility in adapting to standard external data
*        representations (e.g. FITS).
*     set
*        If this is zero, it indicates that the value being written is
*        a default value (or can be re-generated from other values) so
*        need not necessarily be written out. Such values will
*        typically be included in the external representation with
*        (e.g.) a comment character so that they are available to
*        human readers but will be ignored when re-read. They may also
*        be completely omitted in some circumstances.
*
*        If "set" is non-zero, the value will always be explicitly
*        included in the external representation so that it can be
*        re-read.
*     helpful
*        This flag provides a hint about whether a value whose "set"
*        flag is zero (above) should actually appear at all in the
*        external representaton.
*
*        If the external representation allows values to be "commented
*        out" then, by default, values will be included in this form
*        only if their "helpful" flag is non-zero. Otherwise, they
*        will be omitted entirely. When possible, omitting the more
*        obscure values associated with a class is recommended in
*        order to improve readability.
*
*        This default behaviour may be further modified if the
*        Channel's Full attribute is set - either to permit all values
*        to be shown, or to suppress non-essential information
*        entirely.
*     value
*        Pointer to a constant null-terminated string containing the
*        value to be written.
*     comment
*        Pointer to a constant null-terminated string containing a
*        textual comment to be associated with the value.
*
*        Note that this comment may not actually be used, depending on
*        the nature of the Channel supplied and the setting of its
*        Comment attribute.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   char *line;                   /* Pointer to dynamic output string */
   int i;                        /* Loop counter for characters */
   int nc;                       /* Number of output characters */
   int quote;                    /* Quote character found? */
   int size;                     /* Size of allocated memory */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* Use the "set" and "helpful" flags, along with the Channel's
   attributes to decide whether this value should actually be
   written. */
   if ( Use( this, set, helpful, status ) ) {

/* Start building a dynamic string with an initial space, or a comment
   character if "set" is zero. Then add further spaces to suit the
   current indentation level. */
      line = astAppendString( NULL, &nc, set ? " " : "#" );
      for ( i = 0; i < current_indent; i++ ) {
         line = astAppendString( line, &nc, " " );
      }

/* Append the name string followed by " = " and an opening quote
   character (the string will be quoted to protect leading and
   trailing spaces). */
      line = astAppendString( line, &nc, name );
      line = astAppendString( line, &nc, " = \"" );

/* We now append the value string, but must inspect each character so
   that quotes (appearing inside quotes) can be doubled. Determine the
   current size of memory allocated for the dynamic string. */
      size = (int) astSizeOf( line );

/* Loop to inspect each character and see if it is a quote. */
      for ( i = 0; value[ i ]; i++ ) {
         quote = ( value[ i ] == '"' );

/* If more memory is required, extend the dynamic string (allowing for
   doubling of quotes and the final null) and save its new size. */
         if ( nc + 2 + quote > size ) {
            line = astGrow( line, nc + 2 + quote, sizeof( char ) );
            if ( astOK ) {
               size = (int) astSizeOf( line );

/* Quit if an error occurs. */
            } else {
               break;
            }
         }

/* Append the value character to the dynamic string, duplicating each
   quote character. */
         line[ nc++ ] = value[ i ];
         if ( quote ) line[ nc++ ] = '"';
      }

/* Append the closing quote. */
      line = astAppendString( line, &nc, "\"" );

/* If required, also append the comment. */
      if ( astGetComment( this ) && *comment ) {
         line = astAppendString( line, &nc, " \t# " );
         line = astAppendString( line, &nc, comment );
      }

/* Write out the resulting line of text. */
      OutputTextItem( this, line, status );

/* Free the dynamic string. */
      line = astFree( line );
   }
}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   this class using the macros defined for this purpose in the
   "object.h" file. */

/*
*att++
*  Name:
*     SourceFile

*  Purpose:
*     Input file from which to read data.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute specifies the name of a file from which the Channel
*     should read data. If specified it is used in preference to any source
*     function specified when the Channel was created.
*
*     Assigning a new value to this attribute will cause any previously
*     opened SourceFile to be closed. The first subsequent call to
c     astRead
f     AST_READ
*     will attempt to open the new file (an error will be reported if the
*     file cannot be opened), and read data from it. All subsequent call to
c     astRead
f     AST_READ
*     will read data from the new file, until the SourceFile attribute is
*     cleared or changed.
*
*     Clearing the attribute causes any open SourceFile to be closed. All
*     subsequent data reads will use the source function specified when the
*     Channel was created, or will read from standard input if no source
*     function was specified.
*
*     If no value has been assigned to SourceFile, a null string will be
*     returned if an attempt is made to get the attribute value.

*  Notes:
*     - Any open SourceFile is closed when the Channel is deleted.
*     - If the Channel is copied or dumped
c     (using astCopy or astDump)
f     (using AST_COPY or AST_DUMP)
*     the SourceFile attribute is left in a cleared state in the output
*     Channel (i.e. the value of the SourceFile attribute is not copied).

*  Applicability:
*     FitsChan
*        In the case of a FitsChan, the specified SourceFile supplements
*        the source function specified when the FitsChan was created,
*        rather than replacing the source function. The source file
*        should be a text file (not a FITS file) containing one header per
*        line. When a value is assigned to SourceFile, the file is opened
*        and read immediately, and all headers read from the file are
*        appended to the end of any header already in the FitsChan. The file
*        is then closed. Clearing the SourceFile attribute has no further
*        effect, other than nullifying the string (i.e. the file name)
*        associated with the attribute.

*att--
*/

/* Clear the SourceFile value by closing any open file, freeing the
   allocated memory and assigning a NULL pointer. */
astMAKE_CLEAR(Channel,SourceFile,fn_in,((this->fd_in=(this->fd_in?(fclose(this->fd_in),NULL):NULL)),astFree(this->fn_in)))

/* If the SourceFile value is not set, supply a default in the form of a
   pointer to the constant string "". */
astMAKE_GET(Channel,SourceFile,const char *,NULL,( this->fn_in ? this->fn_in : "" ))

/* Set a SourceFile value by closing any open file, freeing any previously
   allocated memory, allocating new memory, storing the string and saving
   the pointer to the copy. */
astMAKE_SET(Channel,SourceFile,const char *,fn_in,((this->fd_in=(this->fd_in?(fclose(this->fd_in),NULL):NULL)),astStore( this->fn_in, value, strlen( value ) + (size_t) 1 )))

/* The SourceFile value is set if the pointer to it is not NULL. */
astMAKE_TEST(Channel,SourceFile,( this->fn_in != NULL ))

/*
*att++
*  Name:
*     SinkFile

*  Purpose:
*     Output file to which to data should be written.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute specifies the name of a file to which the Channel
*     should write data. If specified it is used in preference to any sink
*     function specified when the Channel was created.
*
*     Assigning a new value to this attribute will cause any previously
*     opened SinkFile to be closed. The first subsequent call to
c     astWrite
f     AST_WRITE
*     will attempt to open the new file (an error will be reported if the
*     file cannot be opened), and write data to it. All subsequent call to
c     astWrite
f     AST_WRITE
*     will write data to the new file, until the SinkFile attribute is
*     cleared or changed.
*
*     Clearing the attribute causes any open SinkFile to be closed. All
*     subsequent data writes will use the sink function specified when the
*     Channel was created, or will write to standard output if no sink
*     function was specified.
*
*     If no value has been assigned to SinkFile, a null string will be
*     returned if an attempt is made to get the attribute value.

*  Notes:
*     - A new SinkFile will over-write any existing file with the same
*     name unless the existing file is write protected, in which case an
*     error will be reported.
*     - Any open SinkFile is closed when the Channel is deleted.
*     - If the Channel is copied or dumped
c     (using astCopy or astDump)
f     (using AST_COPY or AST_DUMP)
*     the SinkFile attribute is left in a cleared state in the output
*     Channel (i.e. the value of the SinkFile attribute is not copied).

*  Applicability:
*     FitsChan
*        When the FitsChan is destroyed, any headers in the FitsChan will be
*        written out to the sink file, if one is specified (if not, the
*        sink function used when the FitsChan was created is used). The
*        sink file is a text file (not a FITS file) containing one header
*        per line.

*att--
*/

/* Clear the SinkFile value by closing any open file, freeing the allocated
   memory and assigning a NULL pointer. */
astMAKE_CLEAR(Channel,SinkFile,fn_out,((this->fd_out=(this->fd_out?(fclose(this->fd_out),NULL):NULL)),astFree(this->fn_out)))

/* If the SinkFile value is not set, supply a default in the form of a
   pointer to the constant string "". */
astMAKE_GET(Channel,SinkFile,const char *,NULL,( this->fn_out ? this->fn_out : "" ))

/* Set a SinkFile value by closing any open file, freeing any previously
   allocated memory, allocating new memory, storing the string and saving
   the pointer to the copy. */
astMAKE_SET(Channel,SinkFile,const char *,fn_out,((this->fd_out=(this->fd_out?(fclose(this->fd_out),NULL):NULL)),astStore( this->fn_out, value, strlen( value ) + (size_t) 1 )))

/* The SinkFile value is set if the pointer to it is not NULL. */
astMAKE_TEST(Channel,SinkFile,( this->fn_out != NULL ))


/*
*att++
*  Name:
*     Comment

*  Purpose:
*     Include textual comments in output?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This is a boolean attribute which controls whether textual
*     comments are to be included in the output generated by a
*     Channel. If included, they will describe what each item of
*     output represents.
*
*     If Comment is non-zero, then comments will be included. If
*     it is zero, comments will be omitted.

*  Applicability:
*     Channel
*        The default value is non-zero for a normal Channel.
*     FitsChan
*        The default value is non-zero for a FitsChan.
*     XmlChan
*        The default value is zero for an XmlChan.

*att--
*/

/* This is a boolean value (0 or 1) with a value of -INT_MAX when
   undefined but yielding a default of one. */
astMAKE_CLEAR(Channel,Comment,comment,-INT_MAX)
astMAKE_GET(Channel,Comment,int,0,( this->comment != -INT_MAX ? this->comment : 1 ))
astMAKE_SET(Channel,Comment,int,comment,( value != 0 ))
astMAKE_TEST(Channel,Comment,( this->comment != -INT_MAX ))

/*
*att++
*  Name:
*     Full

*  Purpose:
*     Set level of output detail.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer.

*  Description:
*     This attribute is a three-state flag and takes values of -1, 0
*     or +1.  It controls the amount of information included in the
*     output generated by a Channel.
*
*     If Full is zero, then a modest amount of
*     non-essential but useful information will be included in the
*     output. If Full is negative, all non-essential information will
*     be suppressed to minimise the amount of output, while if it is
*     positive, the output will include the maximum amount of detailed
*     information about the Object being written.

*  Applicability:
*     Channel
*        The default value is zero for a normal Channel.
*     FitsChan
*        The default value is zero for a FitsChan.
*     XmlChan
*        The default value is -1 for an XmlChan.
*     StcsChan
*        The default value is zero for an StcsChan. Set a positive value
*        to cause default values to be included in STC-S descriptions.

*  Notes:
*     - All positive values supplied for this attribute are converted
*     to +1 and all negative values are converted to -1.
*att--
*/

/* This ia a 3-state value (-1, 0 or +1) with a value of -INT_MAX when
   undefined but yielding a default of zero. */
astMAKE_CLEAR(Channel,Full,full,-INT_MAX)
astMAKE_GET(Channel,Full,int,0,( this->full != -INT_MAX ? this->full : 0 ))
astMAKE_SET(Channel,Full,int,full,( value > 0 ? 1 : ( value < 0 ? -1 : 0 ) ))
astMAKE_TEST(Channel,Full,( this->full != -INT_MAX ))

/*
*att++
*  Name:
*     Indent

*  Purpose:
*     Specifies the indentation to use in text produced by a Channel.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This attribute controls the indentation within the output text produced by
f     the AST_WRITE function.
c     the astWrite function.
*     It gives the increase in the indentation for each level in the object
*     heirarchy. If it is set to zero, no indentation will be used. [3]

*  Applicability:
*     Channel
*        The default value is zero for a basic Channel.
*     FitsChan
*        The FitsChan class ignores this attribute.
*     StcsChan
*        The default value for an StcsChan is zero, which causes the entire
*        STC-S description is written out by a single invocation of the sink
*        function. The text supplied to the sink function will not contain
*        any linefeed characters, and each pair of adjacent words will be
*        separated by a single space. The text may thus be arbitrarily large
*        and the StcsLength attribute is ignored.
*
*        If Indent is non-zero, then the text is written out via multiple
*        calls to the sink function, each call corresponding to a single
*        "line" of text (although no line feed characters will be inserted
*        by AST). The complete STC-S description is broken into lines so that:
*
*        - the line length specified by attribute StcsLength is not exceeded
*        - each sub-phrase (time, space, etc.) starts on a new line
*        - each argument in a compound spatial region starts on a new line
*
*        If this causes a sub-phrase to extend to two or more lines, then the
*        second and subsequent lines will be indented by three spaces compared
*        to the first line. In addition, lines within a compound spatial region
*        will have extra indentation to highlight the nesting produced by the
*        parentheses. Each new level of nesting will be indented by a further
*        three spaces.
f
f        Note, the default value of zero is unlikely to be appropriate when
f        an StcsChan is used within Fortran code. In this case, Indent
f        should usually be set non-zero, and the StcsLength attribute set to
f        the size of the CHARACTER variable used to
f        receive the text returned by AST_GETLINE within the sink function.
f        This avoids the possibility of long lines being truncated invisibly
f        within AST_GETLINE.
*     XmlChan
*        The default value for an XmlChan is zero, which results in no
*        linefeeds or indentation strings being added to output text.
*        If any non-zero value is assigned to Indent, then extra linefeed and
*        space characters will be inserted as necessary to ensure that each
*        XML tag starts on a new line, and each tag will be indented by
*        a further 3 spaces to show its depth in the containment hierarchy.
*att--
*/

/* This is an integer value with a value of -INT_MAX when undefined,
   yielding a default of 3. Sub-classes may over-ride theis default. */
astMAKE_CLEAR(Channel,Indent,indent,-INT_MAX)
astMAKE_GET(Channel,Indent,int,3,( this->indent != -INT_MAX ? this->indent : 3 ))
astMAKE_SET(Channel,Indent,int,indent,value)
astMAKE_TEST(Channel,Indent,( this->indent != -INT_MAX ))

/*
*att++
*  Name:
*     ReportLevel

*  Purpose:
*     Determines which read/write conditions are reported.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This attribute determines which, if any, of the conditions that occur
*     whilst reading or writing an Object should be reported. These
*     conditions will generate either a fatal error or a warning, as
*     determined by attribute Strict. ReportLevel can take any of the
*     following values:
*
*     0 - Do not report any conditions.
*
*     1 - Report only conditions where significant information content has been
*     changed. For instance, an unsupported time-scale has been replaced by a
*     supported near-equivalent time-scale. Another example is if a basic
*     Channel unexpected encounters data items that may have been introduced
*     by later versions of AST.
*
*     2 - Report the above, and in addition report significant default
*     values. For instance, if no time-scale was specified when reading an
*     Object from an external data source, report the default time-scale
*     that is being used.
*
*     3 - Report the above, and in addition report any other potentially
*     interesting conditions that have no significant effect on the
*     conversion. For instance, report if a time-scale of "TT"
*     (terrestrial time) is used in place of "ET" (ephemeris time). This
*     change has no signficiant effect because ET is the predecessor of,
*     and is continuous with, TT. Synonyms such as "IAT" and "TAI" are
*     another example.
*
*     The default value is 1. Note, there are many other conditions that
*     can occur whilst reading or writing an Object that completely
*     prevent the conversion taking place. Such conditions will always
*     generate errors, irrespective of the ReportLevel and Strict attributes.

*  Applicability:
*     Channel
*        All Channels have this attribute.
*     FitsChan
*        All the conditions selected by the FitsChan Warnings attribute are
*        reported at level 1.
*att--
*/

/* This is an integer value with a value of -INT_MAX when undefined,
   yielding a default of one. */
astMAKE_CLEAR(Channel,ReportLevel,report_level,-INT_MAX)
astMAKE_GET(Channel,ReportLevel,int,1,( this->report_level != -INT_MAX ? this->report_level : 1 ))
astMAKE_SET(Channel,ReportLevel,int,report_level,value)
astMAKE_TEST(Channel,ReportLevel,( this->report_level != -INT_MAX ))

/*
*att++
*  Name:
*     Skip

*  Purpose:
*     Skip irrelevant data?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This is a boolean attribute which indicates whether the Object
*     data being read through a Channel are inter-mixed with other,
*     irrelevant, external data.
*
*     If Skip is zero (the default), then the source of input data is
*     expected to contain descriptions of AST Objects and comments and
*     nothing else (if anything else is read, an error will
*     result). If Skip is non-zero, then any non-Object data
*     encountered between Objects will be ignored and simply skipped
*     over in order to reach the next Object.

*  Applicability:
*     Channel
*        All Channels have this attribute.
*     FitsChan
*        The FitsChan class sets the default value of this attribute
*        to 1, so that all irrelevant FITS headers will normally be
*        ignored.
*att--
*/

/* This ia a boolean value (0 or 1) with a value of -INT_MAX when
   undefined but yielding a default of zero. */
astMAKE_CLEAR(Channel,Skip,skip,-INT_MAX)
astMAKE_GET(Channel,Skip,int,0,( this->skip != -INT_MAX ? this->skip : 0 ))
astMAKE_SET(Channel,Skip,int,skip,( value != 0 ))
astMAKE_TEST(Channel,Skip,( this->skip != -INT_MAX ))

/*
*att++
*  Name:
*     Strict

*  Purpose:
*     Report an error if any unexpeted data items are found?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This is a boolean attribute which indicates whether a warning
*     rather than an error should be issed for insignificant conversion
*     problems. If it is set non-zero, then fatal errors are issued
*     instead of warnings, resulting in the
c     AST error status being set.
f     inherited STATUS variable being set to an error value.
*     If Strict is zero (the default), then execution continues after minor
*     conversion problems, and a warning message is added to the Channel
*     structure. Such messages can be retrieved using the
c     astWarnings
f     AST_WARNINGS
*     function.

*  Notes:
*     - This attribute was introduced in AST version 5.0. Prior to this
*     version of AST unexpected data items read by a basic Channel always
*     caused an error to be reported. So applications linked against
*     versions of AST prior to version 5.0 may not be able to read Object
*     descriptions created by later versions of AST, if the Object's class
*     description has changed.

*  Applicability:
*     Channel
*        All Channels have this attribute.
*att--
*/

/* This ia a boolean value (0 or 1) with a value of -INT_MAX when
   undefined but yielding a default of zero. */
astMAKE_CLEAR(Channel,Strict,strict,-INT_MAX)
astMAKE_GET(Channel,Strict,int,0,( this->strict != -INT_MAX ? this->strict : 0 ))
astMAKE_SET(Channel,Strict,int,strict,( value != 0 ))
astMAKE_TEST(Channel,Strict,( this->strict != -INT_MAX ))

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for Channel objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for Channel objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     This function attempts to execute even if the global error status is
*     set.
*/

/* Local Variables: */
   AstChannel *this;               /* Pointer to Channel */

/* Obtain a pointer to the Channel structure. */
   this = (AstChannel *) obj;

/* Free memory used to store warnings. */
   astAddWarning( this, 0, NULL, NULL, status );

/* Close any open input or output files. */
   if( this->fd_in ) fclose( this->fd_in );
   if( this->fd_out ) fclose( this->fd_out );

/* Free file name memory. */
   this->fn_in = astFree( this->fn_in );
   this->fn_out = astFree( this->fn_out );
}

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for Channel objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for Channel objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     -  This constructor makes a deep copy.
*/

/* Local Variables: */
   AstChannel *out;                /* Pointer to output Channel */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output Channels. */
   out = (AstChannel *) objout;

/* Just clear any references to the input memory from the output Channel. */
   out->warnings = NULL;
   out->nwarn = 0;
   out->fd_in = NULL;
   out->fn_in = NULL;
   out->fd_out = NULL;
   out->fn_out = NULL;
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for Channel objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the Channel class to an output Channel.

*  Parameters:
*     this
*        Pointer to the Object whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstChannel *this;             /* Pointer to the Channel structure */
   const char *comment;          /* Pointer to comment string */
   int ival;                     /* Integer value */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Channel structure. */
   this = (AstChannel *) this_object;

/* Write out values representing the instance variables for the
   Channel class.  Accompany these with appropriate comment strings,
   possibly depending on the values being written.*/

/* In the case of attributes, we first use the appropriate (private)
   Test...  member function to see if they are set. If so, we then use
   the (private) Get... function to obtain the value to be written
   out.

   For attributes which are not set, we use the astGet... method to
   obtain the value instead. This will supply a default value
   (possibly provided by a derived class which over-rides this method)
   which is more useful to a human reader as it corresponds to the
   actual default attribute value.  Since "set" will be zero, these
   values are for information only and will not be read back. */

/* Indent */
/* ------------ */
   set = TestIndent( this, status );
   ival = set ? GetIndent( this, status ) : astGetIndent( this );
   astWriteInt( channel, "Indnt", set, 0, ival, "Indentation increment" );

/* ReportLevel. */
/* ------------ */
   set = TestReportLevel( this, status );
   ival = set ? GetReportLevel( this, status ) : astGetReportLevel( this );
   astWriteInt( channel, "RpLev", set, 0, ival, "Error reporting level" );

/* Skip. */
/* ----- */
   set = TestSkip( this, status );
   ival = set ? GetSkip( this, status ) : astGetSkip( this );
   astWriteInt( channel, "Skip", set, 0, ival,
                ival ? "Ignore data between Objects" :
                       "No data allowed between Objects" );

/* Strict. */
/* ------- */
   set = TestStrict( this, status );
   ival = set ? GetStrict( this, status ) : astGetStrict( this );
   astWriteInt( channel, "Strict", set, 0, ival,
                ival ? "Report errors insead of warnings" :
                       "Report warnings instead of errors" );

/* Full. */
/* ----- */
   set = TestFull( this, status );
   ival = set ? GetFull( this, status ) : astGetFull( this );
   if ( ival < 0 ) {
      comment = "Suppress non-essential output";
   }else if ( ival == 0 ) {
      comment = "Output standard information";
   } else {
      comment = "Output maximum information";
   }
   astWriteInt( channel, "Full", set, 0, ival, comment );

/* Comment. */
/* -------- */
   set = TestComment( this, status );
   ival = set ? GetComment( this, status ) : astGetComment( this );
   astWriteInt( channel, "Comm", set, 0, ival,
                ival ? "Display comments" :
                       "Omit comments" );
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAChannel and astCheckChannel functions using the
   macros defined for this purpose in the "object.h" header file. */
astMAKE_ISA(Channel,Object)
astMAKE_CHECK(Channel)

AstChannel *astChannel_( const char *(* source)( void ),
                         void (* sink)( const char * ),
                         const char *options, int *status, ...) {
/*
*+
*  Name:
*     astChannel

*  Purpose:
*     Create a Channel.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "channel.h"
*     AstChannel *astChannel( const char *(* source)( void ),
*                             void (* sink)( const char * ),
*                             const char *options, ..., int *status )

*  Class Membership:
*     Channel constructor.

*  Description:
*     This function creates a new Channel and optionally initialises
*     its attributes.
*
*     A Channel implements low-level input/output for the AST library.
*     Writing an Object to a Channel (using astWrite) will generate a
*     textual representation of that Object, and reading from a
*     Channel (using astRead) will create a new Object from its
*     textual representation.
*
*     Normally, when you use a Channel, you should provide "source"
*     and "sink" functions which connect it to an external data store
*     by reading and writing the resulting text. By default, however,
*     a Channel will read from standard input and write to standard
*     output.

*  Parameters:
*     source
*        Pointer to a "source" function that takes no arguments and
*        returns a pointer to a null-terminated string.
*
*        This function will be used by the Channel to obtain lines of
*        input text. On each invocation, it should return a pointer to
*        the next input line read from some external data store, and a
*        NULL pointer when there are no more lines to read.
*
*        If "source" is NULL, the Channel will read from standard
*        input instead.
*     sink
*        Pointer to a "sink" function that takes a pointer to a
*        null-terminated string as an argument and returns void.
*
*        This function will be used by the Channel to deliver lines of
*        output text. On each invocation, it should deliver the
*        contents of the string supplied to some external data store.
*
*        If "sink" is NULL, the Channel will write to standard output
*        instead.
*     options
*        Pointer to a null-terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new Channel. The syntax used is identical to
*        that for the astSet function and may include "printf" format
*        specifiers identified by "%" symbols in the normal way.
*     status
*        Pointer to the inherited status variable.
*     ...
*        If the "options" string contains "%" format specifiers, then
*        an optional list of additional arguments may follow it in
*        order to supply values to be substituted for these
*        specifiers. The rules for supplying these are identical to
*        those for the astSet function (and for the C "printf"
*        function).

*  Returned Value:
*     astChannel()
*        A pointer to the new Channel.

*  Notes:
*     - A NULL pointer value will be returned if this function is
*     invoked with the global error status set, or if it should fail
*     for any reason.
*-

*  Implementation Notes:
*     - This function implements the basic Channel constructor which
*     is available via the protected interface to the Channel class.
*     A public interface is provided by the astChannelId_ function.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstChannel *new;              /* Pointer to new Channel */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the Channel, allocating memory and initialising the
   virtual function table as well if necessary. Supply pointers to
   (local) wrapper functions that can invoke the source and sink
   functions with appropriate arguments for the C language. */
   new = astInitChannel( NULL, sizeof( AstChannel ), !class_init, &class_vtab,
                         "Channel", source, SourceWrap, sink, SinkWrap );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   Channel's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new Channel. */
   return new;
}

AstChannel *astChannelId_( const char *(* source)( void ),
                           void (* sink)( const char * ),
                           const char *options, ... ) {
/*
*++
*  Name:
c     astChannel
f     AST_CHANNEL

*  Purpose:
*     Create a Channel.

*  Type:
*     Public function.

*  Synopsis:
c     #include "channel.h"
c     AstChannel *astChannel( const char *(* source)( void ),
c                             void (* sink)( const char * ),
c                             const char *options, ... )
f     RESULT = AST_CHANNEL( SOURCE, SINK, OPTIONS, STATUS )

*  Class Membership:
*     Channel constructor.

*  Description:
*     This function creates a new Channel and optionally initialises
*     its attributes.
*
*     A Channel implements low-level input/output for the AST library.
c     Writing an Object to a Channel (using astWrite) will generate a
f     Writing an Object to a Channel (using AST_WRITE) will generate a
*     textual representation of that Object, and reading from a
c     Channel (using astRead) will create a new Object from its
f     Channel (using AST_READ) will create a new Object from its
*     textual representation.
*
*     Normally, when you use a Channel, you should provide "source"
c     and "sink" functions which connect it to an external data store
f     and "sink" routines which connect it to an external data store
*     by reading and writing the resulting text. By default, however,
*     a Channel will read from standard input and write to standard
*     output. Alternatively, a Channel can be told to read or write from
*     specific text files using the SinkFile and SourceFile attributes,
*     in which case no sink or source function need be supplied.

*  Parameters:
c     source
f     SOURCE = SUBROUTINE (Given)
c        Pointer to a source function that takes no arguments and
c        returns a pointer to a null-terminated string.  If no value
c        has been set for the SourceFile attribute, this function
c        will be used by the Channel to obtain lines of input text. On
c        each invocation, it should return a pointer to the next input
c        line read from some external data store, and a NULL pointer
c        when there are no more lines to read.
c
c        If "source" is NULL and no value has been set for the SourceFile
c        attribute, the Channel will read from standard input instead.
f        A source routine, which is a subroutine which takes a single
f        integer error status argument.   If no value has been set
f        for the SourceFile attribute, this routine will be used by
f        the Channel to obtain lines of input text. On each
f        invocation, it should read the next input line from some
f        external data store, and then return the resulting text to
f        the AST library by calling AST_PUTLINE. It should supply a
f        negative line length when there are no more lines to read.
f        If an error occurs, it should set its own error status
f        argument to an error value before returning.
f
f        If the null routine AST_NULL is suppied as the SOURCE value,
f        and no value has been set for the SourceFile attribute,
f        the Channel will read from standard input instead.
c     sink
f     SINK = SUBROUTINE (Given)
c        Pointer to a sink function that takes a pointer to a
c        null-terminated string as an argument and returns void.
c        If no value has been set for the SinkFile attribute, this
c        function will be used by the Channel to deliver lines of
c        output text. On each invocation, it should deliver the
c        contents of the string supplied to some external data store.
c
c        If "sink" is NULL, and no value has been set for the SinkFile
c        attribute, the Channel will write to standard output instead.
f        A sink routine, which is a subroutine which takes a single
f        integer error status argument.  If no value has been set
f        for the SinkFile attribute, this routine will be used by
f        the Channel to deliver lines of output text. On each
f        invocation, it should obtain the next output line from the
f        AST library by calling AST_GETLINE, and then deliver the
f        resulting text to some external data store.  If an error
f        occurs, it should set its own error status argument to an
f        error value before returning.
f
f        If the null routine AST_NULL is suppied as the SINK value,
f        and no value has been set for the SinkFile attribute,
f        the Channel will write to standard output instead.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new Channel. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new Channel. The syntax used is identical to that for the
f        AST_SET routine.
c     ...
c        If the "options" string contains "%" format specifiers, then
c        an optional list of additional arguments may follow it in
c        order to supply values to be substituted for these
c        specifiers. The rules for supplying these are identical to
c        those for the astSet function (and for the C "printf"
c        function).
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astChannel()
f     AST_CHANNEL = INTEGER
*        A pointer to the new Channel.

*  Notes:
c     - Application code can pass arbitrary data (such as file
c     descriptors, etc) to source and sink functions using the
c     astPutChannelData function. The source or sink function should use
c     the astChannelData macro to retrieve this data.
f     - The names of the routines supplied for the SOURCE and SINK
f     arguments should appear in EXTERNAL statements in the Fortran
f     routine which invokes AST_CHANNEL. However, this is not generally
f     necessary for the null routine AST_NULL (so long as the AST_PAR
f     include file has been used).
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
f     - Note that the null routine AST_NULL (one underscore) is
f     different to AST__NULL (two underscores), which is the null Object
f     pointer.
*--

*  Implementation Notes:
*     - This function implements the external (public) interface to
*     the astChannel constructor function. It returns an ID value
*     (instead of a true C pointer) to external users, and must be
*     provided because astChannel_ has a variable argument list which
*     cannot be encapsulated in a macro (where this conversion would
*     otherwise occur).
*     - The variable argument list also prevents this function from
*     invoking astChanel_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstChannel *new;              /* Pointer to new Channel */
   va_list args;                 /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the Channel, allocating memory and initialising the
   virtual function table as well if necessary. Supply pointers to
   (local) wrapper functions that can invoke the source and sink
   functions with appropriate arguments for the C language. */
   new = astInitChannel( NULL, sizeof( AstChannel ), !class_init, &class_vtab,
                         "Channel", source, SourceWrap, sink, SinkWrap );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   Channel's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new Channel. */
   return astMakeId( new );
}

AstChannel *astChannelForId_( const char *(* source)( void ),
                              char *(* source_wrap)( const char *(*)( void ), int * ),
                              void (* sink)( const char * ),
                              void (* sink_wrap)( void (*)( const char * ),
                                                  const char *, int * ),
                              const char *options, ... ) {
/*
*+
*  Name:
*     astChannelFor

*  Purpose:
*     Initialise a Channel from a foreign language interface.

*  Type:
*     Public function.

*  Synopsis:
*     #include "channel.h"
*     AstChannel *astChannelFor( const char *(* source)( void ),
*                                char *(* source_wrap)( const char *(*)
*                                                       ( void ), int * ),
*                                void (* sink)( const char * ),
*                                void (* sink_wrap)( void (*)( const char * ),
*                                                    const char *, int * ),
*                                const char *options, ... )

*  Class Membership:
*     Channel constructor.

*  Description:
*     This function creates a new Channel from a foreign language
*     interface and optionally initialises its attributes.
*
*     A Channel implements low-level input/output for the AST library.
*     Writing an Object to a Channel (using astWrite) will generate a
*     textual representation of that Object, and reading from a
*     Channel (using astRead) will create a new Object from its
*     textual representation.
*
*     Normally, when you use a Channel, you should provide "source"
*     and "sink" functions which connect it to an external data store
*     by reading and writing the resulting text. This function also
*     requires you to provide "wrapper" functions which will invoke
*     the source and sink functions. By default, however, a Channel
*     will read from standard input and write to standard output.

*  Parameters:
*     source
*        Pointer to a "source" function which will be used to obtain
*        lines of input text. Generally, this will be obtained by
*        casting a pointer to a source function which is compatible
*        with the "source_wrap" wrapper function (below). The pointer
*        should later be cast back to its original type by the
*        "source_wrap" function before the function is invoked.
*
*        If "source" is NULL, the Channel will read from standard
*        input instead.
*     source_wrap
*        Pointer to a function which can be used to invoke the
*        "source" function supplied (above). This wrapper function is
*        necessary in order to hide variations in the nature of the
*        source function, such as may arise when it is supplied by a
*        foreign (non-C) language interface.
*
*        The single parameter of the "source_wrap" function is a
*        pointer to the "source" function, and it should cast this
*        function pointer (as necessary) and invoke the function with
*        appropriate arguments to obtain the next line of input
*        text. The "source_wrap" function should then return a pointer
*        to a dynamically allocated, null terminated string containing
*        the text that was read. The string will be freed (using
*        astFree) when no longer required and the "source_wrap"
*        function need not concern itself with this. A NULL pointer
*        should be returned if there is no more input to read.
*
*        If "source_wrap" is NULL, the Channel will read from standard
*        input instead.
*     sink
*        Pointer to a "sink" function which will be used to deliver
*        lines of output text. Generally, this will be obtained by
*        casting a pointer to a sink function which is compatible with
*        the "sink_wrap" wrapper function (below). The pointer should
*        later be cast back to its original type by the "sink_wrap"
*        function before the function is invoked.
*
*        If "sink" is NULL, the Channel will write to standard output
*        instead.
*     sink_wrap
*        Pointer to a function which can be used to invoke the "sink"
*        function supplied (above). This wrapper function is necessary
*        in order to hide variations in the nature of the sink
*        function, such as may arise when it is supplied by a foreign
*        (non-C) language interface.
*
*        The first parameter of the "sink_wrap" function is a pointer
*        to the "sink" function, and the second parameter is a pointer
*        to a const, null-terminated character string containing the
*        text to be written.  The "sink_wrap" function should cast the
*        "sink" function pointer (as necessary) and invoke the
*        function with appropriate arguments to deliver the line of
*        output text. The "sink_wrap" function then returns void.
*
*        If "sink_wrap" is NULL, the Channel will write to standard
*        output instead.
*     options
*        Pointer to a null-terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new Channel. The syntax used is identical to
*        that for the astSet function and may include "printf" format
*        specifiers identified by "%" symbols in the normal way.
*     ...
*        If the "options" string contains "%" format specifiers, then
*        an optional list of additional arguments may follow it in
*        order to supply values to be substituted for these
*        specifiers. The rules for supplying these are identical to
*        those for the astSet function (and for the C "printf"
*        function).

*  Returned Value:
*     astChannelFor()
*        A pointer to the new Channel.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
*     function is invoked with the global error status set, or if it
*     should fail for any reason.
*     - This function is only available through the public interface
*     to the Channel class (not the protected interface) and is
*     intended solely for use in implementing foreign language
*     interfaces to this class.
*-

*  Implememtation Notes:
*     - This function behaves exactly like astChannelId_, in that it
*     returns ID values and not true C pointers, but it has two
*     additional arguments. These are pointers to the "wrapper
*     functions" which are needed to accommodate foreign language
*     interfaces.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   AstChannel *new;              /* Pointer to new Channel */
   va_list args;                 /* Variable argument list */
   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialise the Channel, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitChannel( NULL, sizeof( AstChannel ), !class_init, &class_vtab,
                         "Channel", source, source_wrap, sink, sink_wrap );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   Channel's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new Channel. */
   return astMakeId( new );
}

AstChannel *astLoadChannel_( void *mem, size_t size,
                             AstChannelVtab *vtab, const char *name,
                             AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadChannel

*  Purpose:
*     Load a Channel.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "channel.h"
*     AstChannel *astLoadChannel( void *mem, size_t size,
*                                 AstChannelVtab *vtab, const char *name,
*                                 AstChannel *channel )

*  Class Membership:
*     Channel loader.

*  Description:
*     This function is provided to load a new Channel using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     Channel structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a Channel at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the Channel is to be
*        loaded.  This must be of sufficient size to accommodate the
*        Channel data (sizeof(Channel)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the Channel (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the Channel structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstChannel) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new Channel. If this is NULL, a pointer
*        to the (static) virtual function table for the Channel class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "Channel" is used instead.

*  Returned Value:
*     A pointer to the new Channel.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstChannel *new;              /* Pointer to the new Channel */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this Channel. In this case the
   Channel belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstChannel );
      vtab = &class_vtab;
      name = "Channel";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitChannelVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built Channel. */
   new = astLoadObject( mem, size, (AstObjectVtab *) vtab, name,
                        channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "Channel" );

/* Set the pointers to the source and sink functions, and their
   wrapper functions, to NULL (we cannot restore these since they
   refer to process-specific addresses). */
      new->source = NULL;
      new->source_wrap = NULL;
      new->sink = NULL;
      new->sink_wrap = NULL;

/* We do not have any data to pass to the source and sink functions. */
      new->data = NULL;

/* No warnings yet. */
      new->warnings = NULL;
      new->nwarn = 0;

/* Indicate no input or output files have been associated with the
   Channel. */
      new->fd_in = NULL;
      new->fn_in = NULL;
      new->fd_out = NULL;
      new->fn_out = NULL;

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Indent. */
/* ------- */
      new->indent = astReadInt( channel, "indnt", -INT_MAX );
      if ( TestIndent( new, status ) ) SetIndent( new, new->indent, status );

/* ReportLevel. */
/* ------------ */
      new->report_level = astReadInt( channel, "rplev", -INT_MAX );
      if ( TestReportLevel( new, status ) ) SetReportLevel( new,
                                                            new->report_level,
                                                            status );

/* Skip. */
/* ----- */
      new->skip = astReadInt( channel, "skip", -INT_MAX );
      if ( TestSkip( new, status ) ) SetSkip( new, new->skip, status );

/* Strict. */
/* ------- */
      new->strict = astReadInt( channel, "strict", -INT_MAX );
      if ( TestStrict( new, status ) ) SetStrict( new, new->strict, status );

/* Full. */
/* ----- */
      new->full = astReadInt( channel, "full", -INT_MAX );
      if ( TestFull( new, status ) ) SetFull( new, new->full, status );

/* Comment. */
/* -------- */
      new->comment = astReadInt( channel, "comm", -INT_MAX );
      if ( TestComment( new, status ) ) SetComment( new, new->comment, status );

/* If an error occurred, clean up by deleting the new Channel. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new Channel pointer. */
   return new;
}

/* Virtual function interfaces. */
/* ============================ */
/* These provide the external interface to the virtual functions
   defined by this class. Each simply checks the global error status
   and then locates and executes the appropriate member function,
   using the function pointer stored in the object's virtual function
   table (this pointer is located using the astMEMBER macro defined in
   "object.h").

   Note that the member function may not be the one defined here, as
   it may have been over-ridden by a derived class. However, it should
   still have the same interface. */
void astGetNextData_( AstChannel *this, int begin, char **name, char **val, int *status ) {
   *name = NULL;
   *val = NULL;
   if ( !astOK ) return;
   (**astMEMBER(this,Channel,GetNextData))( this, begin, name, val, status );
}
char *astGetNextText_( AstChannel *this, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Channel,GetNextText))( this, status );
}
void astPutNextText_( AstChannel *this, const char *line, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Channel,PutNextText))( this, line, status );
}
AstObject *astRead_( AstChannel *this, int *status ) {
   if ( !astOK ) return NULL;
   astAddWarning( this, 0, NULL, NULL, status );
   return (**astMEMBER(this,Channel,Read))( this, status );
}
void astReadClassData_( AstChannel *this, const char *class, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Channel,ReadClassData))( this, class, status );
}
double astReadDouble_( AstChannel *this, const char *name, double def, int *status ) {
   if ( !astOK ) return 0.0;
   return (**astMEMBER(this,Channel,ReadDouble))( this, name, def, status );
}
int astReadInt_( AstChannel *this, const char *name, int def, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Channel,ReadInt))( this, name, def, status );
}
AstObject *astReadObject_( AstChannel *this, const char *name,
                           AstObject *def, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Channel,ReadObject))( this, name, def, status );
}
char *astReadString_( AstChannel *this, const char *name, const char *def, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Channel,ReadString))( this, name, def, status );
}
void astWriteBegin_( AstChannel *this, const char *class,
                     const char *comment, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Channel,WriteBegin))( this, class, comment, status );
}
void astWriteDouble_( AstChannel *this, const char *name, int set, int helpful,
                      double value, const char *comment, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Channel,WriteDouble))( this, name, set, helpful, value,
                                            comment, status );
}
void astWriteEnd_( AstChannel *this, const char *class, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Channel,WriteEnd))( this, class, status );
}
void astWriteInt_( AstChannel *this, const char *name, int set, int helpful,
                   int value, const char *comment, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Channel,WriteInt))( this, name, set, helpful, value,
                                         comment, status );
}
void astWriteIsA_( AstChannel *this, const char *class, const char *comment, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Channel,WriteIsA))( this, class, comment, status );
}
void astWriteString_( AstChannel *this, const char *name, int set, int helpful,
                      const char *value, const char *comment, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Channel,WriteString))( this, name, set, helpful, value,
                                            comment, status );
}
void astPutChannelData_( AstChannel *this, void *data, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Channel,PutChannelData))( this, data, status );
}

AstKeyMap *astWarnings_( AstChannel *this, int *status ){
   if( !astOK ) return NULL;
   return (**astMEMBER(this,Channel,Warnings))( this, status );
}

/* Because of the variable argument list, we need to work a bit harder on
   astAddWarning. Functions that provide implementations of the
   astAddWarning method recieve the fully expanded message and so do not
   need a variable argument list. */

void astAddWarning_( void *this_void, int level, const char *fmt,
                     const char *method, int *status, ... ) {
   AstChannel *this;
   char buff[ 201 ];
   va_list args;
   int nc;

   this = astCheckChannel( this_void );

   if( fmt ) {
      if( astOK ) {
         va_start( args, status );
         nc = vsprintf( buff, fmt, args );
         va_end( args );
         if( nc > 200 ) {
            astError( AST__INTER, "astAddWarning(%s): Message buffer size "
                      "exceeded (internal AST programming error).",
                      status, astGetClass( this ) );
         } else {
            (**astMEMBER(this,Channel,AddWarning))( this, level, buff, method, status );
         }
      }
   } else {
      (**astMEMBER(this,Channel,AddWarning))( this, level, NULL, method, status );
   }
}

/* Count the number of times astWrite is invoked (excluding invocations
   made from within the astWriteObject method - see below). The count is
   done here so that invocations of astWrite within a sub-class will be
   included. */
int astWrite_( AstChannel *this, AstObject *object, int *status ) {
   astDECLARE_GLOBALS
   if ( !astOK ) return 0;
   astGET_GLOBALS(this);
   nwrite_invoc++;
   astAddWarning( this, 0, NULL, NULL, status );
   return (**astMEMBER(this,Channel,Write))( this, object, status );
}

/* We do not want to count invocations of astWrite made from within the
   astWriteObject method. So decrement the number of invocations first
   (this assumes that each invocation of astWriteObject will only invoke
   astWrite once). */
void astWriteObject_( AstChannel *this, const char *name, int set,
                      int helpful, AstObject *value, const char *comment, int *status ) {
   astDECLARE_GLOBALS
   if ( !astOK ) return;
   astGET_GLOBALS(this);
   nwrite_invoc--;
   (**astMEMBER(this,Channel,WriteObject))( this, name, set, helpful, value,
                                            comment, status );
}





