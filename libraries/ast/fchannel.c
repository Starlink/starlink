/*
*+
*  Name:
*     fchannel.c

*  Purpose:
*     Define a FORTRAN 77 interface to the AST Channel class.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines FORTRAN 77-callable C functions which provide
*     a public FORTRAN 77 interface to the Channel class.

*  Routines Defined:
*     AST_CHANNEL
*     AST_ISACHANNEL
*     AST_READ
*     AST_WRITE

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     6-SEP-1996 (RFWS):
*        Original version.
*     12-DEC-1996 (RFWS):
*        Added SOURCE and SINK arguments to AST_CHANNEL.
*     13-NOV-2003 (DSB):
*        Made SourceWrap and SinkWrap into protected functions rather
*        than private functions, so that they can be used in fxmlchan.c
*/

/* Define the astFORTRAN77 macro which prevents error messages from
   AST C functions from reporting the file and line number where the
   error occurred (since these would refer to this file, they would
   not be useful). */
#define astFORTRAN77

/* Header files. */
/* ============= */
#include "f77.h"                 /* FORTRAN <-> C interface macros (SUN/209) */
#include "c2f77.h"               /* F77 <-> C support functions/macros */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory handling facilities */
#include "channel.h"             /* C interface to the Channel class */

#include <stddef.h>

/* Module Variables. */
/* ================= */
static char *line_in = NULL;     /* Pointer to incoming line of text */
static const char *line_out = NULL; /* Pointer to outgoing line of text */

/* Prototypes for external functions. */
/* ================================== */
/* This is the null function defined by the FORTRAN interface in fobject.c. */
F77_SUBROUTINE(ast_null)( void );

/* Source and sink function interfaces. */
/* ==================================== */
/* These functions are concerned with allowing FORTRAN implementations
   of Channel source and sink functions to be passed to the Channel
   class and invoked when necessary by C code in the main class
   implementation. All FORTRAN-specific aspects of this interface are
   encapsulated here. */
F77_SUBROUTINE(ast_getline)( CHARACTER(LINE),
                             INTEGER(L),
                             INTEGER(STATUS)
                             TRAIL(LINE) ) {
/*
f++
*  Name:
*     AST_GETLINE

*  Purpose:
*     Obtain text to be written by a Channel sink routine.

*  Type:
*     Public function.

*  Synopsis:
*     CALL AST_GETLINE( LINE, L, STATUS )

*  Description:
*     This routine should only be used when implementing a routine
*     which will be passed as the SINK argument to AST_CHANNEL. It
*     should be used to obtain (from the AST library) each line of
*     text which is to be written to the external data sink. One such
*     line should be obtained in this way for each invocation of the
*     sink routine.

*  Parameters:
*     LINE = CHARACTER * ( * ) (Returned)
*        The line of text to be written. Depending on the length of
*        character variable supplied, the returned text may be
*        truncated if necessary. Note, however, that it will not be
*        padded with blanks in order to fill this variable.
*     L = INTEGER (Returned)
*        The number of characters returned, which may be zero. Note
*        that characters beyond the L'th character in the LINE
*        variable are not modified and may therefore contain junk.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine is only available in the Fortran interface to the
*     AST library.
f--
*/

/* Argument Pointers: */
   GENPTR_CHARACTER(LINE)
   GENPTR_INTEGER(L)

/* Local Variables: */
   int i;                        /* Loop counter for characters */

/* Set the error context and watch the STATUS value. */
   astAt( "AST_GETLINE", NULL, 0 );
   astWatchSTATUS(

/* Initialise the returned string length. */
      *L = 0;

/* Check the global error status. */
      if ( !astOK ) return;

/* If there is no outgoing line ready (e.g. if this routine has been
   called at an inappropriate point), we simply return
   nothing. Otherwise, loop to copy the text into the character
   argument supplied, ensuring that its length is not exceeded. */
      if ( line_out ) {
         for ( i = 0; line_out[ i ] && ( i < LINE_length ); i++ ) {
            LINE[ i ] = line_out[ i ];
         }

/* Return the number of characters copied. */
         *L = i;
      }
   )
}

F77_SUBROUTINE(ast_putline)( CHARACTER(LINE),
                             INTEGER(L),
                             INTEGER(STATUS)
                             TRAIL(LINE) ) {
/*
f++
*  Name:
*     AST_PUTLINE

*  Purpose:
*     Store a text line read by a Channel source routine.

*  Type:
*     Public function.

*  Synopsis:
*     CALL AST_PUTLINE( LINE, L, STATUS )

*  Description:
*     This routine should only be used when implementing a routine
*     which will be passed as the SOURCE argument to AST_CHANNEL. It
*     should be used to pass back (to the AST library) each line of
*     text read from the external data source. One such line should be
*     passed back in this way for each invocation of the source
*     routine.

*  Parameters:
*     LINE = CHARACTER * ( * ) (Given)
*        A character string containing the line of input text which
*        has been read.
*     L = INTEGER (Given)
*        The number of characters in the input line, which may be
*        zero. If there is no more input available (e.g. an end of
*        file has been reached), this value should be set negative and
*        this will terminate the read operation on the Channel.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine is only available in the Fortran interface to the
*     AST library.
f--
*/

/* Argument Pointers: */
   GENPTR_CHARACTER(LINE)
   GENPTR_INTEGER(L)

/* Local Variables: */
   int l;                        /* Number of characters in line */

/* Set the error context and watch the STATUS value. */
   astAt( "AST_PUTLINE", NULL, 0 );
   astWatchSTATUS(

/* Initialise the incoming line pointer. */
      line_in = NULL;

/* Check the global error status. */
      if ( !astOK ) return;

/* Obtain the number of characters in the line. */
      l = *L;

/* Negative values (or STATUS set) indicate end of input. If the value
   is not negative, limit the number of characters to the length of
   the character variable supplied. */
      if ( l >= 0 ) {
         if ( l > LINE_length ) l = LINE_length;

/* Create a dynamic string and fill it with the incoming data. Store
   the resulting pointer, which will be picked up by the SourceWrap
   function. */
         line_in = astString( LINE, l );
      }
   )
}

void astSinkWrap_( void (* sink)( const char * ), const char *line, int *status ) {
/*
*+
*  Name:
*     astSinkWrap

*  Purpose:
*     Wrapper function to invoke a FORTRAN Channel sink function.

*  Type:
*     Protected function.

*  Synopsis:
*     void astSinkWrap( void (* sink)( const char * ), const char *line )

*  Description:
*     This function invokes the sink function whose pointer is
*     supplied in order to write an output line to an external data
*     store.

*  Parameters:
*     sink
*        Pointer to a sink function. This should result from a cast
*        applied to a pointer to a function, with a single FORTRAN
*        INTEGER error status argument, that returns void.  This is
*        the form of Channel sink function employed by the FORTRAN
*        language interface to the AST library.
*     line
*        Pointer to a constant null-terminated string containing the
*        line of output text.
*-
*/

/* Local Variables; */
   DECLARE_INTEGER(STATUS);      /* FORTRAN error status variable */

/* Check the global error status. */
   if ( !astOK ) return;

/* Store the pointer to the output text line in the (static)
   "line_out" variable, where it will be accessed by the sink function
   invoking AST_GETLINE. */
   line_out = line;

/* Cast the sink function pointer to a pointer to the FORTRAN
   subroutine and then invoke it. Transfer the AST error status to and
   from the subroutine's error status argument. */
   STATUS = astStatus;
   ( *(void (*)()) sink )( INTEGER_ARG(&STATUS) );
   astSetStatus( STATUS );

/* Clear the outgoing line pointer. */
   line_out = NULL;
}

char *astSourceWrap_( const char *(* source)( void ), int *status ) {
/*
*+
*  Name:
*     astSourceWrap

*  Purpose:
*     Wrapper function to invoke a FORTRAN Channel source function.

*  Type:
*     Protected function.

*  Synopsis:
*     char *astSourceWrap( const char *(* source)( void ) )

*  Description:
*     This function invokes the source function whose pointer is
*     supplied in order to read the next input line from an external
*     data store. It then returns a pointer to a dynamic string
*     containing a copy of the text that was read.

*  Parameters:
*     source
*        Pointer to a source function. This should result from a cast
*        applied to a pointer to a function, with a single FORTRAN
*        INTEGER error status argument, that returns void.  This is
*        the form of Channel source function employed by the FORTRAN
*        language interface to the AST library.

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
*-
*/

/* Local Variables: */
   DECLARE_INTEGER(STATUS);      /* FORTRAN error status variable */
   char *result;                 /* Result pointer to return */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Initialise the incoming line pointer. */
   line_in = NULL;

/* Cast the source function pointer to a pointer to the FORTRAN
   subroutine and then invoke it. Transfer the AST error status to and
   from the subroutine's error status argument. */
   STATUS = astStatus;
   ( *(void (*)()) source )( INTEGER_ARG(&STATUS) );
   astSetStatus( STATUS );

/* This should result in a pointer to a dynamic string containing the
   input text being stored in the (static) "line_in" variable as a
   result of the source function invoking AST_PUTLINE. Save this
   string pointer and clear the original. */
   result = line_in;
   line_in = NULL;

/* If an error occurred, free the returned string. */
   if ( ! astOK ) result = astFree( result );

/* Return the result. */
   return result;
}

/* FORTRAN interface functions. */
/* ============================ */
/* These functions implement the remainder of the FORTRAN interface. */
F77_INTEGER_FUNCTION(ast_channel)( void (* SOURCE)(),
                                   void (* SINK)(),
                                   CHARACTER(OPTIONS),
                                   INTEGER(STATUS)
                                   TRAIL(OPTIONS) ) {
   GENPTR_CHARACTER(OPTIONS)
   F77_INTEGER_TYPE(RESULT);
   char *options;
   const char *(* source)( void );
   int i;
   void (* sink)( const char * );

   astAt( "AST_CHANNEL", NULL, 0 );
   astWatchSTATUS(

/* Set the source and sink function pointers to NULL if a pointer to
   the null routine AST_NULL has been supplied. */
      source = (const char *(*)( void )) SOURCE;
      if ( source == (const char *(*)( void )) F77_EXTERNAL_NAME(ast_null) ) {
         source = NULL;
      }
      sink = (void (*)( const char * )) SINK;
      if ( sink == (void (*)( const char * )) F77_EXTERNAL_NAME(ast_null) ) {
         sink = NULL;
      }
      options = astString( OPTIONS, OPTIONS_length );

/* Truncate the options string to exlucde any trailing spaces. */
      astChrTrunc( options );

/* Change ',' to '\n' (see AST_SET in fobject.c for why). */
      if ( astOK ) {
         for ( i = 0; options[ i ]; i++ ) {
            if ( options[ i ] == ',' ) options[ i ] = '\n';
         }
      }
      RESULT = astP2I( astChannelFor( source, astSourceWrap, sink, astSinkWrap,
                                      "%s", options ) );
      astFree( options );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_isachannel)( INTEGER(THIS),
                                      INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_LOGICAL_TYPE(RESULT);

   astAt( "AST_ISACHANNEL", NULL, 0 );
   astWatchSTATUS(
      RESULT = astIsAChannel( astI2P( *THIS ) ) ? F77_TRUE : F77_FALSE;
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_read)( INTEGER(THIS),
                                INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_INTEGER_TYPE(RESULT);

   astAt( "AST_READ", NULL, 0 );
   astWatchSTATUS(
      RESULT = astP2I( astRead( astI2P( *THIS ) ) );
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_write)( INTEGER(THIS),
                                 INTEGER(OBJECT),
                                 INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(OBJECT)
   F77_INTEGER_TYPE(RESULT);

   astAt( "AST_WRITE", NULL, 0 );
   astWatchSTATUS(
      RESULT = astWrite( astI2P( *THIS ), astI2P( *OBJECT ) );
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_warnings)( INTEGER(THIS),
                                    INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_INTEGER_TYPE(RESULT);

   astAt( "AST_WARNINGS", NULL, 0 );
   astWatchSTATUS(
      RESULT = astP2I( astWarnings( astI2P( *THIS ) ) );
   )
   return RESULT;
}



