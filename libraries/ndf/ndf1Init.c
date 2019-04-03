#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include "sae_par.h"
#include "mers.h"
#include "ndf1.h"
#include "ndf1_types.h"
#include "ndf_err.h"

pthread_once_t starlink_ndf_globals_initialised = PTHREAD_ONCE_INIT;
pthread_key_t starlink_ndf_globals_key;

/* Command line arguments */
int NDF_DCB_argc = 0;
char **NDF_DCB_argv = NULL;

static void ndf1GlobalInitialisation( void );
static void *ndf1ThreadInitialisation( void );
static void ndf1FreeThreadData( void *data );

int *ndf1Init( int *status ){
/*
*+
*  Name:
*     ndf1Init

*  Purpose:
*     Initialise the global and per-thread data used by the NDF library.

*  Synopsis:
*     int *ndf1Init( int *status )

*  Description:
*     This function ensures that all necessary initialisation - both
*     of global data and of thread-specific data - has been done.
*     It should be the first thing called in each public NDF function.
*     Amonst other things, it tells the AST library to use the supplied
*     status pointer.

*  Parameters:
*     status
*        The global status. If this is NULL, the AST internal status
*        pointer is established.

*  Returned Value:
*     The original AST status pointer. This value should be re-established
*     by calling astWatch before the calling NDF function exists.

*  Copyright:
*      Copyright (C) 2018 East Asian Observatory
*      All rights reserved.

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

*  History:
*     31-MAY-2018 (DSB):
*        Original version.

*-
*/

/* Local Variables; */
   int lstat = SAI__OK;
   int *result;

/* Tell AST to use the supplied status variable, or its own internal
   status variable if "status" is NULL. */
   result = astWatch( status );

/* From here on, use the local status value within this function if no
   status pointer was supplied. */
   if( status == NULL ) {
      errMark();
      status = &lstat;
   }

/* Check the inhited status */
   if( *status != SAI__OK ) return result;

/* Ensure that the key used to find thread specific data has been created.
   The key is created by function ndf1GlobalsInitialisation. Invoke this
   function via pthread_once to ensure that it is only invoked once for
   the entire NDF library (not once - or more than once - for each thread).
   Report an error if this fails. */
   if( pthread_once( &starlink_ndf_globals_initialised, ndf1GlobalInitialisation ) ) {
      if( status ) {
         *status = NDF__INERR;
         errRep( " ", "NDF package global initialisation failed.", status );
      } else {
         fprintf( stderr, "NDF package global initialisation failed." );
      }

/* If the global initialisation went OK, we now have a thread specific data
   key. If the current thread does not yet have a structure associated
   with this key in which to hold thread-specific data, create one now
   (initialising its contents) and associate it with the thread specific
   data key. */
   } else if( pthread_getspecific( starlink_ndf_globals_key ) == NULL ) {
      if( pthread_setspecific( starlink_ndf_globals_key, ndf1ThreadInitialisation()) ) {
         if( status ) {
            *status = NDF__INERR;
            errRep( " ", "NDF package thread initialisation failed.", status );
         } else {
            fprintf( stderr, "NDF package thread initialisation failed." );
         }
      }
   }

/* If no status pointer was supplied, display any error messages */
   if( status == &lstat ) {
      if( *status != SAI__OK ) errFlush( status );
      errRlse();
   }

/* Return the original AST status pointer */
   return result;
}

static void ndf1GlobalInitialisation( void ){
/*
*  Name:
*     ndf1GlobalInitialisation

*  Purpose:
*     Perform all global initialisation required by the NDF Library.

*  Description:
*     This function does global initialisation of the NDF library -
*     i.e. initialisation that needs to be done once and applies to
*     all threads (initialisation of each individual inthread is done
*     by ndf1ThreadInitialisation). It is called once only by the
*     pthread_once function, which should be invoked, via the ndf1Init
*     function, at the start of each public NDF function. The argument
*     list of this function is defined by the pthreads library and so
*     there is no "status" argument.
*/

/* Create the key used to access thread-specific global data values.
   Write a message to standard error if it fails (since we have no status
   argument and so cannot used errRep). */
   if( pthread_key_create( &starlink_ndf_globals_key, ndf1FreeThreadData ) ) {
      fprintf( stderr, "ndf: Failed to create Thread-Specific Data key" );

/* If succesful, do other initialisation. */
   } else {
      int status = SAI__OK;
      errMark();

/* Initialise the TCB (all threads share the same tuning parameters). */
      ndf1Intcb( &status );

/* Initialise the FCB (all threads share the same foreign format info). */
      ndf1Infcb( &status );

/* Calculate the largest value that can be squared without overflow for
   each CGEN data type, and store in global variables for future read-only
   use. */
      ndf1StoreSqLimit();

/* Other global data initialisations. */
      Ndf_DCB_ccn[ NDF__LABEL ] = "LABEL";
      Ndf_DCB_ccn[ NDF__TITLE ] = "TITLE";
      Ndf_DCB_ccn[ NDF__UNITS ] = "UNITS";

      Ndf_DCB_accn[ NDF__ALAB ] = "LABEL";
      Ndf_DCB_accn[ NDF__AUNI ] = "UNITS";

      Ndf_DCB_happn[0] = 0;
      Ndf_TMP_tmploc = NULL;
      Ndf_APB_pars = NULL;

/* End the error reporting context, flushing any pending error messages. */
      if( status != SAI__OK ) errFlush( &status );
      errRlse();
   }
}

static void *ndf1ThreadInitialisation( void ){
/*
*  Name:
*     ndf1ThreadInitialisation

*  Purpose:
*     Perform all thread initialisation required by the NDF Library.

*  Description:
*     This function does thread initialisation for the NDF library -
*     i.e. initialisation that needs to be done once and applies to
*     to the current thread only (global initialisation is done by
*     ndf1GlobalInitialisation). It is called once only via the
*     ndf1Init function, which should be invoked at the start of
*     each public NDF function. The argument list of this function
*     is defined by the pthreads library and so there is no "status"
*     argument.

*/

/* Allocate the memory, filling it with zeros. Report an error if the
   memory cannot be allocated. */
   NdfTSD *result = calloc( 1, sizeof( NdfTSD ) );
   if( !result ) {
      fprintf( stderr, "ndf: Failed to create Thread-Specific Data structure" );

/* Do any extra initialisation required. */
   } else {
      result->elbStat = SAI__OK;
      result->acbIdctx = 1;
   }
   return result;
}



static void ndf1FreeThreadData( void *data ){
/*
*  Name:
*     ndf1FreeThreadData

*  Purpose:
*     Free the thread-specific data associated with the current thread.

*  Description:
*     This function frees the resources used by the thread-specific
*     data structure associated with the current thread.

*/
   if( data ) free( data );
}

__attribute__((constructor)) void ndf1GetCmdLine(int argc, char **argv){
/*
*  Name:
*      ndf1GetCmdLine

*  Purpose:
*     Store the command line arguments in gloval variables.

*  Description:
*     This function stores copies of the argc and argv values that
*     contain the command line arguments. These are the same values that
*     are passed to the "main" function in C. This function is not called
*     explicitly anywhere in the NDF library. Instead, the "constructor"
*     attribute causes this function to be run automatically when the NDF
*     library is loaded by the dynamic linker. It is non-portable, but works
*     with gcc on both Linux and MacOSX, and with clang on MacOSX. On a
*     system where this function is not called automatically, the values
*     of NDF_DCB_argc and NDF_DCB_argv will be left at the null values
*     initialised in the declarations of these variables at the top of this
*     file. The ndf1Gtarg function will spot this and return null command
*     line argument values.
*/
   NDF_DCB_argc = argc;
   NDF_DCB_argv = argv;
}
