#if defined( THREAD_SAFE )

#define astCLASS

#include "globals.h"
#include "error.h"
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>

/* Configuration results. */
/* ---------------------- */
#include <config.h>

/* Select the appropriate memory management functions. These will be the
   system's malloc, free and realloc unless AST was configured with the
   "--with-starmem" option, in which case they will be the starmem
   malloc, free and realloc. */
#ifdef HAVE_STAR_MEM_H
#  include <star/mem.h>
#  define MALLOC starMalloc
#  define FREE starFree
#  define REALLOC starRealloc
#else
#  define MALLOC malloc
#  define FREE free
#  define REALLOC realloc
#endif

/* Module variables */
/* ================ */

/* A count of the number of thread-specific data structures created so
   far. Create a mutex to serialise access to this static variable. */
static int nthread = 0;
static pthread_mutex_t nthread_mutex = PTHREAD_MUTEX_INITIALIZER;

/* External variables visible throughout AST */
/* ========================================= */

/* Set a flag indicating that the thread-specific data key has not yet
   been created. */
pthread_once_t starlink_ast_globals_initialised = PTHREAD_ONCE_INIT;

/* Declare the pthreads key that will be associated with the thread-specific
   data for each thread. */
pthread_key_t starlink_ast_globals_key;

/* Declare the pthreads key that will be associated with the thread-specific
   status value for each thread. */
pthread_key_t starlink_ast_status_key;


/* Function definitions: */
/* ===================== */


void astGlobalsCreateKey_( void ) {
/*
*+
*  Name:
*     astGlobalsCreateKey_

*  Purpose:
*     Create the thread specific data key used for accessing global data.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "globals.h"
*     astGlobalsCreateKey_()

*  Description:
*     This function creates the thread-specific data key. It is called
*     once only by the pthread_once function, which is invoked via the
*     astGET_GLOBALS(this) macro by each AST function that requires access to
*     global data.

*  Returned Value:
*     Zero for success.

*-
*/

/* Create the key used to access thread-specific global data values.
   Report an error if it fails. */
   if( pthread_key_create( &starlink_ast_globals_key, NULL ) ) {
      fprintf( stderr, "ast: Failed to create Thread-Specific Data key" );

/* If succesful, create the key used to access the thread-specific status
   value. Report an error if it fails. */
   } else if( pthread_key_create( &starlink_ast_status_key, NULL ) ) {
      fprintf( stderr, "ast: Failed to create Thread-Specific Status key" );

   }

}

AstGlobals *astGlobalsInit_( void ) {
/*
*+
*  Name:
*     astGlobalsInit

*  Purpose:
*     Create and initialise a structure holding thread-specific global
*     data values.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "globals.h"
*     AstGlobals *astGlobalsInit;

*  Description:
*     This function allocates memory to hold thread-specific global data
*     for use throughout AST, and initialises it.

*  Returned Value:
*     Pointer to the structure holding global data values for the
*     currently executing thread.

*-
*/

/* Local Variables: */
   AstGlobals *globals;
   AstStatusBlock *status;

/* Allocate memory to hold the global data values for the currently
   executing thread. Use malloc rather than astMalloc (the AST memory
   module uses global data managed by this module and so using astMalloc
   could put us into an infinite loop). */
   globals = MALLOC( sizeof( AstGlobals ) );

   if ( !globals ){
      fprintf( stderr, "ast: Failed to allocate memory to hold AST "
               "global data values" );

/* Initialise the global data values. */
   } else {

/* Each thread has a unique integer identifier. */
      pthread_mutex_lock( &nthread_mutex );
      globals->thread_identifier = nthread++;
      pthread_mutex_unlock( &nthread_mutex );

#define INIT(class) astInit##class##Globals_( &(globals->class) );
      INIT( Error );
      INIT( Memory );
      INIT( Object );
      INIT( Axis );
      INIT( Mapping );
      INIT( Frame );
      INIT( Channel );
      INIT( CmpMap );
      INIT( KeyMap );
      INIT( FitsChan );
      INIT( FitsTable );
      INIT( CmpFrame );
      INIT( DSBSpecFrame );
      INIT( FrameSet );
      INIT( LutMap );
      INIT( MathMap );
      INIT( PcdMap );
      INIT( PointSet );
      INIT( SkyAxis );
      INIT( SkyFrame );
      INIT( SlaMap );
      INIT( SpecFrame );
      INIT( SphMap );
      INIT( TimeFrame );
      INIT( WcsMap );
      INIT( ZoomMap );
      INIT( FluxFrame );
      INIT( SpecFluxFrame );
      INIT( GrismMap );
      INIT( IntraMap );
      INIT( Plot );
      INIT( Plot3D );
      INIT( Region );
      INIT( Xml );
      INIT( XmlChan );
      INIT( Box );
      INIT( Circle );
      INIT( CmpRegion );
      INIT( DssMap );
      INIT( Ellipse );
      INIT( Interval );
      INIT( MatrixMap );
      INIT( NormMap );
      INIT( NullRegion );
      INIT( PermMap );
      INIT( PointList );
      INIT( PolyMap );
      INIT( Polygon );
      INIT( Prism );
      INIT( RateMap );
      INIT( SelectorMap );
      INIT( ShiftMap );
      INIT( SpecMap );
      INIT( Stc );
      INIT( StcCatalogEntryLocation );
      INIT( StcObsDataLocation );
      INIT( SwitchMap );
      INIT( Table );
      INIT( TimeMap );
      INIT( TranMap );
      INIT( UnitMap );
      INIT( WinMap );
      INIT( StcResourceProfile );
      INIT( StcSearchLocation );
      INIT( StcsChan );
#undef INIT

/* Save the pointer as the value of the starlink_ast_globals_key
   thread-specific data key. */
      if( pthread_setspecific( starlink_ast_globals_key, globals ) ) {
         fprintf( stderr, "ast: Failed to store Thread-Specific Data pointer." );

/* We also take this opportunity to allocate and initialise the
   thread-specific status value. */
      } else {
         status = MALLOC( sizeof( AstStatusBlock ) );
         if( status ) {
            status->internal_status = 0;
            status->status_ptr = &( status->internal_status );

/* If succesful, store the pointer to this memory as the value of the
   status key for the currently executing thread. Report an error if
   this fails. */
            if( pthread_setspecific( starlink_ast_status_key, status ) ) {
               fprintf( stderr, "ast: Failed to store Thread-Specific Status pointer." );
            }

         } else {
            fprintf( stderr, "ast: Failed to allocate memory for Thread-Specific Status pointer." );
         }
      }
   }

/* Return a pointer to the data structure holding the global data values. */
   return globals;
}

#endif

