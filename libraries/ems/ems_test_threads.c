/*
 *+
 *  Name:
 *     ems_test_threads

 *  Purpose:
 *     A simple test of the C EMS threads installation

 *  Copyright:
 *     Copyright (C) Science and Technology Facilties Council.
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
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>

#if ! USE_PTHREADS
int main( void )
{
    printf( "No threads, test skipped\n" );
    return 0;
}
#else

void *make_report( void *threadid );
void *make_report_facility( void *threadid );
void *make_report_load( void *threadid );

#include <sae_par.h>
#include <ems.h>
#include <ems_par.h>
#include <ems_err.h>
#include <f77.h>

#include <pthread.h>

#define NUM_THREADS 5

void *make_report( void *threadid )
{
    intptr_t tid;
    int status = EMS__TEST;

    tid = (intptr_t) threadid;
    if ( tid != 0 && tid != 3 ) {

        /* Basic error handling. */
        emsSeti( "THREADID", tid );
        emsSeti( "STATUS", status );
        emsRep( "THREAD", "Oh no an error in thread ^THREADID (^STATUS)",
                &status );
    }
    return NULL;
}

void *make_report_facility( void *threadid )
{
    intptr_t tid;
    int status = EMS__TEST;

    tid = (intptr_t) threadid;
    if ( tid != 0 && tid != 3 ) {

        /* Test facility lookup. */
        emsSeti( "THREADID", tid );
        emsFacer( "FACMSG", status );
        emsRep( "THREAD",
                "Facility lookup says: ^FACMSG in thread ^THREADID",
                &status );
    }
    return NULL;
}

void *make_report_load( void *threadid )
{
    intptr_t tid;
    int status = EMS__TEST;
    char param[EMS__SZPAR+1];
    int parlen;
    char opstr[EMS__SZMSG];
    int oplen;

    tid = (intptr_t) threadid;
    if ( tid != 0 && tid != 3 ) {

        /* Basic error handling. */
        emsSeti( "THREADID", tid );
        emsSeti( "STATUS", status );
        emsRep( "THREAD", "Oh no an error in thread ^THREADID (^STATUS)",
                &status );

        /* Test message loading. */
        if ( ( tid == 2 || tid == 4 ) && status != SAI__OK ) {
            printf( "Testing message loading (immediate):\n" );
            while ( status != SAI__OK ) {
                emsEload( param, &parlen, opstr, &oplen, &status );
                if ( status != SAI__OK ) {
                    printf( "      %s -- %s\n", param, opstr );
                }
            }
        }
    }
    return NULL;
}

int main ( void )
{
    pthread_t threads[ NUM_THREADS ];
    int rc;
    intptr_t t;
    int status = SAI__OK;

    /* Mandated for threads, also required to stop immediate delivery. */
    emsMark();
    printf( "\n" );
    printf( "Initial status = %d\n", status );

    printf( "In initial thread creating %d threads\n", NUM_THREADS );
    for ( t = 0; t < NUM_THREADS; t++ ) {
        fflush( stdout );
        rc = pthread_create( &threads[ t ], NULL, make_report, (void *)t );
        if ( rc ) {
            printf( "ERROR; return code from pthread_create() is %d\n", rc );
            return -1;
        }
    }

    /*  Wait for threads to complete. */
    for ( t = 0; t < NUM_THREADS; t++ ) {
        pthread_join( threads[ t ], NULL );
    }

    /* Check for the exit status */
    emsStat( &status );
    emsRlse();
    printf( "All thread exit status = %d\n", status );

    /*  Same again, but start with an error in global scope. */
    /*  ==================================================== */

    emsMark();
    printf( "\n" );
    status = EMS__OPTER;
    printf( "Initial status = %d\n", status );

    emsRep( "THREAD", "Initial thread starts in EMS__OPTER", &status );

    printf( "In initial thread creating %d threads\n", NUM_THREADS );
    for ( t = 0; t < NUM_THREADS; t++ ) {
        fflush( stdout );
        rc = pthread_create( &threads[ t ], NULL, make_report, (void *)t );
        if ( rc ) {
            printf( "ERROR; return code from pthread_create() is %d\n", rc );
            return -1;
        }
    }

    /*  Wait for threads to complete. */
    for ( t = 0; t < NUM_THREADS; t++ ) {
        pthread_join( threads[ t ], NULL );
    }

    /* Check for the exit status */
    emsStat( &status );
    emsRep( "THREAD", "Initial thread exits in error", &status );
    emsRlse();
    printf( "All thread exit status = %d\n", status );

    /*  Same again, but complete with an error in global scope. */
    /*  ==================================================== */

    emsMark();
    printf( "\n" );
    status = SAI__OK;
    printf( "Initial status = %d\n", status );

    printf( "In initial thread creating %d threads\n", NUM_THREADS );
    for ( t = 0; t < NUM_THREADS; t++ ) {
        fflush( stdout );
        rc = pthread_create( &threads[ t ], NULL, make_report, (void *)t );
        if ( rc ) {
            printf( "ERROR; return code from pthread_create() is %d\n", rc );
            return -1;
        }
    }

    /*  Wait for threads to complete. */
    for ( t = 0; t < NUM_THREADS; t++ ) {
        pthread_join( threads[ t ], NULL );
    }

    status = EMS__OPTER;
    emsRep( "THREAD", "Initial thread exits in error", &status );

    /* Check for the exit status */
    emsStat( &status );
    emsRlse();
    printf( "Exit status = %d\n", status );

    /*  Same, but report facility error. */
    /*  ================================ */

    emsMark();
    printf( "\n" );
    status = SAI__OK;
    printf( "Initial status = %d\n", status );

    printf( "In initial thread creating %d threads\n", NUM_THREADS );
    for ( t = 0; t < NUM_THREADS; t++ ) {
        fflush( stdout );
        rc = pthread_create( &threads[ t ], NULL, make_report_facility, (void *)t );
        if ( rc ) {
            printf( "ERROR; return code from pthread_create() is %d\n", rc );
            return -1;
        }
    }

    /*  Wait for threads to complete. */
    for ( t = 0; t < NUM_THREADS; t++ ) {
        pthread_join( threads[ t ], NULL );
    }

    status = EMS__OPTER;
    emsRep( "THREAD", "Initial thread exits in error", &status );

    /* Check for the exit status */
    emsStat( &status );
    emsRlse();
    printf( "Exit status = %d\n", status );


    /*  Finally complete by clearing any errors in global scope. */
    /*  ======================================================== */
    /*  Messages printed using an emsEload. */

    emsMark();
    printf( "\n" );
    status = SAI__OK;
    printf( "Initial status = %d\n", status );

    printf( "In initial thread creating %d threads\n", NUM_THREADS );
    printf( "  global will be cleared, messages loaded in thread\n" );
    for ( t = 0; t < NUM_THREADS; t++ ) {
        fflush( stdout );
        rc = pthread_create( &threads[ t ], NULL, make_report_load, (void *)t );
        if ( rc ) {
            printf( "ERROR; return code from pthread_create() is %d\n", rc );
            return -1;
        }
    }

    /*  Wait for threads to complete. */
    for ( t = 0; t < NUM_THREADS; t++ ) {
        pthread_join( threads[ t ], NULL );
    }

    status = EMS__OPTER;
    emsRep( "THREAD", "Initial thread exits in error", &status );

    /* Check for the exit status */
    emsStat( &status );

    /* Annul all errors */
    if ( status != SAI__OK ) {
        emsAnnul( &status );
    }
    emsRlse();
    printf( "Exit status = %d\n", status );

    return 0;
}

#endif  /* USE_PTHREADS */
