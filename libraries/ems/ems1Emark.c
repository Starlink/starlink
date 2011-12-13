/*
 *+
 *  Name:
 *     ems1Emark

 *  Purpose:
 *     Mark a new context in the error message table.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     ems1Emark()

 *  Description:
 *     This sets a new context in the error table so that subsequent
 *     EMSFLUSH or EMSANNUL calls only flush or annul table entries
 *     in this context.

 *  Copyright:
 *     Copyright (C) 1983 Science & Engineering Research Council.
 *     Copyright (C) 2001 Central Laboratory of the Research Councils.
 *     Copyright (C) 2008 Science and Technology Facililties Council.
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
 *     SLW: Sid Wright (UCL)
 *     RFWS: R.F. Warren-Smith (STARLINK)
 *     PCTR: P.C.T. Rees (STARLINK)
 *     RTP: R.T. Platon (STARLINK)
 *     AJC: A.J.Chipperfield (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     17-APR-1983 (SLW):
 *        Original FORTRAN version.
 *     14-FEB-2001 (RTP)
 *        Rewritten in C based on the Fortran routine EMS1_EMARK
 *      6-MAR-2001 (AJC);
 *        Correctly declare mstr and pstr
 *     13-MAY-2008 (PWD):
 *        Use struct to access message table. Initialise thread context
 *        variables.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include "ems_err.h"                 /* EMS_ error codes */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems1.h"                    /* EMS_ private functions prototypes */
#include "ems_defs.h"                /* EMS_ message table */

#if USE_PTHREADS
#include <pthread.h>

/*  Id of the initial thread, needs to be established prior to any thread
 *  creation so we can keep a global error context. We assume this is only
 *  set once within the context of the program.
 */
pthread_t ems_thread_initial_id = 0;

/*  The thread specific data key, this should also be established prior to any
 *  thread creation.
 */
pthread_key_t ems_thread_data_key = 0;

/*  Initialiser, so we know the above have been set (0 may not be an invalid
 *  thread identifier). */
int ems_thread_initial_set = 0;

/*  Mutex for setting the above. */
static pthread_mutex_t foo_mutex = PTHREAD_MUTEX_INITIALIZER;

#endif

void ems1Emark( void )
{
    int istat;                   /* Local status */
    int mlen;                    /* Length of MSTR */
    int plen;                    /* Length of PSTR */
    char mstr[] = "Context stack overflow (EMS fault).";
                                      /* Local error message text */
    char pstr[] = "EMS_EMARK_CXOVF";  /* Local message name text */

    ems_msgtab_t *msgtab = ems1Gmsgtab();  /* Current message table */

    TRACE( "ems1Emark" );
    DEBUG( "ems1Emark", "BEFORE msglev = %d", msgtab->msglev );

    /*  Check for maximum number of error context levels. */
    if ( msgtab->msglev < EMS__MXLEV ) {

        /*  Open a new error message context and set it to contain no
         *  messages. */
        msgtab->msglev++;
        msgtab->msgmrk++;
        msgtab->msgcnt[ msgtab->msgmrk ] = msgtab->msgcnt[ msgtab->msgmrk -1 ];
    } else {

        /*  Context stack full, so increment MSGLEV and stack an error
         *  message. */
        msgtab->msglev++;

        mlen = strlen( mstr );
        plen = strlen( pstr );
        istat = EMS__CXOVF;

        /*  Call EMS1ESTOR to stack the error message. */
        ems1Estor( pstr, plen, mstr, mlen, &istat );
    }

#if USE_PTHREADS
    /*  Record the ID of the initial thread, if not already done and generate
     *  a key for handling thread specific data. Use a mutex just in case this
     *  isn't done before creating any threads (which is a programming
     *  error). */

    pthread_mutex_lock( &foo_mutex );
    if ( ems_thread_initial_set == 0 ) {
        ems_thread_initial_id = pthread_self();
        pthread_key_create( &ems_thread_data_key, ems1Fthreaddata );
        ems_thread_initial_set = 1;
    }
    pthread_mutex_unlock( &foo_mutex );

#endif

    DEBUG( "ems1Emark", "AFTER msglev = %d", msgtab->msglev );
    return;
}
