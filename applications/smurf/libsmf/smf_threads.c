/*
*  Name:
*     smf_threads.c

*  Purpose:
*     Provides various utility functions related to multi-threading.

*  Description:
*     This module currently includes:
*
*     1 - Wrappers for the basic pthreads functions. These wrappers add
*     inherited status handling:
*
*     - smf_mutex_init: Initialise a mutex
*     - smf_cond_init: Initialise a condition variable
*     - smf_thread_create: Create a thread
*     - smf_mutex_lock: Lock a mutex
*     - smf_mutex_unlock: Unlock a mutex
*     - smf_cond_broadcast: Broadcast a condition
*     - smf_cond_signal: Signal a condition
*     - smf_cond_wait: Wait for a condition
*
*     2 - A set of functions that maintains a pool of threads ready for
*     use. Each thread in the pool is described as a "worker" and the
*     whole pool is described as a "workforce". The idea is that a task is
*     split into separate jobs, and all jobs are performed in parallel by
*     the workers in the workforce. Once a workforce has been told about all
*     the jobs within a task (using smf_add_job), the calling thread waits
*     until all the jobs have been completed.
*
*     - smf_create_workforce: Create a workforce with a given number of
*       workers
*     - smf_destroy_workforce: Free all resources used by a workforce.
*     - smf_add_job: Tell the workforce about a specific job that forms
*       part of the overall task.
*     - smf_wait: Block the calling thread until all jobs that the
*       workforce currently knows about have been completed.
*     - smf_job_wait: Block the calling thread until the next job has
*       been completed.

*  Copyright:
*     Copyright (C) 2008-2009 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: B.S. Berry (Starlink)

*  History:
*     11-JUN-2008 (DSB):
*        Original version.
*     18-NOV-2008 (DSB):
*        Changed smf_destroy_workforce so that the worker threads are
*        terminated correctly.
*     20-NOV-2008 (DSB):
*        Initialise result->kill in smf_create_workforce.
*     27-NOV-2008 (DSB):
*        - Correct places where the workforce status was accessed by a
*        thread that did not hold the job-desk mutex.
*        - Correct releasing of the job-desk mutex in smf_run_worker when
*        the worker thread is killed.
*     19-AUG-2009 (DSB):
*        Changed interface to smf_wait_on_job, and checker function
*        passed to smf_add_job.
*     24-AUG-2009 (DSB):
*        Added smf_get_job_data. Changed interface to smf_wait_on_job
*        again.
*     8-OCT-2009 (DSB):
*        Added smf_begin_job_context and smf_end_job_context.
*     19-JAN-2010 (DSB):
*        Major re-vamp of the way errors are handled. Do not rely on EMS
*        to communicate error reports from the worker threads to the
*        manager thread, as EMS only seems to merges status values when
*        the threads die. Instead, details of any errors reported during
*        the execution of a job are stored in the job structure. Details
*        of the first error to occur within a job context are also copied
*        into the workforce structure. When smf_wait exist, it
*        re-establishes any error status described in the workforce
*        structure.
*     26-JAN-2010 (DSB):
*        Ensure all public functions can be called with a null workforce
*        without causing an error to be reported. The only significant
*        change is that smf_add_job now executes the supplied job immediately
*        in the current thread if no workforce is supplied.
*     6-APR-2011 (DSB):
*        Change way in which interdependencies between jobs are handled  -
*        do away with the checker function idea, and instead when adding
*        a job just specify a list of jobs that need to complete before
*        the new job can be started.
*/


/* Include files */
/* ------------- */
/* System include files */
#include <stdio.h>
#include <pthread.h>
#include <sys/time.h>

/* Starlink include files */
#include "ast.h"
#include "sae_par.h"
#include "ems.h"
#include "ems_par.h"

/* SMURF include for malloc definitions */
#include "smf.h"

/* Definition of the public interface of this module */
#include "smf_threads.h"
#include "smf_typ.h"

/* Module macros */
/* ------------- */
#define DESK "BLACK"
#define WAIT "RED"
#define ACTIVE "GREEN"
#define smf_thread_log(text,col,job) {if( fd ) smf_thread_log_(text,col,job);}

/* Module types */
/* ------------ */
typedef struct JobError {
   int *status;
   int nmessage;
   char **messages;
} JobError;

/* Module variables */
/* ---------------- */
static FILE *fd = NULL;
static int njob = 0;
pthread_mutex_t fd_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Module Prototypes */
/* ----------------- */
static int smf_get_job_context( smfWorkForce *workforce, int *status );
static int smf_list_isempty( int conid, smfJob *head, int *status );
static smfJob *smf_find_job( smfJob *head, int ijob, int conid, int *status );
static smfJob *smf_free_job( smfJob *job );
static smfJob *smf_pop_list_first( smfJob **head, int conid, int *status );
static smfJob *smf_pop_list_head( smfJob **head, int *status );
static smfJobStatus *smf_copy_status( smfJobStatus *status );
static smfJobStatus *smf_free_status( smfJobStatus *status );
static smfJobStatus *smf_get_status( int *ems_status );
static smfJobStatus *smf_make_status( void );
static smfJobStatus *smf_report_status( smfJobStatus *status, int *ems_status );
static void *smf_run_worker( void *worker_ptr );
static void smf_clear_status( smfJobStatus *status );
static void smf_export_jobs( smfJob *head, int old, int new, int *status );
static void smf_init_job( smfJob *job, int *status );
static void smf_push_list_foot( smfJob *job, smfJob **head, int *status );
static void smf_push_list_head( smfJob *job, smfJob **head, int *status );
static void smf_remove_from_list( smfJob *job, smfJob **head, int *status );
static void smf_thread_log_( const char *text, const char *colour, int ijob );


/* Public workforce-related functions */
/* ---------------------------------- */

int smf_add_job( smfWorkForce *workforce, int flags, void *data,
                 void (*func)( void *, int * ), int nwait_on,
                 const int *wait_on, int *status ){
/*
*  Name:
*     smf_add_job

*  Purpose:
*     Add a job to the list of jobs to be performed by a given workforce.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     int smf_add_job( smfWorkForce *workforce, int flags, void *data,
*                      void (*func)( void *, int * ),  int nwait_on,
*                      const int *wait_on, int *status )

*  Arguments:
*     workforce
*        Pointer to the workforce. If NULL is supplied, the job is
*        executed immediately in the current thread by calling "func",
*        and the "flags" and "checker" arguments are ignored.
*     flags
*        Flags controlling how the job behaves. See "Job Control Flags:"
*        below.
*     data
*        An arbitrary data pointer that will be passed to the worker
*        allocated to perform this job.
*     func
*        A pointer to a function that the worker will invoke to do the
*        job. This function takes two arguments; 1) the supplied "data"
*        pointer, and 2) an inherited status pointer. It returns void.
*     nwait_on
*        The number of values supplied in the "wait_on" array. If zero,
*        the "wait_on" pointer will be ignored.
*     wait_on
*        An array of integer identifiers for previously created jobs. The
*        length of this array is given by "nwait_on". No attempt will be
*        made to start the new job until all the jobs specified in this
*        array have completed. If NULL is supplied, or if "nwait_on" is
*        zero, the new job will be started as soon as a worker thread
*        becomes available.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     A positive integer identifier for the job. Zero if an error occurs.

*  Description:
*     This function adds a job to the list of jobs to be performed by the
*     workforce. The job will start immediately if a worker thread is
*     available to execute the job, and any jobs specified in the
*     "wait_on" list have completed. Otherwise, it will start as soon as
*     a worker thread becomes available and all the "wait_on" jobs have
*     completed. Jobs are not necessarily started in the order in which
*     they are added to the workforce.

*  Job Control Flags:
*     SMF__REPORT_JOB: Indicates that this job is to be included in the
*     list of jobs for which smf_job_wait will wait.

*/

/* Local Variables: */
   smfJob *job;
   smfJob *job2;
   int i;
   int ijob2;

/* Check inherited status */
   if( *status != SAI__OK ) return 0;

/* If no work force was supplied, execute the job immediately in the
   current thread, and then return. */
   if( !workforce ) {
      (*func)( data, status );
      return 0;
   }

/* Wait until we are at the head of the queue at the job desk. We then
   have exclusive access to the workforce structure, etc. */
   smf_thread_log( "add_job: Join queue", WAIT, -1 );
   smf_mutex_lock( &(workforce->jd_mutex), status );
   smf_thread_log( "add_job: At desk", DESK, -1 );

/* Get a structure in which to place the job description. If any such
   structures are available on the workforce's list of free job structures,
   use one from the free list. Otherwise, create a new one and initialise it. */
   job = smf_pop_list_head( &(workforce->free_jobs), status );
   if( ! job ) {
      job = astMalloc( sizeof( smfJob ) );
      if( job ) job->status = NULL;
      smf_init_job( job, status );
   }

/* Store the supplied function and data pointers. */
   if( job ) {
      job->ijob = ++njob;
      job->flags = flags;
      job->func = func;
      job->data = data;
      job->nwaiting_on = 0;
      job->nheld_up = 0;

/* Store the current job context identifier. */
      job->conid = smf_get_job_context( workforce, status );

/* If a list was supplied of earlier jobs that must complete prior to the
   start of the new job, then check each one and only include jobs that have
   not yet completed. */
      if( wait_on ) {
         for( i = 0; i < nwait_on; i++ ) {
            ijob2 = wait_on[ i ];

/* Get a pointer to the structure describing the job. Search the lists of
   available, waiting and active jobs. */
            job2 = smf_find_job( workforce->available_jobs, ijob2, -1, status );
            if( !job2 ) job2 = smf_find_job( workforce->waiting_jobs, ijob2, -1, status );
            if( !job2 ) job2 = smf_find_job( workforce->active_jobs, ijob2, -1, status );

/* Ignore the job if it has already completed (or never existed). */
            if( job2 ) {

/* The new job is waiting on job2. So add job2 to the list of jobs that must
   complete prior to starting the new job. */
               ijob2 = job->nwaiting_on++;
               job->waiting_on = astGrow( job->waiting_on, job->nwaiting_on,
                                          sizeof( *job->waiting_on ) );
               if( *status == SAI__OK ) {
                  job->waiting_on[ ijob2 ] = job2;
               }

/* Add the new job to the list of jobs that are held up by job2. */
               ijob2 = job2->nheld_up++;
               job2->held_up = astGrow( job2->held_up, job2->nheld_up,
                                        sizeof( *job2->held_up ) );
               if( *status == SAI__OK ) {
                  job2->held_up[ ijob2 ] = job;
               }
            }
         }
      }

/* Add the job to the foot of either the available job list or the
   waiting job list. As each worker thread becomes idle, it will take the
   next job at the head of the available list. When a job finishes, any
   jobs held up waiting for the job will be moved from the waiting list to
   the available list (if they are not also waiting on any other jobs). */
      if( job->nwaiting_on > 0 ) {
         smf_push_list_foot( job, &(workforce->waiting_jobs), status );
         smf_thread_log( "add_job: Pushed job onto waiting jobs list",
                         DESK, job->ijob );
      } else {
         smf_push_list_foot( job, &(workforce->available_jobs), status );
         smf_thread_log( "add_job: Pushed job onto available jobs list",
                         DESK, job->ijob );

/* Tell any idle workers that a new job is available. */
         smf_cond_signal( &(workforce->page), status );
         smf_thread_log( "add_job: Paged idle workers", DESK, job->ijob );
      }
   }

/* We can now leave the job desk, allowing the next thread in the job desk
   queue to access the workforce data. */
   smf_mutex_unlock( &(workforce->jd_mutex), status );
   smf_thread_log( "add_job: Job added", ACTIVE, job->ijob );

/* Return the job identifier. */
   return job->ijob;
}

smfWorkForce *smf_create_workforce( int nworker, int *status ) {
/*
*  Name:
*     smf_create_workforce

*  Purpose:
*     Create a thread pool holding a specified number of threads.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     smfWorkForce *smf_create_workforce( int nworker, int *status )

*  Arguments:
*     nworker
*        The number of threads within the new thread pool.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     A pointer to a structure describing the new thread pool. The
*     returned pool should be freed using smf_destroy_workforce when
*     no longer needed.

*  Description:
*     This function creates a new "workforce" - a pool of threads that
*     can be used to execute tasks in parallel. Each task should be
*     split into two or more jobs, and a description of each job should
*     be given to the workforce using smf_add_job. As each job is added,
*     any available worker thread claims the job and executes it. If all
*     workers are busy, the jobs will be claimed by workers once they
*     have completed their current jobs. Once all jobs have been added,
*     smf_wait should be called to wait until all jobs have ben completed.
*
*     The model used by this module is of a group of people (each person
*     representing a thread), most of which are "workers" whose duty it
*     is to collect jobs from a "job desk" and then go away and perform
*     them, returning to the job desk to report completion of the job
*     and to get a new job. The lists of available jobs, active jobs,
*     etc, are kept at the job desk, and everyone must queue at the job
*     desk to gain access to this information. In order to prevent
*     confusion being caused by different people acccessing the job desk
*     information at the same time, only the person at the head of the
*     queue can access these lists (stored in the workforce structure
*     in reality). This job desk queue is implemented using a mutex - a
*     thread (person) joins the queue by attempting to lock the mutex. At
*     this point the thread blocks (the person waits) until they have
*     reached the head of the queue as indicated by the smf_lock_mutex call
*     returning. [In fact the queue is not guaranteed to be first-in,
*     first-out - some "queue jumping" may occur as determined by the
*     thread scheduler - but that shouldn't matter.]
*
*     Two condition variables are used; one is used to signal that new
*     jobs have been placed on the job desk (any idle workers will
*     respond to this signal by rejoining the job desk queue), and the
*     other is used to signal that all jobs have been completed (i.e.
*     no jobs waiting to be started and no active jobs). The person
*     (thread) who added the jobs to the table will respond to this
*     signal by waking up and continuing with whetever else it has to
*     do (which may include submitting more jobs to the job desk).

*/

/* Local Variables: */
   smfWorkForce *result = NULL;
   int i;
   pthread_t thread_id;
   char *logfile;

/* Check in herited status */
   if( *status != SAI__OK ) return result;

/* If required, open a log file to receive output from smf_thread_log. */
   logfile = getenv( "SMURF_THREAD_LOG" );
   if( logfile ) {
      if( !strcmp( logfile, "<stdout>" ) ) {
         fd = stdout;
      } else {
         fd = fopen( logfile, "w" );
      }
   } else {
      fd = NULL;
   }

/* Allocate memory for the workforce description. */
   result = astMalloc( sizeof( smfWorkForce ) );
   if( result ) {

/* Initialise things. */
      result->nworker = nworker;
      result->active_jobs = NULL;
      result->free_jobs = NULL;
      result->finished_jobs = NULL;
      result->available_jobs = NULL;
      result->waiting_jobs = NULL;
      result->kill = 0;
      result->ncontext = 0;
      result->contexts = NULL;
      result->condepth = 0;
      smf_cond_init( &( result->all_done ), status );
      smf_cond_init( &( result->job_done ), status );
      smf_cond_init( &( result->page ), status );
      smf_mutex_init( &( result->jd_mutex ), status );
      result->status = NULL;

/* Create the threads to host the workers. Each thread remains alive
   until the workforce is destroyed. During its life, the thread
   loops round executing jobs off the workforce's available job list. */
      for( i = 0; i < nworker; i++ ) {
         smf_thread_create( &thread_id, smf_run_worker, result, status );
      }
   }

/* Destroy the workforce if anything went wrong. */
   if( *status != SAI__OK ) result = smf_destroy_workforce( result );

/* Return the workforce pointer. */
   return result;
}

smfWorkForce *smf_destroy_workforce( smfWorkForce *workforce ) {
/*
*  Name:
*     smf_destroy_workforce

*  Purpose:
*     Destroy a workforce

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     smfWorkForce *smf_destroy_workforce( smfWorkForce *workforce )

*  Arguments:
*     workforce
*        Pointer to the workforce to be destroyed. If NULL is supplied,
*        this function returns without action.

*  Returned Value:
*     A NULL pointer is returned.

*  Description:
*     This function frees all resources used by a work force. This
*     includes cancelling the worker threads, and freeing memory
*     structures. The calling thread blocks until any busy workers have
*     completed their jobs. The worker threads themselves are then
*     terminated.

*/

/* Local Variables: */
   smfJob *job;
   int status = SAI__OK;
   int nloop;

/* Check a workforce was supplied. */
   if( workforce ) {

/* Wait until we have exclusive access to the job desk. */
      pthread_mutex_lock( &( workforce->jd_mutex ) );

/* Set a flag indicating that the workers should terminate, and then
   broadcast a signal requesting all idle workers to return to the job
   desk. */
      workforce->kill = workforce->nworker;
      pthread_cond_broadcast( &(workforce->page) );

/* Now wait until all workers have terminated. Spurious wake-ups can occur
   so put this in a loop. The call to pthread_cond_wait will release the
   specified mutex (the job desk mutex) before blocking this thread. This
   enables worker threads to access the job deks to report completion of
   jobs, etc. */
      nloop = 0;
      while( workforce->kill && ++nloop < 1000 &&
             workforce->status == SAI__OK ){
         pthread_cond_wait( &( workforce->all_done ),
                            &( workforce->jd_mutex ) );
      }

/* Free the job description structures. */
      job = smf_pop_list_head( &(workforce->active_jobs), &status );
      while( job ) {
         job = smf_free_job( job );
         job = smf_pop_list_head( &(workforce->active_jobs), &status );
      }

      job = smf_pop_list_head( &(workforce->free_jobs), &status );
      while( job ) {
         job = smf_free_job( job );
         job = smf_pop_list_head( &(workforce->free_jobs), &status );
      }

      job = smf_pop_list_head( &(workforce->available_jobs), &status );
      while( job ) {
         job = smf_free_job( job );
         job = smf_pop_list_head( &(workforce->available_jobs), &status );
      }

      job = smf_pop_list_head( &(workforce->finished_jobs), &status );
      while( job ) {
         job = smf_free_job( job );
         job = smf_pop_list_head( &(workforce->finished_jobs), &status );
      }

      job = smf_pop_list_head( &(workforce->waiting_jobs), &status );
      while( job ) {
         job = smf_free_job( job );
         job = smf_pop_list_head( &(workforce->waiting_jobs), &status );
      }

/* Unlock the job desk mutex prior to dstroying it. */
      pthread_mutex_unlock( &( workforce->jd_mutex ) );

/* Free the mutex and condition variables used by the workforce. */
      pthread_mutex_destroy( &( workforce->jd_mutex ) );
      pthread_cond_destroy( &( workforce->all_done ) );
      pthread_cond_destroy( &( workforce->job_done ) );
      pthread_cond_destroy( &( workforce->page ) );

/* Free the list of job context identifiers. */
      workforce->contexts = astFree( workforce->contexts );

/* Free the workforce status structure. */
      workforce->status = smf_free_status( workforce->status );

/* Free the memory holding the workforce. */
      workforce = astFree( workforce );
   }

/* Close any log file. */
   if( fd ) fclose( fd );

   return NULL;
}

void smf_end_job_context( smfWorkForce *workforce, int *status ){
/*
*  Name:
*     smf_end_job_context

*  Purpose:
*     End the current job context.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void smf_end_job_context( smfWorkForce *workforce, int *status )

*  Arguments:
*     workforce
*        Pointer to the workforce performing the jobs. If NULL is
*        supplied, this function returns without action.
*     status
*        Pointer to the inherited status value.

*  Description:
*     This function ends the job context started by the earlier matching
*     call to smf_begin_job_context. Any remaining jobs belonging to the
*     current job context are exported into the parent job context.

*/

/* Local Variables: */
   int old;
   int new;

/* Check inherited status */
   if( *status != SAI__OK || !workforce ) return;

/* Wait in the job desk queue until we have exclusive access to the job
   desk. */
   smf_mutex_lock( &( workforce->jd_mutex ), status );

/* Decrement the depth of context nesting. */
   (workforce->condepth)--;

/* Report an error if the context depth goes negative. */
   if( workforce->condepth < 0 ) {
      *status = SAI__ERROR;
      emsRep( "", "smf_end_job_context: No matching call to smf_begin_job_context.",
              status );

/* Otherwise, export any remaining jobs created in the current context into the parent
   context. */
   } else {

/* Get the identifiers for the old and new job contexts. */
      old = (workforce->contexts)[ workforce->condepth ];
      new = smf_get_job_context( workforce, status );

/* Search for jobs in the old context and assign them to the new context. */
      smf_export_jobs( workforce->available_jobs, old, new, status );
      smf_export_jobs( workforce->waiting_jobs, old, new, status );
      smf_export_jobs( workforce->active_jobs, old, new, status );
      smf_export_jobs( workforce->finished_jobs, old, new, status );

/* For safety, zero the last entry in the list of context identifiers in the
   workforce. */
      (workforce->contexts)[ workforce->condepth ] = 0;
   }

/* Unlock the mutex so that the next thread can access the job desk. */
   smf_mutex_unlock( &( workforce->jd_mutex ), status );
}

void *smf_get_job_data( int ijob, smfWorkForce *workforce, int *status ){
/*
*  Name:
*     smf_get_job_data

*  Purpose:
*     Returns a job data pointer that was supplied when the job was created.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void *smf_get_job_data( int ijob, smfWorkForce *workforce, int *status )

*  Arguments:
*     ijob
*        Identifier for the job.
*     workforce
*        Pointer to the workforce performing the jobs. If NULL is
*        supplied, this function returns a NULL pointer, and "ijob" is
*        ignored.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     The pointer to the job data.

*  Description:
*     This function returns the pointer that was supplied as argument
*     "data" when smf_add_job was called to create the specified job.

*/

/* Local Variables: */
   smfJob *job;

/* Check inherited status */
   if( *status != SAI__OK || !workforce ) return NULL;

/* Get a pointer to the structure describing the job. Search each
   list of jobs in turn. Report an error if the job is not found.
   First see if the requested job is the one being checked by the
   active worker. */
   job = smf_find_job( workforce->available_jobs, ijob, -1, status );
   if( !job ) job = smf_find_job( workforce->finished_jobs, ijob, -1, status );
   if( !job ) job = smf_find_job( workforce->waiting_jobs, ijob, -1, status );
   if( !job ) job = smf_find_job( workforce->active_jobs, ijob, -1, status );
   if( !job && *status == SAI__OK ) {
      *status = SAI__ERROR;
      emsSeti( "I", ijob );
      emsRep( "", "smf_get_job_data: No job with given 'job' identifier "
              "(^I) found.", status );
   }

/* Return the required pointer. */
   return ( *status == SAI__OK ) ? job->data : NULL;
}

void smf_begin_job_context( smfWorkForce *workforce, int *status ){
/*
*  Name:
*     smf_job_context

*  Purpose:
*     Starts a new job context.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void smf_begin_job_context( smfWorkForce *workforce, int *status )

*  Arguments:
*     workforce
*        Pointer to the workforce performing the jobs. If NULL is
*        supplied, this function returns without action.
*     status
*        Pointer to the inherited status value.

*  Description:
*     This function indicates that all jobs created before the subsequent matching
*     call to smf_end_job_context should be grouped together. This affects the
*     behaviour of functions smf_wait and smf_job_wait.

*/

/* Check inherited status */
   if( *status != SAI__OK || !workforce ) return;

/* Increment the depth of context nesting. */
   (workforce->condepth)++;

/* Ensure the array of context identifiers held in the workforce is large enough. */
   workforce->contexts = astGrow( workforce->contexts, workforce->condepth, sizeof( int ) );

/* Create a new identifier for the new context and store it as the last entry in the list
   of context identifiers in the workforce. */
   (workforce->contexts)[ workforce->condepth-1 ] = (workforce->ncontext)++;
}

int smf_job_wait( smfWorkForce *workforce, int *status ) {
/*
*  Name:
*     smf_job_wait

*  Purpose:
*     Wait for the next job to completed.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     int smf_job_wait( smfWorkForce *workforce, int *status )

*  Arguments:
*     workforce
*        Pointer to the workforce. If NULL is supplied, this function
*        exits immediately, returning a value of zero.
*     status
*        Pointer to the inherited status value.

*  Description:
*     Each consecutive call to this function return the integer identifier
*     for a completed job, in the order in which they are completed. If all
*     completed jobs have already been reported, then this function
*     blocks until the next job is completed.
*
*     Note, only jobs which had the SMF__REPORT_JOB flag set when calling
*     smf_add_job and were created within the current job context (see
*     smf_begin_job_context) are included in the list of returned jobs.

*  Returned Value:
*     The integer identifier for the completed job. This can be compared
*     with the job identifiers returned by smf_add_job to determine which
*     job has finished.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.

*/

/* Local Variables: */
   int conid;
   int result = 0;
   smfJob *job;
   smfJobStatus *job_status = NULL;

/* Check we have a non-NULL workforce pointer. */
   if( !workforce ) return result;

/* Start a new error reporting context. */
   emsBegin( status );

/* Wait in the job desk queue until we have exclusive access to the job
   desk. */
   smf_thread_log( "job_wait: Joining queue", WAIT, -1 );
   smf_mutex_lock( &( workforce->jd_mutex ), status );
   smf_thread_log( "job_wait: At desk", DESK, -1 );

/* Get the current job context identifier. */
   conid = smf_get_job_context( workforce, status );

/* If there are no jobs within the current job context on the finished, active
   or available job lists, report an error. */
   if( smf_list_isempty( conid, workforce->finished_jobs, status ) &&
       smf_list_isempty( conid, workforce->active_jobs, status ) &&
       smf_list_isempty( conid, workforce->available_jobs, status ) ) {
      *status = SAI__ERROR;
      emsRep( "", "smf_job_wait: There are no jobs to wait for.", status );
   }

/* Wait until there is a job within the current context available on the finished
   jobs list. Put it in a loop because of spurious wake ups. */
   while( smf_list_isempty( conid, workforce->finished_jobs, status ) &&
          *status == SAI__OK ){
      smf_thread_log( "job_wait: Waiting for a job (any job) to complete", WAIT, -1 );
      smf_cond_wait( &( workforce->job_done ), &( workforce->jd_mutex ),
                     status );
   }

/* Pop the first job for the current context off the finished list, and note its
   identifier. */
   job = smf_pop_list_first( &(workforce->finished_jobs), conid, status );
   if( job ) {
      result = job->ijob;
      smf_thread_log( "job_wait: Job completed", DESK, result );

/* Copy the details of any errors that occurred during the job to a new
   structure. We do not yet report those errors using EMS as that would
   cause remaining functions called within this function to return
   without action. */
      job_status = smf_copy_status( job->status );

/* Clear the job data, and put the job structure onto the list of free
   job structures. */
      smf_init_job( job, status );
      smf_push_list_head( job, &(workforce->free_jobs), status );
   }

/* Unlock the mutex so that the next thread can access the job desk. */
   smf_mutex_unlock( &( workforce->jd_mutex ), status );
   smf_thread_log( "job_wait: Left desk", ACTIVE, result );

/* Report errors using EMS if any errors occurred during the job. */
   job_status = smf_report_status( job_status, status );

/* End the error reporting context. */
   emsEnd( status );

/* Return the job identifier. */
   return result;
}

void smf_wait( smfWorkForce *workforce, int *status ) {
/*
*  Name:
*     smf_wait

*  Purpose:
*     Wait for a workforce to have completed all its jobs.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     smf_wait( smfWorkForce *workforce, int *status )

*  Arguments:
*     workforce
*        Pointer to the workforce. If NULL is supplied, this function
*        returns immediately.
*     status
*        Pointer to the inherited status value.

*  Description:
*     This function blocks the calling thread until all jobs within the
*     current job context (see smf_begin_job_context) have been completed.
*
*     A side effect of this function is to empty the list of jobs waiting
*     to be reported by smf_job_wait. Upon exit from this function, all
*     jobs waiting to be reported via smf_job_wait will be considered to
*     have been reported (again, this only affects jobs within the current
*     job context).

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.

*/

/* Local Variables: */
   int conid;
   smfJob *job;
   smfJob *new_finished_head;
   smfJobStatus *wf_status = NULL;

/* Check we have a non-NULL workforce pointer. */
   if( !workforce ) return;

/* Start a new error reporting context. */
   emsBegin( status );

/* Wait in the job desk queue until we have exclusive access to the job
   desk. */
   smf_thread_log( "wait: Joining queue", WAIT, njob );
   smf_mutex_lock( &( workforce->jd_mutex ), status );
   smf_thread_log( "wait: At desk", DESK, njob );

/* Get the current job context identifier. */
   conid = smf_get_job_context( workforce, status );

/* If there are any jobs within the current job context available to run,
   or any currently active jobs, wait until the last job completes.
   Spurious wake-ups can occur so put this in a loop. The call to
   pthread_cond_wait (within smf_cond_wait) will release the specified
   mutex (the job desk mutex) before blocking this thread. This enables
   worker threads to access the job desk to report completion of jobs
   and to get new jobs. */
   while( ( !smf_list_isempty( conid, workforce->available_jobs, status ) ||
            !smf_list_isempty( conid, workforce->active_jobs, status ) ) &&
          *status == SAI__OK ) {
      smf_thread_log( "wait: waiting for all done", WAIT, njob );
      smf_cond_wait( &( workforce->all_done ), &( workforce->jd_mutex ),
                     status );
   }

   smf_thread_log( "wait: all done", DESK, njob );

/* Remove jobs for the current job context from the finished job list,
   re-initialising their contents and moving them onto the free list. */
   new_finished_head = NULL;
   job = smf_pop_list_head( &(workforce->finished_jobs), status );
   while( job ) {
      if( job->conid == conid ) {
         smf_init_job( job, status );
         smf_push_list_head( job, &(workforce->free_jobs), status );
      } else {
         smf_push_list_foot( job, &new_finished_head, status );
      }
      job = smf_pop_list_head( &(workforce->finished_jobs), status );
   }

/* Restore the head of the list of remaining finished jobs (i.e. finished
   jobs that do not belong to the current job context). */
   workforce->finished_jobs = new_finished_head;

/* If an error occurred whilst executing any of the jobs in the current
   context, the details of the first such error will be stored in the
   workforce status. Copy the details of any such errors to a new
   structure. We do not yet report those errors using EMS as that would
   cause remaining functions called within this function to return
   without action. */
   wf_status = smf_copy_status( workforce->status );

/* Clear the workforce status so that the workforce can be re-used, even
   if an error occurred in the current job context. */
   smf_clear_status( workforce->status );

/* Once the condition is signalled, the pthread_cond_wait will continue
   to wait until it can lock the specified mutex (the job desk mutex). So
   unlock it now so that other threads can get to the job desk. */
   smf_mutex_unlock( &( workforce->jd_mutex ), status );
   smf_thread_log( "wait: Leaving desk", ACTIVE, njob );

/* Report errors using EMS if any jobs failed. */
   wf_status = smf_report_status( wf_status, status );

/* End the outer error reporting context. */
   emsEnd( status );
}

/* Private workforce-related functions */
/* ----------------------------------- */
static void smf_clear_status( smfJobStatus *status ){
/*
*  Name:
*     smf_clear_status

*  Purpose:
*     Resets a smfJobStatus structure to indicate no error.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void smf_clear_status( smfJobStatus *status )

*  Arguments:
*     status
*        Pointer to the structure describing the job status. May be NULL.

*  Description:
*     This resets all fields of the supplied structure to indicate that
*     no error has occurred.

*/
   int i;

   if( status ) {
      status->ems_status = SAI__OK;
      for( i = 0; i < status->nmessage; i++ ) {
         (status->messages)[ i ] = astFree( (status->messages)[ i ] );
      }
      status->messages = astFree( status->messages );
      status->nmessage = 0;
   }
}

static smfJobStatus *smf_copy_status( smfJobStatus *status ){
/*
*  Name:
*     smf_copy_status

*  Purpose:
*     Copies the details from an existing smfJobStatus structure to a new
*     one.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     smfJobStatus *smf_copy_status( smfJobStatus *status )

*  Arguments:
*     status
*        Pointer to the structure holding the job status to retrieve.

*  Returned Value:
*     A pointer to a new smfJobStatus structure, or NULL if "status" is
*     NULL.

*  Description:
*     This function returns a new smfJobStatus structure holding a copy
*     of the information in the supplied smfJobStatus structure.

*/

/* Local Variables: */
   const char *mess;
   int i;
   smfJobStatus *result;

/* Check we have a usable pointer. */
   if( !status ) return NULL;

/* Create a new smfJobStatus structure. */
   result = smf_make_status();

/* Copy details from the supplied smfJobStatus. */
   result->ems_status = status->ems_status;
   result->messages = astMalloc( sizeof( char *)*status->nmessage );
   if( astOK ) {
      result->nmessage = status->nmessage;
      for( i = 0; i < status->nmessage; i++ ) {
         mess = (status->messages)[ i ];
         if( mess ) {
            (result->messages)[ i ] = astStore( NULL, mess, strlen( mess ) + 1 );
         } else {
            (result->messages)[ i ] = NULL;
         }
      }
   }

/* Free the new structure. */
   return result;
}

static void smf_export_jobs( smfJob *head, int old, int new, int *status ){
/*
*  Name:
*     smf_export_jobs

*  Purpose:
*     Search for jobs belonging to the old context and assign them to the new context.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void smf_export_jobs( smfJob *head, int old, int new, int *status )

*  Arguments:
*     head
*        Pointer to the job at the head of the list to be searched.
*     old
*        Identifier for the old context.
*     new
*        Identifier for the new context.
*     status
*        Pointer to the inherited status value.

*  Description:
*     This function searches the given lists of jobs for jobs belonging to the
*     old context and assigns them to the new context.

*/

/* Local Variables; */
   smfJob *job;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Check the supplied head job first. */
   job = head;

/* Loop round all jobs in the list. */
   while( job ) {

/* If the job belongs to the old context, assign it to the new context. */
      if( job->conid == old ) job->conid = new;

/* Move on to the next job in the list. */
      job = job->next;

/* If we are back at the head, leave the loop. */
      if( job == head ) break;
   }
}

static smfJob *smf_find_job( smfJob *head, int ijob, int conid, int *status ){
/*
*  Name:
*     smf_find_job

*  Purpose:
*     Find the job that has a given ideentifier and return a pointer to it.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     smfJob *smf_find_job( smfJob *head, int ijob, int conid, int *status )

*  Arguments:
*     head
*        Pointer to the job at the head of the list to be searched.
*     ijob
*        The integer identifier for the job. Ignored if negative.
*     conid
*        The job context identifier for the job. Ignored if negative.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     Pointer to the smfJob that has the given identifier, or NULL if no
*     job is found.

*  Description:
*     This function searches the given lists of jobs for a job that has a
*     given identifier and a given context. If found, a pointer to the job
*     structure is returned. Otherwise a NULL pointer is returned (without
*     error).

*/

/* Local Variables; */
   smfJob *result = NULL;
   int ok;

/* Check inherited status and check list is not empty.*/
   if( *status != SAI__OK || !head ) return result;

/* Check the supplied head job first. */
   result = head;

/* Loop until we find a job with the given identifier. */
   while( result ) {

/* Break if the current job has the right identifier and context. */
      ok = 1;
      if( ijob >= 0 && result->ijob != ijob ) ok = 0;
      if( conid >= 0 && result->conid != conid ) ok = 0;
      if( ok ) break;

/* Move on to the next job in the list. */
      result = result->next;

/* If we are back at the head, set the returned pointer to NULL and
   leave the loop. */
      if( result == head ) {
         result = NULL;
         break;
      }
   }

/* Return the pointer to the job structure. */
   return result;
}

static smfJob *smf_free_job( smfJob *job ) {
/*
*  Name:
*     smf_free_job

*  Purpose:
*     Release the resources used by a job.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     smfJob *smf_free_job( smfJob *job )

*  Description:
*     This releases the memory used to hold a job description.

*/
   if( job ) {
      job->status = smf_free_status( job->status );
      job->waiting_on = astFree( job->waiting_on );
      job->nwaiting_on = 0;
      job->held_up = astFree( job->held_up );
      job->nheld_up = 0;
      job = astFree( job );
   }
   return job;
}

static smfJobStatus *smf_free_status( smfJobStatus *status ){
/*
*  Name:
*     smf_free_status

*  Purpose:
*     Free the memory used by a smfJobStatus structure.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     smfJobStatus *smf_free_status( smfJobStatus *status )

*  Description:
*     This frees the memory used by a smfJobStatus structure.

*/
   smf_clear_status( status );
   return astFree( status );
}

static int smf_get_job_context( smfWorkForce *workforce, int *status ){
/*
*  Name:
*     smf_get_job_context

*  Purpose:
*     Get the identifier for the current job context from a workforce.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     int smf_get_job_context( smfWorkForce *workforce, int *status )

*  Arguments:
*     workforce
*        Pointer to the workforce.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     An integer identifying the current job context from a workforce.

*  Description:
*     This function returns an integer identifying the current job
*     context from a workforce.

*/

/* Local Variables: */
   int result;

/* Initialise */
   result = -1;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Get the identifier for the current job context. */
   if( workforce->condepth > 0 ) {
      result = (workforce->contexts)[ workforce->condepth - 1 ];
   }

/* Return the result. */
   return result;
}

static smfJobStatus *smf_get_status( int *ems_status ){
/*
*  Name:
*     smf_get_status

*  Purpose:
*     Creates a new smfJobStatus structure holding details of the current
*     EMS error.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     smfJobStatus *smf_get_status( int *ems_status )

*  Arguments:
*     ems_status
*        The EMS status value. Always returned equal to SAI__OK.

*  Returned Value:
*     If an EMS error exists on exntry, a pointer to a new smfJobStatus
*     holding details of the EMS error is returned. Otherwise, a NULL pointer
*     is returned.

*  Description:
*     If the current EMS status value is not SAI__OK, the current EMS status
*     value and message stack are copied into a new smfJobStatus structure,
*     and the EMS error context is then annulled.

*/

/* Local Variables: */
   char opstr[ EMS__SZMSG + 1 ]; /* Buffer for expanded message text */
   char param[ EMS__SZMSG + 1 ]; /* Buffer for parameter name */
   int *old_status;              /* Pointer to previous AST status value */
   int ast_status;               /* The AST status value */
   int oplen;                    /* Used length of string in opstr */
   int parlen;                   /* Used length of string in param */
   smfJobStatus *result = NULL;  /* Returned pointer */

/* Return without further action if no error has been reported using EMS. */
   if( *ems_status != SAI__OK ) {

/* Tell AST to use a temporary status value - otherwise all the AST
   memory management functions will return without action due to the EMS
   error status. */
      ast_status = SAI__OK;
      old_status = astWatch( &ast_status );

/* Create a new smfJobStatus. */
      result = smf_make_status();

/* Store the EMS status value in the smfJobStatus structure. */
      result->ems_status = *ems_status;

/* Get the first entry on the message stack. */
      emsEload( param, &parlen, opstr, &oplen, ems_status );

/* Loop round all the messages in the current EMS error context (this
   also annuls the context). */
      while( *ems_status != SAI__OK ) {

/* Append the opstr string to the list of messages in the smfJobStatus
   structure. */
         result->messages = astGrow( result->messages, result->nmessage + 1,
                                     sizeof( char * ) );
         if( astOK ) {
            (result->messages)[ (result->nmessage)++ ] = astStore( NULL,
                                                            opstr, oplen + 1 );
         }

/* Get the next entry on the message stack. */
         emsEload( param, &parlen, opstr, &oplen, ems_status );
      }

/* Re-instate the original AST status value. */
      astWatch( old_status );
   }

/* Return the new smfJobStatus pointer. */
   return result;
}

static void smf_init_job( smfJob *job, int *status ){
/*
*  Name:
*     smf_init_job

*  Purpose:
*     Initialise fields of a Job structure.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void smf_init_job( smfJob *job, int *status )

*  Arguments:
*     job
*        Pointer to the smfJob to be initialised.
*     status
*        Pointer to the inherited status value.

*  Description:
*     This function initialises the supplied job to hold null values.

*/

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Initialise the field of the job structure to null values. */
   job->conid = -1;
   job->prev = NULL;
   job->next = NULL;
   job->waiting_on = NULL;
   job->held_up = NULL;
   job->ijob = 0;
   job->flags = 0;
   job->func = NULL;
   job->data = NULL;
   job->nwaiting_on = 0;
   job->nheld_up = 0;
   job->status = smf_free_status( job->status );
}

static int smf_list_isempty( int conid, smfJob *head, int *status ){
/*
*  Name:
*     smf_list_isempty

*  Purpose:
*     Determines if the given linked list of jobs have any jobs in the specified context.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     iint smf_list_isempty( int conid, smfJob *head, int *status )

*  Arguments:
*     conid
*        The identifier for the job context to be searched for.
*     head
*        Pointer to the job at the head of the linked list of jobs to be searched.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     Non-zero if the list contains no jobs in the specified context.

*  Description:
*     This function returns an integer indicating if the supplied list of jobs is
*     devoid of any jobs belonging to a specified job context.

*/

/* Local Variables: */
   int result;
   smfJob *job;

/* Initialise */
   result = 1;

/* Check inherited status */
   if( *status != SAI__OK || !head ) return result;

/* Check the supplied head job. */
   if( head->conid == conid ) {
      result = 0;

/* Otherwise, check all the other jobs in the list. */
   } else {
      job = head->next;
      while( job != head ) {
         if( job->conid == conid ) {
            result = 0;
            break;
         }
         job = job->next;
      }
   }

/* Return the result. */
   return result;
}

static smfJobStatus *smf_make_status( void ){
/*
*  Name:
*     smf_make_status

*  Purpose:
*     Create a smfJobStatus structure and initialise it.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     smfJobStatus *smf_make_status( void )

*  Description:
*     This creates a new smfJobStatus structure and sets all its fields
*     to indicate that no error has occurred.

*/
   smfJobStatus *status = astMalloc( sizeof( *status ) );
   if( status ) {
      status->ems_status = SAI__OK;
      status->messages = NULL;
      status->nmessage = 0;
   }
   return status;
}

static smfJob *smf_pop_list_first( smfJob **head, int conid, int *status ){
/*
*  Name:
*     smf_pop_list_first

*  Purpose:
*     Return the first job that is in a given context and remove it from the list.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     smfJob *smf_pop_list_first( smfJob **head, int conid, int *status )

*  Arguments:
*     head
*        Address of a location at which is stored a pointer to the smfJob at
*        the head of the list.
*     conid
*        The identifier for the required job context.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     Pointer to the first smfJob that was in the required context.

*  Description:
*     This function returns a pointer to the job closest to the head of a list
*     that is in the requested job context, and removes the job from the list.

*  Notes:
*     - The "prev" link in a smfJob structure points towards the list
*     foot, and the "next" link points towards the list head.
*     - The head's next link points to the foot.
*     - The foot's prev link points to the head.

*/

/* Local Variables; */
   smfJob *result;
   smfJob *next;
   smfJob *prev;

/* Initialise. */
   result = NULL;

/* Check inherited status */
   if( *status != SAI__OK || ! *head ) return result;

/* First check the supplied head job. */
   if( (*head)->conid == conid ) {
      result = *head;

/* Othersise, check the other jobs in the list. */
   } else {
      result = (*head)->prev;
      while( result != *head ) {
         if( result->conid == conid ) break;
         result = result->prev;
      }
      if( result == *head ) result = NULL;
   }

/* If we have found a job in the requested context, extract it
   from the list and close up the dangling links. Update the head
   job if the returned job is the original head job. */
   if( result ) {
      prev = result->prev;
      next = result->next;
      if( prev == result ) prev = NULL;
      if( next == result ) next = NULL;
      if( result == *head ) *head = prev;
      if( prev ) prev->next = next;
      if( next ) next->prev = prev;
      result->prev = NULL;
      result->next = NULL;
   }

   return result;
}

static smfJob *smf_pop_list_head( smfJob **head, int *status ){
/*
*  Name:
*     smf_pop_list_head

*  Purpose:
*     Return the job at the head of a list and remove it from the list.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     smfJob *smf_pop_list_head( smfJob **head, int *status ){

*  Arguments:
*     head
*        Address of a location at which is stored a pointer to the smfJob at
*        the head of the list.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     Pointer to the smfJob that was at the head of the list on entry.

*  Description:
*     This function returns a pointer to the job at the head of a list,
*     and removes the job from the list.

*  Notes:
*     - The "prev" link in a smfJob structure points towards the list
*     foot, and the "next" link points towards the list head.
*     - The head's next link points to the foot.
*     - The foot's prev link points to the head.

*/

/* Local Variables; */
   smfJob *result;
   smfJob *next;
   smfJob *prev;

/* Check inherited status */
   if( *status != SAI__OK ) return NULL;

   result = *head;
   if( result ) {
      prev = result->prev;   /* Becomes new head */
      next = result->next;   /* Is the foot */

      if( prev == result ) {
         *head = NULL;
      } else {
         prev->next = next;
         next->prev = prev;
         *head = prev;
      }

      result->prev = NULL;
      result->next = NULL;
   }

   return result;
}

static void smf_push_list_foot( smfJob *job, smfJob **head, int *status ){
/*
*  Name:
*     smf_push_list_foot

*  Purpose:
*     Push a new job onto the foot of a list.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void smf_push_list_foot( smfJob *job, smfJob **head, int *status )

*  Arguments:
*     job
*        Pointer to the smfJob to be added.
*     head
*        Address of a location at which is stored a pointer to the smfJob at
*        the head of the list.
*     status
*        Pointer to the inherited status value.

*  Description:
*     This function adds a new job to the foot of a list.


*  Notes:
*     - The "prev" link in a smfJob structure points towards the list
*     foot, and the "next" link points towards the list head.
*     - The head's next link points to the foot.
*     - The foot's prev link points to the head.
*/

/* Local Variables: */
   smfJob *foot;

/* Check inherited status */
   if( *status != SAI__OK ) return;

   if( *head ) {
      foot = (*head)->next;
      foot->prev = job;
      job->next = foot;
      (*head)->next = job;
      job->prev = *head;

   } else {
      job->next = job;
      job->prev = job;
      *head = job;
   }
}

static void smf_push_list_head( smfJob *job, smfJob **head, int *status ){
/*
*  Name:
*     smf_push_list_head

*  Purpose:
*     Push a new job onto the head of a list.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void smf_push_list_head( smfJob *job, smfJob **head, int *status )

*  Arguments:
*     job
*        Pointer to the smfJob to be added.
*     head
*        Address of a location at which is stored a pointer to the smfJob at
*        the head of the list.
*     status
*        Pointer to the inherited status value.

*  Description:
*     This function adds a new job to the head of a list.

*  Notes:
*     - The "prev" link in a smfJob structure points towards the list
*     foot, and the "next" link points towards the list head.
*     - The head's next link points to the foot.
*     - The foot's prev link points to the head.
*/

/* Local Variables: */
   smfJob *foot;

/* Check inherited status */
   if( *status != SAI__OK ) return;

   if( *head ) {
      foot = (*head)->next;
      foot->prev = job;
      job->next = foot;
      (*head)->next = job;
      job->prev = *head;

   } else {
      job->next = job;
      job->prev = job;
   }
   *head = job;
}

static void smf_remove_from_list( smfJob *job, smfJob **head, int *status ){
/*
*  Name:
*     smf_remove_from_list

*  Purpose:
*     Remove a job from a list.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void smf_remove_from_list( smfJob *job, smfJob **head, int *status )

*  Arguments:
*     job
*        Pointer to the smfJob to be removed.
*     head
*        Address of a location at which is stored a pointer to the smfJob at
*        the head of the list.
*     status
*        Pointer to the inherited status value.

*  Description:
*     This function removes the supplied smfJob from the specified list.


*  Notes:
*     - The "prev" link in a smfJob structure points towards the list
*     foot, and the "next" link points towards the list head.
*     - The head's next link points to the foot.
*     - The foot's prev link points to the head.

*/

/* Local Variables: */
   smfJob *next;
   smfJob *prev;

/* Check inherited status */
   if( *status != SAI__OK ) return;

   prev = job->prev;
   next = job->next;

   if( prev == job ) {
      *head = NULL;
   } else {
      prev->next = next;
      next->prev = prev;
      if( job == *head ) *head = prev;
   }

   job->next = NULL;
   job->prev = NULL;
}

static smfJobStatus *smf_report_status( smfJobStatus *status, int *ems_status ){
/*
*  Name:
*     smf_report_status

*  Purpose:
*     Gets the status value and messages from the given smfJobStatus
*     and reports corresponding EMS errors.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     smfJobStatus *smf_report_status( smfJobStatus *status, int *ems_status )

*  Arguments:
*     status
*        Pointer to the structure holding the job status to retrieve.
*     ems_status
*        The inherited EMS status value.

*  Returned Value:
*     A NULL pointer.

*  Description:
*     If the supplied smfJobStatus describes an error, and the EMS error
*     status is currently SAI__OK, the error described in the smfJobStatus
*     is reported using EMS. The smfJobStatus is then freed, and a NULL
*     pointer returned.

*/

/* Local Variables: */
   int i;                        /* Loop count */

/* Check the current EMS error status, and check we have a usable pointer. */
   if( *ems_status == SAI__OK && status ) {

/* Do nothing if the supplied smfJobStatus does not describe an error. */
      if( status->ems_status != SAI__OK ) {

/* Set the EMS status value. */
         *ems_status = status->ems_status;

/* Loop round the error messagesstored in the smfJobSTatus, reporting
   each one in turn. */
         for( i = 0; i < status->nmessage; i++ ) {
            emsRep( " ", (status->messages)[ i ], ems_status );
         }

/* Clear the supplied structure. */
         smf_clear_status( status );
      }
   }

/* Free the status structure and return a NULL pointer. */
   return smf_free_status( status );
}

static void *smf_run_worker( void *wf_ptr ) {
/*
*  Name:
*     smf_run_worker

*  Purpose:
*     Manages the activity of a worker thread.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void *smf_run_worker( void *wf_ptr )

*  Arguments:
*     wf_ptr
*        Pointer to a data structure describing the work force.

*  Description:
*     This is the main function executed within each worker thread. Each
*     worker goes to the work desk to get a job from the list of
*     remaining jobs, and then executes that job, returning to the work
*     desk when complete to report completion and to get another job.

*/

/* Local Variables: */
   int i;
   int j;
   int jobs_available;
   int ready;
   int status;
   smfJob *job = NULL;
   smfJob *job2 = NULL;
   smfWorkForce *wf;

/* Initialise the local status value. */
   status = SAI__OK;

/* Get a pointer to the workforce description. */
   wf = (smfWorkForce *) wf_ptr;

/* Initialise AST's thread specific data by calling astBegin before any
   other AST functions. */
   astBegin;

/* Switch on AST memory caching for this thread. */
   (void) astTune( "MemoryCaching", 1 );

/* Tell AST to watch the local status variable. */
   astWatch( &status );

/* Join the end of the queue of workers at the job desk. This call will
   block until you reach the head of the queue. */
   smf_mutex_lock( &(wf->jd_mutex), &status );

/* Loop until an error occurs (errors that occur within jobs do not
   affect "status"). */
   while( status == SAI__OK ) {

/* You are now at the job desk and have exclusive access to all the
   information relating to the workforce. Note if there are any available
   jobs waiting to be run. */
      jobs_available = ( wf->available_jobs != NULL );

/* Report completion of any job that you have just completed. */
      if( job ) {
         smf_thread_log( "run_worker: Worker at desk to report job done",
                         DESK, job->ijob );

/* If the job failed, copy information about the error into the workforce
   so long as this is the first failed job. */
         if( !wf->status ) wf->status = smf_copy_status( job->status );

/* Remove the job from the list of currently active jobs. */
         smf_remove_from_list( job, &(wf->active_jobs), &status );

/* If any jobs are held up by the job that has just completed, see if any
   of them can now be moveed from the list of waiting jobs and put onto
   the list of available jobs. */
         for( i = 0; i < job->nheld_up; i++ ) {
            job2 = job->held_up[ i ];
            job->held_up[ i ] = NULL;

/* First remove the job that has just completed from the list of jobs
   which job2 is waiting for. Set a flag indicating if job2 is now ready
   to run (i.e. there are no other active or available jobs for which it is
   waiting). */
            ready = 1;
            for( j = 0; j < job2->nwaiting_on; j++ ) {
               if( job2->waiting_on[ j ] == job ) {
                  job2->waiting_on[ j ] = NULL;
               } else if( job2->waiting_on[ j ] != NULL ) {
                  ready = 0;
               }
            }

/* If job2 is now ready to run, move it off the waiting list onto the
   available list. */
            if( ready ) {
               smf_remove_from_list( job2, &(wf->waiting_jobs), &status );
               smf_push_list_head( job2, &(wf->available_jobs), &status );
            }
         }

/* If required, add the completed job onto the end of the "finished" list, and
   issue the job_done signal. */
         if( job->flags & SMF__REPORT_JOB ) {
            smf_push_list_foot( job, &(wf->finished_jobs), &status );
            smf_cond_signal( &(wf->job_done), &status );

/* Otherwise, clear the job data and put the job structure back onto the
   list of free job structures. */
         } else {
            job->ijob = 0;
            job->flags = 0;
            job->func = NULL;
            job->data = NULL;
            job->nwaiting_on = 0;
            job->nheld_up = 0;
            job->status = smf_free_status( job->status );
            smf_push_list_head( job, &(wf->free_jobs), &status );
         }

/* Indicate you now have no associated job. */
         job = NULL;
      }

/* If the workforce is being killed, decrement the number of workers that
   remain to be terminated. If this is  the last worker, signal the
   "all_done" condition. Then quit the main loop. */
      if( wf->kill ) {
         if( --(wf->kill) == 0 ) smf_cond_signal( &(wf->all_done), &status );
         break;
      }

/* Acquire a new job from the list of available jobs. */
      job = smf_pop_list_head( &(wf->available_jobs), &status );

/* If you now have a job, prepare to perform the job. */
      if( job ) {
         smf_thread_log( "run_worker: preparing to run job",
                         DESK, job->ijob );

/* If there were no available jobs when you arrived at the job desk, but
   there are now, page all idle workers to re-join the job desk queue. */
         if( !jobs_available && wf->available_jobs ) {
            smf_thread_log( "run_worker: paging idle workers",
                            DESK, job->ijob );
            smf_cond_broadcast( &(wf->page), &status );
         }

/* Add the job to the list of active jobs. */
         smf_push_list_head( job, &(wf->active_jobs), &status );

/* Leave the job desk queue, allowing the next worker to report a
   completed job and/or get a new job. */
         smf_mutex_unlock( &(wf->jd_mutex), &status );
         smf_thread_log( "run_worker: left desk to do job", ACTIVE,
                         job->ijob );

/* If no error has occurred, do the job. */
         if( status == SAI__OK ) {
            (*job->func)( job->data, &status );
            smf_thread_log( "run_worker: completed job - joining queue",
                            WAIT, job->ijob );

/* If the job failed, errors will have been reported using EMS. Copy
   details of these errors into the job structure, and annull the EMS error
   condition. If no error has occurred, a NULL pointer is returned by
   smf_get_status. */
            job->status = smf_get_status( &status );

/* Now the job is complete, re-join the end of the job desk queue to report
   its completion and get a new job. */
            smf_mutex_lock( &(wf->jd_mutex), &status );
         }

/* If you still have no job, it means that there are no jobs currently
   available for running. */
      } else {
         smf_thread_log( "run_worker: no job", DESK, -1 );

/* If there are no active or waiting jobs, signal the manager that the job
   list has been completed. */
         if( !wf->active_jobs ) {
            if( !wf->waiting_jobs ) {
               smf_thread_log( "run_worker: announcing all done", DESK, -1 );
               smf_cond_signal( &(wf->all_done), &status );

/* If there are no active jobs but there are still some waiting jobs,
   something has gone wrong. Probably the caller failed to register the
   jobs dependency on some other job, resulting in it never being moved from
   the waiting list to the available list. */
            } else if( status == SAI__OK ) {
               status = SAI__ERROR;
               emsRep( "", "smf_wait: waiting jobs gave no indication "
                       "of when they are to be run.", &status );
            }
         }

/* Now wait until a new job becomes available. This will release the job
   desk mutex, allowing other workers to report their completed jobs.
   When a new job becomes available (as indicated by the "page" condition
   being signalled to the waiting workers), at least one of the waiting
   workers will re-join the job desk queue, and the first will get the
   job. This function blocks until such time as you reach the head of the
   job desk queue and can claim a new job. */
         smf_thread_log( "run_worker: waiting for new jobs", WAIT, -1 );
         smf_cond_wait( &(wf->page), &(wf->jd_mutex), &status );
         smf_thread_log( "run_worker: new jobs!", DESK, -1 );
      }
   }

   smf_thread_log( "run_worker: worker has died", DESK, -1 );

/* If something goes wrong in the threads infrastructure, abort the current
   job by signalling "all done". */
   pthread_cond_signal( &(wf->all_done) );
   pthread_mutex_unlock( &(wf->jd_mutex) );

/* Switch off AST memory caching for this thread. */
   (void) astTune( "MemoryCaching", 0 );

/* End the AST context for this thread. */
   astEnd;

/* The pthreads library requires us to return a pointer. */
   return NULL;
}



/* Public pthreads wrapper functions */
/* --------------------------------- */

void smf_mutex_init( pthread_mutex_t *mutex, int *status ) {
/*
*  Name:
*     smf_mutex_init

*  Purpose:
*     A wrapper for pthread_mutex_init.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void smf_mutex_init( pthread_mutex_t *mutex, int *status )

*  Arguments:
*     mutex
*        The mutex to be initialised.
*     status
*        Pointer to the inherited status value.

*  Description:
*     This function initialises a mutex using default attributes.

*/

   if( *status != SAI__OK ) return;

   if( pthread_mutex_init( mutex, NULL ) ) {
      *status = SAI__ERROR;
      emsRep( "", "Failed to initialise a pthreads mutex", status );
   }
}


void smf_cond_init( pthread_cond_t *cond, int *status ) {
/*
*  Name:
*     smf_cond_init

*  Purpose:
*     A wrapper for pthread_cond_init.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void smf_cond_init( pthread_cond_t *cond, int *status )

*  Arguments:
*     cond
*        The condition variable to be initialised.
*     status
*        Pointer to the inherited status value.

*  Description:
*     This function initialises a condition variable using default
*     attributes.

*/
   if( *status != SAI__OK ) return;

   if( pthread_cond_init( cond, NULL ) ) {
      *status = SAI__ERROR;
      emsRep( "", "Failed to initialise a condition pthreads variable",
               status );
   }
}


void smf_thread_create( pthread_t *thread, void *(*start_routine)(void*),
                        void *arg, int *status ) {
/*
*  Name:
*     smf_thread_create

*  Purpose:
*     A wrapper for pthread_create

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void smf_thread_create( pthread_t *thread, void *(*start_routine)(void*),
                              void *arg, int *status )

*  Arguments:
*     thread
*        Pointer to the pthread structure to initialise.
*     start_routine
*        Pointer to the routine to run in the new thread.
*     arg
*        Pointer to be passed to the start routine.
*     status
*        Pointer to the inherited status value.

*  Description:
*     This function creates a new thread using default attributes.

*/

   int stat;

   if( *status != SAI__OK ) return;

   stat = pthread_create( thread, NULL, start_routine, arg);

   if( stat ) {
      *status = SAI__ERROR;
      emsSeti( "STAT", stat );
      emsRep( "", "Failed to create a pthreads thread (error code ^STAT).",
               status );
   }
}


void smf_mutex_lock( pthread_mutex_t *mutex, int *status ) {
/*
*  Name:
*     smf_mutex_lock

*  Purpose:
*     A wrapper for pthread_mutex_lock

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void smf_mutex_lock( pthread_mutex_t *mutex, int *status )

*  Arguments:
*     mutex
*        Pointer to the mutex.
*     status
*        Pointer to the inherited status value.

*  Description:
*     This function locks a mutex.

*/
   if( *status != SAI__OK ) return;

   if( pthread_mutex_lock( mutex ) ) {
      *status = SAI__ERROR;
      emsRep( "", "Failed to lock a pthreads mutex",
               status );
   }
}


void smf_mutex_unlock( pthread_mutex_t *mutex, int *status ) {
/*
*  Name:
*     smf_mutex_unlock

*  Purpose:
*     A wrapper for pthread_mutex_unlock

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void smf_mutex_unlock( pthread_mutex_t *mutex, int *status )

*  Arguments:
*     mutex
*        Pointer to the mutex.
*     status
*        Pointer to the inherited status value.

*  Description:
*     This function unlocks a mutex.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred, although no further error will be reported if this
*     function should then subsequently fail.

*/
   int old_status;

   old_status = *status;

   if( pthread_mutex_unlock( mutex ) && old_status == SAI__OK ) {
      *status = SAI__ERROR;
      emsRep( "", "Failed to unlock a pthreads mutex variable",
               status );
   } else {
      *status = old_status;
   }
}

void smf_cond_broadcast( pthread_cond_t *cond, int *status ) {
/*
*  Name:
*     smf_cond_broadcast

*  Purpose:
*     A wrapper for pthread_cond_broadcast

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void smf_cond_broadcast( pthread_cond_t *cond, int *status )

*  Arguments:
*     cond
*        Pointer to the condition variable.
*     status
*        Pointer to the inherited status value.

*  Description:
*     This function broadcasts a condition to all threads, unblocking all
*     threads that are blocked on the condition variable.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred, although no further error will be reported if this
*     function should then subsequently fail.

*/
   int old_status;

   old_status = *status;

   if( pthread_cond_broadcast( cond ) && old_status == SAI__OK ) {
      *status = SAI__ERROR;
      emsRep( "", "Failed to broadcast a pthreads condition variable",
               status );
   } else {
      *status = old_status;
   }
}


void smf_cond_signal( pthread_cond_t *cond, int *status ) {
/*
*  Name:
*     smf_cond_signal

*  Purpose:
*     A wrapper for pthread_cond_signal

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void smf_cond_signal( pthread_cond_t *cond, int *status )

*  Arguments:
*     cond
*        Pointer to the condition variable.
*     status
*        Pointer to the inherited status value.

*  Description:
*     This function signals a condition, unblocking at least one thread
*     that is blocked on the condition variable.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred, although no further error will be reported if this
*     function should then subsequently fail.

*/
   int old_status;

   old_status = *status;

   if( pthread_cond_signal( cond ) && old_status == SAI__OK ) {
      *status = SAI__ERROR;
      emsRep( "", "Failed to signal a pthreads condition variable",
               status );
   } else {
      *status = old_status;
   }
}


void smf_cond_wait( pthread_cond_t *cond, pthread_mutex_t *mutex,
                    int *status ){
/*
*  Name:
*     smf_cond_wait

*  Purpose:
*     A wrapper for pthread_cond_wait

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void smf_cond_wait( pthread_cond_t *cond, pthread_mutex_t *mutex,
*                         int *status )

*  Arguments:
*     cond
*        Pointer to the condition variable.
*     mutex
*        Pointer to the associated mutex.
*     status
*        Pointer to the inherited status value.

*  Description:
*     This function blocks the calling thread until a condition is
*     signalled or broadcast.

*/
   if( *status != SAI__OK ) return;

   if( pthread_cond_wait( cond, mutex ) ){
      *status = SAI__ERROR;
      emsRep( "", "Failed to start waiting for a pthreads condition",
               status );
   }
}



static void smf_thread_log_( const char *text, const char *colour, int ijob ) {
   struct timeval tv;
   gettimeofday( &tv, NULL );
   pthread_mutex_lock( &fd_mutex );
   if( ijob > 0 ) {
      fprintf( fd ? fd : stdout, "%ld %ld.%.6ld %s %s (J%d)\n", (long) pthread_self(),
               (long) tv.tv_sec, (long)tv.tv_usec, colour, text, ijob );
   } else {
      fprintf( fd ? fd : stdout, "%ld %ld.%.6ld %s %s\n", (long) pthread_self(),
               (long) tv.tv_sec, (long)tv.tv_usec, colour, text );
   }
   pthread_mutex_unlock( &fd_mutex );
}



