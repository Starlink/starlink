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
*     Copyright (C) 2008 Science & Technology Facilities Council.
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

/* Definition of the public interface of this module */
#include "smf_threads.h"
#include "smf_typ.h"

/* Module macros: */
#define DESK "BLACK"
#define WAIT "RED"
#define ACTIVE "GREEN"
#define smf_thread_log(text,col,job) {if( fd ) smf_thread_log_(text,col,job);}

/* Module variables */
/* ---------------- */
static FILE *fd = NULL;
static int njob = 0;
pthread_mutex_t fd_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Prototypes */
/* ---------- */

/* Private functions used in this module */
static void *smf_run_worker( void *worker_ptr );
static void smf_remove_from_list( smfJob *job, smfJob **head, int *status );
static void smf_push_list_head( smfJob *job, smfJob **head, int *status );
static void smf_push_list_foot( smfJob *job, smfJob **head, int *status );
static smfJob *smf_pop_list_head( smfJob **head, int *status );
static void smf_thread_log_( const char *text, const char *colour, int ijob );


/* Public workforce-related functions */
/* ---------------------------------- */

int smf_add_job( smfWorkForce *workforce, int flags, void *data, 
                  void (*func)( void *, int * ), 
                  int (*checker)( smfJob *, smfWorkForce *, int * ), 
                  int *status ){
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
*                       void (*func)( void *, int * ), 
*                       int (*checker)( smfJob *, smfWorkForce *, int * ), 
*                       int *status ){

*  Arguments:
*     workforce
*        Pointer to the workforce.
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
*     checker
*        An optional pointer to a function that is called prior to starting 
*        the job, which returns a non-zero integer if the job can be run,
*        but returns zero if the job cannot be run now. For instance, zero 
*        should be returned if the job may modify global data that is 
*        already being modified as part of another currently active job. If 
*        zero is returned, the checker MUST use the smf_wait_on_job function 
*        to indicate which jobs have to be completed before this job can run. 
*        The job will then be postponed until those jobs have completed. The
*        arguments supplied to the checker are 1) a pointer to the job
*        being checked, 2) a pointer to the workforce, and 3) an inherited 
*        status pointer. The function should return a non-zero value if an 
*        error occurs. If a NULL pointer is supplied for "checker", the 
*        job will start as soon as a worker is available to carry it out.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     A positive integer identifier for the job. Zero if an error occurs.

*  Description:
*     This function adds a job to the list of jobs to be performed by the 
*     workforce. The job will start immediately if a worker thread is 
*     available to execute the job (and the checker function returns
*     non-zero). Otherwise, it will start as soon as a worker thread becomes 
*     available and its checker fuction returns non-zero. Jobs are not
*     necessarily started in the order in which they are added to the 
*     workforce.

*  Job Control Flags:
*     SMF__REPORT_JOB: Indicates that this job is to be included in the
*     list of jobs for which smf_job_wait will wait.

*/

/* Local Variables: */
   smfJob *job;

/* Check inherited status */
   if( *status != SAI__OK ) return 0;

/* Wait until we are at the head of the queue at the job desk. We then
   have exclusive access to the workforce structure, etc. */
   smf_thread_log( "add_job: Join queue", WAIT, -1 );
   smf_mutex_lock( &(workforce->jd_mutex), status );
   smf_thread_log( "add_job: At desk", DESK, -1 );

/* Get a structure in which to place the job description. If any such
   structures are available on the workforce's list of free job structures, 
   use one from the free list. Otherwise, create a new one. */
   job = smf_pop_list_head( &(workforce->free_jobs), status );
   if( ! job ) {
      job = astMalloc( sizeof( smfJob ) );
      if( job ) {
         job->next = NULL;
         job->prev = NULL;
         job->waiting = NULL;
         job->nwaiting = 0;
      }
   }

/* Store the supplied function and data pointers. */
   if( job ) {
      job->ijob = ++njob;
      job->flags = flags;
      job->func = func;
      job->data = data;
      job->checker = checker;

/* Add the job to the foot of the available job list. As each worker
   thread becomes idle, it will take the next job at the head of this 
   list. */
      smf_push_list_foot( job, &(workforce->available_jobs), status );
      smf_thread_log( "add_job: Pushed job onto available jobs list",
                      DESK, job->ijob );

/* Tell any idle workers that a new job is available. */
      smf_cond_signal( &(workforce->page), status );
      smf_thread_log( "add_job: Paged idle workers", DESK, job->ijob );
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
*     Create a thread pool creating a specified number of threads.

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
*     returned pool should be freed using smf_destroyworkforce when 
*     no longer needed.

*  Description:
*     This function creates a new "workforce" - a pool of threads that
*     can be used to execute tasks in parallel. Each task should be 
*     split into two or more jobs, and a description of each job should
*     be given to the work force using smf_add_job. As each job is added, 
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
      result->status = *status;
      smf_cond_init( &( result->all_done ), status );
      smf_cond_init( &( result->job_done ), status );
      smf_cond_init( &( result->page ), status );
      smf_mutex_init( &( result->jd_mutex ), status );
      
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
*        Pointer to the workforce to be destroyed.

*  Returned ValueL
*     A NULL pointer is returned.      

*  Description:
*     This function frees all resources used by a work force. This
*     includes cancelling the worker threads, and freeing memory
*     structures.

*/

/* Local Variables: */
   smfJob *job;
   int status = SAI__OK;

/* Check a workforce was supplied. */
   if( workforce ) {

/* Free the mutex and condition variables used by the workforce. */
      pthread_mutex_destroy( &( workforce->jd_mutex ) );
      pthread_cond_destroy( &( workforce->all_done ) );
      pthread_cond_destroy( &( workforce->job_done ) );
      pthread_cond_destroy( &( workforce->page ) );

/* Free the job description structures. */
      job = smf_pop_list_head( &(workforce->active_jobs), &status );
      while( job ) {
         job->waiting = astFree( job->waiting );
         job = astFree( job );
         job = smf_pop_list_head( &(workforce->active_jobs), &status );
      }

      job = smf_pop_list_head( &(workforce->free_jobs), &status );
      while( job ) {
         job->waiting = astFree( job->waiting );
         job = astFree( job );
         job = smf_pop_list_head( &(workforce->free_jobs), &status );
      }

      job = smf_pop_list_head( &(workforce->available_jobs), &status );
      while( job ) {
         job->waiting = astFree( job->waiting );
         job = astFree( job );
         job = smf_pop_list_head( &(workforce->available_jobs), &status );
      }

      job = smf_pop_list_head( &(workforce->finished_jobs), &status );
      while( job ) {
         job->waiting = astFree( job->waiting );
         job = astFree( job );
         job = smf_pop_list_head( &(workforce->finished_jobs), &status );
      }

      job = smf_pop_list_head( &(workforce->waiting_jobs), &status );
      while( job ) {
         job->waiting = astFree( job->waiting );
         job = astFree( job );
         job = smf_pop_list_head( &(workforce->waiting_jobs), &status );
      }

/* Free the memory holding the workforce. */
      workforce = astFree( workforce );
   }

   return NULL;
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
*        Pointer to the workforce.
*     status
*        Pointer to the inherited status value.

*  Description:
*     Each consecutive call to this function return the integer identifier 
*     for a completed job, in the order in which they are completed. If all 
*     completed jobs have already been reported, then this function
*     blocks until the next job is completed.
*
*     Note, only jobs which had the SMF__REPORT_JOB flag set when calling
*     smf_add_job are included in the list of returned jobs.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.

*/

/* Local Variables: */
   int result;
   int wf_status;
   smfJob *job;

/* Start a new error reporting context. */
   emsBegin( status );

/* Wait in the job desk queue until we have exclusive access to the job 
   desk. */
   smf_thread_log( "job_wait: Joining queue", WAIT, -1 );
   smf_mutex_lock( &( workforce->jd_mutex ), status );
   smf_thread_log( "job_wait: At desk", DESK, -1 );

/* Wait until there is a job available on the finished jobs list. Put it in 
   a loop because of spurious wake ups. */ 
   while( ! workforce->finished_jobs ){
      smf_thread_log( "job_wait: Waiting for a job to complete", WAIT, -1 );
      smf_cond_wait( &( workforce->job_done ), &( workforce->jd_mutex ), 
                     status );
      if( *status != SAI__OK ) break;
   }

/* Pop the head job of the list, and note its identifier. */
   job = smf_pop_list_head( &(workforce->finished_jobs), status );
   result = job->ijob;
   smf_thread_log( "job_wait: Job completed", DESK, result );

/* Clear the job data, and put the job structure onto the list of free
   job structures. */
   job->ijob = 0;
   job->flags = 0;
   job->func = NULL;
   job->data = NULL;
   job->checker = NULL;
   job->nwaiting = 0;
   smf_push_list_head( job, &(workforce->free_jobs), status );

/* Note the workforce status whilst we still have the job desk mutex. */
   wf_status = workforce->status;

/* Unlock the mutex so that the next thread can access the job desk. */
   smf_mutex_unlock( &( workforce->jd_mutex ), status );
   smf_thread_log( "job_wait: Left desk", ACTIVE, result );

/* Return the workforce status unless some other error has already
   occurred. */
   if( *status == SAI__OK ) *status = wf_status;

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
*        Pointer to the workforce.
*     status
*        Pointer to the inherited status value.

*  Description:
*     This function blocks the calling thread until all jobs allocated to
*     the work force using smf_add_job have been completed.
*
*     A side effect of this function is to empty the list of jobs waiting
*     to be reported by smf_job_wait. Upon exit from this function, all
*     jobs waiting to be reported via smf_job_wait will be considered to 
*     have been reported.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.

*/

/* Local Variables: */
   smfJob *job;

/* Start a new error reporting context. */
   emsBegin( status );

/* Wait in the job desk queue until we have exclusive access to the job 
   desk. */
   smf_thread_log( "wait: Joining queue", WAIT, njob );
   smf_mutex_lock( &( workforce->jd_mutex ), status );
   smf_thread_log( "wait: At desk", DESK, njob );

/* If there are any jobs available to run or any currently active jobs, 
   wait until the last job completes. Spurious wake-ups can occur so put 
   this in a loop. The call to pthread_cond_wait (within smf_cond_wait)
   will release the specified mutex (the job desk mutex) before blocking
   this thread. This enables worker threads to access the job deks to 
   report completion of jobs and to get new jobs. */
   while( workforce->available_jobs || workforce->active_jobs ){
      smf_thread_log( "wait: waiting for all done", WAIT, njob );
      smf_cond_wait( &( workforce->all_done ), &( workforce->jd_mutex ), 
                     status );
      if( *status != SAI__OK ) break;
   }

   smf_thread_log( "wait: all done", DESK, njob );

/* Empty the finished job list, moving them onto the free list. */
   job = smf_pop_list_head( &(workforce->finished_jobs), status );
   while( job ) {
      job->ijob = 0;
      job->flags = 0;
      job->func = NULL;
      job->data = NULL;
      job->checker = NULL;
      job->nwaiting = 0;
      smf_push_list_head( job, &(workforce->free_jobs), status );
      job = smf_pop_list_head( &(workforce->finished_jobs), status );
   }

/* Once the condition is signalled, the pthread_cond_wait will continue
   to wait until it can lock the specified mutex (the job desk mutex). So
   unlock it now so that other threads can get to the job desk. */
   smf_mutex_unlock( &( workforce->jd_mutex ), status );
   smf_thread_log( "wait: Leaving desk", ACTIVE, njob );

/* Return the workforce status. */
   if( *status == SAI__OK ) *status = workforce->status;

/* End the error reporting context. */
   emsEnd( status );
}


void smf_wait_on_job( smfJob *job1, smfJob *job2, int *status ){
/*
*  Name:
*     smf_wait_on_job

*  Purpose:
*     Called to indicate that job1 cannot run until job2 has completed.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     #include "smf_threads.h"
*     void smf_wait_on_job( smfJob *job1, smfJob *job2, int *status );

*  Arguments:
*     job1
*        Pointer to the first job description.
*     job2
*        Pointer to the second job description.
*     status
*        Pointer to the inherited status value.

*  Description:
*     This function stores a pointer to job1 within job2. The consequence
*     of this is that no attempt to run job1 will be made until job2 has 
*     completed.
*
*     This function is normally called from within a job checker function
*     (see smf_add_job).

*/

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Add job1 to the list of jobs to be moved onto the available jobs list
   when job2 completes. */
   job2->waiting = astGrow( job2->waiting, job2->nwaiting + 1, 
                            sizeof(smfJob *) );
   if( astOK ) {
      job2->waiting[ (job2->nwaiting)++ ] = job1;
   }

}

/* Private workforce-related functions */
/* ----------------------------------- */

static smfJob *smf_pop_list_head( smfJob **head, int *status ){
/*
*  Name:
*     smf_pop_list_head

*  Purpose:
*     Return and job at the head of a list and remove it from the list.

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
   int job_status;
   int jobs_available;
   int run;         
   int status;
   smfJob *job = NULL;
   smfWorkForce *wf;

/* Initialise the status value that describes the success of the thread
   handling infra-structure. */
   status = SAI__OK;

/* Get a pointer to the workforce description. */
   wf = (smfWorkForce *) wf_ptr;

/* Initialise AST's thread specific data by calling astBegin before any
   other AST functions. */
   astBegin;

/* Switch on AST memory caching for this thread. */
   (void) astTune( "MemoryCaching", 1 ); 

/* Tell AST to watch the local job status variable. */
   astWatch( &job_status );

/* Join the end of the queue of workers at the job desk. This call will
   block until you reach the head of the queue. */
   smf_mutex_lock( &(wf->jd_mutex), &status );

/* Loop until an error occurs in the threading infrastructure. The thread
   will be killed if smf_destroy_workforce is called. */
   while( status == SAI__OK ) {

/* You are now at the job desk and have exclusive access to all the 
   information relating to the workforce. Note if there are any available
   jobs waiting to be run. */
      jobs_available = ( wf->available_jobs != NULL );

/* Report completion of any job that you have just completed. */
      if( job ) {
         smf_thread_log( "run_worker: Worker at desk to report job done", 
                         DESK, job->ijob );

/* If the job failed, transfer the job status to the workforce so long 
   as this is the first failed job. This causes all subsequent jobs to 
   return without action if an error has occurred. */
         if( wf->status == SAI__OK ) wf->status = job_status;

/* Remove the job from the list of currently active jobs. */
         smf_remove_from_list( job, &(wf->active_jobs), &status );

/* If any jobs were waiting on the completion of this job, move them
   off the list of waiting jobs and put them at the head of the list of 
   available jobs. */
         for( i = 0; i < job->nwaiting; i++ ) {
            smf_remove_from_list( job->waiting[ i ], &(wf->waiting_jobs), 
                                  &status );
            smf_push_list_head( job->waiting[ i ], &(wf->available_jobs), 
                                &status );
         }

/* If required, add the job onto the end of the "finished" list, and
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
            job->checker = NULL;
            job->nwaiting = 0;
            smf_push_list_head( job, &(wf->free_jobs), &status );
         }

/* Indicate you now have no associated job. */
         job = NULL;
      }

/* Now attempt to aquire a new job from the list of available jobs.
   Some of the available jobs may not be ready to run, so loop until you
   find one that is, or the list of available jobs is exhausted. */
      while( !job ) {
         job = smf_pop_list_head( &(wf->available_jobs), &status );
         if( !job ) break;

/* See if this job can be run at the current time. This check is performed 
   by a function that was supplied as part of the job description. If the
   checker decided that the job cannot be run until one of the currently
   active jobs completes, the checker should add the job to the list of
   jobs waiting for the completion of the active job. */
         run = job->checker ? (*job->checker)( job, wf, &status ) : 1;

/* If not, add it to the list of waiting jobs, and loop to get a new job
   off the list of available jobs. */
         if( ! run ) {
            smf_push_list_head( job, &(wf->waiting_jobs), &status );
            job = NULL;
         }
      }

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

/* Use the workforce's inherited status */
         job_status = wf->status;

/* Add the job to the list of active jobs. */
         smf_push_list_head( job, &(wf->active_jobs), &status );

/* Leave the job desk queue, allowing the next worker to report a
   completed job and/or get a new job. */
         smf_mutex_unlock( &(wf->jd_mutex), &status );
         smf_thread_log( "run_worker: left desk to do job", ACTIVE, 
                         job->ijob );

/* Do the job. */
         (*job->func)( job->data, &job_status );
         smf_thread_log( "run_worker: completed job - joining queue",
                         WAIT, job->ijob );

/* Now the job is complete, re-join the end of the job desk queue to report 
   its completion and get a new job. */
         smf_mutex_lock( &(wf->jd_mutex), &status );
         wf->status = job_status;

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
   something has gone wrong. Probably a job checker function failed to 
   register the jobs dependency on some other job, resuling in it never 
   being moved from the waiting list to the available list. */
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

/* If something goes wrong in the threads infrastructure, store the bad
   status value in the work force and abort the current job by signalling 
   "all done". */
   wf->status = status;
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
               (long) tv.tv_sec, tv.tv_usec, colour, text, ijob );
   } else {
      fprintf( fd ? fd : stdout, "%ld %ld.%.6ld %s %s\n", (long) pthread_self(), 
               (long) tv.tv_sec, tv.tv_usec, colour, text );
   }
   pthread_mutex_unlock( &fd_mutex );
}


