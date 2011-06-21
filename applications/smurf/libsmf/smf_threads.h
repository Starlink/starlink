#if !defined( SMF_THREADS_INCLUDED )   /* Include this file only once */
#define SMF_THREADS_INCLUDED
/*
 *  Name:
 *     smf_threads.h

 *  Purpose:
 *     Defines the public interface for the smf_threads.c module.

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
 *     6-JUN-2008 (DSB):
 *        Original version.
 */

#include <pthread.h>

/* Macros */
#define SMF__REPORT_JOB 1
#define SMF__FREE_JOBDATA 2


/* Type definitions */
/* ---------------- */

/* Describes the error status upon completion of a job by a worker. */
typedef struct smfJobStatus {
   int ems_status;
   int nmessage;
   char **messages;
} smfJobStatus;

/* Describes a single job in a linked list of jobs. */
typedef struct smfJob smfJob;
typedef struct smfWorkForce smfWorkForce;

struct smfJob {
  int ijob;                   /* Job identifier */
  int flags;                  /* Job control flags */
  void *data;                 /* Structure holding data for the worker */
  void (*func)(void *, int *);/* The function to be run by the worker */
  int nheld_up;               /* Length of "held_up" array */
  smfJob **held_up;           /* Jobs that cannot run until this one has finished */
  int nwaiting_on;            /* Length of "waiting_on" array */
  smfJob **waiting_on;        /* Jobs that must finish before this one can run */
  smfJob *next;               /* Next job in list */
  smfJob *prev;               /* Previous job in list */
  int conid;                  /* Context idenrifier for job */
  smfJobStatus *status;       /* The error status upon completion of the job */
};

/* Structure describing the whole work force. */
struct smfWorkForce {
  int nworker;                /* No. of workers in the work force */
  smfJob *finished_jobs;      /* Linked list of finished jobs */
  smfJob *active_jobs;        /* Linked list of active jobs */
  smfJob *free_jobs;          /* Linked list of free jobs structures */
  smfJob *available_jobs;     /* Linked list of jobs available to run */
  smfJob *waiting_jobs;       /* Linked list of jobs waiting on other jobs */
  pthread_cond_t job_done;    /* Signals "another job has completed" */
  pthread_cond_t all_done;    /* Signals "no more jobs to be done" */
  pthread_cond_t page;        /* Signals worker to return to job desk */
  pthread_mutex_t jd_mutex;   /* Mutex controlling access to the job desk */
  int kill;                   /* No. of workers still to be terminated */
  int ncontext;               /* Number of context identifiers issued so far */
  int *contexts;              /* List of job context identifiers. */
  int condepth;               /* Depth of job context nesting */
  smfJobStatus *status;       /* First bad error status reported by a worker */
};


/* Prototypes for inherited status wrappers for pthreads functions. */
void smf_mutex_init( pthread_mutex_t *mutex, int *status );
void smf_cond_init( pthread_cond_t *cond, int *status );
void smf_thread_create( pthread_t *thread, void *(*start_routine)(void*), void *arg, int *status );
void smf_mutex_lock( pthread_mutex_t *mutex, int *status );
void smf_mutex_unlock( pthread_mutex_t *mutex, int *status );
void smf_cond_broadcast( pthread_cond_t *cond, int *status );
void smf_cond_signal( pthread_cond_t *cond, int *status );
void smf_cond_wait( pthread_cond_t *cond, pthread_mutex_t *mutex, int *status );

/* Prototypes for public worker and workforce related functions. */
smfWorkForce *smf_create_workforce( int nworker, int *status );
smfWorkForce *smf_destroy_workforce( smfWorkForce *workforce );
smfWorkForce *smf_get_workforce( int nworker, int *status );
void smf_wait( smfWorkForce *workforce, int *status );
int smf_add_job( smfWorkForce *workforce, int flags, void *data,
                 void (*func)( void *, int * ), int nwait_on,
                 const int *wait_on, int *status );
int smf_job_wait( smfWorkForce *workforce, int *status );
void *smf_get_job_data( int ijob, smfWorkForce *workforce, int *status );
void smf_begin_job_context( smfWorkForce *workforce, int *status );
void smf_end_job_context( smfWorkForce *workforce, int *status );

#endif
