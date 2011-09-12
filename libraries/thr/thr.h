#if !defined( THR_THREADS_INCLUDED )   /* Include this file only once */
#define THR_THREADS_INCLUDED
/*
 *  Name:
 *     thr.h

 *  Purpose:
 *     Defines the public interface for the thr.c module.

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
 *     12-SEP-2011 (DSB):
 *        Moved from smurf to a separate library (thr).
 */

#include <pthread.h>

/* Macros */
#define THR__REPORT_JOB 1
#define THR__FREE_JOBDATA 2


/* Type definitions */
/* ---------------- */

/* Describes the error status upon completion of a job by a worker. */
typedef struct ThrJobStatus {
   int ems_status;
   int nmessage;
   char **messages;
} ThrJobStatus;

/* Describes a single job in a linked list of jobs. */
typedef struct ThrJob ThrJob;
typedef struct ThrWorkForce ThrWorkForce;

struct ThrJob {
  int ijob;                   /* Job identifier */
  int flags;                  /* Job control flags */
  void *data;                 /* Structure holding data for the worker */
  void (*func)(void *, int *);/* The function to be run by the worker */
  int nheld_up;               /* Length of "held_up" array */
  ThrJob **held_up;           /* Jobs that cannot run until this one has finished */
  int nwaiting_on;            /* Length of "waiting_on" array */
  ThrJob **waiting_on;        /* Jobs that must finish before this one can run */
  ThrJob *next;               /* Next job in list */
  ThrJob *prev;               /* Previous job in list */
  int conid;                  /* Context idenrifier for job */
  ThrJobStatus *status;       /* The error status upon completion of the job */
};

/* Structure describing the whole work force. */
struct ThrWorkForce {
  int nworker;                /* No. of workers in the work force */
  ThrJob *finished_jobs;      /* Linked list of finished jobs */
  ThrJob *active_jobs;        /* Linked list of active jobs */
  ThrJob *free_jobs;          /* Linked list of free jobs structures */
  ThrJob *available_jobs;     /* Linked list of jobs available to run */
  ThrJob *waiting_jobs;       /* Linked list of jobs waiting on other jobs */
  pthread_cond_t job_done;    /* Signals "another job has completed" */
  pthread_cond_t all_done;    /* Signals "no more jobs to be done" */
  pthread_cond_t page;        /* Signals worker to return to job desk */
  pthread_mutex_t jd_mutex;   /* Mutex controlling access to the job desk */
  int kill;                   /* No. of workers still to be terminated */
  int ncontext;               /* Number of context identifiers issued so far */
  int *contexts;              /* List of job context identifiers. */
  int condepth;               /* Depth of job context nesting */
  ThrJobStatus *status;       /* First bad error status reported by a worker */
};


/* Prototypes for inherited status wrappers for pthreads functions. */
void thrMutexInit( pthread_mutex_t *mutex, int *status );
void thrCondInit( pthread_cond_t *cond, int *status );
void thrThreadCreate( pthread_t *thread, void *(*start_routine)(void*), void *arg, int *status );
void thrMutexLock( pthread_mutex_t *mutex, int *status );
void thrMutexUnlock( pthread_mutex_t *mutex, int *status );
void thrCondBroadcast( pthread_cond_t *cond, int *status );
void thrCondSignal( pthread_cond_t *cond, int *status );
void thrCondWait( pthread_cond_t *cond, pthread_mutex_t *mutex, int *status );

/* Prototypes for public worker and workforce related functions. */
ThrWorkForce *thrCreateWorkforce( int nworker, int *status );
ThrWorkForce *thrDestroyWorkforce( ThrWorkForce *workforce );
ThrWorkForce *thrGetWorkforce( int nworker, int *status );
void thrWait( ThrWorkForce *workforce, int *status );
int thrAddJob( ThrWorkForce *workforce, int flags, void *data,
                 void (*func)( void *, int * ), int nwait_on,
                 const int *wait_on, int *status );
int thrJobWait( ThrWorkForce *workforce, int *status );
void *thrGetJobData( int ijob, ThrWorkForce *workforce, int *status );
void thrBeginJobContext( ThrWorkForce *workforce, int *status );
void thrEndJobContext( ThrWorkForce *workforce, int *status );

/* Prototypes for other public functions */
int thrGetNThread( const char *env, int *status );

#endif
