#include "ndf1_types.h"
#include <pthread.h>

/*
*+
*  Name:
*     ndf1GlobalDecs

*  Purpose:
*     Declare all global variables used within the NDF library.

*  Description:
*     This module is simply a collection of global variable declarations
*     (no executable code). Functions that need to access these variables
*     should include "ndf1_type.h", which includes "extern" statements
*     for each of these variables.
*
*     Accessing global variables in a multi-threaded context can lead to
*     simultaneous read and writes, with unpredictable results.
*     Therefore, all access to these variables should be serialised using
*     the associated mutex as described below. Macros that can be used to
*     lock and unlock these mutexes are defined in "ndf1_type.h".

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
*     26-APR-2019 (DSB):
*        Add Ndf_TCB_fixsw.
*     21-APR-2020 (DSB):
*        Add Ndf_TCB_round.

*-
*/




/* Global variables used by ndf1Nxtsl, ndf1Ffs, etc. Any function that
   uses ndf1Nxtsl to search the list of ACB, DCB, PCB or FCB entries,
   or which accesses these lists directly, should lock the appropriate
   mutex first to ensure the current thread has exclusive access to the
   global variables (the mutex should be unlocked when no longer needed).
   =============================================================== */

NdfDCB **Ndf_DCB = NULL;  /* Pointer to array of all DCB pointers */
NdfACB **Ndf_ACB = NULL;  /* Pointer to array of all ACB pointers */
NdfFCB **Ndf_FCB = NULL;  /* Pointer to array of all FCB pointers */
NdfPCB **Ndf_PCB = NULL;  /* Pointer to array of all PCB pointers */

int Ndf_NDCB = 0;    /* Number of DCBs in above array */
int Ndf_NACB = 0;    /* Number of ACBs in above array */
int Ndf_NFCB = 0;    /* Number of FCBs in above array */
int Ndf_NPCB = 0;    /* Number of PCBs in above array */

/* A mutex to serialise access to each of the above array. A thread
   should only access an array after it has acquired a lock on this
   mutex. */
pthread_mutex_t Ndf_DCB_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t Ndf_ACB_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t Ndf_FCB_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t Ndf_PCB_mutex = PTHREAD_MUTEX_INITIALIZER;




/* Global variables used for local communication between the NDF_ library
   and the "source" and "sink" functions used to read and write AST_ data
   from/to HDS objects...
   ===================================================================   */

/* A mutex to serialise access to the following three values. */
pthread_mutex_t Ndf_DCB_astmutex;

/* HDS object locator. */
HDSLoc *Ndf_DCB_astlc;

/* Line number of text being read/written. */
int Ndf_DCB_astln;

/* Pointer to mapped HDS _CHAR array data. */
char *Ndf_DCB_astpt;




/* The following variables hold information about the application.
   =============================================================== */

/* A mutex to serialise access to the following three values. */
pthread_mutex_t Ndf_DCB_appmutex;

/* Name of the currently-executing application. */
char Ndf_DCB_happn[ NDF__SZAPP + 1 ];

/* The command line arguments */
int NDF_DCB_argc;
char **NDF_DCB_argv;



/* The following variables hold information about character components.
   ==================================================================== */

/* The names of the character components. These are initialised in
   pthread_once initialiser, and are then never changed, so do not need a
   mutex to serialise access to them (multiple threads can read them
   simultaneously withotu problems). */
const char *Ndf_DCB_ccn[ NDF__MXCCN ];
const char *Ndf_DCB_accn[ NDF__MXACN ];




/* The following variables are used to hold information about temporary
   NDFs.
   =============================================================== */

/* A mutex to serialise access to the following three values. */
pthread_mutex_t Ndf_TMP_mutex;

/* A flag indicating if no temporary objects have yet been created, and
   therefore that a temporary container file should be created. Also
   acts as a seed for deriving a unique object name. */
int Ndf_TMP_count;

/* A locator for the temporary container file. */
HDSLoc *Ndf_TMP_tmploc;




/* The following variables are used to record usage of ADAM parameters
   by the NDF library.
   =============================================================== */

/* A mutex to serialise access to the following value. */
pthread_mutex_t Ndf_APB_mutex;

/* An AST KeyMap in which each entry has a key that is an ADAM parameter
   name, and a value that is a boolean flag indicating if ndfCancl should
   cancel the parameter if supplied with a blank parameter name. The
   parameters stored in the KeyMap are the ones for which the NDF library
   has stored explicit locators in the parameter system using
   subparPutfloc and subparPutloc These locators are only release when
   the parameter is cancelled, or the parameter system is shut down. In
   order to avoid apparent HDS locator leaks, applications may use
   ndfCancl to cancel all active NDF parameters. */
AstKeyMap *Ndf_APB_pars;





/* The following variables defines global variables (tuning parameters)
   which control the internal function of the NDF system. They are
   declared and initialised in function ndf1Intcb.
   =============================================================== */


/* A mutex to serialise access to the following tuning parameters. */
pthread_mutex_t Ndf_TCB_mutex;

/* Tuning parameters. */
int Ndf_TCB_docvt;
int Ndf_TCB_etflg;
int Ndf_TCB_keep;
int Ndf_TCB_shcvt;
int Ndf_TCB_warn;
AstKeyMap *Ndf_TCB_pxt;
int Ndf_TCB_autohistory;
int Ndf_TCB_secmax;
int Ndf_TCB_forout;
int Ndf_TCB_forin;
int Ndf_TCB_fixdt;
int Ndf_TCB_fixsw;
int Ndf_TCB_round;












