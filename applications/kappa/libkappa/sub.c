/* Indicate that we want to use the 8-byte NDF interface */
#define NDF_I8 1

#include "f77.h"
#include "mers.h"
#include "ndf.h"
#include "prm.h"
#include "sae_par.h"
#include "star/util.h"
#include "star/lpg.h"
#include <string.h>
#include "star/thr.h"

/* Local data types */
typedef struct  SubData {
   const char *itype;
   int bad;
   int oper;
   size_t p1;
   size_t p2;
   size_t nerr;
   void *pntr1;
   void *pntr2;
   void *pntr3;
} SubData;

/* Prototypes for local functions */
static void sub_work( void *job_data_ptr, int *status );


F77_SUBROUTINE(sub)( INTEGER(STATUS) ){
/*
*+
*  Name:
*     sub

*  Purpose:
*     Subtracts one NDF data structure from another.

*  Type of Module:
*     ADAM A-task

*  Synopsis:
*     void sub( int *status )

*  Parameters:
*     *status
*        The global status.

*  Description:
*     This function subtracts one NDF data structure from another
*     pixel-by-pixel to produce a new NDF.

*  Usage:
*     sub in1 in2 out

*  ADAM Parameters:
*     IN1 = NDF (Read)
*        First NDF, from which the second NDF is to be subtracted.
*     IN2 = NDF (Read)
*        Second NDF, to be subtracted from the first NDF.
*     OUT = NDF (Write)
*        Output NDF to contain the difference between the two input NDFs.
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the NDF supplied for parameter IN1 to be used
*        instead. [!]

*  Examples:
*     sub a b c
*        This subtracts the NDF called b from the NDF called a, to make
*        the NDF called c.  NDF c inherits its title from a.
*     sub out=c in1=a in2=b title="Background subtracted"
*        This subtracts the NDF called b from the NDF called a, to make
*        the NDF called c.  NDF c has the title "Background subtracted".

*  Notes:
*     - If the two input NDFs have different pixel-index bounds, then
*     they will be trimmed to match before being subtracted.  An error
*     will result if they have no pixels in common.

*  Related Applications:
*     KAPPA: ADD, CADD, CDIV, CMULT, CSUB, DIV, MATHS, MULT.

*  Implementation Status:
*     -  This function correctly processes the WCS, AXIS, DATA, QUALITY,
*     LABEL, TITLE, HISTORY, WCS and VARIANCE components of an NDF data
*     structure and propagates all extensions.
*     -  The UNITS component is propagated only if it has the same value in
*     both input NDFs.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  Huge NDFs are supported.

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either Version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful, but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David S. Berry (EAO)

*  History:
*     23-SEP-2020 (DSB):
*        Original C version, based on equivalent Fortran function by RFWS
*        et al.

*-
*/
   GENPTR_INTEGER(STATUS)

/* Local Variables: */
   SubData *job_data;    /* Array of structs used to pass info to workers */
   SubData *pdata;       /* A single struct used to pass info to a worker */
   ThrWorkForce *wf;     /* Pointer to pool of worker threads */
   char dtype[ NDF__SZFTP + 1 ];   /* Data type for output components */
   char form[ NDF__SZFRM + 1 ];    /* Form of the ARRAY */
   char itype[ NDF__SZTYP + 1 ];   /* Data type for processing */
   char unit1[ 31 ];     /* Units string from NDF1 */
   char unit2[ 31 ];     /* Units string from NDF2 */
   const char *clist;    /* List of NDF components to copy */
   int bad;              /* Need to check for bad pixels? */
   int iw;               /* Index of worker thread */
   int ndf1;             /* Identifier for 1st NDF (input) */
   int ndf2;             /* Identifier for 2nd NDF (input) */
   int ndf3;             /* Identifier for 3rd NDF (output) */
   int nw;               /* Number of worker threads to use */
   int var1;             /* Variance component in 1st input NDF? */
   int var2;             /* Variance component in 2nd input NDF? */
   size_t el;            /* Number of mapped elements */
   size_t nerr;          /* Number of errors */
   size_t step;          /* Number of pixels per worker thread */
   void *pntr1;          /* Pointer to 1st NDF mapped array */
   void *pntr2;          /* Pointer to 2nd NDF mapped array */
   void *pntr3;          /* Pointer to 3rd NDF mapped array */

/* Check inherited global status. */
   if( *STATUS != SAI__OK ) return;

/* Begin an NDF context. */
   ndfBegin();

/* Obtain identifiers for the two input NDFs. */
   lpgAssoc( "IN1", "READ", &ndf1, STATUS );
   lpgAssoc( "IN2", "READ", &ndf2, STATUS );

/* Trim the input pixel-index bounds to match. */
   ndfMbnd( "TRIM", &ndf1, &ndf2, STATUS );

/* See if the Units are the same. */
   unit1[ 0 ] = 0;
   ndfCget( ndf1, "Unit", unit1, sizeof(unit1), STATUS );
   unit2[ 0 ] = 0;
   ndfCget( ndf2, "Unit", unit2, sizeof(unit2), STATUS );

/* Determine the list of components to be propagated from "ndf1". We only
   propagate the Unit component if it is the same in both input NDFs. */
   if( !strcmp( unit1, unit2 ) && strlen( unit1 ) > 0 ) {
      clist = "WCS,Axis,Quality,Unit";
   } else {
      clist = "WCS,Axis,Quality";
   }

/* Create a new output NDF based on the first input NDF. Propagate the
   components listed above. */
   lpgProp( ndf1, clist, "OUT", &ndf3, STATUS );

/* Determine which data type to use to process the input data/variance
   arrays and set an appropriate data type for these components in the
   output NDF. */
   ndfMtype( "_BYTE,_WORD,_UBYTE,_UWORD,_INTEGER,_INT64,_REAL,_DOUBLE",
             ndf1, ndf2, "Data,Variance", itype, sizeof(itype), dtype,
             sizeof(dtype), STATUS );
   ndfStype( dtype, ndf3, "Data,Variance", STATUS );

/* Map the input and output data arrays. */
   ndfMap( ndf1, "Data", itype, "READ", &pntr1, &el, STATUS );
   ndfMap( ndf2, "Data", itype, "READ", &pntr2, &el, STATUS );
   ndfMap( ndf3, "Data", itype, "WRITE", &pntr3, &el, STATUS );

/* Merge the bad pixel flag values for the input data arrays to see if
   checks for bad pixels are needed. */
   ndfMbad( 1, ndf1, ndf2, "Data", 0, &bad, STATUS );

/* Find the number of cores/processors available and create a pool of
   threads of the same size. */
   wf = thrGetWorkforce( thrGetNThread( "KAPPA_THREADS", STATUS ), STATUS );

/* How many worker threads in the workforce? May not be equal to the value
   of KAPPA_THREADS if the workforce already existed. */
   nw = wf ? wf->nworker : 1;

/* Calculate the number of pixels to be processed by each thread whilst
   accumulating the running sums needed to perform the fits. */
   if( el > 100*nw ) {
      step = el/nw;
   } else {
      step = el;
      nw = 1;
   }

/* Allocate job data for threads. Each structure describes a single job
   to be allocated to a single worker thread. */
   job_data = astCalloc( nw, sizeof(*job_data) );
   if( *STATUS == SAI__OK ) {

/* Store the index of the first and last pixel to be processed by the worker
   thread in each structure. The last worker picks up any remaining pixels
   left over by the integer division used to calculate "step". Also store
   some other required constant values in each structure. */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->p1 = iw*step;
         if( iw < nw - 1 ) {
            pdata->p2 = pdata->p1 + step - 1;
         } else {
            pdata->p2 = el - 1;
         }

         pdata->bad = bad;
         pdata->itype = itype;
         pdata->pntr1 = pntr1;
         pdata->pntr2 = pntr2;
         pdata->pntr3 = pntr3;

/* Submit a job to the workforce to subtract the pixel data values assigned
   to the worker. */
         pdata->oper = 1;
         thrAddJob( wf, 0, pdata, sub_work, 0, NULL, STATUS );
      }

/* Put the current thread to sleep until all the above jobs have
   completed. */
      thrWait( wf, STATUS );

/* See if there may be bad pixels in the output data array and set the
   output bad pixel flag value accordingly unless the output NDF is
   primitive. */
      if( !bad ) {
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            if( pdata->nerr > 0 ) {
               bad = 1;
               break;
            }
         }
      }
      ndfForm( ndf3, "Data", form, sizeof(form), STATUS );
      if( strcmp( form, "PRIMITIVE" ) ) ndfSbad( bad, ndf3, "Data", STATUS );

/* Unmap the data arrays. */
      ndfUnmap( ndf1, "Data", STATUS );
      ndfUnmap( ndf2, "Data", STATUS );
      ndfUnmap( ndf3, "Data", STATUS );

/* If both input NDFs have a variance component, then map the input and
   output variance arrays. */
      ndfState( ndf1, "Variance", &var1, STATUS );
      ndfState( ndf2, "Variance", &var2, STATUS );
      if( var1 && var2 ) {
         ndfMap( ndf1, "Variance", itype, "READ", &pntr1, &el, STATUS );
         ndfMap( ndf2, "Variance", itype, "READ", &pntr2, &el, STATUS );
         ndfMap( ndf3, "Variance", itype, "WRITE", &pntr3, &el, STATUS );

/* See if checks for bad pixels are necessary. */
         ndfMbad( 1, ndf1, ndf2, "Variance", 0, &bad, STATUS );

/* Submit jobs to the workforce to add the variances. */
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->bad = bad;
            pdata->pntr1 = pntr1;
            pdata->pntr2 = pntr2;
            pdata->pntr3 = pntr3;
            pdata->oper = 2;
            thrAddJob( wf, 0, pdata, sub_work, 0, NULL, STATUS );
         }
         thrWait( wf, STATUS );

/* See if bad pixels may be present in the output variance array and
   set the output bad pixel flag value accordingly unless the output
   NDF is primitive. */
         if( !bad ) {
            for( iw = 0; iw < nw; iw++ ) {
               pdata = job_data + iw;
               if( pdata->nerr > 0 ) {
                  bad = 1;
                  break;
               }
            }
         }
         ndfForm( ndf3, "Variance", form, sizeof(form), STATUS );
         if( strcmp( form, "PRIMITIVE" ) ) ndfSbad( bad, ndf3, "Variance", STATUS );
      }

/* Obtain the output title and insert it into the output NDF. */
      ndfCinp( "TITLE", ndf3, "Title", STATUS );
   }

/* End the NDF context. */
   ndfEnd( STATUS );

/* If an error occurred, then report context information. */
   if( *STATUS != SAI__OK ) errRep( " ", "SUB: Error subtracting two NDF data "
                                    "structures.", STATUS );

}

static void sub_work( void *job_data_ptr, int *status ){
/*
*  Name:
*     sub_work

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     sub.

*  Invocation:
*     void sub_work( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SubData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SubData *pdata;
   size_t ierr;
   size_t n;
   void *pa;
   void *pb;
   void *pc;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SubData *) job_data_ptr;

/* Calculate some convenience values. */
   n = pdata->p2 - pdata->p1 + 1;

/* First deal with subtraction. Select the appropriate function for the
   data type being processed and subtract the data arrays. */
   if( pdata->oper == 1 ) {

      if( !strcmp( pdata->itype, "_BYTE" ) ) {
         pa = (char *) pdata->pntr1 + pdata->p1;
         pb = (char *) pdata->pntr2 + pdata->p1;
         pc = (char *) pdata->pntr3 + pdata->p1;
         vecSubB( pdata->bad, n, pa, pb, pc, &ierr, &(pdata->nerr), status );

      } else if( !strcmp( pdata->itype, "_UBYTE" ) ) {
         pa = (unsigned char *) pdata->pntr1 + pdata->p1;
         pb = (unsigned char *) pdata->pntr2 + pdata->p1;
         pc = (unsigned char *) pdata->pntr3 + pdata->p1;
         vecSubUB( pdata->bad, n, pa, pb, pc, &ierr, &(pdata->nerr), status );

      } else if( !strcmp( pdata->itype, "_DOUBLE" ) ) {
         pa = (double *) pdata->pntr1 + pdata->p1;
         pb = (double *) pdata->pntr2 + pdata->p1;
         pc = (double *) pdata->pntr3 + pdata->p1;
         vecSubD( pdata->bad, n, pa, pb, pc, &ierr, &(pdata->nerr), status );

      } else if( !strcmp( pdata->itype, "_INTEGER" ) ) {
         pa = (int *) pdata->pntr1 + pdata->p1;
         pb = (int *) pdata->pntr2 + pdata->p1;
         pc = (int *) pdata->pntr3 + pdata->p1;
         vecSubI( pdata->bad, n, pa, pb, pc, &ierr, &(pdata->nerr), status );

      } else if( !strcmp( pdata->itype, "_INT64" ) ) {
         pa = (int64_t *) pdata->pntr1 + pdata->p1;
         pb = (int64_t *) pdata->pntr2 + pdata->p1;
         pc = (int64_t *) pdata->pntr3 + pdata->p1;
         vecSubK( pdata->bad, n, pa, pb, pc, &ierr, &(pdata->nerr), status );

      } else if( !strcmp( pdata->itype, "_REAL" ) ) {
         pa = (float *) pdata->pntr1 + pdata->p1;
         pb = (float *) pdata->pntr2 + pdata->p1;
         pc = (float *) pdata->pntr3 + pdata->p1;
         vecSubF( pdata->bad, n, pa, pb, pc, &ierr, &(pdata->nerr), status );

      } else if( !strcmp( pdata->itype, "_WORD" ) ) {
         pa = (short int *) pdata->pntr1 + pdata->p1;
         pb = (short int *) pdata->pntr2 + pdata->p1;
         pc = (short int *) pdata->pntr3 + pdata->p1;
         vecSubW( pdata->bad, n, pa, pb, pc, &ierr, &(pdata->nerr), status );

      } else if( !strcmp( pdata->itype, "_UWORD" ) ) {
         pa = (unsigned short int *) pdata->pntr1 + pdata->p1;
         pb = (unsigned short int *) pdata->pntr2 + pdata->p1;
         pc = (unsigned short int *) pdata->pntr3 + pdata->p1;
         vecSubUW( pdata->bad, n, pa, pb, pc, &ierr, &(pdata->nerr), status );
      }

/* Now deal with addition. Select the appropriate function for the  data type
   being processed and add the data arrays. */
   } else {

      if( !strcmp( pdata->itype, "_BYTE" ) ) {
         pa = (char *) pdata->pntr1 + pdata->p1;
         pb = (char *) pdata->pntr2 + pdata->p1;
         pc = (char *) pdata->pntr3 + pdata->p1;
         vecAddB( pdata->bad, n, pa, pb, pc, &ierr, &(pdata->nerr), status );

      } else if( !strcmp( pdata->itype, "_UBYTE" ) ) {
         pa = (unsigned char *) pdata->pntr1 + pdata->p1;
         pb = (unsigned char *) pdata->pntr2 + pdata->p1;
         pc = (unsigned char *) pdata->pntr3 + pdata->p1;
         vecAddUB( pdata->bad, n, pa, pb, pc, &ierr, &(pdata->nerr), status );

      } else if( !strcmp( pdata->itype, "_DOUBLE" ) ) {
         pa = (double *) pdata->pntr1 + pdata->p1;
         pb = (double *) pdata->pntr2 + pdata->p1;
         pc = (double *) pdata->pntr3 + pdata->p1;
         vecAddD( pdata->bad, n, pa, pb, pc, &ierr, &(pdata->nerr), status );

      } else if( !strcmp( pdata->itype, "_INTEGER" ) ) {
         pa = (int *) pdata->pntr1 + pdata->p1;
         pb = (int *) pdata->pntr2 + pdata->p1;
         pc = (int *) pdata->pntr3 + pdata->p1;
         vecAddI( pdata->bad, n, pa, pb, pc, &ierr, &(pdata->nerr), status );

      } else if( !strcmp( pdata->itype, "_INT64" ) ) {
         pa = (int64_t *) pdata->pntr1 + pdata->p1;
         pb = (int64_t *) pdata->pntr2 + pdata->p1;
         pc = (int64_t *) pdata->pntr3 + pdata->p1;
         vecAddK( pdata->bad, n, pa, pb, pc, &ierr, &(pdata->nerr), status );

      } else if( !strcmp( pdata->itype, "_REAL" ) ) {
         pa = (float *) pdata->pntr1 + pdata->p1;
         pb = (float *) pdata->pntr2 + pdata->p1;
         pc = (float *) pdata->pntr3 + pdata->p1;
         vecAddF( pdata->bad, n, pa, pb, pc, &ierr, &(pdata->nerr), status );

      } else if( !strcmp( pdata->itype, "_WORD" ) ) {
         pa = (short int *) pdata->pntr1 + pdata->p1;
         pb = (short int *) pdata->pntr2 + pdata->p1;
         pc = (short int *) pdata->pntr3 + pdata->p1;
         vecAddW( pdata->bad, n, pa, pb, pc, &ierr, &(pdata->nerr), status );

      } else if( !strcmp( pdata->itype, "_UWORD" ) ) {
         pa = (unsigned short int *) pdata->pntr1 + pdata->p1;
         pb = (unsigned short int *) pdata->pntr2 + pdata->p1;
         pc = (unsigned short int *) pdata->pntr3 + pdata->p1;
         vecAddUW( pdata->bad, n, pa, pb, pc, &ierr, &(pdata->nerr), status );
      }
   }
}

