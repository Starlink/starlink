#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

int
dau_match_types(struct PDD *obj, struct PDD *app)

/*+
 * DAU_MATCH_TYPES - Match data types
 *
 * This function determines whether conversion is possible between object
 * data and application data.
 *
 * Calling sequence:
 *
 *        DAU_MATCH_TYPES(OBJ,APP)
 *
 * OBJ    is the address of a descriptor which contains the object data
 *        attributes.
 * APP    is the address of a descriptor which contains the application
 *        data attributes.
 *
 * Routine value:
 *
 *        DAT__OK    if successful.
 *        DAT__CONIN if the conversion is invalid.
 */

{
   int nbad;
   dat1_cvt(1, 0, obj, app, &nbad );
   return hds_gl_status;
}

int
dat1_cvt( int bad,
          UINT_BIG nval,
          struct PDD *imp,
          struct PDD *exp,
          int *nbad )

/*+
 * dat1_cvt - Translate data
 *
 * This routine 'translates' a contiguous sequence of data values from one
 * location to another. If the data types differ, then conversion is per-
 * formed (if possible), otherwise the values are copied directly.
 *
 * (Note, that during conversion, any data values that cannot be sensibly
 * translated from the source type to the destination type are substituted
 * by a specific 'bad' value, and the return status set accordingly).
 *
 * Routine value:
 *
 *        DAT__OK    if successful.
 *        DAT__CONIN if conversion is not possible.
 *        DAT__CONER if any conversion errors have been detected.
 *
 * 6-AUG-1993 (RFWS):
 *    Changed to allow nval to be supplied as zero (dau_match_types calls
 *    this routine with this value).
 * 20-NOV-2015 (DSB):
 *    Changed "nval" from int to UINT_BIG.
 */

#define MAXBUF ( 32 * 512 )
{
   UINT_BIG remain;              /* Number of values still to transfer      */
   int expfnat;                  /* Output number format native?            */
   int exponat;                  /* Output storage order native?            */
   int impfnat;                  /* Input number format native?             */
   int imponat;                  /* Input storage order native?             */
   int nbad1;                    /* Local conversion error count            */
   int nbad2;                    /* Local conversion error count            */
   int nwrk;                     /* Number elements to store in workspace   */
   int temp_status;              /* Temporary store for global status       */
   int transfer;                 /* Number of values to transfer            */
   int wrklen;                   /* Length of data elements in workspace    */
   struct PDD pdd1;              /* Temporary input data descriptor         */
   struct PDD pdd2;              /* Temporary output data descriptor        */
   struct PDD wrk;               /* Descriptor for workspace array          */
   unsigned char *buf;           /* Pointer to workspace array              */

/* Check the inherited global status. Allow the routine to execute if       */
/* status is set to DAT__CONER indicating a previous conversion error.      */
   if ( !( _ok( hds_gl_status ) || ( hds_gl_status == DAT__CONER ) ) )
      return hds_gl_status;

/* Initialise.                                                              */
   *nbad = 0;

/* Determine whether the input and output storage order matches the         */
/* corresponding native machine storage order.                              */
   imponat = ( imp->order == dat_gl_ndr[ imp->dtype ].order );
   exponat = ( exp->order == dat_gl_ndr[ exp->dtype ].order );

/* Also determine whether the number formats match the corresponding native */
/* machine number formats.                                                  */
   impfnat = ( imp->format == dat_gl_ndr[ imp->dtype ].format );
   expfnat = ( exp->format == dat_gl_ndr[ exp->dtype ].format );

/* Test for each class of conversion in turn:                               */
/* =========================================                                */

/* Case 1:                                                                  */
/* ======                                                                   */
/* Input and output data descriptors match. No conversion is required, so   */
/* simply copy the data.                                                    */
   if ( ( imp->dtype == exp->dtype ) &&
        ( imp->length == exp->length ) &&
        ( imp->order == exp->order ) &&
        ( imp->format == exp->format ) )
   {
      memcpy( (void *) exp->body, (void *) imp->body, nval*imp->length );
   }

/* Case 2:                                                                  */
/* ======                                                                   */
/* Input and output data differ only in their storage order. Simply convert */
/* the storage order.                                                       */
   else if ( ( imp->dtype == exp->dtype ) &&
             ( imp->length == exp->length ) &&
             ( imp->format == exp->format ) )
   {
      dat1_cvt_order( nval, imp, exp, &hds_gl_status );
   }

/* Case 3:                                                                  */
/* ======                                                                   */
/* Both input and output number formats are native to the current machine,  */
/* but the primitive types and/or storage orders differ between the input   */
/* and output. No number format conversion will be needed, but a data type  */
/* conversion will be required, possibly accompanied by a storage order     */
/* reversal.                                                                */
   else if ( impfnat && expfnat )
   {

/* Check if the input and output storage orders both match the native       */
/* machine storage order. If so, then a data type conversion alone will     */
/* suffice.                                                                 */
      if ( imponat && exponat )
      {
         dat1_cvt_dtype( bad, nval, imp, exp, nbad );
      }

/* If either the input or output storage order does not match the native    */
/* storage order for the machine, then the type conversion must be preceded */
/* or followed by a storage order reversal.  Determine how much workspace   */
/* is required for the intermediate results.                                */
      else
      {
         wrklen = imponat ? exp->length : imp->length;
         nwrk = _max( 1, MAXBUF / wrklen );
         nwrk = _min( nwrk, nval );

/* Allocate the workspace, ensuring this happens even if the global status  */
/* is set to DAT__CONER. Check for errors in the allocation routine.        */
         temp_status = hds_gl_status;
         if ( hds_gl_status == DAT__CONER ) hds_gl_status = DAT__OK;
         rec_alloc_mem( _max( nwrk * wrklen, 1 ), (void **) &buf );
         if ( _ok( hds_gl_status ) )
         {
            hds_gl_status = temp_status;

/* Generate temporary copies of the descriptors for the input and output    */
/* data and form an appropriate descriptor for the workspace (this          */
/* specifies which conversions are needed).                                 */
            pdd1 = *imp;
            pdd2 = *exp;
            wrk = imponat ? *exp : *imp;
            wrk.order = imponat ? imp->order : exp->order;
            wrk.body = buf;

/* Loop through the data in units of the temporary buffer size.             */
            remain = nval;
            while ( ( remain > 0 ) &&
                    ( _ok( hds_gl_status ) ||
                    ( hds_gl_status == DAT__CONER ) ) )
            {
               transfer = _min( nwrk, remain );

/* If the input storage order is native to the machine, then perform a type */
/* conversion followed by storage order reversal.                           */
               if ( imponat )
               {
                  dat1_cvt_dtype( bad, transfer, &pdd1, &wrk, &nbad1 );
                  dat1_cvt_order( transfer, &wrk, &pdd2, &hds_gl_status );
               }

/* Otherwise, perform storage order reversal first and then a type          */
/* conversion.                                                              */
               else
               {
                  dat1_cvt_order( transfer, &pdd1, &wrk, &hds_gl_status );
                  dat1_cvt_dtype( bad, transfer, &wrk, &pdd2, &nbad1 );
               }

/* Accumulate the count of conversion errors and increment the input/output */
/* pointers.                                                                */
               *nbad += nbad1;
               remain -= transfer;
               pdd1.body += imp->length * transfer;
               pdd2.body += exp->length * transfer;
            }
         }

/* Deallocate the workspace, ensuring that any errors that occur are        */
/* detected.                                                                */
         temp_status = hds_gl_status;
         if ( hds_gl_status == DAT__CONER )
            hds_gl_status = DAT__OK;
         rec_deall_mem( _max( nwrk * wrklen, 1 ), (void **) &buf );
         if ( _ok( hds_gl_status ) )
            hds_gl_status = temp_status;
      }
   }

/* Case 4:                                                                  */
/* ======                                                                   */
/* The input or output number format differs from the native machine number */
/* format. The input and output data types and storage orders may also      */
/* differ from each other. A number format conversion will be required      */
/* (which may involve storage order changes), possibly preceded or followed */
/* by a data type conversion.                                               */
   else
   {

/* Check if the input and output data types both match. If so, then a       */
/* format conversion alone will suffice.                                    */
      if ( imp->dtype == exp->dtype )
      {
         dat1_cvt_format( bad, nval, imp, exp, nbad, &hds_gl_status );
      }

/* Otherwise, if the input and output data types differ, then the format    */
/* conversion must be preceded or followed by a type conversion. Determine  */
/* how much workspace is required for the intermediate results.             */
      else
      {
         wrklen = impfnat ? exp->length : imp->length;
         nwrk = _max( 1, MAXBUF / wrklen );
         nwrk = _min( nwrk, nval );

/* Allocate the workspace, ensuring this happens even if the global status  */
/* is set to DAT__CONER. Check for errors in the allocation routine.        */
         temp_status = hds_gl_status;
         if ( hds_gl_status == DAT__CONER )
            hds_gl_status = DAT__OK;
         rec_alloc_mem( _max( nwrk * wrklen, 1 ), (void **) &buf );
         if ( _ok( hds_gl_status ) )
         {
            hds_gl_status = temp_status;

/* Generate temporary copies of the descriptors for the input and output    */
/* data and form an appropriate descriptor for the workspace (this          */
/* specifies which conversions are needed).                                 */
            pdd1 = *imp;
            pdd2 = *exp;
            wrk = impfnat ? *exp: *imp;
            wrk.length = impfnat ? dat_gl_ndr[ exp->dtype ].length :
                                   dat_gl_ndr[ imp->dtype ].length;
            wrk.order = impfnat ? dat_gl_ndr[ exp->dtype ].order :
                                  dat_gl_ndr[ imp->dtype ].order;
            wrk.format = impfnat ? dat_gl_ndr[ exp->dtype ].format :
                                   dat_gl_ndr[ imp->dtype ].format;
            wrk.body = buf;

/* Loop through the data in units of the temporary buffer size.             */
            remain = nval;
            while ( ( remain > 0 ) &&
                    ( _ok( hds_gl_status ) ||
                    ( hds_gl_status == DAT__CONER ) ) )
            {
               transfer = _min( nwrk, remain );

/* If the input number format is native to the machine, then perform a type */
/* conversion followed by a format conversion. (Note that a native input    */
/* format also implies a native input storage order.)                       */
               if ( impfnat )
               {
                  dat1_cvt_dtype( bad, transfer, &pdd1, &wrk, &nbad1 );
                  dat1_cvt_format( bad || nbad1, transfer, &wrk, &pdd2,
                                   &nbad2, &hds_gl_status );
               }

/* Otherwise, perform format conversion first and then a type conversion.   */
               else
               {
                  dat1_cvt_format( bad, transfer, &pdd1, &wrk, &nbad1,
                                   &hds_gl_status );
                  dat1_cvt_dtype( bad || nbad1, transfer, &wrk, &pdd2,
                                  &nbad2 );
               }

/* Accumulate the count of conversion errors and increment the input/output */
/* pointers.                                                                */
               *nbad += ( nbad1 + nbad2 );
               remain -= transfer;
               pdd1.body += imp->length * transfer;
               pdd2.body += exp->length * transfer;
            }
         }

/* Deallocate the workspace, ensuring that any errors that occur are        */
/* detected.                                                                */
         temp_status = hds_gl_status;
         if ( hds_gl_status == DAT__CONER )
            hds_gl_status = DAT__OK;
         rec_deall_mem( _max( nwrk * wrklen, 1 ), (void **) &buf );
         if ( _ok( hds_gl_status ) )
            hds_gl_status = temp_status;
      }
   }

/* Return the current global status value.                                  */
   return hds_gl_status;
}
