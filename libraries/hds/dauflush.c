#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+DAUFLUSH.C-*/

/* Include files */

#include "ems.h"                 /* EMS error reporting routines            */

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

/* Control Blocks */

int
dau_flush_data(struct LCP_DATA *data)

/*+
 * DAU_FLUSH_DATA - Flush mapped data
 *
 * This routine will unmap any primitive data that is currently mapped to the
 * specified  locator.  If  the  data object is a discontiguous slice, then a
 * scatter write-back is performed if mapped in either 'WRITE' or 'UPDATE' mode.
 *
 * Calling sequence:
 *
 *        DAU_FLUSH_DATA(DATA)
 *
 * DATA    is the address of the data part of the Locator Control Packet.
 *
 * Routine value:
 *
 *        DAT__OK    if successful.
 */

{
   struct LCP_STATE        *state;
   struct PDD              *app;
   struct PDD              *obj;
   unsigned char *dom;
   int                      writing;
   INT_BIG                  objlen;
   INT_BIG                  objoff;
   INT_BIG                  applen;
   int nbad;
   int mapsave;

/* Return if no data currently mapped.  */
   state = &data->state;
   if ( !state->mapped )
      return hds_gl_status;

/* Begin a new error reporting context.                                     */
   emsBegin( &hds_gl_status );

/* Set the global file mapping flag to the value used when the data were    */
/* originally mapped.                                                       */
   mapsave = hds_gl_map;
   hds_gl_map = data->filemap;

/* Associate the application data and object data attributes descriptors. */

   app     = &data->app;
   obj     = &data->obj;
   writing = (data->mode != 'R');

/* Calculate the length (in bytes) of the virtual memory allocated to the
   application program data and the corresponding length of the object data.
   Determine the byte-offset into the object record's dynamic domain.   */

   applen  = app->length * data->size;
   objlen  = obj->length * data->size;
   objoff  = obj->length * data->offset;

/* Scatter discontiguous object data if the program is writing or updating. */

   if (state->broken)
   {
      if (writing)
      {
         dau_scatter_data(1, data, &nbad );

/* If conversion errors occurred, then report contextual information.       */
         if ( hds_gl_status == DAT__CONER )
         {
            ems_seti_c( "NBAD", nbad );
            emsRep( "DAU_FLUSH_1",
                       "A total of ^NBAD data conversion error(s) occurred.",
                       &hds_gl_status );
         }
     }
     rec_deall_xmem( applen, (void **) &app->body );
  }

/* If a copy of the object data was given to the program, then locate the
   record's dynamic domain and translate the data from the virtual memory copy.
 */

   else if (state->vmcopy)
   {
      if (writing)
      {
         rec_locate_data(&data->han, objlen, objoff, 'W', &dom);
         obj->body = dom;
         dat1_cvt( 1, data->size, app, obj, &nbad );

/* If conversion errors occurred, then report contextual information.       */
         if ( hds_gl_status == DAT__CONER )
         {
            ems_seti_c( "NBAD", nbad );
            emsRep( "DAU_FLUSH_2",
                       "A total of ^NBAD data conversion error(s) occurred.",
                       &hds_gl_status );
         }

         rec_release_data(&data->han, objlen, objoff, 'W', &dom);
      }
      rec_deall_xmem( applen, (void **) &app->body );
   }

/* Otherwise, the application program was given direct access to the data. */

   else
   {
      dom = app->body;
      rec_release_data(&data->han, objlen, objoff, data->mode, &dom);
   }

/* Clear the pointer and the map flags. */

   app->body          = 0;
   state->mapped      = 0;
   state->unlike      = 0;
   state->vmcopy      = 0;

/* Restore the global file mapping flag.                                    */
   hds_gl_map = mapsave;

/* End the error reporting context.                                         */
   emsEnd( &hds_gl_status );

   return hds_gl_status;
}
