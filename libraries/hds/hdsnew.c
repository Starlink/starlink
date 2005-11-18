#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

#include "hds.h"

/*=====================================*/
/* HDS_NEW - Create new container file */
/*=====================================*/

int
hdsNew(char *file_str,
       char *name_str,
       char *type_str,
       int  ndim,
       HDS_PTYPE  dims[],
       HDSLoc **locator,
       int *status)
{

/* RFWS: Make rcl.zero be zero - otherwise it contains junk */

#undef context_name
#undef context_message
#define context_name "HDS_NEW_ERR"
#define context_message\
        "HDS_NEW: Error creating a new HDS container file."

   struct DSC file;
   struct DSC name;
   struct DSC type;

   struct LCP      *lcp;
   struct LCP_DATA *data;
   unsigned char   *crv;
   HDS_PTYPE       (*dbt)[2];
   struct PDD      *obj;
   struct RCL      rcl;
   struct ODL      odl;
   struct HAN      han;
   int             i;
   char            *name1;
   struct RID      rid1;
   int             refcnt;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import file, name and type strings.        */

   _strcsimp( &file, file_str );
   _strcsimp( &name, name_str );
   _strcsimp( &type, type_str );

/* Export the locator.  */

   _call( dat1_alloc_lcp(locator, &lcp ) )
   data = &lcp->data;

/* Set 64-bit file format flag appropriately                            */
   hds_gl_64bit = hds_gl_c64bit;
   
/* Validate the object name and type.   */

   _call( dau_check_name(  &name, data->name ))
   _call( dat1_check_type( &type, data->type ))

/* Determine the object attributes and validate the shape.      */

   _call( dat1_unpack_type( data->type, &data->obj ))
   obj = &data->obj;
   _call( dau_check_shape( ndim, dims, &odl ))

/* Save the shape information in the LCP and calculate the total size
   of the object. (The Dimension Bounds Table holds the 1st 3 axis sizes). */

   data->naxes = odl.naxes;
   dbt         = data->bounds;
   data->size  = 1;
   for (i=0; i<data->naxes; i++)
   {
      data->size *= dims[i];
      if ( i<DAT__MXSLICE)
      {
         dbt[i][LOWER] = 1;
         dbt[i][UPPER] = dims[i];
      }
   }

/* Attach to a new file and return a handle to the container record.    */

   rcl.zero = 1;
   rcl.class = DAT__CONTAINER;
   rcl.slen = 0;
   rcl.dlen = SZCRV;
   rec_attach_file( 1, (const char *) file.body, file.length, 'N', 'W', &rcl,
                    &han );
   _call( hds_gl_status )

/* Setup the Record Control Label for the top-level object and create
   the record.  */

   rcl.class = obj->class;
   rcl.zero  = (obj->class == DAT__STRUCTURE);
   rcl.slen  = DAT__SZTYP + DAT__SZNDIM + ( ndim * DAT__SZDIM );
   rcl.dlen  = data->size * obj->length;
   _call( rec_create_record( &han, &rcl, &data->han ))
   data->parent = rcl.parent;

/* Save the type specification and write the Object Descriptor Label
   to the record's static domain.       */

   _chmove( DAT__SZTYP, data->type, odl.type );
   _call( dat1_put_odl( &data->han, &odl ))

/* Map the container record's dynamic domain and save the Record-ID of
   the new top-level object record.     */

   _call(rec_locate_data( &han, SZCRV, 0, 'W', &crv ))
   dat1_locate_name( crv, 0, &name1 );
   _chmove( DAT__SZNAM, data->name, name1 );
   rec_get_rid( &data->han, &rid1 );
   dat1_pack_crv( &rid1, 0, crv );
   rec_release_data( &han, SZCRV, 0, 'W', &crv );

/* Flag whether structure or primitive.                                     */
   data->struc = (obj->class == DAT__STRUCTURE);

/* Make the output locator a primary locator, increment the container file  */
/* reference count and mark the locator as valid.                           */
   lcp->primary = 1;
   rec_refcnt( &han, 1, &refcnt, &hds_gl_status );
   data->valid = 1;

   return hds_gl_status;
}
