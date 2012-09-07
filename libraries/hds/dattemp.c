#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* C include files:                                                         */
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

/* POSIX include files:                                                     */
/* ===================                                                      */
#if !defined( vms )
#include <sys/types.h>
#include <unistd.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Private rec_ definitions                */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

#include "hds.h"

/* Static variables     */

static struct HAN       temp_handle;/* Handle to temp component list */
static int temp_created;

/*====================================*/
/* DAT_TEMP - Create temporary object */
/*====================================*/

int
datTemp(const char      *type_str,
        int       ndim,
        const HDS_PTYPE dims[],
        HDSLoc    **locator,
        int       *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_TEMP_ERR"
#define context_message\
        "DAT_TEMP: Error creating a temporary HDS object."

   struct DSC  type;

   struct LCP      *lcp;
   struct LCP_DATA *data;
   unsigned char   *crv;
   HDS_PTYPE       (*dbt)[2];
   struct PDD      *obj;
   struct RCL      rcl;
   struct ODL      odl;
   int             ncomp;
   int             i;
   int             clash;
   int             tmp_save;
   char            *name1;
   struct RID      rid1;
   char buf[ DAT__SZNAM + 1 ];      /* Buffer for formatting object name     */

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status   = DAT__OK;

/* Import type string */

   _strcsimp(&type,type_str);

/* Export locator.      */

   _call(dat1_alloc_lcp(locator, &lcp ))
   data  = &lcp->data;

/* Create a temporary container file if not yet done.   */

   if ( !temp_created )
   {
/* This is a sticking plaster - fix!!!!!!!!!!!!!!!!!!! bkm 20060306 */
      if( rec_gl_endslot > 1) {
         hds_gl_64bit = rec_ga_fcv[rec_gl_endslot-1].hds_version >
                        REC__VERSION3;
         tmp_save = hds_gl_c64bit;
         hds_gl_c64bit = hds_gl_64bit;
      } else {
         hds_gl_64bit = hds_gl_c64bit;
         tmp_save = hds_gl_c64bit;
      }
      _call( dat1_make_scratch( ) )
      temp_created = 1;
      hds_gl_c64bit = tmp_save;
   }
   else
   {
      hds_gl_64bit = ( rec_ga_fcv[temp_handle.slot].hds_version >
                       REC__VERSION3 );
   }

/* Verify the object shape and type and determine the attributes.       */

   _call(dau_check_shape(ndim, dims, &odl))
   _call(dat1_check_type(&type, data->type))
   _call(dat1_unpack_type(data->type, &data->obj))
   obj = &data->obj;

/* Save the shape information in the LCP and calculate the total size
   of the object. (The Dimension Bounds Table is used to hold the 1st
   three axis sizes).   */

   data->naxes = odl.naxes;
   dbt         = data->bounds;
   data->size  = 1;
   for (i=0; i<data->naxes; i++)
   {
      data->size *= dims[i];
      if (i < DAT__MXSLICE)
      {
         dbt[i][LOWER] = 1;
         dbt[i][UPPER] = dims[i];
      }
   }

/* Expand the temporary structure Component Record Vector if necessary. */

   dat1_get_ncomp(&temp_handle, &ncomp);
   rec_get_rcl(&temp_handle, &rcl);
   if (ncomp*SZCRV == rcl.dlen)
      _call(rec_extend_record(&temp_handle,
                              SZCRV * hds_gl_ncomp0))

/* Create the component name of the new object from the temporary object    */
/* count (the name is of the form 'TEMP_nn...' padded with blanks to a      */
/* length of DAT__SZNAM). Increment the object count before use. Note we    */
/* must use an intermediate buffer because sprintf writes an extra '\0'     */
/* after the characters we want.                                            */
   (void) sprintf( buf, "TEMP_%-*d", DAT__SZNAM - 5, ++hds_gl_ntemp );
   (void) memcpy( (void *) data->name, (const void *) buf,
                  (size_t) DAT__SZNAM );

/* If temporary objects currently exist, then locate the Component Record   */
/* Vector and check that the new name does not clash with an existing one.  */
   if ( ncomp > 0 )
   {
      _call( rec_locate_data( &temp_handle, rcl.dlen, 0, 'R', &crv ) );
      clash = 1;
      while( clash )
      {
         for ( i = 0; i < ncomp; i++ )
         {
            dat1_locate_name( crv, i, &name1 );
            clash = _cheql( DAT__SZNAM, data->name, name1 );

/* If the name clashes, then generate a new one and start checking again.   */
            if ( clash )
            {
               (void) sprintf( buf, "TEMP_%-*d", DAT__SZNAM - 5,
                               ++hds_gl_ntemp );
               (void) memcpy( (void *) data->name, (const void *) buf,
                              (size_t) DAT__SZNAM );
                break;
            }
         }
      }

/* Ditch the CRV when done.                                                 */
      rec_release_data(&temp_handle, rcl.dlen, 0, 'R', &crv);
   }

/* Fill the appropriate fields in the Record Control Label for the new object
   and create the record.       */

   rcl.class =  obj->class;
   rcl.zero  = (obj->class == DAT__STRUCTURE);
   rcl.slen  =  DAT__SZTYP + DAT__SZNDIM + ( ndim * DAT__SZDIM);
   rcl.dlen  =  data->size * obj->length;
   _call(rec_create_record(&temp_handle, &rcl, &data->han))
   data->parent = rcl.parent;

/* Save the type specification and write the Object Descriptor Label to the
   record's static domain.      */

   _chmove(DAT__SZTYP, data->type, odl.type);
   dat1_put_odl(&data->han, &odl);

/* Map the Component Record Vector and save the name and Record-ID of the
   new temporary component object.      */

   rec_locate_data(&temp_handle, SZCRV, ncomp*SZCRV, 'W', &crv);
dat1_locate_name( crv, 0, &name1 );
   _chmove( DAT__SZNAM, data->name, name1 );
   rec_get_rid( &data->han, &rid1 );
   dat1_pack_crv( &rid1, 0, crv );
   rec_release_data(&temp_handle, SZCRV, ncomp*SZCRV, 'W', &crv);

/* Reset the component count, increment the level and flag whether the new
   object is structure or primitive, then mark the locator as valid.    */

   ++ncomp;
   dat1_put_ncomp(&temp_handle, ncomp);
   data->level = 1;
   data->struc = (obj->class == DAT__STRUCTURE);
   data->valid = 1;
   return hds_gl_status;
}

int
dat1_make_scratch(void)

/*+
 * MAKE_SCRATCH - Create scratch container file
 *
 * This routine creates a temporary container file and, as its top-level object,
 * a structure of type 'HDS_SCRATCH'. The file is created in the process scratch
 * directory and, to prevent conflict with the user's current files, is named
 * after the process-ID.
 *
 * Calling sequence:
 *
 *        MAKE_SCRATCH()
 *
 * Routine value:
 *
 *        DAT__OK    if successful
 */

{
   char fname[ 256 ];
   char *prefix;
   struct DSC    file;
   unsigned char *crv;
   unsigned char *srv;
   struct ODL    odl;
   struct RCL    rcl;
   struct HAN    han[2];
   int nc;                          /* Number of characters written          */
   int ncomp;
   struct RID rid;
   char *name1;
   struct RID rid1;
   int refcnt;

/* Initialise string descriptors.       */

   _dscinit( &file, 0, fname );

/* Create a unique file name from the process ID, including the directory   */
/* prefix, if available.                                                    */
#if defined( vms )
   (void) sprintf( (char *) file.body, "HDS_SCRATCH:T%X%n", getpid( ), &nc );
#else
   prefix = getenv( "HDS_SCRATCH" );
   if ( prefix == NULL )
   {
      nc = sprintf( (char *) file.body, "t%x", getpid( ) );
   }
   else
   {
      nc = sprintf( (char *) file.body, "%s/t%x", prefix, getpid( ) );
   }
#endif
   file.length = nc;

/* Create the scratch file.                                                 */
   rcl.class = DAT__CONTAINER;
   rcl.zero  = 1;
   rcl.slen  = 0;


/* Set 64-bit file format flag appropriately                            */
/*   hds_gl_64bit = hds_gl_c64bit; */

   rcl.dlen  = SZCRV;

   _invoke( rec_attach_file( 1, (const char *) file.body, file.length, 'S',
                             'W', &rcl, &han[ 0 ] ) );
   rec_refcnt( &han[ 0 ], 1, &refcnt, &hds_gl_status );

/* Create a top-level structure record. */
   rcl.class = DAT__STRUCTURE;
   rcl.zero  = 1;
   rcl.slen  = DAT__SZTYP + DAT__SZNDIM;
   rcl.dlen  = SZSRV;
   _invoke(rec_create_record(&han[0], &rcl, &han[1]));

/* Locate the container record and enter the name and record-ID of the      */
/* top-level structure.                                                     */
   rec_locate_data( &han[ 0 ], SZCRV, 0, 'W', &crv );
   dat1_locate_name( crv, 0, &name1 );
   _chcopy( 11, "HDS_SCRATCH", ' ', DAT__SZNAM, name1 );
   rec_get_rid( &han[ 1 ], &rid1 );
   dat1_pack_crv( &rid1, 0, crv );
   rec_release_data( &han[ 0 ], SZCRV, 0, 'W', &crv );

/* Create a component record and zeroise the count.     */

   rcl.class = DAT__COMPONENT;
   rcl.zero     = 0;
   rcl.slen     = DAT__SZNCOMP;
   rcl.dlen     = SZCRV * hds_gl_ncomp0;
   _invoke(rec_create_record(&han[1], &rcl, &temp_handle));
   ncomp = 0;
   dat1_put_ncomp(&temp_handle, ncomp);

/* Locate the structure record and save the ID of the component record. */

   rec_locate_data(&han[1], SZSRV, 0, 'W', &srv);
   rec_get_rid(&temp_handle, &rid);
   dat1_pack_srv( &rid, srv );
   rec_release_data(&han[1], SZSRV, 0, 'W', &srv);

/* Write the structure's Object Descriptor Label.       */

   _chcopy( 11, "HDS_SCRATCH", ' ', DAT__SZTYP, odl.type );
   odl.naxes = 0;
   dat1_put_odl(&han[1], &odl);

   return hds_gl_status;
}
