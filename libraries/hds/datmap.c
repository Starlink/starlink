#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+DATMAP.C-*/

#include <stdio.h>
#include <string.h>
#include "ems.h"      /* EMS error reporting routines            */
#include "hds1.h"     /* Global definitions for HDS              */
#include "rec.h"      /* Public rec_ definitions                 */
#include "str.h"      /* Character string import/export macros   */
#include "dat1.h"     /* Internal dat_ definitions               */
#include "dat_err.h"  /* DAT__ error code definitions            */

#include "hds.h"

#if defined( vms )
#include <descrip.h>  /* VMS descriptor definitions              */
#endif

/*====================*/
/* DAT_MAP - Map data */
/*====================*/
int
datMap(const HDSLoc    *locator,
       const char      *type_str,
       const char      *mode_str,
       int       ndim,
       const HDS_PTYPE dims[],
       void      **pntr,
       int       *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_MAP_ERR"
#define context_message\
        "DAT_MAP: Error mapping an HDS primitive"

   GENPTR_POINTER(pntr)
   struct DSC type;
   struct DSC  mode;

   struct   LCP        *lcp;
   struct   LCP_DATA  *data;
   struct   LCP_STATE *state;

   char       typbuf[DAT__SZTYP];
   HDS_PTYPE  axis[DAT__MXDIM];
   struct   RCL rcl;
   struct   PDD *app;
   struct   PDD *obj;
   unsigned char *dom;
   int naxes;
   int reading;
   INT_BIG objlen;
   INT_BIG objoff;
   INT_BIG applen;
   int i;
   int nbad;
   int temp_status;
   void *retpntr;

#if defined( vms )
   struct dsc$descriptor *dsc;
#endif

/* Enter routine.  */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import the type and mode strings.  */

   _strcsimp  ( &type,   type_str );
   _strcsimp  ( &mode,   mode_str );

/* Import the locator.  */

   _call(dat1_import_loc(locator, &lcp ));
   data = &lcp->data;
   state = &data->state;

/* Ensure that there is no currently mapped data and that the object is
   primitive.  */

   if (state->mapped)
      _callnam(DAT__PRMAP)
   if (data->struc)
      _callnam(DAT__OBJIN)

/* Determine the shape of the object and match the dimensions.  */

   _callnam( dau_get_shape(data, &naxes, axis))
   if (ndim != naxes)
      _callnam(DAT__DIMIN)
   for (i=0; i<naxes; i++)
      if (dims[i] != axis[i])
         _callnam(DAT__DIMIN)

/* Validate the application data type, and the access mode. Return if the
   container file was opened for read-only access and the access mode is
   'WRITE' or 'UPDATE'.  */

   _callnam(dat1_check_type(&type, typbuf))
   dat1_check_mode( (const char *) mode.body, mode.length, &data->mode,
                     &hds_gl_status );
   _callnam( hds_gl_status )
   reading = (data->mode != 'W');
   if (data->read && data->mode != 'R')
      _callnam(DAT__ACCON)

/* Determine the attributes of the application data and reject the operation
   if not primitive.  */

   _callnam(dat1_unpack_type(typbuf, &data->app))
   app = &data->app;
   if (app->class != DAT__PRIMITIVE)
      _callnam(DAT__TYPIN)

/* Match the object and applications data attributes and reject the operation
   if the types are incompatible.   */

   obj = &data->obj;
   _callnam(dau_match_types(obj, app))

/* Ensure that the object data is 'active' if reading or updating.      */
   _callnam(rec_get_rcl(&data->han, &rcl))
   if (reading && !rcl.active)
      _callnam(DAT__UNSET)

/* If the mapping is character-type and an explicit character string length */
/* was not supplied (i.e. there was no '*' character in the type            */
/* specification), then modify the application data attributes descriptor   */
/* to describe a character array (or scalar) of an appropriate length to    */
/* hold the character-formatted values.  In this situation the character    */
/* string length of the application's data is simply made identical to that */
/* of the object data if they are both character type.                      */
   if ( app->dtype == DAT__C )
   {
      if ( memchr( (void *) type.body, '*', type.length ) == NULL )
      {
         if ( obj->dtype != DAT__C )
         {
            app->length = dat_gl_ndr[ obj->dtype ].txtsize;
         }
         else
         {
            app->length = obj->length;
         }
      }
   }

/* If the object and application data types differ in any respect, then a   */
/* virtual memory copy of the data is returned to the program. See if this  */
/* is necessary.                                                            */
   state->unlike = ( obj->dtype != app->dtype ) ||
                   ( obj->length != app->length ) ||
                   ( obj->format != app->format ) ||
                   ( obj->order != app->order );
   state->vmcopy = state->unlike;

/* Calculate the length (in bytes) of the virtual memory required for the
   application program data and the corresponding length of the object data.
   Determine the byte offset into the object record's dynamic domain.  */

   applen = app->length * data->size;
   objlen = obj->length * data->size;
   objoff = obj->length * data->offset;

/* Gather discontiguous object data if the program is reading or updating. */

   if (state->broken)
   {
      _callnam( rec_alloc_xmem( applen, (void **) &app->body ) )
      if (reading)
         dau_gather_data( 1, data, &nbad );
   }

/* If a copy of the object data is to be returned, then locate the record's
   dynamic domain and translate the data to the allocated virtual memory. */

   else if (state->vmcopy)
   {
      _callnam( rec_alloc_xmem( applen, (void **) &app->body ) )
      if (reading)
      {
         _callnam(rec_locate_data(&data->han, objlen, objoff, 'R', &dom))
         obj->body = dom;
         dat1_cvt( 1, data->size, obj, app, &nbad );

/* NB workaround for problems with EMS...                                   */
         temp_status = hds_gl_status;
         if ( (hds_gl_status = DAT__CONER) )
            hds_gl_status = DAT__OK;
         rec_release_data(&data->han, objlen, objoff, 'R', &dom);
         if ( _ok( hds_gl_status ) )
            hds_gl_status = temp_status;
      }
   }

/* Otherwise, the application program gains direct access to the object
   data. */

   else
   {
      rec_locate_data(&data->han, objlen, objoff, data->mode, &dom);
      app->body = dom;
   }

/* Save the current setting of the global file-mapping flag, for use when   */
/* the data are un-mapped.                */
   data->filemap = hds_gl_map;

/* Obtain the returned pointer value and note if the object is mapped.      */
   retpntr = (void *) app->body;
   state->mapped = ( retpntr != NULL );

/* If using VMS and the mapped data are character-type, then construct a    */
/* VMS descriptor to describe the mapped values (this descriptor is stored  */
/* in the LCP). Modify the returned pointer to point at this descriptor     */
/* instead.                                                                 */
#if defined( vms )
   if ( app->dtype == DAT__C )
   {
      dsc = &data->vmsdsc;
      dsc->dsc$w_length = app->length;
      dsc->dsc$b_dtype = DSC$K_DTYPE_T;
      dsc->dsc$b_class = ( naxes == 0 ) ? DSC$K_CLASS_S : DSC$K_CLASS_A;
      dsc->dsc$a_pointer = (char *) app->body;
      retpntr = (void *) dsc;
   }
#endif

/* copy pointer to user */
   *pntr = retpntr;

/* If conversion errors occurred, then report contextual information.        */
   if ( hds_gl_status == DAT__CONER )
   {
      emsSeti( "NBAD", nbad );
      emsRep( "DAT_MAP_2",
                 "A total of ^NBAD data conversion error(s) occurred.",
      &hds_gl_status );
   }

   _callnam(hds_gl_status)

   return hds_gl_status;
}

/*=============================*/
/* DAT_MAPI - Map INTEGER data */
/*=============================*/
int
datMapI(const HDSLoc    *locator,
        const char      *mode_str,
        int       ndim,
        const HDS_PTYPE dims[],
        int       **pntr,
        int       *status )
{
#undef context_name
#undef context_message
#define context_name "DAT_MAPI_ERR"
#define context_message\
        "DAT_MAPI: Error mapping HDS primitive as integer values."
   datMap( locator, "_INTEGER", mode_str, ndim, dims,
           (void**)pntr, status );
   return hds_gl_status;
}

/*=============================*/
/* datMapK - Map INT64 data */
/*=============================*/
int
datMapK(const HDSLoc    *locator,
          const char      *mode_str,
          int       ndim,
          const HDS_PTYPE dims[],
          int       **pntr,
          int       *status )
{
#undef context_name
#undef context_message
#define context_name "DAT_MAPK_ERR"
#define context_message\
        "DAT_MAPK: Error mapping HDS primitive as 64-bit int values."
   datMap( locator, "_INT64", mode_str, ndim, dims,
           (void**)pntr, status );
   return hds_gl_status;
}

/*=============================*/
/* DAT_MAPR - Map REAL data */
/*=============================*/
int
datMapR(const HDSLoc *locator,
        const char      *mode_str,
        int       ndim,
        const HDS_PTYPE dims[],
        float     **pntr,
        int       *status )
{
#undef context_name
#undef context_message
#define context_name "DAT_MAPR_ERR"
#define context_message\
        "DAT_MAPR: Error mapping an HDS primitive as real values."
   datMap( locator, "_REAL", mode_str, ndim, dims,
           (void**)pntr, status );
   return hds_gl_status;
}

/*======================================*/
/* DAT_MAPD - Map DOUBLE PRECISION data */
/*======================================*/
int
datMapD(const HDSLoc *locator,
        const char      *mode_str,
        int       ndim,
        const HDS_PTYPE dims[],
        double    **pntr,
        int       *status )
{
#undef context_name
#undef context_message
#define context_name "DAT_MAPD_ERR"
#define context_message\
        "DAT_MAPD: Error mapping an HDS primitive as double precision values."
   datMap( locator, "_DOUBLE", mode_str, ndim, dims,
           (void**)pntr, status );
   return hds_gl_status;
}

/*=============================*/
/* DAT_MAPL - Map LOGICAL data */
/*=============================*/
int
datMapL(const HDSLoc *locator,
        const char      *mode_str,
        int       ndim,
        const HDS_PTYPE dims[],
        int       **pntr,
        int       *status )
{
#undef context_name
#undef context_message
#define context_name "DAT_MAPL_ERR"
#define context_message\
        "DAT_MAPL: Error mapping an HDS primitive as logical values."
   datMap( locator, "_LOGICAL", mode_str, ndim, dims,
           (void**)pntr, status );
   return hds_gl_status;
}

/*===============================*/
/* DAT_MAPC - Map CHARACTER data */
/*===============================*/
int
datMapC(const HDSLoc *locator,
        const char      *mode_str,
        int       ndim,
        const HDS_PTYPE dims[],
        unsigned char **pntr,
        int       *status )
{
#undef context_name
#undef context_message
#define context_name "DAT_MAPC_ERR"
#define context_message\
        "DAT_MAPC: Error mapping an HDS primitive as character values."
   datMap( locator, "_CHAR", mode_str, ndim, dims,
           (void**)pntr, status );
   return hds_gl_status;
}


/*===============================*/
/* DAT_MAPV - Map values associated with an object as if vectorized */
/*===============================*/
int
datMapV(const HDSLoc *locator,
	const char      *type_str,
        const char      *mode_str,
        void      **pntr,
	size_t    *actval,
        int       *status )
{
#undef context_name
#undef context_message
#define context_name "DAT_MAPV_ERR"
#define context_message\
        "DAT_MAPV: Error mapping an HDS vectorized primitive."

  /* Local variables */
  hdsdim dims[DAT__MXDIM];
  int    ndim;

  /* Initialise return values */
  *pntr = NULL;
  *actval = 0;

  datSize( locator, actval, status );
  datShape( locator, DAT__MXDIM, dims, &ndim, status );
  datMap( locator, type_str, mode_str, ndim, dims,
	  pntr, status );
  return hds_gl_status;
}



/*===============================================*/
/* DAT_BASIC - Map data (in basic machine units) */
/*===============================================*/

int
datBasic(const HDSLoc *locator,
         const char *mode_c,
         unsigned char **pntr,
         int *len,
         int *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_BASIC_ERR"
#define context_message\
        "DAT_BASIC: Error mapping an HDS primitive as basic machine units."

   struct DSC mode;

   struct LCP       *lcp;
   struct LCP_DATA  *data;
   struct LCP_STATE *state;
   struct RCL       rcl;
   struct PDD       *app;
   struct PDD       *obj;

   unsigned char *dom;
   int reading;
   INT_BIG objlen;
   INT_BIG objoff;
   int nbad;
   unsigned char *retpntr;

/* Enter routine.  */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import the mode string.  */

   _strcsimp( &mode,    mode_c );

/* Import the locator.  */

   _call(dat1_import_loc(locator, &lcp ));
   data  = &lcp->data;
   state = &data->state;

/* Ensure that there is no currently mapped data and that the object is
   primitive.  */

   if (state->mapped)
      _callnam(DAT__PRMAP)
   if (data->struc)
      _callnam(DAT__OBJIN)

/* Validate the access mode. Return if the container file was opened for
   read-only access and the mode is 'WRITE' or 'UPDATE'.  */

   dat1_check_mode( (const char *) mode.body, mode.length, &data->mode,
                    &hds_gl_status );
   _callnam( hds_gl_status )
   reading = (data->mode != 'W');
   if (data->read && data->mode != 'R')
      _callnam(DAT__ACCON)

/* Copy the object data attributes to the application data attributes
   descriptor.  */

   data->app = data->obj;
   app = &data->app;
   obj = &data->obj;

/* Ensure that the object data is 'active' if reading or updating.      */

   _callnam( rec_get_rcl(&data->han, &rcl) )
   if (reading && !rcl.active)
      _callnam(DAT__UNSET)
   state->vmcopy = 0;

/* Calculate the length (in bytes) of the virtual memory required for the
   object data. Determine the byte offset into the object record's dynamic
   domain.  */

   objlen = obj->length * data->size;
   objoff = obj->length * data->offset;

/* Gather discontiguous object data if the program is reading or updating. */

   if (state->broken)
   {
      _callnam( rec_alloc_xmem( objlen, (void **) &app->body ) )
      if (reading)
         dau_gather_data( 1, data, &nbad );
   }

/* Otherwise, the application program gains direct access to the object data. */

   else
   {
      rec_locate_data( &data->han, objlen, objoff, data->mode, &dom );
      app->body = dom;
   }

/* Save the current setting of the global file-mapping flag, for use when   */
/* the data are un-mapped.                */
   data->filemap = hds_gl_map;

/* Obtain the returned pointer value and its length. Note if the object is  */
/* mapped.                    */
   retpntr = app->body;
   *len    = objlen;
   state->mapped = ( retpntr != NULL );

/* Copy c pointer to user */
    *pntr = retpntr;

   _callnam(hds_gl_status)

   return hds_gl_status;
}

/*========================*/
/* DAT_UNMAP - Unmap data */
/*========================*/
int
datUnmap(const HDSLoc *locator,
         int  *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_UNMAP_ERR"
#define context_message\
        "DAT_UNMAP: Error unmapping an HDS primitive."

   struct LCP      *lcp;
   struct LCP_DATA *data;

/* Enter routine and import the locator.  */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import locator.  */

   _call(dat1_import_loc(locator, &lcp ));
   data = &lcp->data;

/* Return if the locator is associated with a structure.        */

   if (data->struc)
      _callnam(DAT__OBJIN)

/* Otherwise, flush any mapped data.                                        */
   else
   {
      dau_flush_data( data );
      _callnam( hds_gl_status );
   }
   return hds_gl_status;
}
