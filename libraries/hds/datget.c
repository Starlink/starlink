#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+DATGET.C-*/

/* Include files */
#include <stdio.h>                  /* stdio for sprintf() prototype         */

#include "ems.h"                    /* EMS error reporting routines          */

#include "hds1.h"                  /* Global definitions for HDS             */
#include "rec.h"                   /* Public rec_ definitions                */
#include "str.h"                   /* Character string import/export macros  */
#include "dat1.h"                  /* Internal dat_ definitions              */
#include "dat_err.h"               /* DAT__ error code definitions           */

/* Control Blocks */

/* Function prototype.                                                      */

/* F77_INTEGER_FUNCTION(dat_get)( struct STR *locator_str,
 *                                struct STR *type_str,
 *                                F77_INTEGER_TYPE *ndim,
 *                                F77_INTEGER_TYPE *dims,
 *                                unsigned char *values,
 *                                F77_INTEGER_TYPE *status
 *                                TRAIL(locator_str)
 *                                TRAIL(type_str)
 *                                TRAIL(values) )
 */

/*=====================*/
/* DAT_GET - Read data */
/*=====================*/
int
datGet(char       locator_str[DAT__SZLOC],
       char       *type_str,
       int        ndim,
       HDS_PTYPE  dims[],
       unsigned   char *values,
       int        *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_GET_ERR"
#define context_message\
        "DAT_GET: Error reading value(s) from an HDS primitive object."

   struct DSC type;

   struct LCP       *lcp;
   struct LCP_DATA  *data;
   struct LCP_STATE *state;
   char             typbuf[DAT__SZTYP];
   HDS_PTYPE        axis[DAT__MXDIM];
   struct RCL       rcl;
   struct PDD       *app;
   struct PDD       *obj;
   unsigned char    *dom;
   int              naxes;
   INT_BIG          objlen;
   INT_BIG          objoff;
   int              i;
   int              nbad;
   unsigned char    *buf;   /* Pointer to temporary alignment buffer       */
   int fixalign;            /* Is alignment fixup needed?                  */

/* Enter routine.  */

   if (!_ok(*status))
      return *status;
   hds_gl_status      = DAT__OK;

/* Import the type string.  */

   _strcsimp  ( &type, type_str );

/* Import the locator.  */

   dat1_import_loc( locator_str, DAT__SZLOC, &lcp );
   data = &lcp->data;
   state = &data->state;

/* Ensure that there is no currently mapped data and that the object is
   primitive.  */

   if (state->mapped)
      _call(DAT__PRMAP)
   if (data->struc)
      _call(DAT__OBJIN)

/* Determine the shape of the object and match the dimensions.  */

   _call( dau_get_shape( data, &naxes, axis ))
   if (ndim != naxes)
   {
      _call(DAT__DIMIN)
   }
   for (i=0; i<naxes; i++)
   {
      if (dims[i] != axis[i])
      {
         _call(DAT__DIMIN)
      }
   }

/* Validate the application data type specification.  */

   _call( dat1_check_type( &type, typbuf ))

/* Determine the attributes of the application data and reject the operation
   if not primitive.  */

   _call( dat1_unpack_type( typbuf, &data->app ))
   app = &data->app;
   if (app->class != DAT__PRIMITIVE)
      _call(DAT__TYPIN)

/* Match the object and application data attributes and reject the operation
   if the types are incompatible.   */

   obj = &data->obj;
   _call( dau_match_types( obj, app ))

/* Ensure that the object data is 'active'.  */

   _call( rec_get_rcl( &data->han, &rcl ))
   if (!rcl.active)
      _call(DAT__UNSET)

/* Insert a pointer to the data into the PDD.                               */
   app->body = values;

/* Calculate the length (in bytes) of the object data and determine the      */
/* byte offset into the object record's dynamic domain.          */
   objlen = obj->length * data->size;
   objoff = obj->length * data->offset;

/* If the application data type is _DOUBLE and data type or format      */
/* conversion must occur, then test whether the application buffer is      */
/* adequately aligned.                  */
   fixalign = 0;
   if ( ( app->dtype == DAT__D ) &&
        ( ( app->dtype != obj->dtype ) || ( app->format != obj->format ) ) )
   {
      fixalign = !_aligned_D( app->body );
   }

/* If necessary, allocate a correctly aligned buffer to hold the results    */
/* and modify the application primitive data descriptor to point at this    */
/* buffer.                    */
   if ( fixalign )
   {
      rec_alloc_mem( app->length * data->size, (void **) &buf );
      app->body = buf;
   }

/* Gather the object data if discontiguous.            */
   if ( state->broken )
   {
      dau_gather_data( 1, data, &nbad );
   }

/* Otherwise, locate the record's dynamic domain and translate the data to  */
/* the destination buffer.                */
   else
   {
      rec_locate_data( &data->han, objlen, objoff, 'R', &dom );
      obj->body = dom;
      _call(dat1_cvt( 1, data->size, obj, app, &nbad ))
      rec_release_data( &data->han, objlen, objoff, 'R', &dom );
   }

/* If a temporary buffer has been used to ensure correct data alignment,    */
/* copy the results to the true application buffer and deallocate the      */
/* temporary one.                  */
      
    if ( fixalign )
    {
       if ( _ok( hds_gl_status ) )
       {
          (void) memcpy( (void *) values, (void *) buf,
                         (size_t) ( app->length * data->size ) );
       }
       rec_deall_mem( app->length * data->size, (void **) &buf );
    }

/* Check the global status value before return.            */
    _call(hds_gl_status)
    return hds_gl_status;
}

/*
 * F77_INTEGER_FUNCTION(dat_geti)(struct STR *locator_str,
 *                                F77_INTEGER_TYPE *ndim,
 *                                F77_INTEGER_TYPE *dims,
 *                                F77_INTEGER_TYPE *values,
 *                                F77_INTEGER_TYPE *status
 *                                TRAIL(locator_str) )
 */

/*==============================*/
/* DAT_GETI - Read Integer data */
/*==============================*/
int
datGetI(char      locator_str[DAT__SZLOC],
        int       ndim,
        HDS_PTYPE dims[],
        int       values[],
        int       *status) 
{
#undef context_name
#undef context_message
#define context_name "DAT_GETI_ERR"
#define context_message\
        "DAT_GETI: Error reading integer value(s) from an HDS primitive."

   datGet(locator_str, "_INTEGER", ndim, dims, 
                     (unsigned char *)values, status );

   return hds_gl_status;
}

/*===========================*/
/* DAT_GETR - Read REAL data */
/*===========================*/
int
datGetR(char      locator_str[DAT__SZLOC],
        int       ndim,
        HDS_PTYPE dims[],
        float     values[],
        int       *status) 
{
#undef context_name
#undef context_message
#define context_name "DAT_GETR_ERR"
#define context_message\
        "DAT_GETR: Error reading real value(s) from an HDS primitive."

   datGet(locator_str, "_REAL", ndim, dims,
                      (unsigned char *) values, status );

   return hds_gl_status;
}

/*=======================================*/
/* DAT_GETD - Read DOUBLE PRECISION data */
/*=======================================*/
int
datGetD(char      locator_str[DAT__SZLOC],
        int       ndim,
        HDS_PTYPE dims[],
        double    values[],
        int       *status) 
{
#undef context_name
#undef context_message
#define context_name "DAT_GETD_ERR"
#define context_message\
    "DAT_GETD: Error reading double precision value(s) from an HDS primitive."
        
   datGet(locator_str, "_DOUBLE", ndim, dims,
                     (unsigned char *) values, status );

   return hds_gl_status;
}

/*==============================*/
/* DAT_GETL - Read LOGICAL data */
/*==============================*/
int
datGetL(char      locator_str[DAT__SZLOC],
        int       ndim,
        HDS_PTYPE dims[],
        int       values[],
        int       *status) 
{
#undef context_name
#undef context_message
#define context_name "DAT_GETL_ERR"
#define context_message\
        "DAT_GETL: Error reading logical value(s) from an HDS primitive."
        
   datGet(locator_str, "_LOGICAL", ndim, dims,
                     (unsigned char *) values, status );

   return hds_gl_status;
}

/*================================*/
/* DAT_GETC - Read CHARACTER data */
/*================================*/
int
datGetC(char      locator_str[DAT__SZLOC],
        int       ndim,
        HDS_PTYPE dims[],
        char      values[],
        int       char_len,
        int       *status) 
{
#undef context_name
#undef context_message
#define context_name "DAT_GETC_ERR"
#define context_message\
        "DAT_GETC: Error reading character value(s) from an HDS primitive."
   char stype[] = "_CHAR*nnnn";
   sprintf( &stype[6], "%d", char_len ); 
     
   datGet(locator_str, stype, ndim, dims, (char *) values, status );

   return hds_gl_status;
}
