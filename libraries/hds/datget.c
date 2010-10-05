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

#include "hds.h"

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
datGet(const HDSLoc     *locator,
       const char       *type_str,
       int        ndim,
       const HDS_PTYPE  dims[],
       void       *values,
       int        *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_GET_ERR"
#define context_message \
       "DAT_GET: Error reading value(s) from an HDS primitive object"
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

   _callnam( dau_get_shape( data, &naxes, axis ))
   if (ndim != naxes)
   {
      _callnam(DAT__DIMIN)
   }
   for (i=0; i<naxes; i++)
   {
      if (dims[i] != axis[i])
      {
         _callnam(DAT__DIMIN)
      }
   }

/* Validate the application data type specification.  */

   _callnam( dat1_check_type( &type, typbuf ))

/* Determine the attributes of the application data and reject the operation
   if not primitive.  */

   _callnam( dat1_unpack_type( typbuf, &data->app ))
   app = &data->app;
   if (app->class != DAT__PRIMITIVE)
      _callnam(DAT__TYPIN)

/* Match the object and application data attributes and reject the operation
   if the types are incompatible.   */

   obj = &data->obj;
   _callnam( dau_match_types( obj, app ))

/* Ensure that the object data is 'active'.  */

   _callnam( rec_get_rcl( &data->han, &rcl ))
   if (!rcl.active)
      _callnam(DAT__UNSET)

/* Insert a pointer to the data into the PDD.                               */
   app->body = (unsigned char*)values;

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
      _callnam(dat1_cvt( 1, data->size, obj, app, &nbad ))
      rec_release_data( &data->han, objlen, objoff, 'R', &dom );
   }

/* If a temporary buffer has been used to ensure correct data alignment,    */
/* copy the results to the true application buffer and deallocate the      */
/* temporary one.                  */

    if ( fixalign )
    {
       if ( _ok( hds_gl_status ) )
       {
          (void) memcpy( values, buf,
                         (size_t) ( app->length * data->size ) );
       }
       rec_deall_mem( app->length * data->size, (void **) &buf );
    }

/* Check the global status value before return.            */
    _callnam(hds_gl_status)
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
datGetI(const HDSLoc    *locator,
        int       ndim,
        const HDS_PTYPE dims[],
        int       values[],
        int       *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_GETI_ERR"
#define context_message\
        "DAT_GETI: Error reading integer value(s) from an HDS primitive."

   datGet(locator, "_INTEGER", ndim, dims,
                     values, status );

   return hds_gl_status;
}

/*====================================*/
/* DAT_GETW - Read Short Integer data */
/*====================================*/
int
datGetW(const HDSLoc    *locator,
        int       ndim,
        const HDS_PTYPE dims[],
        short       values[],
        int       *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_GETW_ERR"
#define context_message\
        "DAT_GETW: Error reading short integer value(s) from an HDS primitive."

   datGet(locator, "_WORD", ndim, dims,
                     values, status );

   return hds_gl_status;
}

/*==============================================*/
/* DAT_GETUW - Read Unsigned Short Integer data */
/*==============================================*/
int
datGetUW(const HDSLoc    *locator,
        int       ndim,
        const HDS_PTYPE dims[],
        unsigned short  values[],
        int       *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_GETUW_ERR"
#define context_message\
        "DAT_GETW: Error reading unsigned short integer value(s) from an HDS primitive."

   datGet(locator, "_UWORD", ndim, dims,
                     values, status );

   return hds_gl_status;
}

/*===========================*/
/* DAT_GETR - Read REAL data */
/*===========================*/
int
datGetR(const HDSLoc    *locator,
        int       ndim,
        const HDS_PTYPE dims[],
        float     values[],
        int       *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_GETR_ERR"
#define context_message\
        "DAT_GETR: Error reading real value(s) from an HDS primitive."

   datGet(locator, "_REAL", ndim, dims,
                      (unsigned char *) values, status );

   return hds_gl_status;
}

/*=======================================*/
/* DAT_GETD - Read DOUBLE PRECISION data */
/*=======================================*/
int
datGetD(const HDSLoc    *locator,
        int       ndim,
        const HDS_PTYPE dims[],
        double    values[],
        int       *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_GETD_ERR"
#define context_message\
    "DAT_GETD: Error reading double precision value(s) from an HDS primitive."

   datGet(locator, "_DOUBLE", ndim, dims,
                     values, status );

   return hds_gl_status;
}

/*==============================*/
/* DAT_GETL - Read LOGICAL data */
/*==============================*/
int
datGetL(const HDSLoc    *locator,
        int       ndim,
        const HDS_PTYPE dims[],
        int       values[],
        int       *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_GETL_ERR"
#define context_message\
        "DAT_GETL: Error reading logical value(s) from an HDS primitive."

   datGet(locator, "_LOGICAL", ndim, dims,
                     values, status );

   return hds_gl_status;
}

/*================================*/
/* DAT_GETC - Read CHARACTER data */
/*================================*/
int
datGetC(const HDSLoc    *locator,
        int       ndim,
        const HDS_PTYPE dims[],
        char      values[],
        size_t    char_len,
        int       *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_GETC_ERR"
#define context_message\
        "DAT_GETC: Error reading character value(s) from an HDS primitive."
   char stype[DAT__SZTYP+1];

   datCctyp( char_len, stype );
   datGet(locator, stype, ndim, dims, values, status );

   return hds_gl_status;
}

/*==========================================================*/
/*                                                          */
/*                          GET 1x                          */
/*                                                          */
/*==========================================================*/

/*==================================*/
/* DAT_GET1D - Read 1D Double array */
/*==================================*/

int
datGet1D( const HDSLoc * locator,
	  size_t maxval,
	  double values[],
	  size_t *actval,
	  int * status ) {

  hdsdim dims[1];

  if (*status != DAT__OK) return *status;

  datSize( locator, actval, status );

  if ( *status == DAT__OK && maxval < *actval ) {
    *status = DAT__BOUND;
    emsSeti( "IN", (int)maxval );
    emsSeti( "SZ", (int)*actval );
    emsRep( "DAT_GET1D_ERR", "datGet1D: Bounds mismatch: ^IN < ^SZ", status);
  } else {
    dims[0] = *actval;
    datGetD( locator, 1, dims, values, status );
  }
  return *status;
}

/*==================================*/
/* DAT_GET1I - Read 1D Double array */
/*==================================*/

int
datGet1I( const HDSLoc * locator,
	  size_t maxval,
	  int values[],
	  size_t *actval,
	  int * status ) {

  hdsdim dims[1];

  if (*status != DAT__OK) return *status;

  datSize( locator, actval, status );

  if ( *status == DAT__OK && maxval < *actval ) {
    *status = DAT__BOUND;
    emsSeti( "IN", (int)maxval );
    emsSeti( "SZ", (int)*actval );
    emsRep( "DAT_GET1I_ERR", "datGet1I: Bounds mismatch: ^IN < ^SZ", status);
  } else {
    dims[0] = *actval;
    datGetI( locator, 1, dims, values, status );
  }
  return *status;
}

/*==================================*/
/* DAT_GET1R - Read 1D float array */
/*==================================*/

int
datGet1R( const HDSLoc * locator,
	  size_t maxval,
	  float values[],
	  size_t *actval,
	  int * status ) {

  hdsdim dims[1];

  if (*status != DAT__OK) return *status;

  datSize( locator, actval, status );

  if ( *status == DAT__OK && maxval < *actval ) {
    *status = DAT__BOUND;
    emsSeti( "IN", (int)maxval );
    emsSeti( "SZ", (int)*actval );
    emsRep( "DAT_GET1R_ERR", "datGet1R: Bounds mismatch: ^IN < ^SZ", status);
  } else {
    dims[0] = *actval;
    datGetR( locator, 1, dims, values, status );
  }
  return *status;
}

/*==================================*/
/* DAT_GET1L - Read 1D Logical array */
/*==================================*/

int
datGet1L( const HDSLoc * locator,
	  size_t maxval,
	  int values[],
	  size_t *actval,
	  int * status ) {

  hdsdim dims[1];

  if (*status != DAT__OK) return *status;

  datSize( locator, actval, status );

  if ( *status == DAT__OK && maxval < *actval ) {
    *status = DAT__BOUND;
    emsSeti( "IN", (int)maxval );
    emsSeti( "SZ", (int)*actval );
    emsRep( "DAT_GET1L_ERR", "datGet1L: Bounds mismatch: ^IN < ^SZ", status);
  } else {
    dims[0] = *actval;
    datGetL( locator, 1, dims, values, status );
  }
  return *status;
}

/*==========================================================*/
/*                                                          */
/*                          GET Vx                          */
/*                                                          */
/*==========================================================*/

/*==========================================*/
/* DAT_GETVD - Read vectorized Double array */
/*==========================================*/

int
datGetVD( const HDSLoc * locator,
	  size_t maxval,
	  double values[],
	  size_t *actval,
	  int * status ) {
  HDSLoc * vec = NULL;
  if (*status != DAT__OK) return *status;
  datVec(locator, &vec, status );
  datGet1D( vec, maxval, values, actval, status );
  datAnnul( &vec, status );
  return *status;
}
/*==========================================*/
/* DAT_GETVD - Read vectorized Double array */
/*==========================================*/

int
datGetVI( const HDSLoc * locator,
	  size_t maxval,
	  int values[],
	  size_t *actval,
	  int * status ) {
  HDSLoc * vec = NULL;
  if (*status != DAT__OK) return *status;
  datVec(locator, &vec, status );
  datGet1I( vec, maxval, values, actval, status );
  datAnnul( &vec, status );
  return *status;
}

/*==========================================*/
/* DAT_GETVR - Read vectorized REAL array */
/*==========================================*/

int
datGetVR( const HDSLoc * locator,
	  size_t maxval,
	  float values[],
	  size_t *actval,
	  int * status ) {
  HDSLoc * vec = NULL;
  if (*status != DAT__OK) return *status;
  datVec(locator, &vec, status );
  datGet1R( vec, maxval, values, actval, status );
  datAnnul( &vec, status );
  return *status;
}

/*==========================================*/
/* DAT_GETVL - Read vectorized Logical array */
/*==========================================*/

int
datGetVL( const HDSLoc * locator,
	  size_t maxval,
	  int values[],
	  size_t *actval,
	  int * status ) {
  HDSLoc * vec = NULL;
  if (*status != DAT__OK) return *status;
  datVec(locator, &vec, status );
  datGet1L( vec, maxval, values, actval, status );
  datAnnul( &vec, status );
  return *status;
}
