#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+DATPUT.C-*/

/* Include files */

#include <stdio.h>                  /* stdio for sprintf() prototype         */

#include "ems.h"                    /* EMS error reporting routines          */

#include "hds1.h"                   /* Global definitions for HDS            */
#include "rec.h"                    /* Public rec_ definitions               */
#include "str.h"                    /* Character string import/export macros */
#include "dat1.h"                   /* Internal dat_ definitions             */
#include "dat_err.h"                /* DAT__ error code definitions          */

#include "hds.h"

/* Control Blocks */

/* Function prototype.                                                      */

/*======================*/
/* DAT_PUT - Write data */
/*======================*/
int
datPut( const HDSLoc   *locator,
        const char     *type_str,
        int      ndim,
        const HDS_PTYPE  dims[],
        const void     *values,
        int      *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_PUT_ERR"
#define context_message\
        "DAT_PUT: Error writing value(s) to an HDS primitive."

   struct DSC type;

   struct LCP       *lcp;
   struct LCP_DATA  *data;
   struct LCP_STATE *state;
   char             typbuf[DAT__SZTYP];
   HDS_PTYPE        axis[DAT__MXDIM];
   struct PDD       *app;
   struct PDD       *obj;
   unsigned char    *dom;
   int              naxes;
   INT_BIG          objlen;
   INT_BIG          objoff;
   int              i;
   int              nbad;
   unsigned char    *buf;       /* Pointer to temporary alignment buffer   */
   int              fixalign;   /* Is alignment fixup needed?              */

/* Enter routine.	*/

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import the type string.	*/

   _strcsimp( &type, type_str );

/* Import the locator.	*/

   _call(dat1_import_loc(locator, &lcp ));
   data  = &lcp->data;
   state = &data->state;

/* Ensure that there is no currently mapped data and that the object is
   primitive. Also check that the container file was not opened for read-
   only access.	*/

   if (state->mapped)
      _call(DAT__PRMAP)
   if (data->struc)
      _call(DAT__OBJIN)
   if (data->read)
      _call(DAT__ACCON)

/* Determine the shape of the object and match the dimensions.	*/

   _call( dau_get_shape( data, &naxes, axis ) )
   if (ndim != naxes)
      _call(DAT__DIMIN)
   for (i=0; i<naxes; ++i)
      if (dims[i] != axis[i])
         _call(DAT__DIMIN)

/* Validate the application data type specification.	*/

   _call( dat1_check_type( &type, typbuf ) )

/* Determine the attributes of the application data and reject the operation
   if not primitive.    */

   _call( dat1_unpack_type( typbuf, &data->app ) )
   app = &data->app;
   if (app->class != DAT__PRIMITIVE)
      _call(DAT__TYPIN)

/* Match the object and application data attributes and reject the operation
   if the types are incompatible. */

   obj = &data->obj;
   _call( dau_match_types( obj, app ) )

/* Insert a pointer to the data into the PDD.                                */
     app->body = (unsigned char*)values;

/* Calculate the length (in bytes) of the object data and determine the      */
/* byte offset into the object record's dynamic domain.                      */
   objlen = obj->length * data->size;
   objoff = obj->length * data->offset;

/* If the application data type is _DOUBLE and data type or format           */
/* conversion must occur, then test whether the application buffer is        */
/* adequately aligned.                                                       */
   fixalign = 0;
   if ( ( app->dtype == DAT__D ) &&
        ( ( app->dtype != obj->dtype ) || ( app->format != obj->format ) ) )
   {
      fixalign = !_aligned_D( app->body );
   }

/* If necessary, allocate a correctly aligned buffer to hold the             */
/* application data and copy the data into it. Modify the application        */
/* primitive data descriptor to point at this buffer.                        */
   if ( fixalign )
   {
      rec_alloc_mem( app->length * data->size, (void **) &buf );
      if ( _ok( hds_gl_status ) )
      {
         (void) memcpy( buf, values,
                        (size_t) ( app->length * data->size ) );
      }
      app->body = buf;
   }

/* Scatter the object data if discontiguous.                                 */
   if ( state->broken )
   {
      dau_scatter_data( 1, data, &nbad );
   }

/* Otherwise, locate the record's dynamic domain and translate the data      */
/* from the source buffer.                                                   */
   else
   {
      rec_locate_data( &data->han, objlen, objoff, 'W', &dom );
      obj->body = dom;
      dat1_cvt( 1, data->size, app, obj, &nbad );
      rec_release_data( &data->han, objlen, objoff, 'W', &dom );
   }

/* If a temporary buffer has been used to ensure correct data alignment,     */
/* then deallocate it.                                                       */
   if ( fixalign )
   {
      rec_deall_mem( app->length * data->size, (void **) &buf );
   }

/* Check the global status value before return.                              */
   _call( hds_gl_status )
   return hds_gl_status;
}

/*===============================*/
/* DAT_PUTI - Write Integer data */
/*===============================*/
int
datPutI(const HDSLoc   *locator,
         int     ndim,
        const HDS_PTYPE dims[],
        const int     values[],
         int     *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_PUTI_ERR"
#define context_message\
        "DAT_PUTI: Error writing integer values to an HDS primitive."

   datPut(locator,
          "_INTEGER",
          ndim,
          dims,
          values,
          status);
   return hds_gl_status;
}

/*=====================================*/
/* DAT_PUTW - Write Short Integer data */
/*=====================================*/
int
datPutW(const HDSLoc   *locator,
        int     ndim,
        const HDS_PTYPE dims[],
        const short     values[],
        int     *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_PUTW_ERR"
#define context_message\
        "DAT_PUTW: Error writing short integer values to an HDS primitive."

   datPut(locator,
          "_WORD",
          ndim,
          dims,
          values,
          status);
   return hds_gl_status;
}

/*============================*/
/* DAT_PUTR - Write Real data */
/*============================*/
int
datPutR( const HDSLoc    *locator,
         int       ndim,
         const HDS_PTYPE dims[],
         const float     values[],
         int       *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_PUTR_ERR"
#define context_message\
        "DAT_PUTR: Error writing real values to an HDS primitive."

  datPut(locator,
          "_REAL",
          ndim,
          dims,
          values,
          status);
     return hds_gl_status;
}

/*========================================*/
/* DAT_PUTD - Write Double precision data */
/*========================================*/
int
datPutD( const HDSLoc    *locator,
         int       ndim,
         const HDS_PTYPE dims[],
         const double    values[],
         int       *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_PUTD_ERR"
#define context_message\
     "DAT_PUTD: Error writing double precision value(s) to an HDS primitive."

   datPut(locator,
          "_DOUBLE",
          ndim,
          dims,
          values,
          status);
     return hds_gl_status;
}

/*===============================*/
/* DAT_PUTL - Write Logical data */
/*===============================*/
int
datPutL( const HDSLoc    *locator,
         int       ndim,
         const HDS_PTYPE dims[],
         const int       values[],
         int       *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_PUTL_ERR"
#define context_message\
        "DAT_PUTL: Error writing logical values to an HDS primitive."

     datPut(locator,
          "_LOGICAL",
          ndim,
          dims,
          values,
          status);
     return hds_gl_status;
}

/*=================================*/
/* DAT_PUTC - Write Character data */
/*=================================*/
int
datPutC( const HDSLoc    *locator,
         int       ndim,
         const HDS_PTYPE dims[],
         const char      string[],
         size_t    string_length,
         int       *status)
{
/* Local variables */
  char *string1;
  char stype[DAT__SZTYP+1];

#undef context_name
#undef context_message
#define context_name "DAT_PUTC_ERR"
#define context_message\
        "DAT_PUTC: Error writing character value(s) to an HDS primitive."

/* Encode the (fixed) string length into the primitive type definition  */
/* before calling datPut                                                */
/* Assume that a zero length string (eg. prefix=\"\") is a single space */
/* to make consistent with earlier HDS behaviour!                       */
   if( string_length > 0 ) {
      datCctyp( string_length, stype );
      string1 = (char*)string;
   } else {
      strcpy( stype, "_CHAR" );
      string1 = " ";
   }
   datPut(locator,
          stype,
          ndim,
          dims,
          string1,
          status);
     return hds_gl_status;
}

/*         O N E - D I M           P U T     */

/*=================================*/
/* DAT_PUT1D - Write 1D double array */
/*=================================*/

int
datPut1D( const HDSLoc * locator,
	  size_t nval,
	  const double values[],
	  int * status ) {
  size_t size;
  hdsdim dim[1];

  if ( *status != DAT__OK ) return *status;
  datSize( locator, &size, status );
  if ( *status == DAT__OK && size != nval ) {
    *status = DAT__BOUND;
    emsSeti( "IN", (int)nval );
    emsSeti( "SZ", (int)size );
    emsRep( "DAT_PUT1D_ERR", "Bounds mismatch: ^IN != ^SZ", status);
  } else {
    dim[0] = (hdsdim)size;
    datPutD( locator, 1, dim, values, status );
  }
  return *status;
}

/*=================================*/
/* DAT_PUT1I - Write 1D int array */
/*=================================*/

int
datPut1I( const HDSLoc * locator,
	  size_t nval,
	  const int values[],
	  int * status ) {
  size_t size;
  hdsdim dim[1];

  if ( *status != DAT__OK ) return *status;
  datSize( locator, &size, status );
  if ( *status == DAT__OK && size != nval ) {
    *status = DAT__BOUND;
    emsSeti( "IN", (int)nval );
    emsSeti( "SZ", (int)size );
    emsRep( "DAT_PUT1I_ERR", "Bounds mismatch: ^IN != ^SZ", status);
  } else {
    dim[0] = (hdsdim)size;
    datPutI( locator, 1, dim, values, status );
  }
  return *status;
}

/*======================================*/
/* DAT_PUT1W - Write 1D short int array */
/*======================================*/

int
datPut1W( const HDSLoc * locator,
	  size_t nval,
	  const short values[],
	  int * status ) {
  size_t size;
  hdsdim dim[1];

  if ( *status != DAT__OK ) return *status;
  datSize( locator, &size, status );
  if ( *status == DAT__OK && size != nval ) {
    *status = DAT__BOUND;
    emsSeti( "IN", (int)nval );
    emsSeti( "SZ", (int)size );
    emsRep( "DAT_PUT1W_ERR", "Bounds mismatch: ^IN != ^SZ", status);
  } else {
    dim[0] = (hdsdim)size;
    datPutW( locator, 1, dim, values, status );
  }
  return *status;
}

/*=================================*/
/* DAT_PUT1R - Write 1D float array */
/*=================================*/

int
datPut1R( const HDSLoc * locator,
	  size_t nval,
	  const float values[],
	  int * status ) {
  size_t size;
  hdsdim dim[1];

  if ( *status != DAT__OK ) return *status;
  datSize( locator, &size, status );
  if ( *status == DAT__OK && size != nval ) {
    *status = DAT__BOUND;
    emsSeti( "IN", (int)nval );
    emsSeti( "SZ", (int)size );
    emsRep( "DAT_PUT1R_ERR", "Bounds mismatch: ^IN != ^SZ", status);
  } else {
    dim[0] = (hdsdim)size;
    datPutR( locator, 1, dim, values, status );
  }
  return *status;
}

/*=================================*/
/* DAT_PUT1L - Write 1D logical array */
/*=================================*/

int
datPut1L( const HDSLoc * locator,
	  size_t nval,
	  const int values[],
	  int * status ) {
  size_t size;
  hdsdim dim[1];

  if ( *status != DAT__OK ) return *status;
  datSize( locator, &size, status );
  if ( *status == DAT__OK && size != nval ) {
    *status = DAT__BOUND;
    emsSeti( "IN", (int)nval );
    emsSeti( "SZ", (int)size );
    emsRep( "DAT_PUT1L_ERR", "Bounds mismatch: ^IN != ^SZ", status);
  } else {
    dim[0] = (hdsdim)size;
    datPutL( locator, 1, dim, values, status );
  }
  return *status;
}

/*         V E C T O R I Z E D     P U T     */

/*=================================*/
/* DAT_PUTVD - Write vectorized doubles */
/*=================================*/

int
datPutVD( const HDSLoc * locator,
	  size_t nval,
	  const double values[],
	  int *status ) {
  HDSLoc *vec = NULL;
  datVec( locator, &vec, status );
  datPut1D( vec, nval, values, status );
  datAnnul( &vec, status );
  return *status;
}

/*==================================*/
/* DAT_PUTVI - Write vectorized int */
/*==================================*/

int
datPutVI( const HDSLoc * locator,
	  size_t nval,
	  const int values[],
	  int *status ) {
  HDSLoc *vec = NULL;
  datVec( locator, &vec, status );
  datPut1I( vec, nval, values, status );
  datAnnul( &vec, status );
  return *status;
}

/*====================================*/
/* DAT_PUTVR - Write vectorized float */
/*====================================*/

int
datPutVR( const HDSLoc * locator,
	  size_t nval,
	  const float values[],
	  int *status ) {
  HDSLoc *vec = NULL;
  datVec( locator, &vec, status );
  datPut1R( vec, nval, values, status );
  datAnnul( &vec, status );
  return *status;
}

/*=================================*/
/* DAT_PUTVL - Write vectorized logical */
/*=================================*/

int
datPutVL( const HDSLoc * locator,
	  size_t nval,
	  const int values[],
	  int *status ) {
  HDSLoc *vec = NULL;
  datVec( locator, &vec, status );
  datPut1L( vec, nval, values, status );
  datAnnul( &vec, status );
  return *status;
}
