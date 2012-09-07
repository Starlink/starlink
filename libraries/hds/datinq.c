#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+DATINQ.C-*/

#include <stdio.h>
#include <strings.h>

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

#include "hds.h"

#define TRUE  1
#define FALSE 0

/*==========================*/
/* DAT_NAME - Object name ? */
/*==========================*/
int
datName(const HDSLoc *locator,
        char name_str[DAT__SZNAM+1],
        int *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_NAME_ERR"
#define context_message\
        "DAT_NAME: Error enquiring the name of an HDS object."

   struct LCP      *lcp;
   struct LCP_DATA *data;

   char *p;

/* Enter routine.       */

   name_str[0] = '\0'; /* initialise return string */
   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import locator.      */

   _call(dat1_import_loc(locator, &lcp ))
   data = &lcp->data;

/* Copy the object name from the LCP.   */

   _chmove( DAT__SZNAM, data->name, name_str );

/* Convert to C string */
   name_str[DAT__SZNAM] = '\0';
   if( (p = strchr( name_str, ' ' ) ) != NULL )
       *p = '\0';

   return hds_gl_status;
}


/*==========================*/
/* DAT_TYPE - Object type ? */
/*==========================*/
int
datType(const HDSLoc *locator,
        char type_str[DAT__SZTYP + 1],
        int *status )
{
#undef context_name
#undef context_message
#define context_name "DAT_TYPE_ERR"
#define context_message\
        "DAT_TYPE: Error enquiring the type of an HDS object."

   struct DSC type;

   struct LCP      *lcp;
   struct LCP_DATA *data;

   char buf[ DAT__SZTYP + 1 ];
   int nc;
   char *p;

/* Enter routine.       */

   type_str[0] = '\0'; /* Initialise return string */
   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import type strings.     */

   type.length = DAT__SZTYP;
   type.body   = (unsigned char*)type_str;

/* Import locator.      */

   _call(dat1_import_loc(locator, &lcp ))
   data = &lcp->data;

   if ( data->obj.class == DAT__PRIMITIVE )
   {
      switch ( data->obj.dtype )
      {
         case DAT__D:
         {
            _chcopy( 7, "_DOUBLE", ' ', type.length, type.body );
            break;
         }

         case DAT__R:
         {
            _chcopy( 5, "_REAL", ' ', type.length, type.body );
            break;
         }

         case DAT__I:
         {
           _chcopy( 8, "_INTEGER", ' ', type.length, type.body );
           break;
         }

         case DAT__K:
         {
           _chcopy( 6, "_INT64", ' ', type.length, type.body );
           break;
         }

         case DAT__W:
         {
            _chcopy( 5, "_WORD", ' ', type.length, type.body );
            break;
         }

         case DAT__UW:
         {
            _chcopy( 6, "_UWORD", ' ', type.length, type.body );
            break;
         }

         case DAT__B:
         {
            _chcopy( 5, "_BYTE", ' ', type.length, type.body );
            break;
         }

         case DAT__UB:
         {
            _chcopy( 6, "_UBYTE", ' ', type.length, type.body );
            break;
         }

         case DAT__L:
         {
            _chcopy( 8, "_LOGICAL", ' ', type.length, type.body );
            break;
         }

         case DAT__C:
         {
            nc = snprintf( buf, sizeof(buf), "_CHAR*%d",
                            (int) ( data->obj.length /
                            dat_gl_ndr[ DAT__C ].length ) );
            _chcopy( nc, buf, ' ', (int) type.length, type.body );
            break;
         }
      }
   }
   else
   {
      _chcopy( DAT__SZTYP, data->type, ' ', type.length, type.body );
   }

/* Convert returned type to C string */
   type_str[DAT__SZTYP] = '\0';
   if( ( p = strchr( type_str, ' ' ) ) != NULL )
       *p = '\0';

   return hds_gl_status;
}

/*============================*/
/* DAT_SHAPE - Object shape ? */
/*============================*/
int
datShape(const HDSLoc *locator,
         int       maxdim,
         HDS_PTYPE dims[],
         int       *actdim,
         int       *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_SHAPE_ERR"
#define context_message\
        "DAT_SHAPE: Error enquiring the shape of an HDS object."

   struct LCP      *lcp;
   struct LCP_DATA *data;
   HDS_PTYPE       axis[DAT__MXDIM];
   int             naxes;
   int             i;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import locator string.   */

   _call(dat1_import_loc(locator, &lcp ))
   data = &lcp->data;

/* Enquire the object shape.    */

   _call(dau_get_shape( data, &naxes, axis ))
   *actdim = naxes;

/* Return the required # of dimension sizes.    */

   for (i=0; i<_min(maxdim, *actdim); i++)
      dims[i] = axis[i];

   return hds_gl_status;
}


/*==========================*/
/* DAT_SIZE - Object size ? */
/*==========================*/
int
datSize(const HDSLoc *locator,
        size_t *size,
        int *status )
{
#undef context_name
#undef context_message
#define context_name "DAT_SIZE_ERR"
#define context_message\
        "DAT_SIZE: Error enquiring the size of an HDS object."

   struct LCP      *lcp;
   struct LCP_DATA *data;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import locator string and locator.   */

   _call(dat1_import_loc(locator, &lcp ))
   data = &lcp->data;

/* Return the object size.      */

   *size         = (size_t)data->size;

   return hds_gl_status;
}

/*============================*/
/* DAT_THERE - Object there ? */
/*============================*/
int
datThere(const HDSLoc *locator,
         const char *name_c,
         int *there,
         int *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_THERE_ERR"
#define context_message\
        "DAT_THERE: Error enquiring about the existence of an HDS object."

   struct DSC name;

   struct LCP      *lcp;
   struct LCP_DATA *data;
   char            nambuf[DAT__SZNAM];
   unsigned char   *srv;
   unsigned char   *crv;
   struct RCL      rcl;
   struct RID      rid;
   struct HAN      han;
   INT_BIG         off;
   int             ncomp;
   int             i;
   char            *name1;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import name strings.     */

   _strcsimp( &name, name_c );

/* Import locator.      */

   _call(dat1_import_loc(locator, &lcp ))
   data = &lcp->data;

/* Return if the locator points to anything other than a single structure
   object.      */

   if ( (!data->struc) || (data->naxes != 0) )
      _call(DAT__OBJIN)

/* Validate the component name. */

   _call( dau_check_name( &name, nambuf ))

/* Locate the Structure Record Vector entry and extract the ID of the component
   record. If the ID is null, then no component record exists.  */

   off = data->offset * SZSRV;
   _call( rec_locate_data( &data->han, SZSRV, off, 'R', &srv ))
   dat1_unpack_srv( srv, &rid );
   rec_release_data( &data->han, SZSRV, off, 'R', &srv );
   if ( ( rid.bloc == 0 ) && ( rid.chip == 0 ) )
   {
      *there = FALSE;
      return hds_gl_status;
   }

/* Otherwise, stick a handle on the component record, get the Record Control
   Label and read the component count.  */

   _call( rec_get_handle(&rid, &data->han, &han) )
   _call( rec_get_rcl( &han, &rcl ))
   _call( dat1_get_ncomp( &han, &ncomp ))

/* Locate the Component Record Vector and search for the specified name. */

   _call(rec_locate_data( &han, rcl.dlen, 0, 'R', &crv ))
   rid = rec_gl_ridzero;
   for ( i = 0; i < ncomp; i++ )
   {
      dat1_locate_name( crv, i, &name1 );
      if ( _cheql( DAT__SZNAM, nambuf, name1 ) )
      {
         dat1_unpack_crv( crv, i, &rid );
         break;
      }
   }
   rec_release_data( &han, rcl.dlen, 0, 'R', &crv );

/* If the Record-ID is not null, then the component exists.     */

   *there = ( ( rid.bloc != 0) || ( rid.chip != 0 ) ) ? TRUE : FALSE;

   return hds_gl_status;
}

/*================================*/
/* DAT_STRUC - Structure object ? */
/*================================*/
int
datStruc(const HDSLoc *locator,
         int *struc,
         int *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_STRUC_ERR"
#define context_message\
        "DAT_STRUC: Error enquiring if an HDS object is a structure."

   struct LCP      *lcp;
   struct LCP_DATA *data;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import locator.   */

   _call(dat1_import_loc(locator, &lcp ))
   data = &lcp->data;

/* Set the flag appropriately and return.       */

   *struc = data->struc ? TRUE : FALSE;

   return hds_gl_status;
}

/*===============================*/
/* DAT_PRIM - Primitive object ? */
/*===============================*/
int
datPrim(const HDSLoc *locator,
        int *prim,
        int *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_PRIM_ERR"
#define context_message\
        "DAT_PRIM: Error enquiring if an HDS object is primitive."

  struct LCP      *lcp;
  struct LCP_DATA *data;

/* Enter routine.       */

  *prim = FALSE; /* force intialisation of return value */

/* Previous version forced internal status to be good even if entry status
   was bad. This would always result in DAT_PRIM attaching itself to error
   messages even when it was not really triggering the error state. Change
   to match other routines */
   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import locator */

   _call(dat1_import_loc(locator, &lcp ))
   data = &lcp->data;

/* Set the flag appropriately and return.       */

   *prim = data->struc ? FALSE : TRUE;

   return hds_gl_status;
}

/*====================================*/
/* DAT_NCOMP - Number of components ? */
/*====================================*/
int
datNcomp( const HDSLoc *locator,
          int *ncomp,
          int *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_NCOMP_ERR"
#define context_message\
        "DAT_NCOMP: Error enquiring the number of components in an HDS structure."

   struct LCP      *lcp;
   struct LCP_DATA *data;
   unsigned char   *srv;
   struct RID      rid;
   struct HAN      han;
   INT_BIG         off;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import locator.   */

   _call(dat1_import_loc(locator, &lcp ))
   data = &lcp->data;

/* Return if the source locator points to anything other than a single
   structure object.    */

   if ( (!data->struc) || (data->naxes != 0) )
      _call(DAT__OBJIN)

/* Locate the Structure Record Vector entry and extract the ID of the component
   record. If the ID is null, then no component record exists.  */

   off = data->offset * SZSRV;
   _call( rec_locate_data( &data->han, SZSRV, off, 'R', &srv ))
   dat1_unpack_srv( srv, &rid );
   rec_release_data(&data->han, SZSRV, off, 'R', &srv);
   if ( ( rid.bloc == 0 ) && ( rid.chip == 0 ) )
   {
      *ncomp = 0;
      return hds_gl_status;
   }

/* Otherwise, stick a handle on the component record and read the contents of
   the record's static domain.  */

   _call( rec_get_handle( &rid, &data->han, &han ))
   _call( dat1_get_ncomp( &han, ncomp ))

   return hds_gl_status;
}

/*==============================*/
/* DAT_LEN - Primitive length ? */
/*==============================*/
int
datLen(const HDSLoc *locator,
       size_t *len,
       int *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_LEN_ERR"
#define context_message\
        "DAT_LEN: Error enquiring the element length of an HDS primitive."

   struct LCP      *lcp;
   struct LCP_DATA *data;
   struct PDD       *obj;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import locator.   */

   _call(dat1_import_loc(locator, &lcp ))
   data = &lcp->data;

/* Return if the object is a structure. */

   if (data->struc)
      _call(DAT__OBJIN)

/* Otherwise, associate the object data attributes descriptor.  */

   obj = &data->obj;

/* Return the primitive object length.  */

   *len = (size_t)obj->length;

   return hds_gl_status;
}

/*============================*/
/* DAT_STATE - Object state ? */
/*============================*/
int
datState(const HDSLoc *locator,
         int *state,
         int *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_STATE_ERR"
#define context_message\
        "DAT_STATE: Error enquiring the state of an HDS primitive."

   struct LCP      *lcp;
   struct LCP_DATA *data;
   struct RCL      rcl;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import locator.   */

   _call(dat1_import_loc(locator, &lcp ))
   data = &lcp->data;

/* Return if the object is a structure. */

   if (data->struc)
      _call(DAT__OBJIN)

/* Read the record's control label and determine if the active flag
   is currently set.    */

   _call( rec_get_rcl( &data->han, &rcl ))
   *state = rcl.active ? TRUE : FALSE;

   return hds_gl_status;
}

/*=============================*/
/* DAT_VALID - Valid locator ? */
/*=============================*/
int
datValid(const HDSLoc *locator,
         int *valid,
         int *status)
{

   struct LCP *lcp;

/*
   Enter routine.
*/
   *valid = FALSE;
   if ( !_ok( *status ) )
      return *status;
    hds_gl_status = DAT__OK;

/*
   Defer error reporting. Import the locator and set the flag
   appropriately.
*/
   emsMark( );

   dat1_import_loc(locator, &lcp );
   *valid = ( lcp != NULL ? TRUE : FALSE );

/*
   Annul any errors and end the error context.
*/
   emsAnnul( &hds_gl_status );
   emsRlse( );

/*
   Return the status value.
*/
   *status = hds_gl_status;
   return *status;
}

/*=======================================*/
/* DAT_CONV - Data conversion possible ? */
/*=======================================*/
int
datConv(const HDSLoc *locator,
        const char *type_str,
        int *conv,
        int *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_CONV_ERR"
#define context_message\
        "DAT_CONV: Error determining whether type conversion is possible."

   struct DSC type;

   struct LCP      *lcp;
   struct LCP_DATA *data;
   char            typbuf[DAT__SZTYP];
   struct PDD      app;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import type strings.     */

   _strcsimp( &type, type_str );

/* Import locator.      */

   _call(dat1_import_loc(locator, &lcp ))
   data = &lcp->data;

/* Ensure that the locator points to a primitive and not a structure.   */

   if (data->struc)
      _call(DAT__OBJIN)

/* Validate the type specification and determine the attributes of the
   application data.    */

   _call( dat1_check_type( &type, typbuf ))
   _call( dat1_unpack_type( typbuf, &app ))

/* Ensure that the application data is primitive and set the flag
   appropriately if the types 'match'.  */

   if (app.class != DAT__PRIMITIVE)
      _call(DAT__TYPIN)
   *conv = _ok( dau_match_types( &data->obj, &app ) ) ? TRUE : FALSE;

   return hds_gl_status;
}
