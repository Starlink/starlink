#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+DATALTER.C-*/

/* Include files */

#include <stdio.h>

#include "ems.h"                 /* EMS error reporting routines            */

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

#include "hds.h"

/*==================================*/
/* DAT_ALTER - Alter size of object */
/*==================================*/
int
datAlter(const HDSLoc    *locator,
         int       ndim,
         const HDS_PTYPE dims[],
         int       *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_ALTER_ERR"
#define context_message \
       "DAT_ALTER: Error altering the size of an HDS object."

   struct LCP       *lcp;
   struct LCP_DATA  *data;
   struct LCP_STATE *state;
   struct PDD       *obj;
   HDS_PTYPE          *axis;
   unsigned char    *srv;
   struct RID       rid;
   HDS_PTYPE        (*dbt)[2];
   struct RCL       rcl;
   struct ODL       odl;
   INT_BIG          extent;
   INT_BIG          off;
   INT_BIG          size;
   int              ptr;
   int              i;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import locator.   */

   _call(dat1_import_loc(locator, &lcp ));
   data  = &lcp->data;
   state = &data->state;

/* Ensure that the locator points to an 'untouched' n-dimensional object and
   that the container file was not opened for read-only access.         */

   if (state->mapped || state->vmcopy || state->unlike || state->slice ||
       state->cell   || state->vector || state->broken || data->naxes == 0)
      _callnam(DAT__OBJIN)
   if (data->read)
      _callnam(DAT__ACCON)

/* Check that the # of dimensions match.        */

   if (data->naxes != ndim)
      _callnam(DAT__DIMIN)

/* Read the Object Descriptor Label and associate the axis vector.      */

   _callnam( dat1_get_odl( &data->han, &odl ))
   axis = odl.axis;

/* Calculate the extent by which the object's size is to be altered.
   (Return if any of the dimension sizes except the last don't match)   */

   extent = 1;
   for (i=0; i<=(ndim-2); i++)
   {
      if (dims[i] != axis[i])
         _callnam(DAT__DIMIN)
      extent *= dims[i];
   }
   extent *= dims[ndim-1] - axis[ndim-1];
   size = data->size + extent;

/* Validate the new shape of the object and adjust the extent to a byte
   count.       */

   _callnam( dau_check_shape( ndim, dims, &odl ))
   obj = &data->obj;
   extent *= obj->length;

/* Expand the record's dynamic domain if the extent is positive.        */

   if (extent > 0)
      _callnam( rec_extend_record( &data->han, extent ))

/* Otherwise, shrink the domain if negative, after ensuring that no
   component records exist for an n-dimensional structure object.       */

   else if (extent < 0)
   {
      extent = -extent;
      if (data->struc)
      {
         _callnam( rec_get_rcl( &data->han, &rcl ))
         off = rcl.dlen - extent;
         _callnam(rec_locate_data( &data->han, extent, off, 'R', &srv ))
         ptr = 0;
         for ( i = 0; ( i < extent ) && _ok( hds_gl_status );
               i += SZSRV )
            {
               dat1_unpack_srv( srv + i, &rid );
               if ( ( rid.bloc != 0 ) || ( rid.chip != 0 ) )
               {
                  ptr = 1;
                  break;
               }
            }
            rec_release_data( &data->han, extent, off, 'R', &srv );
            if (ptr != 0)
               _callnam(DAT__DELIN)
      }
      _callnam(rec_shrink_record( &data->han, extent ))
   }

/* Write the updated Object Descriptor Label and readjust the size.     */

   _callnam(dat1_put_odl( &data->han, &odl ))
   data->size = size;

/* Adjust the last axis in the Dimension Bounds Table.  */

   if (ndim <= DAT__MXSLICE)
   {
      dbt = data->bounds;
      dbt[ndim-1][UPPER] = axis[ndim-1];
   }
   return hds_gl_status;
}


/*===============================*/
/* DAT_RESET - Reset object state*/
/*===============================*/
int
datReset( const HDSLoc *locator,
          int *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_RESET_ERR"
#define context_message\
        "DAT_RESET: Error resetting an HDS object to an undefined state."

   struct LCP      *lcp;
   struct LCP_DATA *data;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import locator string and locator.   */

   _call(dat1_import_loc(locator, &lcp ));
   data = &lcp->data;

/* If the container file is not open for read-only access, then clear
   the domain(D) active flag.   */

   if (data->read)
      _callnam(DAT__ACCON)
   _callnam(rec_reset_record( &data->han ))
   return hds_gl_status;
}


/*===================================*/
/* DAT_MOULD - Alter shape of object */
/*===================================*/
int
datMould( const HDSLoc    *locator,
          int       ndim,
          const HDS_PTYPE dims[],
          int       *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_MOULD_ERR"
#define context_message\
        "DAT_MOULD: Error altering the shape of an HDS object."

   struct LCP       *lcp;
   struct LCP_DATA  *data;
   struct LCP_STATE *state;
   struct ODL       odl;
   HDS_PTYPE        (*dbt)[2];
   UINT_BIG          size;
   int              i;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import locator string and locator.   */

   _call(dat1_import_loc(locator, &lcp ));
   data  = &lcp->data;
   state = &data->state;

/* Ensure that the locator points to an 'untouched' n-dimensional object and
   that the container file was not opened for read-only access.         */

   if (state->mapped || state->vmcopy || state->unlike || state->slice ||
       state->cell   || state->vector || state->broken || data->naxes == 0)
      _callnam(DAT__OBJIN)
   if (data->read)
      _callnam(DAT__ACCON)

/* Check that the new dimensionality is less than or equal to the old. Then
   calculate the new size of the object and check that it is the same as
   the existing size.   */

   if ((ndim) > data->naxes)
      _callnam(DAT__DIMIN)
   size = 1;
   for (i=0; i<(ndim); i++)
      size    *= dims[i];
   if (size != data->size)
      _callnam(DAT__DIMIN)

/* Read the Object Descriptor Label (needed for the object type) and validate
   the new shape of the object. */

   _callnam(dat1_get_odl( &data->han, &odl ))
   _callnam(dau_check_shape( ndim, dims, &odl ))

/* Write the updated Object Descriptor Label.   */

   _callnam(dat1_put_odl( &data->han, &odl ))

/* Adjust the number of dimensions and the Dimension Bounds Table.      */

   data->naxes = ndim;
   dbt = data->bounds;
   for (i=0; i<(ndim); i++)
   {
      if (i<DAT__MXSLICE)
      {
         dbt[i][LOWER] = 1;
         dbt[i][UPPER] = dims[i];
      }
   }
   return hds_gl_status;
}

/* Check status and return on error after setting error message that
   includes the component name. Private version of _call() */
#define _callnc(event)\
{\
*status = (event);\
if (!_ok(*status))\
        {\
        char private_context_message[132];\
        char dname[DAT__SZNAM+1];\
        int privstat = DAT__OK;\
        emsMark();\
        datName(locator, dname, &privstat );\
        if (privstat != DAT__OK) dname[0] = '\0';\
        emsAnnul(&privstat);\
        emsRlse();\
        sprintf( private_context_message,\
                 context_message ": '%s' -> '%s'", dname, name_str); \
        hds_gl_status = *status;\
        emsRep(context_name,private_context_message,status);\
              return hds_gl_status;\
        }\
}

/*===========================*/
/* DAT_RENAM - Rename object */
/*===========================*/
int
datRenam(const HDSLoc *locator,
         const char *name_str,
         int  *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_RENAM_ERR"
#define context_message\
        "DAT_RENAM: Error changing the name of an HDS object."

   struct DSC      name;
   struct LCP      *lcp;
   struct LCP_DATA *data;
   struct RCL      rcl;
   unsigned char   *crv;
   char            nambuf[DAT__SZNAM];
   struct HAN      han;
   int             ncomp;
   struct RID      rid;
   int             i;
   char            *name1;
   struct RID      rid1;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status     = DAT__OK;

/* Import locator and name strings.     */

   _strcsimp  ( &name, name_str );
   _call(dat1_import_loc(locator, &lcp ));

   data = &lcp->data;

/* Return if the container file was opened for read only access.    */

   if (data->read)
      _callnc(DAT__ACCON)

/* Validate the new object name.        */

   _callnc(dau_check_name( &name, nambuf ))

/* Identify the object record's ID so that the correct slot in the Component
   Record can be identified. Then stick a handle on its parent record. This is
   the Component Record.        */

   rid = rec_gl_ridzero;
   rec_get_rid( &data->han, &rid );
   rec_get_handle( &data->parent, &data->han, &han );

/* Map to the Component Record Vector and check that no existing component
   has the same name.   */

   _callnc(rec_get_rcl( &han, &rcl ))
   if ( rcl.class == DAT__CONTAINER )
   {
     ncomp = 1;
   }
   else
   {
      _callnc(dat1_get_ncomp( &han, &ncomp ))
   }
   _callnc(rec_locate_data( &han, rcl.dlen, 0, 'U', &crv ))
   for ( i = 0; i < ncomp; i++ )
   {
      dat1_locate_name( crv, i, &name1 );
      if ( _cheql( DAT__SZNAM, nambuf, name1 ) )
      {
         rec_release_data( &han, rcl.dlen, 0, 'U', &crv );
         _callnc( DAT__COMEX )
      }
   }

/* Search for the entry that has the correct Record ID. */

   for ( i = 0; ; i++ )
   {
      dat1_unpack_crv( crv, i, &rid1 );
      if ( ( rid1.bloc == rid.bloc ) && ( rid1.chip == rid.chip ) )
      {
         break;
      }
   }
/* Copy nambuf to the Locator Control Packet and to the Component Record
   Vector and then ditch the Component Record Vector. */

   _chmove(DAT__SZNAM, nambuf, data->name);
   dat1_locate_name( crv, i, &name1 );
   _chmove(DAT__SZNAM, nambuf, name1 );
   rec_release_data( &han, rcl.dlen, 0, 'U', &crv );
   return hds_gl_status;
}


/*===========================*/
/* DAT_RETYP - Retype object */
/*===========================*/
int
datRetyp(const HDSLoc *locator,
         const char *type_str,
	 int *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_RETYP_ERR"
#define context_message\
        "DAT_RETYP: Error changing the type of an HDS object."

   struct DSC      type;
   struct LCP      *lcp;
   struct LCP_DATA *data;
   struct ODL      odl;
   char            typbuf[DAT__SZTYP];

   struct PDD      pdd;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status     = DAT__OK;

/* Import type strings.     */

   _strcsimp  ( &type, type_str );

/* Import locator.      */

   _call(dat1_import_loc(locator, &lcp ));
   data = &lcp->data;

/* Return if the container file was opened for read only access.            */
   if (data->read)
      _callnam(DAT__ACCON)

/* Validate the new object type specification and pack it.                  */
   _callnam(dat1_check_type( &type, typbuf ))
   _callnam( dat1_unpack_type( typbuf, &pdd ) )

/* Check that the change in type does not imply a change from a structure   */
/* type to a primitive type (or vice versa). Report an error if it does.    */
   if ( ( pdd.class == DAT__STRUCTURE ) !=
        ( data->obj.class == DAT__STRUCTURE ) )
   {
      hds_gl_status = DAT__TYPIN;
      emsSetnc( "TYPE", (char *) type.body, type.length );
      emsRep( "DAT_RETYP",
                 "Invalid new data type \'^TYPE\' specified; conversion \
between structured and primitive types is not allowed (possible programming \
error).",
                 &hds_gl_status );
   }

/* Also check that the change in type does not imply a change in data       */
/* object length (e.g. changing from _BYTE to _WORD would imply a doubling  */
/* of the storage space). Report an error if it does.                       */
   else if ( pdd.length != data->obj.length )
   {
      hds_gl_status = DAT__TYPIN;
      emsSetnc( "TYPE", (char *) type.body, type.length );
      emsRep( "DAT_RETYP",
                 "Invalid new data type \'^TYPE\' specified; implies a \
change in object size (possible programming error).",
                 &hds_gl_status );
   }

/* Read the Object Descriptor Label. */
   else
   {
      _callnam(dat1_get_odl( &data->han, &odl ))

/* Copy the new packed type specification to the Object Descriptor Label    */
/* and then rewrite the Object Descriptor Label.                            */
      _chmove( DAT__SZTYP, typbuf, odl.type );
      _callnam( dat1_put_odl( &data->han, &odl ) )

/* Store the packed and unpacked forms of the new type in the LCP.          */
      _chmove( DAT__SZTYP, typbuf, data->type );
      data->obj = pdd;
   }

/* If an error occurred, then report contextual information.                */
   if ( !_ok( hds_gl_status ) )
   {
      emsRep( "DAT_RETYP_ERR",
                 "DAT_RETYP: Error changing the type of an HDS object.",
                 &hds_gl_status );
   }

/* Return the current global status value.                                  */
   *status = hds_gl_status;
   return *status;
}
