#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+DATFININD.C-*/

/*#include "cnf.h"            * F77 <-> C string handling functions      */
#include "ems.h"            /* EMS error reporting routines             */
#include "hds1.h"           /* Global definitions for HDS               */
#include "rec.h"            /* Public rec_ definitions                  */
#include "str.h"            /* Character string import/export macros    */
#include "dat1.h"           /* Internal dat_ definitions                */
#include "dat_err.h"        /* DAT__ error code definitions             */

#include "hds.h"

/* F77_INTEGER_FUNCTION(dat_find)(struct STR *locator1_str,
 *                              struct STR *name_str,
 *                              struct STR *locator2_str,
 *                              F77_INTEGER_TYPE *status
 *                              TRAIL(locator1_str)
 *                              TRAIL(name_str)
 *                              TRAIL(locator2_str) )
 */

/*=================================*/
/* DAT_FIND - Find named component */
/*=================================*/
int
datFind( const HDSLoc *locator1,
         const char *name_str,
         HDSLoc **locator2,
         int  *status )
{
   struct DSC name;

   struct LCP      *lcp1;
   struct LCP_DATA *data1=NULL;
   struct LCP      *lcp2;
   struct LCP_DATA *data2;
   unsigned char   *srv;
   unsigned char   *crv;
   char            nambuf[DAT__SZNAM];
   HDS_PTYPE       *axis;
   HDS_PTYPE       (*dbt)[2];
   struct PDD      *obj;
   struct RCL      rcl;
   struct ODL      odl;
   struct HAN      han;
   struct RID      rid;
   INT_BIG         off;
   int             ncomp;
   int             i;
   char            *name1;
   int             loc2ok = 0;

/* Import the name string */
   _strcsimp(   &name, name_str );

/* Check the inherited global status.              */
   hds_gl_status = *status;
   if ( _ok( hds_gl_status ) )
   {

/* Import the input locator.                       */
     dat1_import_loc(locator1, &lcp1 );
      if ( _ok( hds_gl_status ) )
      {
         data1 = &lcp1->data;

/* Report an error if the input locator points to anything other than a      */
/* single structure object.                */
         if ( ( !data1->struc ) || ( data1->naxes != 0 ) )
         {
            hds_gl_status = DAT__OBJIN;
           emsRep( "DAT_FIND_1",
                      "Input object is not a scalar structure.",
                      &hds_gl_status );
         }
      }

/* Validate the component name.                */
      dau_check_name( &name, nambuf );

/* Locate the Structure Record Vector entry and extract the ID of the      */
/* component record.                  */
      if ( _ok( hds_gl_status ) )
      {
         off = data1->offset * SZSRV;
         rec_locate_data( &data1->han, SZSRV, off, 'R', &srv );
         dat1_unpack_srv( srv, &rid );
         if ( _ok( hds_gl_status ) )
         {

/* If the Record-ID is null, then no component record exists, so report an  */
/* error.                    */
            if ( ( rid.bloc == 0 ) && ( rid.chip == 0 ) )
            {
               hds_gl_status = DAT__OBJNF;
               emsSetnc( "NAME", nambuf, DAT__SZNAM );
               emsRep( "DAT_FIND_2",
                          "Object \'^NAME\' not found.",
                          &hds_gl_status );
            }
         }

/* Release the structure record vector.              */
         rec_release_data( &data1->han, SZSRV, off, 'R', &srv );
      }

/* Stick a handle on the component record, get the Record Control Label and */
/* determine the # of components in the list.            */
      if ( _ok( hds_gl_status ) )
      {
         rec_get_handle( &rid, &data1->han, &han );
         rec_get_rcl( &han, &rcl );
         dat1_get_ncomp( &han, &ncomp );

/* Locate the Component Record Vector and search for the specified name. If */
/* found, then extract the ID of the object record.          */
         rec_locate_data( &han, rcl.dlen, 0, 'R', &crv );
         rid = rec_gl_ridzero;
         for ( i = 0; ( i < ncomp ) && _ok( hds_gl_status ); i++ )
         {
            dat1_locate_name( crv, i, &name1 );
            if ( _ok( hds_gl_status ) )
            {
               if ( _cheql( DAT__SZNAM, nambuf, name1 ) )
               {
                  dat1_unpack_crv( crv, i, &rid );
                  break;
               }
            }
         }
         rec_release_data( &han, rcl.dlen, 0, 'R', &crv );

/* Report an error if the object cannot be found.          */
         if ( _ok( hds_gl_status ) )
         {
            if ( ( rid.bloc == 0 ) && ( rid.chip == 0 ) )
            {
               hds_gl_status = DAT__OBJNF;
               emsSetnc( "NAME", nambuf, DAT__SZNAM );
               emsRep( "DAT_FIND_3",
                          "Object \'^NAME\' not found.",
                          &hds_gl_status );
            }
         }
      }

/* Export the destination locator and stick a handle on the object record.  */
      dat1_alloc_lcp(locator2, &lcp2 );
      if ( _ok( hds_gl_status ) )
      {
         loc2ok = 1;
         data2 = &lcp2->data;
         rec_get_handle( &rid, &han, &data2->han );
         rec_get_rid( &han, &data2->parent );

/* Save the component name, propagate the group specification and increment */
/* the level counter.                  */
         _chmove( DAT__SZNAM, nambuf, data2->name );
         _chmove( DAT__SZGRP, data1->group, data2->group );
         data2->level = data1->level + 1;

/* Read the Object Descriptor Label from the object record's static domain  */
/* and determine the object attributes.              */
         dat1_get_odl( &data2->han, &odl );
         _chmove( DAT__SZTYP, odl.type, data2->type );
         dat1_unpack_type( data2->type, &data2->obj );
         obj = &data2->obj;

/* Save the shape information in the LCP and calculate the total size of    */
/* the object. (The Dimension Bounds Table is used to hold the 1st three    */
/* axis sizes).                    */
         if ( _ok( hds_gl_status ) )
         {
            axis = odl.axis;
            data2->naxes = odl.naxes;
            dbt = data2->bounds;
            data2->size = 1;
            for ( i = 0; i < data2->naxes; i++ )
            {
               data2->size *= axis[ i ];
               if ( i < DAT__MXSLICE )
               {
                  dbt[ i ][ LOWER ] = 1;
                  dbt[ i ][ UPPER ] = axis[ i ];
               }
            }

/* Flag whether structure or primitive and note the access mode.      */
            data2->struc = ( obj->class == DAT__STRUCTURE );
            data2->read = data1->read;
         }

/* If successful, then mark the new LCP as valid.          */
         if ( _ok( hds_gl_status ) )
         {
            data2->valid = 1;
         }

/* Otherwise, defuse the new LCP.              */
         else
         {
            dau_defuse_lcp( &lcp2 );
         }
      }

/* If an error occurred, then report contextual information.        */
      if ( !_ok( hds_gl_status ) )
      {
         emsRep( "DAT_FIND_ERR",
                    "DAT_FIND: Error finding a named component in an \
HDS structure.",
                    &hds_gl_status );
      }
   }

/* If the routine will exit with status set, then nullify the output      */
/* locator. (unless it has not been allocated by this routine)            */
   if ( !_ok( hds_gl_status ) && loc2ok )
   {
     dat1_free_hdsloc( locator2 );
   }

/* Return the current global status value.            */
   *status = hds_gl_status;
   return *status;
}
 
/* F77_INTEGER_FUNCTION(dat_index)(struct STR *locator1_str,
 *                                 F77_INTEGER_TYPE *index,
 *                                 struct STR *locator2_str,
 *                                 F77_INTEGER_TYPE *status
 *                                 TRAIL(locator1_str)
 *                                 TRAIL(locator2_str) )
 */

/*=======================================*/
/* DAT_INDEX - Index into component list */
/*=======================================*/
int
datIndex(const HDSLoc *locator1,
         int index,
         HDSLoc **locator2,
         int *status )
{
#undef context_name
#undef context_message
#define context_name "DAT_INDEX_ERR"
#define context_message\
        "DAT_INDEX: Error indexing into the component list of an HDS structure."

   struct LCP      *lcp1;
   struct LCP_DATA *data1;
   struct LCP      *lcp2;
   struct LCP_DATA *data2;
   unsigned char   *srv;
   unsigned char   *crv;
   char            nambuf[DAT__SZNAM];
   HDS_PTYPE       *axis;
   HDS_PTYPE       (*dbt)[2];
   struct PDD      *obj;
   struct RCL      rcl;
   struct ODL      odl;
   struct HAN      han;
   struct RID      rid;
   INT_BIG         off;
   int             ncomp;
   int             i;
   int             szcrv;
   char            *name1;

/* Enter routine.  */

   if (!_ok(*status))
      return *status;
   hds_gl_status  = DAT__OK;


/* Import the source locator.  */

   _call(dat1_import_loc(locator1, &lcp1 ));
   data1 = &lcp1->data;

/* Return if the locator points to anything other than a single structure
   object.  */

   if (!data1->struc || data1->naxes != 0)
      _call(DAT__OBJIN)

/* Locate the Structure Record Vector entry and extract the ID of the component
   record.  */

   off = data1->offset * SZSRV;
   _call( rec_locate_data( &data1->han, SZSRV, off, 'R', &srv ))
   dat1_unpack_srv( srv, &rid );
   rec_release_data( &data1->han, SZSRV, off, 'R', &srv );

/* If the Record-ID is null, then no component record exists.  */

   if ( ( rid.bloc == 0 ) && ( rid.chip == 0 ) )
      _call(DAT__OBJNF)

/* Otherwise, stick a handle on the component record, get the Record Control
   Label and determine the # of components in the list.  */

   _call( rec_get_handle( &rid, &data1->han, &han ))
   _call( rec_get_rcl( &han, &rcl ))
   _call( dat1_get_ncomp( &han, &ncomp ))

/* Return if the index is positioned past the end of the component list or   */
/* is invalid.                                                               */
   if ( ( index > ncomp ) || ( index < 1 ) )
      _call(DAT__OBJNF)

/* Locate the required element of the Component Record Vector, save the      */
/* object name and extract the ID of the object record.                      */
    szcrv = SZCRV;          
   _call( rec_locate_data( &han, szcrv, ( index - 1 ) * szcrv,
          'R', &crv ) )
   dat1_locate_name( crv, 0, &name1 );
   _chmove( DAT__SZNAM, name1, nambuf );
   dat1_unpack_crv( crv, 0, &rid );
   rec_release_data( &han, szcrv, ( index - 1 ) * szcrv, 'R',
                     &crv );

/* Export the destination locator and stick a handle on the object record. */

   _call(dat1_alloc_lcp(locator2, &lcp2 ))
   data2 = &lcp2->data;
   rec_get_handle( &rid, &han, &data2->han );
   rec_get_rid( &han, &data2->parent );

/* Save the component name, propagate the group specification and increment
   the level counter.  */

   _chmove( DAT__SZNAM, nambuf, data2->name );
   _chmove( DAT__SZGRP, data1->group, data2->group );
   data2->level = data1->level + 1;

/* Read the Object Descriptor Label from the object record's static domain  */
/* and determine the object attributes.              */

   _call( dat1_get_odl( &data2->han, &odl ))
   _chmove( DAT__SZTYP, odl.type, data2->type );
   _call( dat1_unpack_type( data2->type, &data2->obj ))
   obj = &data2->obj;

/* Save the shape information in the LCP and calculate the total size
   of the object. (The Dimension Bounds Table is used to hold the 1st
   three axis sizes).   */

   axis = odl.axis;
   data2->naxes = odl.naxes;
   dbt = data2->bounds;
   data2->size = 1;
   for ( i=0; i<data2->naxes; i++ )
   {
      data2->size *= axis[i];
      if (i<DAT__MXSLICE)
      {
         dbt[i][LOWER] = 1;
         dbt[i][UPPER] = axis[i];
      }
   }

/* Flag whether structure or primitive and mark the locator as valid.  */

   data2->struc = (obj->class == DAT__STRUCTURE);
   data2->read = data1->read;
   data2->valid = 1;

   return hds_gl_status;
}
