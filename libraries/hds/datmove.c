#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+DATMOVE.C-*/

/* Include files */

#include "cnf.h"                 /* F77 <-> C string handling functions     */
#include "ems.h"                 /* EMS error reporting routines            */

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

#include "hds.h"

/* Control Blocks */

/*========================*/
/* DAT_MOVE - Move object */
/*========================*/
int
datMove(HDSLoc **locator1,
        HDSLoc *locator2,
        char *name_str,
        int *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_MOVE_ERR"
#define context_message\
        "DAT_MOVE: Error moving an HDS object to a new location."

   struct DSC name;

   struct LCP       *lcp1;
   struct LCP_DATA  *data1;
   struct LCP_STATE *state1;
   struct LCP       *lcp2;
   struct LCP_DATA  *data2;
   unsigned char    *srv;
   unsigned char    *crv;
   unsigned char    crv1[ DAT__SZCRV ];
   struct RCL       rcl;
   struct RCL       rcl1;
   char             nambuf[DAT__SZNAM];
   struct HAN       han[3];
   INT_BIG          off;
   struct RID       rid;
   struct RID       rid1;
   int              ncomp;
   int              entryy;
   int              i;
   int              szcrv;
   char             *name1;

   rid = rec_gl_ridzero;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import the name string.        */

   _strcsimp( &name, name_str );

/* Import the first locator.    */

   _call(dat1_import_loc(*locator1, &lcp1 ));
   data1 = &lcp1->data;
   state1 = &data1->state;

/* Return if the locator points to an array slice or cell, or is associated
   with a top-level object, or if the container file was opened for read
   only access. */

   if (state1->slice || state1->cell || data1->level == 0)
      _call(DAT__OBJIN)
   if (data1->read)
      _call(DAT__ACCON)

/* Import the second locator and return if it points to anything other than
   a single structure object.   */

   _call(dat1_import_loc(locator2, &lcp2 ));
   data2 = &lcp2->data;
   if (!data2->struc || data2->naxes != 0)
      _call(DAT__OBJIN)

/* Validate the object's new name.      */

   _call( dau_check_name( &name, nambuf ))

/* Locate the recipient Structure Record Vector entry which contains the ID of
   the component record.        */

   off = data2->offset * SZSRV;
   _call( rec_locate_data( &data2->han, SZSRV, off, 'U', &srv ))
   dat1_unpack_srv( srv, &rid );

/* Component Record-ID null ?   */

   if ( ( rid.bloc == 0 ) && ( rid.chip == 0 ) )
   {

/*    If it is, then the structure has no components - create a new record. */

      rcl.class    = DAT__COMPONENT;
      rcl.zero     = 0;
      rcl.slen     = DAT__SZNCOMP;
      rcl.dlen     = SZCRV * hds_gl_ncomp;
      rec_create_record( &data2->han, &rcl, &han[0] );
      rec_get_rid( &han[0], &rid );

/* Pack the new record ID into the Structure Record Vector.                 */
      dat1_pack_srv( &rid, srv );
      hds_gl_ncomp = hds_gl_ncomp0;
      ncomp        = 0;
   }

/* Otherwise, components exist. */

   else
   {
/*    Stick a handle on the component record, get the Record Control Label and
      read the component count. */

      rec_get_handle( &rid, &data2->han, &han[0] );
      rec_get_rcl( &han[0], &rcl );
      dat1_get_ncomp( &han[0], &ncomp );
   }

/* Release the Structure Record Vector and expand the CRV if necessary. */

   rec_release_data( &data2->han, SZSRV, off, 'U', &srv );

   _call( hds_gl_status )

   szcrv = SZCRV;
   if (ncomp*szcrv == rcl.dlen)
      _call(rec_extend_record(&han[0], szcrv * hds_gl_ncomp0))

/* Locate the recipient Component Record Vector and ensure that an object of
   the same name does not already exist.        */

   _call( rec_locate_data( &han[0], rcl.dlen, 0, 'U', &crv ))
   for ( i = 0; i < ncomp; i++ )
   {
      dat1_locate_name( crv, i, &name1 );
      if ( _cheql( DAT__SZNAM, nambuf, name1 ) )
      {
         rec_release_data(&han[0], rcl.dlen, 0, 'U', &crv);
         _call(DAT__COMEX)
      }
   }

/* Identify the record of the object to be moved and save the name in the
   recipient component list.    */

   rec_get_rid( &data1->han, &rid );
   dat1_locate_name( crv, ncomp, &name1 );
   _chmove( DAT__SZNAM, nambuf, name1 );

/* Read the Record Control Label of the object to be moved and stick a handle
   on its parent component record.      */

   rec_get_rcl( &data1->han, &rcl1 );
   rec_get_handle( &rcl1.parent, &data1->han, &han[1] );

/* Determine whether the object is to be moved within the same container file
   or to another one.   */

   if( rec_same_file( &data1->han, &data2->han ) )
   {

/* If moving to within the same file, save the ID in the recipient component
   list and adopt the object record as the child of the destination component
   record. */

      dat1_pack_crv( &rid, ncomp, crv );
      rec_adopt_record( &data1->han, &han[0] );
   }

/* If moving to another file, manufacture a Component Record Vector for the
   input structure (only the RID component is needed) and use the MOVE_OBJECT
   routine to copy and erase the object.        */

   else                           /* NB, pro_exit won't have been called */
   {
      rec_get_rid( &data1->han, &rid1 );
      dat1_pack_crv( &rid1, 0, crv1 );
      _call( dat1_move_object( 1, &data1->han, crv1, &han[ 0 ],
                               crv + ncomp*SZCRV ) )
   }

/* Bump the component count in the destination object and release the CRV. */

   ++ncomp;
   dat1_put_ncomp( &han[0], ncomp );
   rec_release_data( &han[0], rcl.dlen, 0, 'U', &crv );

/* Read the Record Control Label of component record and adjust the count. */

   rec_get_rcl( &han[1], &rcl );
   dat1_get_ncomp( &han[1], &ncomp );
   --ncomp;

/* Component list now empty ?   */

   if (ncomp == 0)
   {
/*    If it is, then first delete the component record, stick a handle on
      the parent structure record and read its control label.   */

      rec_get_rid( &han[1], &rid );
      _call( rec_delete_record( &han[1]) )
      rec_get_handle( &rcl.parent, &han[1], &han[2] );
      rec_get_rcl( &han[2], &rcl );     /* Was han[1] but surely bug? */

/*    Locate the Structure Record Vector and clear the appropriate entry. */

      _call( rec_locate_data( &han[2], rcl.dlen, 0, 'U', &srv ))
      for ( i = 0; ; i += SZSRV )
      {
         dat1_unpack_srv( srv + i, &rid1 );
         if ( ( rid1.bloc == rid.bloc ) && ( rid1.chip == rid.chip ) )
         {
            dat1_pack_srv( &rec_gl_ridzero, srv + i );
            break;
         }
      }
      rec_release_data( &han[2], rcl.dlen, 0, 'U', &srv );
   }

/* Otherwise, more components exist.    */

   else
   {

/*    Locate the Component Record Vector and search for the ID of the record
      just 'moved'.     */

      _call( rec_locate_data( &han[1], rcl.dlen, 0, 'U', &crv ))
      entryy = 0;
      for ( entryy = 0; ; entryy++ )
      {
         dat1_unpack_crv( crv, entryy, &rid1 );
         if ( ( rid1.bloc == rid.bloc ) && ( rid1.chip == rid.chip ) )
            break;
      }

/*    If the object is not at the end of the component list, then left-shift
      all the trailing entries. */
      szcrv = SZCRV;
      (void) memmove( (void *) ( crv + entryy * szcrv ),
                      (void *) ( crv + ( entryy + 1 ) * szcrv ),
                      (size_t) ( ( ncomp - entryy ) * szcrv ) );

/* Release the Component Record Vector, shrink the record's dynamic domain  */
/* (if necessary, allowing some hysteresis) and write-back the new          */
/* component count.                                                         */
      rec_release_data( &han[ 1 ], rcl.dlen, 0, 'U', &crv );
      szcrv = SZCRV;
      if ( ( rcl.dlen - szcrv * ncomp ) >=
           ( 2 * szcrv * hds_gl_ncomp0 ) )
      {
          rec_shrink_record( &han[ 1 ], szcrv * hds_gl_ncomp0 );
      }
      dat1_put_ncomp( &han[ 1 ], ncomp );
   }

/* Annul the source LCP and nullify its locator value before returning.     */
   dat1_annul_lcp( &lcp1 );
   dat1_free_hdsloc( locator1 );

   return hds_gl_status;
}

int dat1_move_object(int ncomp, struct HAN *src, unsigned char *src_crv,
                     struct HAN *des, unsigned char *des_crv)

/*+
 * MOVE_OBJECT
 *
 * This routine moves an object recursively from one part of a container file
 * to another part of the same container file or to another container file. It
 * copies then erases and is intended for moving between container files since
 * a move within the same file can be accomplished very cheaply by adopting new
 * record parents.
 *
 * The handles passed as arguments are handles to Component Records and
 * the destination Component Record must have been created although need
 * not contain valid data. It is the caller's responsibility to fill in
 * component name information and this routine's responsibility to fill in
 * the Record IDs.
 *
 * The Component Record Vectors passed as arguments are merely pointers to the
 * dynamic domains of the associated records and are passed as arguments to
 * avoid unnecessary calls to REC_LOCATE. Only the RID component is used.
 *
 * It is possible to 'lie' about NCOMP, SRC_CRV and DES_CRV. For example,
 * to move the second component of the source object to the third component
 * of the destination object, one could pass NCOMP as 1, SRC_CRV as SRC_CRV+1
 * and DES_CRV as DES_CRV+2.
 *
 * Calling sequence:
 *
 *          MOVE_OBJECT(NCOMP,SRC,SRC_CRV,DES,DES_CRV)
 *
 * NCOMP    is the number of components in the source object. (ie the value of
 *          the static domain of the source Component Record)
 * SRC      is the address of a longword containing the handle to the Component
 *          Record of the source object. (Actually this is only used as the KIN
 *          argument to REC_STICK so can be a handle to any record in the
 *          source container file.)
 * SRC_CRV  is the address of the Component Record Vector of the source object.
 * DES      is the address of a longword containing the handle to the Component
 *          Record of the destination object.
 * DES_CRV  is the address of the Component Record Vector of the destination
 *          object.
 *
 * Routine value:
 *
 *          DAT__OK     if successful.
 */
{
   struct RCL rcl1; /* Record Control Label for Struc / Prim Record  */
   struct RCL rcl2; /* Record Control Label for next lev Compon Rec  */
   struct ODL odl1; /* Object Descriptor Label for Struc / Prim Rec  */
   unsigned char *src_srv1;   /* Pointer to src next level SRV */
   unsigned char *des_srv1;   /* Pointer to des next level SRV */
   unsigned char *src_crv2;   /* Pointer to source next CRV    */
   unsigned char *des_crv2;   /* Pointer to dest next CRV      */
   int           comp;        /* Component counter             */
   int           active;      /* Whether dynamic domain is active */
   struct HAN   src1; /* Handle to source Structure / Primitive Record */
   struct HAN   des1; /* Handle to destin Structure / Primitive Record */
   unsigned char *spntr1; /* Pointer to source Struc / Prim dynamic domain */
   unsigned char *dpntr1; /* Pointer to destin Struc / Prim dynamic domain */
   int nelem;             /* Number of elements in source structure array  */
   int axis;              /* Axis counter                                  */
   int elem;              /* Element counter                               */
   struct HAN src2;       /* Handle to source next level Component Record  */
   struct HAN des2;       /* Handle to destin next level Component Record  */
   int        ncomp2;     /* Number of components at next level            */
   struct RID src_rid;
   struct RID des_rid;
   struct RID rid1;

/* Go through each component of the source object moving it to the destination
   object. First stick a handle on the Record ID in the Component Record and
   then peek at the Record Control Label. Remember whether the dynamic domain
   is active.   */

   for ( comp=0; comp<ncomp; comp++ )
   {
      dat1_unpack_crv( src_crv, comp, &rid1 );
      rec_get_handle( &rid1, src, &src1 );
      _invoke( rec_get_rcl( &src1,&rcl1 ))
      active  = rcl1.active;

/* Next, using the same Record Control Label (only class, zero, slen and
   dlen fields are needed), create an identical record in the destination
   and copy the static and dynamic domains. At this stage we do not care
   whether the object is primitive or structured. Locating the dynamic domain
   for write access automatically sets it active - it should be active if
   the domain it was copied from is active.     */

      _invoke( rec_create_record( des, &rcl1, &des1 ))
      _invoke( dat1_get_odl( &src1,&odl1 ))
      _invoke( dat1_put_odl( &des1,&odl1 ))
      _invoke( rec_locate_data( &src1, rcl1.dlen, 0, 'R', &spntr1 ) )
      _invoke( rec_locate_data( &des1, rcl1.dlen, 0, 'W', &dpntr1 ) )
      _chmove( rcl1.dlen, spntr1, dpntr1);
      if (!active)
         _invoke( rec_reset_record( &des1 ) )

/* Identify the new record in the appropriate field of the destination Component
   Record Vector.       */

      rec_get_rid( &des1, &rid1 );
      dat1_pack_crv( &rid1, comp, des_crv );

/* If the component is primitive this is all that is necessary. If it is
   structured then it may be an array, so calculate the number of elements and
   cast the source and destination components' dynamic domains as Structure
   Record Vectors.      */

      if (rcl1.class == DAT__STRUCTURE)
      {
         nelem = 1;
         for ( axis=0; axis < odl1.naxes; axis++ )
            nelem *= odl1.axis[axis];
         src_srv1 = (unsigned char *) spntr1;
         des_srv1 = (unsigned char *) dpntr1;

/* Now, go through the elements of the structure array and for those which
   have a Component Record, create an identical one in the destination and
   copy the static (number of components) and dynamic (component names and
   their Record IDs) domains. Note that the Record IDs copied to the destination
   are not valid - the valid values will be filled in by the recursive call
   to MOVE_OBJECT. Also note the same handling of the "dynamic domain active"
   flag.        */

         for ( elem = 0; elem < nelem; elem++ )
         {
            dat1_unpack_srv( src_srv1 + ( elem * SZSRV ), &src_rid );
            if  ( ( src_rid.bloc != 0 ) || ( src_rid.chip != 0 ) )
            {
               rec_get_handle( &src_rid, &src1, &src2 );
               _invoke( rec_get_rcl( &src2, &rcl2 ) )
               active = rcl2.active;
               _invoke( rec_create_record( &des1, &rcl2, &des2 ) )
               _invoke( dat1_get_ncomp( &src2, &ncomp2 ) )
               _invoke( dat1_put_ncomp( &des2, ncomp2 ) )
               _invoke( rec_locate_data( &src2, rcl2.dlen, 0, 'R',
                                         &src_crv2 ) )
               _invoke( rec_locate_data( &des2, rcl2.dlen, 0, 'W',
                                         &des_crv2 ) )
               _chmove( rcl2.dlen, src_crv2, des_crv2 );
               if (!active) _invoke( rec_reset_record( &des2 ) )

/* Identify the new record in the appropriate field of the destination      */
/* Structure Record Vector.                                                 */
               rec_get_rid( &des2, &des_rid );
               dat1_pack_srv( &des_rid,
                              des_srv1 + ( elem * SZSRV ) );

/* Call MOVE_OBJECT to move this element from source to destination.        */
               _invoke( dat1_move_object( ncomp2, &src2, src_crv2,
                                          &des2, des_crv2 ) )

/* Ditch the source and destination Component Record dynamic domains.       */
               rec_release_data( &src2, rcl2.dlen, 0, 'R', &src_crv2 );
               rec_release_data( &des2, rcl2.dlen, 0, 'W', &des_crv2 );

/* Rubout the source Component Record Vector and move on to the next        */
/* element.                                                                 */
               _invoke( rec_delete_record( &src2 ) )
            }
         }
      }

/* Ditch the source and destination Structure / Primitive Record dynamic
   domains.     */

      rec_release_data( &src1, rcl1.dlen, 0, 'R', &spntr1 );
      rec_release_data( &des1, rcl1.dlen, 0, 'W', &dpntr1 );

/* Having copied and erased all the sub-structures of this component, rubout
   the source object record and move on to the next component.  */

      _invoke( rec_delete_record( &src1 ))
   }
   return hds_gl_status;
}
