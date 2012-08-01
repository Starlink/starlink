#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+DAUCOPY.C-*/

/* Include files */

#include "ems.h"                 /* EMS error reporting routines            */

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Private rec_ definitions                */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

int
dau_copy_object(int ncomp,
                struct HAN *src,
                unsigned char *src_crv,
                struct HAN *des,
                unsigned char *des_crv)

/*+
 * DAU_COPY_OBJECT
 *
 * This routine copies an object recursively from one part of a container file
 * to another part of the same container file or to another container file.
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
 * to copy the second component of the source object to the third component
 * of the destination object, one could pass NCOMP as 1, SRC_CRV as SRC_CRV+1
 * and DES_CRV as DES_CRV+2.
 *
 * Calling sequence:
 *
 *          DAU_COPY_OBJECT(NCOMP,SRC,SRC_CRV,DES,DES_CRV)
 *
 * NCOMP    is the number of components in the source object. (ie the value of
 *          the static domain of the source Component Record)
 * SRC        is the address of a longword containing the handle to the Component
 *                Record of the source object. (Actually this is only used as the KIN
 *                argument to REC_STICK so can be a handle to any record in the
 *                source container file.)
 * SRC_CRV  is the address of the Component Record Vector of the source object.
 * DES      is the address of a longword containing the handle to the Component
 *          Record of the destination object.
 * DES_CRV  is the address of the Component Record Vector of the destination
 *          object.
 *
 * Routine value:
 *
 *     DAT__OK if successful
 */
{
   struct RCL rcl1;         /* Record Control Label for Struc / Prim Record  */
   struct RCL rcl2;         /* Record Control Label for next lev Compon Rec  */
   struct ODL odl1;         /* Object Descriptor Label for Struc / Prim Rec  */
   unsigned char *src_srv1; /* Pointer to src next level Struc Record Vector */
   unsigned char *des_srv1; /* Pointer to des next level Struc Record Vector */
   unsigned char *src_crv2; /* Pointer to source next Compon Record Vector   */
   unsigned char *des_crv2; /* Pointer to destin next Compon Record Vector   */
   int comp;                /* Component counter                             */
   int active;              /* Whether dynamic domain is active              */
   struct HAN src1;         /* Handle to source Structure / Primitive Record */
   struct HAN   des1;       /* Handle to destin Structure / Primitive Record */
   unsigned char *spntr1;   /* Pointer to source Struc / Prim dynamic domain */
   unsigned char *dpntr1;   /* Pointer to destin Struc / Prim dynamic domain */
   UINT_BIG nelem=0;        /* Number of elements in source structure array  */
   UINT_BIG nelem1;         /* Number of elements in source component array  */
   int axis;                /* Axis counter                                  */
   UINT_BIG elem;           /* Element counter                               */
   struct HAN src2;         /* Handle to source next level Component Record  */
   struct HAN des2;         /* Handle to destin next level Component Record  */
   int ncomp2;              /* Number of components at next level            */
   struct RID src_rid;
   struct RID des_rid;
   struct RID rid1;
   UINT_BIG dlen1;          /* Record dlen storage                           */
   UINT_BIG dlen2;          /* Record dlen storage                           */

/* Go through each component of the source object copying it to the destination
   object. First stick a handle on the Record ID in the Component Record and
   then peek at the Record Control Label. Remember whether the dynamic domain
   is active.   */

   for (comp=0; comp<ncomp; comp++)
   {
      SET_64BIT_MODE(src);
      dat1_unpack_crv( src_crv, comp, &rid1 );
      rec_get_handle( &rid1, src, &src1 );
      _invoke(rec_get_rcl(&src1,&rcl1))
      active = rcl1.active;
      _invoke(dat1_get_odl(&src1,&odl1))
      _invoke(rec_locate_data(&src1,rcl1.dlen,0,'R',&spntr1))
      dlen1 = rcl1.dlen;

/* Next, using the same Record Control Label (only class, zero, slen and
   dlen fields are needed), create an identical record in the destination
   and copy the static and dynamic domains. At this stage we do not care
   whether the object is primitive or structured. Locating the dynamic domain
   for write access automatically sets it active - it should be active iff
   the domain it was copied from is active.     */

      SET_64BIT_MODE(des);
      if( rcl1.class == DAT__STRUCTURE ) {
         nelem = 1;
         for (axis=0; axis<odl1.naxes; axis++)
            nelem *= odl1.axis[axis];
         rcl1.dlen = nelem * SZSRV;
      }
      _invoke(rec_create_record(des,&rcl1,&des1))
      _invoke(dat1_put_odl(&des1,&odl1))
      _invoke(rec_locate_data(&des1,rcl1.dlen,0,'W',&dpntr1))
      if( rcl1.class != DAT__STRUCTURE &&
          rcl1.class != DAT__COMPONENT )
         _chmove(rcl1.dlen,spntr1,dpntr1);
       else
          memset( (void *) dpntr1, 0, rcl1.dlen );
      if (!active)
         _invoke(rec_reset_record(&des1))

/* Identify the new record in the appropriate field of the destination
   Component Record Vector. */

      rec_get_rid( &des1, &rid1 );
      dat1_pack_crv( &rid1, comp, des_crv );

/* If the component is primitive this is all that is necessary. If it is
   structured then it may be an array, so calculate the number of elements and
   cast the source and destination components' dynamic domains as Structure
   Record Vectors.      */

      if (rcl1.class == DAT__STRUCTURE)
      {
          src_srv1 = spntr1;
          des_srv1 = dpntr1;

/* Now, go through the elements of the structure array and for those which
   have a Component Record, create an identical one in the destination and
   copy the static (number of components) and dynamic (component names and
   their Record IDs) domains. Note that the Record IDs copied to the destination
   are not valid - the valid values will be filled in by the recursive call
   to DAU_COPY_OBJECT. Also note the same handling of the "dynamic domain
   active" flag.        */

         for ( elem = 0; elem < nelem; elem++ )
         {
            SET_64BIT_MODE(src);
            dat1_unpack_srv( src_srv1 + ( elem * SZSRV ), &src_rid);
            if  ( ( src_rid.bloc != 0 ) || ( src_rid.chip != 0 ) )
            {
               rec_get_handle(&src_rid,&src1,&src2);
               _invoke(rec_get_rcl(&src2,&rcl2))
               active = rcl2.active;
               _invoke(dat1_get_ncomp(&src2,&ncomp2))
               _invoke(rec_locate_data(&src2,rcl2.dlen,0,'R',&src_crv2))
               nelem1 = rcl2.dlen/SZCRV;
               dlen2  = rcl2.dlen;

               SET_64BIT_MODE(des);
               rcl2.dlen = nelem1 * SZCRV;
               _invoke(rec_create_record(&des1,&rcl2,&des2))
               _invoke(dat1_put_ncomp(&des2,ncomp2))
               _invoke(rec_locate_data(&des2,rcl2.dlen,0,'W',&des_crv2))
               if( rcl2.dlen == dlen2 )
                  _chmove(rcl2.dlen,src_crv2,des_crv2);
               else
               {
                  int szcrv2 = SZCRV;
                  int szcrv1 = dlen2/nelem1;
                  unsigned char *p1, *p2;
                  int i;

                  memset( (void *) des_crv2, 0, rcl2.dlen);
                  for( p1 = src_crv2, p2 = des_crv2, i = 0; i <nelem1; i++ )
                  {
                     _chmove( DAT__SZNAM, p1, p2 );
                     p1 += szcrv1;
                     p2 += szcrv2;
                  }
                }

               if (!active)
                  _invoke(rec_reset_record(&des2))

/* Identify the new record in the appropriate field of the destination       */
/* Structure Record Vector.                                                  */
               rec_get_rid( &des2, &des_rid );
               dat1_pack_srv( &des_rid, des_srv1 + ( elem * SZSRV) );

/* Call DAU_COPY_OBJECT to copy this element from source to destination. */
               _invoke(dau_copy_object(ncomp2,&src2,src_crv2, &des2,des_crv2))

/* Ditch the source and destination Component Record dynamic domains and
   move on to the next element. */
               SET_64BIT_MODE(src);
               rec_release_data(&src2,dlen2,0,'R',&src_crv2);
               SET_64BIT_MODE(des);
               rec_release_data(&des2,rcl2.dlen,0,'W',&des_crv2);
            }
         }
      }

/* Ditch the source and destination Structure / Primitive Record dynamic
   domains and move on to the next component.   */

      SET_64BIT_MODE(src);
      rec_release_data(&src1,dlen1,0,'R',&spntr1);
      SET_64BIT_MODE(des);
      rec_release_data(&des1,rcl1.dlen,0,'W',&dpntr1);
   }
   return hds_gl_status;
}
