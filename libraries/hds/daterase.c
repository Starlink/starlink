#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
/*+DATERASE.C-*/

/* 24-Nov-88 - WFL - Add missing REC_$DITCH call */

/* Include files */

#include "f77.h"		 /* F77 <-> C interface macros		    */
#include "ems.h"		 /* EMS error reporting routines	    */

#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "str.h"		 /* Character string import/export macros   */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

/* Control Blocks */



   F77_INTEGER_FUNCTION(dat_erase)
                       (locator_str,name_str,status,locator_lenarg,name_lenarg)

/*==========================*/
/* DAT_ERASE - Erase object */
/*==========================*/

struct STR	 	 *locator_str;
struct STR	       	 *name_str;
int			 *status;
int	  		 locator_lenarg;
int	  		 name_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_ERASE_ERR"
#define context_message\
        "DAT_ERASE: Error erasing an HDS structure component."

struct DSC		 locator;
struct DSC		 name;
int			 locator_len = locator_lenarg;
int			 name_len = name_lenarg;

struct LCP		 *lcp;
struct LCP_DATA		 *data;
unsigned char *srv;
unsigned char *crv;
char			  nambuf[DAT__SZNAM];
struct RCL		  rcl;
struct HAN 		  han;
struct RID		  rid;
int			  off;
int			  ncomp;
int			  entryy;  /* 'entry' is a reserved word */
int			  i;
char *name1;

/* Enter routine.	*/

if (!_ok(*status))
	return *status;
hds_gl_status = DAT__OK;

/* Import locator and name strings.	*/

_strimp(&locator,locator_str,&locator_len);
_strimp(&name,name_str,&name_len);

/* Import locator.	*/

_call(dau_import_loc(&locator, &lcp))
data	      = &lcp->data;

/* Return if the locator points to anything other than a single structure
   object or if the container file was opened for read-only access.	*/

if (!data->struc || data->naxes != 0)
	_call(DAT__OBJIN)
if (data->read)
	_call(DAT__ACCON)

/* Validate the component name.	*/

_call(dau_check_name(&name, nambuf))

/* Locate the Structure Record Vector entry and extract the ID of the
   component record.	*/

off           = data->offset * DAT__SZSRV;
_call(rec_locate_data(&data->han, DAT__SZSRV, off, 'R', &srv))
dat1_unpack_srv( srv, &rid );
rec_release_data(&data->han, DAT__SZSRV, off, 'R', &srv);

/* If the Record-ID is null, then no component record exists.	*/

if ( ( rid.bloc == 0) && ( rid.chip == 0 ) )
	_call(DAT__OBJNF)

/* Otherwise, stick a handle on the component record, get the Record Control
   Label and determine the # of components in the list.	*/

_call(rec_get_handle(&rid, &data->han, &han))
_call(rec_get_rcl(&han, &rcl))
_call(dat1_get_ncomp(&han, &ncomp))

/* Locate the Component Record Vector and search for the specified name. If
   found, then save the Record-ID and entry #.	*/


_call(rec_locate_data(&han, rcl.dlen, 0, 'R', &crv))
rid           = rec_gl_ridzero;
for ( i = 0; i < ncomp; i++ )
{
   dat1_locate_name( crv, i, &name1 );
   if ( _cheql( DAT__SZNAM, nambuf, name1 ) )
   {
      dat1_unpack_crv( crv, i, &rid );
      entryy = i;
      break;
   }
}
rec_release_data(&han, rcl.dlen, 0, 'R', &crv);

/* Return if the name was not found.	*/

if ( ( rid.bloc == 0 ) && ( rid.chip == 0 ) )
	_call(DAT__OBJNF)

/* ERASE_OBJECT does the work. Note that its HAN argument can be a handle
   to any record in the container file.	*/

_call(rec_locate_data(&han, DAT__SZCRV, entryy*DAT__SZCRV, 'R', &crv))
_call(dat1_erase_object(1, &data->han, crv))
rec_release_data(&han, DAT__SZCRV, entryy*DAT__SZCRV, 'R', &crv);

/* Decrement the component count.	*/

--ncomp;

/* If the object just deleted was the only one in the component list, then
   also delete the component record, clear the entry in the Structure
   Record Vector, and return.	*/

if (ncomp == 0)
	{
	_call(rec_delete_record(&han))
	_call(rec_locate_data(&data->han, DAT__SZSRV, off, 'W', &srv))
        dat1_pack_srv( &rec_gl_ridzero, srv );
	rec_release_data(&data->han, DAT__SZSRV, off, 'W', &srv);
	return hds_gl_status;
	}

/* Otherwise, if the object is not at the end of the component list, then
   left-shift all the trailing entries in the Component Record Vector.	*/

_call(rec_get_rcl(&han, &rcl))
if (ncomp != entryy)
	{
	_call(rec_locate_data(&han, rcl.dlen, 0, 'U', &crv))
        (void) memmove( (void *) ( crv + entryy * DAT__SZCRV ),
                        (const void *) ( crv + ( entryy + 1 ) * DAT__SZCRV ),
                        (size_t) ( ( ncomp - entryy ) * DAT__SZCRV ) );
	rec_release_data(&han, rcl.dlen, 0, 'U', &crv);
	}

/* Shrink the record's dynamic domain (if necessary, allowing some	    */
/* hysteresis) and reset the component count.				    */

if ( ( rcl.dlen - DAT__SZCRV * ncomp ) >= ( 2 * DAT__SZCRV * hds_gl_ncomp0 ) )
{
   _call( rec_shrink_record( &han, DAT__SZCRV * hds_gl_ncomp0 ) )
}
_call(dat1_put_ncomp(&han, ncomp))
return hds_gl_status;
}

dat1_erase_object(ncomp,kin,crv)

/*+
 * ERASE_OBJECT
 *
 * This routine erases an object recursively.
 *
 * The handle argument is a handle to any record in the container file.
 *
 * The Component Record Vector argument is merely a pointer to the dynamic
 * domain of the associated record and is passed as an argument to avoid
 * unnecessary calls to REC_LOCATE. Only the RID component is used.
 *
 * It is possible to 'lie' about NCOMP and CRV. For example, to erase the
 * second component of the object, one could pass NCOMP as 1 and CRV as CRV+1.
 *
 * Calling sequence:
 *
 *	    ERASE_OBJECT(NCOMP,KIN,CRV)
 *
 * NCOMP    is the number of components in the object. (ie the value of
 *	    the static domain of the Component Record)
 * KIN	    is the address of a longword containing the handle to any record
 *	    in the container file.
 * CRV      is the address of the Component Record Vector of the object.
 *
 * Routine value:
 *
 *	    DAT__OK	if successful.
 */

int		 ncomp;
struct HAN *kin;
unsigned char *crv;

{
struct RCL	 rcl;	/* Record Control Label for Struc / Prim Record	*/
struct RCL	 rcl2;	/* Record Control Label for next lev Compon Rec	*/
struct ODL	 odl;	/* Object Descriptor Label for Structure Record	*/
unsigned char *srv;	/* Pointer to src next level Struc Record Vector*/
unsigned char *crv2;	/* Pointer to next level Component Record Vector*/
int		 comp;	/* Component counter				*/
struct HAN 	 han;	/* Handle to Structure / Primitive Record	*/
struct HAN	 han2;	/* Handle to next level Component Record	*/
int		 nelem;	/* Number of elements in structure array	*/
int		 axis;	/* Axis counter					*/
int		 elem;	/* Element counter			     	*/
int		 ncomp2;/* Number of components at next level		*/
struct RID rid;
struct RID rid1;

/* Go through each component of the source object erasing it. First stick a
   handle on the Record ID in the Component Record and then peek at the Record
   Control Label.	*/

for (comp=0; comp<ncomp; comp++)
	{
        dat1_unpack_crv( crv, comp, &rid1 );
	rec_get_handle( &rid1, kin, &han );
	_invoke( rec_get_rcl( &han, &rcl ) )

/* If the component is structured then it is necessary to erase the sub-
   structures. It may be an array, so read the Object Descriptor Label,
   calculate the number of elements and locate the Structure Record Vector. */

	if (rcl.class == DAT__STRUCTURE)
		{
		_invoke(dat1_get_odl(&han, &odl))
		nelem		 = 1;
		for (axis=0; axis<odl.naxes; axis++)
			nelem	*= odl.axis[axis];
		_invoke(rec_locate_data(&han, rcl.dlen, 0, 'R', &srv))

/* Now, go through the elements of the structure array and for those which  */
/* have a Component Record read / locate the number of components and the   */
/* next level Component Record Vector.					    */
		for ( elem = 0; elem < nelem; elem++ )
                {
		   dat1_unpack_srv( srv + ( elem * DAT__SZSRV ), &rid );
		   if  ( ( rid.bloc != 0 ) || ( rid.chip != 0 ) )
		   {
		      rec_get_handle( &rid, &han, &han2 );
		      _invoke( rec_get_rcl( &han2, &rcl2 ) )
		      _invoke( dat1_get_ncomp( &han2, &ncomp2 ) )
		      _invoke( rec_locate_data( &han2, rcl2.dlen, 0, 'R',
				                &crv2 ) )

/* Call ERASE_OBJECT to erase the sub-structure.	*/
       		      _invoke(dat1_erase_object(ncomp2,&han2,crv2))

/* Ditch and rubout the next level Component Record Vector.	*/
		      rec_release_data(&han2, rcl2.dlen, 0, 'R', &crv2);
		      _invoke(rec_delete_record(&han2))
		   }
                }

/* Ditch the object record.	*/
	      	_invoke(rec_release_data(&han, rcl.dlen, 0, 'R', &srv))
		}

/* Having erased all the sub-structures of this component, rubout the object
   record.	*/

 	_invoke(rec_delete_record(&han))
	}
return hds_gl_status;
}
