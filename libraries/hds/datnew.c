#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
/*+DATNEW.C-*/

/* Include files */

#include "f77.h"		 /* F77 <-> C interface macros		    */
#include "ems.h"		 /* EMS error reporting routines	    */

#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "str.h"		 /* Character string import/export macros   */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

/* Control Blocks */



   F77_INTEGER_FUNCTION(dat_new)
                       (locator_str,name_str,type_str,ndim,dims,status,
	 locator_lenarg,name_lenarg,type_lenarg)

/*================================*/
/* DAT_NEW - Create new component */
/*================================*/                    

struct STR	 	 *locator_str;
struct STR	 	 *name_str;
struct STR	 	 *type_str;
int			 *ndim;
int			 *dims;
int			 *status;
int	  		  locator_lenarg;
int	  		  name_lenarg;
int	  		  type_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_NEW_ERR"
#define context_message\
        "DAT_NEW: Error creating a new HDS component."

struct DSC		  locator;
struct DSC		  name;
struct DSC		  type;
int			  locator_len = locator_lenarg;
int			  name_len = name_lenarg;
int			  type_len = type_lenarg;

struct LCP		 *lcp;
struct LCP_DATA	 	 *data;
struct PDD	  	  obj;
unsigned char *srv;
unsigned char *crv;
char			  nambuf[DAT__SZNAM];
char			  typbuf[DAT__SZTYP];
struct RCL		  rcl;
struct ODL		  odl;
struct HAN		  han[2];
int			  size;
int			  ncomp;
struct RID		  rid;
int			  off;
int			  i;
char *name1;
struct RID rid1;

/* Enter routine.	*/

if (!_ok(*status))
	return *status;
hds_gl_status = DAT__OK;

/* Import locator, name and type strings.	*/

_strimp(&locator,locator_str,&locator_len);
_strimp(&name,name_str,&name_len);
_strimp(&type,type_str,&type_len);

/* Import locator.	*/

_call(dau_import_loc(&locator, &lcp))
data	      = &lcp->data;

/* Return if the locator points to anything other than a single structure
   object or if the container file was opened for read-only access. 	*/

if (!data->struc || data->naxes != 0)
	_call(DAT__OBJIN)
if (data->read)
	_call(DAT__ACCON)

/* Validate the object name and type specifications.	*/

_call(dau_check_name(&name, nambuf))
_call(dat1_check_type(&type, typbuf))

/* Determine the object attributes and verify the shape.	*/

_call(dat1_unpack_type(typbuf, &obj))
_call(dau_check_shape(*ndim, dims, &odl))

/* Calculate the total size of the object.	*/

size          = 1;
for (i=0; i<*ndim; i++)
	size *= dims[i];

/* Locate the Structure Record Vector entry which contains the ID of the
   component record.	*/

off           = data->offset * DAT__SZSRV;
_call(rec_locate_data(&data->han, DAT__SZSRV, off, 'U', &srv))
dat1_unpack_srv( srv, &rid );

/* If the component Record-ID is null, then create a new record.	*/

if ( ( rid.bloc == 0 ) && ( rid.chip == 0 ) )
	{
	rcl.class    = DAT__COMPONENT;
        rcl.zero = 0;
	rcl.slen     = DAT__SZNCOMP;
	rcl.dlen     = DAT__SZCRV * hds_gl_ncomp;
	rec_create_record(&data->han, &rcl, &han[0]);
	rec_get_rid(&han[0], &rid);

/* Pack the new record ID into the Structure Record Vector.		    */
        dat1_pack_srv( &rid, srv );
	hds_gl_ncomp = hds_gl_ncomp0;
	ncomp        = 0;
	}

/* Otherwise, stick a handle on the component record, get the Record Control
   Label and read the component count.	*/

else
	{
	rec_get_handle(&rid, &data->han, &han[0]);
	rec_get_rcl(&han[0], &rcl);
	dat1_get_ncomp(&han[0], &ncomp);
	}

/* Unmap the Structure Record Vector and expand the Component Record Vector
   if necessary.	*/

rec_release_data(&data->han, DAT__SZSRV, off, 'U', &srv);
_call(hds_gl_status)
if (ncomp*DAT__SZCRV == rcl.dlen)
	_call(rec_extend_record(&han[0], DAT__SZCRV * hds_gl_ncomp0))

/* If the structure currently has components, then locate the Component Record
   Vector and ensure that an object of the same name does not already exist. */

if (ncomp > 0)
	{
	rid = rec_gl_ridzero;
	_call(rec_locate_data(&han[0], rcl.dlen, 0, 'R', &crv))
	for ( i = 0; i < ncomp; i++ )
	{
	   dat1_locate_name( crv, i, &name1 );
           if ( _cheql( DAT__SZNAM, nambuf, name1 ) )
           {
	      dat1_unpack_crv( crv, i, &rid );
              break;
           }
	}
	rec_release_data(&han[0], rcl.dlen, 0, 'R', &crv);
	if ( ( rid.bloc != 0 ) || ( rid.chip != 0 ) )
		_call(DAT__COMEX)
	}

/* Fill the appropriate fields in the Record Control Label for the new object
   and create the record.	*/

rcl.class     =  obj.class;
rcl.zero      = (obj.class == DAT__STRUCTURE);
rcl.slen      = DAT__SZTYP + DAT__SZNDIM + ( *ndim * DAT__SZDIM );
rcl.dlen      = size * obj.length;
_call(rec_create_record(&han[0], &rcl, &han[1]))

/* Save the type specification and write the Object Descriptor Label to
   the record's static domain.	*/

_chmove(DAT__SZTYP, typbuf, odl.type);
_call(dat1_put_odl(&han[1], &odl))

/* Remap the Component Record Vector and save the name specification and
   Record-ID of the new object.	*/

_call(rec_locate_data(&han[0], DAT__SZCRV, ncomp*DAT__SZCRV, 'W', &crv))
dat1_locate_name( crv, 0, &name1 );
_chmove( DAT__SZNAM, nambuf, name1 );
rec_get_rid( &han[1], &rid1 );
dat1_pack_crv( &rid1, 0, crv );
rec_release_data(&han[0], DAT__SZCRV, ncomp*DAT__SZCRV, 'W', &crv);

/* Bump the component count.	*/

++ncomp;
_call(dat1_put_ncomp(&han[0], ncomp))
return hds_gl_status;
}
