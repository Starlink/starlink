#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "f77.h"		 /* F77 <-> C interface macros		    */
#include "ems.h"		 /* EMS error reporting routines	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "str.h"		 /* Character string import/export macros   */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   F77_INTEGER_FUNCTION(hds_new)
                       (file_str,name_str,type_str,ndim,dims,locator_str,
	 status,file_lenarg,name_lenarg,type_lenarg,locator_lenarg)

/*=====================================*/
/* HDS_NEW - Create new container file */
/*=====================================*/
/* RFWS: Make rcl.zero be zero - otherwise it contains junk */

struct STR 	 	 *file_str;
struct STR	 	 *name_str;
struct STR	 	 *type_str;
int			 *ndim;
int			 *dims;
struct STR	 	 *locator_str;
int	  		 *status;
int			  file_lenarg;
int	   		  name_lenarg;
int			  type_lenarg;
int			  locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "HDS_NEW_ERR"
#define context_message\
        "HDS_NEW: Error creating a new HDS container file."

struct DSC		  file;
struct DSC		  name;
struct DSC		  type;
struct DSC		  locator;
int	     		  file_len = file_lenarg;
int			  name_len = name_lenarg;
int			  type_len = type_lenarg;
int			  locator_len = locator_lenarg;

struct LCP	 	 *lcp;
struct LCP_DATA		 *data;
unsigned char *crv;
int        		(*dbt)[2];
struct PDD	 	 *obj;
struct RCL		  rcl;
struct ODL		  odl;
struct HAN	 	  han;
int		   	  i;
char *name1;
struct RID rid1;
int refcnt;

/* Enter routine.	*/

if (!_ok(*status))
	return *status;
hds_gl_status	= DAT__OK;

/* Import file, name and type strings and export locator string.	*/

_strimp(&file,file_str,&file_len);
_strimp(&name,name_str,&name_len);
_strimp(&type,type_str,&type_len);
_strexp(&locator,locator_str,&locator_len);

/* Export the locator.	*/

_call(dau_export_loc(&locator, &lcp))
data		= &lcp->data;

/* Validate the object name and type.	*/

_call(dau_check_name(&name, data->name))
_call(dat1_check_type(&type, data->type))

/* Determine the object attributes and validate the shape.	*/

_call(dat1_unpack_type(data->type, &data->obj))
obj		= &data->obj;
_call(dau_check_shape(*ndim, dims, &odl))

/* Save the shape information in the LCP and calculate the total size
   of the object. (The Dimension Bounds Table holds the 1st 3 axis sizes). */

data->naxes	= odl.naxes;
dbt		= data->bounds;
data->size	= 1;
for (i=0; i<data->naxes; i++)
	{
	data->size *= dims[i];
	if ( i<DAT__MXSLICE)
		{
		dbt[i][LOWER] = 1;
		dbt[i][UPPER] = dims[i];
		}
	}

/* Attach to a new file and return a handle to the container record.	*/

rcl.zero = 1;
rcl.class	= DAT__CONTAINER;
rcl.slen	= 0;
rcl.dlen	= DAT__SZCRV;
rec_attach_file( 1, (const char *) file.body, file.length, 'N', 'W', &rcl,
		 &han );
_call( hds_gl_status )

/* Setup the Record Control Label for the top-level object and create
   the record.	*/

rcl.class	= obj->class;
rcl.zero	= (obj->class == DAT__STRUCTURE);
rcl.slen	= DAT__SZTYP + DAT__SZNDIM + ( *ndim * DAT__SZDIM );
rcl.dlen	= data->size * obj->length;
_call(rec_create_record(&han, &rcl, &data->han))
data->parent = rcl.parent;

/* Save the type specification and write the Object Descriptor Label
   to the record's static domain.	*/

_chmove(DAT__SZTYP, data->type, odl.type);
_call(dat1_put_odl(&data->han, &odl))

/* Map the container record's dynamic domain and save the Record-ID of
   the new top-level object record.	*/

_call(rec_locate_data(&han, DAT__SZCRV, 0, 'W', &crv))
dat1_locate_name( crv, 0, &name1 );
_chmove( DAT__SZNAM, data->name, name1 );
rec_get_rid( &data->han, &rid1 );
dat1_pack_crv( &rid1, 0, crv );
rec_release_data(&han, DAT__SZCRV, 0, 'W', &crv);

/* Flag whether structure or primitive.					    */
data->struc	= (obj->class == DAT__STRUCTURE);

/* Make the output locator a primary locator, increment the container file  */
/* reference count and mark the locator as valid.			    */
      lcp->primary = 1;
      rec_refcnt( &han, 1, &refcnt, &hds_gl_status );
      data->valid = 1;

return hds_gl_status;
}
