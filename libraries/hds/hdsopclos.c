#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
/*+HDSOPCLOS.C-*/

/* Include files */

#include "f77.h"		 /* F77 <-> C interface macros		    */
#include "cnf.h"		 /* F77 <-> C string handling functions	    */
#include "ems.h"		 /* EMS error reporting routines	    */

#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "str.h"		 /* Character string import/export macros   */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

/* Control Blocks */



   F77_INTEGER_FUNCTION(hds_open)
                       (file_str,mode_str,locator_str,status,file_lenarg,
	  mode_lenarg,locator_lenarg)

/*=========================================*/
/* HDS_OPEN - Open existing container file */
/*=========================================*/

struct STR	 	 *file_str;
struct STR	 	 *mode_str;
struct STR	 	 *locator_str;
int			 *status;
int			  file_lenarg;
int			  mode_lenarg;
int			  locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "HDS_OPEN_ERR"
#define context_message\
        "HDS_OPEN: Error opening an HDS container file."

struct DSC		  file;
struct DSC		  mode;
struct DSC		  locator;
int			  file_len = file_lenarg;
int			  mode_len = mode_lenarg;
int			  locator_len = locator_lenarg;

struct LCP		 *lcp;
struct LCP_DATA		 *data;
unsigned char *crv;
int			 *axis;
int		 	(*dbt)[2];
struct PDD	 	 *obj;
struct RCL		  rcl;
struct ODL		  odl;
struct HAN		  han;
int			  i;
char *name1;
struct RID rid1;
int refcnt;

/* Enter routine.	*/

if (!_ok(*status))
	return *status;
hds_gl_status	= DAT__OK;

/* Import file and mode strings and export locator string.	*/

_strimp(&file,file_str,&file_len);
_strimp(&mode,mode_str,&mode_len);
_strexp(&locator,locator_str,&locator_len);

/* Export the locator.	*/

_call(dau_export_loc(&locator, &lcp))
data		= &lcp->data;

/* Validate the access mode and open the file.	*/

dat1_check_mode( (const char *) mode.body, mode.length, &data->mode,
		 &hds_gl_status );
_call( hds_gl_status )
rec_attach_file( 1, (const char *) file.body, file.length, 'O', data->mode,
	         &rcl, &han );
_call( hds_gl_status )

/* Map the container record's dynamic domain, copy the object name
   and attach a handle to the top-level object.	*/

_call(rec_locate_data(&han, DAT__SZCRV, 0, 'R', &crv))
dat1_locate_name( crv, 0, &name1 );
_chmove( DAT__SZNAM, name1, data->name );
dat1_unpack_crv( crv, 0, &rid1 );
rec_get_handle( &rid1, &han, &data->han );
rec_release_data(&han, DAT__SZCRV, 0, 'R', &crv);

/* Save the parent RID.	*/

rec_get_rid(&han, &data->parent);

/* Read the Object Descriptor Label from the object record's static
   domain and determine the object attributes.	*/

_call(dat1_get_odl(&data->han, &odl))
_chmove(DAT__SZTYP, odl.type, data->type);
_call(dat1_unpack_type(data->type, &data->obj))
obj	      	= &data->obj;

/* Save the shape information in the LCP and calculate the total size
   of the object. (The Dimension Bounds Table is used to hold the 1st
   three axis sizes). 	*/

axis		= odl.axis;
data->naxes	= odl.naxes;
dbt		= data->bounds;
data->size	= 1;
for (i=0; i<data->naxes; ++i)
	{
	data->size *= axis[i];
	if (i < DAT__MXSLICE)
		{
		dbt[i][LOWER] = 1;
		dbt[i][UPPER] = axis[i];
		}
	}

/* Fill in Locator Control Packet fields, making this a primary locator and */
/* incrementing the container file reference count.			    */
data->struc	= (obj->class == DAT__STRUCTURE);
data->read	= (data->mode == 'R');
lcp->primary = 1;
rec_refcnt( &han, 1, &refcnt, &hds_gl_status );
data->valid	= 1;
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(hds_close)
                       (locator_str,status,locator_lenarg)

/*==================================*/
/* HDS_CLOSE - Close container file */
/*==================================*/

struct STR		*locator_str;
int	  		*status;
int			 locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "HDS_CLOSE_ERR"
#define context_message\
        "HDS_CLOSE: Error closing an HDS container file."

struct DSC		 locator;
int			 locator_len = locator_lenarg;

struct LCP		*lcp;
struct LCP_DATA		*data;

/* Enter routine.	*/

if(!_ok(*status))
	return *status;
hds_gl_status = DAT__OK;

/* Import the locator string and the locator.	*/

_strimp(&locator,locator_str,&locator_len);
_call(dau_import_loc(&locator, &lcp))
data	      = &lcp->data;

/* Return if the locator is not associated with a top-level object.	*/
if (data->level != 0)
	_call(DAT__OBJIN)

/* Otherwise, mark the Locator Control Packet as "primary" and annul the    */
/* LCP (which will close the container file if its reference count drops to */
/* zero). Note that this process fails to conserve the reference count for  */
/* the file, but it reproduces the historical behaviour of this routine,    */
/* which has always been anomalous in this respect.			    */
      lcp->primary = 1;
      dat1_annul_lcp( &lcp );

/* Nullify the locator value.						    */
      cnf_expn( DAT__NOLOC, DAT__SZLOC, (char *) locator.body,
                (int) locator.length );

/* Exit the routine.							    */
      return hds_gl_status;
   }
