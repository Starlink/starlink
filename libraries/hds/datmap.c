#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
/*+DATMAP.C-*/

#include <string.h>
#include "f77.h"		 /* F77 <-> C interface macros		    */
#include "ems.h"		 /* EMS error reporting routines	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "str.h"		 /* Character string import/export macros   */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

#if defined( vms )
#include <descrip.h>		 /* VMS descriptor definitions		    */
#endif

   F77_INTEGER_FUNCTION(dat_map)(struct STR *locator_str,
                                 struct STR *type_str,
				 struct STR *mode_str,
				 int *ndim,
				 int *dims,
				 POINTER(pntr),
		                 int *status,
				 int locator_lenarg,
				 int type_lenarg,
				 int mode_lenarg)

/*====================*/
/* DAT_MAP - Map data */
/*====================*/

{
#undef context_name
#undef context_message
#define context_name "DAT_MAP_ERR"
#define context_message\
        "DAT_MAP: Error mapping an HDS primitive."

GENPTR_POINTER(pntr)

struct DSC		  locator;
struct DSC		  type;
struct DSC		  mode;
int			  locator_len = locator_lenarg;
int			  type_len = type_lenarg;
int			  mode_len = mode_lenarg;

struct LCP		 *lcp;
struct LCP_DATA		 *data;
struct LCP_STATE	 *state;
char			  typbuf[DAT__SZTYP];
int			  axis[DAT__MXDIM];
struct RCL		  rcl;
struct PDD	 	 *app;
struct PDD	 	 *obj;
unsigned char *dom;
int			  naxes;
int			  reading;
int			  objlen;
int			  objoff;
int			  applen;
int			  i;
int nbad;
int temp_status;
void *retpntr;

#if defined( vms )
struct dsc$descriptor *dsc;
#endif

/* Enter routine.	*/

if (!_ok(*status))
	return *status;
hds_gl_status = DAT__OK;

/* Import the locator, type and mode strings.	*/

_strimp(&locator,locator_str,&locator_len);
_strimp(&type,type_str,&type_len);
_strimp(&mode,mode_str,&mode_len);

/* Import the locator.	*/

_call(dau_import_loc(&locator, &lcp))
data	      = &lcp->data;
state	      = &data->state;

/* Ensure that there is no currently mapped data and that the object is
   primitive.	*/

if (state->mapped)
	_call(DAT__PRMAP)
if (data->struc)
	_call(DAT__OBJIN)

/* Determine the shape of the object and match the dimensions.	*/

_call(dau_get_shape(data, &naxes, axis))
if (*ndim != naxes)
	_call(DAT__DIMIN)
for (i=0; i<naxes; i++)
	if (dims[i] != axis[i])
		_call(DAT__DIMIN)

/* Validate the application data type, and the access mode. Return if the
   container file was opened for read-only access and the access mode is
   'WRITE' or 'UPDATE'.	*/

_call(dat1_check_type(&type, typbuf))
dat1_check_mode( (const char *) mode.body, mode.length, &data->mode,
		 &hds_gl_status );
_call( hds_gl_status )
reading       = (data->mode != 'W');
if (data->read && data->mode != 'R')
	_call(DAT__ACCON)

/* Determine the attributes of the application data and reject the operation
   if not primitive.	*/

_call(dat1_unpack_type(typbuf, &data->app))
app           = &data->app;
if (app->class != DAT__PRIMITIVE)
	_call(DAT__TYPIN)

/* Match the object and applications data attributes and reject the operation
   if the types are incompatible. 	*/

obj           = &data->obj;
_call(dau_match_types(obj, app))

/* Ensure that the object data is 'active' if reading or updating.	    */
_call(rec_get_rcl(&data->han, &rcl))
if (reading && !rcl.active)
	_call(DAT__UNSET)
                        
/* If the mapping is character-type and an explicit character string length */
/* was not supplied (i.e. there was no '*' character in the type	    */
/* specification), then modify the application data attributes descriptor   */
/* to describe a character array (or scalar) of an appropriate length to    */
/* hold the character-formatted values.  In this situation the character    */
/* string length of the application's data is simply made identical to that */
/* of the object data if they are both character type.			    */
      if ( app->dtype == DAT__C )
      {
         if ( memchr( (void *) type.body, '*', type.length ) == NULL )
	 {
	    if ( obj->dtype != DAT__C )
	    {
	       app->length = dat_gl_ndr[ obj->dtype ].txtsize;
	    }
	    else
	    {
	       app->length = obj->length;
	    }
         }
      }

/* If the object and application data types differ in any respect, then a   */
/* virtual memory copy of the data is returned to the program. See if this  */
/* is necessary.							    */
      state->unlike = ( obj->dtype != app->dtype ) ||
                      ( obj->length != app->length ) ||
		      ( obj->format != app->format ) ||
		      ( obj->order != app->order );
      state->vmcopy = state->unlike;

/* Calculate the length (in bytes) of the virtual memory required for the
   application program data and the corresponding length of the object data.
   Determine the byte offset into the object record's dynamic domain.	*/

applen        = app->length * data->size;
objlen        = obj->length * data->size;
objoff        = obj->length * data->offset;

/* Gather discontiguous object data if the program is reading or updating. */

if      (state->broken)
	{
	_call( rec_alloc_xmem( applen, (void **) &app->body ) )
	if (reading)
		dau_gather_data( 1, data, &nbad );
	}

/* If a copy of the object data is to be returned, then locate the record's
   dynamic domain and translate the data to the allocated virtual memory. */

else if (state->vmcopy)
	{
	_call( rec_alloc_xmem( applen, (void **) &app->body ) )
	if (reading)
		{
		_call(rec_locate_data(&data->han, objlen, objoff, 'R', &dom))
		obj->body	= dom;
		dat1_cvt( 1, data->size, obj, app, &nbad );

/* NB workaround for problems with EMS...				    */
		temp_status = hds_gl_status;
		if ( hds_gl_status = DAT__CONER ) hds_gl_status = DAT__OK;
		rec_release_data(&data->han, objlen, objoff, 'R', &dom);
		if ( _ok( hds_gl_status ) ) hds_gl_status = temp_status;
		}
	}

/* Otherwise, the application program gains direct access to the object data. */

else
	{
	rec_locate_data(&data->han, objlen, objoff, data->mode, &dom);
	app->body = dom;
	}

/* Save the current setting of the global file-mapping flag, for use when   */
/* the data are un-mapped.						    */
      data->filemap = hds_gl_map;

/* Obtain the returned pointer value and note if the object is mapped.	    */
      retpntr = (void *) app->body;
      state->mapped = ( retpntr != NULL );

/* If using VMS and the mapped data are character-type, then construct a    */
/* VMS descriptor to describe the mapped values (this descriptor is stored  */
/* in the LCP). Modify the returned pointer to point at this descriptor	    */
/* instead.								    */
#if defined( vms )
      if ( app->dtype == DAT__C )
      {
         dsc = &data->vmsdsc;
	 dsc->dsc$w_length = app->length;
	 dsc->dsc$b_dtype = DSC$K_DTYPE_T;
	 dsc->dsc$b_class = ( naxes == 0 ) ? DSC$K_CLASS_S : DSC$K_CLASS_A;
	 dsc->dsc$a_pointer = (char *) app->body;
	 retpntr = (void *) dsc;
      }
#endif

/* Return a Fortran pointer to the object data. */
      *pntr = cnf_fptr( retpntr );

/* If conversion errors occurred, then report contextual information.	    */
      if ( hds_gl_status == DAT__CONER )
      {
         ems_seti_c( "NBAD", nbad );
	 ems_rep_c( "DAT_MAP_2",
	            "A total of ^NBAD data conversion error(s) occurred.",
		    &hds_gl_status );
      }

_call(hds_gl_status)
return hds_gl_status;
}
   F77_INTEGER_FUNCTION(dat_mapi)(struct STR *locator_str,
				  struct STR *mode_str,
				  int *ndim,
				  int *dims,
				  POINTER(pntr),
				  int *status,
				  int locator_lenarg,
				  int mode_lenarg)

/*==============================*/
/* DAT_MAPI - Map  Integer data */
/*==============================*/

{
#undef context_name
#undef context_message
#define context_name "DAT_MAPI_ERR"
#define context_message\
        "DAT_MAPI: Error mapping an HDS primitive as integer values."

GENPTR_POINTER(pntr)

struct STR     	     	  type_str;
int			  type_lenarg;

_strconst(&type_str,"_INTEGER");
type_lenarg = _strlen(&type_str);

F77_CALL(dat_map)(locator_str, &type_str, mode_str, ndim, dims, pntr, status,
	 locator_lenarg, type_lenarg, mode_lenarg);
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_mapr)(struct STR *locator_str,
				  struct STR *mode_str,
				  int *ndim,
				  int *dims,
				  POINTER(pntr),
				  int *status,
				  int locator_lenarg,
				  int mode_lenarg)

/*===========================*/
/* DAT_MAPR - Map  Real data */
/*===========================*/

{
#undef context_name
#undef context_message
#define context_name "DAT_MAPR_ERR"
#define context_message\
        "DAT_MAPR: Error mapping an HDS primitive as real values."

GENPTR_POINTER(pntr)

struct STR     	     	  type_str;
int			  type_lenarg;

_strconst(&type_str,"_REAL");
type_lenarg = _strlen(&type_str);

F77_CALL(dat_map)(locator_str, &type_str, mode_str, ndim, dims, pntr, status,
	 locator_lenarg, type_lenarg, mode_lenarg);
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_mapd)(struct STR *locator_str,
				  struct STR *mode_str,
				  int *ndim,
				  int *dims,
				  POINTER(pntr),
				  int *status,
				  int locator_lenarg,
				  int mode_lenarg)

/*=======================================*/
/* DAT_MAPD - Map  Double precision data */
/*=======================================*/

{
#undef context_name
#undef context_message
#define context_name "DAT_MAPD_ERR"
#define context_message\
        "DAT_MAPD: Error mapping an HDS primitive as double precision values."

GENPTR_POINTER(pntr)

struct STR     	     	  type_str;
int			  type_lenarg;

_strconst(&type_str,"_DOUBLE");
type_lenarg = _strlen(&type_str);

F77_CALL(dat_map)(locator_str, &type_str, mode_str, ndim, dims, pntr, status,
	 locator_lenarg, type_lenarg, mode_lenarg);
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_mapl)(struct STR *locator_str,
				  struct STR *mode_str,
				  int *ndim,
				  int *dims,
				  POINTER(pntr),
				  int *status,
				  int locator_lenarg,
				  int mode_lenarg)

/*==============================*/
/* DAT_MAPL - Map  Logical data */
/*==============================*/

{
#undef context_name
#undef context_message
#define context_name "DAT_MAPL_ERR"
#define context_message\
        "DAT_MAPL: Error mapping an HDS primitive as logical values."

GENPTR_POINTER(pntr)

struct STR     	     	  type_str;
int			  type_lenarg;

_strconst(&type_str,"_LOGICAL");
type_lenarg = _strlen(&type_str);

F77_CALL(dat_map)(locator_str, &type_str, mode_str, ndim, dims, pntr, status,
	 locator_lenarg, type_lenarg, mode_lenarg);
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_mapc)(struct STR *locator_str,
                                  struct STR *mode_str,
				  int *ndim,
				  int *dims,
				  POINTER(pntr),
				  int *status,
				  int locator_lenarg,
				  int mode_lenarg)

/*================================*/
/* DAT_MAPC - Map  Character data */
/*================================*/

{
#undef context_name
#undef context_message
#define context_name "DAT_MAPC_ERR"
#define context_message\
        "DAT_MAPC: Error mapping an HDS primitive as character values."

GENPTR_POINTER(pntr)

struct STR     	     	  type_str;
int			  type_lenarg;

_strconst(&type_str,"_CHAR");
type_lenarg = _strlen(&type_str);

F77_CALL(dat_map)(locator_str, &type_str, mode_str, ndim, dims, pntr, status,
	 locator_lenarg, type_lenarg, mode_lenarg);
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_basic)(struct STR *locator_str,
				   struct STR *mode_str,
				   POINTER(pntr),
				   int *length,
				   int *status,
				   int locator_lenarg,
				   int mode_lenarg)

/*===============================================*/
/* DAT_BASIC - Map data (in basic machine units) */
/*===============================================*/

{
#undef context_name
#undef context_message
#define context_name "DAT_BASIC_ERR"
#define context_message\
        "DAT_BASIC: Error mapping an HDS primitive as basic machine units."

GENPTR_POINTER(pntr)

struct DSC		  locator;
struct DSC		  mode;
int			  locator_len = locator_lenarg;
int			  mode_len = mode_lenarg;

struct LCP    		 *lcp;
struct LCP_DATA		 *data;
struct LCP_STATE	 *state;
struct RCL		  rcl;
struct PDD	 	 *app;
struct PDD	 	 *obj;
unsigned char *dom;
int			  reading;
int			  objlen;
int			  objoff;
int nbad;
unsigned char *retpntr;

/* Enter routine.	*/

if (!_ok(*status))
	return *status;
hds_gl_status = DAT__OK;

/* Import the locator and mode strings.	*/

_strimp(&locator,locator_str,&locator_len);
_strimp(&mode,mode_str,&mode_len);

/* Import the locator.	*/

_call(dau_import_loc(&locator, &lcp))
data	      = &lcp->data;
state	      = &data->state;

/* Ensure that there is no currently mapped data and that the object is
   primitive.	*/

if (state->mapped)
	_call(DAT__PRMAP)
if (data->struc)
	_call(DAT__OBJIN)

/* Validate the access mode. Return if the container file was opened for
   read-only access and the mode is 'WRITE' or 'UPDATE'.	*/

dat1_check_mode( (const char *) mode.body, mode.length, &data->mode,
		 &hds_gl_status );
_call( hds_gl_status )
reading       = (data->mode != 'W');
if (data->read && data->mode != 'R')
	_call(DAT__ACCON)

/* Copy the object data attributes to the application data attributes
   descriptor.	*/

data->app = data->obj;
app           = &data->app;
obj           = &data->obj;

/* Ensure that the object data is 'active' if reading or updating.	    */

_call(rec_get_rcl(&data->han, &rcl))
if (reading && !rcl.active)
	_call(DAT__UNSET)
state->vmcopy = 0;

/* Calculate the length (in bytes) of the virtual memory required for the
   object data. Determine the byte offset into the object record's dynamic
   domain.	*/

objlen        = obj->length * data->size;
objoff        = obj->length * data->offset;

/* Gather discontiguous object data if the program is reading or updating. */

if      (state->broken)
	{
	_call( rec_alloc_xmem( objlen, (void **) &app->body ) )
	if (reading)
		dau_gather_data( 1, data, &nbad );
	}

/* Otherwise, the application program gains direct access to the object data. */

else
	{
	rec_locate_data(&data->han, objlen, objoff, data->mode, &dom);
	app->body = dom;
	}

/* Save the current setting of the global file-mapping flag, for use when   */
/* the data are un-mapped.						    */
      data->filemap = hds_gl_map;

/* Obtain the returned pointer value and its length. Note if the object is  */
/* mapped.								    */
      retpntr = app->body;
      *length = objlen;
      state->mapped = ( retpntr != NULL );

/* Return a Fortran pointer to the object data. */
      *pntr = cnf_fptr( retpntr );

_call(hds_gl_status)
return hds_gl_status;
}
   F77_INTEGER_FUNCTION(dat_unmap)
                       (locator_str,status,locator_lenarg)

/*========================*/
/* DAT_UNMAP - Unmap data */
/*========================*/

struct STR 		*locator_str;
int			*status;
int			 locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_UNMAP_ERR"
#define context_message\
        "DAT_UNMAP: Error unmapping an HDS primitive."

struct DSC		 locator;
int			 locator_len = locator_lenarg;

struct LCP    		*lcp;
struct LCP_DATA		*data;

/* Enter routine and import the locator.	*/

if (!_ok(*status))
	return *status;
hds_gl_status = DAT__OK;

/* Import locator string and locator.	*/

_strimp(&locator,locator_str,&locator_len);
_call(dau_import_loc(&locator, &lcp))
data	      = &lcp->data;

/* Return if the locator is associated with a structure.		    */

if (data->struc)
	_call(DAT__OBJIN)

/* Otherwise, flush any mapped data.                                        */
else
{
   dau_flush_data( data );
   _call( hds_gl_status );
}
return hds_gl_status;
}
