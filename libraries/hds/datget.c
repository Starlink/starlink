#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
/*+DATGET.C-*/

/* Include files */

#include "f77.h"		 /* F77 <-> C interface macros		    */
#include "ems.h"		 /* EMS error reporting routines	    */

#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "str.h"		 /* Character string import/export macros   */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

/* Control Blocks */

/* Function prototype.                                                      */
   F77_INTEGER_FUNCTION(dat_get)
                       ( struct STR *locator_str,
                         struct STR *type_str,
                         int *ndim,
                         int *dims,
                         unsigned char *values,
                         int *status,
                         int locator_lenarg,
                         int type_lenarg,
                         int values_lenarg );


   F77_INTEGER_FUNCTION(dat_get)
                       (locator_str,type_str,ndim,dims,values,status,
	 locator_lenarg,type_lenarg,values_lenarg)

/*=====================*/
/* DAT_GET - Read data */
/*=====================*/

struct STR	 	 *locator_str;
struct STR	 	 *type_str;
int			 *ndim;
int			 *dims;
unsigned char *values;
int			 *status;
int			  locator_lenarg;
int			  type_lenarg;
int			  values_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_GET_ERR"
#define context_message\
        "DAT_GET: Error reading value(s) from an HDS primitive object."

struct DSC		  locator;
struct DSC		  type;
int			  locator_len = locator_lenarg;
int			  type_len = type_lenarg;
int			  values_len = values_lenarg;

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
int			  objlen;
int			  objoff;
int			  i;
int nbad;
struct DSC appdsc;
unsigned char *buf;		 /* Pointer to temporary alignment buffer   */
int fixalign;			 /* Is alignment fixup needed?		    */

/* Enter routine.	*/

if (!_ok(*status))
	return *status;
hds_gl_status      = DAT__OK;

/* Import the type and locator strings.	*/

_strimp(&type,type_str,&type_len);
_strimp(&locator,locator_str,&locator_len);

/* Import the locator.	*/

_call(dau_import_loc(&locator, &lcp))
data	           = &lcp->data;
state		   = &data->state;

/* Ensure that there is no currently mapped data and that the object is
   primitive.	*/

if (state->mapped)
	_call(DAT__PRMAP)
if (data->struc)
	_call(DAT__OBJIN)

/* Determine the shape of the object and match the dimensions.	*/

_call(dau_get_shape(data, &naxes, axis))
if (*ndim != naxes)
{
   _call(DAT__DIMIN)
}
for (i=0; i<naxes; i++)
{
   if (dims[i] != axis[i])
   {
      _call(DAT__DIMIN)
   }
}

/* Validate the application data type specification.	*/

_call(dat1_check_type(&type, typbuf))

/* Determine the attributes of the application data and reject the operation
   if not primitive.	*/

_call(dat1_unpack_type(typbuf, &data->app))
app                = &data->app;
if (app->class != DAT__PRIMITIVE)
	_call(DAT__TYPIN)

/* Match the object and application data attributes and reject the operation
   if the types are incompatible. 	*/

obj                = &data->obj;
_call(dau_match_types(obj, app))

/* Ensure that the object data is 'active'.	*/

_call(rec_get_rcl(&data->han, &rcl))
if (!rcl.active)
	_call(DAT__UNSET)

/* If the application is reading character data, then obtain a descriptor   */
/* and derive a pointer to the character buffer supplied for insertion into */
/* the application Primitive Data Descriptor. Override the application data */
/* length in the PDD with the actual length of the buffer supplied.	    */
      if ( app->dtype == DAT__C )
      {
         _strexp( &appdsc, (struct STR *) values, &values_len );
         app->body = appdsc.body;
         app->length = appdsc.length;
      }

/* If non-character data are being passed, then simply insert a pointer to  */
/* the data into the PDD.						    */
      else
      {
         app->body = values;
      }

/* Calculate the length (in bytes) of the object data and determine the	    */
/* byte offset into the object record's dynamic domain.			    */
      objlen = obj->length * data->size;
      objoff = obj->length * data->offset;

/* If the application data type is _DOUBLE and data type or format	    */
/* conversion must occur, then test whether the application buffer is	    */
/* adequately aligned.							    */
      fixalign = 0;
      if ( ( app->dtype == DAT__D ) &&
           ( ( app->dtype != obj->dtype ) || ( app->format != obj->format ) ) )
      {
         fixalign = !_aligned_D( app->body );
      }

/* If necessary, allocate a correctly aligned buffer to hold the results    */
/* and modify the application primitive data descriptor to point at this    */
/* buffer.								    */
      if ( fixalign )
      {
	 rec_alloc_mem( app->length * data->size, (void **) &buf );
	 app->body = buf;
      }

/* Gather the object data if discontiguous.				    */
      if ( state->broken )
      {
         dau_gather_data( 1, data, &nbad );
      }

/* Otherwise, locate the record's dynamic domain and translate the data to  */
/* the destination buffer.						    */
      else
      {
         rec_locate_data( &data->han, objlen, objoff, 'R', &dom );
	 obj->body = dom;
	 dat1_cvt( 1, data->size, obj, app, &nbad );
	 rec_release_data( &data->han, objlen, objoff, 'R', &dom );
      }

/* If a temporary buffer has been used to ensure correct data alignment,    */
/* copy the results to the true application buffer and deallocate the	    */
/* temporary one.							    */
      if ( fixalign )
      {
         if ( _ok( hds_gl_status ) )
         {
            (void) memcpy( (void *) values, (void *) buf,
	                   (size_t) ( app->length * data->size ) );
         }
         rec_deall_mem( app->length * data->size, (void **) &buf );
      }

/* Check the global status value before return.				    */
      _call(hds_gl_status)
      return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_geti)
                       (locator_str,ndim,dims,values,status,locator_lenarg)

/*==============================*/
/* DAT_GETI - Read Integer data */
/*==============================*/

struct STR	 	 *locator_str;
int			 *ndim;
int			 *dims;
int			 *values;
int			 *status;
int			  locator_lenarg;


{
#undef context_name
#undef context_message
#define context_name "DAT_GETI_ERR"
#define context_message\
        "DAT_GETI: Error reading integer value(s) from an HDS primitive."

struct STR     	     	  type_str;
int			  type_lenarg;
int dummy;

_strconst(&type_str,"_INTEGER");
type_lenarg = _strlen(&type_str);

dummy = 0;
F77_CALL(dat_get)(locator_str, &type_str, ndim, dims,
                           (unsigned char *)values, status, locator_lenarg,
	                   type_lenarg, dummy );
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_getr)
                       (locator_str,ndim,dims,values,status,locator_lenarg)

/*===========================*/
/* DAT_GETR - Read Real data */
/*===========================*/

struct STR	 	 *locator_str;
int			 *ndim;
int			 *dims;
int			 *values;
int			 *status;
int			  locator_lenarg;


{
#undef context_name
#undef context_message
#define context_name "DAT_GETR_ERR"
#define context_message\
        "DAT_GETR: Error reading real value(s) from an HDS primitive."

struct STR     	     	  type_str;
int			  type_lenarg;
int dummy;

_strconst(&type_str,"_REAL");
type_lenarg = _strlen(&type_str);

dummy = 0;
F77_CALL(dat_get)(locator_str, &type_str, ndim, dims,
			   (unsigned char *) values,
                             status, locator_lenarg,
	 type_lenarg, dummy );
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_getd)
                       (locator_str,ndim,dims,values,status,locator_lenarg)

/*=======================================*/
/* DAT_GETD - Read Double precision data */
/*=======================================*/

struct STR	 	 *locator_str;
int			 *ndim;
int			 *dims;
double *values;
int			 *status;
int			  locator_lenarg;


{
#undef context_name
#undef context_message
#define context_name "DAT_GETD_ERR"
#define context_message\
        "DAT_GETD: Error reading double precision value(s) from an HDS primitive."

struct STR     	     	  type_str;
int			  type_lenarg;
int dummy;

_strconst(&type_str,"_DOUBLE");
type_lenarg = _strlen(&type_str);

dummy = 0;
F77_CALL(dat_get)(locator_str, &type_str, ndim, dims,
			   (unsigned char *) values,
                             status, locator_lenarg, type_lenarg, dummy );
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_getl)
                       (locator_str,ndim,dims,values,status,locator_lenarg)

/*==============================*/
/* DAT_GETL - Read Logical data */
/*==============================*/

struct STR	 	 *locator_str;
int			 *ndim;
int			 *dims;
int			 *values;
int			 *status;
int			  locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_GETL_ERR"
#define context_message\
        "DAT_GETL: Error reading logical value(s) from an HDS primitive."

struct STR     	     	  type_str;
int			  type_lenarg;
int dummy;

_strconst(&type_str,"_LOGICAL");
type_lenarg = _strlen(&type_str);

dummy = 0;
F77_CALL(dat_get)(locator_str, &type_str, ndim, dims,
			   (unsigned char *) values,
		             status, locator_lenarg, type_lenarg, dummy );
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_getc)
                       (locator_str,ndim,dims,values,status,locator_lenarg,values_lenarg)

/*================================*/
/* DAT_GETC - Read Character data */
/*================================*/

struct STR	 	 *locator_str;
int			 *ndim;
int			 *dims;
int			 *values;
int			 *status;
int			  locator_lenarg;
int			  values_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_GETC_ERR"
#define context_message\
        "DAT_GETC: Error reading character value(s) from an HDS primitive."

struct STR     	     	  type_str;
int			  type_lenarg;

_strconst(&type_str,"_CHAR");
type_lenarg = _strlen(&type_str);

F77_CALL(dat_get)(locator_str, &type_str, ndim, dims,
			   (unsigned char *) values,
                             status, locator_lenarg, type_lenarg,
			     values_lenarg);
return hds_gl_status;
}
