#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
/*+DATPUT.C-*/

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
   F77_INTEGER_FUNCTION(dat_put)
                       ( struct STR *locator_str,
                         struct STR *type_str,
                         int *ndim,
                         int *dims,
                         unsigned char *values,
                         int *status,
	                 int locator_lenarg,
                         int type_lenarg,
                         int values_lenarg);

   F77_INTEGER_FUNCTION(dat_put)
                       (locator_str,type_str,ndim,dims,values,status,
	 locator_lenarg,type_lenarg,values_lenarg)

/*======================*/
/* DAT_PUT - Write data */
/*======================*/

struct STR	 	 *locator_str;
struct STR	 	 *type_str;
int		    	 *ndim;
int			 *dims;
unsigned char *values;
int	  		 *status;
int			  locator_lenarg;
int			  type_lenarg;
int			  values_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_PUT_ERR"
#define context_message\
        "DAT_PUT: Error writing value(s) to an HDS primitive."

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
hds_gl_status = DAT__OK;

/* Import the locator and type strings.	*/

_strimp(&locator,locator_str,&locator_len);
_strimp(&type,type_str,&type_len);

/* Import the locator.	*/

_call(dau_import_loc(&locator, &lcp))
data	      = &lcp->data;
state	      = &data->state;

/* Ensure that there is no currently mapped data and that the object is
   primitive. Also check that the container file was not opened for read-
   only access.	*/

if (state->mapped)
	_call(DAT__PRMAP)
if (data->struc)
	_call(DAT__OBJIN)
if (data->read)
	_call(DAT__ACCON)

/* Determine the shape of the object and match the dimensions.	*/

_call(dau_get_shape(data, &naxes, axis))
if (*ndim != naxes)
	_call(DAT__DIMIN)
for (i=0; i<naxes; ++i)
	if (dims[i] != axis[i])
		_call(DAT__DIMIN)

/* Validate the application data type specification.	*/

_call(dat1_check_type(&type, typbuf))

/* Determine the attributes of the application data and reject the operation
   if not primitive.	*/

_call(dat1_unpack_type(typbuf, &data->app))
app           = &data->app;
if (app->class != DAT__PRIMITIVE)
	_call(DAT__TYPIN)

/* Match the object and application data attributes and reject the operation
   if the types are incompatible. 	*/

obj           = &data->obj;
_call(dau_match_types(obj, app))

/* If the application is passing character data, then obtain a descriptor   */
/* and derive a pointer to the character data for insertion into the	    */
/* application Primitive Data Descriptor. Override the application data	    */
/* length in the PDD with the actual length of the string(s) supplied.	    */
      if ( app->dtype == DAT__C )
      {
         _strimp( &appdsc, (struct STR *) values, &values_len );
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

/* If necessary, allocate a correctly aligned buffer to hold the	    */
/* application data and copy the data into it. Modify the application	    */
/* primitive data descriptor to point at this buffer.			    */
      if ( fixalign )
      {
	 rec_alloc_mem( app->length * data->size, (void **) &buf );
         if ( _ok( hds_gl_status ) )
         {
            (void) memcpy( (void *) buf, (void *) values,
	                   (size_t) ( app->length * data->size ) );
         }
	 app->body = buf;
      }

/* Scatter the object data if discontiguous.				    */
      if ( state->broken )
      {
         dau_scatter_data( 1, data, &nbad );
      }

/* Otherwise, locate the record's dynamic domain and translate the data	    */
/* from the source buffer.						    */
      else
      {
         rec_locate_data( &data->han, objlen, objoff, 'W', &dom );
	 obj->body = dom;
	 dat1_cvt( 1, data->size, app, obj, &nbad );
	 rec_release_data( &data->han, objlen, objoff, 'W', &dom );
      }

/* If a temporary buffer has been used to ensure correct data alignment,    */
/* then deallocate it.							    */
      if ( fixalign )
      {
         rec_deall_mem( app->length * data->size, (void **) &buf );
      }

/* Check the global status value before return.				    */
      _call( hds_gl_status )
      return hds_gl_status;
   }

   F77_INTEGER_FUNCTION(dat_puti)
                       (locator_str,ndim,dims,values,status,locator_lenarg)

/*===============================*/
/* DAT_PUTI - Write Integer data */
/*===============================*/

struct STR	 	 *locator_str;
int			 *ndim;
int			 *dims;
int			 *values;
int			 *status;
int			  locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_PUTI_ERR"
#define context_message\
        "DAT_PUTI: Error writing integer values to an HDS primitive."

struct STR     	     	  type_str;
int			  type_lenarg;
int dummy;

_strconst(&type_str,"_INTEGER");
type_lenarg = _strlen(&type_str);
            
dummy = 0;
F77_CALL(dat_put)
      (locator_str, &type_str, ndim, dims, (unsigned char *)values, status,
      locator_lenarg,
	 type_lenarg, dummy );
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_putr)
                       (locator_str,ndim,dims,values,status,locator_lenarg)

/*============================*/
/* DAT_PUTR - Write Real data */
/*============================*/

struct STR	 	 *locator_str;
int			 *ndim;
int			 *dims;
int			 *values;
int			 *status;
int			  locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_PUTR_ERR"
#define context_message\
        "DAT_PUTR: Error writing real values to an HDS primitive."

struct STR     	     	  type_str;
int			  type_lenarg;
int dummy;

_strconst(&type_str,"_REAL");
type_lenarg = _strlen(&type_str);

dummy = 0;
F77_CALL(dat_put)
      (locator_str, &type_str, ndim, dims, (unsigned char *)values, status,
      locator_lenarg,
	 type_lenarg, dummy);
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_putd)
                       (locator_str,ndim,dims,values,status,locator_lenarg)

/*========================================*/
/* DAT_PUTD - Write Double precision data */
/*========================================*/

struct STR	 	 *locator_str;
int			 *ndim;
int			 *dims;
int			 *values;
int			 *status;
int			  locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_PUTD_ERR"
#define context_message\
        "DAT_PUTD: Error writing double precision value(s) to an HDS primitive."

struct STR     	     	  type_str;
int			  type_lenarg;
int dummy;

_strconst(&type_str,"_DOUBLE");
type_lenarg = _strlen(&type_str);

dummy = 0;
F77_CALL(dat_put)
      (locator_str, &type_str, ndim, dims, (unsigned char *)values, status,
      locator_lenarg,
	 type_lenarg, dummy);
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_putl)
                       (locator_str,ndim,dims,values,status,locator_lenarg)

/*===============================*/
/* DAT_PUTL - Write Logical data */
/*===============================*/

struct STR	 	 *locator_str;
int			 *ndim;
int			 *dims;
int			 *values;
int			 *status;
int			  locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_PUTL_ERR"
#define context_message\
        "DAT_PUTL: Error writing logical values to an HDS primitive."

struct STR     	     	  type_str;
int			  type_lenarg;
int dummy;

_strconst(&type_str,"_LOGICAL");
type_lenarg = _strlen(&type_str);

dummy = 0;
F77_CALL(dat_put)
      (locator_str, &type_str, ndim, dims, (unsigned char *)values, status,
      locator_lenarg,
	 type_lenarg, dummy);
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_putc)
                       (locator_str,ndim,dims,values,status,locator_lenarg,values_lenarg)

/*=================================*/
/* DAT_PUTC - Write Character data */
/*=================================*/

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
#define context_name "DAT_PUTC_ERR"
#define context_message\
        "DAT_PUTC: Error writing character value(s) to an HDS primitive."

struct STR     	     	  type_str;
int			  type_lenarg;

_strconst(&type_str,"_CHAR");
type_lenarg = _strlen(&type_str);

F77_CALL(dat_put)
      (locator_str, &type_str, ndim, dims, (unsigned char *)values, status,
      locator_lenarg,
	 type_lenarg, values_lenarg);
return hds_gl_status;
}
