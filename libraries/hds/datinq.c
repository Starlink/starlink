#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
/*+DATINQ.C-*/

#include <stdio.h>
#include "f77.h"		 /* F77 <-> C interface macros		    */
#include "ems.h"		 /* EMS error reporting routines	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "str.h"		 /* Character string import/export macros   */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   F77_INTEGER_FUNCTION(dat_name)
                       (locator_str,name_str,status,locator_lenarg,name_lenarg)

/*==========================*/
/* DAT_NAME - Object name ? */
/*==========================*/

struct STR		*locator_str;
struct STR	      	*name_str;
int			*status;
int	  		 locator_lenarg;
int	  		 name_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_NAME_ERR"
#define context_message\
        "DAT_NAME: Error enquiring the name of an HDS object."

struct DSC		 locator;
struct DSC		 name;
int			 locator_len = locator_lenarg;
int			 name_len = name_lenarg;

struct LCP	     	*lcp;
struct LCP_DATA		*data;

/* Enter routine.	*/

if (!_ok(*status))
	return *status;
hds_gl_status     = DAT__OK;

/* Import locator and name strings.	*/

_strimp(&locator,locator_str,&locator_len);
_strimp(&name,name_str,&name_len);

/* Import locator.	*/

_call(dau_import_loc(&locator, &lcp))
data	          = &lcp->data;

/* Copy the object name from the LCP.	*/

_chcopy( DAT__SZNAM, data->name, ' ', name.length, name.body );
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_type)
                       (locator_str,type_str,status,locator_lenarg,type_lenarg)

/*==========================*/
/* DAT_TYPE - Object type ? */
/*==========================*/

struct STR		*locator_str;
struct STR	      	*type_str;
int			*status;
int	  		 locator_lenarg;
int	  		 type_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_TYPE_ERR"
#define context_message\
        "DAT_TYPE: Error enquiring the type of an HDS object."

struct DSC		 locator;
struct DSC		 type;
int			 locator_len = locator_lenarg;
int			 type_len = type_lenarg;

struct LCP	     	*lcp;
struct LCP_DATA		*data;

int nc;
char buf[ DAT__SZTYP + 1 ];

/* Enter routine.	*/

if (!_ok(*status))
	return *status;
hds_gl_status     = DAT__OK;

/* Import locator and type strings.	*/

_strimp(&locator,locator_str,&locator_len);
_strimp(&type,type_str,&type_len);

/* Import locator.	*/

_call(dau_import_loc(&locator, &lcp))
data	          = &lcp->data;

   if ( data->obj.class == DAT__PRIMITIVE )
   {
      switch ( data->obj.dtype )
      {
         case DAT__D:
            {
	       _chcopy( 7, "_DOUBLE", ' ', type.length, type.body );
	       break;
            }

         case DAT__R:
            {
	       _chcopy( 5, "_REAL", ' ', type.length, type.body );
	       break;
            }

         case DAT__I:
            {
	       _chcopy( 8, "_INTEGER", ' ', type.length, type.body );
	       break;
            }

         case DAT__W:
            {
	       _chcopy( 5, "_WORD", ' ', type.length, type.body );
	       break;
            }

         case DAT__UW:
            {
	       _chcopy( 6, "_UWORD", ' ', type.length, type.body );
	       break;
            }

         case DAT__B:
            {
	       _chcopy( 5, "_BYTE", ' ', type.length, type.body );
	       break;
            }

         case DAT__UB:
            {
	       _chcopy( 6, "_UBYTE", ' ', type.length, type.body );
	       break;
            }

         case DAT__L:
            {
	       _chcopy( 8, "_LOGICAL", ' ', type.length, type.body );
	       break;
            }

         case DAT__C:
            {
	       (void) sprintf( buf, "_CHAR*%d%n",
	                       (int) ( data->obj.length /
			               dat_gl_ndr[ DAT__C ].length ), &nc );
	       _chcopy( nc, buf, ' ', (int) type.length, type.body );
               break;
            }
      }
   }
   else
   {
      _chcopy( DAT__SZTYP, data->type, ' ', type.length, type.body );
   }

return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_shape)
                       (locator_str,maxdim,dims,actdim,status,locator_lenarg)

/*============================*/
/* DAT_SHAPE - Object shape ? */
/*============================*/

struct STR	 	 *locator_str;
int			 *maxdim;
int			 *dims;
int			 *actdim;
int			 *status;
int			  locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_SHAPE_ERR"
#define context_message\
        "DAT_SHAPE: Error enquiring the shape of an HDS object."

struct DSC		  locator;
int			  locator_len = locator_lenarg;

struct LCP		 *lcp;
struct LCP_DATA		 *data;
int			  axis[DAT__MXDIM];
int			  naxes;
int			  i;

/* Enter routine.	*/

if (!_ok(*status))
	return *status;
hds_gl_status = DAT__OK;

/* Import locator string and locator.	*/

_strimp(&locator,locator_str,&locator_len);
_call(dau_import_loc(&locator, &lcp))
data	      = &lcp->data;

/* Enquire the object shape.	*/

_call(dau_get_shape(data, &naxes, axis))
*actdim       = naxes;

/* Return the required # of dimension sizes.	*/

for (i=0; i<_min(*maxdim, *actdim); i++)
	dims[i] = axis[i];
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_size)
                       (locator_str,size,status,locator_lenarg)      

/*==========================*/
/* DAT_SIZE - Object size ? */
/*==========================*/

struct STR		*locator_str;
int			*size;
int			*status;
int			 locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_SIZE_ERR"
#define context_message\
        "DAT_SIZE: Error enquiring the size of an HDS object."

struct DSC		 locator;
int			 locator_len = locator_lenarg;

struct LCP		*lcp;
struct LCP_DATA		*data;

/* Enter routine.	*/

if (!_ok(*status))
	return *status;
hds_gl_status = DAT__OK;

/* Import locator string and locator.	*/

_strimp(&locator,locator_str,&locator_len);
_call(dau_import_loc(&locator, &lcp))
data	      = &lcp->data;

/* Return the object size.	*/

*size         = data->size;
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_there)
                       (locator_str,name_str,there,status,locator_lenarg,name_lenarg)

/*============================*/
/* DAT_THERE - Object there ? */
/*============================*/

struct STR	 	 *locator_str;
struct STR	 	 *name_str;
int			 *there;
int			 *status;
int	  		  locator_lenarg;
int	  		  name_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_THERE_ERR"
#define context_message\
        "DAT_THERE: Error enquiring about the existence of an HDS object."

struct DSC		  locator;
struct DSC		  name;
int			  locator_len = locator_lenarg;
int			  name_len = name_lenarg;

struct LCP	    	 *lcp;
struct LCP_DATA		 *data;
char			  nambuf[DAT__SZNAM];
unsigned char *srv;
unsigned char *crv;
struct RCL		  rcl;
struct RID		  rid;
struct HAN		  han;
int			  off;
int			  ncomp;
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
   object.	*/

if ( (!data->struc) || (data->naxes != 0) )
	_call(DAT__OBJIN)

/* Validate the component name.	*/

_call(dau_check_name(&name, nambuf))

/* Locate the Structure Record Vector entry and extract the ID of the component
   record. If the ID is null, then no component record exists.	*/

off           = data->offset * DAT__SZSRV;
_call(rec_locate_data(&data->han, DAT__SZSRV, off, 'R', &srv))
dat1_unpack_srv( srv, &rid );
rec_release_data(&data->han, DAT__SZSRV, off, 'R', &srv);
if ( ( rid.bloc == 0 ) && ( rid.chip == 0 ) )
	{
	*there = F77_FALSE;
	return hds_gl_status;
	}

/* Otherwise, stick a handle on the component record, get the Record Control
   Label and read the component count.	*/

_call(rec_get_handle(&rid, &data->han, &han))
_call(rec_get_rcl(&han, &rcl))
_call(dat1_get_ncomp(&han, &ncomp))

/* Locate the Component Record Vector and search for the specified name. */

_call(rec_locate_data(&han, rcl.dlen, 0, 'R', &crv))
rid           = rec_gl_ridzero;
for ( i = 0; i < ncomp; i++ )
{
   dat1_locate_name( crv, i, &name1 );
   if ( _cheql( DAT__SZNAM, nambuf, name1 ) )
   {
      dat1_unpack_crv( crv, i, &rid );
      break;
   }
}
rec_release_data(&han, rcl.dlen, 0, 'R', &crv);

/* If the Record-ID is not null, then the component exists.	*/

*there = ( ( rid.bloc != 0) || ( rid.chip != 0 ) ) ? F77_TRUE : F77_FALSE;
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_struc)
                       (locator_str,struc,status,locator_lenarg)

/*================================*/
/* DAT_STRUC - Structure object ? */
/*================================*/

struct STR		*locator_str;
int			*struc;
int			*status;
int			 locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_STRUC_ERR"
#define context_message\
        "DAT_STRUC: Error enquiring if an HDS object is a structure."

struct DSC		 locator;
int			 locator_len = locator_lenarg;

struct LCP	    	*lcp;
struct LCP_DATA		*data;

/* Enter routine.	*/

if (!_ok(*status))
	return *status;
hds_gl_status = DAT__OK;

/* Import locator string and locator.	*/

_strimp(&locator,locator_str,&locator_len);
_call(dau_import_loc(&locator, &lcp))
data	      = &lcp->data;

/* Set the flag appropriately and return.	*/

*struc = data->struc ? F77_TRUE : F77_FALSE;
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_prim)
                       (locator_str,prim,status,locator_lenarg)

/*===============================*/
/* DAT_PRIM - Primitive object ? */
/*===============================*/

struct STR		*locator_str;
int			*prim;
int			*status;
int			 locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_PRIM_ERR"
#define context_message\
        "DAT_PRIM: Error enquiring if an HDS object is primitive."

struct DSC		 locator;
int			 locator_len = locator_lenarg;

struct LCP		*lcp;
struct LCP_DATA		*data;

/* Enter routine.	*/

if (!_ok(*status))
	return *status;
hds_gl_status = DAT__OK;

/* Import locator string and locator.	*/

_strimp(&locator,locator_str,&locator_len);
_call(dau_import_loc(&locator, &lcp))
data	      = &lcp->data;

/* Set the flag appropriately and return.	*/

*prim = data->struc ? F77_FALSE : F77_TRUE;
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_ncomp)
                       (locator_str,ncomp,status,locator_lenarg)

/*====================================*/
/* DAT_NCOMP - Number of components ? */
/*====================================*/

struct STR		*locator_str;
int			*ncomp;
int			*status;
int			 locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_NCOMP_ERR"
#define context_message\
        "DAT_NCOMP: Error enquiring the number of components in an HDS structure."

struct DSC		 locator;
int			 locator_len = locator_lenarg;

struct LCP		*lcp;
struct LCP_DATA		*data;
unsigned char *srv;
struct RID		 rid;
struct HAN		 han;
int			 off;

/* Enter routine.	*/

if (!_ok(*status))
	return *status;
hds_gl_status = DAT__OK;

/* Import locator string and locator.	*/

_strimp(&locator,locator_str,&locator_len);
_call(dau_import_loc(&locator, &lcp))
data	      = &lcp->data;

/* Return if the source locator points to anything other than a single
   structure object.	*/

if ( (!data->struc) || (data->naxes != 0) )
	_call(DAT__OBJIN)

/* Locate the Structure Record Vector entry and extract the ID of the component
   record. If the ID is null, then no component record exists.	*/

off           = data->offset * DAT__SZSRV;
_call(rec_locate_data(&data->han, DAT__SZSRV, off, 'R', &srv))
dat1_unpack_srv( srv, &rid );
rec_release_data(&data->han, DAT__SZSRV, off, 'R', &srv);
if ( ( rid.bloc == 0 ) && ( rid.chip == 0 ) )
	{
	*ncomp = 0;
	return hds_gl_status;
	}

/* Otherwise, stick a handle on the component record and read the contents of
   the record's static domain.	*/

_call(rec_get_handle(&rid, &data->han, &han))
_call(dat1_get_ncomp(&han, ncomp))
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_len)
                       (locator_str,len,status,locator_lenarg)

/*==============================*/
/* DAT_LEN - Primitive length ? */
/*==============================*/

struct STR		*locator_str;
int			*len;
int			*status;
int			 locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_LEN_ERR"
#define context_message\
        "DAT_LEN: Error enquiring the element length of an HDS primitive."

struct DSC		 locator;
int			 locator_len = locator_lenarg;

struct LCP	       	*lcp;
struct LCP_DATA		*data;
struct PDD		*obj;

/* Enter routine.	*/

if (!_ok(*status))
	return *status;
hds_gl_status = DAT__OK;

/* Import locator string and locator.	*/

_strimp(&locator,locator_str,&locator_len);
_call(dau_import_loc(&locator, &lcp))
data	      = &lcp->data;

/* Return if the object is a structure.	*/

if (data->struc)
	_call(DAT__OBJIN)

/* Otherwise, associate the object data attributes descriptor.	*/

obj           = &data->obj;

/* Return the primitive object length.	*/

*len          = obj->length;
return hds_gl_status;
}
   F77_INTEGER_FUNCTION(dat_state)
                       (locator_str,state,status,locator_lenarg)

/*============================*/
/* DAT_STATE - Object state ? */
/*============================*/

struct STR		*locator_str;
int			*state;
int			*status;
int			 locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_STATE_ERR"
#define context_message\
        "DAT_STATE: Error enquiring the state of an HDS primitive."

struct DSC		 locator;
int			 locator_len = locator_lenarg;

struct LCP	     	*lcp;
struct LCP_DATA		*data;
struct RCL		 rcl;

/* Enter routine.	*/

if (!_ok(*status))
	return *status;
hds_gl_status = DAT__OK;

/* Import locator string and locator.	*/

_strimp(&locator,locator_str,&locator_len);
_call(dau_import_loc(&locator, &lcp))
data	      = &lcp->data;

/* Return if the object is a structure.	*/

if (data->struc)
	_call(DAT__OBJIN)

/* Read the record's control label and determine if the active flag
   is currently set.	*/

_call(rec_get_rcl(&data->han, &rcl))
*state = rcl.active ? F77_TRUE : F77_FALSE;
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(dat_valid)
                       (locator_str,valid,status,locator_lenarg)

/*=============================*/
/* DAT_VALID - Valid locator ? */
/*=============================*/

   struct STR		*locator_str;
   int			*valid;
   int			*status;
   int			 locator_lenarg;

   {
      struct DSC		 locator;
      int			 locator_len = locator_lenarg;
      struct LCP		*lcp;

/*
   Enter routine.
*/
      if ( !_ok( *status ) ) return *status;
      hds_gl_status = DAT__OK;
/*
   Import the locator string.
*/
      _strimp( &locator, locator_str, &locator_len );
/*
   Defer error reporting. Import the locator and set the flag
   appropriately.
*/
      ems_mark_c( );
      *valid = _ok( dau_import_loc( &locator, &lcp ) ) ? F77_TRUE : F77_FALSE;
/*
   Annul any errors and end the error context.
*/
      ems_annul_c( &hds_gl_status );
      ems_rlse_c( );

/*
   Return the status value.
*/
      *status = hds_gl_status;
      return *status;
   }

   F77_INTEGER_FUNCTION(dat_conv)
                       (locator_str,type_str,conv,status,locator_lenarg,type_lenarg)

/*=======================================*/
/* DAT_CONV - Data conversion possible ? */
/*=======================================*/

struct STR		*locator_str;
struct STR	      	*type_str;
int   			*conv;
int			*status;
int	  		 locator_lenarg;
int	  		 type_lenarg;

{
#undef context_name
#undef context_message
#define context_name "DAT_CONV_ERR"
#define context_message\
        "DAT_CONV: Error determining whether type conversion is possible."

struct DSC		 locator;
struct DSC		 type;
int			 locator_len = locator_lenarg;
int			 type_len = type_lenarg;

struct LCP	     	*lcp;
struct LCP_DATA		*data;
char			 typbuf[DAT__SZTYP];
struct PDD	 	 app;

/* Enter routine.	*/

if (!_ok(*status))
	return *status;
hds_gl_status = DAT__OK;

/* Import locator and type strings.	*/

_strimp(&locator,locator_str,&locator_len);
_strimp(&type,type_str,&type_len);

/* Import locator.	*/

_call(dau_import_loc(&locator, &lcp))
data	      = &lcp->data;

/* Ensure that the locator points to a primitive and not a structure.	*/

if (data->struc)
	_call(DAT__OBJIN)

/* Validate the type specification and determine the attributes of the
   application data.	*/

_call(dat1_check_type(&type, typbuf))
_call(dat1_unpack_type(typbuf, &app))

/* Ensure that the application data is primitive and set the flag
   appropriately if the types 'match'.	*/

if (app.class != DAT__PRIMITIVE)
	_call(DAT__TYPIN)
*conv = _ok( dau_match_types( &data->obj, &app ) ) ? F77_TRUE : F77_FALSE;
return hds_gl_status;
}
