#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
/*+DAUCHECK.C-*/

/* Include files */

#include <ctype.h>	/* Character classification			*/

#include "ems.h"		 /* EMS error reporting routines	    */

#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "str.h"		 /* Character string import/export macros   */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

/* Control Blocks */


dau_check_shape(ndim,dims,odl)

/*+
 * DAU_CHECK_SHAPE - Check shape information
 *
 * This routine will validate the 'shape' of a data object. An error status
 * will be returned if the # of dimensions is not within the range zero ==>
 * DAT_K_MAXNDIM (zero indicates scalar object), or if any of the dimension
 * sizes is negative or zero.
 *
 * Calling sequence:
 *
 * 	  DAU_CHECK_SHAPE(NDIM,DIMS,ODL)
 *
 * NDIM	  is the number of dimensions in the object.
 * DIMS   is the address of a longword vector whose elements contain the
 * 	  size of each dimension.
 * ODL	  is the address of an Object Descriptor Label which is to receive
 *	  the shape information if valid.
 *
 * Routine value:
 *
 * 	  DAT__OK    if successful.
 * 	  DAT__DIMIN if the dimension information is invalid.
 */

int		  ndim;
int		 *dims;
struct ODL	 *odl;

{
int		  n;

/* Check the # of axes and each axis size.	*/

if ( ( ndim < 0 ) || ( ndim > DAT__MXDIM ) ) return hds_gl_status = DAT__DIMIN;
for (n=0; n<ndim; n++)
	if (dims[n] < 1) return hds_gl_status = DAT__DIMIN;

/* Save the information in the Object Descriptor Label.	*/

odl->naxes = ndim;
for ( n = 0; n < ndim; n++ )
{
   odl->axis[ n ] = dims[ n ];
}
return hds_gl_status;
}

dau_check_name(name,buf)

/*+
 * DAU_CHECK_NAME - Check name
 *
 * This routine validates the syntax of a 'name' specification. If successful,
 * the contents are formatted - any embedded blanks are removed and all lower-
 * case letters are converted to uppercase.
 *
 * Calling sequence:
 *
 * 	  DAU_CHECK_NAME(NAME,BUF)
 *
 * NAME	  is the address of a character string descriptor which points to the
 *	  object name.
 * BUF	  is the address of a buffer which is to receive the 'formatted' name
 *	  string.
 *
 * Routine value:
 *
 * 	  DAT__OK    if successful.
 * 	  DAT__NAMIN if the string does not conform to the syntax of an object
 * 		     name.
 */

struct DSC	 	 *name;
char			 *buf;

{
char *txt = (char *) name->body;
short			  len = name->length;
int			  n   = 0;
int			  i;

/* First clear the destination buffer and then scan through the string,
   continually checking the syntax and converting lower case characters
   to uppercase.	*/

(void) memset( (void *) buf, (int) ' ', (size_t) DAT__SZNAM );
for (i=0; i<len; i++)
	if (isspace(txt[i]))
		;
/*
   Report an error if the maximum name length is exceeded.
*/
	else if (n >= DAT__SZNAM)
        {
           hds_gl_status = DAT__NAMIN;
           ems_setc_c( "NAME", txt, (int) len );
           ems_seti_c( "SZNAM", DAT__SZNAM );
           ems_rep_c( "DAU_CHECK_NAME_1",
                      "Invalid name string \'^NAME\' specified; more than \
^SZNAM characters long (possible programming error).",
                      &hds_gl_status );
           return hds_gl_status;
        }
/*
   Report an error if a non-printable character is encountered.
*/
        else if (!isprint(txt[i]))
        {
           hds_gl_status = DAT__NAMIN;
           ems_setc_c( "NAME", txt, (int) len );
           ems_seti_c( "CODE", (int) txt[ i ] );
           ems_seti_c( "POSN", i + 1 );
           ems_rep_c( "DAU_CHECK_NAME_2",
                      "Invalid name string \'^NAME\' specified; contains \
illegal character (code=^CODE decimal) at position ^POSN (possible \
programming error).",
                      &hds_gl_status );
           return hds_gl_status;
        }
        else
        {
           buf[n++] = toupper(txt[i]);
        }
/*
   Report an error if the name string is empty.
*/
if (n == 0)
{
   hds_gl_status = DAT__NAMIN;
   ems_rep_c( "DAU_CHECK_NAME_3",
              "Invalid blank name string specified (possible programming \
error).",
              &hds_gl_status );
   return hds_gl_status;
}
return hds_gl_status;
}
