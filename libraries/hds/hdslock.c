#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
/*+HDSLOCK.C-*/

/* Include files */

#include "f77.h"		 /* F77 <-> C interface macros		    */
#include "ems.h"		 /* EMS error reporting routines	    */

#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "str.h"		 /* Character string import/export macros   */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

/* Control Blocks */


   F77_INTEGER_FUNCTION(hds_lock)
                       (locator_str,status,locator_lenarg)

/*================================*/
/* HDS_LOCK - Lock container file */
/*================================*/

struct STR		*locator_str;
int			*status;
int			 locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "HDS_LOCK_ERR"
#define context_message\
        "HDS_LOCK: Error locking an HDS container file."

struct DSC		 locator;
int			 locator_len = locator_lenarg;

struct LCP		*lcp;
struct LCP_DATA		*data;

/* Enter routine.	*/

if(!_ok(*status))
	return *status;
hds_gl_status = DAT__OK;

/* Import the locator string and locator.	*/

_strimp(&locator,locator_str,&locator_len);
_call(dau_import_loc(&locator, &lcp))
data	      = &lcp->data;

/* Return if the locator is not associated with a top-level object.	*/

if (data->level != 0)
	_call(DAT__OBJIN)

/* Lock the container file for exclusive write-access.	*/

_call(rec_lock(&data->han))
return hds_gl_status;
}

   F77_INTEGER_FUNCTION(hds_free)
                       (locator_str,status,locator_lenarg)

/*================================*/
/* HDS_FREE - Free container file */
/*================================*/

struct STR		*locator_str;
int	  		*status;
int			 locator_lenarg;

{
#undef context_name
#undef context_message
#define context_name "HDS_FREE_ERR"
#define context_message\
        "HDS_FREE: Error releasing locks on an HDS container file."

struct DSC		 locator;
int			 locator_len = locator_lenarg;

struct LCP		*lcp;
struct LCP_DATA		*data;

/* Enter routine.	 */

if(!_ok(*status))
	return *status;
hds_gl_status = DAT__OK;

/* Import the locator string and locator.	*/

_strimp(&locator,locator_str,&locator_len);
_call(dau_import_loc(&locator, &lcp))
data	      = &lcp->data;

/* Return if the locator is not associated with a top-level object.	*/

if (data->level != 0)
	_call(DAT__OBJIN)

/* Unlock the container file 	*/

_call(rec_unlock(&data->han))
return hds_gl_status;
}
