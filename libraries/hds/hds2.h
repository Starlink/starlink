#if !defined( HDS2_INCLUDED )    /* hds2.h already included?                */
#define HDS2_INCLUDED 1


/* Somewhere to put prototypes that have been (temporarily) removed
   from the public API */

#include "rec.h"
#include "dat1.h"

/*================================================*/
/* datWhere - Find primitive position in HDS file */
/*            Currently not part of the public    */
/*            C API because INT_BIG               */
/*            (aka hds_int64) is not a public     */
/*            type                                */
/*================================================*/

int
datWhere(HDSLoc *locator,
         INT_BIG *block,
         int *offset,
         int *status);


#endif /* HDS2_INCLUDED */
