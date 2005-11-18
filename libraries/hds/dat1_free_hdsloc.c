#if HAVE_CONFIG_H
# include <config.h>
#endif

#include "hds1.h"
#include "rec.h"
#include "dat1.h"

/* Small routine to free a LOC structure (aka HDSLoc) previously
   allocated using dat1_alloc_lcp.

   C equivalent of copying DAT__NOLOC into a DAT__SZLOC character
   buffer.

*/

void dat1_free_hdsloc ( struct LOC ** loc ) {

  if ( *loc != NULL ) {
    free( *loc );
    *loc = NULL;
  }
}
