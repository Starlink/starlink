#if !defined( LOADER_INCLUDED )  /* Include this file only once */
#define LOADER_INCLUDED
/*
*+

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     18-NOV-1997 (RFWS):
*        Original version.
*-
*/

#include "object.h"
#include "channel.h"

#if defined(astCLASS)            /* Protected */

typedef AstObject *(AstLoaderType)( void *, size_t, int, AstObjectVtab *,
                                    const char *, AstChannel * );

AstLoaderType *astGetLoader( const char * );

#endif
#endif
