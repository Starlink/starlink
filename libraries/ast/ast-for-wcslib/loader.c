/* A dummy version of loader.c for use in the WCSLIB sub-set of AST */
#define astCLASS
#include "channel.h"
#include "loader.h"
#include "mapping.h"
#include "mathmap.h"
#include "object.h"
#include "pointset.h"
#include "unitmap.h"
#include "zoommap.h"

#include "error.h"
#include "ast_err.h"
#include <stddef.h>
#include <string.h>

AstLoaderType *astGetLoader( const char *class ) {
   if ( !astOK ) return NULL;

#define LOAD(name) \
if ( !strcmp( class, #name ) ) return (AstLoaderType *) astLoad##name##_

   LOAD(Channel);
   LOAD(Mapping);
   LOAD(MathMap);
   LOAD(Object);
   LOAD(PointSet);
   LOAD(UnitMap);
   LOAD(ZoomMap);

   astError( AST__OCLUK, "astGetLoader: Object of unknown class \"%s\" cannot "
                         "be loaded.", class );
   return NULL;
#undef LOAD
}
