#define astCLASS
#include "axis.h"
#include "channel.h"
#include "cmpframe.h"
#include "cmpmap.h"
#include "dssmap.h"
#include "fitschan.h"
#include "frame.h"
#include "frameset.h"
#include "intramap.h"
#include "loader.h"
#include "lutmap.h"
#include "mapping.h"
#include "matrixmap.h"
#include "object.h"
#include "permmap.h"
#include "plot.h"
#include "pointset.h"
#include "skyaxis.h"
#include "skyframe.h"
#include "slamap.h"
#include "sphmap.h"
#include "unitmap.h"
#include "wcsmap.h"
#include "winmap.h"
#include "zoommap.h"

#include "error.h"
#include "ast_err.h"
#include <stddef.h>
#include <string.h>

/*
*+
*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     18-NOV-1997 (RFWS):
*        Original version.
*     18-MAR-1998 (RFWS):
*        Added the IntraMap class.
*-
*/

AstLoaderType *astGetLoader( const char *class ) {
   if ( !astOK ) return NULL;

#define LOAD(name) \
if ( !strcmp( class, #name ) ) return (AstLoaderType *) astLoad##name##_

   LOAD(Axis);
   LOAD(Channel);
   LOAD(CmpFrame);
   LOAD(CmpMap);
   LOAD(DssMap);
   LOAD(FitsChan);
   LOAD(Frame);
   LOAD(FrameSet);
   LOAD(IntraMap);
   LOAD(LutMap);
   LOAD(Mapping);
   LOAD(MatrixMap);
   LOAD(Object);
   LOAD(PermMap);
   LOAD(Plot);
   LOAD(PointSet);
   LOAD(SkyAxis);
   LOAD(SkyFrame);
   LOAD(SlaMap);
   LOAD(SphMap);
   LOAD(UnitMap);
   LOAD(WcsMap);
   LOAD(WinMap);
   LOAD(ZoomMap);

   astError( AST__OCLUK, "astGetLoader: Object of unknown class \"%s\" cannot "
                         "be loaded.", class );
   return NULL;
#undef LOAD
}
