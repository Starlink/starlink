#if !defined( GLOBALS_INCLUDED )   /* Include this file only once */
#define GLOBALS_INCLUDED 1

/* If thread-safety is required... */
#if defined( THREAD_SAFE ) && ( defined( astCLASS ) || defined( astFORTRAN77) )

/* Include files: */
/* ============== */

/* AST includes */
#include "axis.h"
#include "box.h"
#include "channel.h"
#include "circle.h"
#include "cmpframe.h"
#include "cmpmap.h"
#include "cmpregion.h"
#include "dsbspecframe.h"
#include "dssmap.h"
#include "ellipse.h"
#include "error.h"
#include "fitschan.h"
#include "fitstable.h"
#include "fluxframe.h"
#include "frame.h"
#include "frameset.h"
#include "grismmap.h"
#include "interval.h"
#include "intramap.h"
#include "keymap.h"
#include "lutmap.h"
#include "mapping.h"
#include "mathmap.h"
#include "matrixmap.h"
#include "memory.h"
#include "normmap.h"
#include "nullregion.h"
#include "object.h"
#include "pcdmap.h"
#include "permmap.h"
#include "plot.h"
#include "plot3d.h"
#include "pointlist.h"
#include "pointset.h"
#include "polygon.h"
#include "polymap.h"
#include "prism.h"
#include "ratemap.h"
#include "region.h"
#include "selectormap.h"
#include "shiftmap.h"
#include "skyaxis.h"
#include "skyframe.h"
#include "slamap.h"
#include "specfluxframe.h"
#include "specframe.h"
#include "specmap.h"
#include "sphmap.h"
#include "stc.h"
#include "stccatalogentrylocation.h"
#include "stcobsdatalocation.h"
#include "stcresourceprofile.h"
#include "stcsearchlocation.h"
#include "stcschan.h"
#include "switchmap.h"
#include "table.h"
#include "timeframe.h"
#include "timemap.h"
#include "tranmap.h"
#include "unitmap.h"
#include "wcsmap.h"
#include "winmap.h"
#include "xml.h"
#include "xmlchan.h"
#include "zoommap.h"



/* System includes */
#include <pthread.h>

/* Macros */
/* ====== */

/* The name of the variable used to access thread-specific global data */
#define AST__GLOBALS ast_globals

/* Defines a macro that gives access to a specific global data item. */
#define astGLOBAL(class,name) (AST__GLOBALS->class.name)


/* Declares the pointer for the structure holding thread-specific values
   for AST global data. */
#define astDECLARE_GLOBALS AstGlobals *AST__GLOBALS;


/* A macro that should be invoked in each function that refers to a
   global data item. The "This" parameter should be a pointer to an
   Object, or NULL. It ensures the thread-specific data key has been
   created. It also allocates and initialises memory to hold the global
   data. */
#define astGET_GLOBALS(This) \
\
/* If the supplied Object pointer contains a pointer to the thread-specific \
   data structure, return it. */ \
   if( This && ((AstObject *)This)->globals ) { \
      AST__GLOBALS = ((AstObject *)This)->globals; \
\
/* Otherwise, ensure that the thread specific data key has been created. */ \
   } else if( pthread_once( &starlink_ast_globals_initialised, \
                            astGlobalsCreateKey_ ) ) { \
      AST__GLOBALS = NULL; \
      fprintf( stderr, "Starlink AST package initialisation failed." ); \
\
/* If the current thread does not yet have a structure to hold \
   thread-specific global data, create one now (initialising its \
   contents) and associate it with the thread speciifc data key. */ \
   } else if( ( AST__GLOBALS =  \
                pthread_getspecific( starlink_ast_globals_key ) ) == NULL ) { \
      AST__GLOBALS = astGlobalsInit_(); \
      if( pthread_setspecific( starlink_ast_globals_key, AST__GLOBALS ) ) { \
         fprintf( stderr, "Starlink AST failed to store Thread-Specific " \
                  "Data pointer." ); \
      } \
   }


/* A macro that expands to the value of a unique integer identifier for
   the calling thread. */
#define AST__THREAD_ID (AST__GLOBALS->thread_identifier) \


#define astMAKE_INITGLOBALS(class) \
\
void astInit##class##Globals_( Ast##class##Globals *globals ){ \
   GLOBAL_inits \
}

/* Type definitions */
/* ================ */

typedef struct AstGlobals {
   int thread_identifier;
   AstMemoryGlobals Memory;
   AstErrorGlobals  Error;
   AstObjectGlobals Object;
   AstAxisGlobals Axis;
   AstMappingGlobals Mapping;
   AstFrameGlobals Frame;
   AstChannelGlobals Channel;
   AstCmpMapGlobals CmpMap;
   AstKeyMapGlobals KeyMap;
   AstFitsChanGlobals FitsChan;
   AstFitsTableGlobals FitsTable;
   AstCmpFrameGlobals CmpFrame;
   AstDSBSpecFrameGlobals DSBSpecFrame;
   AstFrameSetGlobals FrameSet;
   AstLutMapGlobals LutMap;
   AstMathMapGlobals MathMap;
   AstPcdMapGlobals PcdMap;
   AstPointSetGlobals PointSet;
   AstSkyAxisGlobals SkyAxis;
   AstSkyFrameGlobals SkyFrame;
   AstSlaMapGlobals SlaMap;
   AstSpecFrameGlobals SpecFrame;
   AstSphMapGlobals SphMap;
   AstTimeFrameGlobals TimeFrame;
   AstWcsMapGlobals WcsMap;
   AstZoomMapGlobals ZoomMap;
   AstFluxFrameGlobals FluxFrame;
   AstSpecFluxFrameGlobals SpecFluxFrame;
   AstGrismMapGlobals GrismMap;
   AstIntraMapGlobals IntraMap;
   AstPlotGlobals Plot;
   AstPlot3DGlobals Plot3D;
   AstRegionGlobals Region;
   AstBoxGlobals Box;
   AstXmlGlobals Xml;
   AstXmlChanGlobals XmlChan;
   AstCircleGlobals Circle;
   AstCmpRegionGlobals CmpRegion;
   AstDssMapGlobals DssMap;
   AstEllipseGlobals Ellipse;
   AstIntervalGlobals Interval;
   AstMatrixMapGlobals MatrixMap;
   AstNormMapGlobals NormMap;
   AstNullRegionGlobals NullRegion;
   AstPermMapGlobals PermMap;
   AstPointListGlobals PointList;
   AstPolyMapGlobals PolyMap;
   AstPolygonGlobals Polygon;
   AstPrismGlobals Prism;
   AstRateMapGlobals RateMap;
   AstSelectorMapGlobals SelectorMap;
   AstShiftMapGlobals ShiftMap;
   AstSpecMapGlobals SpecMap;
   AstStcGlobals Stc;
   AstStcCatalogEntryLocationGlobals StcCatalogEntryLocation;
   AstStcObsDataLocationGlobals StcObsDataLocation;
   AstSwitchMapGlobals SwitchMap;
   AstTableGlobals Table;
   AstTimeMapGlobals TimeMap;
   AstTranMapGlobals TranMap;
   AstUnitMapGlobals UnitMap;
   AstWinMapGlobals WinMap;
   AstStcResourceProfileGlobals	StcResourceProfile;
   AstStcSearchLocationGlobals StcSearchLocation;
   AstStcsChanGlobals StcsChan;
} AstGlobals;


/* Externally declared variables */
/* ============================= */


/* The pthreads key that is associated with the thread-specific data for
   each thread. Declared in global.c. */
extern pthread_key_t starlink_ast_globals_key;

/* The pthreads key that is associated with the thread-specific status
   value for each thread. Declared in global.c. */
extern pthread_key_t starlink_ast_status_key;

/* This is a flag indicating that the thread-specific data key has not yet
   been created. Declared in globals.c. */
extern pthread_once_t starlink_ast_globals_initialised;

/* Function Prototypes: */
/* ==================== */

void astGlobalsCreateKey_( void );
AstGlobals *astGlobalsInit_( void );


/* If thread-safety is not required, define some null macros. */
#else

#define astDECLARE_GLOBALS
#define astGET_GLOBALS(This)
#define astINIT_GLOBALS

#endif
#endif
