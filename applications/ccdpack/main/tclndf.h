/*
*+
*  Name:
*     tclndf.h

*  Type of Module:
*     C header file.

*  Purpose:
*     Hold declarations for C extensions to Tcl which manipulate ndf objects.

*  Description:
*     This file holds header information required by C language extensions
*     to Tcl which manipulate ndf objects, including the code which 
*     implements ndf objects itself.

*  Authors:
*     MBT: Mark Taylor (STARLINK)

*  History:
*     5-SEP-2000 (MBT):
*        Initial version.
*-
*/


#include "ndf.h"
#include "dat_par.h"
#include "sae_par.h"
#include "tcl.h"
#include "mers.h"

/* General purpose buffer length.
*/
#define TCLNDF_BUFLENG 4096


/* Definition of the structure which holds NDF information. 
*/
   typedef struct {
      char *name;               /* Name of the NDF as supplied to constructor */
      int identifier;           /* NDF identifier */
      int ndim;                 /* Number of dimensions of the NDF */
      int lbnd[ NDF__MXDIM ];   /* Lower pixel-index bounds of NDF */
      int ubnd[ NDF__MXDIM ];   /* Upper pixel-index bounds of NDF */
      int nel;                  /* Number of elements in the NDF */
      int mapped;               /* Is the data component currently mapped? */
      int bad;                  /* Whether data array may contain bad values */
      AstFrameSet *wcs;         /* Pointer to the WCS component of the NDF */
      void *data;               /* Pointer to the mapped array component */
      struct {
         int *data;                /* Array of integers corresponding to data */
         double loval;             /* Lower scale value plotarray uses */
         double hival;             /* Upper scale value plotarray uses */
         double zoom;              /* Size scaling factor of array */
         int xdim;                 /* Number of elements in first dimension */
         int ydim;                 /* Number of elements in second dimension */
         int iframe;               /* Index in WCS frameset of plot frame */
         int exists;               /* Has the array been written? */
      } plotarray;              /* PGPLOT-friendly array of integers */
      struct {
         char *data;               /* The text of the FITS headers */
         int loaded;               /* Has the FITS array been loaded? */
         int ncard;                /* Number of FITS cards present */
         char loc[ DAT__SZLOC ];   /* Locator to NDF .MORE.FITS extension */
      } fits;                   /* Contents of FITS extension. */
      Tcl_HashTable perchash;   /* Percentiles hash table */
      char ntype[ DAT__SZTYP ]; /* HDS type of NDF DATA component */
      char mtype[ DAT__SZTYP ]; /* HDS type of mapped data array */
   } Ndf;


/* Macro for calling a routine with a Starlink-like STATUS argument.
   The code which forms the macro argument is executed, and if the
   contents of a lexically scoped variable called status is set,
   the macro will bail out with a TCL error, and the text of the 
   Starlink error will be returned in the result string. 
   This macro should be used only from Tcl-type routines which are 
   expected to return a TCL integer error code and leave a result in
   the result of a Tcl_Interp called interp.  It should be used to
   call all and only routines which use a Starlink-like status 
   argument.
*/
#define STARCALL(code) \
   errMark(); \
   { \
      int status_val = SAI__OK; \
      int *status = &status_val; \
      code \
      if ( *status != SAI__OK ) { \
         char errmsg[ TCLNDF_BUFLENG ]; \
         char errname[ ERR__SZPAR ]; \
         int errmsg_leng; \
         int errname_leng; \
         Tcl_Obj *result; \
         result = Tcl_NewStringObj( "", 0 ); \
         while ( *status != SAI__OK ) { \
            errLoad( errname, ERR__SZPAR, &errname_leng, errmsg, \
                     TCLNDF_BUFLENG, &errmsg_leng, status ); \
            Tcl_AppendStringsToObj( result, "\n", errmsg, (char *) NULL ); \
         } \
         Tcl_SetObjResult( interp, result ); \
         errRlse(); \
         return TCL_ERROR; \
      } \
      else { \
         errRlse(); \
      } \
   }


/* Macro for calling AST routines.  These effectively behave like normal
   Starlink-type functions, but the handling of the status argument is
   a bit different (see SUN/211)
*/
#define ASTCALL(code) \
   errMark(); \
   { \
      int status_val = SAI__OK; \
      int *status = &status_val; \
      int *old_status = astWatch( status ); \
      code \
      astWatch( old_status ); \
      if ( *status != SAI__OK ) { \
         char errmsg[ TCLNDF_BUFLENG ]; \
         char errname[ ERR__SZPAR ]; \
         int errmsg_leng; \
         int errname_leng; \
         Tcl_Obj *result; \
         result = Tcl_NewStringObj( "", 0 ); \
         while ( *status != SAI__OK ) { \
            errLoad( errname, ERR__SZPAR, &errname_leng, errmsg, \
                     TCLNDF_BUFLENG, &errmsg_leng, status ); \
            Tcl_AppendStringsToObj( result, "\n", errmsg, (char *) NULL ); \
         } \
         Tcl_SetObjResult( interp, result ); \
         errRlse(); \
         return TCL_ERROR; \
      } \
      else { \
         errRlse(); \
      } \
   }


/* Prototypes for Ndf object functions. */
   Tcl_ObjCmdProc NdfCmd;
   Tcl_ObjCmdProc ObjectNdfCmd;
   Tcl_CmdDeleteProc DeleteNdf;


/* Prototype for ndf extension initialisation command.  This should 
   initialise the "ndf" object creation command, and also the additional
   Tcl commands listed above. */
   int Ndf_Init( Tcl_Interp *interp );


/* Prototypes for various auxiliary functions. */
   int tclmemok( Tcl_Interp *interp, void *ptr );
   int dcompare( const void *a, const void *b );
   void domapdata( Ndf *ndf, int *status );
   void dounmapdata( Ndf *ndf, int *status );
   int *getpixbloc( Ndf *ndf, int iframe, double zoom, double loval, 
                    double hival, int locolour, int hicolour, int badcolour,
                    int *status );
   void getbbox( Ndf *ndf, int iframe, double *lbox, double *ubox, 
                 int *status );
   double getpixelsize( Ndf *ndf, int iframe, int *status );
   int NdfGetIframeFromObj( Tcl_Interp *interp, Tcl_Obj *obj, 
                            AstFrameSet *fset, int *iframe );
   int NdfGetNdfFromObj( Tcl_Interp *interp, Tcl_Obj *obj, Ndf **ndf );


/* Utility macros. 
*/
#define max(a,b) (((a) > (b)) ? (a) : (b))
#define min(a,b) (((a) < (b)) ? (a) : (b))


