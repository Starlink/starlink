/*
*+
*  Name:
*     tclndf.h

*  Type of Module:
*     C header file.

*  Purpose:
*     Hold declarations for C extensions to Tcl which manipulate NDFs.

*  Description:
*     This file holds header information required by C language extensions
*     to Tcl which manipulate ndf and ndfset objects, including the 
*     code which implements the objects itself.

*  Authors:
*     MBT: Mark Taylor (STARLINK)

*  History:
*     5-SEP-2000 (MBT):
*        Initial version.
*     9-MAR-2001 (MBT):
*        Upgraded for use with Sets.
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


/* Definition of the structure which holds basic information about
*  a single NDF.
*/
   typedef struct {
      int identifier;           /* NDF identifier */
      int ndim;                 /* Number of dimensions of the NDF */
      int lbnd[ NDF__MXDIM ];   /* Lower pixel-index bounds of NDF */
      int ubnd[ NDF__MXDIM ];   /* Upper pixel-index bounds of NDF */
      int nel;                  /* Number of elements in the NDF */
      int mapped;               /* Is the data component currently mapped? */
      int bad;                  /* Whether data array may contain bad values */
      void *data;               /* Pointer to the mapped array component */
      char ntype[ DAT__SZTYP ]; /* HDS type of NDF DATA component */
      char mtype[ DAT__SZTYP ]; /* HDS type of mapped data array */
      Tcl_HashTable perchash;   /* Percentiles hash table */
      struct {
         char *data;               /* The text of the FITS headers */
         int loaded;               /* Has the FITS array been loaded? */
         int ncard;                /* Number of FITS cards present */
         char loc[ DAT__SZLOC ];   /* Locator to NDF .MORE.FITS extension */
      } fits;                   /* Contents of FITS extension. */
   } Ndf1;


/* Definition of the structure which holds a (PGPLOT-friendly) plottable
*  array of pixels and related information.
*/
   typedef struct {
      int *data;                /* Array of integers corresponding to data */
      double loperc;            /* Lower percentile value plotarray uses */
      double hiperc;            /* Upper percentile value plotarray uses */
      double zoom;              /* Size scaling factor of array */
      int xdim;                 /* Number of elements in first dimension */
      int ydim;                 /* Number of elements in second dimension */
      int iframe;               /* Index in WCS frameset of plot frame */
      int exists;               /* Has the array been written? */
   } Plotarray;


/* Definition of the structure which holds information about an ndfset
   object or an ndf object.  If it represents an ndfset, then nmember 
   must be greater than zero, and the first nmember elements of 
   the content.ndfs[] array must point to Ndfset structures representing
   ndf objects.  If it represents and ndf object, then nmember must be
   zero, and content.ndf1 must point to an Ndf1 structure.  It is not
   allowed for an ndfset to contain an ndfset.
*/
   typedef struct NdfsetStruct NdfOrNdfset;
   typedef NdfOrNdfset Ndf;
   typedef NdfOrNdfset Ndfset;
   struct NdfsetStruct {
      char *name;               /* Name of the NDF Set or NDF */
      int nmember;              /* Number of member NDFs (0 for ndf object) */
      union {
         Ndf **ndfs;               /* Member ndf objects (if nmember > 0) */
         Ndf1 *ndf1;               /* Ndf1 data structure (if nmember = 0) */
      } content;                /* Content of the structure */
      AstFrameSet *wcs;         /* Pointer to a WCS frameset */
      Plotarray *plotarray;     /* Pointer to a plottable array */
   };


/* Macro for calling a routine with a Starlink-like STATUS argument.
   The code which forms the macro argument is executed, and if the
   contents of a lexically scoped variable called status is set,
   the macro will bail out with a TCL error, and the text of the 
   Starlink error will be returned in the result string. 
   This macro should be used only from Tcl-type routines which are 
   expected to return a TCL integer error code and leave a result in
   the result of a Tcl_Interp called interp.  It should be used to
   call all routines which use a Starlink-like status argument.

   This macro and the similar ASTCALL macro below may be used to invoke
   large sections of code; however note that multiple-line comments
   within a macro argument will cause cpp(1) to get confused about
   which line is which, so this may cause difficulty when running
   under the debugger.
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


/* Prototypes for ndf and ndfset object functions. */
   Tcl_ObjCmdProc NdfCmd;
   Tcl_ObjCmdProc ObjectNdfCmd;
   Tcl_CmdDeleteProc DeleteNdf;
   Tcl_ObjCmdProc NdfsetCmd;
   Tcl_ObjCmdProc ObjectNdfsetCmd;
   Tcl_CmdDeleteProc DeleteNdfset;


/* Prototype for ndf extension initialisation command.  This should 
   initialise the "ndf" and "ndfset" object creation commands, and also
   the additional Tcl commands listed above. */
   int Ndf_Init( Tcl_Interp *interp );


/* Prototypes for various auxiliary functions. */
   int newNdf( Tcl_Interp *interp, Tcl_Obj *ndfnameobj, Ndf **ndf );
   void forgetNdf( Ndf *ndf );
   void getpercentiles( Ndf1 *ndf1, int nperc, double *percs, double *values,
                        int *status );
   int tclmemok( Tcl_Interp *interp, void *ptr );
   int dcompare( const void *a, const void *b );
   void domapdata( Ndf1 *ndf1, int *status );
   void dounmapdata( Ndf1 *ndf1, int *status );
   double getpixelsize( NdfOrNdfset *ndfset, int iframe, int *status );
   void getbbox( NdfOrNdfset *ndfset, int iframes[], double *lbox, 
                 double *ubox, int *status );
   Tcl_ObjCmdProc ndfdisplay;
   int *getpixbloc( NdfOrNdfset *ndfset, int iframes[], double zoom, 
                    double loperc, double hiperc, int locolour, int hicolour,
                    int badcolour, int *status );
   int NdfGetNdfFromObj( Tcl_Interp *interp, Tcl_Obj *obj,
                         NdfOrNdfset **ndfset );
   int NdfGetIframeFromObj( Tcl_Interp *interp, Tcl_Obj *obj, 
                            AstFrameSet *fset, int *iframe );
   int NdfGetIframesFromObj( Tcl_Interp *interp, Tcl_Obj *obj,
                             NdfOrNdfset *ndfset, int *iframes );


/* Declarations for commands coping with handling events in the background. */
   Tcl_ObjCmdProc tclbgcmd;
   void tclupdate( void );


/* Utility macros. 
*/
#define max(a,b) (((a) > (b)) ? (a) : (b))
#define min(a,b) (((a) < (b)) ? (a) : (b))


/* Macros for classifying and identifying HDS types.  Note we use the
   Fortran/HDS-like names here, but the magic bad values defined in
   img.h are defined using the C-like names (W->S, UW->US and R->F).
*/
#define CCD_TYPE_NONE 0
#define CCD_TYPE_B 1
#define CCD_TYPE_UB 2
#define CCD_TYPE_W 3
#define CCD_TYPE_UW 4
#define CCD_TYPE_I 5
#define CCD_TYPE_R 6
#define CCD_TYPE_D 7

#define CCD_TYPE(t) ( !strcmp( t, "_BYTE" ) ? CCD_TYPE_B : \
                    ( !strcmp( t, "_UBYTE" ) ? CCD_TYPE_UB : \
                    ( !strcmp( t, "_WORD" ) ? CCD_TYPE_W : \
                    ( !strcmp( t, "_UWORD" ) ? CCD_TYPE_UW : \
                    ( !strcmp( t, "_INTEGER" ) ? CCD_TYPE_I : \
                    ( !strcmp( t, "_REAL" ) ? CCD_TYPE_R : \
                    ( !strcmp( t, "_DOUBLE" ) ? CCD_TYPE_D : \
                    ( CCD_TYPE_NONE ) ) ) ) ) ) ) )

/* $Id$ */
