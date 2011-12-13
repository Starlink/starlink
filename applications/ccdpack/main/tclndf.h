/*
*+
*  Name:
*     tclndf.h

*  Purpose:
*     Hold declarations for C extensions to Tcl which manipulate NDFs.

*  Language:
*     ANSI C

*  Type of Module:
*     C header file.

*  Description:
*     This file holds header information required by C language extensions
*     to Tcl which manipulate ndf and ndfset objects, including the
*     code which implements the objects itself.

*  Copyright:
*     Copyright (C) 2000-2001 Central Laboratory of the Research
*     Councils. Copyright (C) 2005 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-SEP-2000 (MBT):
*        Initial version.
*     9-MAR-2001 (MBT):
*        Upgraded for use with Sets.
*     18-NOV-2005 (TIMJ):
*        Use HDSLoc
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#ifndef CCD_TCLNDF_DEFINED
#define CCD_TCLNDF_DEFINED

#include "ndf.h"
#include "dat_par.h"
#include "star/hds_types.h"
#include "ast.h"
#include "tcl.h"


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
         size_t ncard;             /* Number of FITS cards present */
         HDSLoc *loc;              /* Locator to NDF .MORE.FITS extension */
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

#endif  /* CCD_TCLNDF_DEFINED */

/* $Id$ */
