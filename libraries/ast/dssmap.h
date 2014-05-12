#if !defined( DSSMAP_INCLUDED ) /* Include this file only once */
#define DSSMAP_INCLUDED
/*
*+
*  Name:
*     dssmap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the DssMap class.

*  Invocation:
*     #include "dssmap.h"

*  Description:
*     This include file defines the interface to the DssMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The DssMap class implements Mappings which use a Digitised Sky
*     Survey plate fit to transform between pixel coordinates and
*     Equatorial coordinates.

*  Inheritance:
*     The DssMap class inherits from the Mapping class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     None.

*  Methods Over-Ridden:
*     Public:
*        None.
*
*     Protected:
*        astTransform
*           Apply a DssMap to transform a set of points.

*  New Methods Defined:
*     Public:
*        astDssFits
*           Create a FitsChan holding a FITS description of the DSS plate fit.
*
*     Protected:
*        None.

*  Other Class Functions:
*     Public:
*        astIsADssMap
*           Test class membership.
*        astDssMap
*           Create a DssMap.
*
*     Protected:
*        astCheckDssMap
*           Validate class membership.
*        astInitDssMap
*           Initialise a DssMap.
*        astInitDssMapVtab
*           Initialise the virtual function table for the DssMap class.
*        astLoadDssMap
*           Load a DssMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstDssMap
*           DssMap object type.
*
*     Protected:
*        AstDssMapVtab
*           DssMap virtual function table type.

*  Feature Test Macros:
*     astCLASS
*        If the astCLASS macro is undefined, only public symbols are
*        made available, otherwise protected symbols (for use in other
*        class implementations) are defined. This macro also affects
*        the reporting of error context information, which is only
*        provided for external calls to the AST library.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.
*     (except for code supplied by Doug Mink, as noted in this file)

*  Authors:
*     DSB: D.S. Berry (Starlink)

*  History:
*     18-FEB-1997 (DSB):
*        Original version.
*     30-JUN-1997 (DSB):
*        All public functions made protected.
*     4-NOV-1997 (DSB):
*        Removed copy of supplied FitsChan from DssMap structure.
*     8-JAN-2003 (DSB):
*        Added protected astInitDssMapVtab method.
*     21-OCT-2004 (DSB):
*        Removed wcstools prototypes which clash with the MS Windows
*        runtime library.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#if defined(astCLASS)            /* Protected */

/* The code within this #if...#endif block is covered by the following
   statement of terms and conditions, which differ from the terms and
   conditions which apply elsewhere in this file.

***************************************************************************
*
* Copyright:   1988 Smithsonian Astrophysical Observatory
*              You may do anything you like with these files except remove
*              this copyright.  The Smithsonian Astrophysical Observatory
*              makes no representations about the suitability of this
*              software for any purpose.  It is provided "as is" without
*              express or implied warranty.
*
*****************************************************************************
*/

/* >>>>>>>>>>>>>>>>>> SAOimage wcs.h header file <<<<<<<<<<<<<<<<<< */

/* libwcs/wcs.h
   November 1, 1996
   By Doug Mink, Harvard-Smithsonian Center for Astrophysics */

struct WorldCoor {
  double	xref;		/* x reference coordinate value (deg) */
  double	yref;		/* y reference coordinate value (deg) */
  double	xrefpix;	/* x reference pixel */
  double	yrefpix;	/* y reference pixel */
  double	xinc;		/* x coordinate increment (deg) */
  double	yinc;		/* y coordinate increment (deg) */
  double	rot;		/* rotation (deg)  (from N through E) */
  double	crot,srot;	/* Cosine and sine of rotation angle */
  double	cd11,cd12,cd21,cd22;
				/* rotation matrix */
  double	dc11,dc12,dc21,dc22;
				/* inverse rotation matrix */
  double	equinox;	/* Equinox of coordinates default to 1950.0 */
  double	epoch;		/* Epoch of coordinates default to equinox */
  double	nxpix;		/* Number of pixels in X-dimension of image */
  double	nypix;		/* Number of pixels in Y-dimension of image */
  double	plate_ra;	/* Right ascension of plate center */
  double	plate_dec;	/* Declination of plate center */
  double	plate_scale;	/* Plate scale in arcsec/mm */
  double	x_pixel_offset;	/* X pixel offset of image lower right */
  double	y_pixel_offset;	/* Y pixel offset of image lower right */
  double	x_pixel_size;	/* X pixel_size */
  double	y_pixel_size;	/* Y pixel_size */
  double	ppo_coeff[6];
  double	amd_x_coeff[20]; /* X coefficients for plate model */
  double	amd_y_coeff[20]; /* Y coefficients for plate model */
  double	xpix;		/* x (RA) coordinate (pixels) */
  double	ypix;		/* y (dec) coordinate (pixels) */
  double	xpos;		/* x (RA) coordinate (deg) */
  double	ypos;		/* y (dec) coordinate (deg) */
  int		pcode;		/* projection code (1-8) */
  int		changesys;	/* 1 for FK4->FK5, 2 for FK5->FK4 */
  				/* 3 for FK4->galactic, 4 for FK5->galactic */
  int		printsys;	/* 1 to print coordinate system, else 0 */
  int		ndec;		/* Number of decimal places in PIX2WCST */
  int		degout;		/* 1 to always print degrees in PIX2WCST */
  int		tabsys;		/* 1 to put tab between RA & Dec, else 0 */
  int		rotmat;		/* 0 if CDELT, CROTA; 1 if CD */
  int		coorflip;	/* 0 if x=RA, y=Dec; 1 if x=Dec, y=RA */
  int		offscl;		/* 0 if OK, 1 if offscale */
  int		plate_fit;	/* 1 if plate fit, else 0 */
  int		wcson;		 /* 1 if WCS is set, else 0 */
  char		c1type[8];	/*  1st coordinate type code:
					RA--, GLON, ELON */
  char		c2type[8];	/*  2nd coordinate type code:
					DEC-, GLAT, ELAT */
  char		ptype[8];	/*  projection type code:
				    -SIN, -TAN, -ARC, -NCP, -GLS, -MER, -AIT */
  char		radecsys[16];	/* Reference frame: FK4, FK4-NO-E, FK5, GAPPT*/
  char		sysout[16];	/* Reference frame for output: FK4, FK5 */
  char		center[32];	/* Center coordinates (with frame) */
  char		search_format[120];	/* search command format */
				/* where %s is replaced by WCS coordinates */
};

#ifndef PI
#define PI		3.141592653589793
#endif

/* Conversions among hours of RA, degrees and radians. */
#define degrad(x)	((x)*PI/180.)
#define raddeg(x)	((x)*180./PI)
#define hrdeg(x)	((x)*15.)
#define deghr(x)	((x)/15.)
#define hrrad(x)	degrad(hrdeg(x))
#define radhr(x)	deghr(raddeg(x))


/* WCS subroutines in wcs.c */

/* >>>>> DSB: Prototypes for "subroutines in wcs.c" have been removed since
   they clash with prototypes defined by the MS windows runtime library and
   are not needed by AST. */

/* Oct 26 1994	New file
 * Dec 21 1994	Add rotation matrix
 * Dec 22 1994	Add flag for coordinate reversal

 * Mar  6 1995	Add parameters for Digital Sky Survey plate fit
 * Jun  8 1995	Add parameters for coordinate system change
 * Jun 21 1995	Add parameter for plate scale
 * Jul  6 1995	Add parameter to note whether WCS is set
 * Aug  8 1995	Add parameter to note whether to print coordinate system
 * Oct 16 1995	Add parameters to save image dimensions and center coordinates

 * Feb 15 1996	Add coordinate conversion functions
 * Feb 20 1996	Add flag for tab tables
 * Apr 26 1996	Add epoch of positions (actual date of image)
 * Jul  5 1996	Add subroutine declarations
 * Jul 19 1996	Add WCSFULL declaration
 * Aug  5 1996	Add WCSNINIT to initialize WCS for non-terminated header
 * Oct 31 1996	Add DCnn inverse rotation matrix
 * Nov  1 1996	Add NDEC number of decimal places in output
 */
/* >>>>>>>>>>>>>>>>>>>> End of SAOimage wcs.h header file <<<<<<<<<<<<<<<< */
#endif

#include "mapping.h"             /* Coordinate mappings (parent class) */
#include "fitschan.h"            /* Storage for FITS header cards */

/* C header files. */
/* --------------- */
#if defined(astCLASS)            /* Protected */
#include <stddef.h>
#endif

/* Type Definitions. */
/* ================= */
/* DssMap structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstDssMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   void *wcs;                    /* Pointer to structure holding plate fit info */

} AstDssMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstDssMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   AstFitsChan *(* DssFits)( AstDssMap *, int * );

} AstDssMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstDssMapGlobals {
   AstDssMapVtab Class_Vtab;
   int Class_Init;
} AstDssMapGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitDssMapGlobals_( AstDssMapGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(DssMap)          /* Check class membership */
astPROTO_ISA(DssMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstDssMap *astDssMap_( void *, const char *, int *, ...);
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstDssMap *astInitDssMap_( void *, size_t, int, AstDssMapVtab *,
                             const char *, AstFitsChan *, int * );

/* Vtab initialiser. */
void astInitDssMapVtab_( AstDssMapVtab *, const char *, int * );

/* Loader. */
AstDssMap *astLoadDssMap_( void *, size_t, AstDssMapVtab *,
                             const char *, AstChannel *, int * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
AstFitsChan *astDssFits_( AstDssMap *, int * );

#endif

/* Function interfaces. */
/* ==================== */
/* These macros are wrap-ups for the functions defined by this class
   to make them easier to invoke (e.g. to avoid type mis-matches when
   passing pointers to objects from derived classes). */

/* Interfaces to standard class functions. */
/* --------------------------------------- */
/* Some of these functions provide validation, so we cannot use them
   to validate their own arguments. We must use a cast when passing
   object pointers (so that they can accept objects from derived
   classes). */

/* Check class membership. */
#define astCheckDssMap(this) astINVOKE_CHECK(DssMap,this,0)
#define astVerifyDssMap(this) astINVOKE_CHECK(DssMap,this,1)

/* Test class membership. */
#define astIsADssMap(this) astINVOKE_ISA(DssMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astDssMap astINVOKE(F,astDssMap_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitDssMap(mem,size,init,vtab,name,fits) \
astINVOKE(O,astInitDssMap_(mem,size,init,vtab,name,astCheckFitsChan(fits),STATUS_PTR))

/* Vtab Initialiser. */
#define astInitDssMapVtab(vtab,name) astINVOKE(V,astInitDssMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadDssMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadDssMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckDssMap to validate DssMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#define astDssFits(this) astINVOKE(O,astDssFits_(astCheckDssMap(this),STATUS_PTR))
#endif
#endif





