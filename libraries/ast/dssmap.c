/*
*class++
*  Name:
*     DssMap

*  Purpose:
*     Map points using a Digitised Sky Survey plate solution.

*  Constructor Function:
*     The DssMap class does not have a constructor function.  A DssMap
*     is created only as a by-product of reading a FrameSet (using
c     astRead) from a FitsChan which contains FITS header cards
f     AST_READ) from a FitsChan which contains FITS header cards
*     describing a DSS plate solution, and whose Encoding attribute is
*     set to "DSS". The result of such a read, if successful, is a
*     FrameSet whose base and current Frames are related by a DssMap.

*  Description:
*     The DssMap class implements a Mapping which transforms between
*     2-dimensional pixel coordinates and an equatorial sky coordinate
*     system (right ascension and declination) using a Digitised Sky
*     Survey (DSS) astrometric plate solution.
*
*     The input coordinates are pixel numbers along the first and
*     second dimensions of an image, where the centre of the first
*     pixel is located at (1,1) and the spacing between pixel centres
*     is unity.
*
*     The output coordinates are right ascension and declination in
*     radians. The celestial coordinate system used (FK4, FK5, etc.)
*     is unspecified, and will usually be indicated by appropriate
*     keywords in a FITS header.

*  Inheritance:
*     The DssMap class inherits from the Mapping class.

*  Attributes:
*     The DssMap class does not define any new attributes beyond those
*     which are applicable to all Mappings.

*  Functions:
c     The DssMap class does not define any new functions beyond those
f     The DssMap class does not define any new routines beyond those
*     which are applicable to all Mappings.

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
*     RFWS: R.F. Warren-Smith (Starlink, RAL)

*  History:
*     18-FEB-1997 (DSB):
*        Original version.
*     30-JUN-1997 (DSB):
*        astDssFits and astDssMap made protected instead of public.
*     15-JUL-1997 (RFWS):
*        Tidied public prologues.
*     5-SEP-197 (RFWS):
*        Added prototypes for platepos and platepix.
*     4-NOV-1997 (DSB):
*        o  A copy of the supplied FitsChan is no longer stored inside
*        the DssMap. The FitsChan returned by DssFits is now derived from
*        the wcs information stored in the SAOimage "WorldCoor" structure
*        (stored within the DssMap), and only contains the keywords
*        necessary to reconstruct the DssMap.
*        o  The external representation of a DssMap is now stored in a set
*        of scalar values, rather than a FitsChan.
*     22-DEC-1997 (DSB):
*        Bug fixed in MapMerge which caused a core dump when a
*        DssMap/WinMap combination is succesfully simplified.
*     8-JAN-2003 (DSB):
*        Changed private InitVtab method to protected astInitDssMapVtab
*        method.
*     14-FEB-2006 (DSB):
*        Override astGetObjSize.
*     10-MAY-2006 (DSB):
*        Override astEqual.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS DssMap

/* Macro which returns the nearest integer to a given floating point
   value. */
#define NINT(x) (int)((x)+(((x)>0.0)?0.5:-0.5))

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "memory.h"              /* Memory allocation facilities */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "mapping.h"             /* Coordinate mappings (parent class) */
#include "channel.h"             /* I/O channels */
#include "fitschan.h"            /* Manipulation of FITS header cards */
#include "wcsmap.h"              /* Degrees/radians conversion factors */
#include "winmap.h"              /* Shift and scale mappings */
#include "dssmap.h"              /* Interface definition for this class */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <float.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static int (* parent_getobjsize)( AstObject *, int * );
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );


#ifdef THREAD_SAFE
/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(DssMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(DssMap,Class_Init)
#define class_vtab astGLOBAL(DssMap,Class_Vtab)


#include <pthread.h>


#else


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstDssMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstFitsChan *DssFits( AstDssMap *, int * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static int platepix( double, double, struct WorldCoor *, double *, double * );
static int platepos( double, double, struct WorldCoor *, double *, double * );
static struct WorldCoor *BuildWcs( AstFitsChan *, const char *, const char *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *obj, int * );
static void Dump( AstObject *, AstChannel *, int * );
static int Equal( AstObject *, AstObject *, int * );

static int GetObjSize( AstObject *, int * );
/* Member functions. */
/* ================= */
static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two DssMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "dssmap.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     DssMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two DssMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a DssMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the DssMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstDssMap *that;
   AstDssMap *this;
   int i;
   int nin;
   int nout;
   int result;
   struct WorldCoor *this_wcs;
   struct WorldCoor *that_wcs;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two DssMap structures. */
   this = (AstDssMap *) this_object;
   that = (AstDssMap *) that_object;

/* Check the second object is a DssMap. We know the first is a
   DssMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsADssMap( that ) ) {

/* Get the number of inputs and outputs and check they are the same for both. */
      nin = astGetNin( this );
      nout = astGetNout( this );
      if( astGetNin( that ) == nin && astGetNout( that ) == nout ) {

/* If the Invert flags for the two DssMaps differ, it may still be possible
   for them to be equivalent. First compare the DssMaps if their Invert
   flags are the same. In this case all the attributes of the two DssMaps
   must be identical. */
         if( astGetInvert( this ) == astGetInvert( that ) ) {

            this_wcs = ( struct WorldCoor *) this->wcs;
            that_wcs = ( struct WorldCoor *) that->wcs;

            if( this_wcs->x_pixel_offset == that_wcs->x_pixel_offset &&
                this_wcs->y_pixel_offset == that_wcs->y_pixel_offset &&
                this_wcs->ppo_coeff[2] == that_wcs->ppo_coeff[2] &&
                this_wcs->ppo_coeff[5] == that_wcs->ppo_coeff[5] &&
                this_wcs->x_pixel_size == that_wcs->x_pixel_size &&
                this_wcs->y_pixel_size == that_wcs->y_pixel_size &&
                this_wcs->plate_dec == that_wcs->plate_dec &&
                this_wcs->plate_ra == that_wcs->plate_ra ) {

                result = 1;
                for( i = 0; i < 13; i++ ) {
                   if( this_wcs->amd_x_coeff[i] != that_wcs->amd_x_coeff[i] ||
                       this_wcs->amd_y_coeff[i] != that_wcs->amd_y_coeff[i] ) {
                      result = 0;
                      break;
                   }
                }

             }

/* If the Invert flags for the two DssMaps differ, the attributes of the two
   DssMaps must be inversely related to each other. */
         } else {

/* In the specific case of a DssMap, Invert flags must be equal. */
            result = 0;

         }
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static int GetObjSize( AstObject *this_object, int *status ) {
/*
*  Name:
*     GetObjSize

*  Purpose:
*     Return the in-memory size of an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "dssmap.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     DssMap member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied DssMap,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the DssMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstDssMap *this;         /* Pointer to DssMap structure */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the DssMap structure. */
   this = (AstDssMap *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by thsi class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );
   result += astTSizeOf( this->wcs );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}


static struct WorldCoor *BuildWcs( AstFitsChan *fits, const char *method,
                                   const char *class, int *status ) {
/*
*  Name:
*     BuildWcs

*  Purpose:
*     Copy DSS plate fit information from a FitsChan to a SAOimage
*     WorldCoor structure.

*  Type:
*     Private function.

*  Synopsis:
*     #include "dssmap.h"
*     struct WorldCoor *BuildWcs( AstFitsChan *fits, const char *method,
*                                 const char *class )

*  Class Membership:
*     DssMap member function.

*  Description:
*     This creates a WorldCoor structure and copies the required data
*     from the supplied FitsChan into the new WorldCoor structure. Note,
*     only those components of the WorldCoor structure which are needed to
*     transform between pixel and sky coordinates are initialised in the
*     returned structure.

*  Parameters:
*     fits
*        Pointer to the FitsChan containing the FITS header describing
*        the DSS plate fit.
*     method
*        The calling method (for error messages).
*     class
*        The object class (for error messages).

*  Returned Value:
*     A pointer to the new WorldCoor structure. This should be freed
*     using astFree when no longer needed.

*  Notes:
*     -  A NULL pointer is returned if an error has already occurred, or
*     if this function should fail for any reason.

*/

/* Local Variables: */
   char name_buff[ 10 ];          /* Buffer for keyword name */
   char *name;                    /* Pointer to jeyword name string */
   char *ckeyval;                 /* Pointer to string keyword value */
   struct WorldCoor *ret;         /* Pointer to the returned structure */
   double rah,ram,ras;            /* Centre RA hours, minutes and seconds */
   double dsign;                  /* Sign of centre dec */
   double decd,decm,decs;         /* Centre Dec degrees, minutes, seconds */
   double dec_deg;                /* Centre Dec in degrees */
   double ra_hours;               /* Centre RA in hours */
   int i;                         /* Coefficient index */

/* Check the local error status. */
   if ( !astOK ) return NULL;

/* Get memory to hold the returned structure. */
   ret = (struct WorldCoor *) astMalloc( sizeof( struct WorldCoor ) );

/* Check the memory can be used. */
   if( astOK ){

/* The following code is based on the "wcsinit" function in SAOimage file
   wcs.c. Note, only the values needed in the platepos and platepix
   functions are set up. The FITS keywords are accessed in the order in
   which they are usually stored in a FITS file. This will cut down the
   time spent searching for keywords. Report an error if any required
   keyword is not found. */

/* Plate center RA */
      rah = 0.0;
      ram = 0.0;
      ras = 0.0;

      name = "PLTRAH";
      if( !astGetFitsF( fits, name, &rah ) && astOK ){
         astError( AST__BDFTS, "%s(%s): No value has been supplied for the "
                   "FITS keyword '%s'.", status, method, class, name );
      }

      name = "PLTRAM";
      if( !astGetFitsF( fits, name, &ram ) && astOK ){
         astError( AST__BDFTS, "%s(%s): No value has been supplied for the "
                  "FITS keyword '%s'.", status, method, class, name );
      }

      name = "PLTRAS";
      if( !astGetFitsF( fits, name, &ras ) && astOK ){
         astError( AST__BDFTS, "%s(%s): No value has been supplied for the "
                  "FITS keyword '%s'.", status, method, class, name );
      }

      ra_hours = rah + (ram / (double)60.0) + (ras / (double)3600.0);
      ret->plate_ra = AST__DD2R*15.0*ra_hours;


/* Plate center Dec */
      name = "PLTDECSN";
      if( !astGetFitsS( fits, name, &ckeyval ) && astOK ){
         dsign = 1.0;

      } else {
         if( *ckeyval == '-' ){
            dsign = -1.0;
         } else {
            dsign = 1.0;
         }

      }

      decd = 0.0;
      decm = 0.0;
      decs = 0.0;

      name = "PLTDECD";
      if( !astGetFitsF( fits, name, &decd ) && astOK ){
         astError( AST__BDFTS, "%s(%s): No value has been supplied for the "
                  "FITS keyword '%s'.", status, method, class, name );
      }

      name = "PLTDECM";
      if( !astGetFitsF( fits, name, &decm ) && astOK ){
         astError( AST__BDFTS, "%s(%s): No value has been supplied for the "
                  "FITS keyword '%s'.", status, method, class, name );
      }

      name = "PLTDECS";
      if( !astGetFitsF( fits, name, &decs ) && astOK ){
         astError( AST__BDFTS, "%s(%s): No value has been supplied for the "
                  "FITS keyword '%s'.", status, method, class, name );
      }

      dec_deg = dsign * (decd+(decm/(double)60.0)+(decs/(double)3600.0));
      ret->plate_dec = AST__DD2R*dec_deg;

/* Plate Scale arcsec per mm  */
      name = "PLTSCALE";
      if( !astGetFitsF( fits, name, &ret->plate_scale ) && astOK ){
         astError( AST__BDFTS, "%s(%s): No value has been supplied for the "
                  "FITS keyword '%s'.", status, method, class, name );
      }

/* X and Y corners (in pixels) */
      name = "CNPIX1";
      if( !astGetFitsF( fits, name, &ret->x_pixel_offset ) && astOK ){
         astError( AST__BDFTS, "%s(%s): No value has been supplied for the "
                  "FITS keyword '%s'.", status, method, class, name );
      }

      name = "CNPIX2";
      if( !astGetFitsF( fits, name, &ret->y_pixel_offset ) && astOK ){
         astError( AST__BDFTS, "%s(%s): No value has been supplied for the "
                  "FITS keyword '%s'.", status, method, class, name );
      }

/* X and Y pixel sizes (microns). */
      name = "XPIXELSZ";
      if( !astGetFitsF( fits, name, &ret->x_pixel_size ) && astOK ){
         astError( AST__BDFTS, "%s(%s): No value has been supplied for the "
                  "FITS keyword '%s'.", status, method, class, name );
      }

      name = "YPIXELSZ";
      if( !astGetFitsF( fits, name, &ret->y_pixel_size ) && astOK ){
         astError( AST__BDFTS, "%s(%s): No value has been supplied for the "
                  "FITS keyword '%s'.", status, method, class, name );
      }

/* Orientation Coefficients. Only report an error if PPO3 or PPO6 are
   missing (these are the only two which are actually used). Assume a
   value of zero for any of the others which are missing. */
      name = name_buff;
      for ( i = 0; i < 6; i++ ) {
         sprintf( name_buff, "PPO%d", i + 1 );
         if( !astGetFitsF( fits, name, &ret->ppo_coeff[i] ) ) {
            ret->ppo_coeff[i] = 0.0;
            if( ( i == 2 || i == 5 ) && astOK ) {
               astError( AST__BDFTS, "%s(%s): No value has been supplied "
                         "for the FITS keyword '%s'.", status, method, class,
                         name );
               break;
            }
         }
      }

/* Plate solution x and y coefficients. Report an error if any of
   coefficients 1 to 14 are missing. Assume a value of zero for any
   others which are missing.  */
      name = name_buff;
      for( i = 0; i < 19; i++ ){
         sprintf( name_buff, "AMDX%d", i + 1 );
         if( !astGetFitsF( fits, name, &ret->amd_x_coeff[i] ) ) {
            ret->amd_x_coeff[i] = 0.0;
            if( i < 13 && astOK ){
               astError( AST__BDFTS, "%s(%s): No value has been supplied "
                         "for the FITS keyword '%s'.", status, method, class, name );
               break;
            }
         }
      }

      for( i = 0; i < 19; i++ ){
         sprintf( name_buff, "AMDY%d", i + 1 );
         if( !astGetFitsF( fits, name, &ret->amd_y_coeff[i] ) ){
            ret->amd_y_coeff[i] = 0.0;
            if( i < 13 && astOK ){
               astError( AST__BDFTS, "%s(%s): No value has been supplied "
                         "for the FITS keyword '%s'.", status, method, class, name );
               break;
            }
         }
      }

/* If anything went wrong, free the returned structure. */
      if( !astOK ) ret = (struct WorldCoor *) astFree( (void *) ret );
   }

/* Return the pointer. */
   return ret;
}

static AstFitsChan *DssFits( AstDssMap *this, int *status ) {
/*
*+
*  Name:
*     astDssFits

*  Purpose:
*     Return a pointer to a FitsChan describing a DssMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "dssmap.h"
*     AstFitsChan *DssFits( AstDssMap *this )

*  Class Membership:
*     DssMap method.

*  Description:
*     This function returns a pointer to a DSS-encoded FitsChan containing
*     cards generated from the information stored with the DssMap. The
*     keywords contained in the FitsChan are those which would ne needed to
*     re-create the DssMap (see astDSSMap).

*  Parameters:
*     this
*        Pointer to the DssMap.

*  Returned Value:
*     astDssFits()
*        A pointer to the FitsChan.

*  Notes:
*     - The returned pointer should be annuled using astAnnul when no longer
*     needed.
*     - A value of NULL will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstFitsChan *ret;              /* Pointer to the returned FitsChan  */
   char *comm;                    /* Pointer to keyword comment string */
   char *name;                    /* Pointer to keyword name string */
   char name_buff[ 10 ];          /* Buffer for keyword name */
   double dec;                    /* Centre Dec in degrees */
   double decd,decm,decs;         /* Centre Dec degrees, minutes, seconds */
   double ra;                     /* Centre RA in hours */
   double rah,ram,ras;            /* Centre RA hours, minutes and seconds */
   int i;                         /* Coefficient index */
   struct WorldCoor *wcs;         /* WCS information from the DssMap */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Store a pointer to the WCS information stored in the DSSMap. */
   wcs = (struct WorldCoor *) this->wcs,

/* Create a new empty FitsChan, using DSS encoding. */
   ret = astFitsChan( NULL, NULL, "Encoding=DSS", status );

/* Create the keyword values and stored them in the returned FitsChan... */

/* Plate centre RA. */
   ra = wcs->plate_ra/( AST__DD2R*15.0 );
   ra = modf( ra, &rah );
   ra = modf( 60.0*ra, &ram );
   ras = 60.0*ra;

   astSetFitsI( ret, "PLTRAH", NINT( rah ), "Plate centre RA", 0 );
   astSetFitsI( ret, "PLTRAM", NINT( ram ), " ", 0 );
   astSetFitsF( ret, "PLTRAS", ras, " ", 0 );

/* Plate centre DEC. */
   dec = wcs->plate_dec/AST__DD2R;
   if( dec < 0.0 ) {
      dec = -dec;
      astSetFitsS( ret, "PLTDECSN", "-", "Plate centre DEC", 0 );
   } else {
      astSetFitsS( ret, "PLTDECSN", "+", "Plate centre DEC", 0 );
   }

   dec = modf( dec, &decd );
   dec = modf( 60.0*dec, &decm );
   decs = 60.0*dec;

   astSetFitsI( ret, "PLTDECD", NINT( decd ), " ", 0 );
   astSetFitsI( ret, "PLTDECM", NINT( decm ), " ", 0 );
   astSetFitsF( ret, "PLTDECS", decs, " ", 0 );

/* Plate Scale arcsec per mm  */
   astSetFitsF( ret, "PLTSCALE", wcs->plate_scale, "Plate Scale arcsec per mm",
                0 );

/* X and Y corners (in pixels) */
   astSetFitsI( ret, "CNPIX1", NINT( wcs->x_pixel_offset ),
                "X corner  (pixels)", 0 );
   astSetFitsI( ret, "CNPIX2", NINT( wcs->y_pixel_offset ),
                "Y corner", 0 );

/* X and Y pixel sizes (microns). */
   astSetFitsF( ret, "XPIXELSZ", wcs->x_pixel_size,
                "X pixel size (microns)", 0 );
   astSetFitsF( ret, "YPIXELSZ", wcs->y_pixel_size,
                "Y pixel size (microns)", 0 );

/* Orientation Coefficients. */
   name = name_buff;
   comm = "Orientation Coefficients";
   for ( i = 0; i < 6; i++ ) {
      sprintf( name_buff, "PPO%d", i + 1 );
      astSetFitsF( ret, name, wcs->ppo_coeff[i], comm, 0 );
      comm = " ";
   }

/* Plate solution x and y coefficients. */
   comm = "Plate solution x coefficients";
   for( i = 0; i < 19; i++ ){
      sprintf( name_buff, "AMDX%d", i + 1 );
      astSetFitsF( ret, name, wcs->amd_x_coeff[i], comm, 0 );
      comm = " ";
   }

   comm = "Plate solution y coefficients";
   for( i = 0; i < 19; i++ ){
      sprintf( name_buff, "AMDY%d", i + 1 );
      astSetFitsF( ret, name, wcs->amd_y_coeff[i], comm, 0 );
      comm = " ";
   }

/* Return a pointer to the FitsChan. */
   return ret;
}

void astInitDssMapVtab_(  AstDssMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitDssMapVtab

*  Purpose:
*     Initialise a virtual function table for a DssMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "dssmap.h"
*     void astInitDssMapVtab( AstDssMapVtab *vtab, const char *name )

*  Class Membership:
*     DssMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the DssMap class.

*  Parameters:
*     vtab
*        Pointer to the virtual function table. The components used by
*        all ancestral classes will be initialised if they have not already
*        been initialised.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the virtual function table belongs (it
*        is this pointer value that will subsequently be returned by the Object
*        astClass function).
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */
   AstMappingVtab *mapping;      /* Pointer to Mapping component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitMappingVtab( (AstMappingVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsADssMap) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstMappingVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->DssFits = DssFits;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   mapping = (AstMappingVtab *) vtab;
   object = (AstObjectVtab *) vtab;

   parent_transform = mapping->Transform;
   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;
   mapping->Transform = Transform;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   object->Equal = Equal;
   mapping->MapMerge = MapMerge;

/* Declare the class dump, copy and delete function. */
   astSetDump( object, Dump, "DssMap", "DSS plate fit mapping" );
   astSetCopy( object, Copy );
   astSetDelete( object, Delete );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static int MapMerge( AstMapping *this, int where, int series, int *nmap,
                     AstMapping ***map_list, int **invert_list, int *status ) {
/*
*  Name:
*     MapMerge

*  Purpose:
*     Simplify a sequence of Mappings containing a DssMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     DssMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated DssMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated DssMap with a Mapping which it
*     considers simpler, or to merge it with the Mappings which
*     immediately precede it or follow it in the sequence (both will
*     normally be considered). This is sufficient to ensure the
*     eventual simplification of most Mapping sequences by repeated
*     application of this function.
*
*     In some cases, the function may attempt more elaborate
*     simplification, involving any number of other Mappings in the
*     sequence. It is not restricted in the type or scope of
*     simplification it may perform, but will normally only attempt
*     elaborate simplification in cases where a more straightforward
*     approach is not adequate.

*  Parameters:
*     this
*        Pointer to the nominated DssMap which is to be merged with
*        its neighbours. This should be a cloned copy of the DssMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        DssMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated DssMap resides.
*     series
*        A non-zero value indicates that the sequence of Mappings to
*        be simplified will be applied in series (i.e. one after the
*        other), whereas a zero value indicates that they will be
*        applied in parallel (i.e. on successive sub-sets of the
*        input/output coordinates).
*     nmap
*        Address of an int which counts the number of Mappings in the
*        sequence. On entry this should be set to the initial number
*        of Mappings. On exit it will be updated to record the number
*        of Mappings remaining after simplification.
*     map_list
*        Address of a pointer to a dynamically allocated array of
*        Mapping pointers (produced, for example, by the astMapList
*        method) which identifies the sequence of Mappings. On entry,
*        the initial sequence of Mappings to be simplified should be
*        supplied.
*
*        On exit, the contents of this array will be modified to
*        reflect any simplification carried out. Any form of
*        simplification may be performed. This may involve any of: (a)
*        removing Mappings by annulling any of the pointers supplied,
*        (b) replacing them with pointers to new Mappings, (c)
*        inserting additional Mappings and (d) changing their order.
*
*        The intention is to reduce the number of Mappings in the
*        sequence, if possible, and any reduction will be reflected in
*        the value of "*nmap" returned. However, simplifications which
*        do not reduce the length of the sequence (but improve its
*        execution time, for example) may also be performed, and the
*        sequence might conceivably increase in length (but normally
*        only in order to split up a Mapping into pieces that can be
*        more easily merged with their neighbours on subsequent
*        invocations of this function).
*
*        If Mappings are removed from the sequence, any gaps that
*        remain will be closed up, by moving subsequent Mapping
*        pointers along in the array, so that vacated elements occur
*        at the end. If the sequence increases in length, the array
*        will be extended (and its pointer updated) if necessary to
*        accommodate any new elements.
*
*        Note that any (or all) of the Mapping pointers supplied in
*        this array may be annulled by this function, but the Mappings
*        to which they refer are not modified in any way (although
*        they may, of course, be deleted if the annulled pointer is
*        the final one).
*     invert_list
*        Address of a pointer to a dynamically allocated array which,
*        on entry, should contain values to be assigned to the Invert
*        attributes of the Mappings identified in the "*map_list"
*        array before they are applied (this array might have been
*        produced, for example, by the astMapList method). These
*        values will be used by this function instead of the actual
*        Invert attributes of the Mappings supplied, which are
*        ignored.
*
*        On exit, the contents of this array will be updated to
*        correspond with the possibly modified contents of the
*        "*map_list" array.  If the Mapping sequence increases in
*        length, the "*invert_list" array will be extended (and its
*        pointer updated) if necessary to accommodate any new
*        elements.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     If simplification was possible, the function returns the index
*     in the "map_list" array of the first element which was
*     modified. Otherwise, it returns -1 (and makes no changes to the
*     arrays supplied).

*  Notes:
*     - A value of -1 will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstDssMap *dm;        /* Pointer to supplied DssMap */
   AstDssMap *dmnew;     /* Pointer to replacement DssMap */
   AstFitsChan *fits;    /* FITS headers for replacement DssMap */
   AstFitsChan *fits_dss;/* FITS headers for supplied DssMap */
   AstWinMap *wm;        /* Pointer to the adjacent WinMap */
   double *a;            /* Pointer to shift terms */
   double *b;            /* Pointer to scale terms */
   double cnpix1;        /* X pixel origin */
   double cnpix2;        /* Y pixel origin */
   double xpixelsz;      /* X pixel size */
   double ypixelsz;      /* Y pixel size */
   int i;                /* Loop counter */
   int ok;               /* All FITS keywords found? */
   int old_winv;         /* original Invert value for supplied WinMap */
   int result;           /* Result value to return */
   int wmi;              /* Index of adjacent WinMap in map list */
   struct WorldCoor *wcs;/* Pointer to SAOimage wcs structure */

/* Initialise. */
   result = -1;

/* Check the global error status. */
   if ( !astOK ) return result;

/* The only simplification easily possible is if a WinMap maps the pixel
   coordinates prior to a DssMap. If the DssMap has not been inverted, the
   WinMap must be applied before the DssMap, otherwise the WinMap must be
   applied after the DssMap. */
   if( series ){

      if( !( *invert_list )[ where ] ){
         wmi = where - 1;
      } else {
         wmi = where + 1;
      }

      if( wmi >= 0 && wmi < *nmap ){
         if( !strcmp( astGetClass( ( *map_list )[ wmi ] ), "WinMap" ) ){

/* Temporarily set the Invert attribute of the WinMap to the supplied value. */
            wm = (AstWinMap *) ( *map_list )[ wmi ];
            old_winv = astGetInvert( wm );
            astSetInvert( wm, ( *invert_list )[ wmi ] );

/* Get a copy of the scale and shift terms from the WinMap. */
            astWinTerms( wm, &a, &b );

/* Check that the scale and shift terms are usable. */
            if( astOK &&
                a[ 0 ] != AST__BAD && b[ 0 ] != AST__BAD && b[ 0 ] != 0.0 &&
                a[ 1 ] != AST__BAD && b[ 1 ] != AST__BAD && b[ 1 ] != 0.0 ){

/* Get the values of the keywords which define the origin and scales of
   the DssMap pixel coordinate system. */
               dm = (AstDssMap *) ( *map_list )[ where ];
               wcs = (struct WorldCoor *) ( dm->wcs );

               cnpix1 = wcs->x_pixel_offset;
               cnpix2 = wcs->y_pixel_offset;
               xpixelsz = wcs->x_pixel_size;
               ypixelsz = wcs->y_pixel_size;

/* Calculate new values which take into account the WinMap. */
               if( wmi == where - 1 ){
                  xpixelsz *= b[ 0 ];
                  ypixelsz *= b[ 1 ];
                  cnpix1 = 0.5 + ( cnpix1 + a[ 0 ] - 0.5 )/b[ 0 ];
                  cnpix2 = 0.5 + ( cnpix2 + a[ 1 ] - 0.5 )/b[ 1 ];

               } else {
                  xpixelsz /= b[ 0 ];
                  ypixelsz /= b[ 1 ];
                  cnpix1 = b[ 0 ]*( cnpix1 - 0.5 ) - a[ 0 ] + 0.5;
                  cnpix2 = b[ 1 ]*( cnpix2 - 0.5 ) - a[ 1 ] + 0.5;
               }

/* The CNPIX1 and CNPIX2 keywords are integer keywords. Therefore, we can
   only do the simplification if the new values are integer to a good
   approximation. We use one hundredth of a pixel. */
               if( fabs( cnpix1 - NINT( cnpix1 ) ) < 0.01 &&
                   fabs( cnpix2 - NINT( cnpix2 ) ) < 0.01 ){

/* Get a copy of the FitsChan holding the header cards which define the
   DssMap. */
                  fits_dss = astDssFits( dm );
                  fits = astCopy( fits_dss );
                  fits_dss = astAnnul( fits_dss );

/* Update the value of each of the changed keywords. */
                  ok = 1;

                  astClearCard( fits );
                  if( astFindFits( fits, "CNPIX1", NULL, 0 ) ){
                     astSetFitsI( fits, "CNPIX1", NINT( cnpix1 ), NULL, 1 );
                  } else {
                     ok = 0;
                  }

                  astClearCard( fits );
                  if( astFindFits( fits, "CNPIX2", NULL, 0 ) ){
                     astSetFitsI( fits, "CNPIX2", NINT( cnpix2 ), NULL, 1 );
                  } else {
                     ok = 0;
                  }

                  astClearCard( fits );
                  if( astFindFits( fits, "XPIXELSZ", NULL, 0 ) ){
                     astSetFitsF( fits, "XPIXELSZ", xpixelsz, NULL, 1 );
                  } else {
                     ok = 0;
                  }

                  astClearCard( fits );
                  if( astFindFits( fits, "YPIXELSZ", NULL, 0 ) ){
                     astSetFitsF( fits, "YPIXELSZ", ypixelsz, NULL, 1 );
                  } else {
                     ok = 0;
                  }

/* If all the keywords were updated succesfully, create the new DssMap
   based on the modified FITS header cards. */
                  if( ok ){
		     dmnew = astDssMap( fits, "", status );

/* Anull the DssMap pointer in the list and replace it with the new one.
   The invert flag is left unchanged. */
                     dm = astAnnul( dm );
                     ( *map_list )[ where ] = (AstMapping *) dmnew;

/* Annul the WinMap pointer in the list, and shuffle any remaining
   Mappings down to fill the gap. */
                     wm = astAnnul( wm );
                     for ( i = wmi + 1; i < *nmap; i++ ) {
                        ( *map_list )[ i - 1 ] = ( *map_list )[ i ];
                        ( *invert_list )[ i - 1 ] = ( *invert_list )[ i ];
                     }

/* Clear the vacated element at the end. */
                     ( *map_list )[ *nmap - 1 ] = NULL;
                     ( *invert_list )[ *nmap - 1 ] = 0;

/* Decrement the Mapping count and return the index of the first
   modified element. */
                     ( *nmap )--;
                     result = astMIN( wmi, where );

                  }

/* Annul the FitsChan holding the modified header cards. */
                  fits = astAnnul( fits );
               }
            }

/* Free the arrays holding scale and shift terms from the WinMap. */
            a = (double *) astFree( (void *) a );
            b = (double *) astFree( (void *) b );

/* Reinstate the original setting of the Invert attribute of the WinMap (if
   it still exists). */
            if( wm ) astSetInvert( wm, old_winv );

         }
      }
   }

/* Return the result. */
   return result;
}

static AstPointSet *Transform( AstMapping *this, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Apply a DssMap to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "dssmap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     DssMap member function (over-rides the astTransform protected
*     method inherited from the Mapping class).

*  Description:
*     This function takes a DssMap and a set of points encapsulated in a
*     PointSet and transforms the points so as to apply the required DSS
*     plate fit.

*  Parameters:
*     this
*        Pointer to the DssMap.
*     in
*        Pointer to the PointSet holding the input coordinate data.
*     forward
*        A non-zero value indicates that the forward coordinate transformation
*        should be applied, while a zero value requests the inverse
*        transformation.
*     out
*        Pointer to a PointSet which will hold the transformed (output)
*        coordinate values. A NULL value may also be given, in which case a
*        new PointSet will be created by this function.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the output (possibly new) PointSet.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*     -  The number of coordinate values per point in the input PointSet must
*     be 2.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstPointSet *result;          /* Pointer to output PointSet */
   AstDssMap *map;               /* Pointer to DssMap to be applied */
   double **ptr_in;              /* Pointer to input coordinate data */
   double **ptr_out;             /* Pointer to output coordinate data */
   double *aa;                   /* Pointer to next longitude value */
   double *bb;                   /* Pointer to next latitude value */
   double *xx;                   /* Pointer to next pixel X value */
   double *yy;                   /* Pointer to next pixel Y value */
   int npoint;                   /* Number of points */
   int point;                    /* Loop counter for points */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the DssMap. */
   map = (AstDssMap *) this;

/* Apply the parent mapping using the stored pointer to the Transform member
   function inherited from the parent Mapping class. This function validates
   all arguments and generates an output PointSet if necessary, but does not
   actually transform any coordinate values. */
   result = (*parent_transform)( this, in, forward, out, status );

/* We will now extend the parent astTransform method by performing the
   calculations needed to generate the output coordinate values. */

/* Determine the numbers of points from the input PointSet and obtain
   pointers for accessing the input and output coordinate values. */
   npoint = astGetNpoint( in );
   ptr_in = astGetPoints( in );
   ptr_out = astGetPoints( result );

/* Determine whether to apply the forward or inverse mapping, according to the
   direction specified and whether the mapping has been inverted. */
   if ( astGetInvert( map ) ) forward = !forward;

/* Perform coordinate arithmetic. */
/* ------------------------------ */
   if ( astOK ) {

/* First deal with forward transformations. */
      if( forward ){

/* Store pointers to the next value on each axis. */
         xx = ptr_in[ 0 ];
         yy = ptr_in[ 1 ];
         aa = ptr_out[ 0 ];
         bb = ptr_out[ 1 ];

/* Loop to apply the plate fit to all the points, checking for (and
   propagating) bad values in the process. */
         for ( point = 0; point < npoint; point++ ) {
            if( *xx != AST__BAD && *yy != AST__BAD ){

/* If the pixel position is transformed succesfully, convert the returned
   RA/DEC from degrees to radians. Otherwise, store bad values. NB,
   platepos returns zero for success. */
               if( !platepos( *xx, *yy, (struct WorldCoor *) map->wcs,
                              aa, bb ) ){
                  (*aa) *= AST__DD2R;
                  (*bb) *= AST__DD2R;

               } else {
                  *aa = AST__BAD;
                  *bb = AST__BAD;
               }

            } else {
               *aa = AST__BAD;
               *bb = AST__BAD;
            }

/* Move on to the next point. */
            xx++;
            yy++;
            aa++;
            bb++;
         }

/* Now deal with inverse transformations in the same way. */
      } else {
         aa = ptr_in[ 0 ];
         bb = ptr_in[ 1 ];
         xx = ptr_out[ 0 ];
         yy = ptr_out[ 1 ];

         for ( point = 0; point < npoint; point++ ) {
            if( *aa != AST__BAD && *bb != AST__BAD ){

               if( platepix( AST__DR2D*(*aa), AST__DR2D*(*bb),
                             (struct WorldCoor *) map->wcs, xx, yy ) ){
                  *xx = AST__BAD;
                  *yy = AST__BAD;
               }

            } else {
               *xx = AST__BAD;
               *yy = AST__BAD;
            }

            xx++;
            yy++;
            aa++;
            bb++;
         }
      }
   }

/* Return a pointer to the output PointSet. */
   return result;
}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   this class using the macros defined for this purpose in the
   "object.h" file. For a description of each attribute, see the class
   interface (in the associated .h file). */

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for DssMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for DssMap objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     void

*  Notes:
*     -  This constructor makes a deep copy.
*/


/* Local Variables: */
   AstDssMap *in;                /* Pointer to input DssMap */
   AstDssMap *out;               /* Pointer to output DssMap */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output DssMaps. */
   in = (AstDssMap *) objin;
   out = (AstDssMap *) objout;

/* Store a copy of the input SAOIMAGE WorldCoor structure in the output. */
   out->wcs = astStore( NULL, in->wcs, sizeof( struct WorldCoor ) );

   return;

}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for DssMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for DssMap objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     void

*  Notes:
*     This function attempts to execute even if the global error status is
*     set.
*/

/* Local Variables: */
   AstDssMap *this;            /* Pointer to DssMap */

/* Obtain a pointer to the DssMap structure. */
   this = (AstDssMap *) obj;

/* Free the SAOIMAGE WorldCoor structure. */
   this->wcs = astFree( this->wcs );

}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for DssMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the DssMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the DssMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

   AstDssMap *this;          /* Pointer to the DssMap structure */
   struct WorldCoor *wcs;    /* Pointer to SAOimage wcs structure */
   char name_buff[ 11 ];     /* Buffer for keyword string */
   int i;                    /* Coefficient index */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the DssMap structure. */
   this = (AstDssMap *) this_object;

/* Get a pointer to the WorldCoor structure holding the description of the
   DssMap. */
   wcs = (struct WorldCoor *) ( this->wcs );

/* Write out values representing the contents of the WorldCoor structure.
   Only the components which are required to re-create the DssMap are
   written out. */
   astWriteDouble( channel, "PltRA", 1, 1, wcs->plate_ra, "Plate centre RA (radians)" );
   astWriteDouble( channel, "PltDec", 1, 1, wcs->plate_dec, "Plate centre Dec (radians)" );
   astWriteDouble( channel, "PltScl", 1, 1, wcs->plate_scale, "Plate scale (arcsec/mm)" );
   astWriteDouble( channel, "CNPix1", 1, 1, wcs->x_pixel_offset, "X Pixel offset (pixels)" );
   astWriteDouble( channel, "CNPix2", 1, 1, wcs->y_pixel_offset, "Y Pixel offset (pixels)" );
   astWriteDouble( channel, "XPixSz", 1, 1, wcs->x_pixel_size, "X Pixel size (microns)" );
   astWriteDouble( channel, "YPixSz", 1, 1, wcs->y_pixel_size, "Y Pixel size (microns)" );

   for( i = 0; i < 6; i++ ) {
      sprintf( name_buff, "PPO%d", i + 1 );
      astWriteDouble( channel, name_buff, 1, 1, wcs->ppo_coeff[i],
                      "Orientation coefficients" );
   }

   for( i = 0; i < 19; i++ ) {
      sprintf( name_buff, "AMDX%d", i + 1 );
      astWriteDouble( channel, name_buff, 1, 1, wcs->amd_x_coeff[i],
                      "Plate solution X coefficients" );
   }

   for( i = 0; i < 19; i++ ) {
      sprintf( name_buff, "AMDY%d", i + 1 );
      astWriteDouble( channel, name_buff, 1, 1, wcs->amd_y_coeff[i],
                      "Plate solution Y coefficients" );
   }

}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsADssMap and astCheckDssMap functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(DssMap,Mapping)
astMAKE_CHECK(DssMap)

AstDssMap *astDssMap_( void *fits_void, const char *options, int *status, ...) {
/*
*+
*  Name:
*     astDssMap

*  Purpose:
*     Create a DssMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "dssmap.h"
*     AstDssMap *astDssMap( AstFitsChan *fits, const char *options, int *status, ... )

*  Class Membership:
*     DssMap constructor.

*  Description:
*     This function creates a new DssMap and optionally initialises its
*     attributes.
*
*     A DssMap is a Mapping which uses a Digitised Sky Survey plate fit to
*     transform a set of points from pixel coordinates to equatorial
*     coordinates (i.e. Right Ascension and Declination).

*  Parameters:
*     fits
*        A pointer to a FitsChan holding a set of FITS header cards
*        describing the plate fit to be used. The FitsChan may contain
*        other header cards which will be ignored, and it is unchanged on
*        exit. The required information is copied from the FitsChan, and
*        so the supplied FitsChan may subsequently be changed or deleted
*        without changing the DssMap.
*     options
*        Pointer to a null-terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new DssMap. The syntax used is identical to
*        that for the astSet function and may include "printf" format
*        specifiers identified by "%" symbols in the normal way.
*     status
*        Pointer to the inherited status variable.
*     ...
*        If the "options" string contains "%" format specifiers, then
*        an optional list of additional arguments may follow it in
*        order to supply values to be substituted for these
*        specifiers. The rules for supplying these are identical to
*        those for the astSet function (and for the C "printf"
*        function).

*  Returned Value:
*     astDssMap()
*        A pointer to the new DssMap.

*  Attributes:
*     The DssMap class has no additional attributes over and above those
*     common to all Mappings.

*  Notes:
*     - The supplied FitsChan must contain values for the following FITS
*     keywords: CNPIX1, CNPIX2, PPO3, PPO6, XPIXELSZ, YPIXELSZ, PLTRAH,
*     PLTRAM, PLTRAS, PLTDECD, PLTDECM, PLTDECS, PLTDECSN, PLTSCALE,
*     AMDX1, AMDX2, ..., AMDX13, AMDY1, AMDY2, ..., AMDY13.
*     - A null Object pointer (AST__NULL) will be returned if this
*     function is invoked with the AST error status set, or if it
*     should fail for any reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstFitsChan *fits;            /* Pointer to supplied FitsChan */
   AstDssMap *new;               /* Pointer to new DssMap */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   new = NULL;
   if ( !astOK ) return new;

/* Obtain and validate a pointer to the FitsChan structure provided. */
   fits = astCheckFitsChan( fits_void );
   if ( astOK ) {

/* Initialise the DssMap, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitDssMap( NULL, sizeof( AstDssMap ), !class_init, &class_vtab,
                           "DssMap", fits );

/* If successful, note that the virtual function table has been
   initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new DssMap's attributes. */
         va_start( args, status );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return a pointer to the new DssMap. */
   return new;
}

AstDssMap *astInitDssMap_( void *mem, size_t size, int init,
                           AstDssMapVtab *vtab, const char *name,
                           AstFitsChan *fits, int *status ) {
/*
*+
*  Name:
*     astInitDssMap

*  Purpose:
*     Initialise a DssMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "dssmap.h"
*     AstDssMap *astInitDssMap( void *mem, size_t size, int init,
*                               AstDssMapVtab *vtab, const char *name,
*                               AstFitsChan *fits )

*  Class Membership:
*     DssMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new DssMap object. It allocates memory (if necessary) to accommodate
*     the DssMap plus any additional data associated with the derived class.
*     It then initialises a DssMap structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a DssMap at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the DssMap is to be initialised.
*        This must be of sufficient size to accommodate the DssMap data
*        (sizeof(DssMap)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the DssMap (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the DssMap
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the DssMap's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new DssMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).
*     fits
*        Pointer to a FitsChan containing the DSS FITS Header.

*  Returned Value:
*     A pointer to the new DssMap.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstDssMap *new;              /* Pointer to new DssMap */
   struct WorldCoor *wcs;       /* Pointer to SAOIMAGE wcs structure */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitDssMapVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Create a structure holding the information required by the SAOIMAGE
   "platepos" function. The required values are extracted from the
   supplied FitsChan. An error is reported and NULL returned if any required
   keywords are missing or unusable. */
   if ( ( wcs = BuildWcs( fits, "astInitDssMap", name, status ) ) ) {

/* Initialise a 2-D Mapping structure (the parent class) as the first component
   within the DssMap structure, allocating memory if necessary. Specify that
   the Mapping should be defined in both the forward and inverse directions. */
      new = (AstDssMap *) astInitMapping( mem, size, 0,
                                          (AstMappingVtab *) vtab, name,
                                          2, 2, 1, 1 );

      if ( astOK ) {

/* Initialise the DssMap data. */
/* --------------------------- */
/* Store a copy of the SAOIMAGE wcs structure. */
         new->wcs = astStore( NULL, (void *) wcs, sizeof( struct WorldCoor ) );

/* If an error occurred, clean up by deleting the new DssMap. */
         if ( !astOK ) new = astDelete( new );
      }

/* Free the SAOIMAGE wcs structure. */
      wcs = (struct WorldCoor *) astFree( (void *) wcs );

   }

/* Return a pointer to the new DssMap. */
   return new;
}

AstDssMap *astLoadDssMap_( void *mem, size_t size,
                             AstDssMapVtab *vtab, const char *name,
                             AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadDssMap

*  Purpose:
*     Load a DssMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "dssmap.h"
*     AstDssMap *astLoadDssMap( void *mem, size_t size,
*                                 AstDssMapVtab *vtab, const char *name,
*                                 AstChannel *channel )

*  Class Membership:
*     DssMap loader.

*  Description:
*     This function is provided to load a new DssMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     DssMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a DssMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the DssMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        DssMap data (sizeof(DssMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the DssMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the DssMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstDssMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new DssMap. If this is NULL, a pointer
*        to the (static) virtual function table for the DssMap class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "DssMap" is used instead.

*  Returned Value:
*     A pointer to the new DssMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstDssMap *new;              /* Pointer to the new DssMap */
   char name_buff[ 11 ];        /* Buffer for item name */
   int i;                       /* Coefficient index */
   struct WorldCoor *wcs;       /* Pointer to Wcs information */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this DssMap. In this case the
   DssMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstDssMap );
      vtab = &class_vtab;
      name = "DssMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitDssMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built DssMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {


/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "DssMap" );

/* Allocate memory to hold the WorldCoor structure which holds the wcs
   information in a form usable by the SAOimage projection functions. */
      new->wcs = astMalloc( sizeof(struct WorldCoor) );
      if( astOK ) {

/* Get a pointer to the WorldCoor structure holding the description of the
   DssMap. */
         wcs = (struct WorldCoor *) ( new->wcs );

/* Read the values representing the contents of the WorldCoor structure.
   Only the components which are required to re-create the DssMap are
   read. */
         wcs->plate_ra = astReadDouble( channel, "pltra", AST__BAD );
         if( wcs->plate_ra == AST__BAD && astOK ){
            astError( AST__RDERR, "astRead(DssMap): 'PltRA' object (Plate "
                      "centre RA) missing from input." , status);
         }

         wcs->plate_dec = astReadDouble( channel, "pltdec", AST__BAD );
         if( wcs->plate_dec == AST__BAD && astOK ){
            astError( AST__RDERR, "astRead(DssMap): 'PltDec' object (Plate "
                      "centre Dec) missing from input." , status);
         }

         wcs->plate_scale = astReadDouble( channel, "pltscl", AST__BAD );
         if( wcs->plate_scale == AST__BAD && astOK ){
            astError( AST__RDERR, "astRead(DssMap): 'PltScl' object (Plate "
                      "scale) missing from input." , status);
         }

         wcs->x_pixel_offset = astReadDouble( channel, "cnpix1", AST__BAD );
         if( wcs->x_pixel_offset == AST__BAD && astOK ){
            astError( AST__RDERR, "astRead(DssMap): 'CNPix1' object (X pixel "
                      "offset) missing from input." , status);
         }

         wcs->y_pixel_offset = astReadDouble( channel, "cnpix2", AST__BAD );
         if( wcs->y_pixel_offset == AST__BAD && astOK ){
            astError( AST__RDERR, "astRead(DssMap): 'CNPix2' object (Y pixel "
                      "offset) missing from input." , status);
         }

         wcs->x_pixel_size = astReadDouble( channel, "xpixsz", AST__BAD );
         if( wcs->x_pixel_size == AST__BAD && astOK ){
            astError( AST__RDERR, "astRead(DssMap): 'XPixSz' object (X pixel "
                      "size) missing from input." , status);
         }

         wcs->y_pixel_size = astReadDouble( channel, "ypixsz", AST__BAD );
         if( wcs->y_pixel_size == AST__BAD && astOK ){
            astError( AST__RDERR, "astRead(DssMap): 'YPixSz' object (Y pixel "
                      "size) missing from input." , status);
         }

         for( i = 0; i < 6 && astOK; i++ ) {
            sprintf( name_buff, "ppo%d", i + 1 );
            wcs->ppo_coeff[i] = astReadDouble( channel, name_buff, AST__BAD );
            if( wcs->ppo_coeff[i] == AST__BAD ){
               if( i == 2 || i == 5 ) {
                  if( astOK ) astError( AST__RDERR, "astRead(DssMap): 'PPO%d' "
                                        "object (orientation coefficient %d) "
                                        "missing from input.", status, i + 1, i + 1 );
               } else {
                  wcs->ppo_coeff[i] = 0.0;
               }
            }
         }

         for( i = 0; i < 19 && astOK; i++ ) {
            sprintf( name_buff, "amdx%d", i + 1 );
            wcs->amd_x_coeff[i] = astReadDouble( channel, name_buff, AST__BAD );
            if( wcs->amd_x_coeff[i] == AST__BAD ){
               if( i < 13 ){
                  if( astOK ) astError( AST__RDERR, "astRead(DssMap): 'AMDX%d' "
                                        "object (plate solution X coefficient "
                                        "%d) missing from input.", status, i + 1, i + 1 );
               } else {
                  wcs->amd_x_coeff[i] = 0.0;
               }
            }
         }

         for( i = 0; i < 19 && astOK; i++ ) {
            sprintf( name_buff, "amdy%d", i + 1 );
            wcs->amd_y_coeff[i] = astReadDouble( channel, name_buff, AST__BAD );
            if( wcs->amd_y_coeff[i] == AST__BAD ){
               if( i < 13 ){
                  if( astOK ) astError( AST__RDERR, "astRead(DssMap): 'AMDY%d' "
                                        "object (plate solution Y coefficient "
                                        "%d) missing from input.", status, i + 1, i + 1 );
               } else {
                  wcs->amd_y_coeff[i] = 0.0;
               }
            }
         }
      }

/* If an error occurred, clean up by deleting the new DssMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new DssMap pointer. */
   return new;
}

/* Virtual function interfaces. */
/* ============================ */
/* These provide the external interface to the virtual functions defined by
   this class. Each simply checks the global error status and then locates and
   executes the appropriate member function, using the function pointer stored
   in the object's virtual function table (this pointer is located using the
   astMEMBER macro defined in "object.h").

   Note that the member function may not be the one defined here, as it may
   have been over-ridden by a derived class. However, it should still have the
   same interface. */
AstFitsChan *astDssFits_( AstDssMap *this, int *status ){
   if( !astOK ) return NULL;
   return (**astMEMBER(this,DssMap,DssFits))( this, status );
}

/* The code which follows in this file is covered by the following
   statement of terms and conditions, which differ from the terms and
   conditions which apply above.

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

/* >>>>>>>>>>>>>>>>>>>>>>  platepos.c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< */

/* File saoimage/wcslib/platepos.c
 * February 25, 1996
 * By Doug Mink, Harvard-Smithsonian Center for Astrophysics

 * Module:	platepos.c (Plate solution WCS conversion
 * Purpose:	Compute WCS from Digital Sky Survey plate fit
 * Subroutine:	platepos() converts from pixel location to RA,Dec
 * Subroutine:	platepix() converts from RA,Dec to pixel location

    These functions are based on the astrmcal.c portion of GETIMAGE by
    J. Doggett and the documentation distributed with the Digital Sky Survey.

    >>>>>>>   STARLINK VERSION <<<<<<<<

*/

/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  Changed by R.F. Warren-Smith (Starlink) to make the function static. */

static int
platepos (xpix, ypix, wcs, xpos, ypos)

/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/* Routine to determine accurate position for pixel coordinates */
/* returns 0 if successful otherwise 1 = angle too large for projection; */
/* based on amdpos() from getimage */

/* Input: */
double	xpix;		/* x pixel number  (RA or long without rotation) */
double	ypix;		/* y pixel number  (dec or lat without rotation) */
struct WorldCoor *wcs;	/* WCS parameter structure */

/* Output: */
double	*xpos;		/* Right ascension or longitude in degrees */
double	*ypos;		/* Declination or latitude in degrees */

{
  double x, y, xmm, ymm, xmm2, ymm2, xmm3, ymm3, x2y2;
  double xi, xir, eta, etar, raoff, ra, dec;
  double cond2r = 1.745329252e-2;
  double cons2r = 206264.8062470964;
  double twopi = 6.28318530717959;
  double ctan, ccos;

/*  Ignore magnitude and color terms
  double mag = 0.0;
  double color = 0.0; */

/* Convert from image pixels to plate pixels */
  x = xpix + wcs->x_pixel_offset - 1.0 + 0.5;
  y = ypix + wcs->y_pixel_offset - 1.0 + 0.5;

/* Convert from pixels to millimeters */
  xmm = (wcs->ppo_coeff[2] - x * wcs->x_pixel_size) / 1000.0;
  ymm = (y * wcs->y_pixel_size - wcs->ppo_coeff[5]) / 1000.0;
  xmm2 = xmm * xmm;
  ymm2 = ymm * ymm;
  xmm3 = xmm * xmm2;
  ymm3 = ymm * ymm2;
  x2y2 = xmm2 + ymm2;

/*  Compute coordinates from x,y and plate model */

  xi =  wcs->amd_x_coeff[ 0]*xmm	+ wcs->amd_x_coeff[ 1]*ymm +
	wcs->amd_x_coeff[ 2]		+ wcs->amd_x_coeff[ 3]*xmm2 +
	wcs->amd_x_coeff[ 4]*xmm*ymm	+ wcs->amd_x_coeff[ 5]*ymm2 +
	wcs->amd_x_coeff[ 6]*(x2y2)	+ wcs->amd_x_coeff[ 7]*xmm3 +
	wcs->amd_x_coeff[ 8]*xmm2*ymm	+ wcs->amd_x_coeff[ 9]*xmm*ymm2 +
	wcs->amd_x_coeff[10]*ymm3	+ wcs->amd_x_coeff[11]*xmm*(x2y2) +
	wcs->amd_x_coeff[12]*xmm*x2y2*x2y2;

/*  Ignore magnitude and color terms
	+ wcs->amd_x_coeff[13]*mag	+ wcs->amd_x_coeff[14]*mag*mag +
	wcs->amd_x_coeff[15]*mag*mag*mag + wcs->amd_x_coeff[16]*mag*xmm +
	wcs->amd_x_coeff[17]*mag*x2y2	+ wcs->amd_x_coeff[18]*mag*xmm*x2y2 +
	wcs->amd_x_coeff[19]*color; */

  eta =	wcs->amd_y_coeff[ 0]*ymm	+ wcs->amd_y_coeff[ 1]*xmm +
	wcs->amd_y_coeff[ 2]		+ wcs->amd_y_coeff[ 3]*ymm2 +
	wcs->amd_y_coeff[ 4]*xmm*ymm	+ wcs->amd_y_coeff[ 5]*xmm2 +
	wcs->amd_y_coeff[ 6]*(x2y2)	+ wcs->amd_y_coeff[ 7]*ymm3 +
	wcs->amd_y_coeff[ 8]*ymm2*xmm	+ wcs->amd_y_coeff[ 9]*ymm*xmm2 +
	wcs->amd_y_coeff[10]*xmm3	+ wcs->amd_y_coeff[11]*ymm*(x2y2) +
	wcs->amd_y_coeff[12]*ymm*x2y2*x2y2;

/*  Ignore magnitude and color terms
	+ wcs->amd_y_coeff[13]*mag	+ wcs->amd_y_coeff[14]*mag*mag +
	wcs->amd_y_coeff[15]*mag*mag*mag + wcs->amd_y_coeff[16]*mag*ymm +
	wcs->amd_y_coeff[17]*mag*x2y2)	+ wcs->amd_y_coeff[18]*mag*ymm*x2y2 +
	wcs->amd_y_coeff[19]*color; */

/* Convert to radians */

  xir = xi / cons2r;
  etar = eta / cons2r;

/* Convert to RA and Dec */

  ctan = tan (wcs->plate_dec);
  ccos = cos (wcs->plate_dec);
  raoff = atan2 (xir / ccos, 1.0 - etar * ctan);
  ra = raoff + wcs->plate_ra;
  if (ra < 0.0) ra = ra + twopi;
  *xpos = ra / cond2r;

  dec = atan (cos (raoff) / ((1.0 - (etar * ctan)) / (etar + ctan)));
  *ypos = dec / cond2r;
  return 0;
}


/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  Changed by R.F. Warren-Smith (Starlink) to make the function static. */

static int
platepix (xpos, ypos, wcs, xpix, ypix)

/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

/* Routine to determine pixel coordinates for sky position */
/* returns 0 if successful otherwise 1 = angle too large for projection; */
/* based on amdinv() from getimage */

/* Input: */
double	xpos;		/* Right ascension or longitude in degrees */
double	ypos;		/* Declination or latitude in degrees */
struct WorldCoor *wcs;	/* WCS parameter structure */

/* Output: */
double	*xpix;		/* x pixel number  (RA or long without rotation) */
double	*ypix;		/* y pixel number  (dec or lat without rotation) */

{
  double div,xi,eta,x,y,xy,x2,y2,x2y,y2x,x3,y3,x4,y4,x2y2,cjunk,dx,dy;
  double sypos,cypos,syplate,cyplate,sxdiff,cxdiff;
  double f,fx,fy,g,gx,gy, xmm, ymm;
  double conr2s = 206264.8062470964;
  double tolerance = 0.0000005;
  int    max_iterations = 50;
  int    i;
  double xr, yr; 	/* position in radians */
  double cond2r = 1.745329252e-2;

/* Convert RA and Dec in radians to standard coordinates on a plate */
  xr = xpos * cond2r;
  yr = ypos * cond2r;
  sypos = sin (yr);
  cypos = cos (yr);
  syplate = sin (wcs->plate_dec);
  cyplate = cos (wcs->plate_dec);
  sxdiff = sin (xr - wcs->plate_ra);
  cxdiff = cos (xr - wcs->plate_ra);
  div = (sypos * syplate) + (cypos * cyplate * cxdiff);
  xi = cypos * sxdiff * conr2s / div;
  eta = ((sypos * cyplate) - (cypos * syplate * cxdiff)) * conr2s / div;

/* Set initial value for x,y */
  xmm = xi / wcs->plate_scale;
  ymm = eta / wcs->plate_scale;

/* Iterate by Newton's method */
  for (i = 0; i < max_iterations; i++) {

    /* X plate model */
    xy = xmm * ymm;
    x2 = xmm * xmm;
    y2 = ymm * ymm;
    x2y = x2 * ymm;
    y2x = y2 * xmm;
    x2y2 = x2 + y2;
    cjunk = x2y2 * x2y2;
    x3 = x2 * xmm;
    y3 = y2 * ymm;
    x4 = x2 * x2;
    y4 = y2 * y2;
    f = wcs->amd_x_coeff[0]*xmm      + wcs->amd_x_coeff[1]*ymm +
        wcs->amd_x_coeff[2]          + wcs->amd_x_coeff[3]*x2 +
        wcs->amd_x_coeff[4]*xy       + wcs->amd_x_coeff[5]*y2 +
        wcs->amd_x_coeff[6]*x2y2     + wcs->amd_x_coeff[7]*x3 +
        wcs->amd_x_coeff[8]*x2y      + wcs->amd_x_coeff[9]*y2x +
        wcs->amd_x_coeff[10]*y3      + wcs->amd_x_coeff[11]*xmm*x2y2 +
        wcs->amd_x_coeff[12]*xmm*cjunk;
    /* magnitude and color terms ignored
      + wcs->amd_x_coeff[13]*mag +
        wcs->amd_x_coeff[14]*mag*mag   + wcs->amd_x_coeff[15]*mag*mag*mag +
        wcs->amd_x_coeff[16]*mag*xmm   + wcs->amd_x_coeff[17]*mag*(x2+y2) +
        wcs->amd_x_coeff[18]*mag*xmm*(x2+y2)  + wcs->amd_x_coeff[19]*color;
    */

    /*  Derivative of X model wrt x */
    fx = wcs->amd_x_coeff[0]           + wcs->amd_x_coeff[3]*2.0*xmm +
         wcs->amd_x_coeff[4]*ymm       + wcs->amd_x_coeff[6]*2.0*xmm +
         wcs->amd_x_coeff[7]*3.0*x2    + wcs->amd_x_coeff[8]*2.0*xy +
         wcs->amd_x_coeff[9]*y2        + wcs->amd_x_coeff[11]*(3.0*x2+y2) +
         wcs->amd_x_coeff[12]*(5.0*x4 +6.0*x2*y2+y4);
    /* magnitude and color terms ignored
         wcs->amd_x_coeff[16]*mag      + wcs->amd_x_coeff[17]*mag*2.0*xmm +
         wcs->amd_x_coeff[18]*mag*(3.0*x2+y2);
    */

    /* Derivative of X model wrt y */
    fy = wcs->amd_x_coeff[1]           + wcs->amd_x_coeff[4]*xmm +
         wcs->amd_x_coeff[5]*2.0*ymm   + wcs->amd_x_coeff[6]*2.0*ymm +
         wcs->amd_x_coeff[8]*x2        + wcs->amd_x_coeff[9]*2.0*xy +
         wcs->amd_x_coeff[10]*3.0*y2   + wcs->amd_x_coeff[11]*2.0*xy +
         wcs->amd_x_coeff[12]*4.0*xy*x2y2;
    /* magnitude and color terms ignored
         wcs->amd_x_coeff[17]*mag*2.0*ymm +
         wcs->amd_x_coeff[18]*mag*2.0*xy;
    */

    /* Y plate model */
    g = wcs->amd_y_coeff[0]*ymm       + wcs->amd_y_coeff[1]*xmm +
       wcs->amd_y_coeff[2]            + wcs->amd_y_coeff[3]*y2 +
       wcs->amd_y_coeff[4]*xy         + wcs->amd_y_coeff[5]*x2 +
       wcs->amd_y_coeff[6]*x2y2       + wcs->amd_y_coeff[7]*y3 +
       wcs->amd_y_coeff[8]*y2x        + wcs->amd_y_coeff[9]*x2y +
       wcs->amd_y_coeff[10]*x3        + wcs->amd_y_coeff[11]*ymm*x2y2 +
       wcs->amd_y_coeff[12]*ymm*cjunk;
    /* magnitude and color terms ignored
       wcs->amd_y_coeff[13]*mag        + wcs->amd_y_coeff[14]*mag*mag +
       wcs->amd_y_coeff[15]*mag*mag*mag + wcs->amd_y_coeff[16]*mag*ymm +
       wcs->amd_y_coeff[17]*mag*x2y2 +
       wcs->amd_y_coeff[18]*mag*ymm*x2y2 + wcs->amd_y_coeff[19]*color;
    */

    /* Derivative of Y model wrt x */
    gx = wcs->amd_y_coeff[1]           + wcs->amd_y_coeff[4]*ymm +
         wcs->amd_y_coeff[5]*2.0*xmm   + wcs->amd_y_coeff[6]*2.0*xmm +
         wcs->amd_y_coeff[8]*y2       + wcs->amd_y_coeff[9]*2.0*xy +
         wcs->amd_y_coeff[10]*3.0*x2  + wcs->amd_y_coeff[11]*2.0*xy +
         wcs->amd_y_coeff[12]*4.0*xy*x2y2;
    /* magnitude and color terms ignored
         wcs->amd_y_coeff[17]*mag*2.0*xmm +
         wcs->amd_y_coeff[18]*mag*ymm*2.0*xmm;
    */

    /* Derivative of Y model wrt y */
    gy = wcs->amd_y_coeff[0]            + wcs->amd_y_coeff[3]*2.0*ymm +
         wcs->amd_y_coeff[4]*xmm        + wcs->amd_y_coeff[6]*2.0*ymm +
         wcs->amd_y_coeff[7]*3.0*y2     + wcs->amd_y_coeff[8]*2.0*xy +
         wcs->amd_y_coeff[9]*x2         + wcs->amd_y_coeff[11]*(x2+3.0*y2) +
         wcs->amd_y_coeff[12]*(5.0*y4 + 6.0*x2*y2 + x4);
    /* magnitude and color terms ignored
         wcs->amd_y_coeff[16]*mag       + wcs->amd_y_coeff[17]*mag*2.0*ymm +
         wcs->amd_y_coeff[18]*mag*(x2+3.0*y2);
    */

    f = f - xi;
    g = g - eta;
    dx = ((-f * gy) + (g * fy)) / ((fx * gy) - (fy * gx));
    dy = ((-g * fx) + (f * gx)) / ((fx * gy) - (fy * gx));
    xmm = xmm + dx;
    ymm = ymm + dy;
    if ((fabs(dx) < tolerance) && (fabs(dy) < tolerance)) break;
    }

/* Convert mm from plate center to plate pixels */
  x = (wcs->ppo_coeff[2] - xmm*1000.0) / wcs->x_pixel_size;
  y = (wcs->ppo_coeff[5] + ymm*1000.0) / wcs->y_pixel_size;

/* Convert from plate pixels to image pixels */
  *xpix = x - wcs->x_pixel_offset + 1.0 - 0.5;
  *ypix = y - wcs->y_pixel_offset + 1.0 - 0.5;

/* If position is off of the image, return offscale code */

/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  Commented out by D.Berry (Starlink) in order to remove dependancy
  on NAXIS1/NAXIS2 keywords >>>>>>>>

  if (*xpix < 0.5 || *xpix > wcs->nxpix+0.5)
    return -1;
  if (*ypix < 0.5 || *ypix > wcs->nypix+0.5)
    return -1;

>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

  return 0;
}
/* Mar  6 1995	Original version of this code
   May  4 1995	Fix eta cross terms which were all in y
   Jun 21 1995	Add inverse routine
   Oct 17 1995	Fix inverse routine (degrees -> radians)
   Nov  7 1995	Add half pixel to image coordinates to get astrometric
                  plate coordinates
   Feb 26 1996	Fix plate to image pixel conversion error
   Feb 18 1997  Modified by D.S. Berry (Starlink) to avoid use of the image
                  dimensions stored in wcs->nxpix and wcs->nypix.
   Sep  5 1997  Modified by R.F. Warren-Smith (Starlink) to make the
                platepos and platepix functions static.
 */




