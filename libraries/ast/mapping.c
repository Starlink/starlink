#define REBIN 1
/*
*class++
*  Name:
*     Mapping

*  Purpose:
*     Inter-relate two coordinate systems.

*  Constructor Function:
*     None.

*  Description:
*     This class provides the basic facilities for transforming a set
*     of coordinates (representing "input" points) to give a new set
*     of coordinates (representing "output" points).  It is used to
*     describe the relationship which exists between two different
*     coordinate systems.  However, the Mapping class does not have a
*     constructor function of its own, as it is simply a container
*     class for a family of specialised Mappings which implement
*     particular types of coordinate transformation.

*  Inheritance:
*     The Mapping class inherits from the Object class.

*  Attributes:
*     In addition to those attributes common to all Objects, every
*     Mapping also has the following attributes:
*
*     - Invert: Mapping inversion flag
*     - Nin: Number of input coordinates for a Mapping
*     - Nout: Number of output coordinates for a Mapping
*     - Report: Report transformed coordinates?
*     - TranForward: Forward transformation defined?
*     - TranInverse: Inverse transformation defined?

*  Functions:
c     In addition to those functions applicable to all Objects, the
c     following functions may also be applied to all Mappings:
f     In addition to those routines applicable to all Objects, the
f     following routines may also be applied to all Mappings:
*
c     - astInvert: Invert a Mapping
c     - astMapBox: Find a bounding box for a Mapping
c     - astSimplify: Simplify a Mapping
c     - astTran1: Transform 1-dimensional coordinates
c     - astTran2: Transform 2-dimensional coordinates
c     - astTranN: Transform N-dimensional coordinates
c     - astTranP: Transform N-dimensional coordinates held in separate arrays
f     - AST_INVERT: Invert a Mapping
f     - AST_MAPBOX: Find a bounding box for a Mapping
f     - AST_SIMPLIFY: Simplify a Mapping
f     - AST_TRAN1: Transform 1-dimensional coordinates
f     - AST_TRAN2: Transform 2-dimensional coordinates
f     - AST_TRANN: Transform N-dimensional coordinates

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     1-FEB-1996 (RFWS):
*        Original version.
*     29-FEB-1996 (RFWS):
*        Minor improvements to error messages.
*     15-JUL-1996 (RFWS):
*        Support external interface.
*     13-DEC-1996 (RFWS):
*        Added the astMapMerge method.
*     13-DEC-1996 (RFWS):
*        Added the astSimplify method.
*     27-MAY-1997 (RFWS):
*        Improved the astSimplify method to use astMapMerge to
*        simplify a single Mapping where possible.
*     29-MAY-1998 (RFWS):
*        Added the MapBox method.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to the header
   files that define class interfaces that they should make "protected"
   symbols available. */
#define astCLASS Mapping

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "channel.h"             /* I/O channels */
#include "mapping.h"             /* Interface definition for this class */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Module type definitions. */
/* ======================== */
/* Data structure to hold information about a Mapping for use by
   optimisation algorithms. */
typedef struct MapData {
   AstMapping *mapping;          /* Pointer to the Mapping */
   AstPointSet *pset_in;         /* Pointer to input PointSet */
   AstPointSet *pset_out;        /* Pointer to output PointSet */
   double *lbnd;                 /* Pointer to lower constraints on input */
   double *ubnd;                 /* Pointer to upper constraints on input */
   double **ptr_in;              /* Pointer to input PointSet coordinates */
   double **ptr_out;             /* Pointer to output PointSet coordinates */
   int coord;                    /* Index of output coordinate to optimise */
   int forward;                  /* Use forward transformation? */
   int negate;                   /* Negate the output value? */
   int nin;                      /* Number of input coordinates per point */
   int nout;                     /* Number of output coordinates per point */
} MapData;

/* Module Variables. */
/* ================= */
/* Define the class virtual function table and its initialisation flag as
   static variables. */
static AstMappingVtab class_vtab; /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

/* Pointers to parent class methods which are extended by this class. */
static const char *(* parent_getattrib)( AstObject *, const char * );
static int (* parent_testattrib)( AstObject *, const char * );
static void (* parent_clearattrib)( AstObject *, const char * );
static void (* parent_setattrib)( AstObject *, const char * );

/* Prototypes for private member functions. */
/* ======================================== */
static AstMapping *Simplify( AstMapping * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet * );
static const char *GetAttrib( AstObject *, const char * );
static double LocalMaximum( const MapData *, double, double, double [] );
static double MapFunction( const MapData *, const double [], int * );
static double NewVertex( const MapData *, int, double, double [], double [], int *, double [] );
static double Random( long int * );
static double UphillSimplex( const MapData *, double, int, const double [], double [], double *, int * );
static int GetInvert( AstMapping * );
static int GetNin( AstMapping * );
static int GetNout( AstMapping * );
static int GetReport( AstMapping * );
static int GetTranForward( AstMapping * );
static int GetTranInverse( AstMapping * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int ** );
static int TestAttrib( AstObject *, const char * );
static int TestInvert( AstMapping * );
static int TestReport( AstMapping * );
static void ClearAttrib( AstObject *, const char * );
static void ClearInvert( AstMapping * );
static void ClearReport( AstMapping * );
static void Copy( const AstObject *, AstObject * );
static void Delete( AstObject * );
static void Dump( AstObject *, AstChannel * );
static void GlobalBounds( MapData *, double *, double *, double [], double [] );
static void InitVtab( AstMappingVtab * );
static void Invert( AstMapping * );
static void MapBox( AstMapping *, const double [], const double [], int, int, double *, double *, double [], double [] );
static void MapList( AstMapping *, int, int, int *, AstMapping ***, int ** );
static void ReportPoints( AstMapping *, int, AstPointSet *, AstPointSet * );
static void SetAttrib( AstObject *, const char * );
static void SetInvert( AstMapping *, int );
static void SetReport( AstMapping *, int );
static void SpecialBounds( const MapData *, double *, double *, double [], double [] );
static void Tran1( AstMapping *, int, const double [], int, double [] );
static void Tran2( AstMapping *, int, const double [], const double [], int, double [], double [] );
static void TranN( AstMapping *, int, int, int, const double (*)[], int, int, int, double (*)[] );
static void TranP( AstMapping *, int, int, const double *[], int, int, double *[] );
static void ValidateMapping( AstMapping *, int, int, int, int, const char * );

/* Member functions. */
/* ================= */
#if REBIN
typedef enum DataType {
   LDOUBLE,
   DOUBLE,
   FLOAT,
   LONG,
   ULONG,
   INT,
   UINT,
   SHORT,
   USHORT,
   BYTE,
   UBYTE
} DataType;

#define MAKE_INTERPOLATE_PIXEL_NEAREST(abbrev,type) \
static int InterpolatePixelNearest##abbrev( int ndim, \
                                            const int *lbnd, const int *ubnd, \
                                            const type *in, \
                                            int npoint, const int *offset, \
                                            double *coord, \
                                            int usebad, double badflag, \
                                            type *out ) { \
\
/* Local Variables: */ \
   double *xn_max;               /* Pointer to upper limits array (n-d) */ \
   double *xn_min;               /* Pointer to lower limits array (n-d) */ \
   double x;                     /* x coordinate value */ \
   double xmax;                  /* x upper limit */ \
   double xmin;                  /* x lower limit */ \
   double xn;                    /* Coordinate value (n-d) */ \
   double y;                     /* y coordinate value */ \
   double ymax;                  /* y upper limit */ \
   double ymin;                  /* y lower limit */ \
   int *stride;                  /* Pointer to array of dimension strides */ \
   int bad;                      /* Output pixel bad? */ \
   int idim;                     /* Loop counter for dimensions */ \
   int ix;                       /* Number of pixel offset in x direction */ \
   int ixn;                      /* Number of pixels offset (n-d) */ \
   int iy;                       /* Number of pixel offset in y directyion */ \
   int off_in;                   /* Pixel offset into input array */ \
   int point;                    /* Loop counter for output points */ \
   int result;                   /* Returned result value */ \
   int s;                        /* Temporary variable for strides */ \
   int ystride;                  /* Stride along input array y direction */ \
\
/* Initialise. */ \
   result = 0; \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Handle the 1-dimensional case optimally. */ \
/* ---------------------------------------- */ \
   if ( ndim == 1 ) { \
\
/* Calculate the coordinate limits of the input array. */ \
      xmin = ( (double) lbnd[ 0 ] ) - 0.5; \
      xmax = ( (double) ubnd[ 0 ] ) + 0.5; \
\
/* Loop through the list of output points. */ \
      for ( point = 0; point < npoint; point++ ) { \
\
/* Obtain the x coordinate of the current point and test if it lies \
   outside the input image, or is bad. */ \
         x = coord[ point ]; \
         bad = ( x < xmin ) || ( x > xmax ) || ( x == AST__BAD ); \
         if ( !bad ) { \
\
/* If not, then obtain the offset within the input image of the pixel \
   which contains the current point. */ \
            off_in = ( (int) floor( x + 0.5 ) ) - lbnd[ 0 ]; \
\
/* If necessary, test if the input pixel is bad. */ \
            bad = ( in[ off_in ] == badflag ) && usebad; \
         } \
\
/* If the output pixel is not bad, obtain its value from the input \
   image. */ \
         if ( !bad ) { \
            out[ offset[ point ] ] = in[ off_in ]; \
\
/* Otherwise, assign a bad output pixel and count it. */ \
         } else { \
            out[ offset[ point ] ] = badflag; \
            result++; \
         } \
      } \
\
/* Handle the 2-dimensional case optimally. */ \
/* ---------------------------------------- */ \
   } else if ( ndim == 2 ) { \
\
/* Calculate the input array stride along the y direction. */ \
      ystride = ubnd[ 0 ] - lbnd[ 0 ] + 1; \
\
/* Calculate the coordinate limits of the input array in each \
   dimension. */ \
      xmin = ( (double) lbnd[ 0 ] ) - 0.5; \
      xmax = ( (double) ubnd[ 0 ] ) + 0.5; \
      ymin = ( (double) lbnd[ 1 ] ) - 0.5; \
      ymax = ( (double) ubnd[ 1 ] ) + 0.5; \
\
/* Loop through the list of output points. */ \
      for ( point = 0; point < npoint; point++ ) { \
\
/* Obtain the x coordinate of the current point and test if it lies \
   outside the input image, or is bad. */ \
         x = coord[ point ]; \
         bad = ( x < xmin ) || ( x > xmax ) || ( x == AST__BAD ); \
         if ( !bad ) { \
\
/* If not, then similarly obtain and test the y coordinate. */ \
            y = coord[ npoint + point ]; \
            bad = ( y < ymin ) || ( y > ymax ) || ( y == AST__BAD ); \
            if ( !bad ) { \
\
/* Obtain the offsets along each input image dimension of the input \
   pixel which contains the current point. */ \
               ix = ( (int) floor( x + 0.5 ) ) - lbnd[ 0 ]; \
               iy = ( (int) floor( y + 0.5 ) ) - lbnd[ 1 ]; \
\
/* Accumulate this pixel's offset from the start of the input \
   array. */ \
               off_in = iy * ystride + ix; \
\
/* If necessary, test if the input pixel is bad. */ \
               bad = ( in[ off_in ] == badflag ) && usebad; \
            } \
         } \
\
/* If the output pixel is not bad, obtain its value from the input \
   image. */ \
         if ( !bad ) { \
            out[ offset[ point ] ] = in[ off_in ]; \
\
/* Otherwise, assign a bad output pixel and count it. */ \
         } else { \
            out[ offset[ point ] ] = badflag; \
            result++; \
         } \
      } \
\
/* Handle other numbers of dimensions. */ \
/* ----------------------------------- */ \
   } else { \
\
/* Allocate workspace. */ \
      stride = astMalloc( sizeof( int ) * (size_t) ndim ); \
      xn_max = astMalloc( sizeof( double ) * (size_t) ndim ); \
      xn_min = astMalloc( sizeof( double ) * (size_t) ndim ); \
      if ( astOK ) { \
\
/* Calculate the input array stride along each dimension. */ \
         for ( s = 1, idim = 0; idim < ndim; idim++ ) { \
            stride[ idim ] = s; \
            s *= ubnd[ idim ] - lbnd[ idim ] + 1; \
\
/* Calculate the coordinate limits of the input array in each \
   dimension. */ \
            xn_min[ idim ] = ( (double) lbnd[ idim ] ) - 0.5; \
            xn_max[ idim ] = ( (double) ubnd[ idim ] ) + 0.5; \
         } \
\
/* Loop through the list of output points. */ \
         for ( point = 0; point < npoint; point++ ) { \
\
/* Initialise the offset into the input image. Then loop to obtain \
   each coordinate of the current output point. */ \
            off_in = 0; \
            for ( idim = 0; idim < ndim; idim++ ) { \
               xn = coord[ idim * npoint + point ]; \
\
/* Test if the coordinate lies outside the input image, or is bad.  If \
   either is true, the corresponding output pixel value will be bad, \
   so give up on this point. */ \
               bad = ( xn < xn_min[ idim ] ) || ( xn > xn_max[ idim ] ) || \
                     ( xn == AST__BAD ); \
               if ( bad ) break; \
\
/* Obtain the offset along the current input image dimension of the \
   input pixel which contains the current point. */ \
               ixn = ( (int) floor( xn + 0.5 ) ) - lbnd[ idim ]; \
\
/* Accumulate this pixel's offset from the start of the input \
   array. */ \
               off_in += ixn * stride[ idim ]; \
            } \
\
/* Once the required input pixel has been located, test if it is bad \
   if necessary. */ \
            bad = bad || ( ( in[ off_in ] == badflag ) && usebad ); \
\
/* If the output pixel is not bad, obtain its value from the input \
   image. */ \
            if ( !bad ) { \
               out[ offset[ point ] ] = in[ off_in ]; \
\
/* Otherwise, assign a bad output pixel and count it. */ \
            } else { \
               out[ offset[ point ] ] = badflag; \
               result++; \
            } \
         } \
      } \
\
/* Free workspace. */ \
      stride = astFree( stride ); \
      xn_max = astFree( xn_max ); \
      xn_min = astFree( xn_min ); \
   } \
\
/* If an error has occurred, clear the returned result. */ \
   if ( !astOK ) result = 0; \
\
/* Return the result. */ \
   return result; \
}
MAKE_INTERPOLATE_PIXEL_NEAREST(LD,long double)
MAKE_INTERPOLATE_PIXEL_NEAREST(D,double)
MAKE_INTERPOLATE_PIXEL_NEAREST(F,float)
MAKE_INTERPOLATE_PIXEL_NEAREST(L,long int)
MAKE_INTERPOLATE_PIXEL_NEAREST(UL,unsigned long int)
MAKE_INTERPOLATE_PIXEL_NEAREST(I,int)
MAKE_INTERPOLATE_PIXEL_NEAREST(UI,unsigned int)
MAKE_INTERPOLATE_PIXEL_NEAREST(S,short int)
MAKE_INTERPOLATE_PIXEL_NEAREST(US,unsigned short int)
MAKE_INTERPOLATE_PIXEL_NEAREST(B,signed char)
MAKE_INTERPOLATE_PIXEL_NEAREST(UB,unsigned char)
#undef MAKE_INTERPOLATE_PIXEL_NEAREST

#define MAKE_INTERPOLATE_PIXEL_LINEAR(abbrev,type,is_flt,flt_type) \
static int InterpolatePixelLinear##abbrev( int ndim, \
                                           const int *lbnd, const int *ubnd, \
                                           const type *in, \
                                           int npoint, const int *offset, \
                                           double *coord, \
                                           int usebad, type badflag, \
                                           type *out ) { \
   double *frac_hi; \
   double *frac_lo; \
   double *wt; \
   double dx; \
   double pixwt; \
   double x; \
   flt_type sum; \
   flt_type val; \
   flt_type wtsum; \
   int *hi; \
   int *idim; \
   int *lo; \
   int *stride; \
   int bad; \
   int dim; \
   int done; \
   int ix; \
   int off; \
   int pixel; \
   int point; \
   int result; \
   int s; \
 \
   result = 0; \
   if ( !astOK ) return result; \
 \
/* Allocate workspace. */ \
   frac_hi = astMalloc( sizeof( double ) * (size_t) ndim ); \
   frac_lo = astMalloc( sizeof( double ) * (size_t) ndim ); \
   hi = astMalloc( sizeof( int ) * (size_t) ndim ); \
   idim = astMalloc( sizeof( int ) * (size_t) ndim ); \
   lo = astMalloc( sizeof( int ) * (size_t) ndim ); \
   stride = astMalloc( sizeof( int ) * (size_t) ndim ); \
   wt = astMalloc( sizeof( double ) * (size_t) ndim ); \
 \
/* Calculate the input pixel stride for each dimension. */ \
   if ( astOK ) { \
      s = 1; \
      for ( dim = 0; dim < ndim; dim++ ) { \
         stride[ dim ] = s; \
         s *= ubnd[ dim ] - lbnd[ dim ] + 1; \
      } \
 \
/* Loop through the output points (i.e. pixel centres). */ \
      for ( point = 0; point < npoint; point++ ) { \
 \
/* Initialise the offset into the input image. Then examine each \
   coordinate from the corresponding input point in turn. */ \
         pixel = 0; \
         off = 0; \
         for ( dim = 0; dim < ndim; dim++ ) { \
            x = coord[ dim * npoint + point ]; \
 \
/* Test if the coordinate lies outside the input image, or is bad. If \
   either is true, the corresponding output pixel value will be bad, \
   so quit. */ \
            bad = ( x < ( ( (double) lbnd[ dim ] ) ) - 0.5 ) || \
                  ( x > ( ( (double) ubnd[ dim ] ) ) + 0.5 ) || \
                  ( x == AST__BAD ); \
            if ( bad ) break; \
 \
/* Obtain the index along the current input image dimension of the \
   pixel which contains the input point. */ \
            ix = (int) floor( x + 0.5 ); \
 \
/* Accumulate this pixel's offset (in pixels) from the start of the \
   input image. */ \
            pixel += ( ix - lbnd[ dim ] ) * stride[ dim ]; \
 \
/* Calculate the offset of the input position from the pixel's centre. */ \
            dx = x - (double) ix; \
 \
/* Test if the position lies below (or on) the pixel's centre. */ \
            if ( dx <= 0.0 ) { \
 \
/* If so, obtain the offsets from the start of the image (due to \
   displacement along the current dimension) of the two pixels which \
   will contribute to the output value. If necessary, restrict the \
   pixel with the lower index to ensure it does not lie outside the \
   input image. */ \
               hi[ dim ] = ix; \
               frac_hi[ dim ] = 1.0 + dx; \
               if ( ix > lbnd[ dim ] ) { \
                  lo[ dim ] = ix - 1; \
                  frac_lo[ dim ] = -dx; \
               } else { \
                  lo[ dim ] = hi[ dim ]; \
                  frac_lo[ dim ] = frac_hi[ dim ]; \
               } \
                \
/* If the input position lies above the pixel's centre, repeat the \
   above process, this time restricting the pixel with the larger \
   index if necessary. */ \
            } else { \
               lo[ dim ] = ix; \
               frac_lo[ dim ] = 1.0 - dx; \
               if ( ix < ubnd[ dim ] ) { \
                  hi[ dim ] = ix + 1; \
                  frac_hi[ dim ] = dx; \
               } else { \
                  hi[ dim ] = lo[ dim ]; \
                  frac_hi[ dim ] = frac_lo[ dim ]; \
               } \
            } \
 \
/* Store the lower index involved in interpolation along each \
   dimension and accumulate the offset from the start of the image of \
   the pixel which has these indices. */ \
 \
            idim[ dim ] = lo[ dim ]; \
            off += ( lo[ dim ] - lbnd[ dim ] ) * stride[ dim ]; \
 \
/* Also store the fractional weight associated with the lower pixel \
   along each dimension. */ \
            wt[ dim ] = frac_lo[ dim ]; \
         } \
 \
         bad = bad || ( ( in[ pixel ] == badflag ) && usebad ); \
        \
         if ( bad ) { \
            out[ offset[ point ] ] = badflag; \
            result++; \
 \
         } else { \
            done = 0; \
            sum = (flt_type) 0.0; \
            wtsum = (flt_type) 0.0; \
            while ( !done ) { \
               if ( ( in[ off ] != badflag ) || !usebad ) { \
                  for ( pixwt = 1.0, dim = 0; dim < ndim; dim++ ) { \
                     pixwt *= wt[ dim ]; \
                  } \
                  sum += ( (flt_type) pixwt ) * ( (flt_type) in[ off ] ); \
                  wtsum += pixwt; \
               } \
               dim = 0; \
               while ( !done ) { \
                  if ( idim[ dim ] != hi[ dim ] ) { \
                     idim[ dim ] = hi[ dim ]; \
                     off += stride[ dim ]; \
                     wt[ dim ] = frac_hi[ dim ]; \
                     break; \
                  } else { \
                     idim[ dim ] = lo[ dim ]; \
                     off -= stride[ dim ]; \
                     wt[ dim ] = frac_lo[ dim ]; \
                     done = ( ++dim == ndim ); \
                  } \
               } \
            } \
            if ( is_flt ) { \
               out[ offset[ point ] ] = (type) ( sum / wtsum ); \
            } else { \
               val = sum / wtsum; \
               out[ offset[ point ] ] = \
                  (type) ( val + ( ( val > (flt_type) 0.0 ) ? \
                                   ( (flt_type) 0.5 ) : \
                                   ( (flt_type) -0.5 ) ) ); \
            } \
         } \
      } \
   } \
   frac_hi = astFree( frac_hi ); \
   frac_lo = astFree( frac_lo ); \
   hi = astFree( hi ); \
   idim = astFree( idim ); \
   lo = astFree( lo ); \
   stride = astFree( stride ); \
   wt = astFree( wt ); \
   if ( !astOK ) result = 0; \
   return result; \
}
MAKE_INTERPOLATE_PIXEL_LINEAR(LD,long double,1,long double)
MAKE_INTERPOLATE_PIXEL_LINEAR(D,double,1,double)
MAKE_INTERPOLATE_PIXEL_LINEAR(F,float,1,float)
MAKE_INTERPOLATE_PIXEL_LINEAR(L,long int,0,long double)
MAKE_INTERPOLATE_PIXEL_LINEAR(UL,unsigned long int,0,long double)
MAKE_INTERPOLATE_PIXEL_LINEAR(I,int,0,double)
MAKE_INTERPOLATE_PIXEL_LINEAR(UI,unsigned int,0,double)
MAKE_INTERPOLATE_PIXEL_LINEAR(S,short int,0,float)
MAKE_INTERPOLATE_PIXEL_LINEAR(US,unsigned short int,0,float)
MAKE_INTERPOLATE_PIXEL_LINEAR(B,signed char,0,float)
MAKE_INTERPOLATE_PIXEL_LINEAR(UB,unsigned char,0,float)
#undef MAKE_INTERPOLATE_PIXEL_LINEAR

static int ResampleSection( AstMapping *this, const double *linear_fit,
                      int ndim_in, const int *lbnd_in, const int *ubnd_in,
                      const void *in, DataType type,
                      AstInterpolate method,
                      int usebad, const void *badflag_ptr,
                      int ndim_out, const int *lbnd_out, const int *ubnd_out,
                      const int *lbnd, const int *ubnd, void *out ) {
   int npoint;
   int idim;
   int *offset;
   AstPointSet *pset_in;         /* Input PointSet for transformation */
   AstPointSet *pset_out;        /* Output PointSet for transformation */
   double **ptr_in;
   double **ptr_out;
   int *dim;
   int done;
   int point;
   int *stride;
   int off;
   int s;
   int result;
   const double *zero;
   const double *grad;
   int a1;
   int a2;
   int g1;
   int g2;

   result = 0;
   if ( !astOK ) return result;

   npoint = 1;
   for ( idim = 0; idim < ndim_out; idim++ ) {
      npoint *= ubnd[ idim ] - lbnd[ idim ] + 1;
   }
   offset = astMalloc( sizeof( int ) * (size_t) npoint );
   dim = astMalloc( sizeof( int ) * (size_t) ndim_out );
   stride = astMalloc( sizeof( int ) * (size_t) ndim_out );

   off = 0;
   s = 1;
   for ( idim = 0; idim < ndim_out; idim++ ) {
      stride[ idim ] = s;
      s *= ubnd_out[ idim ] - lbnd_out[ idim ] + 1;
      dim[ idim ] = lbnd[ idim ];
      off += ( dim[ idim ] - lbnd_out[ idim ] ) * stride[ idim ];
   }

   if ( linear_fit ) {
      double *accum;
      int coord_in;
      int coord_out;
      int ndim_in = astGetNin( this );
      int nd1;
      zero = linear_fit;
      grad = linear_fit + ndim_out;
      pset_in = astPointSet( npoint, ndim_in, "" );
      ptr_in = astGetPoints( pset_in );
      accum = astMalloc( sizeof( double ) *
                         (size_t) ( ndim_in * ( ndim_out + 1 ) ) );
      nd1 = ndim_out + 1;
      for ( coord_in = 0; coord_in < ndim_in; coord_in++ ) {
         a1 = coord_in * nd1;
         g1 = coord_in * ndim_out;
         accum[ a1 + ndim_out ] = 0.0;
         for ( coord_out = ndim_out - 1; coord_out >= 0; coord_out-- ) {
            a2 = a1 + coord_out;
            g2 = g1 + coord_out;
            accum[ a2 ] = accum[ a2 + 1 ] + grad[ g2 ] *
                                        (double) lbnd[ coord_out ];
         }
      }

      for ( done = 0, point = 0; !done; point++ ) {
         for ( coord_in = 0; coord_in < ndim_in; coord_in++ ) {
            ptr_in[ coord_in ][ point ] = accum[ coord_in * nd1 ];
         }
         offset[ point ] = off;
         coord_out = 0;
         do {
            if ( dim[ coord_out ] < ubnd[ coord_out ] ) {
               dim[ coord_out ]++;
               off += stride[ coord_out ];
               break;
            } else {
               dim[ coord_out ] = lbnd[ coord_out ];
               off -= ( ubnd[ coord_out ] - lbnd[ coord_out ] ) * stride[ coord_out ];
               done = ( ++coord_out == ndim_out );
            }
         } while ( !done );
         for ( coord_in = 0; coord_in < ndim_in; coord_in++ ) {
            a1 = coord_in * nd1;
            g1 = coord_in * ndim_out;
            for ( idim = coord_out; idim >= 0; idim-- ) {
               a2 = a1 + idim;
               g2 = g1 + idim;
               accum[ a2 ] = accum[ a2 + 1 ] + dim[ idim ] * grad[ g2 ];
            }
         }
      }
      accum = astFree( accum );
   } else {
      pset_out = astPointSet( npoint, ndim_out, "" );
      ptr_out = astGetPoints( pset_out );
      for ( done = 0, point = 0; !done; point++ ) {
         for ( idim = 0; idim < ndim_out; idim++ ) {
            ptr_out[ idim ][ point ] = (double) dim[ idim ];
         }
         offset[ point ] = off;
         idim = 0;
         while ( !done ) {
            if ( dim[ idim ] < ubnd[ idim ] ) {
               dim[ idim ]++;
               off += stride[ idim ];
               break;
            } else {
               dim[ idim ] = lbnd[ idim ];
               off -= ( ubnd[ idim ] - lbnd[ idim ] ) * stride[ idim ];
               done = ( ++idim == ndim_out );
            }
         }
      }
      pset_in = astTransform( this, pset_out, 0, NULL );
      ptr_in = astGetPoints( pset_in );
      pset_out = astAnnul( pset_out );
   }
   if ( astOK ) {
      if ( method == AST__NEAREST ) {

#define CASE(name,abbrev,type) \
            case ( name ): \
               result = \
               InterpolatePixelNearest##abbrev( ndim_in, lbnd_in, ubnd_in, \
                                                (type *) in, npoint, offset, \
                                                ptr_in[ 0 ], usebad, \
                                                *( (type *) badflag_ptr ), \
                                                (type *) out ); \
               break;
       
         switch ( type ) {
            CASE(LDOUBLE,LD,long double)
            CASE(DOUBLE,D,double)
            CASE(FLOAT,F,float)
            CASE(LONG,L,long int)
            CASE(ULONG,UL,unsigned long int)
            CASE(INT,I,int)
            CASE(UINT,UI,unsigned int)
            CASE(SHORT,S,short int)
            CASE(USHORT,US,unsigned short int)
            CASE(BYTE,B,signed char)
            CASE(UBYTE,UB,unsigned char)
         }
#undef CASE
               
      } else if ( method == AST__LINEAR ) {

#define CASE(name,abbrev,type) \
            case ( name ): \
               result = \
               InterpolatePixelLinear##abbrev( ndim_in, lbnd_in, ubnd_in,\
                                               (type *) in, npoint, offset, \
                                               ptr_in[ 0 ], usebad, \
                                               *( (type *) badflag_ptr ), \
                                               (type *) out ); \
               break;

         switch ( type ) {
            CASE(LDOUBLE,LD,long double)
            CASE(DOUBLE,D,double)
            CASE(FLOAT,F,float)
            CASE(LONG,L,long int)
            CASE(ULONG,UL,unsigned long int)
            CASE(INT,I,int)
            CASE(UINT,UI,unsigned int)
            CASE(SHORT,S,short int)
            CASE(USHORT,US,unsigned short int)
            CASE(BYTE,B,signed char)
            CASE(UBYTE,UB,unsigned char)
         }
#undef CASE

      } else {
         result = ( *method )( ndim_in, lbnd_in, ubnd_in,
                               (double *) in,
                               npoint, offset, ptr_in[ 0 ],
                               usebad,
                               *( (double *) badflag_ptr ),
                               (double *) out );
      }
   }
   dim = astFree( dim );
   offset = astFree( offset );
   pset_in = astAnnul( pset_in );
   stride = astFree( stride );
   if ( !astOK ) result = 0;
   return result;
}

static int Min( int a, int b ) {
  return ( a < b ) ? a : b;
}

static double *TestIfLinear( AstMapping *this,
                             const int *lbnd, const int *ubnd, double acc) {

   AstPointSet *pset_ina;
   AstPointSet *pset_inb;
   AstPointSet *pset_outa;
   AstPointSet *pset_outb;
   double **ptr_ina;
   double **ptr_inb;
   double **ptr_outa;
   double **ptr_outb;
   double *fit;
   double z;
   double diff;
   double err;
   double x0;
   double x;
   double y;
   int *limit;
   int coord_in;
   int coord_out;
   int done;
   int face1;
   int face2;
   int face;
   int linear;
   int ndim_in;
   int ndim_out;
   int npoint;
   int point;
   double in1;
   double in2;
   double out1;
   double out2;
   double outdiff;
   double *zero;
   int ii;
   double *grad;

   ndim_in = astGetNin( this );
   ndim_out = astGetNout( this );
   linear = 1;

   fit = astMalloc( sizeof( double ) * (size_t) ( ( ndim_in + 1 ) * ndim_out ) );
   zero = fit;
   grad = fit + ndim_out;
   limit = astMalloc( sizeof( int ) * (size_t) ndim_out );
   pset_outa = astPointSet( 2 * ndim_out, ndim_out, "" );
   ptr_outa = astGetPoints( pset_outa );
   ptr_ina = AST__NULL;
   if ( astOK ) {

/* Set up output coordinates at centre of each face. */
      for ( face = 0; face < ( 2 * ndim_out ); face++ ) {
         for ( coord_out = 0; coord_out < ndim_out; coord_out++ ) {
            ptr_outa[ coord_out ][ face ] =
               0.5 * ( lbnd[ coord_out ] + ubnd[ coord_out ] );
         }
         ptr_outa[ face / 2 ][ face ] = ( face % 2 ) ?
                                        ubnd[ face / 2 ] : lbnd[ face / 2 ];
      }
   }

/* Transform to input coordinate space. */
   pset_ina = astTransform( this, pset_outa, 0, NULL );
   ptr_ina = astGetPoints( pset_ina );
   if ( astOK ) {

/* Calculate gradients and zero points assuming a linear transformation. */
      ii = 0;
      for ( coord_in = 0; coord_in < ndim_in; coord_in++ ) {
         z = 0.0;
         for ( coord_out = 0; coord_out < ndim_out; coord_out++ ) {
            face1 = 2 * coord_out;
            face2 = face1 + 1;
            out1 = ptr_outa[ coord_out ][ face1 ];
            out2 = ptr_outa[ coord_out ][ face2 ];
            in1 = ptr_ina[ coord_in ][ face1 ];
            in2 = ptr_ina[ coord_in ][ face2 ];
            if ( ( in1 == AST__BAD ) || ( in2 == AST__BAD ) ) {
               linear = 0;
               break;
            }
            z += ( in1 + in2 );
            outdiff = out2 - out1;
            if ( outdiff != 0.0 ) {
               grad[ ii++ ] = ( in2 - in1 ) / outdiff;
            } else {
               grad[ ii++ ] = 0.0;
            }
         }
         if ( !linear ) break;
         zero[ coord_in ] = z / (double) ( 2 * ndim_out );
      }
      ii = 0;
      for ( coord_in = 0; coord_in < ndim_in; coord_in++ ) {
         for ( coord_out = 0; coord_out < ndim_out; coord_out++ ) {
            x0 = 0.5 * (double) ( lbnd[ coord_out ] + ubnd[ coord_out ] );
            zero[ coord_in ] -= grad[ ii++ ] * x0;
         }
      }
   }

/* Set up test points in th eoutput coordinate space. */
   if ( astOK && linear ) {
      npoint = 1;
      for ( coord_out = 0; coord_out < ndim_out; coord_out++ ) {
         npoint *= 2;
         limit[ coord_out ] = 0;
      }
      npoint = 1 + 2 * ndim_out + 2 * npoint;
      pset_outb = astPointSet( npoint, ndim_out, "" );
      ptr_outb = astGetPoints( pset_outb );

/* One point at the centre. */
      point = 0;
      for ( coord_out = 0; coord_out < ndim_out; coord_out++ ) {
         ptr_outb[ coord_out ][ point ] =
           0.5 * (double) ( lbnd[ coord_out ] + ubnd[ coord_out ] );
      }
      point++;

/* One half way to the centre of each face. */
      for ( face = 0; face < ( 2 * ndim_out ); face++ ) {
         for ( coord_out = 0; coord_out < ndim_out; coord_out++ ) {
            ptr_outb[ coord_out ][ point ] =
               0.5 * ( lbnd[ coord_out ] + ubnd[ coord_out ] );
         }
         ptr_outb[ face / 2 ][ point ] =
           0.5 * ( ( (double) ( ( face % 2 ) ? ubnd[ face / 2 ] :
                                               lbnd[ face / 2 ] ) ) +
                   ptr_outb[ face / 2 ][ 0 ] );
         point++;
      }

/* One at each vertex and half way to each vertex. */
      done = 0;
      while ( !done ) {
         for ( coord_out = 0; coord_out < ndim_out; coord_out++ ) {
            ptr_outb[ coord_out ][ point ] = limit[ coord_out ] ?
                                             ubnd[ coord_out ] :
                                             lbnd[ coord_out ];
            ptr_outb[ coord_out ][ point + 1 ] =
              0.5 * ( ptr_outb[ coord_out ][ point ] +
                      ptr_outb[ coord_out ][ 0 ] );
         }
         point += 2;
         coord_out = 0;
         while ( !done ) {
            if ( !limit[ coord_out ] ) {
               limit[ coord_out ] = 1;
               break;
            } else {
               limit[ coord_out ] = 0;
               done = ( ++coord_out == ndim_out );
            }
         }
      }

/* Transform the test points to the input coordinate space. */
      pset_inb = astTransform( this, pset_outb, 0, NULL );
      ptr_inb = astGetPoints( pset_inb );

/* Apply the linear transformation to each test point. */
      for ( point = 0; point < npoint; point++ ) {
         err = 0.0;
         ii = 0;
         for ( coord_in = 0; coord_in < ndim_in; coord_in++ ) {
            y = zero[ coord_in ];
            for ( coord_out = 0; coord_out < ndim_out; coord_out++ ) {
               x = ptr_outb[ coord_out ][ point ];
               if ( x == AST__BAD ) {
                  linear = 0;
                  break;
               }
               y += grad[ ii++ ] * x;
            }
            if ( !linear ) break;

/* Test if the maximum difference is significant. */
            diff = ( y - ptr_inb[ coord_in ][ point ] );
            err += diff * diff;
         }
         if ( !linear ) break;
         if ( sqrt( err ) > acc ) {
            linear = 0;
            break;
         }
      }
   }
   pset_ina = astAnnul( pset_ina );
   pset_outa = astAnnul( pset_outa );
   limit = astFree( limit );
   if ( ! linear || !astOK ) fit = astFree( fit );
   return fit;
}
                         
static int ResampleBlock( AstMapping *this, const double *linear,
                      int ndim_in, const int *lbnd_in, const int *ubnd_in,
                      const void *in, DataType type,
                      AstInterpolate method, double acc,
                      int usebad, const void *badflag_ptr,
                      int ndim_out, const int *lbnd_out, const int *ubnd_out,
                      const int *lbnd, const int *ubnd, void *out ) {

   int *lbnd_sect;
   int *ubnd_sect;
   int *step;
   int idim;
   int done;
   int npix;
   int mxstep;
   int dim;
   int mxpix = 65536;
   int lolim;
   int hilim;
   int result;

   result = 0;
   if ( !astOK ) return result;

   lbnd_sect = astMalloc( sizeof( int ) * (size_t) ndim_out );
   ubnd_sect = astMalloc( sizeof( int ) * (size_t) ndim_out );
   step = astMalloc( sizeof( int ) * (size_t) ndim_out );

   mxstep = 0;
   npix = 1;
   for ( idim = 0; idim < ndim_out; idim++ ) {
      dim = ubnd[ idim ] - lbnd[ idim ] + 1;
      if ( mxstep < dim ) mxstep = dim;
      npix *= dim;
   }
   if ( npix > mxpix ) {
      lolim = 1;
      hilim = mxstep;
      while ( ( hilim - lolim ) > 1 ) {
         mxstep = ( hilim + lolim ) / 2;
         for ( npix = 1, idim = 0; idim < ndim_out ; idim++ ) {
            dim = ubnd[ idim ] - lbnd[ idim ] + 1;
            npix *= ( dim < mxstep ) ? dim : mxstep;
         }
         *( ( npix <= mxpix ) ? &lolim : &hilim ) = mxstep;
      }
      mxstep = lolim;
   }
   if ( mxstep < 2 ) mxstep = 2;
   for ( idim = 0; idim < ndim_out ; idim++ ) {
      dim = ubnd[ idim ] - lbnd[ idim ] + 1;
      step[ idim ] = ( dim < mxstep ) ? dim : mxstep;
      lbnd_sect[ idim ] = lbnd[ idim ];
      ubnd_sect[ idim ] = Min( lbnd[ idim ] + step[ idim ] - 1, ubnd[ idim ] );
   }
#if 0
   npix = 1;
   for ( idim = 0; idim < ndim_out ; idim++ ) {
      printf( "%d ", step[ idim ] );
      npix *= step[ idim ];
   }
   printf( "\n" );
   printf( "best mxstep is at %d, giving %d pixels\n", mxstep, npix );
#endif
   done = 0;
   while ( !done ) {
#if 0
      {
         int i;
         for ( i = 0; i < ndim_out; i++ ) {
            printf( "%d:%d%s", lbnd_sect[ i ], ubnd_sect[ i ],
                    ( i < ndim_out - 1 ) ? ", " : "" );
         }
         printf( "\n" );
      }
#endif
      result += ResampleSection( this, linear, ndim_in, lbnd_in, ubnd_in,
                                 in,
                                 type, method,
                            usebad, badflag_ptr,
                            ndim_out, lbnd_out, ubnd_out,
                            lbnd_sect, ubnd_sect, out );
      idim = 0;
      while ( !done ) {
         if ( ubnd_sect[ idim ] < ubnd[ idim ] ) {
            lbnd_sect[ idim ] = Min( lbnd_sect[ idim ] + step[ idim ],
                                     ubnd[ idim ] );
            ubnd_sect[ idim ] = Min( lbnd_sect[ idim ] + step[ idim ] - 1,
                                     ubnd[ idim ] );
            break;
         } else {
            lbnd_sect[ idim ] = lbnd[ idim ];
            ubnd_sect[ idim ] = Min( lbnd[ idim ] + step[ idim ] - 1,
                                     ubnd[ idim ] );
            done = ( ++idim == ndim_out );
         }
      }
   }

   lbnd_sect = astFree( lbnd_sect );
   ubnd_sect = astFree( ubnd_sect );
   step = astFree( step );
   if ( !astOK ) result = 0;
   return result;
}

#define MAKE_RESAMPLE(name,abbrev,type) \
static int Resample##abbrev( AstMapping *this, int ndim_in, \
                             const int *lbnd_in, const int *ubnd_in, \
                             const type *in, AstInterpolate method, double acc, \
                             int usebad, type badflag, int ndim_out, \
                             const int *lbnd_out, const int *ubnd_out, \
                             const int *lbnd, const int *ubnd, type *out ) { \
   double *linear_fit; \
   int result; \
   int npix; \
   int mxdim; \
   int coord_out; \
   int dim; \
   int divide; \
   int toobig; \
   int toosmall; \
   int dimx; \
 \
   result = 0; \
   if ( !astOK ) return result; \
 \
   npix = 1; \
   mxdim = 0; \
   dimx = 1; \
   for ( coord_out = 0; coord_out < ndim_out; coord_out++ ) { \
      dim = ubnd[ coord_out ] - lbnd[ coord_out ] + 1; \
      npix *= dim; \
      if ( dim > mxdim ) { \
         mxdim = dim; \
         dimx = coord_out; \
      } \
   } \
    \
   toosmall = npix <= 100; \
   toobig = mxdim > 50; \
 \
   linear_fit = NULL; \
   if ( toosmall ) { \
      divide = 0; \
   } else if ( toobig ) { \
      divide = 1; \
   } else { \
     linear_fit = TestIfLinear( this, lbnd, ubnd, acc ); \
     divide = !linear_fit; \
   } \
   if ( astOK ) { \
      if ( !divide ) { \
         result += ResampleBlock( this, linear_fit, \
                                  ndim_in, lbnd_in, ubnd_in, \
                                  (void *) in, name, \
                                  method, acc, \
                                  usebad, (void *) &badflag, \
                                  ndim_out, lbnd_out, ubnd_out, \
                                  lbnd, ubnd, (void *) out ); \
      } else { \
         int lo[ 100 ]; \
         int hi[ 100 ]; \
         int tmp; \
         for ( coord_out = 0; coord_out < ndim_out; coord_out++ ) { \
            lo[ coord_out ] = lbnd[ coord_out ]; \
            hi[ coord_out ] = ubnd[ coord_out ]; \
         } \
         tmp = hi[ dimx ]; \
         hi[ dimx ] = ( ubnd[ dimx ] + lbnd[ dimx ] ) / 2; \
         result += Resample##abbrev( this, ndim_in, lbnd_in, ubnd_in, in, \
                             method, acc, \
                             usebad, badflag, \
                             ndim_out, lbnd_out, ubnd_out, \
                             lo, hi, out ); \
         lo[ dimx ] = hi[ dimx ] + 1; \
         hi[ dimx ] = tmp; \
         result += Resample##abbrev( this, ndim_in, lbnd_in, ubnd_in, in, \
                             method, acc, \
                             usebad, badflag, \
                             ndim_out, lbnd_out, ubnd_out, \
                             lo, hi, out ); \
      } \
   } \
   if ( linear_fit ) linear_fit = astFree( linear_fit ); \
   if ( !astOK ) result = 0; \
   return result; \
}
MAKE_RESAMPLE(LDOUBLE,LD,long double)
MAKE_RESAMPLE(DOUBLE,D,double)
MAKE_RESAMPLE(FLOAT,F,float)
MAKE_RESAMPLE(LONG,L,long int)
MAKE_RESAMPLE(ULONG,UL,unsigned long int)
MAKE_RESAMPLE(INT,I,int)
MAKE_RESAMPLE(UINT,UI,unsigned int)
MAKE_RESAMPLE(SHORT,S,short int)
MAKE_RESAMPLE(USHORT,US,unsigned short int)
MAKE_RESAMPLE(BYTE,B,signed char)
MAKE_RESAMPLE(UBYTE,UB,unsigned char)
#undef MAKE_RESAMPLE
#endif

static void ClearAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a Mapping.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     void ClearAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     Mapping member function (over-rides the astClearAttrib protected
*     method inherited from the Object class).

*  Description:
*     This function clears the value of a specified attribute for a
*     Mapping, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the Mapping.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*/

/* Local Variables: */
   AstMapping *this;             /* Pointer to the Mapping structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Mapping structure. */
   this = (AstMapping *) this_object;

/* Check the attribute name and clear the appropriate attribute. */

/* Invert. */
/* ------- */
   if ( !strcmp( attrib, "invert" ) ) {
      astClearInvert( this );

/* Report. */
/* ------- */
   } else if ( !strcmp( attrib, "report" ) ) {
      astClearReport( this );

/* If the name was not recognised, test if it matches any of the
   read-only attributes of this class. If it does, then report an
   error. */
   } else if ( !strcmp( attrib, "nin" ) ||
        !strcmp( attrib, "nout" ) ||
        !strcmp( attrib, "tranforward" ) ||
        !strcmp( attrib, "traninverse" ) ) {
      astError( AST__NOWRT, "astClear: Invalid attempt to clear the \"%s\" "
                "value for a %s.", attrib, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib );
   }
}

static const char *GetAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a Mapping.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     const char *GetAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     Mapping member function (over-rides the protected astGetAttrib
*     method inherited from the Object class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a Mapping, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the Mapping.
*     attrib
*        Pointer to a null terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.

*  Returned Value:
*     Pointer to a null terminated string containing the attribute
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the Mapping, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the Mapping. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Constants: */
#define BUFF_LEN 50              /* Max. characters in result buffer */

/* Local Variables: */
   AstMapping *this;             /* Pointer to the Mapping structure */
   const char *result;           /* Pointer value to return */
   int invert;                   /* Invert attribute value */
   int nin;                      /* Nin attribute value */
   int nout;                     /* Nout attribute value */
   int report;                   /* Report attribute value */
   int tran_forward;             /* TranForward attribute value */
   int tran_inverse;             /* TranInverse attribute value */
   static char buff[ BUFF_LEN + 1 ]; /* Buffer for string result */

/* Initialise. */
   result = NULL;

/* Check the global error status. */   
   if ( !astOK ) return result;

/* Obtain a pointer to the Mapping structure. */
   this = (AstMapping *) this_object;

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "buff" as a null terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* Invert. */
/* ------- */
   if ( !strcmp( attrib, "invert" ) ) {
      invert = astGetInvert( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", invert );
         result = buff;
      }

/* Nin. */
/* ---- */
   } else if ( !strcmp( attrib, "nin" ) ) {
      nin = astGetNin( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", nin );
         result = buff;
      }

/* Nout. */
/* ----- */
   } else if ( !strcmp( attrib, "nout" ) ) {
      nout = astGetNout( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", nout );
         result = buff;
      }

/* Report. */
/* ------- */
   } else if ( !strcmp( attrib, "report" ) ) {
      report = astGetReport( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", report );
         result = buff;
      }

/* TranForward. */
/* ------------ */
   } else if ( !strcmp( attrib, "tranforward" ) ) {
      tran_forward = astGetTranForward( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", tran_forward );
         result = buff;
      }

/* TranInverse. */
/* ------------ */
   } else if ( !strcmp( attrib, "traninverse" ) ) {
      tran_inverse = astGetTranInverse( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", tran_inverse );
         result = buff;
      }

/* If the attribute name was not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_getattrib)( this_object, attrib );
   }

/* Return the result. */
   return result;

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

static int GetNin( AstMapping *this ) {
/*
*+
*  Name:
*     astGetNin

*  Purpose:
*     Get the number of input coordinates for a Mapping.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "mapping.h"
*     int astGetNin( AstMapping *this )

*  Class Membership:
*     Mapping method.

*  Description:
*     This function returns the number of input coordinate values
*     required per point by a Mapping (i.e. the number of dimensions
*     of the space in which input points reside).

*  Parameters:
*     this
*        Pointer to the Mapping.

*  Returned Value:
*     Number of coordinate values required.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   int invert;                   /* Invert attribute value */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Determine if the Mapping has been inverted. */
   invert = astGetInvert( this );

/* Obtain the Nin value. */
   if ( astOK ) result = invert ? this->nout : this->nin;

/* Return the result. */
   return result;
}

static int GetNout( AstMapping *this ) {
/*
*+
*  Name:
*     astGetNout

*  Purpose:
*     Get the number of output coordinates for a Mapping.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "mapping.h"
*     int astGetNout( AstMapping *this )

*  Class Membership:
*     Mapping method.

*  Description:
*     This function returns the number of output coordinate values
*     generated per point by a Mapping (i.e. the number of dimensions
*     of the space in which output points reside).

*  Parameters:
*     this
*        Pointer to the Mapping.

*  Returned Value:
*     Number of coordinate values generated.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   int invert;                   /* Invert attribute value */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Determine if the Mapping has been inverted. */
   invert = astGetInvert( this );

/* Obtain the Nout value. */
   if ( astOK ) result = invert ? this->nin : this->nout;

/* Return the result. */
   return result;
}

static int GetTranForward( AstMapping *this ) {
/*
*+
*  Name:
*     astGetTranForward

*  Purpose:
*     Determine if a Mapping defines a forward coordinate transformation.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "mapping.h"
*     int astGetTranForward( AstMapping *this )

*  Class Membership:
*     Mapping method.

*  Description:
*     This function returns a value indicating whether a Mapping is
*     able to perform a coordinate transformation in the "forward"
*     direction.

*  Parameters:
*     this
*        Pointer to the Mapping.

*  Returned Value:
*     Zero if the forward coordinate transformation is not defined, or
*     1 if it is.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   int invert;                   /* Mapping inverted? */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Determine if the Mapping has been inverted. */
   invert = astGetInvert( this );

/* If OK, obtain the result. */
   if ( astOK ) result = invert ? this->tran_inverse : this->tran_forward;

/* Return the result. */
   return result;
}

static int GetTranInverse( AstMapping *this ) {
/*
*+
*  Name:
*     astGetTranInverse

*  Purpose:
*     Determine if a Mapping defines an inverse coordinate transformation.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "mapping.h"
*     int astGetTranInverse( AstMapping *this )

*  Class Membership:
*     Mapping method.

*  Description:
*     This function returns a value indicating whether a Mapping is
*     able to perform a coordinate transformation in the "inverse"
*     direction.

*  Parameters:
*     this
*        Pointer to the Mapping.

*  Returned Value:
*     Zero if the inverse coordinate transformation is not defined, or
*     1 if it is.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   int invert;                   /* Mapping inverted? */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Determine if the Mapping has been inverted. */
   invert = astGetInvert( this );

/* If OK, obtain the result. */
   if ( astOK ) result = invert ? this->tran_forward : this->tran_inverse;

/* Return the result. */
   return result;
}

static void GlobalBounds( MapData *mapdata, double *lbnd, double *ubnd,
                          double xl[], double xu[] ) {
/*
*  Name:
*     GlobalBounds

*  Purpose:
*     Estimate global coordinate bounds for a Mapping.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     void GlobalBounds( MapData *mapdata, double *lbnd, double *ubnd,
*                        double xl[], double xu[] );

*  Class Membership:
*     Mapping member function.

*  Description:
*     This function estimates the global lower and upper bounds of a
*     Mapping function within a constrained region of its input
*     coordinate space. It uses a robust global optimisation algorithm
*     based on the selection of pseudo-random starting positions,
*     followed by the location of local minima and maxima using the
*     downhill (or uphill) simplex method. The algorithm will cope
*     with the case where there are several competing minima (or
*     maxima) with nearly equal values. It attempts to locate the
*     global bounds to full machine precision when possible.

*  Parameters:
*     mapdata
*        Pointer to a MapData structure describing the Mapping
*        function, its coordinate constraints, etc.
*     lbnd
*        Pointer to a double.  On entry, this should contain a
*        previously-obtained upper limit on the global lower bound, or
*        AST__BAD if no such limit is available. On exit, it will be
*        updated with a new estimate of the global lower bound, if a
*        better one has been found.
*     ubnd
*        Pointer to a double.  On entry, this should contain a
*        previously-obtained lower limit on the global upper bound, or
*        AST__BAD if no such limit is available. On exit, it will be
*        updated with a new estimate of the global upper bound, if a
*        better one has been found.
*     xl
*        Pointer to an array of double, with one element for each
*        input coordinate. On entry, if *lbnd is not equal to AST__OK,
*        this should contain the input coordinates of a point at which
*        the Mapping function takes the value *lbnd. On exit, this
*        function returns the position of a (not necessarily unique)
*        input point at which the Mapping function takes the value of
*        the new global lower bound.  This array is not altered if an
*        improved estimate of the global lower bound cannot be found.
*     xu
*        Pointer to an array of double, with one element for each
*        input coordinate. On entry, if *ubnd is not equal to AST__OK,
*        this should contain the input coordinates of a point at which
*        the Mapping function takes the value *ubnd. On exit, this
*        function returns the position of a (not necessarily unique)
*        input point at which the Mapping function takes the value of
*        the new global upper bound.  This array is not altered if an
*        improved estimate of the global upper bound cannot be found.

*  Notes:
*     - The efficiency of this function will usually be improved if
*     previously-obtained estimates of the extrema and their locations
*     are provided.
*     - The values returned via "lbnd", "ubnd", "xl" and "xu" will be
*     set to the value AST__BAD if this function should fail for any
*     reason. Their initial values on entry will not be altered if the
*     function is invoked with the global error status set.
*/

/* Local Constants: */
   const int maxiter = 10000;    /* Maximum number of iterations */
   const int minsame = 3;        /* Minimum no. consistent extrema required */
   const int nbatch = 32;        /* No. function samples obtained per batch */

/* Local Variables: */
   AstPointSet *pset_in;         /* Input PointSet for batch transformation */
   AstPointSet *pset_out;        /* Output PointSet for batch transformation */
   double **ptr_in;              /* Pointer to batch input coordinates */
   double **ptr_out;             /* Pointer to batch output coordinates */
   double *active_hi;            /* Estimated upper limits of active region */
   double *active_lo;            /* Estimated lower limits of active region */
   double *sample_hi;            /* Upper limits of sampled region */
   double *sample_lo;            /* Lower limits of sampled region */
   double *sample_width;         /* Nominal widths of sampled region */
   double *x;                    /* Pointer to array of coordinates */
   double acc;                   /* Convergence accuracy for finding maximum */
   double active_width;          /* Estimated width of active region */
   double new_max;               /* Value of new local maximum */
   double new_min;               /* Value of new local minimum */
   double oversize;              /* Over-size factor for sampled region */
   double random;                /* Pseudo-random number */
   int bad;                      /* Transformed position is bad? */
   int batch;                    /* Next element to use in position batch */
   int coord;                    /* Loop counter for coordinates */
   int done_max;                 /* Satisfactory global maximum found? */
   int done_min;                 /* Satisfactory global minimum found? */
   int iter;                     /* Loop counter for iterations */
   int ncall;                    /* Number of Mapping function calls (junk) */
   int ncoord;                   /* Number of coordinates in search space */
   int nmax;                     /* Number of local maxima found */
   int nmin;                     /* Number of local minima found */
   int nsame_max;                /* Number of equivalent local maxima found */
   int nsame_min;                /* Number of equivalent local minima found */
   long int seed = 1776655449;   /* Arbitrary pseudo-random number seed */

/* Check the global error status */
   if ( !astOK ) return;

/* Initialise. */
   done_max = 0;
   done_min = 0;
   ncall = 0;
   nmax = 0;
   nmin = 0;
   nsame_max = 0;
   nsame_min = 0;

/* Extract the number of input coordinates for the Mapping function
   and allocate workspace. */
   ncoord = mapdata->nin;
   active_hi = astMalloc( sizeof( double ) * (size_t) ncoord );
   active_lo = astMalloc( sizeof( double ) * (size_t) ncoord );
   sample_hi = astMalloc( sizeof( double ) * (size_t) ncoord );
   sample_lo = astMalloc( sizeof( double ) * (size_t) ncoord );
   sample_width = astMalloc( sizeof( double ) * (size_t) ncoord );
   x = astMalloc( sizeof( double ) * (size_t) ncoord );
   if ( astOK ) {

/* Calculate the factor by which the size of the region we sample will
   exceed the size of the Mapping function's active region (the region
   where the transformed coordinates are non-bad) in each
   dimension. This is chosen so that the volume ratio will be 2. */
      oversize = pow( 2.0, 1.0 / ( (double) ncoord ) );

/* Initialise the limits iof the active region to unknown. */
      for ( coord = 0; coord < ncoord; coord++ ) {
         active_lo[ coord ] = DBL_MAX;;
         active_hi[ coord ] = -DBL_MAX;

/* Initialise the nominal widths of the sampled region to be the
   actual widths of the search region times the over-size factor. */
         sample_width[ coord ] = ( mapdata->ubnd[ coord ] -
                                   mapdata->lbnd[ coord ] ) * oversize;

/* Initialise the sampled region to match the search region. */
         sample_lo[ coord ] = mapdata->lbnd[ coord ];
         sample_hi[ coord ] = mapdata->ubnd[ coord ];
      }

/* Set up position buffer. */
/* ======================= */
/* Create two PointSets to act as buffers to hold a complete batch of
   input and output coordinates. Obtain pointers to their coordinate
   arrays. */
      pset_in = astPointSet( nbatch, ncoord, "" );
      pset_out = astPointSet( nbatch, mapdata->nout, "" );
      ptr_in = astGetPoints( pset_in );
      ptr_out = astGetPoints( pset_out );

/* Initialise the next element to be used in the position buffer to
   indicate that the buffer is initially empty. */
      batch = nbatch;
   }

/* Define a macro to fill the position buffer with a set of
   pseudo-random positions and to transform them. */
#define FILL_POSITION_BUFFER {\
\
/* We first generate a suitable volume over which to distribute the\
   batch of pseudo-random positions. Initially, this will be the\
   entire search volume, but if we find that the only non-bad\
   transformed coordinates we obtain are restricted to a small\
   sub-region of this input volume, then we reduce the sampled volume\
   so as to concentrate more on the active region. */\
\
/* Loop through each input coordinate, checking that at least one\
   non-bad transformed point has been obtained. If not, we do not\
   adjust the sampled volume, as we do not yet know where the active\
   region lies. */\
   for ( coord = 0; coord < ncoord; coord++ ) {\
      if ( active_hi[ coord ] >= active_lo[ coord ] ) {\
\
/* Estimate the width of the active region from the range of input\
   coordinates that have so far produced non-bad transformed\
   coordinates. */\
         active_width = active_hi[ coord ] - active_lo[ coord ];\
\
/* If the current width of the sampled volume exceeds this estimate by\
   more than the required factor, then reduce the width of the sampled\
   volume. The rate of reduction is set so that the volume of the\
   sampled region can halve with every fourth batch of positions. */\
         if ( ( active_width * oversize ) < sample_width[ coord ] ) {\
            sample_width[ coord ] /= pow( oversize, 0.25 );\
\
/* If the width of the sampled volume does not exceed that of the\
   known active region by the required factor, then adjust it so that\
   it does. Note that we must continue to sample some points outside\
   the known active region in case we have missed any (in which case\
   the sampled region will expand again to include them). */\
         } else if ( ( active_width * oversize ) > sample_width[ coord ] ) {\
            sample_width[ coord ] = active_width * oversize;\
         }\
\
/* Calculate the lower and upper bounds on the sampled volume, using\
   the new width calculated above and centring it on the active\
   region, as currently known. */\
         sample_lo[ coord ] = ( active_lo[ coord ] + active_hi[ coord ] -\
                                sample_width[ coord ] ) * 0.5;\
         sample_hi[ coord ] = ( active_lo[ coord ] + active_hi[ coord ] +\
                                sample_width[ coord ] ) * 0.5;\
\
/* Ensure that the sampled region does not extend beyond the original\
   search region. */\
         if ( sample_lo[ coord ] < mapdata->lbnd[ coord ] ) {\
            sample_lo[ coord ] = mapdata->lbnd[ coord ];\
         }\
         if ( sample_hi[ coord ] > mapdata->ubnd[ coord ] ) {\
            sample_hi[ coord ] = mapdata->ubnd[ coord ];\
         }\
      }\
   }\
\
/* Having determined the size of the sampled volume, create a batch of\
   pseudo-random positions uniformly distributed within it. */\
   for ( batch = 0; batch < nbatch; batch++ ) {\
      for ( coord = 0; coord < ncoord; coord++ ) {\
         random = Random( &seed );\
         ptr_in[ coord ][ batch ] = sample_lo[ coord ] * random +\
                                    sample_hi[ coord ] * ( 1.0 - random );\
      }\
   }\
\
/* Transform these positions. We process them in a single batch in\
   order to minimise the overheads in doing this. */\
   (void) astTransform( mapdata->mapping, pset_in, mapdata->forward,\
                        pset_out );\
\
/* Indicate that the position buffer is now full. */\
   batch = 0;\
}

/* Fill the position buffer using the above macro. (Note that because
   we do not yet have an estimate of the size of the active region,
   this does not change the sampled region size from our earlier
   initialised values. */
   FILL_POSITION_BUFFER;

/* Iterate. */
/* ======== */
/* Loop to perform up to "maxiter" iterations to estimate the global
   minimum and maximum. */
   for ( iter = 0; astOK && ( iter < maxiter ); iter++ ) {

/* Determine the search accuracy. */
/* ============================== */
/* Decide the accuracy to which local extrema should be found. The
   intention here is to optimise performance, especially where one
   extremum lies near zero and so could potentially be found to
   unnecessarily high precision. If we make a mis-assumption (the code
   below is not fool-proof), we will slow things down for this
   iteration, but the error will be corrected in future iterations
   once better estimates are available. */

/* If we have no current estimate of either global extremum, we assume
   the values we eventually obtain will be of order unity and required
   to machine single precision. */
      acc = (double) FLT_EPSILON;

/* If we already have an estimate of both global extrema, we set the
   accuracy so that the difference between them will be known to
   machine single precision. */
      if ( ( *lbnd != AST__BAD ) && ( *ubnd != AST__BAD ) ) {
         acc = fabs( *ubnd - *lbnd ) * (double) FLT_EPSILON;

/* If we have an estimate of only one global extremum, we assume that
   the difference between the two global extrema will eventually be of
   the same order as the estimate we currently have, so long as this
   is not less than unity. */
      } else if ( *lbnd != AST__BAD ) {
         if ( fabs( *lbnd ) > 1.0 ) acc = fabs( *lbnd) * (double) FLT_EPSILON;
      } else if ( *ubnd != AST__BAD ) {
         if ( fabs( *ubnd ) > 1.0 ) acc = fabs( *ubnd) * (double) FLT_EPSILON;
      }

/* Search for a new local minimum. */
/* =============================== */
/* If we are still searching for the global minimum, then obtain a set
   of starting coordinates from which to find a new local minimum. */
      if ( !done_min ) {

/* On the first iteration, start searching at the position where the
   best estimate of the global minimum (if any) has previously been
   found. We know that this produces non-bad transformed
   coordinates. */
         bad = 0;
         if ( !iter && ( *lbnd != AST__BAD ) ) {
            for ( coord = 0; coord < ncoord; coord++ ) {
               x[ coord ] = xl[ coord ];
            }

/* Otherwise, if no estimate of the global minimum is available, then
   start searching at the position where the best estimate of the
   global maximum (if any) has been found. This may be a long way from
   a local minimum, but at least it will yield a non-bad value for the
   Mapping function, so some sort of estimate of the global minimum
   will be obtained. This is important in cases where finding the
   active region of the function is the main problem. Note that this
   condition can only occur once, since the global minimum will have
   an estimate on the next iteration. */
         } else if ( ( *lbnd == AST__BAD ) && ( *ubnd != AST__BAD ) ) {
            for ( coord = 0; coord < ncoord; coord++ ) {
               x[ coord ] = xu[ coord ];
            }

/* Having exhausted the above possibilities, we use pseudo-random
   starting positions which are uniformly distributed throughout the
   search volume. First check to see if the buffer containing such
   positions is empty and refill it if necessary. */
         } else {
            if ( batch >= nbatch ) FILL_POSITION_BUFFER;

/* Test the next available set of output (transformed) coordinates in
   the position buffer to see if they are bad. */
            if ( astOK ) {
               for ( coord = 0; coord < mapdata->nout; coord++ ) {
                  bad = ( ptr_out[ coord ][ batch ] == AST__BAD );
                  if ( bad ) break;
               }

/* If not, we have a good starting position for finding a local
   minimum, so extract the corresponding input coordinates. */
               if ( !bad ) {
                  for ( coord = 0; coord < ncoord; coord++ ) {
                     x[ coord ] = ptr_in[ coord ][ batch ];
                  }
               }

/* Increment the position buffer location. */
               batch++;
            }
         }

/* If we do not have a good starting position, we can't do anything
   more on this iteration. A new position will be obtained and tested
   on the next iteration and this (we hope) will eventually identify a
   suitable starting point. */
         if ( astOK && !bad ) {

/* Form estimates of the lower and upper limits of the active region
   from the starting positions used. */
            for ( coord = 0; coord < ncoord; coord++ ) {
               if ( x[ coord ] < active_lo[ coord ] ) {
                  active_lo[ coord ] = x[ coord ];
               }
               if ( x[ coord ] > active_hi[ coord ] ) {
                  active_hi[ coord ] = x[ coord ];
               }
            }

/* Indicate that the Mapping function should be negated (because we
   want a local minimum) and then search for a local maximum in this
   negated function. If the result is non-bad (as it should always be,
   barring an error), then negate it to obtain the value of the local
   minimum found. */
            mapdata->negate = 1;
            new_min = LocalMaximum( mapdata, acc, 0.01, x );
            if ( new_min != AST__BAD ) {
               new_min = -new_min;

/* Update the estimates of the lower and upper bounds of the active
   region to take account of where the minimum was found. */
               for ( coord = 0; coord < ncoord; coord++ ) {
                  if ( x[ coord ] < active_lo[ coord ] ) {
                     active_lo[ coord ] = x[ coord ];
                  }
                  if ( x[ coord ] > active_hi[ coord ] ) {
                     active_hi[ coord ] = x[ coord ];
                  }
               }

/* Count the number of times we successfully locate a local minimum
   (ignoring the fact they might all be the same one). */
               nmin++;

/* Update the global minimum. */
/* ========================== */
/* If this is the first estimate of the global minimum, then set to
   one the count of the number of consecutive iterations where this
   estimate remains unchanged. Store the minimum value and its
   position. */
               if ( *lbnd == AST__BAD ) {
                  nsame_min = 1;
                  *lbnd = new_min;
                  for ( coord = 0; coord < ncoord; coord++ ) {
                     xl[ coord ] = x[ coord ];
                  }

/* Otherwise, test if this local minimum is lower than the previous
   estimate of the global minimum. If so, then reset the count of
   unchanged estimates of the global mimimum to one if the difference
   exceeds the accuracy with which the minimum was found (i.e. if we
   have found a significantly different minimum). Otherwise, just
   increment this count (because we have found the same minimum but by
   chance with slightly improved accuracy). Store the new minimum and
   its position. */
               } else if ( new_min < *lbnd ) {
                  nsame_min = ( ( *lbnd - new_min ) > acc ) ? 1 :
                                                              nsame_min + 1;
                  *lbnd = new_min;
                  for ( coord = 0; coord < ncoord; coord++ ) {
                     xl[ coord ] = x[ coord ];
                  }

/* If the latest local minimum is no improvement on previous estimates
   of the global minimum, then increment the count of unchanged
   estimates of the global mimimum, but do not save the new one. */
               } else {
                  nsame_min++;
               }

/* Determine if a satisfactory estimate of the global minimum has been
   obtained.  It has if the number of consecutive local minima which
   have not significantly improved the estimate is at least equal to
   "minsame", and at least 30% of the total number of local minima
   found. */
               if ( ( nsame_min >= minsame ) &&
                    ( nsame_min >= (int) ( 0.3f * (float) nmin + 0.5f ) ) ) {
                  done_min = 1;
               }
            }
         }
      }

/* Search for a new local maximum. */
/* =============================== */
/* Now repeat all of the above to find a new local maximum which
   estimates the global maximum. */
      if ( !done_max ) {

/* Choose a suitable starting position, based on one already available
   if appropriate. */
         if ( !iter && ( *ubnd != AST__BAD ) ) {
            for ( coord = 0; coord < ncoord; coord++ ) {
               x[ coord ] = xu[ coord ];
            }

         } else if ( ( *ubnd == AST__BAD ) && ( *lbnd != AST__BAD ) ) {
            for ( coord = 0; coord < ncoord; coord++ ) {
               x[ coord ] = xl[ coord ];
            }

/* Otherwise use a pseudo-random position, refilling the position
   buffer if necessary. Check if the transformed coordinates are
   bad. */
         } else {
            if ( batch >= nbatch ) FILL_POSITION_BUFFER;
            if ( astOK ) {
               for ( coord = 0; coord < mapdata->nout; coord++ ) {
                  bad = ( ptr_out[ coord ][ batch ] == AST__BAD );
                  if ( bad ) break;
               }
               if ( !bad ) {
                  for ( coord = 0; coord < ncoord; coord++ ) {
                     x[ coord ] = ptr_in[ coord ][ batch ];
                  }
               }
               batch++;
            }
         }

/* If the coordinates are OK, update the active region limits. */
         if ( astOK && !bad ) {
            for ( coord = 0; coord < ncoord; coord++ ) {
               if ( x[ coord ] < active_lo[ coord ] ) {
                  active_lo[ coord ] = x[ coord ];
               }
               if ( x[ coord ] > active_hi[ coord ] ) {
                  active_hi[ coord ] = x[ coord ];
               }
            }

/* Find a local maximum in the Mapping function. */
            mapdata->negate = 0;
            new_max = LocalMaximum( mapdata, acc, 0.01, x );
            if ( new_max != AST__BAD ) {

/* Use the result to further update the active region limits. */
               for ( coord = 0; coord < ncoord; coord++ ) {
                  if ( x[ coord ] < active_lo[ coord ] ) {
                     active_lo[ coord ] = x[ coord ];
                  }
                  if ( x[ coord ] > active_hi[ coord ] ) {
                     active_hi[ coord ] = x[ coord ];
                  }
               }

/* Count the number of local maxima found. */
               nmax++;

/* Update the estimate of the global maximum. */
               if ( *ubnd == AST__BAD ) {
                  nsame_max = 1;
                  *ubnd = new_max;
                  for ( coord = 0; coord < ncoord; coord++ ) {
                     xu[ coord ] = x[ coord ];
                  }

               } else if ( new_max > *ubnd ) {
                  nsame_max = ( ( new_max - *ubnd ) > acc ) ? 1 :
                                                              nsame_max + 1;
                  *ubnd = new_max;
                  for ( coord = 0; coord < ncoord; coord++ ) {
                     xu[ coord ] = x[ coord ];
                  }

               } else {
                  nsame_max++;
               }

/* Test for a satisfactory global maximum estimate. */
               if ( ( nsame_max >= minsame ) &&
                    ( nsame_max >= (int) ( 0.3f * (float) nmax + 0.5 ) ) ) {
                  done_max = 1;
               }
            }
         }
      }

/* Quit iterating once both the global minimum and the global maximum
   have been found. */
      if ( done_min && done_max ) break;
   }

/* Free workspace. */
   active_hi = astFree( active_hi );
   active_lo = astFree( active_lo );
   sample_hi = astFree( sample_hi );
   sample_lo = astFree( sample_lo );
   sample_width = astFree( sample_width );
   x = astFree( x );

/* Annul temporary PointSets. */
   pset_in = astAnnul( pset_in );
   pset_out = astAnnul( pset_out );

/* If the global minimum has been found, attempt to polish the result
   to machine precision by requesting that it be found with an
   accuracy tolerance of zero (subject to the maximum number of
   iterations that LocalMaximum will perform,). */
   if ( astOK ) {
      if ( *lbnd != AST__BAD ) {
         mapdata->negate = 1;
         *lbnd = LocalMaximum( mapdata, 0.0, sqrt( DBL_EPSILON ), xl );
         if ( *lbnd != AST__BAD ) *lbnd = - *lbnd;
      }

/* Similarly polish the estimate of the global maximum. */
      if ( *ubnd != AST__BAD ) {
         mapdata->negate = 0;
         *ubnd = LocalMaximum( mapdata, 0.0, sqrt( DBL_EPSILON ), xu );
      }

/* If either extremum could not be found, then report an error. */
      if ( ( *lbnd == AST__BAD ) || ( *ubnd == AST__BAD ) ) {
         astError( AST__MBBNF, "astMapBox(%s): No valid output coordinates "
                   "(after %d test points).", astGetClass( mapdata->mapping ),
                   2 * maxiter );
      }

/* If an error occurred, then return bad extremum values and
   coordinates. */
      if ( !astOK ) {
         *lbnd = AST__BAD;
         *ubnd = AST__BAD;
         for ( coord = 0; coord < ncoord; coord++ ) {
            xl[ coord ] = AST__BAD;
            xu[ coord ] = AST__BAD;
         }
      }
   }

/* Undefine macros local to this function. */
#undef FILL_POSITION_BUFFER
}

static void InitVtab( AstMappingVtab *vtab ) {
/*
*  Name:
*     InitVtab

*  Purpose:
*     Initialise a virtual function table for a Mapping.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     void InitVtab( AstMappingVtab *vtab )

*  Class Membership:
*     Mapping member function.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the Mapping class.

*  Parameters:
*     vtab
*        Pointer to the virtual function table. The components used by
*        all ancestral classes should already have been initialised.
*/

/* Local Variables: */
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAMapping) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_init variable to generate this unique value. */
   vtab->check = &class_init;

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->ClearInvert = ClearInvert;
   vtab->ClearReport = ClearReport;
   vtab->GetInvert = GetInvert;
   vtab->GetNin = GetNin;
   vtab->GetNout = GetNout;
   vtab->GetReport = GetReport;
   vtab->GetTranForward = GetTranForward;
   vtab->GetTranInverse = GetTranInverse;
   vtab->Invert = Invert;
   vtab->MapBox = MapBox;
   vtab->MapList = MapList;
   vtab->MapMerge = MapMerge;
   vtab->ReportPoints = ReportPoints;
   vtab->ResampleB = ResampleB;
   vtab->ResampleD = ResampleD;
   vtab->ResampleF = ResampleF;
   vtab->ResampleI = ResampleI;
   vtab->ResampleL = ResampleL;
   vtab->ResampleLD = ResampleLD;
   vtab->ResampleS = ResampleS;
   vtab->ResampleUB = ResampleUB;
   vtab->ResampleUI = ResampleUI;
   vtab->ResampleUL = ResampleUL;
   vtab->ResampleUS = ResampleUS;
   vtab->SetInvert = SetInvert;
   vtab->SetReport = SetReport;
   vtab->Simplify = Simplify;
   vtab->TestInvert = TestInvert;
   vtab->TestReport = TestReport;
   vtab->Tran1 = Tran1;
   vtab->Tran2 = Tran2;
   vtab->TranN = TranN;
   vtab->TranP = TranP;
   vtab->Transform = Transform;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;

   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;
   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;
   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;
   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;

/* Declare the destructor, copy constructor and dump function. */
   astSetDelete( vtab, Delete );
   astSetCopy( vtab, Copy );
   astSetDump( vtab, Dump, "Mapping", "Mapping between coordinate systems" );
}

static void Invert( AstMapping *this ) {
/*
*++
*  Name:
c     astInvert
f     AST_INVERT

*  Purpose:
*     Invert a Mapping.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "mapping.h"
c     void astInvert( AstMapping *this )
f     CALL AST_INVERT( THIS, STATUS )

*  Class Membership:
*     Mapping method.

*  Description:
c     This function inverts a Mapping by reversing the boolean sense
f     This routine inverts a Mapping by reversing the boolean sense
*     of its Invert attribute. If this attribute is zero (the
*     default), the Mapping will transform coordinates in the way
*     specified when it was created. If it is non-zero, the input and
*     output coordinates will be inter-changed so that the direction
*     of the Mapping is reversed. This will cause it to display the
*     inverse of its original behaviour.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Mapping.
f     STATUS = INTEGER (Given and Returned)
f        The global status.
*--
*/

/* Local Variables: */
   int invert;                   /* New Invert attribute value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Determine the new Invert attribute value. */
   invert = !astGetInvert( this );

/* Clear the old value. */
   astClearInvert( this );

/* If the resulting default value is not the one required, then set a
   new value explicitly. */
   if ( astGetInvert( this ) != invert ) astSetInvert( this, invert );
}

static double LocalMaximum( const MapData *mapdata, double acc, double fract,
                            double x[] ) {
/*
*  Name:
*     LocalMaximum

*  Purpose:
*     Find a local maximum in a Mapping function.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     double LocalMaximum( const MapData *mapdata, double acc, double fract,
*                          double x[] );

*  Class Membership:
*     Mapping member function.

*  Description:
*     This function finds a local maximum in the Mapping function
*     supplied.  It employs the modified simplex method (as
*     implemented by UphillSimplex), but repeatedly re-starts the
*     simplex algorithm and tests for convergence of successive
*     maxima, so as to further improve robustness on difficult
*     problems.

*  Parameters:
*     mapdata
*        Pointer to a MapData structure describing the Mapping
*        function, its coordinate constraints, etc.
*     acc
*        The required accuracy with which the maximum is to be found.
*     fract
*        A value between 0.0 and 1.0 which determines the initial step
*        length along each coordinate axis. It should be given as a
*        fraction of the difference between the upper and lower
*        constraint values for each axis (as specified in the
*        "mapdata" structure).
*     x
*        Pointer to an array of double containing the coordinates of
*        an initial estimate of the position of the maximum. On exit,
*        this will be updated to contain the best estimate of the
*        maximum's position, as found by this function.

*  Returned Value:
*     The best estimate of the Mapping function's maximum value.

*  Notes:
*     - A value of AST__BAD will be returned, and no useful
*     information about a solution will be produced, if this function
*     is invoked with the global error status set or if it should fail
*     for any reason.
*/

/* Local Constants: */
   const int maxcall = 2500;     /* Maximum number of function evaluations */
   const int maxiter = 10;       /* Maximum number of iterations */

/* Local Variables: */
   double *dx;                   /* Pointer to array of step lengths */
   double err;                   /* Simplex error estimate */
   double maximum;               /* Simplex maximum value */
   double middle;                /* Middle coordinate between bounds */
   double result;                /* Result value to return */
   int coord;                    /* Loop counter for coordinates */
   int done;                     /* Iterations complete? */
   int iter;                     /* Loop counter for iterations */
   int ncall;                    /* Number of function calls (junk) */

/* Initialise. */
   result = AST__BAD;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Allocate workspace. */
   dx = astMalloc( sizeof( double ) * (size_t) mapdata->nin );

/* Perform iterations to repeatedly identify a local maximum. */
   for ( iter = 0; astOK && ( iter < maxiter ); iter++ ) {

/* Set up initial step lengths along each coordinate axis, adjusting
   their signs to avoid placing points outside the coordinate
   constraints (i.e. step away from the closer boundary on each
   axis). */
      for ( coord = 0; coord < mapdata->nin; coord++ ) {
         middle = 0.5 * ( mapdata->lbnd[ coord ] + mapdata->ubnd[ coord ] );
         dx[ coord ] = fract * ( mapdata->ubnd[ coord ] -
                                 mapdata->lbnd[ coord ] );
         if ( x[ coord ] > middle ) dx[ coord ] = -dx[ coord ];
      }

/* Find an approximation to a local maximum using the simplex method
   and check for errors. */
      maximum = UphillSimplex( mapdata, acc, maxcall, dx, x, &err, &ncall );
      if ( astOK ) {

/* Use this maximum value if no previous maximum has been found. */
         if ( result == AST__BAD ) {
            result = maximum;

/* Otherwise use it only if it improves on the previous maximum. */
         } else if ( maximum >= result ) {

/* We iterate, re-starting the simplex algorithm from its previous
   best position so as to guard against premature false
   convergence. Iterations continue until the improvement in the
   maximum is no greater than the required accuracy (and the simplex
   algorithm itself has converged to the required accuracy). Note when
   iterations should cease. */
            done = ( ( ( maximum - result ) <= acc ) && ( err <= acc ) );

/* Store the best maximum and quit iterating if appropriate. */
            result = maximum;
            if ( done ) break;
         }

/* Otherwise, decrement the initial step size for the next iteration. */
         fract /= 1000.0;
      }
   }
   
/* Free the workspace. */
   dx = astFree( dx );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = AST__BAD;

/* return the result. */
   return result;
}

static void MapBox( AstMapping *this,
                    const double lbnd_in[], const double ubnd_in[],
                    int forward, int coord_out,
                    double *lbnd_out, double *ubnd_out,
                    double xl[], double xu[] ) {
/*
*+
*  Name:
*     astMapBox

*  Purpose:
*     Find a bounding box for a Mapping.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "mapping.h"
*     void astMapBox( AstMapping *this,
*                     const double lbnd_in[], const double ubnd_in[],
*                     int forward, int coord_out,
*                     double *lbnd_out, double *ubnd_out,
*                     double xl[], double xu[] );

*  Class Membership:
*     Mapping method.

*  Description:
*     This function allows you to find the "bounding box" which just
*     encloses another box after it has been transformed by a Mapping
*     (using either its forward or inverse transformation). A typical
*     use might be to calculate the size which an image would have
*     after being transformed by the Mapping.
*
*     The function works on one dimension at a time. When supplied
*     with the lower and upper bounds of a rectangular region (box) of
*     input coordinate space, it finds the lowest and highest values
*     taken by a nominated output coordinate within that
*     region. Optionally, it also returns the input coordinates where
*     these bounding values are attained. It should be used repeatedly
*     if the extent of the bounding box is required in more than one
*     dimension.

*  Parameters:
*     this
*        Pointer to the Mapping.
*     lbnd_in
*        Pointer to an array of double, with one element for each
*        Mapping input coordinate. This should contain the lower bound
*        of the input box in each dimension.
*     ubnd_in
*        Pointer to an array of double, with one element for each
*        Mapping input coordinate. This should contain the upper bound
*        of the input box in each dimension.
*
*        Note that it is permissible for the lower bound to exceed the
*        corresponding upper bound, as the values will simply be
*        swapped before use.
*     forward
*        If this value is non-zero, then the Mapping's forward
*        transformation will be used to transform the input
*        box. Otherwise, its inverse transformation will be used.
*
*        (If the inverse transformation is selected, then references
*        to "input" and "output" coordinates in this description
*        should be transposed. For example, the size of the "lbnd_in"
*        and "ubnd_in" arrays should match the number of output
*        coordinates, as given by the Mapping's Nout attribute.)
*     coord_out
*        The (zero-based) index of the output coordinate for which the
*        lower and upper bounds are required.
*     lbnd_out
*        Pointer to a double in which to return the lowest value taken
*        by the nominated output coordinate within the specified
*        region of input coordinate space.
*     ubnd_out
*        Pointer to a double in which to return the highest value
*        taken by the nominated output coordinate within the specified
*        region of input coordinate space.
*     xl
*        An optional pointer to an array of double, with one element
*        for each Mapping input coordinate. If given, this array will
*        be filled with the coordinates of an input point (although
*        not necessarily a unique one) for which the nominated output
*        coordinate takes the lower bound value returned in
*        "*lbnd_out".
*
*        If these coordinates are not required, a NULL pointer may be
*        supplied.
*     xu
*        An optional pointer to an array of double, with one element
*        for each Mapping input coordinate. If given, this array will
*        be filled with the coordinates of an input point (although
*        not necessarily a unique one) for which the nominated output
*        coordinate takes the upper bound value returned in
*        "*ubnd_out".
*
*        If these coordinates are not required, a NULL pointer may be
*        supplied.

*  Notes:
*     - Any input points which are transformed by the Mapping to give
*     output coordinates containing the value AST__BAD are regarded as
*     invalid and are ignored, They will make no contribution to
*     determining the output bounds, even although the nominated
*     output coordinate might still have a valid value at such points.
*     - An error will occur if the required output bounds cannot be
*     found. Typically, this might occur if all the input points which
*     the function considers turn out to be invalid (see above). The
*     number of points considered before generating such an error is
*     quite large, however, so this is unlikely to occur by accident
*     unless valid points are restricted to a very small subset of the
*     input coordinate space.
*     - The values returned via "lbnd_out", "ubnd_out", "xl" and "xu"
*     will be set to the value AST__BAD if this function should fail
*     for any reason. Their initial values on entry will not be
*     altered if the function is invoked with the global error status
*     set.
*-

*  Implementation Notes:
*     - This function implements the basic astMapBox method available
*     via the protected interface to the Mapping class. The public
*     interface to this method is provided by the astMapBoxId_
*     function.
*/

/* Local Variables: */
   MapData mapdata;              /* Structure to describe Mapping function */
   double *x_l;                  /* Pointer to coordinate workspace */
   double *x_u;                  /* Pointer to coordinate workspace */
   double lbnd;                  /* Required lower bound */
   double ubnd;                  /* Required upper bound */
   int coord;                    /* Loop counter for coordinates. */
   int nin;                      /* Effective number of input coordinates */
   int nout;                     /* Effective number of output coordinates */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain the effective numbers of input and output coordinates for
   the Mapping, taking account of which transformation is to be
   used. */
   nin = forward ? astGetNin( this ) : astGetNout( this );
   nout = forward ? astGetNout( this ) : astGetNin( this );

/* Check that the output coordinate index supplied is valid and report
   an error if it is not. Use public (one-based) coordinate numbering
   in the error message. */
   if ( astOK ) {
      if ( ( coord_out < 0 ) || ( coord_out >= nout ) ) {
         astError( AST__BADCI, "astMapBox(%s): Output coordinate index (%d) "
                   "invalid - it should be in the range 1 to %d.",
                   astGetClass( this ), coord_out + 1, nout );
      }
   }

/* Initialise a MapData structure to describe the Mapping function
   whose limits are to be found.  Since it may be evaluated many
   times, we attempt to simplify the Mapping supplied. */
   if ( astOK ) {
      mapdata.mapping = astSimplify( this );

/* Store the number of input/output coordinates and the index of the
   output coordinate in which we are interested. */
      mapdata.nin = nin;
      mapdata.nout = nout;
      mapdata.coord = coord_out;

/* Note which Mapping transformation is being used. */
      mapdata.forward = forward;

/* Store pointers to arrays which will contain the input coordinate
   bounds. */
      mapdata.lbnd = astMalloc( sizeof( double ) * (size_t) nin );
      mapdata.ubnd = astMalloc( sizeof( double ) * (size_t) nin );

/* Create PointSets for passing coordinate data to and from the
   Mapping. */
      mapdata.pset_in = astPointSet( 1, nin, "" );
      mapdata.pset_out = astPointSet( 1, nout, "" );

/* Obtain pointers to these PointSets' coordinate arrays. */
      mapdata.ptr_in = astGetPoints( mapdata.pset_in );
      mapdata.ptr_out = astGetPoints( mapdata.pset_out );

/* Allocate workspace for the returned input coordinates. */
      x_l = astMalloc( sizeof( double ) * (size_t) nin );
      x_u = astMalloc( sizeof( double ) * (size_t) nin );
      if ( astOK ) {

/* Initialise the output bounds and corresponding input coordinates to
   "unknown". */
         lbnd = AST__BAD;
         ubnd = AST__BAD;
         for ( coord = 0; coord < nin; coord++ ) {
            x_l[ coord ] = AST__BAD;
            x_u[ coord ] = AST__BAD;

/* Initialise the input bounds, ensuring they are the correct way
   around (if not already supplied this way). */
            mapdata.lbnd[ coord ] = ( lbnd_in[ coord ] < ubnd_in[ coord ] ) ?
                                      lbnd_in[ coord ] : ubnd_in[ coord ];
            mapdata.ubnd[ coord ] = ( ubnd_in[ coord ] > lbnd_in[ coord ] ) ?
                                      ubnd_in[ coord ] : lbnd_in[ coord ];
         }

/* First examine a set of special input points to obtain an initial
   estimate of the required output bounds. Do this only so long as the
   number of points involved is not excessive. */
         if ( nin <= 12 ) SpecialBounds( &mapdata, &lbnd, &ubnd, x_l, x_u );

/* Then attempt to refine this estimate using a global search
   algorithm. */
         GlobalBounds( &mapdata, &lbnd, &ubnd, x_l, x_u );

/* If an error occurred, generate a contextual error message. */
         if ( !astOK ) {
            astError( astStatus, "Unable to find a bounding box for a %s.",
                      astGetClass( this ) );
         }
      }

/* Return the output bounds and, if required, the input coordinate
   values which correspond with them. */
      if ( astOK ) {
         *lbnd_out = lbnd;
         *ubnd_out = ubnd;
         for ( coord = 0; coord < nin; coord++ ) {
            if ( xl ) xl[ coord ] = x_l[ coord ];
            if ( xu ) xu[ coord ] = x_u[ coord ];
         }
      }

/* Annul the simplified Mapping pointer and the temporary
   PointSets. Also free the workspace. */
      mapdata.mapping = astAnnul( mapdata.mapping );
      mapdata.lbnd = astFree( mapdata.lbnd );
      mapdata.ubnd = astFree( mapdata.ubnd );
      mapdata.pset_in = astAnnul( mapdata.pset_in );
      mapdata.pset_out = astAnnul( mapdata.pset_out );
      x_l = astFree( x_l );
      x_u = astFree( x_u );
   }
      
/* If an error occurred, then return bad bounds values and
   coordinates. */
   if ( !astOK ) {
      *lbnd_out = AST__BAD;
      *ubnd_out = AST__BAD;
      for ( coord = 0; coord < nin; coord++ ) {
         if ( xl ) xl[ coord ] = AST__BAD;
         if ( xu ) xu[ coord ] = AST__BAD;
      }
   }
}

static double MapFunction( const MapData *mapdata, const double in[],
                           int *ncall ) {
/*
*  Name:
*     MapFunction

*  Purpose:
*     Return the value of a selected transformed coordinate.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     double MapFunction( const MapData *mapdata, const double in[],
*                         int *ncall );

*  Class Membership:
*     Mapping member function.

*  Description:
*     This function takes a set of input coordinates and applies a
*     Mapping's coordinate transformation to them. It then returns the
*     value of one of the transformed coordinates.
*
*     It is provided for use by optimisation functions (e.g. those
*     used for finding bounding boxes). The Mapping to be used and
*     associated parameters (such as constraints on the range of input
*     coordinates and the index of the output coordinate to be
*     returned) are supplied in a MapData structure. The value
*     returned will be negated if the "negate" component of this
*     structure is non-zero.
*
*     The value AST__BAD will be returned by this function if the
*     input coordinates lie outside the constrained range given in
*     the MapData structure, or if any of the transformed output
*     coordinates is bad.

*  Parameters:
*     mapdata
*        Pointer to a MapData structure which describes the Mapping to
*        be used.
*     in
*        A double array containing the input coordinates of a single point.
*     ncall
*        Pointer to an int containing a count of the number of times
*        the Mapping's coordinate transformation has been used. This
*        value will be updated to reflect any use made by this
*        function. Normally, this means incrementing the value by 1,
*        but this will be omitted if the input coordinates supplied
*        are outside the constrained range so that no transformation
*        is performed.

*  Returned Value:
*     The selected output coordinate value, or AST__BAD, as appropriate.

*  Notes:
*     - A value of AST__BAD will be returned if this function is
*     invoked with the global error status set, or if it should fail
*     for any reason.
*/

/* Local Variables: */
   double result;                /* Result to be returned */
   int bad;                      /* Output coordinates invalid? */
   int coord_in;                 /* Loop counter for input coordinates */
   int coord_out;                /* Loop counter for output coordinates */
   int outside;                  /* Input point outside bounds? */

/* Initialise. */
   result = AST__BAD;

/* Check the global error status. */
   if ( !astOK ) return result;

/* See if the input point lies outside the required bounds. */
   outside = 0;
   for ( coord_in = 0; coord_in < mapdata->nin; coord_in++ ) {
      if ( ( in[ coord_in ] < mapdata->lbnd[ coord_in ] ) ||
           ( in[ coord_in ] > mapdata->ubnd[ coord_in ] ) ) {
         outside = 1;
         break;
      }

/* Also store the input coordinates in the memory associated with the
   Mapping's input PointSet. */
      mapdata->ptr_in[ coord_in ][ 0 ] = in[ coord_in ];
   }

/* If the input coordinates are within bounds, transform them, using the
   PointSets identified in the "mapdata" structure. */
   if ( !outside ) {
      (void) astTransform( mapdata->mapping, mapdata->pset_in,
                           mapdata->forward, mapdata->pset_out );

/* Increment the number of calls to astTransform and check the error
   status. */
      ( *ncall )++;
      if ( astOK ) {

/* If OK, test if any of the output coordinates is bad. */
         bad = 0;
         for ( coord_out = 0; coord_out < mapdata->nout; coord_out++ ) {
            if ( mapdata->ptr_out[ coord_out ][ 0 ] == AST__BAD ) {
               bad = 1;
               break;
            }
         }

/* If not, then extract the required output coordinate, negating it if
   necessary. */
         if ( !bad ) {
            result = mapdata->ptr_out[ mapdata->coord ][ 0 ];
            if ( mapdata->negate ) result = -result;
         }
      }
   }

/* Return the result. */
   return result;
}

static void MapList( AstMapping *this, int series, int invert, int *nmap,
                     AstMapping ***map_list, int **invert_list ) {
/*
*+
*  Name:
*     astMapList

*  Purpose:
*     Decompose a Mapping into a sequence of simpler Mappings.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "mapping.h"
*     void astMapList( AstMapping *this, int series, int invert, int *nmap,
*                      AstMapping ***map_list, int **invert_list )

*  Class Membership:
*     Mapping method.

*  Description:
*     This function decomposes a Mapping (which, in derived classes,
*     may be a compound Mapping) into a sequence of simpler Mappings
*     which may be applied in sequence to achieve the same effect. The
*     Mapping is decomposed as far as possible, but it is not
*     guaranteed that this will necessarily yield any more than one
*     Mapping, which may actually be the original one supplied.
*
*     This function is provided to support both the simplification of
*     compound Mappings, and the analysis of Mapping structure so that
*     particular forms can be recognised.

*  Parameters:
*     this
*        Pointer to the Mapping to be decomposed (the Mapping is not
*        actually modified by this function).
*     series
*        If this value is non-zero, an attempt will be made to
*        decompose the Mapping into a sequence of equivalent Mappings
*        which can be applied in series (i.e. one after the other). If
*        it is zero, the decomposition will instead yield Mappings
*        which can be applied in parallel (i.e. on successive sub-sets
*        of the input/output coordinates).
*     invert
*        The value to which the Mapping's Invert attribute is to be
*        (notionally) set before performing the
*        decomposition. Normally, the value supplied here will be the
*        actual Invert value obtained from the Mapping (e.g. using
*        astGetInvert).  Sometimes, however, when a Mapping is
*        encapsulated within another structure, that structure may
*        retain an Invert value (in order to prevent external
*        interference) which should be used instead.
*
*        Note that the actual Invert value of the Mapping supplied is
*        not used (or modified) by this function.
*     nmap
*        The address of an int which holds a count of the number of
*        individual Mappings in the decomposition. On entry, this
*        should count the number of Mappings already in the
*        "*map_list" array (below). On exit, it is updated to include
*        any new Mappings appended by this function.
*     map_list
*        Address of a pointer to an array of Mapping pointers. On
*        entry, this array pointer should either be NULL (if no
*        Mappings have yet been obtained) or should point at a
*        dynamically allocated array containing Mapping pointers
*        ("*nmap" in number) which have been obtained from a previous
*        invocation of this function.
*
*        On exit, the dynamic array will be enlarged to contain any
*        new Mapping pointers that result from the decomposition
*        requested. These pointers will be appended to any previously
*        present, and the array pointer will be updated as necessary
*        to refer to the enlarged array (any space released by the
*        original array will be freed automatically).
*
*        The new Mapping pointers returned will identify a sequence of
*        Mappings which, when applied in order, will perform a forward
*        transformation equivalent to that of the original Mapping
*        (after its Invert flag has first been set to the value
*        requested above). The Mappings should be applied in series or
*        in parallel according to the type of decomposition requested.
*
*        All the Mapping pointers returned by this function should be
*        annulled by the caller, using astAnnul, when no longer
*        required. The dynamic array holding these pointers should
*        also be freed, using astFree.
*     invert_list
*        Address of a pointer to an array of int. On entry, this array
*        pointer should either be NULL (if no Mappings have yet been
*        obtained) or should point at a dynamically allocated array
*        containing Invert attribute values ("*nmap" in number) which
*        have been obtained from a previous invocation of this
*        function.
*
*        On exit, the dynamic array will be enlarged to contain any
*        new Invert attribute values that result from the
*        decomposition requested. These values will be appended to any
*        previously present, and the array pointer will be updated as
*        necessary to refer to the enlarged array (any space released
*        by the original array will be freed automatically).
*
*        The new Invert values returned identify the values which must
*        be assigned to the Invert attributes of the corresponding
*        Mappings (whose pointers are in the "*map_list" array) before
*        they are applied. Note that these values may differ from the
*        actual Invert attribute values of these Mappings, which are
*        not relevant.
*
*        The dynamic array holding these values should be freed by the
*        caller, using astFree, when no longer required.

*  Notes:
*     - It is unspecified to what extent the original Mapping and the
*     individual (decomposed) Mappings are
*     inter-dependent. Consequently, the individual Mappings cannot be
*     modified without risking modification of the original.
*     - If this function is invoked with the global error status set,
*     or if it should fail for any reason, then the *nmap value, the
*     list of Mapping pointers and the list of Invert values will all
*     be returned unchanged.
*- 
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Since we are dealing with a basic Mapping, only one new Mapping
   pointer will be returned. Extend the dynamic arrays to accommodate
   this Mapping. */
   *map_list = astGrow( *map_list, *nmap + 1, sizeof( AstMapping * ) );
   *invert_list = astGrow( *invert_list, *nmap + 1, sizeof( int ) );
   if ( astOK ) {

/* Return the invert flag value for the Mapping and a clone of the
   Mapping pointer. */
      ( *invert_list )[ *nmap ] = ( invert != 0 );
      ( *map_list )[ *nmap ] = astClone( this );

/* If OK, return the new Mapping count. */
      if ( astOK ) ( *nmap )++;
   }
}

static int MapMerge( AstMapping *this, int where, int series, int *nmap,
                     AstMapping ***map_list, int **invert_list ) {
/*
*+
*  Name:
*     astMapMerge

*  Purpose:
*     Simplify a sequence of Mappings.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "mapping.h"
*     int astMapMerge( AstMapping *this, int where, int series, int *nmap,
*                      AstMapping ***map_list, int **invert_list )

*  Class Membership:
*     Mapping method.

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated Mapping in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated Mapping with one which it
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
*        Pointer to the nominated Mapping which is to be merged with
*        its neighbours. This should be a cloned copy of the Mapping
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        Mapping it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated Mapping resides.
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

*  Returned Value:
*     If simplification was possible, the function returns the index
*     in the "map_list" array of the first element which was
*     modified. Otherwise, it returns -1 (and makes no changes to the
*     arrays supplied).

*  Notes:
*     - A value of -1 will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* This is the default method which is inherited by all Mappings which
   do not explicitly provide their own simplification method. Return
   -1 to indicate that no simplification is provided. */
   return -1;
}

static double NewVertex( const MapData *mapdata, int lo, double scale,
                         double x[], double f[], int *ncall, double xnew[] ) {
/*
*  Name:
*     NewVertex

*  Purpose:
*     Locate a new vertex for a simplex.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     double NewVertex( const MapData *mapdata, int lo, double scale,
*                       double x[], double f[], int *ncall, double xnew[] );

*  Class Membership:
*     Mapping member function.

*  Description:
*     This function is provided for use during optimisation of a
*     Mapping function using the simplex method. It generates the
*     coordinates of a new simplex vertex and evaluates the Mapping
*     function at that point.  If the function's value is better then
*     (i.e. larger than) the value at the previously worst vertex,
*     then it is used to replace that vertex.

*  Parameters:
*     mapdata
*        Pointer to a MapData structure which describes the Mapping
*        function to be used.
*     lo
*        The (zero-based) index of the simplex vertex which initially
*        has the worst (lowest) value.
*     scale
*        The scale factor to be used to generate the new vertex. The
*        distance of the worst vertex from the centre of the face
*        opposite it is scaled by this factor to give the new vertex
*        position. Negative factors result in reflection through this
*        opposite face.
*     x
*        An array of double containing the coordinates of the vertices
*        of the simplex. The coordinates of the first vertex are
*        stored first, then those of the second vertex, etc. This
*        array will be updated by this function if the new vertex is
*        used to replace an existing one.
*     f
*        An array of double containing the Mapping function values at
*        each vertex of the simplex. This array will be updated by
*        this function if the new vertex is used to replace an
*        existing one.
*     ncall
*        Pointer to an int containing a count of the number of times
*        the Mapping function has been invoked. This value will be
*        updated to reflect the actions of this function.
*     xnew
*        An array of double with one element for each input coordinate
*        of the Mapping function. This is used as workspace.

*  Returned Value:
*     The Mapping function value at the new vertex. This value is
*     returned whether or not the new vertex replaces an existing one.

*  Notes:
*     - A value of AST__BAD will be returned by this function if it is
*     invoked with the global error status set, or if it should fail
*     for any reason.
*     - A value of AST__BAD will also be returned if the new vertex
*     lies outside the constrained range of input coordinates
*     associated with the Mapping function (as specified in the
*     MapData structure supplied) or if any of the transformed output
*     coordinates produced by the underlying Mapping is bad. In either
*     case the new vertex will not be used to replace an existing one.
*/

/* Local Variables: */
   double fnew;                  /* Function value at new vertex */
   double xface;                 /* Coordinate of centre of magnification */
   int coord;                    /* Loop counter for coordinates */
   int ncoord;                   /* Number of coordinates */
   int nvertex;                  /* Number of simplex vertices */
   int vertex;                   /* Loop counter for vertices */

/* Initialise. */
   fnew = AST__BAD;

/* Check the global error status. */
   if ( !astOK ) return fnew;
   
/* Obtain the number of Mapping input coordinates from the MapData
   structure and calculate the number of simplex vertices. */
   ncoord = mapdata->nin;
   nvertex = ncoord + 1;

/* Loop to obtain each coordinate of the new vertex. */
   for ( coord = 0; coord < ncoord; coord++ ) {

/* Loop over all vertices except the lowest one and average their
   coordinates. This gives the coordinate of the centre of the face
   opposite the lowest vertex, which will act as the centre of
   magnification. */
      xface = 0.0;
      for ( vertex = 0; vertex < nvertex; vertex++ ) {
         if ( vertex != lo ) {

/* Divide each coordinate by the number of vertices as the sum is
   accumulated in order to minimise the risk of overflow. */
            xface += x[ vertex * ncoord + coord ] /
                     ( (double ) ( nvertex - 1 ) );
         }
      }

/* Magnify the lowest vertex's distance from this point by the
   required factor to give the coordinates of the new vertex. */
      xnew[ coord ] = xface + ( x[ lo * ncoord + coord ] - xface ) * scale;
   }

/* Evaluate the Mapping function at the new vertex. */
   fnew = MapFunction( mapdata, xnew, ncall );
 
/* If the result is not bad and exceeds the previous value at the
   lowest vertex, then replace the lowest vertex with this new one. */
   if ( astOK && ( fnew != AST__BAD ) && ( fnew > f[ lo ] ) ) {
      for ( coord = 0; coord < ncoord; coord++ ) {
         x[ lo * ncoord + coord ] = xnew[ coord ];
      }
      f[ lo ] = fnew;
   }

/* Return the value at the new vertex. */
   return fnew;
}

static double Random( long int *seed ) {
/*
*  Name:
*     Random

*  Purpose:
*     Return a pseudo-random value in the range 0 to 1.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     double Random( long int *seed );

*  Class Membership:
*     Mapping member function.

*  Description:
*     This function returns a pseudo-random double value from a PDF
*     uniformly distributed in the range 0 to 1. It also updates a
*     seed value so that a sequence of pseudo-random values may be
*     obtained with successive invocations.

*  Parameters:
*     seed
*        Pointer to a long int which should initially contain a
*        non-zero seed value. This will be updated with a new seed
*        which may be supplied on the next invocation in order to
*        obtain a different pseudo-random value.

*  Returned Value:
*     The pseudo-random value.
*/

/* Local Variables: */
   long int i;                   /* Temporary storage */

/* This a basic random number generator using constants given in
   Numerical Recipes (Press et al.). */
   i = *seed / 127773;
   *seed = ( *seed - i * 127773 ) * 16807 - i * 2836;
   if ( *seed < 0 ) *seed += 2147483647;

/* Return the result as a double value in the range 0 to 1. */
   return ( (double) ( *seed - 1 ) ) / ( (double) 2147483646 );
}

static void ReportPoints( AstMapping *this, int forward,
                          AstPointSet *in_points, AstPointSet *out_points ) {
/*
*+
*  Name:
*     astReportPoints

*  Purpose:
*     Report the effect of transforming a set of points using a Mapping.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "mapping.h"
*     void astReportPoints( AstMapping *this, int forward,
*                           AstPointSet *in_points, AstPointSet *out_points )

*  Class Membership:
*     Mapping method.

*  Description:
*     This function reports the coordinates of a set of points before
*     and after being transformed by a Mapping, by writing them to
*     standard output.

*  Parameters:
*     this
*        Pointer to the Mapping.
*     forward
*        A non-zero value indicates that the Mapping's forward
*        coordinate transformation has been applied, while a zero
*        value indicates the inverse transformation.
*     in_points
*        Pointer to a PointSet which is associated with the
*        coordinates of a set of points before the Mapping was
*        applied.
*     out_points
*        Pointer to a PointSet which is associated with the
*        coordinates of the same set of points after the Mapping has
*        been applied.

*  Notes:
*     - This method is provided as a development and debugging aid to
*     be invoked when coordinates are transformed by public Mapping
*     methods and under control of the "Report" Mapping attribute.
*     - Derived clases may over-ride this method in order to change
*     the way in which coordinates are formatted, etc.
*-
*/

/* Local Variables: */
   double **ptr_in;              /* Pointer to array of input data pointers */
   double **ptr_out;             /* Pointer to array of output data pointers */
   int coord;                    /* Loop counter for coordinates */
   int ncoord_in;                /* Number of input coordinates per point */
   int ncoord_out;               /* Number of output coordinates per point */
   int npoint;                   /* Number of points to report */
   int npoint_in;                /* Number of input points */
   int npoint_out;               /* Number of output points */
   int point;                    /* Loop counter for points */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain the numbers of points and coordinates associated with each
   PointSet. */ 
   npoint_in = astGetNpoint( in_points );
   npoint_out = astGetNpoint( out_points );
   ncoord_in = astGetNcoord( in_points );
   ncoord_out = astGetNcoord( out_points );

/* Obtain the pointers that give access to the coordinate data
   associated with each PointSet. */
   ptr_in = astGetPoints( in_points );
   ptr_out = astGetPoints( out_points );

/* In the event that both PointSets don't contain equal numbers of
   points (this shouldn't actually happen), simply use the minimum
   number. */
   npoint = ( npoint_in < npoint_out ) ? npoint_in : npoint_out;

/* Loop to report the effect of the Mapping on each point in turn. */
   for ( point = 0; point < npoint; point++ ) {

/* Report the input coordinates (in parentheses and separated by
   commas). Replace coordinate values of AST__BAD with the string
   "<bad>" to indicate missing values. */
      printf( "(" );
      for ( coord = 0; coord < ncoord_in; coord++ ) {
         if ( ptr_in[ coord ][ point ] == AST__BAD ) {
            printf( "%s<bad>", coord ? ", " : "" );
         } else {
            printf( "%s%.*g", coord ? ", " : "",
                              DBL_DIG, ptr_in[ coord ][ point ] );
         }
      }

/* Similarly report the output coordinates. */
      printf( ") --> (" );
      for ( coord = 0; coord < ncoord_out; coord++ ) {
         if ( ptr_out[ coord ][ point ] == AST__BAD ) {
            printf( "%s<bad>", coord ? ", " : "" );
         } else {
            printf( "%s%.*g", coord ? ", " : "",
                              DBL_DIG, ptr_out[ coord ][ point ] );
         }
      }
      printf( ")\n" );
   }
}

static void SetAttrib( AstObject *this_object, const char *setting ) {
/*
*  Name:
*     astSetAttrib

*  Purpose:
*     Set an attribute value for a Mapping.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     void SetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     Mapping member function (over-rides the astSetAttrib protected
*     method inherited from the Object class).

*  Description:
*     This function assigns an attribute value for a Mapping, the
*     attribute and its value being specified by means of a string of
*     the form:
*
*        "attribute= value "
*
*     Here, "attribute" specifies the attribute name and should be in
*     lower case with no white space present. The value to the right
*     of the "=" should be a suitable textual representation of the
*     value to be assigned and this will be interpreted according to
*     the attribute's data type.  White space surrounding the value is
*     only significant for string attributes.

*  Parameters:
*     this
*        Pointer to the Mapping.
*     setting
*        Pointer to a null terminated string specifying the new attribute
*        value.
*/

/* Local Variables: */
   AstMapping *this;             /* Pointer to the Mapping structure */
   int invert;                   /* Invert attribute value */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by sscanf */
   int report;                   /* Report attribute value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Mapping structure. */
   this = (AstMapping *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Test for each recognised attribute in turn, using "sscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */

/* Invert. */
/* ------- */
   if ( nc = 0,
        ( 1 == sscanf( setting, "invert= %d %n", &invert, &nc ) )
        && ( nc >= len ) ) {
      astSetInvert( this, invert );

/* Report. */
/* ------- */
   } else if ( nc = 0,
        ( 1 == sscanf( setting, "report= %d %n", &report, &nc ) )
        && ( nc >= len ) ) {
      astSetReport( this, report );

/* Define a macro to see if the setting string matches any of the
   read-only attributes of this class. */
#define MATCH(attrib) \
        ( nc = 0, ( 0 == sscanf( setting, attrib "=%*[^\n]%n", &nc ) ) && \
                  ( nc >= len ) )

/* If the attribute was not recognised, use this macro to report an error
   if a read-only attribute has been specified. */
   } else if ( MATCH( "nin" ) ||
        MATCH( "nout" ) ||
        MATCH( "tranforward" ) ||
        MATCH( "traninverse" ) ) {
      astError( AST__NOWRT, "astSet: The setting \"%s\" is invalid for a %s.",
                setting, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting );
   }

/* Undefine macros local to this function. */
#undef MATCH
}

static AstMapping *Simplify( AstMapping *this ) {
/*
*++
*  Name:
c     astSimplify
f     AST_SIMPLIFY

*  Purpose:
*     Simplify a Mapping.

*  Type:
*     Public function.

*  Synopsis:
c     #include "mapping.h"
c     AstMapping *astSimplify( AstMapping *this )
f     RESULT = AST_SIMPLIFY( THIS, STATUS )

*  Class Membership:
*     Mapping method.

*  Description:
*     This function simplifies a Mapping (which may be a compound
*     Mapping such as a CmpMap) to eliminate redundant computational
*     steps, or to merge separate steps which can be performed more
*     efficiently in a single operation.
*
*     As a simple example, a Mapping which multiplied coordinates by
*     5, and then multiplied the result by 10, could be simplified to
*     a single step which multiplied by 50. Similarly, a Mapping which
*     multiplied by 5, and then divided by 5, could be reduced to a
*     simple copying operation.
*
*     This function should typically be applied to Mappings which have
*     undergone substantial processing or have been formed by merging
*     other Mappings. It is of potential benefit, for example, in
*     reducing execution time if applied before using a Mapping to
*     transform a large number of coordinates.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the original Mapping.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astSimplify()
f     AST_SIMPLIFY = INTEGER
*        A new pointer to the (possibly simplified) Mapping.

*  Notes:
*     - This function can safely be applied even to Mappings which
*     cannot be simplified. If no simplification is possible, it
c     behaves exactly like astClone and returns a pointer to the
f     behaves exactly like AST_CLONE and returns a pointer to the
*     original Mapping.
*     - The Mapping returned by this function may not be independent
*     of the original (even if simplification was possible), and
*     modifying it may therefore result in indirect modification of
*     the original. If a completely independent result is required, a
c     copy should be made using astCopy.
f     copy should be made using AST_COPY.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--
*/

/* Local Variables: */
   AstMapping **map_list;        /* Pointer to array of Mapping pointers */
   AstMapping *map;              /* Cloned pointer to nominated Mapping */
   AstMapping *result;           /* Pointer to result Mapping */
   int *invert_list;             /* Pointer to array of invert flags */
   int imap;                     /* Loop counter for Mappings */
   int modified;                 /* Index of first modified element */
   int nmap;                     /* Number of Mappings */
   int simpler;                  /* Simplification achieved? */

/* Initialise. */
   result = NULL;

/* Check the inherited status. */
   if ( !astOK ) return result;

/* Initialise dynamic arrays of Mapping pointers and associated invert
   flags. */
   nmap = 0;
   map_list = NULL;
   invert_list = NULL;

/* Build a Mapping list to contain this Mapping (the list should only
   have 1 element). */
   astMapList( this, 1, astGetInvert( this ), &nmap, &map_list, &invert_list );

/* Pass the list repeatedly to the "astMapMerge" method for
   simplification. */
   simpler = 0;
   while ( astOK ) {
      map = astClone( map_list[ 0 ] );
      modified = astMapMerge( map, 0, 1, &nmap, &map_list, &invert_list );
      map = astAnnul( map );

/* Quit looping if the number of Mappings increases above 1, or if no
   further change occurs. Note if any simplification was achieved. */
      if ( ( nmap > 1 ) || ( modified < 0 ) ) break;
      simpler = 1;
   }

/* Check whether simplification has occurred. If not, simply clone the
   original Mapping pointer. This is what will normally happen for
   Mapping classes which inherit the default (null) "astMapMerge"
   method from this class and do not define one of their own. */
   if ( astOK ) {
      if ( !simpler || ( nmap > 1 ) ) {
         result = astClone( this );

/* If simplification occurred, test if the resulting Mapping has the
   Invert attribute value we want. If so, we can simply clone a
   pointer to it. */
      } else {
         if ( invert_list[ 0 ] == astGetInvert( map_list[ 0 ] ) ) {
            result = astClone( map_list[ 0 ] );

/* If not, we must make a copy. */
         } else {
            result = astCopy( map_list[ 0 ] );

/* Either clear the copy's Invert attribute, or set it to 1, as
   required. */
            if ( invert_list[ 0 ] ) {
               astSetInvert( result, 1 );
            } else {
               astClearInvert( result );
            }
         }
      }
   }

/* Loop to annul all the pointers in the Mapping list. */
   for ( imap = 0; imap < nmap; imap++ ) {
      map_list[ imap ] = astAnnul( map_list[ imap ] );
   }

/* Free the dynamic arrays. */
   map_list = astFree( map_list );
   invert_list = astFree( invert_list );

/* If an error occurred, annul the returned Mapping. */
   if ( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;   
}

static void SpecialBounds( const MapData *mapdata, double *lbnd, double *ubnd,
                           double xl[], double xu[] ) {
/*
*  Name:
*     SpecialBounds

*  Purpose:
*     Estimate coordinate bounds using special points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     void SpecialBounds( const MapData *mapdata, double *lbnd, double *ubnd,
*                         double xl[], double xu[] );

*  Class Membership:
*     Mapping member function.

*  Description:
*     This function makes a rough estimate of the lower and upper
*     bounds of a Mapping function over a constrained region of its
*     input coordinate space by transforming a set of special test
*     points. The points used lie at the corners of the constrained
*     region, at the centre of each of its faces, at its centroid, and
*     (if within the coordinate constraints) the origin.
*
*     In many practical cases, the true extrema may actually lie at
*     one or other of these points, in which case the true bounds will
*     be found. In other cases, this function only provides an
*     approximate limit on each bound (there is no way of telling if
*     this is the case, however). In either case, having these initial
*     estimates can speed subsequent searches to find the global
*     extrema as well as making that search more secure

*  Parameters:
*     mapdata
*        Pointer to a MapData structure describing the Mapping
*        function, its coordinate constraints, etc.
*     lbnd
*        Pointer to a double.  On entry, this should contain a
*        previously-obtained upper limit on the lower bound, or
*        AST__BAD if no such limit is available. On exit, it will be
*        updated with a new estimate of the lower bound, if a better
*        one has been found.
*     ubnd
*        Pointer to a double.  On entry, this should contain a
*        previously-obtained lower limit on the upper bound, or
*        AST__BAD if no such limit is available. On exit, it will be
*        updated with a new estimate of the upper bound, if a better
*        one has been found.
*     xl
*        Pointer to an array of double, with one element for each
*        input coordinate, in which to return the position of a (not
*        necessarily unique) input point at which the lower output
*        bound is reached. This array is not altered if an improved
*        estimate of the lower bound cannot be found.
*     xu
*        Pointer to an array of double, with one element for each
*        input coordinate, in which to return the position of a (not
*        necessarily unique) input point at which the upper output
*        bound is reached. This array is not altered if an improved
*        estimate of the upper bound cannot be found.
*/

/* Local Variables: */
   AstPointSet *pset_in;         /* PointSet for input coordinates */
   AstPointSet *pset_out;        /* PointSet for output coordinates */
   double **ptr_in;              /* Pointer to input coordinates */
   double **ptr_out;             /* Pointer to output coordinates */
   double f;                     /* Output coordinate value */
   int *limit;                   /* Workspace for lower/upper limit flags */
   int bad;                      /* Output coordinate bad? */
   int coord;                    /* Loop counter for coordinates */
   int done;                     /* All corners done? */
   int face;                     /* Loop counter for faces */
   int ncoord;                   /* Number of input coordinates */
   int npoint;                   /* Number of points */
   int origin;                   /* Origin lies within bounds? */
   int point;                    /* Loop counter for points */
   
/* Obtain the number of coordinate axes and calculate the number of
   points required in order to place one at every corner of the
   constrained region of the coordinate space. */
   ncoord = mapdata->nin;
   for ( npoint = 1, coord = 0; coord < ncoord; coord++ ) npoint *= 2;

/* Also include placing one at the centre of every face and one at the
   centroid of the constrained coordinate space. */
   npoint += 2 * ncoord + 1;
   
/* Determine if the origin lies within the bounds. If so, include it
   as a further point. */
   origin = 1;
   for ( coord = 0; coord < ncoord; coord++ ) {
      if ( ( mapdata->lbnd[ coord ] > 0.0 ) ||
           ( mapdata->ubnd[ coord ] < 0.0 ) ) {
         origin = 0;
         break;
      }
   }
   if ( origin ) npoint++;

/* Create a PointSet to hold the coordinates and obtain a pointer to
   its coordinate values. Also allocate workspace for calculating the
   corner coordinates. */
   pset_in = astPointSet( npoint, ncoord, "" );
   ptr_in = astGetPoints( pset_in );
   limit = astMalloc( sizeof( int ) * (size_t) ncoord );
   if ( astOK ) {
   
/* Initialise the workspace. */
      for ( coord = 0; coord < ncoord; coord++ ) limit[ coord ] = 0;

/* Loop to visit every corner. */
      done = 0;
      point = 0;
      while ( !done ) {

/* At each corner, translate the contents of the "limit" array
   (containing zeros and ones) into the lower or upper bound on the
   corresponding axis. This gives the coordinates of the corner, which
   we store in the input PointSet. */
         for ( coord = 0; coord < ncoord; coord++ ) {
            ptr_in[ coord ][ point ] = limit[ coord ] ?
                                       mapdata->ubnd[ coord ] :
                                       mapdata->lbnd[ coord ];
         }

/* Increment the count of points (i.e. corners). */
         point++;
      
/* Now update the limit array to identify the next corner. */
         coord = 0;
         while ( !done ) {

/* Flip the first zero found to become a one. This gives a new
   corner. */
            if ( !limit[ coord ] ) {
               limit[ coord ] = 1;
               break;

/* However, first flip any previous ones to become zeros and then
   examine the next element. We have processed all corners once the
   array is entirely filled with ones. */
            } else {
               limit[ coord ] = 0;
               done = ( ++coord == ncoord );
            }
         }
      }

/* Once the corners have been processed, loop to consider the centre
   of each face. */
      for ( face = 0; face < ( 2 * ncoord ); face++ ) {

/* First calculate the centroid value for each coordinate.  Then set
   one of these coordinates to the bound where the face lies. */
         for ( coord = 0; coord < ncoord; coord++ ) {
            ptr_in[ coord ][ point ] = 0.5 * ( mapdata->lbnd[ coord ] +
                                               mapdata->ubnd[ coord ] );
         }
         ptr_in[ face / 2 ][ point ] = ( face % 2 ) ?
                                       mapdata->lbnd[ face / 2 ] :
                                       mapdata->ubnd[ face / 2 ];

/* Increment the count of points. */
         point++;
      }

/* Place a point at the centroid of the constrained coordinate
   space. */
      for ( coord = 0; coord < ncoord; coord++ ) {
         ptr_in[ coord ][ point ] = 0.5 * ( mapdata->lbnd[ coord ] +
                                            mapdata->ubnd[ coord ] );
      }
      point++;

/* Finally, add the origin, if it lies within the constraints. */
      if ( origin ) {
         for ( coord = 0; coord < ncoord; coord++ ) {
            ptr_in[ coord ][ point ] = 0.0;
         }
      }

/* Once all the input coordinates have been calculated, transform them
   and obtain a pointer to the resulting coordinate values. */
      pset_out = astTransform( mapdata->mapping, pset_in, mapdata->forward,
                               NULL );
      ptr_out = astGetPoints( pset_out );
      if ( astOK ) {

/* Loop through each point and test if any of its transformed
   coordinates is bad. */
         for ( point = 0; point < npoint; point++ ) {
            bad = 0;
            for ( coord = 0; coord < ncoord; coord++ ) {
               if ( ptr_out[ coord ][ point ] == AST__BAD ) {
                  bad = 1;
                  break;
               }
            }

/* If so, we ignore the point. Otherwise, extract the required
   coordinate. */
            if ( !bad ) {
               f = ptr_out[ mapdata->coord ][ point ];

/* Use this to update the lower and upper bounds we are seeking. If
   either bound is updated, also store the coordinates of the
   corresponding input point. */
               if ( ( *lbnd == AST__BAD ) || ( f < *lbnd ) ) {
                  *lbnd = f;
                  for ( coord = 0; coord < ncoord; coord++ ) {
                     xl[ coord ] = ptr_in[ coord ][ point ];
                  }
               }
               if ( ( *ubnd == AST__BAD ) || ( f > *ubnd ) ) {
                  *ubnd = f;
                  for ( coord = 0; coord < ncoord; coord++ ) {
                     xu[ coord ] = ptr_in[ coord ][ point ];
                  }
               }
            }
         }
      }
   }

/* Annul the temporary PointSets and free the workspace. */
   pset_in = astAnnul( pset_in );
   pset_out = astAnnul( pset_out );
   limit = astFree( limit );
}

static int TestAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a Mapping.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     int TestAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     Mapping member function (over-rides the astTestAttrib protected
*     method inherited from the Object class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a Mapping's attributes.

*  Parameters:
*     this
*        Pointer to the Mapping.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstMapping *this;             /* Pointer to the Mapping structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the Mapping structure. */
   this = (AstMapping *) this_object;

/* Check the attribute name and test the appropriate attribute. */

/* Invert. */
/* ------- */
   if ( !strcmp( attrib, "invert" ) ) {
      result = astTestInvert( this );

/* Report. */
/* ------- */
   } else if ( !strcmp( attrib, "report" ) ) {
      result = astTestReport( this );

/* If the name is not recognised, test if it matches any of the
   read-only attributes of this class. If it does, then return
   zero. */
   } else if ( !strcmp( attrib, "nin" ) ||
        !strcmp( attrib, "nout" ) ||
        !strcmp( attrib, "tranforward" ) ||
        !strcmp( attrib, "traninverse" ) ) {
      result = 0;

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib );
   }

/* Return the result, */
   return result;
}

static void Tran1( AstMapping *this, int npoint, const double xin[],
                   int forward, double xout[] ) {
/*
*++
*  Name:
c     astTran1
f     AST_TRAN1

*  Purpose:
*     Transform 1-dimensional coordinates.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "mapping.h"
c     void astTran1( AstMapping *this, int npoint, const double xin[],
c                    int forward, double xout[] )
f     CALL AST_TRAN1( THIS, NPOINT, XIN, FORWARD, XOUT, STATUS )

*  Class Membership:
*     Mapping method.

*  Description:
c     This function applies a Mapping to transform the coordinates of
f     This routine applies a Mapping to transform the coordinates of
*     a set of points in one dimension.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Mapping to be applied.
c     npoint
f     NPOINT = INTEGER (Given)
*        The number of points to be transformed.
c     xin
f     XIN( NPOINT ) = DOUBLE PRECISION (Given)
c        An array of "npoint" coordinate values for the input
f        An array of coordinate values for the input
*        (untransformed) points.
c     forward
f     FORWARD = LOGICAL (Given)
c        A non-zero value indicates that the Mapping's forward
c        coordinate transformation is to be applied, while a zero
c        value indicates that the inverse transformation should be
c        used.
f        A .TRUE. value indicates that the Mapping's forward
f        coordinate transformation is to be applied, while a .FALSE.
f        value indicates that the inverse transformation should be
f        used.
c     xout
f     XOUT( NPOINT ) = DOUBLE PRECISION (Returned)
c        An array (with "npoint" elements) into which the
f        An array into which the
*        coordinates of the output (transformed) points will be written.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - The Mapping supplied must have the value 1 for both its Nin
*     and Nout attributes.
*--
*/

/* Local Variables: */
   AstPointSet *in_points;       /* Pointer to input PointSet */
   AstPointSet *out_points;      /* Pointer to output PointSet */
   const double *in_ptr[ 1 ];    /* Array of input data pointers */
   double *out_ptr[ 1 ];         /* Array of output data pointers */

/* Check the global error status. */
   if ( !astOK ) return;

/* Validate the Mapping and numbers of points/coordinates. */
   ValidateMapping( this, forward, npoint, 1, 1, "astTran1" );

/* Set up pointers to the input and output coordinate arrays. */
   if ( astOK ) {
      in_ptr[ 0 ] = xin;
      out_ptr[ 0 ] = xout;

/* Create PointSets to describe the input and output points. */
      in_points = astPointSet( npoint, 1, "" );
      out_points = astPointSet( npoint, 1, "" );

/* Associate the data pointers with the PointSets (note we must
   explicitly remove the "const" qualifier from the input data here,
   although they will not be modified).  */
      astSetPoints( in_points, (double **) in_ptr );
      astSetPoints( out_points, out_ptr );

/* Apply the required transformation to the coordinates. */
      (void) astTransform( this, in_points, forward, out_points );

/* If the Mapping's Report attribute is set, report the effect the
   Mapping has had on the coordinates. */
      if ( astGetReport( this ) ) astReportPoints( this, forward,
                                                   in_points, out_points );

/* Delete the two PointSets. */
      astDelete( in_points );
      astDelete( out_points );
   }
}

static void Tran2( AstMapping *this,
                   int npoint, const double xin[], const double yin[],
                   int forward, double xout[], double yout[] ) {
/*
*++
*  Name:
c     astTran2
f     AST_TRAN2

*  Purpose:
*     Transform 2-dimensional coordinates.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "mapping.h"
c     void astTran2( AstMapping *this,
c                    int npoint, const double xin[], const double yin[],
c                    int forward, double xout[], double yout[] )
f     CALL AST_TRAN2( THIS, NPOINT, XIN, YIN, FORWARD, XOUT, YOUT, STATUS )

*  Class Membership:
*     Mapping method.

*  Description:
c     This function applies a Mapping to transform the coordinates of
f     This routine applies a Mapping to transform the coordinates of
*     a set of points in two dimensions.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Mapping to be applied.
c     npoint
f     NPOINT = INTEGER (Given)
*        The number of points to be transformed.
c     xin
f     XIN( NPOINT ) = DOUBLE PRECISION (Given)
c        An array of "npoint" X-coordinate values for the input
f        An array of X-coordinate values for the input
*        (untransformed) points.
c     yin
f     YIN( NPOINT ) = DOUBLE PRECISION (Given)
c        An array of "npoint" Y-coordinate values for the input
f        An array of Y-coordinate values for the input
*        (untransformed) points.
c     forward
f     FORWARD = LOGICAL (Given)
c        A non-zero value indicates that the Mapping's forward
c        coordinate transformation is to be applied, while a zero
c        value indicates that the inverse transformation should be
c        used.
f        A .TRUE. value indicates that the Mapping's forward
f        coordinate transformation is to be applied, while a .FALSE.
f        value indicates that the inverse transformation should be
f        used.
c     xout
f     XOUT( NPOINT ) = DOUBLE PRECISION (Returned)
c        An array (with "npoint" elements) into which the
f        An array into which the
*        X-coordinates of the output (transformed) points will be written.
c     yout
f     YOUT( NPOINT ) = DOUBLE PRECISION (Returned)
c        An array (with "npoint" elements) into which the
f        An array into which the
*        Y-coordinates of the output (transformed) points will be written.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - The Mapping supplied must have the value 2 for both its Nin
*     and Nout attributes.
*--
*/

/* Local Variables: */
   AstPointSet *in_points;       /* Pointer to input PointSet */
   AstPointSet *out_points;      /* Pointer to output PointSet */
   const double *in_ptr[ 2 ];    /* Array of input data pointers */
   double *out_ptr[ 2 ];         /* Array of output data pointers */

/* Check the global error status. */
   if ( !astOK ) return;

/* Validate the Mapping and the numbers of points/coordinates. */
   ValidateMapping( this, forward, npoint, 2, 2, "astTran2" );

/* Set up pointers to the input and output coordinate arrays. */
   if ( astOK ) {
      in_ptr[ 0 ] = xin;
      in_ptr[ 1 ] = yin;
      out_ptr[ 0 ] = xout;
      out_ptr[ 1 ] = yout;

/* Create PointSets to describe the input and output points. */
      in_points = astPointSet( npoint, 2, "" );
      out_points = astPointSet( npoint, 2, "" );

/* Associate the data pointers with the PointSets (note we must
   explicitly remove the "const" qualifier from the input data here,
   although they will not be modified).  */
      astSetPoints( in_points, (double **) in_ptr );
      astSetPoints( out_points, out_ptr );

/* Apply the required transformation to the coordinates. */
      (void) astTransform( this, in_points, forward, out_points );

/* If the Mapping's Report attribute is set, report the effect the
   Mapping has had on the coordinates. */
      if ( astGetReport( this ) ) astReportPoints( this, forward,
                                                   in_points, out_points );

/* Delete the two PointSets. */
      astDelete( in_points );
      astDelete( out_points );
   }
}

static void TranN( AstMapping *this, int npoint,
                   int ncoord_in, int indim, const double (*in)[],
                   int forward,
                   int ncoord_out, int outdim, double (*out)[] ) {
/*
*++
*  Name:
c     astTranN
f     AST_TRANN

*  Purpose:
*     Transform N-dimensional coordinates.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "mapping.h"
c     void astTranN( AstMapping *this, int npoint,
c                    int ncoord_in, int indim, const double (*in)[],
c                    int forward,
c                    int ncoord_out, int outdim, double (*out)[] )
f     CALL AST_TRANN( THIS, NPOINT,
f                     NCOORD_IN, INDIM, IN,
f                     FORWARD, NCOORD_OUT, OUTDIM, OUT, STATUS )

*  Class Membership:
*     Mapping method.

*  Description:
c     This function applies a Mapping to transform the coordinates of
f     This routine applies a Mapping to transform the coordinates of
*     a set of points in an arbitrary number of dimensions. It is the
*     appropriate routine to use if the coordinates are not purely 1-
*     or 2-dimensional and are stored in a single array (which they
*     need not fill completely).
c
c     If the coordinates are not stored in a single array, then the
c     astTranP function might be more suitable.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Mapping to be applied.
c     npoint
f     NPOINT = INTEGER (Given)
*        The number of points to be transformed.
c     ncoord_in
f     NCOORD_IN = INTEGER (Given)
*        The number of coordinates being supplied for each input point
*        (i.e. the number of dimensions of the space in which the
*        input points reside).
c     indim
f     INDIM = INTEGER (Given)
c        The number of elements along the second dimension of the "in"
f        The number of elements along the first dimension of the IN
*        array (which contains the input coordinates). This value is
*        required so that the coordinate values can be correctly
*        located if they do not entirely fill this array. The value
c        given should not be less than "npoint".
f        given should not be less than NPOINT.
c     in
f     IN( INDIM, NCOORD_IN ) = DOUBLE PRECISION (Given)
c        A 2-dimensional array, of shape "[ncoord_in][indim]",
c        containing the coordinates of the input (untransformed)
c        points. These should be stored such that the value of
c        coordinate number "coord" for input point number "point" is
c        found in element "in[coord][point]".
f        An array containing the coordinates of the input
f        (untransformed) points. These should be stored such that the
f        value of coordinate number COORD for input point number POINT
f        is found in element IN(POINT,COORD).
c     forward
f     FORWARD = LOGICAL (Given)
c        A non-zero value indicates that the Mapping's forward
c        coordinate transformation is to be applied, while a zero
c        value indicates that the inverse transformation should be
c        used.
f        A .TRUE. value indicates that the Mapping's forward
f        coordinate transformation is to be applied, while a .FALSE.
f        value indicates that the inverse transformation should be
f        used.
c     ncoord_out
f     NCOORD_OUT = INTEGER (Given)
*        The number of coordinates being generated by the Mapping for
*        each output point (i.e. the number of dimensions of the
*        space in which the output points reside). This need not be
c        the same as "ncoord_in".
f        the same as NCOORD_IN.
c     outdim
f     OUTDIM = INTEGER (Given)
c        The number of elements along the second dimension of the "out"
f        The number of elements along the first dimension of the OUT
*        array (which will contain the output coordinates). This value
*        is required so that the coordinate values can be correctly
*        located if they will not entirely fill this array. The value
c        given should not be less than "npoint".
f        given should not be less than NPOINT.
c     out
f     OUT( OUTDIM, NCOORD_OUT ) = DOUBLE PRECISION (Returned)
c        A 2-dimensional array, of shape "[ncoord_out][outdim]", into
c        which the coordinates of the output (transformed) points will
c        be written. These will be stored such that the value of
c        coordinate number "coord" for output point number "point"
c        will be found in element "out[coord][point]".
f        An array into which the coordinates of the output
f        (transformed) points will be written. These will be stored
f        such that the value of coordinate number COORD for output
f        point number POINT will be found in element OUT(POINT,COORD).
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
c     - If the forward coordinate transformation is being applied, the
c     Mapping supplied must have the value of "ncoord_in" for its Nin
c     attribute and the value of "ncoord_out" for its Nout attribute. If
c     the inverse transformation is being applied, these values should
c     be reversed.
f     - If the forward coordinate transformation is being applied, the
f     Mapping supplied must have the value of NCOORD_IN for its Nin
f     attribute and the value of NCOORD_OUT for its Nout attribute. If
f     the inverse transformation is being applied, these values should
f     be reversed.
*--
*/

/* Local Variables: */
   AstPointSet *in_points;       /* Pointer to input PointSet */
   AstPointSet *out_points;      /* Pointer to output PointSet */
   const double **in_ptr;        /* Pointer to array of input data pointers */
   double **out_ptr;             /* Pointer to array of output data pointers */
   int coord;                    /* Loop counter for coordinates */

/* Check the global error status. */
   if ( !astOK ) return;

/* Validate the mapping and numbers of points/coordinates. */
   ValidateMapping( this, forward, npoint, ncoord_in, ncoord_out, "astTranN" );

/* Also validate the input array dimension argument. */
   if ( astOK && ( indim < npoint ) ) {
      astError( AST__DIMIN, "astTranN(%s): The input array dimension value "
                "(%d) is invalid.", astGetClass( this ), indim );
      astError( AST__DIMIN, "This should not be less than the number of "
                "points being transformed (%d).", npoint );
   }

/* Similarly, validate the output array dimension argument. */
   if ( astOK && ( outdim < npoint ) ) {
      astError( AST__DIMIN, "astTranN(%s): The output array dimension value "
                "(%d) is invalid.", astGetClass( this ), outdim );
      astError( AST__DIMIN, "This should not be less than the number of "
                "points being transformed (%d).", npoint );
   }

/* Allocate memory to hold the arrays of input and output data
   pointers. */
   if ( astOK ) {
      in_ptr = (const double **) astMalloc( sizeof( const double * ) *
                                            (size_t) ncoord_in );
      out_ptr = astMalloc( sizeof( double * ) * (size_t) ncoord_out );

/* Initialise the input data pointers to locate the coordinate data in
   the "in" array. */
      if ( astOK ) {
         for ( coord = 0; coord < ncoord_in; coord++ ) {
            in_ptr[ coord ] = *in + coord * indim;
         }

/* Similarly initialise the output data pointers to point into the
   "out" array. */
         for ( coord = 0; coord < ncoord_out; coord++ ) {
            out_ptr[ coord ] = *out + coord * outdim;
         }

/* Create PointSets to describe the input and output points. */
         in_points = astPointSet( npoint, ncoord_in, "" );
         out_points = astPointSet( npoint, ncoord_out, "" );

/* Associate the data pointers with the PointSets (note we must
   explicitly remove the "const" qualifier from the input data here,
   although they will not be modified).  */
         astSetPoints( in_points, (double **) in_ptr );
         astSetPoints( out_points, out_ptr );

/* Apply the required transformation to the coordinates. */
         (void) astTransform( this, in_points, forward, out_points );

/* If the Mapping's Report attribute is set, report the effect the
   Mapping has had on the coordinates. */
         if ( astGetReport( this ) ) astReportPoints( this, forward,
                                                      in_points, out_points );

/* Delete the two PointSets. */
         astDelete( in_points );
         astDelete( out_points );
      }

/* Free the memory used for the data pointers. */
      in_ptr = (const double **) astFree( (void *) in_ptr );
      out_ptr = astFree( out_ptr );
   }
}

static void TranP( AstMapping *this, int npoint,
                   int ncoord_in, const double *ptr_in[],
                   int forward, int ncoord_out, double *ptr_out[] ) {
/*
c++
*  Name:
*     astTranP

*  Purpose:
*     Transform N-dimensional coordinates held in separate arrays.

*  Type:
*     Public virtual function.

*  Synopsis:
*     #include "mapping.h"
*     void astTranP( AstMapping *this, int npoint,
*                    int ncoord_in, const double *ptr_in[],
*                    int forward, int ncoord_out, double *ptr_out[] )

*  Class Membership:
*     Mapping method.

*  Description:
*     This function applies a Mapping to transform the coordinates of
*     a set of points in an arbitrary number of dimensions. It is the
*     appropriate routine to use if the coordinates are not purely 1-
*     or 2-dimensional and are stored in separate arrays, since each
*     coordinate array is located by supplying a separate pointer to
*     it.
*
*     If the coordinates are stored in a single (2-dimensional) array,
*     then the astTranN function might be more suitable.

*  Parameters:
*     this
*        Pointer to the Mapping to be applied.
*     npoint
*        The number of points to be transformed.
*     ncoord_in
*        The number of coordinates being supplied for each input point
*        (i.e. the number of dimensions of the space in which the
*        input points reside).
*     ptr_in
*        An array of pointers to double, with "ncoord_in"
*        elements. Element "ptr_in[coord]" should point at the first
*        element of an array of double (with "npoint" elements) which
*        contain the values of coordinate number "coord" for each
*        input (untransformed) point. The value of coordinate number
*        "coord" for input point number "point" is therefore given by
*        "ptr_in[coord][point]".
*     forward
*        A non-zero value indicates that the Mapping's forward
*        coordinate transformation is to be applied, while a zero
*        value indicates that the inverse transformation should be
*        used.
*     ncoord_out
*        The number of coordinates being generated by the Mapping for
*        each output point (i.e. the number of dimensions of the space
*        in which the output points reside). This need not be the same
*        as "ncoord_in".
*     ptr_out
*        An array of pointers to double, with "ncoord_out"
*        elements. Element "ptr_out[coord]" should point at the first
*        element of an array of double (with "npoint" elements) into
*        which the values of coordinate number "coord" for each output
*        (transformed) point will be written.  The value of coordinate
*        number "coord" for output point number "point" will therefore
*        be found in "ptr_out[coord][point]".

*  Notes:
*     - If the forward coordinate transformation is being applied, the
*     Mapping supplied must have the value of "ncoord_in" for its Nin
*     attribute and the value of "ncoord_out" for its Nout
*     attribute. If the inverse transformation is being applied, these
*     values should be reversed.
*     - This routine is not available in the Fortran 77 interface to
*     the AST library.
c--
*/

/* Local Variables: */
   AstPointSet *in_points;       /* Pointer to input PointSet */
   AstPointSet *out_points;      /* Pointer to output PointSet */

/* Check the global error status. */
   if ( !astOK ) return;

/* Validate the Mapping and number of points/coordinates. */
   ValidateMapping( this, forward, npoint, ncoord_in, ncoord_out, "astTranP" );

/* Create PointSets to describe the input and output points. */
   if ( astOK ) {
      in_points = astPointSet( npoint, ncoord_in, "" );
      out_points = astPointSet( npoint, ncoord_out, "" );

/* Associate the data pointers with the PointSets (note we must
   explicitly remove the "const" qualifier from the input data here,
   although they will not be modified).  */
      astSetPoints( in_points, (double **) ptr_in );
      astSetPoints( out_points, ptr_out );

/* Apply the required transformation to the coordinates. */
      (void) astTransform( this, in_points, forward, out_points );

/* If the Mapping's Report attribute is set, report the effect the
   Mapping has had on the coordinates. */
      if ( astGetReport( this ) ) astReportPoints( this, forward,
                                                   in_points, out_points );

/* Delete the two PointSets. */
      astDelete( in_points );
      astDelete( out_points );
   }
}

static AstPointSet *Transform( AstMapping *this, AstPointSet *in,
                               int forward, AstPointSet *out ) {
/*
*+
*  Name:
*     astTransform

*  Purpose:
*     Transform a set of points.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "mapping.h"
*     AstPointSet *astTransform( AstMapping *this, AstPointSet *in,
*                                int forward, AstPointSet *out )

*  Class Membership:
*     Mapping method.

*  Description:
*     This function takes a Mapping and a set of points encapsulated
*     in a PointSet, and applies either the forward or inverse
*     coordinate transformation (if defined by the Mapping) to the
*     points.

*  Parameters:
*     this
*        Pointer to the Mapping. The nature of the coordinate
*        transformation will depend on the class of Mapping
*        supplied. Note that there is no constructor for the Mapping
*        class itself, so this object should be from a derived class.
*     in
*        Pointer to the PointSet holding the input coordinate data.
*     forward
*        A non-zero value indicates that the forward coordinate
*        transformation should be applied, while a zero value requests
*        the inverse transformation.
*     out
*        Pointer to a PointSet which will hold the transformed
*        (output) coordinate values. A NULL value may also be given,
*        in which case a new PointSet will be created by this
*        function.

*  Returned Value:
*     Pointer to the output (possibly new) PointSet.

*  Notes:
*     - An error will result if the Mapping supplied does not define
*     the requested coordinate transformation (either forward or
*     inverse).
*     - The number of coordinate values per point in the input
*     PointSet must match the number of input coordinates for the
*     Mapping being applied (or number of output coordinates if the
*     inverse transformation is requested).
*     - If an output PointSet is supplied, it must have space for
*     sufficient number of points and coordinate values per point to
*     accommodate the result (e.g. the number of Mapping output
*     coordinates, or number of input coordinates if the inverse
*     transformation is requested). Any excess space will be ignored.
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstPointSet *result;          /* Pointer to output PointSet */
   int def;                      /* Coordinate transformation defined? */
   int ncoord_in;                /* Number of input PointSet coordinates */
   int ncoord_out;               /* Number of coordinates in output PointSet */
   int nin;                      /* Number of input Mapping coordinates */
   int nout;                     /* Number of output Mapping coordinates */
   int npoint;                   /* Number of points to transform */
   int npoint_out;               /* Number of points in output PointSet */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise. */
   result = NULL;

/* Determine if a coordinate transformation is defined for the requested
   direction. */
   def = forward ? astGetTranForward( this ) : astGetTranInverse( this );

/* Report an error if the transformation is not defined. */
   if ( astOK && !def ) {
      astError( AST__TRNND, "astTransform(%s): %s coordinate transformation "
                "is not defined by the %s supplied.", astGetClass( this ),
                forward ? "A forward" : "An inverse", astGetClass( this ) );
   }

/* Obtain the effective number of input and output coordinate values for the
   transformation to be performed, taking account of the transformation
   direction required. Note we use Mapping methods to obtain these values, as
   this will take account of whether the Mapping has been inverted. */
   nin = forward ? astGetNin( this ) : astGetNout( this );
   nout = forward ? astGetNout( this ) : astGetNin( this );

/* Obtain the number of input points to transform and the number of coordinate
   values per input point. */
   npoint = astGetNpoint( in );
   ncoord_in = astGetNcoord( in );

/* If OK, check that the number of input coordinates matches the number
   required by the mapping. Report an error if these numbers do not match. */
   if ( astOK && ( ncoord_in != nin ) ) {
      astError( AST__NCPIN, "astTransform(%s): Bad number of coordinate "
                "values (%d) in input %s.", astGetClass( this ), ncoord_in,
                astGetClass( in ) );
      astError( AST__NCPIN, "The %s given requires %d coordinate value(s) for "
                "each input point.", astGetClass( this ), nin );
   }

/* If still OK, and a non-NULL pointer has been given for the output PointSet,
   then obtain the number of points and number of coordinates per point for
   this PointSet. */
   if ( astOK && out ) {
      npoint_out = astGetNpoint( out );
      ncoord_out = astGetNcoord( out );

/* Check that the dimensions of this PointSet are adequate to accommodate the
   output coordinate values and report an error if they are not. */
      if ( astOK ) {
         if ( npoint_out < npoint ) {
            astError( AST__NOPTS, "astTransform(%s): Too few points (%d) in "
                      "output %s.", astGetClass( this ), npoint_out,
                      astGetClass( out ) );
            astError( AST__NOPTS, "The %s needs space to hold %d transformed "
                      "point(s).", astGetClass( this ), npoint );
         } else if ( ncoord_out < nout ) {
            astError( AST__NOCTS, "astTransform(%s): Too few coordinate "
                      "values per point (%d) in output %s.",
                      astGetClass( this ), ncoord_out, astGetClass( out ) );
            astError( AST__NOCTS, "The %s supplied needs space to store %d "
                      "coordinate value(s) per transformed point.",
                      astGetClass( this ), nout );
         }
      }
   }

/* If all the validation stages are passed successfully, and a NULL output
   pointer was given, then create a new PointSet to encapsulate the output
   coordinate data. */
   if ( astOK ) {
      if ( !out ) {
         result = astPointSet( npoint, nout, "" );

/* Otherwise, use the PointSet supplied. */
      } else {
         result = out;
      }
   }

/* Return a pointer to the output PointSet. Note that we do not actually
   transform (or even copy) the coordinates. This is left for derived classes
   to implement. */
   return result;
}

static double UphillSimplex( const MapData *mapdata, double acc, int maxcall,
                             const double dx[], double xmax[], double *err,
                             int *ncall ) {
/*
*  Name:
*     UphillSimplex

*  Purpose:
*     Find a function maximum using a modification of the simplex method.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     double UphillSimplex( const MapData *mapdata, double acc, int maxcall,
*                           const double dx[], double xmax[], double *err,
*                           int *ncall );

*  Class Membership:
*     Mapping member function.

*  Description:
*     This function applies a modification of the simplex method to
*     find a local maximum in the value returned by a Mapping
*     function. The modification used allows the method to cope with
*     coordinate constraints and (equivalently) regions where the
*     function returns "bad" values. The method is robust and not
*     susceptible to overflow, so is suitable for applying to Mapping
*     functions of unknown form.

*  Parameters:
*     mapdata
*        Pointer to a MapData structure which describes the Mapping
*        function, its coordinate constraints, etc.
*     acc
*        The accuracy required in the value of the maximum.
*     maxcall
*        The maximum number of Mapping function evaluations to use.
*     dx
*        Pointer to an array of double containing an offset along each
*        input coordinate for the Mapping function supplied. These
*        offsets will be used to construct the initial simplex
*        (i.e. they are the initial "step lengths" for each
*        coordinate) and may be positive or negative.
*     xmax
*        Pointer to an array of double which contains the coordinates
*        of an initial estimate of the location of the maximum. On
*        exit, this will be updated to contain the best estimate of
*        the location of the maximum as generated by this function.
*     err
*        Pointer to a double in which to return an estimate of the
*        error in the value of the maximum found. For normal
*        convergence, this should be no larger than "acc". However, if
*        the maximum number of Mapping function evaluations is
*        reached, the returned value may be larger than this, although
*        it should still be valid. In such cases, re-starting the
*        algorithm at the new location returned in "xmax" may be
*        advisable.
*     ncall
*        Pointer to an int in which the number of Mapping function
*        evaluations will be returned.

*  Returned Value:
*     An estimate of the Mapping function value at the local maximum.

*  Notes:
*     - The function may return before the requested accuracy has been
*     met and before all Mapping function evaluations have been
*     made. This signifies that an excessive number of function values
*     have been needed outside the coordinate constraints. This is
*     only likely if the function is unable to make progress near such
*     a constraint, in which case the algorithm should probably be
*     re-started.
*     - A value of AST__BAD will be returned if no maximum could be
*     found.  This means that all the Mapping function evaluations
*     performed returned a value of AST__BAD.
*     - A value of AST__BAD will also be returned and no useful
*     information about a solution will be produced if this routine is
*     invoked with the global error status set, or if it should fail
*     for any reason.
*/

/* Local Constants: */
   const double factor = 3.0;    /* Simplex contraction/expansion factor */

/* Local Variables: */
   double *f;                    /* Pointer to array of function values */
   double *x;                    /* Pointer to array of vertex coordinates */
   double *xnew;                 /* Pointer to workspace array */
   double fnew;                  /* New function value */
   double fsave;                 /* Saved function value */
   double offset;                /* Coordinate difference between vertices */
   double range;                 /* Range of simplex values */
   double result;                /* Value to return */
   double tmp;                   /* Temporary store for coordinate */
   int coord;                    /* Loop counter for coordinates */
   int hi;                       /* Index of best vertex */
   int lo;                       /* Index of worst vertex */
   int ncalla;                   /* Number of function calls attempted */
   int ncoord;                   /* Number of function dimensions */
   int nextlo;                   /* Index of second worst vertex */
   int nvertex;                  /* Number of simplex vertices */
   int vertex;                   /* Loop counter for vertices */

/* Initialise. */
   result = AST__BAD;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Further initialisation. */
   *err = DBL_MAX;
   *ncall = 0;

/* Obtain the number of input coordinates for the Mapping function and
   calculate the number of simplex vertices. */
   ncoord = mapdata->nin;
   nvertex = ncoord + 1;

/* Allocate workspace. */
   f = astMalloc( sizeof( double ) * (size_t) nvertex );
   x = astMalloc( sizeof( double ) * (size_t) ( ncoord * nvertex ) );
   xnew = astMalloc( sizeof( double ) * (size_t) ncoord );
   if ( astOK ) {

/* Loop to set up an initial simplex. */
      for ( vertex = 0; vertex < nvertex; vertex++ ) {
         for ( coord = 0; coord < ncoord; coord++ ) {
            tmp = xmax[ coord ];

/* Displace each point (except the first) the required amount along
   one of the axes to generate the coordinates of the simplex
   vertices. */
            if ( coord == ( vertex - 1 ) ) tmp += dx[ coord ];
            x[ vertex * ncoord + coord ] = tmp;
         }

/* Evaluate the Mapping function at each vertex. */
         f[ vertex ] = MapFunction( mapdata, &x[ vertex * ncoord ], ncall );
         if ( f[ vertex ] == AST__BAD ) f[ vertex ] = -DBL_MAX;
      }

/* Initialise the number of times we attempt to call the Mapping
   function (not necessarily the same as the number of times it was
   actually called, which is stored in *ncall). */
      ncalla = nvertex;

/* Loop until convergence is reached or an error occurs. */
      while( astOK ) {

/* Initialise the index of the lowest vertex of the simplex, the next
   lowest vertex and the highest vertex. */
         lo = ( f[ 0 ] < f[ 1 ] ) ? 0 : 1;
         nextlo = 1 - lo;
         hi = 0;

/* Loop to inspect each vertex and update these values. Ensure that in
   the case of equal vertices, the first one is taken to be the
   highest. This makes the maximisation stable (so that if no better
   maximum can be found, the original position is returned rather than
   a nearby position that yields the same function value). */
         for ( vertex = 0; vertex < nvertex; vertex++ ) {
            if ( f[ vertex ] <= f[ lo ] ) {
               nextlo = lo;
               lo = vertex;
            } else if ( ( f[ vertex ] <= f[ nextlo ] ) && ( vertex != lo ) ) {
               nextlo = vertex;
            }
            if ( f[ vertex ] > f[ hi ] ) hi = vertex;
         }

/* Estimate the error on the result as the difference between the
   highest and lowest simplex vertices. */
         if ( ( f[ hi ] == -DBL_MAX ) || ( f[ lo ] == -DBL_MAX ) ) {
            range = DBL_MAX;
         } else {
            range = f[ hi ] - f[ lo ];
         }

/* Test for convergence. Ideally, the accuracy criterion should have
   been met. However, also quit if the maximum number of Mapping
   function evaluations has been reached, or the number of points at
   which function values have been requested reaches three times this
   limit (this latter number will typically be larger because points
   lying outside the coordinate constraints do not result in the
   Mapping function being evaluated). */
         if ( range <= fabs( acc ) ||
              ( *ncall >= maxcall ) || ( ncalla >= ( 3 * maxcall ) ) ) {

/* If quitting, return the coordinates and function value at the best
   simplex vertex, and the error estimate. */
            for ( coord = 0; coord < ncoord; coord++ ) {
               xmax[ coord ] = x[ hi * ncoord + coord ];
            }
            result = ( f[ hi ] == -DBL_MAX ) ? AST__BAD : f[ hi ];
            *err = range;
            break;
         }

/* If performing another iteration, first try reflecting the worst
   vertex through the opposite face of the simplex. Check for
   errors. */
         fnew = NewVertex( mapdata, lo, -1.0, x, f, ncall, xnew );
         ncalla++;
         if ( astOK ) {

/* If this results in a point lying in a forbiddden region (either
   outside the coordinate constraints or where the Mapping function
   yields bad coordinate values), then we must make a departure from
   the standard simplex algorithm. This is because the inability to
   make forward progress in this case can cause the simplex to
   repeatedly contract about each face (except one) in turn. This
   mechanism normally results in lateral contraction as the simplex
   attempts to squeeze through a narrow gap which is impeding
   progress. However, in this case there is no gap to get through, so
   the lateral contraction can eventually make the simplex become
   degenerate (due to rounding). This prevents it from expanding
   laterally again and exploring the region adjacent to the constraint
   boundary once it has become small enough. */
            if ( fnew == AST__BAD ) {

/* To overcome this, we instead contract the worst simplex vertex
   towards the best vertex (this has the cumulative effect of
   contracting the simplex without changing its shape). First find the
   offset in each coordinate between these two vertices. */
               for ( coord = 0; coord < ncoord; coord++ ) {
                  offset = x[ lo * ncoord + coord ] - x[ hi * ncoord + coord ];

/* Scale the offset to obtain the new coordinate. */
                  x[ lo * ncoord + coord ] = x[ hi * ncoord + coord ] +
                                             offset / factor;

/* If the distance between the two vertices has not decreased, we are
   in a region where rounding errors prevent them approaching each
   other any more closely, so simply set them equal. */
                  if ( fabs( x[ lo * ncoord + coord ] -
                             x[ hi * ncoord + coord ] ) >= fabs( offset ) ) {
                     x[ lo * ncoord + coord ] = x[ hi * ncoord + coord ];
                  }
               }

/* Evaluate the Mapping function at the new vertex. */
               f[ lo ] = MapFunction( mapdata, &x[ lo * ncoord ], ncall );
               if ( f[ lo ] == AST__BAD ) f[ lo ] = -DBL_MAX;
               ncalla++;

/* We now return to the standard simplex algorithm. If the new vertex
   is a new maximum, then see if more of the same is even better by
   trying to expand the best vertex away from the opposite face. */
            } else if ( fnew >= f[ hi ] ) {
               fnew = NewVertex( mapdata, lo, factor, x, f, ncall, xnew );
               ncalla++;

/* Otherwise, if the new vertex was no improvement on the second
   worst, then try contracting the worst vertex towards the opposite
   face. */
            } else if ( fnew <= f[ nextlo ] ) {
               fsave = f[ lo ];
               fnew = NewVertex( mapdata, lo, 1.0 / factor, x, f, ncall, xnew );
               ncalla++;

/* If this didn't result in any improvement, then contract the entire
   simplex towards the best vertex. Use the same approach as earlier
   to protect against rounding so that all the simplex vertices will
   eventually coalesce if this process is repeated enough times. */
               if ( astOK && ( fnew <= fsave ) ) {
                  for ( vertex = 0; vertex < nvertex; vertex++ ) {
                     if ( vertex != hi ) {
                        for ( coord = 0; coord < ncoord; coord++ ) {
                           offset = x[ vertex * ncoord + coord ] -
                                    x[ hi * ncoord + coord ];
                           x[ vertex * ncoord + coord ] =
                               x[ hi * ncoord + coord ] + offset / factor;
                           if ( fabs( x[ vertex * ncoord + coord ] -
                                      x[ hi * ncoord + coord ] ) >=
                                fabs( offset ) ) {
                              x[ vertex * ncoord + coord ] =
                                 x[ hi * ncoord + coord ];
                           }
                        }

/* Evaluate the Mapping function at each new vertex. */
                        f[ vertex ] = MapFunction( mapdata,
                                                   &x[ vertex * ncoord ],
                                                   ncall );
                        if ( f[ vertex ] == AST__BAD ) f[ vertex ] = -DBL_MAX;
                        ncalla++;
                     }
                  }
               }
            }
         }
      }
   }

/* Free workspace. */
   f = astFree( f );
   x = astFree( x );
   xnew = astFree( xnew );

/* If an error occurred, clear the returned result. */
   if ( !astOK ) result = AST__BAD;

/* Return the result. */
   return result;
}

static void ValidateMapping( AstMapping *this, int forward,
                             int npoint, int ncoord_in, int ncoord_out,
                             const char *method ) {
/*
*  Name:
*     ValidateMapping

*  Purpose:
*     Validate a Mapping for use to transform coordinates.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     void ValidateMapping( AstMapping *this, int forward,
*                           int npoint, int ncoord_in, int ncoord_out,
*                           const char *method )

*  Class Membership:
*     Mapping member function.

*  Description:
*     This function checks that a Mapping is suitable for transforming
*     a set of points. It also checks that the number of points and
*     the number of coordinate values per point is valid. If an error
*     is detected, the global error status is set and an error report
*     made. Otherwise, the function returns without further action.

*  Parameters:
*     this
*        Pointer to the Mapping.
*     forward
*        A non-zero value indicates that the forward coordinate
*        transformation is to be checked, while a zero value requests
*        the inverse transformation.
*     npoint
*        The number of points being transformed.
*     ncoord_in
*        The number of coordinates associated with each input point.
*     ncoord_out
*        The number of coordinates associated with each output point.
*     method
*        Pointer to a null terminated character string containing the
*        name of the method which invoked this function to validate a
*        Mapping. This is used solely for constructing error messages.
*/

/* Local Variables: */
   int nin;                    /* Mapping Nin attribute value */
   int nout;                   /* Mapping Nout attribute value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Report an error if the requested transformation is not defined. */
   if ( !( forward ? astGetTranForward( this ) : astGetTranInverse( this ) )
        && astOK ) {
      astError( AST__TRNND, "%s(%s): %s coordinate transformation "
                "is not defined by the %s supplied.", method,
                astGetClass( this ),
                ( forward ? "A forward" : "An inverse" ),
                astGetClass( this ) );
   }

/* Obtain the effective values of the Nin and Nout attributes for the
   Mapping. */
   nin = forward ? astGetNin( this ) : astGetNout( this );
   nout = forward ? astGetNout( this ) : astGetNin( this );

/* If OK, check that the number of input coordinates matches the
   number required by the Mapping. Report an error if these numbers do
   not match. */
   if ( astOK && ( ncoord_in != nin ) ) {
      astError( AST__NCPIN, "%s(%s): Bad number of input coordinate values "
                "(%d).", method, astGetClass( this ), ncoord_in );
      astError( AST__NCPIN, "The %s given requires %d coordinate value%s for "
                "each input point.", astGetClass( this ), nin,
                ( nin == 1 ) ? "" : "s" );
   }

/* If OK, also check that the number of output coordinates matches the
   number required by the Mapping. Report an error if these numbers do
   not match. */
   if ( astOK && ( ncoord_out != nout ) ) {
      astError( AST__NCPIN, "%s(%s): Bad number of output coordinate values "
                "(%d).", method, astGetClass( this ), ncoord_out );
      astError( AST__NCPIN, "The %s given generates %s%d coordinate value%s "
                "for each output point.", astGetClass( this ),
                ( nout < ncoord_out ) ? "only " : "", nout,
                ( nout == 1 ) ? "" : "s" );
   }

/* Check that the number of points being transformed is not negative
   and report an error if necessary. */
   if ( astOK && ( npoint < 0 ) ) {
      astError( AST__NPTIN, "%s(%s): Number of points to be transformed (%d) "
                "is invalid.", method, astGetClass( this ), npoint );
   }
}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   this class using the macros defined for this purpose in the
   "object.h" file. */
/*
*att++
*  Name:
*     Invert

*  Purpose:
*     Mapping inversion flag.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This attribute controls which one of a Mapping's two possible
*     coordinate transformations is considered the "forward"
*     transformation (the other being the "inverse"
*     transformation). If the attribute value is zero (the default),
*     the Mapping's behaviour will be the same as when it was first
*     created. However, if it is non-zero, its two transformations
*     will be inter-changed, so that the Mapping displays the inverse
*     of its original behaviour.
*
*     Inverting the boolean sense of the Invert attribute will cause
*     the values of a Mapping's Nin and Nout attributes to be
*     interchanged. The values of its TranForward and TranInverse
*     attributes will also be interchanged. This operation may be
c     performed with the astInvert function.
f     performed with the AST_INVERT routine.

*  Applicability:
*     Mapping
*        All Mappings have this attribute.
*     UnitMap
*        The value of the Invert attribute has no effect on the
*        behaviour of a UnitMap.
*     FrameSet
*        Inverting the boolean sense of the Invert attribute for a
*        FrameSet will cause its base and current Frames (and its Base
*        and Current attributes) to be interchanged. This, in turn,
*        may affect other properties and attributes of the FrameSet
*        (such as Nin, Nout, Naxes, TranForward, TranInverse,
*        etc.). The Invert attribute of a FrameSet is not itself
*        affected by selecting a new base or current Frame.
*att--
*/
/* This ia a boolean value (0 or 1) with a value of -INT_MAX when
   undefined but yielding a default of zero. */
astMAKE_CLEAR(Mapping,Invert,invert,-INT_MAX)
astMAKE_GET(Mapping,Invert,int,0,( ( this->invert == -INT_MAX ) ?
                                   0 : this->invert ))
astMAKE_SET(Mapping,Invert,int,invert,( value != 0 ))
astMAKE_TEST(Mapping,Invert,( this->invert != -INT_MAX ))

/*
*att++
*  Name:
*     Nin

*  Purpose:
*     Number of input coordinates for a Mapping.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer, read-only.

*  Description:
*     This attribute gives the number of coordinate values required to
*     specify an input point for a Mapping (i.e. the number of
*     dimensions of the space in which the Mapping's input points
*     reside).

*  Applicability:
*     Mapping
*        All Mappings have this attribute.
*     CmpMap
*        If a CmpMap's component Mappings are joined in series, then
*        its Nin attribute is equal to the Nin attribute of the first
*        component (or to the Nout attribute of the second component
*        if the the CmpMap's Invert attribute is non-zero).
*
*        If a CmpMap's component Mappings are joined in parallel, then
*        its Nin attribute is given by the sum of the Nin attributes
*        of each component (or to the sum of their Nout attributes if
*        the CmpMap's Invert attribute is non-zero).
*     Frame
*        The Nin attribute for a Frame is always equal to the number
*        of Frame axes (Naxes attribute).
*     FrameSet
*        The Nin attribute of a FrameSet is equal to the number of
*        axes (Naxes attribute) of its base Frame (as specified by the
*        FrameSet's Base attribute). The Nin attribute value may
*        therefore change if a new base Frame is selected.
*att-- 
*/

/*
*att++
*  Name:
*     Nout

*  Purpose:
*     Number of output coordinates for a Mapping.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer, read-only.

*  Description:
*     This attribute gives the number of coordinate values generated
*     by a Mapping to specify each output point (i.e. the number of
*     dimensions of the space in which the Mapping's output points
*     reside).

*  Applicability:
*     Mapping
*        All Mappings have this attribute.
*     CmpMap
*        If a CmpMap's component Mappings are joined in series, then
*        its Nout attribute is equal to the Nout attribute of the
*        second component (or to the Nin attribute of the first
*        component if the the CmpMap's Invert attribute is non-zero).
*
*        If a CmpMap's component Mappings are joined in parallel, then
*        its Nout attribute is given by the sum of the Nout attributes
*        of each component (or to the sum of their Nin attributes if
*        the CmpMap's Invert attribute is non-zero).
*     Frame
*        The Nout attribute for a Frame is always equal to the number
*        of Frame axes (Naxes attribute).
*     FrameSet
*        The Nout attribute of a FrameSet is equal to the number of
*        FrameSet axes (Naxes attribute) which, in turn, is equal to
*        the Naxes attribute of the FrameSet's current Frame (as
*        specified by the Current attribute). The Nout attribute value
*        may therefore change if a new current Frame is selected.
*att--
*/

/*
*att++
*  Name:
*     Report

*  Purpose:
*     Report transformed coordinates?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This attribute controls whether coordinate values are reported
*     whenever a Mapping is used to transform a set of points. If its
*     value is zero (the default), no report is made. However, if it
*     is non-zero, the coordinates of each point are reported (both
*     before and after transformation) by writing them to standard
*     output.
*
*     This attribute is provided as an aid to debugging, and to avoid
*     having to report values explicitly in simple programs.

*  Applicability:
*     Mapping
*        All Mappings have this attribute.
*     CmpMap
*        When applied to a compound Mapping (CmpMap), only the Report
*        attribute of the CmpMap, and not those of its component
*        Mappings, is used.  Coordinate information is never reported
*        for the component Mappings individually, only for the
*        complete CmpMap.
*     Frame
*        When applied to any Frame, the formatting capabilities of the
c        Frame (as provided by the astFormat function) will be used to
f        Frame (as provided by the AST_FORMAT function) will be used to
*        format the reported coordinates.
*     FrameSet
*        When applied to any FrameSet, the formatting capabilities of
*        the base and current Frames will be used (as above) to
*        individually format the input and output coordinates, as
*        appropriate. The Report attribute of a FrameSet is not itself
*        affected by selecting a new base or current Frame, but the
*        resulting formatting capabilities may be.

*  Notes:
*     - Unlike most other attributes, the value of the Report
*     attribute is not transferred when a Mapping is copied. Instead,
*     its value is undefined (and therefore defaults to zero) in any
*     copy. Similarly, it becomes undefined in any external
c     representation of a Mapping produced by the astWrite function.
f     representation of a Mapping produced by the AST_WRITE routine.
*att--
*/
/* This ia a boolean value (0 or 1) with a value of -INT_MAX when
   undefined but yielding a default of zero. */
astMAKE_CLEAR(Mapping,Report,report,-INT_MAX)
astMAKE_GET(Mapping,Report,int,0,( ( this->report == -INT_MAX ) ?
                                   0 : this->report ))
astMAKE_SET(Mapping,Report,int,report,( value != 0 ))
astMAKE_TEST(Mapping,Report,( this->report != -INT_MAX ))

/*
*att++
*  Name:
*     TranForward

*  Purpose:
*     Forward transformation defined?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean), read-only.

*  Description:
*     This attribute indicates whether a Mapping is able to transform
*     coordinates in the "forward" direction (i.e. converting input
*     coordinates into output coordinates). If this attribute is
*     non-zero, the forward transformation is available. Otherwise, it
*     is not.

*  Applicability:
*     Mapping
*        All Mappings have this attribute.
*     CmpMap
*        The TranForward attribute value for a CmpMap is given by the
*        boolean AND of the value for each component Mapping.
*     FrameSet
*        The TranForward attribute of a FrameSet applies to the
*        transformation which converts between the FrameSet's base
*        Frame and its current Frame (as specified by the Base and
*        Current attributes). This value is given by the boolean AND
*        of the TranForward values which apply to each of the
*        individual sub-Mappings required to perform this conversion.
*        The TranForward attribute value for a FrameSet may therefore
*        change if a new Base or Current Frame is selected.

*  Notes:
*     - An error will result if a Mapping with a TranForward value of
*     zero is used to transform coordinates in the forward direction.
*att--
*/

/*
*att++
*  Name:
*     TranInverse

*  Purpose:
*     Inverse transformation defined?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean), readonly.

*  Description:
*     This attribute indicates whether a Mapping is able to transform
*     coordinates in the "inverse" direction (i.e. converting output
*     coordinates back into input coordinates). If this attribute is
*     non-zero, the inverse transformation is available. Otherwise, it
*     is not.

*  Applicability:
*     Mapping
*        All Mappings have this attribute.
*     CmpMap
*        The TranInverse attribute value for a CmpMap is given by the
*        boolean AND of the value for each component Mapping.
*     FrameSet
*        The TranInverse attribute of a FrameSet applies to the
*        transformation which converts between the FrameSet's current
*        Frame and its base Frame (as specified by the Current and
*        Base attributes). This value is given by the boolean AND of
*        the TranInverse values which apply to each of the individual
*        sub-Mappings required to perform this conversion.
*        The TranInverse attribute value for a FrameSet may therefore
*        change if a new Base or Current Frame is selected.

*  Notes:
*     - An error will result if a Mapping with a TranInverse value of
*     zero is used to transform coordinates in the inverse direction.
*att--
*/

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for Mapping objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout )

*  Description:
*     This function implements the copy constructor for Mapping objects.

*  Parameters:
*     objin
*        Pointer to the Mapping to be copied.
*     objout
*        Pointer to the Mapping being constructed.

*  Notes:
*     - This constructor exists simply to ensure that the "Report"
*     attribute is cleared in any copy made of a Mapping.
*/

/* Local Variables: */
   AstMapping *out;              /* Pointer to output Mapping */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the output Mapping. */
   out = (AstMapping *) objout;

/* Clear the output Report attribute. */
   out->report = -INT_MAX;
}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for Mapping objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj )

*  Description:
*     This function implements the destructor for Mapping objects.

*  Parameters:
*     obj
*        Pointer to the Mapping to be deleted.

*  Notes:
*     - This destructor does nothing and exists only to maintain a
*     one-to-one correspondence between destructors and copy
*     constructors.
*/

/* Return without action. */
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for Mapping objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel )

*  Description:
*     This function implements the Dump function which writes out data
*     for the Mapping class to an output Channel.

*  Parameters:
*     this
*        Pointer to the Mapping whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*/

/* Local Variables: */
   AstMapping *this;             /* Pointer to the Mapping structure */
   int invert;                   /* Mapping inverted? */
   int ival;                     /* Integer value */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Mapping structure. */
   this = (AstMapping *) this_object;

/* Write out values representing the instance variables for the
   Mapping class.  Accompany these with appropriate comment strings,
   possibly depending on the values being written.*/

/* In the case of attributes, we first use the appropriate (private)
   Test...  member function to see if they are set. If so, we then use
   the (private) Get... function to obtain the value to be written
   out.

   For attributes which are not set, we use the astGet... method to
   obtain the value instead. This will supply a default value
   (possibly provided by a derived class which over-rides this method)
   which is more useful to a human reader as it corresponds to the
   actual default attribute value.  Since "set" will be zero, these
   values are for information only and will not be read back. */

/* Determine if the Mapping is inverted. The output values
   (e.g. number of input and output coordinates) will refer to the
   Mapping ***before*** this inversion flag is applied, but we need it
   when using (e.g.) the astGetNin/astGetNout methods to determine
   which one will return the required value. */
   invert = astGetInvert( this );

/* (NB. there is a subtle point here that dictates the extent to which
   this inversion flag can be used... All use of methods (such as
   astGetInvert, which might be over-ridden by derived classes) must
   be restricted to determining the values of "unset" output
   quantities only (below). This is because when re-loading the
   Mapping, the derived classes will not have been loaded at the point
   when these values are re-read - hence any value whose
   interpretation depends on these methods cannot be reliably
   recovered.) */

/* Nin. */
/* ---- */
/* Use the instance variable directly to avoid the effect of the
   Invert attribute on the private member function. Treat zero as the
   default. */
   set = ( this->nin != 0 );
   ival = set ? this->nin : ( !invert ? astGetNin( this ) :
                                        astGetNout( this ) );
   astWriteInt( channel, "Nin", set, 0, ival,
                "Number of input coordinates" );

/* Nout. */
/* ----- */
/* Use the instance variable directly. Treat zero as the default. */
   set = ( this->nout != this->nin );
   ival = set ? this->nout : ( !invert ? astGetNout( this ) :
                                         astGetNin( this ) );
   astWriteInt( channel, "Nout", set, 0, ival,
                "Number of output coordinates" );

/* Invert. */
/* ------- */
   set = TestInvert( this );
   ival = set ? GetInvert( this ) : astGetInvert( this );
   astWriteInt( channel, "Invert", set, 0, ival,
                ival ? "Mapping inverted" :
                       "Mapping not inverted" );

/* TranForward. */
/* ------------ */
/* Use the instance variable directly. Treat 1 as the default. */
   set = ( this->tran_forward == 0 );
   ival = set ? this->tran_forward : ( !invert ? astGetTranForward( this ) :
                                                 astGetTranInverse( this ) );
   astWriteInt( channel, "Fwd", set, 0, ival,
                ival ? "Forward transformation defined" :
                       "Forward transformation not defined" );

/* TranInverse. */
/* ------------ */
/* Use the instance variable directly. Treat 1 as the default. */
   set = ( this->tran_inverse == 0 );
   ival = set ? this->tran_inverse : ( !invert ? astGetTranInverse( this ) :
                                                 astGetTranForward( this ) );
   astWriteInt( channel, "Inv", set, 0, ival,
                ival ? "Inverse transformation defined" :
                       "Inverse transformation not defined" );

/* Report. */
/* ------- */
   set = TestReport( this );
   ival = set ? GetReport( this ) : astGetReport( this );
   astWriteInt( channel, "Report", set, 0, ival,
                ival ? "Report coordinate transformations" :
                       "Don't report coordinate transformations" );
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAMapping and astCheckMapping functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(Mapping,Object,check,&class_init)
astMAKE_CHECK(Mapping)

AstMapping *astInitMapping_( void *mem, size_t size, int init,
                             AstMappingVtab *vtab, const char *name,
                             int nin, int nout,
                             int tran_forward, int tran_inverse ) {
/*
*+
*  Name:
*     astInitMapping

*  Purpose:
*     Initialise a Mapping.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "mapping.h"
*     AstMapping *astInitMapping( void *mem, size_t size, int init,
*                                 AstMappingVtab *vtab, const char *name,
*                                 int nin, int nout,
*                                 int tran_forward, int tran_inverse )

*  Class Membership:
*     Mapping initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new Mapping object. It allocates memory (if necessary) to accommodate
*     the Mapping plus any additional data associated with the derived class.
*     It then initialises a Mapping structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a Mapping at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the Mapping is to be initialised.
*        This must be of sufficient size to accommodate the Mapping data
*        (sizeof(Mapping)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the Mapping (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the Mapping
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the Mapping's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new Mapping.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).
*     nin
*        The number of coordinate values per input point.
*     nout
*        The number of coordinate vales per output point.
*     tran_forward
*        A non-zero value indicates that the Mapping will be able to
*        transform coordinates in the forward direction. A zero value
*        indicates that it will not.
*     tran_inverse
*        A non-zero value indicates that the Mapping will be able to
*        transform coordinates in the inverse direction. A zero value
*        indicates that it will not.

*  Returned Value:
*     A pointer to the new Mapping.

*  Notes:
*     -  The Mappings produced by this function implement all the basic methods
*     defined by the Mapping class. However, their astTransform method does not
*     actually perform any coordinate transformation (although it performs all
*     necessary argument validation and creates an output PointSet if
*     necessary, leaving its coordinate values undefined).
*     -  This means that Mappings produced by this function are of limited use
*     on their own, but may easily be extended by a derived class simply by
*     over-riding the astTransform method to add the necessary coordinate
*     arithmetic.
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstMapping *new;              /* Pointer to new Mapping */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise. */
   new = NULL;

/* Check the initialisation values for validity, reporting an error if
   necessary. */
   if ( nin < 0 ) {
      astError( AST__BADNI, "astInitMapping(%s): Bad number of input "
                "coordinates (%d).", name, nin );
      astError( AST__BADNI, "This number should be zero or more." );
   } else if ( nout < 0 ) {
      astError( AST__BADNO, "astInitMapping(%s): Bad number of output "
                "coordinates (%d).", name, nout );
      astError( AST__BADNI, "This number should be zero or more." );
   }

/* Check that the coordinate transformation is defined in at least one
   direction (forward or inverse) and report an error if it is not. */
   if ( astOK ) {
      if ( !tran_forward && !tran_inverse ) {
         astError( AST__NODEF, "astInitMapping(%s): The coordinate "
                   "transformation is not defined in either the forward or "
                   "inverse direction.", name );
      }
   }

/* Initialise an Object structure (the parent class) as the first component
   within the Mapping structure, allocating memory if necessary. */
   new = (AstMapping *) astInitObject( mem, size, init,
                                       (AstObjectVtab *) vtab, name );

/* If necessary, initialise the virtual function table. */
/* ---------------------------------------------------- */
   if ( init ) InitVtab( vtab );
   if ( astOK ) {

/* Initialise the Mapping data. */
/* ---------------------------- */
/* Store the numbers of input and output coordinates. */
      new->nin = nin;
      new->nout = nout;

/* Store the flags indicating which coordinate transformations are
   defined (constrain these values to 0 or 1). */
      new->tran_forward = ( tran_forward != 0 );
      new->tran_inverse = ( tran_inverse != 0 );

/* Initialise other attributes to their undefined values. */
      new->invert = -INT_MAX;
      new->report = -INT_MAX;

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new object. */
   return new;
}

AstMapping *astLoadMapping_( void *mem, size_t size, int init,
                             AstMappingVtab *vtab, const char *name,
                             AstChannel *channel ) {
/*
*+
*  Name:
*     astLoadMapping

*  Purpose:
*     Load a Mapping.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "mapping.h"
*     AstMapping *astLoadMapping( void *mem, size_t size, int init,
*                                 AstMappingVtab *vtab, const char *name,
*                                 AstChannel *channel )

*  Class Membership:
*     Mapping loader.

*  Description:
*     This function is provided to load a new Mapping using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     Mapping structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a Mapping at the start of the memory
*     passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory into which the Mapping is to be
*        loaded.  This must be of sufficient size to accommodate the
*        Mapping data (sizeof(Mapping)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the Mapping (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the Mapping structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstMapping) is used instead.
*     init
*        A boolean flag indicating if the Mapping's virtual function
*        table is to be initialised. If this value is non-zero, the
*        virtual function table will be initialised by this function.
*
*        If the "vtab" parameter is NULL, the "init" value is ignored
*        and the (static) virtual function table initialisation flag
*        for the Mapping class is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new Mapping. If this is NULL, a pointer
*        to the (static) virtual function table for the Mapping class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "Mapping" is used instead.

*  Returned Value:
*     A pointer to the new Mapping.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstMapping *new;              /* Pointer to the new Mapping */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this Mapping. In this case the
   Mapping belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstMapping );
      init = !class_init;
      vtab = &class_vtab;
      name = "Mapping";
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built Mapping. */
   new = astLoadObject( mem, size, init, (AstObjectVtab *) vtab, name,
                        channel );

/* If required, initialise the part of the virtual function table used
   by this class. */
   if ( init ) InitVtab( vtab );

/* Note if we have successfully initialised the (static) virtual
   function table owned by this class (so that this is done only
   once). */
   if ( astOK ) {
      if ( ( vtab == &class_vtab ) && init ) class_init = 1;

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "Mapping" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Nin. */
/* ---- */
      new->nin = astReadInt( channel, "nin", 0 );
      if ( new->nin < 0 ) new->nin = 0;

/* Nout. */
/* ----- */
      new->nout = astReadInt( channel, "nout", new->nin );
      if ( new->nout < 0 ) new->nout = 0;

/* Invert. */
/* ------- */
      new->invert = astReadInt( channel, "invert", -INT_MAX );
      if ( TestInvert( new ) ) SetInvert( new, new->invert );

/* TranForward. */
/* ------------ */
      new->tran_forward = ( astReadInt( channel, "fwd", 1 ) != 0 );

/* TranInverse. */
/* ------------ */
      new->tran_inverse = ( astReadInt( channel, "inv", 1 ) != 0 );

/* Report. */
/* ------- */
      new->report = astReadInt( channel, "report", -INT_MAX );
      if ( TestReport( new ) ) SetReport( new, new->report );

/* If an error occurred, clean up by deleting the new Mapping. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new Mapping pointer. */
   return new;
}

/* Virtual function interfaces. */
/* ============================ */
/* These provide the external interface to the virtual functions
   defined by this class. Each simply checks the global error status
   and then locates and executes the appropriate member function,
   using the function pointer stored in the object's virtual function
   table (this pointer is located using the astMEMBER macro defined in
   "object.h").

   Note that the member function may not be the one defined here, as
   it may have been over-ridden by a derived class. However, it should
   still have the same interface. */

int astGetNin_( AstMapping *this ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Mapping,GetNin))( this );
}
int astGetNout_( AstMapping *this ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Mapping,GetNout))( this );
}
int astGetTranForward_( AstMapping *this ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Mapping,GetTranForward))( this );
}
int astGetTranInverse_( AstMapping *this ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Mapping,GetTranInverse))( this );
}
void astInvert_( AstMapping *this ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Mapping,Invert))( this );
}
void astMapBox_( AstMapping *this,
                 const double lbnd_in[], const double ubnd_in[], int forward,
                 int coord_out, double *lbnd_out, double *ubnd_out,
                 double xl[], double xu[] ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Mapping,MapBox))( this, lbnd_in, ubnd_in, forward,
                                       coord_out, lbnd_out, ubnd_out, xl, xu );
}
void astMapList_( AstMapping *this, int series, int invert, int *nmap,
                  AstMapping ***map_list, int **invert_list ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Mapping,MapList))( this, series, invert,
                                        nmap, map_list, invert_list );
}
int astMapMerge_( AstMapping *this, int where, int series, int *nmap,
                  AstMapping ***map_list, int **invert_list ) {
   if ( !astOK ) return -1;
   return (**astMEMBER(this,Mapping,MapMerge))( this, where, series, nmap,
                                                map_list, invert_list );
}
void astReportPoints_( AstMapping *this, int forward,
                       AstPointSet *in_points, AstPointSet *out_points ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Mapping,ReportPoints))( this, forward,
                                             in_points, out_points );
}
#define MAKE_RESAMPLE_(abbrev,type) \
int astResample##abbrev##_( AstMapping *this, int ndim_in, \
                            const int *lbnd_in, const int *ubnd_in, \
                            const type *in, AstInterpolate method, \
                            double acc, int usebad, type badflag, \
                            int ndim_out, const int *lbnd_out, \
                            const int *ubnd_out, const int *lbnd, \
                            const int *ubnd, type *out ) { \
   if ( !astOK ) return 0; \
   return (**astMEMBER(this,Mapping,Resample##abbrev))( this, ndim_in, \
                                                        lbnd_in, ubnd_in, in, \
                                                        method, acc, usebad, \
                                                        badflag, ndim_out, \
                                                        lbnd_out, ubnd_out, \
                                                        lbnd, ubnd, out ); \
}
MAKE_RESAMPLE_(LD,long double)
MAKE_RESAMPLE_(D,double)
MAKE_RESAMPLE_(F,float)
MAKE_RESAMPLE_(L,long int)
MAKE_RESAMPLE_(UL,unsigned long int)
MAKE_RESAMPLE_(I,int)
MAKE_RESAMPLE_(UI,unsigned int)
MAKE_RESAMPLE_(S,short int)
MAKE_RESAMPLE_(US,unsigned short int)
MAKE_RESAMPLE_(B,signed char)
MAKE_RESAMPLE_(UB,unsigned char)
#undef MAKE_RESAMPLE_
AstMapping *astSimplify_( AstMapping *this ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Mapping,Simplify))( this );
}
AstPointSet *astTransform_( AstMapping *this, AstPointSet *in,
                            int forward, AstPointSet *out ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Mapping,Transform))( this, in, forward, out );
}
void astTran1_( AstMapping *this, int npoint, const double xin[],
                int forward, double xout[] ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Mapping,Tran1))( this, npoint, xin, forward, xout );
}
void astTran2_( AstMapping *this,
                int npoint, const double xin[], const double yin[],
                int forward, double xout[], double yout[] ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Mapping,Tran2))( this, npoint, xin, yin,
                                      forward, xout, yout );
}
void astTranN_( AstMapping *this, int npoint,
                int ncoord_in, int indim, const double (*in)[],
                int forward, int ncoord_out, int outdim, double (*out)[] ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Mapping,TranN))( this, npoint,
                                      ncoord_in, indim, in,
                                      forward, ncoord_out, outdim, out );
}
void astTranP_( AstMapping *this, int npoint,
                int ncoord_in, const double *ptr_in[],
                int forward, int ncoord_out, double *ptr_out[] ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Mapping,TranP))( this, npoint,
                                      ncoord_in, ptr_in,
                                      forward, ncoord_out, ptr_out );
}

/* Public Interface Function Prototypes. */
/* ------------------------------------- */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
void MapBoxId_( AstMapping *, const double [], const double [], int, int, double *, double *, double [], double [] );

/* Special interface function implementations. */
/* ------------------------------------------- */
void astMapBoxId_( AstMapping *this,
                   const double lbnd_in[], const double ubnd_in[],
                   int forward, int coord_out,
                   double *lbnd_out, double *ubnd_out,
                   double xl[], double xu[] ) {
/*
*++
*  Name:
c     astMapBox
f     AST_MAPBOX

*  Purpose:
*     Find a bounding box for a Mapping.

*  Type:
*     Protected virtual function.

*  Synopsis:
c     #include "mapping.h"
c     void astMapBox( AstMapping *this,
c                     const double lbnd_in[], const double ubnd_in[],
c                     int forward, int coord_out,
c                     double *lbnd_out, double *ubnd_out,
c                     double xl[], double xu[] );
f     CALL AST_MAPBOX( THIS, LBND_IN, UBND_IN, FORWARD, COORD_OUT,
f                      LBND_OUT, UBND_OUT, XL, XU, STATUS )

*  Class Membership:
*     Mapping method.

*  Description:
c     This function allows you to find the "bounding box" which just
c     encloses another box after it has been transformed by a Mapping
c     (using either its forward or inverse transformation). A typical
c     use might be to calculate the size of an image after being
c     transformed by a Mapping.
f     This routine allows you to find the "bounding box" which just
f     encloses another box after it has been transformed by a Mapping
f     (using either its forward or inverse transformation). A typical
f     use might be to calculate the size of an image after being
f     transformed by a Mapping.
*
c     The function works on one dimension at a time. When supplied
c     with the lower and upper bounds of a rectangular region (box) of
c     input coordinate space, it finds the lowest and highest values
c     taken by a nominated output coordinate within that
c     region. Optionally, it also returns the input coordinates where
c     these bounding values are attained. It should be used repeatedly
c     to obtain the extent of the bounding box in more than one
c     dimension.
f     The routine works on one dimension at a time. When supplied with
f     the lower and upper bounds of a rectangular region (box) of
f     input coordinate space, it finds the lowest and highest values
f     taken by a nominated output coordinate within that region. It
f     also returns the input coordinates where these bounding values
f     are attained. It should be used repeatedly to obtain the extent
f     of the bounding box in more than one dimension.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Mapping.
c     lbnd_in
f     LBND_IN( * ) = DOUBLE PRECISION (Given)
c        Pointer to an array of double, with one element for each
c        Mapping input coordinate. This should contain the lower bound
c        of the input box in each input dimension.
f        An array with one element for each Mapping input
f        coordinate. This should contain the lower bound of the input
f        box in each input dimension.
c     ubnd_in
f     UBND_IN( * ) = DOUBLE PRECISION (Given)
c        Pointer to an array of double, with one element for each
c        Mapping input coordinate. This should contain the upper bound
c        of the input box in each input dimension.
f        An array with one element for each Mapping input
f        coordinate. This should contain the upper bound of the input
f        box in each input dimension.
*
*        Note that it is permissible for the upper bound to be less
*        than the corresponding lower bound, as the values will simply
*        be swapped before use.
c     forward
f     FORWARD = LOGICAL (Given)
c        If this value is non-zero, then the Mapping's forward
c        transformation will be used to transform the input
c        box. Otherwise, its inverse transformation will be used.
f        If this value is .TRUE., then the Mapping's forward
f        transformation will be used to transform the input
f        box. Otherwise, its inverse transformation will be used.
*
c        (If the inverse transformation is selected, then references
c        to "input" and "output" coordinates in this description
c        should be transposed. For example, the size of the "lbnd_in"
c        and "ubnd_in" arrays should match the number of output
c        coordinates, as given by the Mapping's Nout
c        attribute. Similarly, the "coord_out" parameter, below,
c        should nominate one of the Mapping's input coordinates.)
f        (If the inverse transformation is selected, then references
f        to "input" and "output" coordinates in this description
f        should be transposed. For example, the size of the LBND_IN
f        and UBND_IN arrays should match the number of output
f        coordinates, as given by the Mapping's Nout attribute.
f        Similarly, the COORD_OUT argument, below, should nominate one
f        of the Mapping's input coordinates.)
c     coord_out
f     COORD_OUT = INTEGER (Given)
*        The index of the output coordinate for which the lower and
*        upper bounds are required. This value should be at least one,
*        and no larger than the number of Mapping output coordinates.
c     lbnd_out
f     LBND_OUT = DOUBLE PRECISION (Returned)
c        Pointer to a double in which to return the lowest value taken
c        by the nominated output coordinate within the specified
c        region of input coordinate space.
f        The lowest value taken by the nominated output coordinate
f        within the specified region of input coordinate space.
c     ubnd_out
f     UBND_OUT = DOUBLE PRECISION (Returned)
c        Pointer to a double in which to return the highest value
c        taken by the nominated output coordinate within the specified
c        region of input coordinate space.
f        The highest value taken by the nominated output coordinate
f        within the specified region of input coordinate space.
c     xl
f     XL( * ) = DOUBLE PRECISION (Returned)
c        An optional pointer to an array of double, with one element
c        for each Mapping input coordinate. If given, this array will
c        be filled with the coordinates of an input point (although
c        not necessarily a unique one) for which the nominated output
c        coordinate attains the lower bound value returned in
c        "*lbnd_out".
c
c        If these coordinates are not required, a NULL pointer may be
c        supplied.
f        An array with one element for each Mapping input
f        coordinate. This will return the coordinates of an input
f        point (although not necessarily a unique one) for which the
f        nominated output coordinate attains the lower bound value
f        returned in LBND_OUT.
c     xu
f     XU( * ) = DOUBLE PRECISION (Returned)
c        An optional pointer to an array of double, with one element
c        for each Mapping input coordinate. If given, this array will
c        be filled with the coordinates of an input point (although
c        not necessarily a unique one) for which the nominated output
c        coordinate attains the upper bound value returned in
c        "*ubnd_out".
c
c        If these coordinates are not required, a NULL pointer may be
c        supplied.
f        An array with one element for each Mapping input
f        coordinate. This will return the coordinates of an input
f        point (although not necessarily a unique one) for which the
f        nominated output coordinate attains the upper bound value
f        returned in UBND_OUT.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - Any input points which are transformed by the Mapping to give
*     output coordinates containing the value AST__BAD are regarded as
*     invalid and are ignored. They will make no contribution to
*     determining the output bounds, even although the nominated
*     output coordinate might still have a valid value at such points.
c     - An error will occur if the required output bounds cannot be
c     found. Typically, this might happen if all the input points
c     which the function considers turn out to be invalid (see
c     above). The number of points considered before generating such
c     an error is quite large, so this is unlikely to occur by
c     accident unless valid points are restricted to a very small
c     subset of the input coordinate space.
f     - An error will occur if the required output bounds cannot be
f     found. Typically, this might happen if all the input points
f     which the routine considers turn out to be invalid (see
f     above). The number of points considered before generating such
f     an error is quite large, so this is unlikely to occur by
f     accident unless valid points are restricted to a very small
f     subset of the input coordinate space.
c     - The values returned via "lbnd_out", "ubnd_out", "xl" and "xu"
c     will be set to the value AST__BAD if this function should fail
c     for any reason. Their initial values on entry will not be
c     altered if the function is invoked with the AST error status
c     set.
f     - The values returned via LBND_OUT, UBND_OUT, XL and XU will be
f     set to the value AST__BAD if this routine should fail for any
f     reason. Their initial values on entry will not be altered if the
f     routine is invoked with STATUS set to an error value.
*--

*  Implementation Notes:
*     This function implements the public interface for the astMapBox
*     method. It is identical to astMapBox_ except that the nominated
*     output coordinate given in "coord_out" is decremented by one
*     before use.  This is to allow the public interface to use
*     one-based coordinate numbering (internally, zero-based
*     coordinate numbering is used).
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Invoke the protected version of this function with the "coord_out"
   value decremented. */
   astMapBox_( this, lbnd_in, ubnd_in, forward, coord_out - 1,
               lbnd_out, ubnd_out, xl, xu );
}
