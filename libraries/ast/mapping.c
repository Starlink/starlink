#define BOUNDS 1
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
c     - astSimplify: Simplify a Mapping
c     - astTran1: Transform 1-dimensional coordinates
c     - astTran2: Transform 2-dimensional coordinates
c     - astTranN: Transform N-dimensional coordinates
c     - astTranP: Transform N-dimensional coordinates held in separate arrays
f     - AST_INVERT: Invert a Mapping
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
   const double *lbnd;           /* Pointer to lower constraints on input */
   const double *ubnd;           /* Pointer to upper constraints on input */
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
static double MapFunction( const MapData *, const double [], int * );
static double NewVertex( const MapData *, int, double, double [], double [], int *, double [] );
static double Random( long int * );
static int GetInvert( AstMapping * );
static int GetNin( AstMapping * );
static int GetNout( AstMapping * );
static int GetReport( AstMapping * );
static int GetTranDef( AstMapping *, int );
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
static void InitVtab( AstMappingVtab * );
static void Invert( AstMapping * );
static void MapList( AstMapping *, int, int, int *, AstMapping ***, int ** );
static void ReportPoints( AstMapping *, int, AstPointSet *, AstPointSet * );
static void SetAttrib( AstObject *, const char * );
static void SetInvert( AstMapping *, int );
static void SetReport( AstMapping *, int );
static void Tran1( AstMapping *, int, const double [], int, double [] );
static void Tran2( AstMapping *, int, const double [], const double [], int, double [], double [] );
static void TranN( AstMapping *, int, int, int, const double (*)[], int, int, int, double (*)[] );
static void TranP( AstMapping *, int, int, const double *[], int, int, double *[] );
static void ValidateMapping( AstMapping *, int, int, int, int, const char *);

/* Member functions. */
/* ================= */

#if BOUNDS

#if 1
static double UphillSimplex( const MapData *mapdata, double acc, int maxcall,
                               double dx[], double xmax[], int *ncall ) {
   double *f;
   double *xnew;
   double *x;
   double fnew;
   double fsave;
   double offset;
   double result;
   double tmp;
   int coord;
   int lo;
   int nextlo;
   int hi;
   int ncalla;
   int ncoord;
   int nvertex;
   int vertex;

/* Initialise. */
   result = AST__BAD;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain the number of input coordinates for the Mapping function and
   calculate the number of simplex vertices. */
   ncoord = mapdata->nin;
   nvertex = ncoord + 1;

/* Initialise the number of calls to the Mapping function. */
   *ncall = 0;
   ncalla = 0;

/* Allocate workspace. */
   f = astMalloc( sizeof( double ) * (size_t) nvertex );
   x = astMalloc( sizeof( double ) * (size_t) ( ncoord * nvertex ) );
   xnew = astMalloc( sizeof( double ) * (size_t) ncoord );
   if ( astOK ) {

/* Loop to set up an initial simplex. */
      for ( vertex = 0; vertex < nvertex; vertex++ ) {
         for ( coord = 0; coord < ncoord; coord++ ) {
            tmp = xmax[ coord ];

/* Displace each vertex (except the first) the required amount along
   one of the axes to generate the coordinates of the simplex
   vertices. */
            if ( coord == ( vertex - 1 ) ) tmp += dx[ coord ];
            x[ vertex * ncoord + coord ] = tmp;
         }

/* Evaluate the Mapping function at each vertex. */
         f[ vertex ] = MapFunction( mapdata, &x[ vertex * ncoord ], ncall );
      }

/* Update the number of times we attempted to call the Mapping
   function (not necessarily the same as the number of times it was
   actually called, which is stored in *ncall). */
      ncalla += ncoord;

/* Loop until convergence is reached or an error occurs. */
      while( astOK ) {
         lo = ( f[ 0 ] < f[ 1 ] ) ? 0 : 1;
         nextlo = 1 - lo;
         hi = 0;
         for ( vertex = 0; vertex < nvertex; vertex++ ) {
            if ( f[ vertex ] >= f[ hi ] ) hi = vertex;
            if ( f[ vertex ] < f[ lo ] ) {
               nextlo = lo;
               lo = vertex;
            } else if ( f[ vertex ] < f[ nextlo ] && vertex != lo ) {
               nextlo = vertex;
            }
         }

         if ( ( fabs( f[ lo ] - f[ hi ] ) <= acc ) ||
             ( *ncall >= maxcall ) ||
             ( ncalla >= 3 * maxcall ) ) {
            for ( coord = 0; coord < ncoord; coord++ ) {
               xmax[ coord ] = x[ hi * ncoord + coord ];
            }
            result = f[ hi ];
            break;
         }

         fnew = NewVertex( mapdata, lo, -1.0, x, f, ncall, xnew );
         ncalla++;

/* Contract lowest point towards highest. */
         if ( fnew == AST__BAD ) {
            for ( coord = 0; coord < ncoord; coord++ ) {
               offset = x[ lo * ncoord + coord ] - x[ hi * ncoord + coord ];
               if ( fabs( offset ) <=
                   fabs( x[ hi * ncoord + coord ] ) * DBL_EPSILON ) offset = 0.0;
               x[ lo * ncoord + coord ] = x[ hi * ncoord + coord ] + 0.5 * offset;
            }
            f[ lo ] = MapFunction( mapdata, &x[ lo * ncoord ], ncall );
            ncalla++;
         } else if ( fnew >= f[ hi ] ) {
            fnew = NewVertex( mapdata, lo, 2.0, x, f, ncall, xnew );
            ncalla++;
         } else if ( fnew <= f[ nextlo ] ) {
            fsave = f[ lo ];
            fnew = NewVertex( mapdata, lo, 0.5, x, f, ncall, xnew );
            ncalla++;
            if ( fnew <= fsave ) {
               for ( vertex = 0; vertex < nvertex; vertex++ ) {
                  if ( vertex != hi ) {
                     for ( coord = 0; coord < ncoord; coord++ ) {
                        offset = x[ vertex * ncoord + coord ] -
                        x[ hi * ncoord + coord ];
                        if ( fabs( offset ) <=
                            fabs( x[ hi * ncoord + coord ] ) * DBL_EPSILON ) offset = 0.0;
                        x[ vertex * ncoord + coord ] =
                        x[ hi * ncoord + coord ] + 0.5 * offset;
                     }
                     f[ vertex ] = MapFunction( mapdata, &x[ vertex * ncoord ], ncall );
                  }
               }
               ncalla += ncoord;
            }
         }
      }
   }

/* Free workspace. */
   x = astFree( x );
   f = astFree( f );
   xnew = astFree( xnew );

   return result;
}
#endif

static double SimplexMaximum( const MapData *mapdata, double x[], double fract,
                              double acc, int *ncall ) {
   double result;
   int nin;
   double *dx;
   const double *lbnd;
   const double *ubnd;
   double middle;
   int coord;
   double maximum;
   int iter;
   const int maxiter = 10;
   int done;

   result = AST__BAD;

   if ( !astOK ) return result;

   nin = mapdata->nin;
   dx = astMalloc( sizeof( double ) * (size_t) nin );

   lbnd = mapdata->lbnd;
   ubnd = mapdata->ubnd;
   for ( iter = 0; iter < maxiter; iter++ ) {
      for ( coord = 0; coord < nin; coord++ ) {
         middle = 0.5 * ( ubnd[ coord ] + lbnd[ coord ] );
         dx[ coord ] = fract * ( ubnd[ coord ] - lbnd[ coord ] );
         if ( x[ coord ] >= middle ) dx[ coord ] = -dx[ coord ];
      }
      maximum = UphillSimplex( mapdata, 0.0, 5000, dx, x, ncall );
printf( "maximum %d = %.20g, fract = %g, ncall = %d\n", iter, maximum, fract, *ncall );
      if ( result == AST__BAD ) {
         result = maximum;
      } else if ( maximum >= result ) {
printf( "imnprove, acc = %g %g\n", maximum - result, acc );
         done = ( ( maximum - result ) <= acc );
         result = maximum;
         if ( done ) break;
      }
      fract /= 100.0;
   }
   
   dx = astFree( dx );
   return result;
}


#if 1
static double PdfWeight( int ndim, const double x[],
                         const double lbnd[], const double ubnd[],
                         int npoint, const double x1[], const double x2[] ) {
   double wt;
   int point;
   int dim;
   double wt1;
   double wt2;
   double dxu;
   double dxl;
   double dx;
   double delta;
  
   wt = 1.0;
   for ( point = 0; point < npoint; point++ ) {
      wt1 = wt2 = 0.0;
      for ( dim = 0; dim < ndim; dim++ ) {
         dxl = fabs( x1[ point * ndim + dim ] - lbnd[ dim ] );
         dxu = fabs( ubnd[ dim ] - x1[ point * ndim + dim ] );
         dx = ( dxu > dxl ) ? dxu : dxl;
         delta = fabs( x[ dim ] - x1[ point * ndim + dim ] ) / dx;
         wt1 = ( wt1 > delta ) ? wt1 : delta;

         dxl = fabs( x2[ point * ndim + dim ] - lbnd[ dim ] );
         dxu = fabs( ubnd[ dim ] - x2[ point * ndim + dim ] );
         dx = ( dxu > dxl ) ? dxu : dxl;
         delta = fabs( x[ dim ] - x2[ point * ndim + dim ] ) / dx;
         wt2 = ( wt2 > delta ) ? wt2 : delta;
      }
      wt *= wt1 * wt2;
   }
   return wt;
}

void astBoxBound( AstMapping *this,
                  const double lbnd_in[], const double ubnd_in[],
                  int coord_out, double lbnd_out[], double ubnd_out[] ) {

   MapData mapdata;
   int nin;
   int nout;
   long int irand = 77665544;
   double random;
   int ncall;
   double *x_in;
   double *x_out;
   int iter;
   int coord;
   int limit;
   double maximum;
   double new_maximum;
   int reject;
   double wt;
   int nreject;
   int i;
   double wtmax;
   double wtsum;
   const int nsamp = 50;
   const int maxiter = 50;
   const int miniter = 5;
   int nsame;
   const double acc = DBL_EPSILON;
   int iopt;
   
   nin = astGetNin( this );
   nout = astGetNout( this );

   mapdata.mapping = this;
   mapdata.nin = nin;
   mapdata.nout = nout;
   mapdata.coord = coord_out;
   mapdata.forward = 1;
   mapdata.lbnd = lbnd_in;
   mapdata.ubnd = ubnd_in;
   mapdata.pset_in = astPointSet( 1, nin, "" );
   mapdata.pset_out = astPointSet( 1, nout, "" );
   mapdata.ptr_in = astGetPoints( mapdata.pset_in );
   mapdata.ptr_out = astGetPoints( mapdata.pset_out );
 
   for ( limit = 0; limit < 2; limit++ ) {
      mapdata.negate = !limit;
      maximum = AST__BAD;

      x_in = x_out = NULL;
      nsame = 0;
      iopt = 0;
      for ( iter = 0; iter < maxiter; iter++ ) {
         x_in = astGrow( x_in, nin * ( iter + 1 ), sizeof( double ) );
         x_out = astGrow( x_out, nin * ( iter + 1 ), sizeof( double ) );

         wtsum = wtmax = 0.0;
         for ( i = 0; i < nsamp; i++ ) {
            for ( coord = 0; coord < nin; coord++ ) {
               random = Random( &irand );
                 x_in[ iter * nin + coord ] = ubnd_in[ coord ] * random +
                                            lbnd_in[ coord ] * ( 1.0 - random );
            }
            wt = PdfWeight( nin, &x_in[ iter * nin ], lbnd_in, ubnd_in,
                            iter, x_in, x_out );
            wtsum += wt;
            wtmax = ( wt > wtmax ) ? wt : wtmax;
         }
         if ( wtsum < wtmax ) wtmax = wtsum;
         reject = 1;
         nreject = 0;
         while ( reject ) {
            for ( coord = 0; coord < nin; coord++ ) {
               random = Random( &irand );
               x_out[ iter * nin + coord ] =
                 x_in[ iter * nin + coord ] = ubnd_in[ coord ] * random +
                                            lbnd_in[ coord ] * ( 1.0 - random );
            }
            wt = 1.0;
            if ( wtmax > 0.0 ) {
              wt = PdfWeight( nin, &x_in[ iter * nin ], lbnd_in, ubnd_in,
                              iter, x_in, x_out ) / wtmax;
            }
            reject = ( Random( &irand ) > wt );
            if ( reject ) nreject++;
         }
         printf( "nreject = %d\n", nreject );

printf( "X minimising (limit=%d, iter=%d) starting at:\n", limit, iter );
         for ( coord = 0; coord < nin; coord ++ ) {
printf( "%g ", x_in[ iter * nin + coord ] );
         }
printf( "\n" );
         new_maximum = SimplexMaximum( &mapdata, &x_out[ iter * nin ],
                                       0.1, acc, &ncall );
         printf( "maximum found at " );
         for ( coord = 0; coord < nin; coord++ ) {
            printf( "%.20g ", x_out[ iter * nin + coord ] );
         }
         printf( "\n\n" );
         if ( new_maximum > maximum ) {
            if ( ( new_maximum - maximum ) > acc ) {
               nsame = 0;
            } else {
               nsame++;
            }
            maximum = new_maximum;
            iopt = iter;
         } else {
            nsame++;
         }
printf( "nsame, iopt = %d %d\n", nsame, iopt );
         if ( ( ( iter + 1 ) >= miniter ) &&
              ( nsame >= ( 0.3 * ( iter + 1 ) ) ) ) break;
      }
      if ( !limit ) {
         *lbnd_out = - maximum;
      } else {
         *ubnd_out = maximum;
      }
      printf( "best limit = %g at ", maximum );
      for ( coord = 0; coord < nin; coord++ ) {
         printf( "%g ", x_out[ iopt * nin + coord ] );
      }
      printf( "\n\n" );
      x_in = astFree( x_in );
      x_out = astFree( x_out );
   }
}

#else
void astBoxBound( AstMapping *this,
                  const double lbnd_in[], const double ubnd_in[],
                  double lbnd_out[], double ubnd_out[] ) {

#define NFIT 10
#define NMULT 10
   const double decrement = 2.0;
   const int maxiter = 1;
   
   double fitness[ NFIT ];
   double v[ NFIT * NMULT ];
   double **fit_ptr;
   double noise;
   int nin;
   int nout;
   int point;
   int coord;
   AstPointSet *fitset;
   AstPointSet *pset1_in;
   AstPointSet *pset1_out;
   double **ptr1_in;
   double **ptr1_out;
   AstPointSet *pset2_in;
   AstPointSet *pset2_out;
   double **ptr2_in;
   double **ptr2_out;
   AstPointSet *pset3_in;
   AstPointSet *pset3_out;
   double **ptr3_in;
   double **ptr3_out;
   int mult;
   double x;
   int bad;
   double fit1;
   double fit2;
   double fit;
   int icoord;
   int p;
   int pp;
   int iter;
   int limit;
   float seed = 0.776543289;
   long int irand = 77665544;
   double random;
   MapData mapdata;
   

   nin = astGetNin( this );
   nout = astGetNout( this );
   for ( coord = 0; coord < nout; coord++ ) {
      lbnd_out[ coord ] = AST__BAD;
      ubnd_out[ coord ] = AST__BAD;
   }


   pset1_in = astPointSet( 1, nin, "" );
   ptr1_in = astGetPoints( pset1_in );
   if ( astOK ) {
      for ( coord = 0; coord < nin; coord ++ ) {
         ptr1_in[ coord ][ 0 ] = 0.5 * ( lbnd_in[ coord ] + ubnd_in[ coord ] );
      }
   }
   pset1_out = astTransform( this, pset1_in, 1, NULL );
   ptr1_out = astGetPoints( pset1_out );
   bad = 0;
   if ( astOK ) {
      for ( coord = 0; coord < nout; coord++ ) {
         if ( bad = ( ptr1_out[ coord ][ 0 ] == AST__BAD ) ) break;
      }
      if ( !bad ) {
         for ( coord = 0; coord < nout; coord++ ) {
            lbnd_out[ coord ] = ubnd_out[ coord ] = ptr1_out[ coord ][ 0 ];
         }
      }
   }

   pset2_in = astPointSet( NFIT, nin, "" );
   ptr2_in = astGetPoints( pset2_in );
   pset2_out = astPointSet( NFIT, nout, "" );
   ptr2_out = astGetPoints( pset2_out );

   pset3_in = astPointSet( NFIT * NMULT, nin, "" );
   ptr3_in = astGetPoints( pset3_in );
   pset3_out = astPointSet( NFIT * NMULT, nout, "" );
   ptr3_out = astGetPoints( pset3_out );
   
   mapdata.mapping = this;
   mapdata.nin = nin;
   mapdata.nout = nout;
   mapdata.forward = 1;
   mapdata.lbnd = lbnd_in;
   mapdata.ubnd = ubnd_in;
   mapdata.pset_in = astPointSet( 1, nin, "" );
   mapdata.pset_out = astPointSet( 1, nout, "" );
   mapdata.ptr_in = astGetPoints( mapdata.pset_in );
   mapdata.ptr_out = astGetPoints( mapdata.pset_out );
   
   for ( limit = 0; limit < 2 * nout; limit++ ) {
      icoord = limit / 2;
      mapdata.negate = !( limit % 2 );
      mapdata.coord = icoord;

      if ( astOK ) {
         for ( coord = 0; coord < nin; coord++ ) {
            for ( point = 0; point < NFIT; point++ ) {
               ptr2_in[ coord ][ point ] = ptr1_in[ coord ][ 0 ];
            }
         }
         for ( coord = 0; coord < nout; coord++ ) {
            for ( point = 0; point < NFIT; point++ ) {
               ptr2_out[ coord ][ point ] = ptr1_out[ coord ][ 0 ];
               if ( limit % 2 ) ptr2_out[ coord ][ point ] *= -1.0;
            }
         }
      }
      
      noise = 1.0;
      for ( iter = 0; iter < maxiter; iter++ ) {
         for ( coord = 0; coord < nin; coord++ ) {
            for ( point = 0; point < NFIT; point++ ) {
               for ( mult = 0; mult < NMULT; mult++ ) {
                  do {
#if 0
                     random = ( 1.0 - noise ) * Gauss( &irand ) * noise +
                              noise * 2.0 * ( Random( &irand ) - 0.5 ) * noise;
#elif 0
                     random = ( 1.0 - noise ) * slaGresid( noise ) +
                              noise * 2.0 * ( slaRandom( seed ) - 0.5 ) * noise;
#else
                     random = Gauss( &irand ) * noise;
#endif                     
                     x = ptr2_in[ coord ][ point ] + 0.5 * random *
                         ( ubnd_in[ coord ] - lbnd_in[ coord ] );
                  } while ( x < lbnd_in[ coord ] || x > ubnd_in[ coord ] );
                  ptr3_in[ coord ][ point * NMULT + mult ] = x;
               }
            }
         }
         astTransform( this, pset3_in, 1, pset3_out );
         
         for ( point = 0; point < NMULT * NFIT; point++ ) {
            x = ptr3_out[ icoord ][ point ];
            if ( limit % 2 ) x = -x;
            if ( x >= ptr2_out[ icoord ][ 0 ] ) {
               for ( p = 1; p < NFIT + 1; p++ ) {
                  if ( p == NFIT || x < ptr2_out[ icoord ][ p ] ||
                      ( x == ptr2_out[ icoord ][ p ] &&
                       slaGresid( 1.0 ) >= 0.0 ) ) {
                     for ( coord = 0; coord < nin; coord++ ) {
                        for ( pp = 1; pp < p; pp++ ) {
                           ptr2_in[ coord ][ pp - 1 ] = ptr2_in[ coord ][ pp ];
                        }
                        ptr2_in[ coord ][ p - 1 ] = ptr3_in[ coord ][ point ];
                     }
                     for ( pp = 1; pp < p; pp++ ) {
                        ptr2_out[ icoord ][ pp - 1 ] = ptr2_out[ icoord ][ pp ];
                     }
                     ptr2_out[ icoord ][ p - 1 ] = ptr3_out[ icoord ][ point ];
                     if ( limit % 2 ) ptr2_out[ icoord ][ p - 1 ] *= -1.0;
                     break;
                  }
               }
            }
         }
         noise /= decrement;
      }

#if 1
         for ( point = 0; point < NFIT; point++ ) {
            for ( coord = 0; coord < nin; coord++ ) {
               printf( "%g ", ptr2_in[ coord ][ point ] );
            }
            printf( "--> " );
            for ( coord = 0; coord < nout; coord++ ) {
               printf( "%g ", ptr2_out[ coord ][ point ] );
            }
            printf( "\n" );
         }
         printf( "\n" );
#endif
      {
         double p[ 100 ];
         int ncall;
         double answer;
         for ( coord = 0; coord < nin; coord++ ) {
            p[ coord ] = ptr2_in[ coord ][ NFIT - 1 ];
         }
         answer = SimplexMaximum( &mapdata, p, noise, DBL_EPSILON, &ncall );
         printf( "ymin = %g\n", answer );
         printf( "ncall = %d\n", ncall );

         if ( limit % 2 ) {
            lbnd_out[ icoord ] = answer;
         } else {
            ubnd_out[ icoord ] = -answer;
         }
      }
   }
   mapdata.pset_in = astAnnul( mapdata.pset_in );
   mapdata.pset_out = astAnnul( mapdata.pset_out );

   pset1_in = astAnnul( pset1_in );
   pset1_out = astAnnul( pset1_out );
   pset2_in = astAnnul( pset2_in );
   pset2_out = astAnnul( pset2_out );
   pset3_in = astAnnul( pset3_in );
   pset3_out = astAnnul( pset3_out );
}
#endif
#endif

/*************************************/
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

static int GetTranDef( AstMapping *this, int forward ) {
/*
*+
*  Name:
*     astGetTranDef

*  Purpose:
*     Determine if a Mapping defines a coordinate transformation.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "mapping.h"
*     int astGetTranDef( AstMapping *this, int forward )

*  Class Membership:
*     Mapping method.

*  Description:
*     This function returns a value indicating whether a Mapping is
*     able to perform a coordinate transformation in a specified
*     direction (i.e.  whether the forward or the inverse
*     transformation is defined).

*  Parameters:
*     this
*        Pointer to the Mapping.
*     forward
*        A non-zero value indicates that information is required about
*        the forward coordinate transformation, while a zero value
*        requests information about the inverse transformation.

*  Returned Value:
*     Zero if the coordinate transformation is not defined, or 1 if it is.

*  Notes:
*     - Use of this function is deprecated - use astGetTranForward
*     and/or astGetTranInverse instead.
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   int result;                      /* Transformation defined? */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Obtain the required information. */
   result = forward ? astGetTranForward( this ) : astGetTranInverse( this );

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
   vtab->GetTranDef = GetTranDef;
   vtab->GetTranForward = GetTranForward;
   vtab->GetTranInverse = GetTranInverse;
   vtab->Invert = Invert;
   vtab->MapList = MapList;
   vtab->MapMerge = MapMerge;
   vtab->ReportPoints = ReportPoints;
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
*        The index of the simplex vertex which initially has the worst
*        (lowest) value.
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
int astGetTranDef_( AstMapping *this, int forward ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Mapping,GetTranDef))( this, forward );
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
