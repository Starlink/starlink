/*
*+
*  Name:
*     intersect

*  Type of Module:
*     C extension to Tcl.

*  Language:
*     ANSI C.

*  Purpose:
*     Calculates the vertices of the overlap of two convex polygons.

*  Usage:
*     intersect points1 points2

*  Description:
*     This command calculates the vertices of the polygon which forms
*     the intersection between two given convex polygons.

*  Arguments:
*     points1 = list of pairs
*        Vertices of the first polygon.  May or may not be closed.
*        Vertices must be in clockwise order.
*     points2 = list of pairs
*        Vertices of the second polygon.  May or may not be closed.
*        Vertices must be in clockwise order.

*  Return Value:
*     A list of points giving the vertices of the intersect polygon.
*     It will be closed.

*  Authors:
*     OROURKE: J. O'Rourke
*     MBT: Mark Taylor (Starlink)

*  History:
*     1-JAN-1994 (OROURKE):
*        Original version published in J. O'Rourke, "Computational 
*        Geometry in C", Cambridge University Press (1994).
*     25-OCT-2000 (MBT):
*        Adapted for use with Tcl.

*-
*/

#include        <stdio.h>
#include        <math.h>
#define X       0
#define Y       1
#define DIM     2               /* Dimension of points */
#define PMAX    100             /* Max # of pts in polygon */
typedef double  tPointi[DIM];   /* type integer point */
typedef double  tPointd[DIM];   /* type double point */
typedef tPointi tPolygoni[PMAX];/* type integer polygon */
typedef enum { FALSE, TRUE } bool;
bool    Left( tPointi a, tPointi b, tPointi c );
void    ConvexIntersect( tPolygoni P, tPolygoni Q, int n, int m, tPointd *Z, int *pnz );

void preverse( int n, tPolygoni poly );

#include "tcl.h"


   int IntersectCmd( ClientData clientData, Tcl_Interp *interp, int objc,
                     Tcl_Obj *CONST objv[] ) {

/* Local Variables. */
      tPolygoni poly[ 2 ];
      tPointi intpoly[ PMAX * 2 ];
      int i;
      int intnvert;
      int j;
      int nvertex[ 2 ];
      Tcl_Obj *result;

/* Check syntax. */
      if ( objc != 3 ) {
         Tcl_WrongNumArgs( interp, 1, objv, "points1 points2" );
         return TCL_ERROR;
      }

/* Get arguments. */
      for ( i = 0; i < 2; i++ ) {
         Tcl_Obj **ov;
         if ( Tcl_ListObjGetElements( interp, objv[ 1 + i ], &nvertex[ i ], 
                                      &ov ) != TCL_OK ) {
            return TCL_ERROR;
         }
         if ( nvertex[ i ] < 3 || nvertex[ i ] > PMAX ) {
            Tcl_SetObjResult( interp,
               Tcl_NewStringObj( "Invalid number of points in polygon", -1 ) );
            return TCL_ERROR;
         }
         for ( j = 0; j < nvertex[ i ]; j++ ) {
            Tcl_Obj **pov;
            int poc;
            if ( Tcl_ListObjGetElements( interp, ov[ j ], &poc, &pov ) 
                 != TCL_OK ) {
               return TCL_ERROR;
            }
            if ( poc != 2 ) {
               Tcl_SetObjResult( interp,
                  Tcl_NewStringObj( "Each point must be a double", -1 ) );
               return TCL_ERROR;
            }
            if ( Tcl_GetDoubleFromObj( interp, pov[ 0 ], &poly[ i ][ j ][ X ] ) 
                 != TCL_OK ||
                 Tcl_GetDoubleFromObj( interp, pov[ 1 ], &poly[ i ][ j ][ Y ] )
                 != TCL_OK ) {
               return TCL_ERROR;
            }
         }
      }

/* Check that both polygons are defined in a clockwise direction, which is
   required for the intersection algorithm.  If not, reverse the order of
   the vertices.  We make the assumption that they are in fact convex. */
      for ( i = 0; i < 2; i++ ) {
         if ( ! Left( poly[ i ][ 0 ], poly[ i ][ 1 ], poly[ i ][ 2 ] ) ) {
            printf( "reverse %d\n", i );
            preverse( nvertex[ i ], poly[ i ] );
         }
      }

/* Do the calculation.  Check for an intersection; if we don't get one 
   then try reversing the order of the vertices of one or both polygons.
   This may be necessary since the algorithm only works if both polygons
   are specified in clockwise order.  There is probably a more efficient
   way of doing this. */
      ConvexIntersect( poly[ 0 ], poly[ 1 ], nvertex[ 0 ], nvertex[ 1 ],
                       intpoly, &intnvert );

/* Set the result. */
      result = Tcl_NewListObj( 0, NULL );
      for ( i = 0; i < intnvert; i++ ) {
         Tcl_Obj *pov[ 2 ];
         if ( i > 0 && intpoly[ i ][ X ] == intpoly[ i - 1 ][ X ] 
                    && intpoly[ i ][ Y ] == intpoly[ i - 1 ][ Y ] ) {
            /* no point in writing two identical adjacent points. */
         } 
         else {
            pov[ 0 ] = Tcl_NewDoubleObj( intpoly[ i ][ X ] );
            pov[ 1 ] = Tcl_NewDoubleObj( intpoly[ i ][ Y ] );
            Tcl_ListObjAppendElement( interp, result, 
                                      Tcl_NewListObj( 2, pov ) );
         }
      }
      Tcl_SetObjResult( interp, result );

/* Return with success status. */
      return TCL_OK;
   }


   void preverse( int n, tPolygoni poly ) {
      int i;
      double x, y;
      for ( i = 0; i < ( n + 1 ) / 2; i++ ) {
         x = poly[ i ][ X ];
         y = poly[ i ][ Y ];
         poly[ i ][ X ] = poly[ n - 1 - i ][ X ];
         poly[ i ][ Y ] = poly[ n - 1 - i ][ Y ];
         poly[ n - 1 - i ][ X ] = x;
         poly[ n - 1 - i ][ Y ] = y;
      }
   }


/*************************************************************************/
/* The following code is lifted mostly from O'Rourke's book.             */
/* Modifications were made to allow it to return the vertices of an      */
/* overlap polygon to a calling routine, and to allow double precision   */
/* coordinates for the initial polygons rather than just integer ones.   */
/*************************************************************************/

/*
This code is described in "Computational Geometry in C" (Second Edition),
Chapter 7.  It is not written to be comprehensible without the
explanation in that book.

Written by Joseph O'Rourke.
Last modified: December 1997
Questions to orourke@cs.smith.edu.
--------------------------------------------------------------------
This code is Copyright 1997 by Joseph O'Rourke.  It may be freely
redistributed in its entirety provided that this copyright notice is
not removed.
--------------------------------------------------------------------
*/
typedef enum { Pin, Qin, Unknown } tInFlag;



/*---------------------------------------------------------------------
Function prototypes.
---------------------------------------------------------------------*/
double  Dot( tPointi a, tPointi b );
int	AreaSign( tPointi a, tPointi b, tPointi c );
char    SegSegInt( tPointi a, tPointi b, tPointi c, tPointi d, tPointd p, tPointd q );
char    ParallelInt( tPointi a, tPointi b, tPointi c, tPointi d, tPointd p, tPointd q );
bool    Between( tPointi a, tPointi b, tPointi c );
void    Assigndi( tPointd p, tPointi a );
void    SubVec( tPointi a, tPointi b, tPointi c );
bool    LeftOn( tPointi a, tPointi b, tPointi c );
bool    Left( tPointi a, tPointi b, tPointi c );
void    PrintPoly( int n, tPolygoni P );
void    ConvexIntersect( tPolygoni P, tPolygoni Q, int n, int m, tPointd *Z, int *pnz );
tInFlag InOut( tPointd p, tInFlag inflag, int aHB, int bHA );
int     Advance( int a, int *aa, int n, bool inside, tPointi v );
void	OutputPolygons( void );

void firstpoint( tPointd p, tPointd dest );
void addpoint( tPointd p, tPointd dest );

/*---------------------------------------------------------------------
---------------------------------------------------------------------*/
void ConvexIntersect( tPolygoni P, tPolygoni Q, int n, int m,
                      tPointd *Z, int *pnz )
                           /* P has n vertices, Q has m vertices. */
{
   int     a, b;           /* indices on P and Q (resp.) */
   int     a1, b1;         /* a-1, b-1 (resp.) */
   tPointi A, B;           /* directed edges on P and Q (resp.) */
   int     cross;          /* sign of z-component of A x B */
   int     bHA, aHB;       /* b in H(A); a in H(b). */
   tPointi Origin = {0,0}; /* (0,0) */
   tPointd p;              /* double point of intersection */
   tPointd q;              /* second point of intersection */
   tInFlag inflag;         /* {Pin, Qin, Unknown}: which inside */
   int     aa, ba;         /* # advances on a & b indices (after 1st inter.) */
   bool    FirstPoint;     /* Is this the first point? (used to initialize).*/
   tPointd p0;             /* The first point. */
   int     code;           /* SegSegInt return code. */ 

   /* Initialize variables. */
   a = 0; b = 0; aa = 0; ba = 0;
   inflag = Unknown; FirstPoint = TRUE;
   *pnz = 0;

   do {
      /* Computations of key variables. */
      a1 = (a + n - 1) % n;
      b1 = (b + m - 1) % m;

      SubVec( P[a], P[a1], A );
      SubVec( Q[b], Q[b1], B );

      cross = AreaSign( Origin, A, B );
      aHB   = AreaSign( Q[b1], Q[b], P[a] );
      bHA   = AreaSign( P[a1], P[a], Q[b] );

      /* If A & B intersect, update inflag. */
      code = SegSegInt( P[a1], P[a], Q[b1], Q[b], p, q );
      if ( code == '1' || code == 'v' ) {
         if ( inflag == Unknown && FirstPoint ) {
            aa = ba = 0;
            FirstPoint = FALSE;
            p0[X] = p[X]; p0[Y] = p[Y];
            firstpoint( p0, Z[(*pnz)++] );
         }
         addpoint( p, Z[(*pnz)++] );
         inflag = InOut( p, inflag, aHB, bHA );
      }

      /*-----Advance rules-----*/

      /* Special case: A & B overlap and oppositely oriented. */
      if ( ( code == 'e' ) && (Dot( A, B ) < 0) ) {
         firstpoint( p, Z[(*pnz)++] );
         addpoint( q, Z[(*pnz)++] );
         return;
      }

      /* Special case: A & B parallel and separated. */
      if ( (cross == 0) && ( aHB < 0) && ( bHA < 0 ) ) {
            /* printf("%%P and Q are disjoint.\n"); */
            return;
      }

      /* Special case: A & B collinear. */
      else if ( (cross == 0) && ( aHB == 0) && ( bHA == 0 ) ) {
            /* Advance but do not output point. */
            if ( inflag == Pin ) {
               if ( inflag == Qin ) addpoint( Q[b], Z[(*pnz)++] );
               b = Advance( b, &ba, m, inflag == Qin, Q[b] );
            }
            else {
               if ( inflag == Pin ) addpoint( P[a], Z[(*pnz)++] );
               a = Advance( a, &aa, n, inflag == Pin, P[a] );
            }
         }

      /* Generic cases. */
      else if ( cross >= 0 ) {
         if ( bHA > 0) {
            if ( inflag == Pin ) addpoint( P[a], Z[(*pnz)++] );
            a = Advance( a, &aa, n, inflag == Pin, P[a] );
         }
         else {
            if ( inflag == Qin ) addpoint( Q[b], Z[(*pnz)++] );
            b = Advance( b, &ba, m, inflag == Qin, Q[b] );
         }
      }
      else /* if ( cross < 0 ) */{
         if ( aHB > 0) {
            if ( inflag == Qin ) addpoint( Q[b], Z[(*pnz)++] );
            b = Advance( b, &ba, m, inflag == Qin, Q[b] );
         }
         else {
            if ( inflag == Pin ) addpoint( P[a], Z[(*pnz)++] );
            a = Advance( a, &aa, n, inflag == Pin, P[a] );
         }
      }

   /* Quit when both adv. indices have cycled, or one has cycled twice. */
   } while ( ((aa < n) || (ba < m)) && (aa < 2*n) && (ba < 2*m) );

   if ( !FirstPoint ) /* If at least one point output, close up. */
            addpoint( p0, Z[(*pnz)++] );

   /* Deal with special cases: not implemented. */
   if ( inflag == Unknown) {
      /* printf("%%The boundaries of P and Q do not cross.\n"); */
   }
}

void firstpoint( tPointi p, tPointi dest ) {
   dest[X] = p[X];
   dest[Y] = p[Y];
}

void addpoint( tPointd p, tPointd dest ) {
   dest[X] = p[X];
   dest[Y] = p[Y];
}

/*---------------------------------------------------------------------
Prints out the double point of intersection, and toggles in/out flag.
---------------------------------------------------------------------*/
tInFlag InOut( tPointd p, tInFlag inflag, int aHB, int bHA )
{
   /* Update inflag. */
   if      ( aHB > 0)
      return Pin;
   else if ( bHA > 0)
      return Qin;
   else    /* Keep status quo. */
      return inflag;
}
/*---------------------------------------------------------------------
   Advances and prints out an inside vertex if appropriate.
---------------------------------------------------------------------*/
int     Advance( int a, int *aa, int n, bool inside, tPointi v )
{
   (*aa)++;
   return  (a+1) % n;
}

/*
   Returns true iff c is strictly to the left of the directed
   line through a to b.
*/
bool    Left( tPointi a, tPointi b, tPointi c )
{
        return  AreaSign( a, b, c ) > 0;
}

bool    LeftOn( tPointi a, tPointi b, tPointi c )
{
        return  AreaSign( a, b, c ) >= 0;
}

bool    Collinear( tPointi a, tPointi b, tPointi c )
{
        return  AreaSign( a, b, c ) == 0;
}
/*---------------------------------------------------------------------
a - b ==> c.
---------------------------------------------------------------------*/
void    SubVec( tPointi a, tPointi b, tPointi c )
{
   int i;

   for( i = 0; i < DIM; i++ )
      c[i] = a[i] - b[i];
}


int	AreaSign( tPointi a, tPointi b, tPointi c )
{
    double area2;

    area2 = ( b[0] - a[0] ) * (double)( c[1] - a[1] ) -
            ( c[0] - a[0] ) * (double)( b[1] - a[1] );

    /* These values of 0.001 are arbitrary - they should be small though. */
    if      ( area2 >  0.001 ) return  1;
    else if ( area2 < -0.001 ) return -1;
    else                      return  0;
}

/*---------------------------------------------------------------------
SegSegInt: Finds the point of intersection p between two closed
segments ab and cd.  Returns p and a char with the following meaning:
   'e': The segments collinearly overlap, sharing a point.
   'v': An endpoint (vertex) of one segment is on the other segment,
        but 'e' doesn't hold.
   '1': The segments intersect properly (i.e., they share a point and
        neither 'v' nor 'e' holds).
   '0': The segments do not intersect (i.e., they share no points).
Note that two collinear segments that share just one point, an endpoint
of each, returns 'e' rather than 'v' as one might expect.
---------------------------------------------------------------------*/
char	SegSegInt( tPointi a, tPointi b, tPointi c, tPointi d, tPointd p, tPointd q )
{
   double  s, t;       /* The two parameters of the parametric eqns. */
   double num, denom;  /* Numerator and denoninator of equations. */
   char code = '?';    /* Return char characterizing intersection. */

   denom = a[X] * (double)( d[Y] - c[Y] ) +
           b[X] * (double)( c[Y] - d[Y] ) +
           d[X] * (double)( b[Y] - a[Y] ) +
           c[X] * (double)( a[Y] - b[Y] );

   /* If denom is zero, then segments are parallel: handle separately. */
   if (denom == 0.0)
      return  ParallelInt(a, b, c, d, p, q);

   num =    a[X] * (double)( d[Y] - c[Y] ) +
            c[X] * (double)( a[Y] - d[Y] ) +
            d[X] * (double)( c[Y] - a[Y] );
   if ( (num == 0.0) || (num == denom) ) code = 'v';
   s = num / denom;

   num = -( a[X] * (double)( c[Y] - b[Y] ) +
            b[X] * (double)( a[Y] - c[Y] ) +
            c[X] * (double)( b[Y] - a[Y] ) );
   if ( (num == 0.0) || (num == denom) ) code = 'v';
   t = num / denom;

   if      ( (0.0 < s) && (s < 1.0) &&
             (0.0 < t) && (t < 1.0) )
     code = '1';
   else if ( (0.0 > s) || (s > 1.0) ||
             (0.0 > t) || (t > 1.0) )
     code = '0';

   p[X] = a[X] + s * ( b[X] - a[X] );
   p[Y] = a[Y] + s * ( b[Y] - a[Y] );

   return code;
}
char   ParallelInt( tPointi a, tPointi b, tPointi c, tPointi d, tPointd p, tPointd q )
{

   if ( !Collinear( a, b, c) )
      return '0';

   if ( Between( a, b, c ) && Between( a, b, d ) ) {
      Assigndi( p, c );
      Assigndi( q, d );
      return 'e';
   }
   if ( Between( c, d, a ) && Between( c, d, b ) ) {
      Assigndi( p, a );
      Assigndi( q, b );
      return 'e';
   }
   if ( Between( a, b, c ) && Between( c, d, b ) ) {
      Assigndi( p, c );
      Assigndi( q, b );
      return 'e';
   }
   if ( Between( a, b, c ) && Between( c, d, a ) ) {
      Assigndi( p, c );
      Assigndi( q, a );
      return 'e';
   }
   if ( Between( a, b, d ) && Between( c, d, b ) ) {
      Assigndi( p, d );
      Assigndi( q, b );
      return 'e';
   }
   if ( Between( a, b, d ) && Between( c, d, a ) ) {
      Assigndi( p, d );
      Assigndi( q, a );
      return 'e';
   }
   return '0';
}
void	Assigndi( tPointd p, tPointi a )
{
   int i;
   for ( i = 0; i < DIM; i++ )
      p[i] = a[i];
}
/*---------------------------------------------------------------------
Returns TRUE iff point c lies on the closed segement ab.
Assumes it is already known that abc are collinear.
---------------------------------------------------------------------*/
bool    Between( tPointi a, tPointi b, tPointi c )
{
   tPointi      ba, ca;

   /* If ab not vertical, check betweenness on x; else on y. */
   if ( a[X] != b[X] )
      return ((a[X] <= c[X]) && (c[X] <= b[X])) ||
             ((a[X] >= c[X]) && (c[X] >= b[X]));
   else
      return ((a[Y] <= c[Y]) && (c[Y] <= b[Y])) ||
             ((a[Y] >= c[Y]) && (c[Y] >= b[Y]));
}

/*---------------------------------------------------------------------
Returns the dot product of the two input vectors.
---------------------------------------------------------------------*/
double  Dot( tPointi a, tPointi b )
{
    int i;
    double sum = 0.0;

    for( i = 0; i < DIM; i++ )
       sum += a[i] * b[i];

    return  sum;
}

