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
#include        <stdio.h>
#include        <math.h>
#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1
#define X       0
#define Y       1
typedef enum { FALSE, TRUE } bool;
typedef enum { Pin, Qin, Unknown } tInFlag;

#define DIM     2               /* Dimension of points */
typedef int     tPointi[DIM];   /* type integer point */
typedef double  tPointd[DIM];   /* type double point */
#define PMAX    1000            /* Max # of pts in polygon */

typedef tPointi tPolygoni[PMAX];/* type integer polygon */

/*---------------------------------------------------------------------
Function prototypes.
---------------------------------------------------------------------*/
void    ClosePostscript( void );
void	PrintSharedSeg( tPointd p, tPointd q );
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
void    ConvexIntersect( tPolygoni P, tPolygoni Q, int n, int m );
tInFlag InOut( tPointd p, tInFlag inflag, int aHB, int bHA );
int     Advance( int a, int *aa, int n, bool inside, tPointi v );
void	OutputPolygons( void );
/*-------------------------------------------------------------------*/

/* Global variables */
int     	n, m;
tPolygoni	P, Q;

main()
{

   n = ReadPoly( P );
   m = ReadPoly( Q );
   OutputPolygons();
   ConvexIntersect( P, Q, n, m );

   ClosePostscript();
}

/*---------------------------------------------------------------------
---------------------------------------------------------------------*/
void    ConvexIntersect( tPolygoni P, tPolygoni Q, int n, int m )
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

   do {
      /*printf("%%Before Advances:a=%d, b=%d; aa=%d, ba=%d; inflag=%d\n", a, b, aa, ba, inflag);*/
      /* Computations of key variables. */
      a1 = (a + n - 1) % n;
      b1 = (b + m - 1) % m;

      SubVec( P[a], P[a1], A );
      SubVec( Q[b], Q[b1], B );

      cross = AreaSign( Origin, A, B );
      aHB   = AreaSign( Q[b1], Q[b], P[a] );
      bHA   = AreaSign( P[a1], P[a], Q[b] );
      printf("%%cross=%d, aHB=%d, bHA=%d\n", cross, aHB, bHA );

      /* If A & B intersect, update inflag. */
      code = SegSegInt( P[a1], P[a], Q[b1], Q[b], p, q );
      printf("%%SegSegInt: code = %c\n", code );
      if ( code == '1' || code == 'v' ) {
         if ( inflag == Unknown && FirstPoint ) {
            aa = ba = 0;
            FirstPoint = FALSE;
            p0[X] = p[X]; p0[Y] = p[Y];
            printf("%8.2lf %8.2lf moveto\n", p0[X], p0[Y] );
         }
         inflag = InOut( p, inflag, aHB, bHA );
         printf("%%InOut sets inflag=%d\n", inflag);
      }

      /*-----Advance rules-----*/

      /* Special case: A & B overlap and oppositely oriented. */
      if ( ( code == 'e' ) && (Dot( A, B ) < 0) )
            PrintSharedSeg( p, q ), exit(EXIT_SUCCESS);

      /* Special case: A & B parallel and separated. */
      if ( (cross == 0) && ( aHB < 0) && ( bHA < 0 ) )
            printf("%%P and Q are disjoint.\n"), exit(EXIT_SUCCESS);

      /* Special case: A & B collinear. */
      else if ( (cross == 0) && ( aHB == 0) && ( bHA == 0 ) ) {
            /* Advance but do not output point. */
            if ( inflag == Pin )
               b = Advance( b, &ba, m, inflag == Qin, Q[b] );
            else
               a = Advance( a, &aa, n, inflag == Pin, P[a] );
         }

      /* Generic cases. */
      else if ( cross >= 0 ) {
         if ( bHA > 0)
            a = Advance( a, &aa, n, inflag == Pin, P[a] );
         else
            b = Advance( b, &ba, m, inflag == Qin, Q[b] );
      }
      else /* if ( cross < 0 ) */{
         if ( aHB > 0)
            b = Advance( b, &ba, m, inflag == Qin, Q[b] );
         else
            a = Advance( a, &aa, n, inflag == Pin, P[a] );
      }
      printf("%%After advances:a=%d, b=%d; aa=%d, ba=%d; inflag=%d\n", a, b, aa, ba, inflag);

   /* Quit when both adv. indices have cycled, or one has cycled twice. */
   } while ( ((aa < n) || (ba < m)) && (aa < 2*n) && (ba < 2*m) );

   if ( !FirstPoint ) /* If at least one point output, close up. */
            printf("%8.2lf %8.2lf lineto\n", p0[X], p0[Y] );

   /* Deal with special cases: not implemented. */
   if ( inflag == Unknown) 
      printf("%%The boundaries of P and Q do not cross.\n");
}

/*---------------------------------------------------------------------
Prints out the double point of intersection, and toggles in/out flag.
---------------------------------------------------------------------*/
tInFlag InOut( tPointd p, tInFlag inflag, int aHB, int bHA )
{
   printf("%8.2lf %8.2lf lineto\n", p[X], p[Y] );

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
   if ( inside )
      printf("%5d    %5d    lineto\n", v[X], v[Y] );
   (*aa)++;
   return  (a+1) % n;
}

/*
   Reads in the coordinates of the vertices of a polygon from stdin,
   puts them into P, and returns n, the number of vertices.
   Formatting conventions: etc.
*/
int   ReadPoly( tPolygoni P )
{
   int   n = 0;
   int   nin;

   scanf("%d", &nin);
   /*printf("%%Polygon:\n");
   printf("%%  i   x   y\n");*/
   while ( (n < nin) && (scanf("%d %d",&P[n][0],&P[n][1]) != EOF) ) {
      /*printf("%%%3d%4d%4d\n", n, P[n][0], P[n][1]);*/
      ++n;
   }
/*
   if (n < PMAX)
      printf("%%n = %3d vertices read\n",n);
   else   printf("Error in read_poly:  too many points; max is %d\n", PMAX);
   putchar('\n');
*/

   return   n;
}

void   PrintPoly( int n, tPolygoni P )
{
   int   i;

   printf("Polygon:\n");
   printf("  i   l   x   y\n");
   for( i = 0; i < n; i++ )
      printf("%3d%4d%4d%4d\n", i, P[i][0], P[i][1]);
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

    /* The area should be an integer. */
    if      ( area2 >  0.5 ) return  1;
    else if ( area2 < -0.5 ) return -1;
    else                     return  0;
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

   /*printf("%%SegSegInt: a,b,c,d: (%d,%d), (%d,%d), (%d,%d), (%d,%d)\n",
	a[X],a[Y], b[X],b[Y], c[X],c[Y], d[X],d[Y]);*/

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
   /*printf("num=%lf, denom=%lf, s=%lf\n", num, denom, s);*/

   num = -( a[X] * (double)( c[Y] - b[Y] ) +
            b[X] * (double)( a[Y] - c[Y] ) +
            c[X] * (double)( b[Y] - a[Y] ) );
   if ( (num == 0.0) || (num == denom) ) code = 'v';
   t = num / denom;
   /*printf("num=%lf, denom=%lf, t=%lf\n", num, denom, t);*/

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
/*   
   printf("ParallelInt: a,b,c,d: (%d,%d), (%d,%d), (%d,%d), (%d,%d)\n",
	a[X],a[Y], b[X],b[Y], c[X],c[Y], d[X],d[Y]);
*/

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

void   OutputPolygons( void )
{
   int i;
   int xmin, ymin, xmax, ymax;

   /* Compute Bounding Box for Postscript header. */
   xmin = xmax = P[0][X];
   ymin = ymax = P[0][Y];
   for (i = 1; i < n; i++) {
      if      ( P[i][X] > xmax ) xmax = P[i][X];
      else if ( P[i][X] < xmin ) xmin = P[i][X];
      if      ( P[i][Y] > ymax ) ymax = P[i][Y];
      else if ( P[i][Y] < ymin ) ymin = P[i][Y];
   }
   for (i = 0; i < m; i++) {
      if      ( Q[i][X] > xmax ) xmax = Q[i][X];
      else if ( Q[i][X] < xmin ) xmin = Q[i][X];
      if      ( Q[i][Y] > ymax ) ymax = Q[i][Y];
      else if ( Q[i][Y] < ymin ) ymin = Q[i][Y];
   }


   /* PostScript header */
   printf("%%!PS\n");
   printf("%%%%Creator: convconv.c (Joseph O'Rourke)\n");
   printf("%%%%BoundingBox: %d %d %d %d\n",
      xmin, ymin, xmax, ymax);
   printf("%%%%EndComments\n");
   printf(".00 .00 setlinewidth\n");
   printf("%d %d translate\n", -xmin+100, -ymin+100 );
   /* The +100 shifts the figure from the lower left corner. */

   printf("\n%%Polygon P:\n");
   printf("newpath\n");
   printf("%d\t%d\tmoveto\n", P[0][X], P[0][Y]);
   for( i = 1; i <= n; i++ )
      printf("%d\t%d\tlineto\n", P[i%n][X], P[i%n][Y]);
   printf("closepath stroke\n");

   printf("\n%%Polygon Q:\n");
   printf("newpath\n");
   printf("%d\t%d\tmoveto\n", Q[0][X], Q[0][Y]);
   for( i = 1; i <= m; i++ )
      printf("%d\t%d\tlineto\n", Q[i%m][X], Q[i%m][Y]);
   printf("closepath stroke\n");

   printf("2 2 setlinewidth\n");
   printf("newpath\n");
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

void	PrintSharedSeg( tPointd p, tPointd q )
{
   printf("%%A int B:\n");
   printf("%8.2lf %8.2lf moveto\n", p[X], p[Y] );
   printf("%8.2lf %8.2lf lineto\n", q[X], q[Y] );
   ClosePostscript();
}

void   ClosePostscript( void )
{
   printf("closepath stroke\n");
   printf("showpage\n%%%%EOF\n");
}
