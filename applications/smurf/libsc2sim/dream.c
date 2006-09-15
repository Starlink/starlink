/* dream - routines for supporting DREAM data */

/* Standard includes */
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <time.h>

/* SMURF includes */
#include "smurf_par.h"

/* SC2DA includes */
#include "sc2da/Dits_Err.h"
#include "sc2da/Ers.h"
#include "sc2da/dream_par.h"

/* Simulator includes */
#include "libsc2sim/dream.h"

/* Other includes */
#include "fitsio.h"

static int sub;                  /* Subsystem nr */
static int nbolx;                /* number of bolometers in X direction */
static int nboly;                /* number of bolometers in Y direction */
static char errmess[132];        /* error message string */

/*+ dream_2store - Store 2-dim data file to disk */

void dream_2store 
( 
char *foutput,     /* output file name (given) */
int nxsize,        /* image dimension (given) */
int nysize,        /* image dimension (given) */
double *buf,       /* pixel values (given) */
int *status        /* global status (given and returned) */
)

/*  Description :
     Store image in FITS format using the CFITSIO routines.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
     19Aug2002 : Original version (bdk)
*/

{

   fitsfile *fptr;         /* fitsio file pointer */
   long naxis;             /* array dimensionality */
   long naxes[2];          /* array dimensions */
   long pstart[2];          /* subarray start coordinates */
   long pend[2];            /* subarray end coordinates */


   if ( !StatusOkP(status) ) return;

   fits_create_file ( &fptr, foutput, status );

/* Output the whole image */

   naxis = 2;
   naxes[0] = nxsize;
   naxes[1] = nysize;
   pstart[0] = 1;
   pstart[1] = 1;
   pend[0] = nxsize;
   pend[1] = nysize;

   fits_create_img ( fptr, DOUBLE_IMG, naxis, naxes, status );

   fits_write_subset ( fptr, TDOUBLE, pstart, pend, buf, status );

   fits_close_file ( fptr, status );

   fits_report_error ( stderr, *status );
}




/*+ dream_bolinfo - Return information per bolometer */

void dream_bolinfo 
(
int boln,           /* bolometer number within subarray (given) */
double *tau_bol,    /* Bolometer time constant in msec (returned) */ 
double *bol_calib,  /* calibration factor (returned) */
char *bol_name,     /* name of bolometer (returned) */
int *sample_ord     /* The group in which the data sampling moment for
                       boln takes place (returned) */
)

/*  Description :
     Return bolometer parameters which are assumed to be calibratable
     fixed values.

    Authors :
     H.W. van Someren Greve (greve@astron.nl)

    History :
      21-10-2001 : Original version (GREVE)
      5Sep2002 :  C translation (bdk)
*/

{
   int row;                       /* Row number */
   int col;                       /* column number */
   char subc[4] = { 'A','B','C','D' };


   *tau_bol   = 5.0;
   *bol_calib = 1.0;

/* We don't know the bolometer names, so we invent something.
   "B2614" means : row 14, column 26 of subsystem B. */

   row = boln / nbolx;
   col = boln % nbolx;
   sprintf ( bol_name, "%c%d%d", subc[0], col, row );

/* We don't know the bolometer sampling order, so we invent something.
   Sampling will be done per column, and for a single subsystem. */

   *sample_ord = boln;

}


/*+ dream_bolinit - initialise bolometer data */

void dream_bolinit
(
int sarray,        /* subarray number 1-4 (given) */
int nx,            /* number of bolometers in X (given) */
int ny,            /* number of bolometers in Y (given) */
int *status        /* global status (given and returned) */
)

/* Description :
    Store the values for subsequent use.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    21Jan2003 : original (bdk)
*/

{
   if ( !StatusOkP(status) ) return;

   sub = sarray;
   nbolx = nx;
   nboly = ny;
}


/*+ dream_boljiginx - Find bolometer and jiggle indices */

void dream_boljiginx
(
int igrid,       /* Index in grid image (given) */
int nbolx,       /* Number of bolometers in X (given) */
int nboly,       /* Number of bolometers in Y (given) */
int nxpsol,      /* Nr of points in X-dir (given) */
int nextxpl,     /* Nr of extra cols left side (given) */
int nextypb,     /* Nr of extra rows bottom side (given) */
int *ibol,       /* Index in Bolometer table (returned) */
int *colj,       /* Jiggle offset in X-dir (returned) */
int *rowj        /* Jiggle offset in Y-dir (returned) */
)

/* Description :
    Find for the required grid point the bolometer index and the 
    Jiggle point offsets.

    The routine extracts the row and column of the grid point in the image, 
    then checks if this row and column coincides with a bolometer position.
    This is almost always the case, unless we have a row and/or column at the
    outside of the image. 
    In that case we return the index of the nearest bolometer plus the offset
    (colj, rowj) of the grid position w.r.t. the bolometer position.
    So usually colj and rowj are zero.

   Authors :
    H.W. van Someren Greve (greve@astron.nl)

   History :
     07-09-2001 : Original version (GREVE)
     05Dec2002 : C translation (bdk)
*/
{
   int ri;                        /* Grid row in image */
   int ci;                        /* Grid column in image */
   int rb;                        /* Bolometer row */
   int cb;                        /* Bolometer column */


   ri = igrid / nxpsol;                /* Row in image of igrid */
   ci = igrid % nxpsol;                /* Col. in image of igrid */

/* Find the proper Bolometer row and Jiggle offset in the X-direction */

   rb = ri - nextypb;                 /* Guess of bolometer row */
   if ( rb < 0 )                      /* If too low, then */
   {
      *rowj = rb;                     /* Set Jiggle offset X */
      rb = 0;                         /* and set lowest row */
   }
   else if ( rb >= nboly )            /* If too high, then */
   {
      *rowj = rb - nboly + 1;         /* Set Jiggle offset X  */
      rb = nboly - 1;                 /* and set highest row */
   }
   else                               /* Bolometer row must be good */
   {
      *rowj = 0;                      /* Set no jiggle offset X */
   }

/* Find the proper Bolometer column and Jiggle offset in the Y-direction */

   cb = ci - nextxpl;                 /* Guess of bolometer column */
   if ( cb < 0 )                      /* If too low, then */
   {
      *colj = cb;                     /* Set Jiggle offset Y */
      cb = 0;                         /* and set lowest column */
   }
   else if ( cb >= nbolx )            /* If too high, then */
   {
      *colj = cb - nbolx + 1;         /* Set Jiggle offset Y */
      cb = nbolx - 1;                 /* and set highest column */
   }
   else                               /* Bolometer column must be good */
   {
      *colj = 0;                      /* Set no jiggle offset Y */
   }

   *ibol = rb * nbolx + cb;         /* Make proper bolometer index */

}


/*+ dream_convdist - Calculate the distance for a given PSF response */

double dream_convdist 
(
int conv_shape,   /* Code for the convolution function. 
                     0 - Gaussian (given) */
double conv_sig,  /* Convolution function parameter.
                     If the convolution function is gaussian :
                       CONVAL(r) = exp[-r^2/(2.s^2)]
                       r - Distance in units of HPBW from the centre.
                       s - The specified value of Conv_sig. (given */
double v,         /* The value of the convolution function (<= 1) (given) */
int *status       /* global status (given and returned) */
)

/* Description :
    Calculate the distance in units of the HPBW at which the convolution
    function response value has a specified value.

   Authors :
    H.W. van Someren Greve (greve@astron.nl)

   History :
     08-09-2001 : Original version (GREVE)
     01Nov2002 : C translation (bdk)
*/

{
   double s;
   double convdist = 0.0;

   if ( !StatusOkP(status) ) return 0.0;

   if ( conv_shape == 0 )
   {

/* Gaussian */

      s = 2.0 * conv_sig * conv_sig;
      if ( v <= 1.0)
      {
         convdist = sqrt ( -s * log(v) );
      }
      else
      {
         sprintf ( errmess, "Illegal convolution value %e", v );
         *status = DITS__APP_ERROR;
         ErsRep ( 0, status, errmess );
      }
   }
   else
   {
      sprintf ( errmess, "Convolution function %d not implemented", 
        conv_shape );
      *status = DITS__APP_ERROR;
      ErsRep ( 0, status, errmess );
   }

   return convdist;
}




/*+ dream_getbool - prompt for a boolean value */

void dream_getbool
(
char *prompt,   /* prompt string (given) */
int vdefault,   /* default value (given) */
int *value,     /* value obtained (returned) */
int *status     /* global status (given and returned) */
)

/* Description :
    Prompt for a valid boolean value given a prompt string and a default.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
     01Nov2002 : original (bdk)
*/

{
   int ok;          /* flag for re-prompting */
   char ans[80];    /* input string */
   char cdef[2];    /* default string */

   if ( vdefault == 1 )
   {
      strcpy ( cdef, "y" );
   }
   else
   {
      strcpy ( cdef, "n" );
   }

   ok = 0;
   while ( ok == 0 )
   {
      dream_getstring ( prompt, cdef, ans, status );
      if ( StatusOkP(status) )
      {
         if ( ( ans[0] == 'y' ) || ( ans[0] == 'Y' ) )
         {
            ok = 1;
            *value = 1;
         }
         else if ( ( ans[0] == 'n' ) || ( ans[0] == 'N' ) )
         {
            ok = 1;
            *value = 0;
         }
      }
      else
      {
         break;
      }
   }
}




/*+ dream_getdouble - prompt for a double value */

void dream_getdouble
(
char *prompt,    /* prompt string (given) */
double vdefault, /* default value (given) */
double vmin,     /* minimum acceptable value (given) */
double vmax,     /* maximum acceptable value (given) */
double *value,   /* value obtained (returned) */
int *status      /* global status (given and returned) */
)

/* Description :
    Prompt for a valid double value given a prompt string a default and a
    valid range.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
     01Nov2002 : original (bdk)
*/

{
   int ok;          /* flag for re-prompting */
   char ans[80];    /* input string */
   char cdef[80];   /* default string */

   sprintf ( cdef, "%e", vdefault );

   ok = 0;
   while ( ok == 0 )
   {
      dream_getstring ( prompt, cdef, ans, status );
      if ( StatusOkP(status) )
      {
         *value = atof ( ans );
         if ( ( *value >= vmin ) && ( *value <= vmax ) )
         {
            ok = 1;
         }
      }
      else
      {
         break;
      }
   }
}


/*+ dream_getint - prompt for an int value */

void dream_getint
(
char *prompt,   /* prompt string (given) */
int vdefault,   /* default value (given) */
int vmin,       /* minimum acceptable value (given) */
int vmax,       /* maximum acceptable value (given) */
int *value,     /* value obtained (returned) */
int *status     /* global status (given and returned) */
)

/* Description :
    Prompt for a valid int value given a prompt string a default and a
    valid range.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
     01Nov2002 : original (bdk)
*/

{
   int ok;          /* flag for re-prompting */
   char ans[80];    /* input string */
   char cdef[80];   /* default string */

   sprintf ( cdef, "%d", vdefault );

   ok = 0;
   while ( ok == 0 )
   {
      dream_getstring ( prompt, cdef, ans, status );
      if ( StatusOkP(status) )
      {
         *value = atoi ( ans );
         if ( ( *value >= vmin ) && ( *value <= vmax ) )
         {
            ok = 1;
         }
      }
      else
      {
         break;
      }
   }
}


/*+ dream_getstring - prompt for string input */

void dream_getstring 
( 
char *prompt,       /* prompt string (given) */
char *cdef,         /* default string (given) */
char *ans,          /* response (returned) */
int *status         /* global status (given and returned) */
)

/*  Description :
     Use the prompt to get an answer with a possible default.

    Authors :
     B.D.Kelly (bdk@roe.ac.uk)

    History :
     05Sep2002 : original (bdk)
*/

{
   char string[132];


   if ( !StatusOkP(status) ) return;

   printf ( "%s [%s] > ", prompt, cdef );
   gets ( string );
   if ( strlen(string) == 0 )
   {
      strcpy ( ans, cdef );
   }
   else
   {
      strcpy ( ans, string );
   }
}






/*+ dream_grid_index - Calculate the grid indices for all bolometers */

void dream_grid_index
(
int ngrid,           /* Number of grid positions within the jiggle area
                        (given) */
int nbolx,           /* Number of bolometers in X (given) */
int nboly,           /* Number of bolometers in Y (given) */
int jigpts[][2],     /* Table with relative grid coordinates determined
                        within the Jiggle area (given) */
int *nunkno,         /* number of unknowns for solution (returned) */
int gridindex[],     /* bolometer index for each grid position (returned) */
int *status          /* global status (given and returned) */
)

/* Description :
    This routine creates an array, gridindex, which allows a simple
    look-up of position in the full reconstructed map given an individual
    bolometer and one of the grid points calculated from that bolometer.

     0000000000000000000000
     0000000000000000000000
     0000000000000000000000
     000000***0000000000000
     000000***0000000000000
     000000***0000000000000
     0000000000000000000000

    In this example the reconstructed map has 22 grid points in the
    X-direction. The grid points are counted from the bottom-left corner
    starting at zero. The first asterisk is at index 28. Assume the
    asterisks mark the positions of grid values generated by bolometer
    number 6, and each bolometer generates a total of nine positions as
    shown, then
      gridindex[bolno*ngrid+bolgrid] = mapgrid
    ie
      gridindex[6*9+0] = 28

 
   Authors :
    H.W. van Someren Greve (greve@astron.nl)

   History :
    07-09-2001 : Original version (GREVE)
    02Dec2002 : C translation (bdk)
    07Apr2003 : return nunkno and make gridindex refer directly to final
                map positions(bdk)
    16May2003 : update description (bdk)
*/

{
   int xmin;                  /* Jiggle limits in X-dir */
   int xmax;                  /* Jiggle limits in X-dir */
   int ymin;                  /* Jiggle limits in Y-dir */
   int ymax;                  /* Jiggle limits in Y-dir */
   int nrxl;                  /* Nr of extra cols left side */
   int nryb;                  /* Nr of extra rows bottom side */
   int nheight;               /* Maximum image height */
   int nwidth;                /* Maximum image width */
   int i;                     /* Bolometer location */
   int j;                     /* Bolometer location */
   int ip;                    /* Grid location */
   int jp;                    /* Grid location */
   int m;                     /* calculated grid index in full map */
   int n;                     /* Loop var over bol */
   int p;                     /* Loop var over grid */
      

   if ( !StatusOkP(status) ) return;

/* Find the limits in the Jiggle area */

   xmin = jigpts[0][0];
   xmax = xmin;
   ymin = jigpts[0][1];
   ymax = ymin;

   for ( j=0; j<ngrid; j++ )
   {
        if (jigpts[j][0] < xmin) xmin = jigpts[j][0];
        if (jigpts[j][0] > xmax) xmax = jigpts[j][0];
        if (jigpts[j][1] < ymin) ymin = jigpts[j][1];
        if (jigpts[j][1] > ymax) ymax = jigpts[j][1];
   }

/* Derive the number of extra rows and columns and the maximal image width.
   Nwidth is the maximum number of columns in the grid image 
   (or the maximum nr of points in X-direction)
   If Nwidth is larger than Nbolx, then some extra columns could be 
   added in the solution matrix. 
   The number of extra columns depend on the Jiggle pattern, and is
   usually of order 1 or 2.
   NrXl is the nr of extra points in X-direction at the left side.
   Extra columns are possible at the left side and/or at the right side 
   of the image. The maximum extra nr of columns at the right side is then
   Nwidth-NrXl. 
   NrYb is the nr of extra points in Y-direction at the bottom side
   The number of extra rows depend on the Jiggle pattern, and is
   usually of order 1 or 2.
   Extra rows are possible at the bottom and/or at the top of the image. 
*/

   nwidth = nbolx + xmax - xmin;         /* Nr of points in X-dir */
   nheight = nboly + ymax - ymin;        /* Nr of points in Y-dir */
   nrxl = -xmin;                         /* Nr of extra X-p (left side) */
   nryb = -ymin;                         /* Nr of extra Y-p (bottom side) */
   *nunkno = nbolx*nboly + nwidth*nheight;
   if ( dream_trace(3) )
   {
      printf ( "dream_grid_index: nbolx = %d nboly = %d\n", nbolx, nboly );
      printf ( "dream_grid_index: nwidth = %d nheight = %d\n", nwidth,
        nheight );
   }

   for ( n=0; n<nbolx*nboly; n++ )
   {
      i = n % nbolx;                             /* Bol. -X- index */
      j = n / nbolx;                             /* Bol. -Y- index */
      for ( p=0; p<ngrid; p++ )
      {
         ip = i + jigpts[p][0];                  /* Grid -X- index */
         jp = j + jigpts[p][1];                  /* Grid -Y- index */
         m = (jp+nryb) * nwidth + ip + nrxl;

         gridindex[n*ngrid+p] = m;              /* Set Grid index */
      }
   }

}



/*+ dream_grid_pos - Calculate the grid positions in the image */

void dream_grid_pos 
(
int nxpsol,           /* Nr of columns (X) in the grid image (given) */
int nypsol,           /* Nr of rows (Y) in the grid image (given) */
int nextxpl,          /* Nr of extra columns at the left side (given) */
int nextypb,          /* Nr of extra rows at the bottom side (given) */
int nbolx,            /* Number of bolometers in X (given) */
int nboly,            /* Number of bolometers in Y (given) */
double bol_xy[][2],   /* The (distorted) bolometer coordinates in X and Y
                         direction in arcsec (given) */
double bol_distx,     /* average bolometer distance in X (given) */
double bol_disty,     /* average bolometer distance in Y (given) */
double grid_xy[][2],  /* The (distorted) grid coordinates in X and Y in
                         arcsec (returned) */
int *status           /* global status (given and returned) */
)

/* Description :
    This routine calculates the Nasmyth grid coordinates from the table
    with bolometer positions and information about the Jiggle pattern.
    In general the grid points cover an area which is larger than the
    area covered by the bolometers alone.
    This means that we add rows and columns at the edge of the bolometer
    area. Usually only 2 rows and 2 columns are added, 1 row at the bottom
    and 1 row at the top, and 1 column at the left and 1 column at the right
    side.

    If nxpsol is larger than nbolx, then some extra columns have been 
    added in the solution matrix (nextxpl). 
    The number of extra columns depend on the Jiggle pattern, and is
    usually of order 1 or 2.
    Extra columns are possible at the left side and/or at the right side 
    of the image. The extra nr of columns at the right side is then
    nxpsol - nbolx - nextxpl. 

    If nypsol is larger than nboly, then some extra rows have been 
    added in the solution matrix (nextypb). 
    The number of extra rows depend on the Jiggle pattern, and is
    usually of order 1 or 2.
    Extra rows are possible at the bottom and/or at the top of the image. 
    The extra nr of rows at the top is then nypsol - nboly - nextypb. 

    We us routine boljiginx for finding for each grid position index the 
    corresponding bolometer index and the relative Jiggle pattern position.

   Authors :
    H.W. van Someren Greve (greve@astron.nl)

   History :
    07-09-2001 : Original version (GREVE)
    03Dec2002 : C translation (bdk)
*/

{
   int i;                         /* Loop variable */
   int bix;                       /* Bol table index */
   int cj;                        /* Current Jiggle column */
   int rj;                        /* Current Jiggle row */


   if ( !StatusOkP(status) ) return;


   for ( i=0; i<nxpsol*nypsol; i++ )
   {

/* Calculate for each grid point i, the bolometer index bix, and the
   Jiggle point distance from the grid point to the bolometer position,
   (cj, rj) in units of the Jiggle step size.
   In most cases, cj and rj should be zero, and are only not zero at the 
   edges, if i doesn't coincide with a bolometer position n. */

      dream_boljiginx ( i, nbolx, nboly, nxpsol, nextxpl, nextypb, 
        &bix, &cj, &rj );

/* Now make for each grid point the Nasmyth coordinates. */

      grid_xy[i][0] = bol_xy[bix][0] + (double)cj * bol_distx;
      grid_xy[i][1] = bol_xy[bix][1] + (double)rj * bol_disty;

   }

}


/*+ dream_jigpts - Calculate the grid points in the Jiggle */

void dream_jigpts 
(
int npath,           /* Number of positions in jigpath (given) */
double jigpath[][2], /* Buffer containing the X and Y coordinates of each
                        point in the path of the SMU during the Jiggle,
                        in units of the HPBW (given) */
int conv_shape,      /* Code for the shape of the convolution function.
                        0 - Gaussian (given) */

double conv_sig,     /* The meaning depends on the code for the
                        convolution function.
                        If Gaussian : convolution(r) = exp[-r^2/(2.s^2)]
                        r - Distance in units of HPBW (=6.18 arcsec for
                            the long wave array)
                        s - The specified value of Conv_sig (given) */
int *ngrid,          /* Nr of Jiggle positions in JigPts.
                        This is also equal to the number of coefficients
                        per line in the equation matrix (returned) */

int jigpts[][2],     /* The relative Jiggle positions for which an
                        intensity must be calculated in units of the
                        average bolometer distance (returned) */
int *status          /* global status (given and returned) */
)

/* Description :
    This routine extracts the Jiggle positions within the Jiggle area
    for which an intensity must be calculated.
    This can me slightly more than the nr of Jiggle positions visited
    but may however not exceed DREAM__MXGRID.

   Authors :
    H.W. van Someren Greve (greve@astron.nl)

   History :
     07-09-2001 : Original version (GREVE)
     31Oct2002  : C translation (bdk)
     20Jun2003  : change constant name to DREAM__MXGRID (bdk)
*/

{
   double limit = 0.66;
   int i;                      /* Loop variable */
   int j;                      /* Loop variable */
   int n;                      /* Table index */
   int mx;                     /* Nr of points in X-dir */
   int my;                     /* Nr of points in Y-dir */
   double r;                   /* Distance */
   double xmax;                /* Max grid position in X */
   double xmin;                /* Min grid position in X */
   double ymax;                /* Max grid position in Y */
   double ymin;                /* Min grid position in Y */


   if ( !StatusOkP(status) ) return;

/*  Find the maximum and minimum coordinates which are visited during 
    the complete Jiigle path */

   xmin = 1000.0;
   xmax = -1000.0;
   ymin = 1000.0;
   ymax = -1000.0;
   for ( i=0; i<npath; i++ )
   {
        if ( jigpath[i][0] < xmin ) xmin = jigpath[i][0];
        if ( jigpath[i][0] > xmax ) xmax = jigpath[i][0];
        if ( jigpath[i][1] < ymin ) ymin = jigpath[i][1];
        if ( jigpath[i][1] > ymax ) ymax = jigpath[i][1];
   }

   if ( dream_trace(4) )
   {
      printf ( "dream_jigpts: Xmin and Xmax in jigpath %e %e\n", xmin,
        xmax );
      printf ( "dream_jigpts: Ymin and Ymax in jigpath %e %e\n", ymin,
        ymax );
   }

/* Important now is to find out how many grid points fall between 
   Xmin and Xmax, and between Ymin and Ymax.

   We neglect any contributions less than limit, so we have to calculate 
   the radius of the circle within the Convolution function contributions 
   are larger than limit. */

   r = dream_convdist ( conv_shape, conv_sig, limit, status );

   xmin = xmin - r;
   xmax = xmax + r;
   ymin = ymin - r;
   ymax = ymax + r;

   mx = (int)xmax - (int)xmin + 1;
   my = (int)ymax - (int)ymin + 1;
   *ngrid = mx * my;

   if ( dream_trace(4) )
   {
      printf ( "dream_jigpts: convolution limit %e\n", r );
      printf ( "dream_jigpts: Area Xmin and Xmax %e %e\n", xmin, xmax );
      printf ( "dream_jigpts: Area Ymin and Ymax %e %e\n", ymin, ymax );
   }

/* Check the values mx and my.
   mx should be equal to my, because we only allow Jiggle patterns
   which define a square area.
   mx*my should be <= DREAM__MXGRID. */

   if ( mx != my )
   {
      *status = DITS__APP_ERROR;
      sprintf ( errmess, "dream_jigpts: error mx != my %d %d", mx, my );
      ErsRep ( 0, status, errmess );
   }

   if ( *ngrid > DREAM__MXGRID )
   {
      *status = DITS__APP_ERROR;
      sprintf ( errmess, "dream_jigpts: error ngrid > DREAM__MXGRID %d %d", 
        *ngrid, DREAM__MXGRID );
      ErsRep ( 0, status, errmess );
   }

/* Fill JigPts with the required Jiggle positions in X and Y.
   Order from left under to top right. */

   if ( StatusOkP(status) )
   {

      if ( dream_trace(2) )
      {
         printf ( "dream_jigpts: Nr of grid points selected %d\n", *ngrid );
         printf ( " Nr  -X- -Y-\n" );
      }

      n = 0;

      for ( j=(int)ymin; j<=(int)ymax; j++ )
      {
         for ( i=(int)xmin; i<=(int)xmax; i++ )
         {
             jigpts[n][0] = i;
             jigpts[n][1] = j;
             if ( dream_trace(2) )
             {
                printf ( " %d    %d   %d\n", n, jigpts[n][0], jigpts[n][1] );
             }
             n++;
         }
      }
   }
}





/*+ dream_smupath - Calculate the path positions of the SMU */

void dream_smupath 
(
int nvert,           /* number of vertices in the jiggle pattern, 
                        implemented are :
                        =1 : No visit of points.
                        At the moment a circle but that does not work !
                        =4 : Visit 4 points on a square.
                        =5 : Visit 5 points on a '+'
                        =8 : Visit 8 points on a star. (This is the best)
                        (given) */
double vertex_t,     /* Time for movement between vertices in msec (given) */
int jig_vert[][2],   /* Table with relative vertex coordinates in time
                        (given) */
double jig_stepx,    /* The step size in -X- direction between Jiggle
                        positions in arcsec (given) */
double jig_stepy,    /* The step size in -Y- direction between Jiggle
                        positions in arcsec (given) */
int movecode,        /* The code for the SMU waveform that determines the
                        SMU motion (given) */
int nppp,            /* The number of calculated coordinates in the path
                        between 2 successive vertices (given) */
double sample_t,     /* time between samples in msec (given) */
double smu_offset,   /* smu timing offset in msec (given) */
int pathsz,          /* maximum no of path points (given) */
double jigpath[][2], /* Buffer containing the X and Y coordinates of each
                        point in the path of the SMU during the Jiggle in
                        units of arcsec (returned) */
int *status          /* global status (given and returned) */
)

/* Description :
    This routine calculates all the SMU positions during the cycle
    This depends on the Jiggle Pattern, the SMU waveform, and the 
    overdimensioning factor.

   Authors :
    H.W. van Someren Greve (greve@astron.nl)

   History :
     07-09-2001 : Original version (GREVE)
     31Oct2002  : C translation (bdk)
     13Feb2003  : change to use dream_smupos and to allow smu_offset (bdk)
     20Jun2003  : pass-in pathsz, change constant name to DREAM__MXVERT (bdk)
*/

{
   int np;                          /* nr of calculated points */
   int j;                           /* Loop variable */
   double anginc;                   /* Angle increment */
   double t;                        /* time at a point in the path */      
   double vertxy[DREAM__MXVERT][2]; /* coordinates of jiggle vertices */


   if ( !StatusOkP(status) ) return;

/* Make the number of positions in the Jiggle Path, and check the size */

   np = nppp * nvert;

   if ( np > pathsz )
   {
      *status = DITS__APP_ERROR;
      sprintf ( errmess,
        "%d points in the Jiggle path requested, but only %d points allowed",
        np, pathsz );
      ErsRep ( 0, status, errmess );
   }


   if ( nvert > 1)
   {


/* Calculate all the positions. */

      if ( dream_trace(3) )
      {
         printf ( "dream_smupath: vertext offsets are\n" );
      }
      for ( j=0; j<nvert; j++ )
      {
         vertxy[j][0] = jig_stepx * (double)jig_vert[j][0];
         vertxy[j][1] = jig_stepy * (double)jig_vert[j][1];
         if ( dream_trace(3) )
         {
            printf ( "dream_smupath: %e   %e\n", vertxy[j][0], vertxy[j][1] );
         }
      }

      for ( j=0; j<np; j++ )
      {
         t = (double)j * sample_t + smu_offset;
         dream_smupos ( t, vertex_t, movecode, nvert, vertxy,
           &(jigpath[j][0]), &(jigpath[j][1]), status );
      }
   }
   else if ( nvert == 1)
   {
      anginc = 2.0 * M_PI / (double)np;
      for ( j=0; j<np; j++ )
      {
         jigpath[j][0] = 1.2 * jig_stepx * cos ( (double)j * anginc );
         jigpath[j][1] = 0.9 * jig_stepy * sin ( (double)j * anginc );
      }

   }


   if ( dream_trace(2) )
   {
      printf ( "dream_smupath : jigpath with %d positions\n", np );

      printf ( "Position    -X-       -Y-\n" );
      for ( j=0; j<np; j++ )
      {
         printf ( "  %d    %e  %e\n", j, jigpath[j][0], jigpath[j][1] );
      }
   }

}



/*+ dream_smupos - Calculate the SMU position at an instant */

void dream_smupos
(
double t,           /* Time from start of pattern in msec (given) */
double vertex_t,    /* Time for movement between vertices in msec (given) */
int movecode,       /* The code for the SMU waveform that determines the
                       SMU motion (given) */
int nvert,          /* number of vertices in the DREAM pattern (given) */
double vertxy[][2], /* Table of vertex offsets in arcsec (given) */
double *x,          /* calculated X-position (returned) */
double *y,          /* calculated Y-position (returned) */
int *status         /* global status (given and returned) */
)

/* Description :
   This routine calculates the SMU waveform between 2 Jiggle positions.

   Authors :
    H.W. van Someren Greve (greve@astron.nl)

   History :
     07-09-2001 : Original version (GREVE)
     01Nov2002 : C translation (bdk)
     12Feb2003 : adapted from original routine dsim_smuwave (bdk)
*/

{
   int jstart;                /* index of vertex before sample point */
   int jend;                  /* index of vertex after sample point */
   double mxv;                /* Maximum value */
   double theta;              /* Fractional time between vertices in radians */
   double frac;               /* fractional distance between vertices */
   double offset;             /* time after passing first vertex in msec */

   if ( !StatusOkP(status) ) return;

/* Get vertex coordinates bracketing the measurement instant */

   if ( t < 0.0 )
   {

/* Add a cycle */

      jstart = ( (int)( (t + (double)nvert * vertex_t ) / vertex_t) ) 
        % nvert;
      offset = t + (double)nvert * vertex_t - (double)jstart * vertex_t;
   }
   else
   {
      jstart = ( (int)(t / vertex_t) ) % nvert;
      offset = t - (double)jstart * vertex_t;
   }
   jend = ( jstart + 1 ) % nvert;   


   if ( movecode == 0 )
   {

/* Block wave. This is unrealistic, but is added for compatibility
   with old code. */

      *x = vertxy[jend][0];
      *y = vertxy[jend][1];

   }
   else if ( movecode == 1 )
   {

/* 2 term not damped. */

      mxv = sin ( M_PI / 4.0 ) + sin ( 3.0 * M_PI / 4.0 ) / 3.0;
      theta = ( -M_PI / 4.0 ) + offset * M_PI / vertex_t;
      frac = 1.104 * ( ( sin(theta) + sin ( 3.0 * theta ) / 3.0 ) / ( 2.0 * mxv ) 
           + 0.5 );
      *x = frac * ( vertxy[jend][0] - vertxy[jstart][0] ) + vertxy[jstart][0];
      *y = frac * ( vertxy[jend][1] - vertxy[jstart][1] ) + vertxy[jstart][1];
   }
   else if ( movecode == 2 )
   {

/* 3 term not damped. */

      mxv = sin ( M_PI / 6.0 ) + sin ( 3.0 * M_PI / 6.0 ) / 3.0 + 
        sin ( 5.0 *M_PI / 6.0 ) / 5.0; 

      theta = ( -M_PI / 6.0 ) + offset * M_PI / vertex_t;
      frac = 1.104 * ( ( sin(theta) + sin ( 3.0 * theta ) / 3.0 + 
           sin ( 5.0 * theta ) / 5.0 ) / ( 2.0 * mxv ) + 0.5 );
      *x = frac * ( vertxy[jend][0] - vertxy[jstart][0] ) + vertxy[jstart][0];
      *y = frac * ( vertxy[jend][1] - vertxy[jstart][1] ) + vertxy[jstart][1];

   }
   else if ( movecode == 3 )
   {

/* 4 term not damped. */

      mxv = sin ( M_PI / 8.0 ) + sin ( 3.0 * M_PI / 8.0 ) / 3.0 + 
        sin ( 5.0 * M_PI / 8.0 ) / 5.0 + sin ( 7.0 * M_PI / 8.0 ) / 7.0; 
      theta = (-M_PI/8.0) + offset * M_PI / vertex_t;
      frac = 1.104 * ( ( sin(theta) + sin ( 3.0 * theta ) / 3.0 + 
        sin ( 5.0 * theta ) / 5.0 + sin ( 7.0 * theta ) / 7.0 ) 
        / ( 2.0 * mxv ) + 0.5 );
      *x = frac * ( vertxy[jend][0] - vertxy[jstart][0] ) + vertxy[jstart][0];
      *y = frac * ( vertxy[jend][1] - vertxy[jstart][1] ) + vertxy[jstart][1];

   }
   else if ( movecode == 4 )
   {

/* 2 term flat end. */

      mxv = sin(M_PI/4.0) + sin(3.0*M_PI/4.0) / 3.0;
      theta = (-M_PI/4.0) + offset * M_PI / vertex_t;
      if ( theta < M_PI/4.0 )
      {
         frac =1.02 * ( ( sin(theta) + sin(3.0*theta) / 3.0 ) / (2.0*mxv) + 0.5 );
      }
      else 
      {
         frac = 1.0;
      }
      *x = frac * ( vertxy[jend][0] - vertxy[jstart][0] ) + vertxy[jstart][0];
      *y = frac * ( vertxy[jend][1] - vertxy[jstart][1] ) + vertxy[jstart][1];

   }
   else if ( movecode == 5 )
   {

/* 3 term flat end. */

      mxv = sin(M_PI/6.0) + sin(3.0*M_PI/6.0) / 3.0 + sin(5.0*M_PI/6.0) / 5.0;
      theta = (-M_PI/6.0) + offset * M_PI / vertex_t;
      if ( theta < M_PI/6.0 )
      {
         frac = ( sin(theta) + sin(3.0*theta) / 3.0 +  sin(5.0*theta) / 5.0 ) / 
           (2.0*mxv) + 0.5;
      }
      else 
      {
         frac = 1.0;
      }
      *x = frac * ( vertxy[jend][0] - vertxy[jstart][0] ) + vertxy[jstart][0];
      *y = frac * ( vertxy[jend][1] - vertxy[jstart][1] ) + vertxy[jstart][1];

   }
   else if ( movecode == 6 )
   {

/* 4 term flat end. */

      mxv = sin(M_PI/8.0) + sin(3.0*M_PI/8.0) / 3.0 + 
        sin(5.0*M_PI/8.0) / 5.0 + sin(7.0*M_PI/8.0) / 7.0;
      theta = -M_PI/8.0 + offset * M_PI / vertex_t;
      if ( theta < M_PI/8.0 )
      {
         frac = ( sin(theta) + sin(3.0*theta) / 3.0 + sin(5.0*theta) / 5.0 + 
           sin(7.0*theta) / 7.0 ) / (2.0*mxv) + 0.5;
      }
      else 
      {
         frac = 1.0;
      }
      *x = frac * ( vertxy[jend][0] - vertxy[jstart][0] ) + vertxy[jstart][0];
      *y = frac * ( vertxy[jend][1] - vertxy[jstart][1] ) + vertxy[jstart][1];

   }
   else if ( movecode == 7 )
   {

/* ScubaWave. After 1 Ms 0.098. After 8 Ms 0.913. After 9 Ms 1.000.
   popepi points is equivalent with 64 Ms. */

      if ( offset >= 9.0 )
      {
         frac = 1.0;
      }
      else if ( offset >= 8.0 )
      {
         frac = 0.913 + 0.087 * ( offset - 8.0 );
      }
      else if ( offset >= 1.0 )
      {
         frac = 0.098 + 0.116428571 * ( offset - 1.0 );
      }
      else 
      {
         frac = 0.098 * offset;
      }
      *x = frac * ( vertxy[jend][0] - vertxy[jstart][0] ) + vertxy[jstart][0];
      *y = frac * ( vertxy[jend][1] - vertxy[jstart][1] ) + vertxy[jstart][1];

   }
   else if ( movecode == 8 )
   {

/* Experimental. 
   This is an experimental wave form, which may change often..
   Now it is a cosine waveform from 0 to 1 in the full time. */

      frac = 0.5 * ( 1.0 - cos ( M_PI * offset / vertex_t ) );
      *x = frac * ( vertxy[jend][0] - vertxy[jstart][0] ) + vertxy[jstart][0];
      *y = frac * ( vertxy[jend][1] - vertxy[jstart][1] ) + vertxy[jstart][1];

   }

}



/*+ dream_smuwave - Calculate the SMU wave form */

void dream_smuwave 
(
int movecode,     /* The code for the SMU waveform that determines the
                     SMU motion (given) */
int npts,         /* The number of points in buffer A (given) */
double a[], /* Buffer containing the wave form (returned) */
int *status       /* global status (given and returned) */
)

/* Description :
   This routine calculates the SMU waveform between 2 Jiggle positions.

   Authors :
    H.W. van Someren Greve (greve@astron.nl)

   History :
     07-09-2001 : Original version (GREVE)
     01Nov2002 : C translation (bdk)
*/

{
   int i;                            /* Loop variable */
   double mxv;                       /* Maximum value */
   double popepi;                    /* Nr of points */
   double x;                         /* Variable */


   if ( !StatusOkP(status) ) return;


   popepi = (double)npts;

   if ( movecode == 0 )
   {

/* Block wave. This is unrealistic, but is added for compatibility
   with old code. */

      for ( i=0; i<npts; i++ )
      {
         a[i] = 1.0;
      }
   }
   else if ( movecode == 1 )
   {

/* 2 term not damped. */

      mxv = sin ( M_PI / 4.0 ) + sin ( 3.0 * M_PI / 4.0 ) / 3.0;
      for ( i=0; i<npts; i++ )
      {
         x = ( -M_PI / 4.0 ) + (double)i * M_PI / popepi;
         a[i] = 1.104 * ( ( sin(x) + sin ( 3.0 * x ) / 3.0 ) / ( 2.0 * mxv ) 
           + 0.5 );
      }
   }
   else if ( movecode == 2 )
   {

/* 3 term not damped. */

      mxv = sin ( M_PI / 6.0 ) + sin ( 3.0 * M_PI / 6.0 ) / 3.0 + 
        sin ( 5.0 *M_PI / 6.0 ) / 5.0; 

      for ( i=0; i<npts; i++ )
      {
         x = -M_PI / 6.0 + (double)i * M_PI / popepi;
         a[i] = 1.104 * ( ( sin(x) + sin ( 3.0 * x ) / 3.0 + 
           sin ( 5.0 * x ) / 5.0 ) / ( 2.0 * mxv ) + 0.5 );
      }
   }
   else if ( movecode == 3 )
   {

/* 4 term not damped. */

      mxv = sin ( M_PI / 8.0 ) + sin ( 3.0 * M_PI / 8.0 ) / 3.0 + 
        sin ( 5.0 * M_PI / 8.0 ) / 5.0 + sin ( 7.0 * M_PI / 8.0 ) / 7.0; 
      for ( i=0; i<npts; i++ )
      {
         x = (-M_PI/8.0) + (double)i * M_PI / popepi;
         a[i] = 1.104 * ( ( sin(x) + sin ( 3.0 * x ) / 3.0 + 
           sin(5.0*x) / 5.0 + sin (7.0*x) / 7.0 ) / (2.0*mxv) + 0.5 );
      }
   }
   else if ( movecode == 4 )
   {

/* 2 term flat end. */

      mxv = sin(M_PI/4.0) + sin(3.0*M_PI/4.0) / 3.0;
      for ( i=0;i<npts; i++ )
      {
         x = (-M_PI/4.0) + (double)i * M_PI / popepi;
         if ( x < M_PI/4.0 )
         {
            a[i] =1.02 * ( ( sin(x) + sin(3.0*x) / 3.0 ) / (2.0*mxv) + 0.5 );
         }
         else 
         {
            a[i] = 1.0;
         }
      }
   }
   else if ( movecode == 5 )
   {

/* 3 term flat end. */

      mxv = sin(M_PI/6.0) + sin(3.0*M_PI/6.0) / 3.0 + sin(5.0*M_PI/6.0) / 5.0;
      for ( i=0;i<npts; i++ )
      {
         x = (-M_PI/6.0) + (double)i * M_PI / popepi;
         if ( x < M_PI/6.0 )
         {
            a[i] = ( sin(x) + sin(3.0*x) / 3.0 +  sin(5.0*x) / 5.0 ) / 
              (2.0*mxv) + 0.5;
         }
         else 
         {
            a[i] = 1.0;
         }
      }
   }
   else if ( movecode == 6 )
   {

/* 4 term flat end. */

      mxv = sin(M_PI/8.0) + sin(3.0*M_PI/8.0) / 3.0 + 
        sin(5.0*M_PI/8.0) / 5.0 + sin(7.0*M_PI/8.0) / 7.0;
      for ( i=0; i<npts; i++ )
      {
         x = -M_PI/8.0 + (double)i * M_PI / popepi;
         if ( x < M_PI/8.0 )
         {
            a[i] = ( sin(x) + sin(3.0*x) / 3.0 + sin(5.0*x) / 5.0 + 
              sin(7.0*x) / 7.0 ) / (2.0*mxv) + 0.5;
         }
         else 
         {
            a[i] = 1.0;
         }
      }
   }
   else if ( movecode == 7 )
   {

/* ScubaWave. After 1 Ms 0.098. After 8 Ms 0.913. After 9 Ms 1.000.
   popepi points is equivalent with 64 Ms. */

      for ( i=0; i<npts; i++ )
      {
         x = (double)i * 64.0 /popepi;
         if ( x >= 9.0 )
         {
            a[i] = 1.0;
         }
         else if ( x >= 8.0 )
         {
            a[i] = 0.913 + 0.087 * ( x - 8.0 );
         }
         else if ( x >= 1.0 )
         {
            a[i] = 0.098 + 0.116428571 * ( x - 1.0 );
         }
         else 
         {
            a[i] = 0.098 * x;
         }
      }
   }
   else if ( movecode == 8 )
   {

/* Experimental. 
   This is an experimental wave form, which may change often..
   Now it is a cosine waveform from 0 to 1 in the full time. */

      for ( i=0; i<npts; i++ )
      {
         a[i] = ( cos ( M_PI * ( (double)i - popepi ) / popepi ) + 1.0 ) / 2.0;
      }

   }

}



/*+ dream_timenow - get current time and date as strings */

void dream_timenow
( 
int dlength,       /* length of date string (given) */
int tlength,       /* length of time string (given) */
int ilength,       /* length of iso date string (given) */
char cur_day[],    /* String with the date. E.g. "17-May-2001" 
                      (returned) */
char cur_time[],   /* String with the time. E.g. "21:59:26" (returned) */
char iso_time[],   /* ISO date in YYYY-MM-DDThh:mm:ss.s format (returned but can be null) */
char ymd[],        /* Contraction of ISO date in YYYYMMDD format (returned) */
int *status        /* global status (given and returned) */
)

/*  Description :
     Use the standard C library time routines.
    Authors :
     B.D.Kelly (ROE)
    History :
     04Sep2002:  original (bdk@roe.ac.uk)
*/

{
   struct tm *timestruct;
   time_t timeval;

   if ( !StatusOkP(status) ) return;

   time ( &timeval );
   timestruct = localtime ( &timeval );

   if (cur_day) strftime ( cur_day, dlength, "%d-%b-%Y", timestruct );
   if (cur_time) strftime ( cur_time, tlength, "%X", timestruct );
   if (iso_time) strftime( iso_time, ilength, "%Y-%m-%dT%H:%M:%S", timestruct);
   if (ymd) strftime( ymd, ilength, "%Y%m%d", timestruct);

}




/*+ dream_trace - provide a flag for debugging level */

int dream_trace
( 
int value       /* trace level (given) */
)

/*  Description :
     On first entry store the given trace level. On subsequent entries
     compare the new given value with the stored value.
     If new value <= stored value, return 1, else return 0.
    Authors :
     B.D.Kelly (ROE)
    History :
     04Sep2002:  original (bdk@roe.ac.uk)
*/

{
   static int localtrace = -1;            /* stored level */
   int ans;                               /* returned value */

   if ( localtrace == -1 )
   {
      localtrace = value;
      ans = 0;
   }
   else if ( value <= localtrace )
   {
      ans = 1;
   }
   else
   {
      ans = 0;
   }

   return ans;
}


/*+ dream_traceinit - initialise dream_trace facility */

void dream_traceinit
(
void
)

/*  Description :
     Check for the TRACE environment variable. Call dsim_trace() with the
     integer value of this, or zero.
    Authors :
     B.D.Kelly (ROE)
    History :
     04Feb2002:  original (bdk@roe.ac.uk)
*/

{
   char *sval;             /* pointer to value of TRACE variable */
   char *s;                /* pointer for stol */
   int tval;               /* integer value of TRACE variable */

   sval = getenv ( "TRACE" );
   if ( sval == NULL )
   {
      tval = 0;
   }
   else
   {
      tval = strtol ( sval, &s, 10 );
   }
   dream_trace ( tval );
}



/*+ dream_traceout - output trace information */

void dream_traceout 
( 
char *fmt,        /* printf-style format string (given) */
...               /* variable argument list (given) */
)

/*  Description :
     Pass the arguments to vprintf. In future this might be replaced by a
     DRAMA call.

    Authors :
     B.D.Kelly (bdk@roe.ac.uk)

    History :
     4Feb2003 : original (bdk)
*/

{
   va_list args;

   va_start ( args, fmt );

   vprintf ( fmt, args );

   va_end ( args );

}


