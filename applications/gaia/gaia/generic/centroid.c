/*
 *  Name:
 *     centroidCmd
 *
 *  Purpose:
 *     Interface from RTD to CENTROID routine.
 *
 *  Description:
 *     This routine is designed to be called from RTD, the Real Time
 *     Display tool, as a foreign method. It accepts an info structure
 *     that gives direct access to the displayed image and an unparsed
 *     sequence of arguments.
 *
 *     The arguments string must contain the following arguments:
 *
 *
 *     The arguments string must contain the following arguments:
 *
 *        -coords   pairs_of_doubles
 *        -isize    integer
 *	  -maxshift double
 *        -toler    double
 *        -maxit    integer
 *
 *      -coords this is a list of the postions to be centroided. The
 *              return of this function is returned via a Tcl result
 *              as a corresponding list of positions.
 *      -isize the centroid search box size.
 *      -maxshift the maximum shift in position.
 *      -toler the tolerence in centroid for convergence.
 *      -maxit the maximum number of iterations used in centroid estimation.
 *
 *  Notes:
 *     -  The -coords parameter should be paired doubles, these are
 *        read until the end of the string or until a new argument
 *        string is started.
 *
 *  Arguments:
 *     StarImageInfo = struct * (Given)
 *        Pointer to an ImageInfo structure.
 *     args = char * (Given)
 *        Pointer to any arguments.
 *     errStr = char ** (Returned)
 *        Pointer to pointer to a string that contains an error
 *        message if appropriate. Only set if return is 0.
 *
 *  Copyright:
 *     Copyright (C) 1998 Central Laboratory of the Research Councils
 *
 *  Authors:
 *     PWD: Peter W. Draper (STARLINK - Durham University)
 *
 *  History:
 *     12-DEC-1997 (PWD):
 *        Original version.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "StarRtdForeignCmds.h"
#include "f77.h"
#include "cnf.h"
#include "sae_par.h"
#include "ems.h"
#include "ems_par.h"

/*  Function prototypes */

extern F77_SUBROUTINE(rtd_cent)( CHARACTER(type), POINTER(image),
				 INTEGER(nx), INTEGER(ny),
				 POINTER(xin),POINTER(yin),
				 INTEGER(nin), INTEGER(isize),
				 DOUBLE(maxshf), INTEGER(maxit),
				 DOUBLE(toler), POINTER(xout),
				 POINTER(yout), INTEGER(nout),
				 INTEGER(status) TRAIL(type));

int centroidCmd( struct StarImageInfo *info, char *args, char **errStr )
{

  /* Local declarations (Fortran versions)                           */
  DECLARE_CHARACTER(type, 10);          /* HDS type of data */
  DECLARE_DOUBLE(maxshift);            /* Maximum allowable shift */
  DECLARE_DOUBLE(toler);                /* Tolerence */
  DECLARE_INTEGER(isize);               /* Search box side */
  DECLARE_INTEGER(maxit);               /* Maximum refining iterations */
  DECLARE_INTEGER(nx);                  /* Size of image in X */
  DECLARE_INTEGER(ny);                  /* Size of image in Y */
  DECLARE_INTEGER(status);              /* Starlink STATUS */
  DECLARE_POINTER(image);               /* Pointer to image data. */
  DECLARE_POINTER(*xin);                /* Pointer to X positions. */
  DECLARE_POINTER(*xout);               /* Pointer to X positions. */
  DECLARE_POINTER(*yin);                /* Pointer to Y positions. */
  DECLARE_POINTER(*yout);               /* Pointer to Y positions. */

  /* Local variables: */
  char **listArgv;
  char *opPtr;
  char *opStr;
  char buffer[TCL_DOUBLE_SPACE+2];
  char param[EMS__SZPAR];
  double *xnew;
  double *xold;
  double *ynew;
  double *yold;
  int have;
  int i;
  int iend;
  int istart;
  int j;
  int listArgc = 0;
  int need;
  int nin;
  int nout;
  int result;
  int used;

#ifdef _DEBUG_
  printf( "Called centroidCmd \n");
#endif

  /* Runtime initialisations. */
  opStr = (char *)NULL;
  result = 1;
  isize = 9;
  maxit = 3;
  maxshift = 5.5;
  toler = 0.05;

  /*  Parse the input args by splitting it into pieces as a Tcl list.*/
  if ( Tcl_SplitList( info->interp, args, &listArgc, &listArgv ) != TCL_OK ) {
    *errStr = (char *) malloc( (size_t) EMS__SZMSG * sizeof(char) );
    strcpy( *errStr, "failed to interpret coordinates as a list" );
    return 0;
  }
  istart = -1;
  iend = -1;
  i = 0;
  while ( i < listArgc ) {
    if ( strcmp( listArgv[i],  "-coords" ) == 0 ) {
      i++;
      /* Record where this starts and look for end of string or next
	 option (-[a-z]).  */
      istart = i;
      for ( ;i < listArgc; i++ ) {
	if ( *listArgv[i] == '-' && isalpha((int) *listArgv[i]) ) {
	  i--;
	  iend = i;
	  break;
	} else if (listArgv[i] == '\0' ) {
	  iend = i;
	  break;
	}
      }
    } else if ( strcmp( listArgv[i],  "-toler" ) == 0 ) {
      i++;
      toler = (F77_DOUBLE_TYPE) atof( listArgv[i] );
    } else if ( strcmp( listArgv[i],  "-maxshift" ) == 0 ) {
      i++;
      maxshift = (F77_INTEGER_TYPE) atoi( listArgv[i] );
    } else if ( strcmp( listArgv[i],  "-maxit" ) == 0 ) {
      i++;
      maxit = (F77_INTEGER_TYPE) atoi( listArgv[i] );
    } else if ( strcmp( listArgv[i],  "-isize" ) == 0 ) {
      i++;
      isize = (F77_INTEGER_TYPE) atoi( listArgv[i] );
    }
    i++;
  }
  if ( istart == -1 ) {
    /* No coordinates so nothing to do, except complain */
    Tcl_Free( (char *) listArgv );
    *errStr = (char *) malloc( (size_t) EMS__SZMSG * sizeof(char) );
    strcpy( *errStr, "no centroid coordinates given" );
    return 0;
  }
  if ( iend == -1 ) {
    iend = listArgc - 1;
  }

  /*  Decode the coordinates into separate values and copy these to
      arrays which we can passed to RTD_CENT. Note we also need arrays
      of the same size for the return positions. */
  nin = ( iend - istart + 1 ) / 2;
  if ( nin * 2 != ( iend - istart + 1 ) ) {
    Tcl_Free( (char *) listArgv );
    *errStr = malloc( (size_t) EMS__SZMSG * sizeof(char) );
    strcpy( *errStr, "coordinate lists contain an odd number of values" );
    return 0;
  }

  /* Now translate all these values into doubles.*/
  xnew = (double *) malloc( (size_t) nin * sizeof(double));
  ynew = (double *) malloc( (size_t) nin * sizeof(double));
  xold = (double *) malloc( (size_t) nin * sizeof(double));
  yold = (double *) malloc( (size_t) nin * sizeof(double));
  for ( i = istart, j = 0; i < iend; i += 2, j++ ) {
    xold[j] = atof( listArgv[i] );
    yold[j] = atof( listArgv[i+1] );
  }
  xin = (F77_POINTER_TYPE *) xold;
  yin = (F77_POINTER_TYPE *) yold;
  xout = (F77_POINTER_TYPE *) xnew;
  yout = (F77_POINTER_TYPE *) ynew;

  /*  Determine HDS type of image. */
  switch ( info->type ) {
  case  BYTE_IMAGE:
    cnf_exprt( "_BYTE", (char *) type, 10 );
    break;
  case  X_IMAGE:
    cnf_exprt( "_UBYTE", (char *) type, 10 );
    break;
  case  SHORT_IMAGE:
    cnf_exprt( "_WORD", (char *) type, 10 );
    break;
  case  USHORT_IMAGE:
    cnf_exprt( "_UWORD", (char *) type, 10 );
    break;
  case  LONG_IMAGE:
    cnf_exprt( "_INTEGER", (char *) type, 10 );
    break;
  case  FLOAT_IMAGE:
    cnf_exprt( "_REAL", (char *) type, 10 );
    break;
  }

  /*  Set up the image information. */
  image = (F77_POINTER_TYPE) info->imageData;
  nx = (F77_INTEGER_TYPE) info->nx;
  ny = (F77_INTEGER_TYPE) info->ny;

  /*  Call Fortran routine to do the work. */
  ems_mark_c();
  status = SAI__OK;
  F77_CALL(rtd_cent)( CHARACTER_ARG(type), POINTER_ARG(&image),
		      INTEGER_ARG(&nx), INTEGER_ARG(&ny),
		      POINTER_ARG(xin),POINTER_ARG(yin),
		      INTEGER_ARG(&nin), INTEGER_ARG(&isize),
		      DOUBLE_ARG(&maxshift), INTEGER_ARG(&maxit),
		      DOUBLE_ARG(&toler), POINTER_ARG(xout),
		      POINTER_ARG(yout), INTEGER_ARG(&nout),
		      INTEGER_ARG(&status) TRAIL_ARG(type));

  if ( status == SAI__OK ) {
    /*  Need to encode the return values as a list of x,y coordinate
	pairs and return them as the result. */

    opPtr = Tcl_Alloc( EMS__SZMSG * sizeof(char) );
    opPtr[0] = '\0';
    have = EMS__SZMSG;
    used = 0;
    for ( i = 0; i < nin; i++ ) {
      (void) sprintf( buffer, " %f %f ", xnew[i], ynew[i] );
      need = strlen( buffer );
      if ( ( need + used + 2 ) > have ) {
	have = have + EMS__SZMSG;
        opPtr = Tcl_Realloc( opPtr, have );
      }
      (void) strcat( opPtr, buffer );
      used += need + 1;
    }
    Tcl_ResetResult( info->interp );
    Tcl_SetResult( info->interp, opPtr, TCL_DYNAMIC );
    result = 1;

  } else {
    /*  Centroid routine exited in error, so get the error from EMS
	and return it as errStr. */
    opStr = (char *) NULL;
    used = 0;
    while ( status != SAI__OK ) {
      opStr = realloc( (void *)opStr, (size_t) EMS__SZMSG * sizeof(char) );
      opPtr = opStr + used;
      ems_stat_c( &status );
      ems_eload_c( param, &i, opPtr, &j, &status);
      used += j;
      opStr[used++] ='\n';
    }
    opStr[used] = '\0';
    *errStr = opStr;

    /*  Set success of routine to false. */
    result = 0;
  }
  ems_rlse_c();
  Tcl_Free( (char *) listArgv );
  free( xnew );
  free( ynew );
  free( xold );
  free( yold );
  return result;
}
