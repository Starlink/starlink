/*
 *  Name:
 *     writesliceCmd
 *
 *  Purpose:
 *     Interface from RTD to RTD_SLICE routine.
 *
 *  Description:
 *     This routine is designed to be called from RTD, the Real Time
 *     Display tool, as a foreign method. It accepts an info structure
 *     that gives direct access to the displayed image and an unparsed
 *     sequence of arguments.
 *
 *     The arguments string must contain the following arguments:
 *
 *        -file ndf_name
 *        -line x1 y1 x1 y2
 *        -nelem number_of_elements
 *        -name image_name
 *
 *      -file is the name of the file that will contain the slice. If
 *            this isn't an NDF then it should be a supported foreign
 *            data file. 
 *
 *      -line end points of the line in pixel indices (integers).
 *
 *      -nelem is the number of pixels the output slice will contain. 
 *
 *      -name is the name of the image from which the slice is being
 *            taken.
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
 *     25-NOV-1997 (PWD):
 *        Original version.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "StarRtdForeignCmds.h"
#include "f77.h"
#include "cnf.h"
#include "sae_par.h"
#include "ems.h"
#include "ems_par.h"
#define MAXFILE 132

/*  Function prototypes */

extern F77_SUBROUTINE(rtd_slice)( CHARACTER(name), POINTER(image),
                                  CHARACTER(type), INTEGER(nx),
                                  INTEGER(ny), INTEGER(xs1), INTEGER(ys1),
                                  INTEGER(xs2), INTEGER(ys2),
                                  INTEGER(nelem), CHARACTER(file),
                                  INTEGER(status)
                                  TRAIL(name) TRAIL(type) TRAIL(file));

int writesliceCmd( struct StarImageInfo *info, char *args, char **errStr )
{

  /* Local declarations (Fortran versions)                           */
  DECLARE_CHARACTER(file, MAXFILE);     /* Name of spectrum file*/
  DECLARE_CHARACTER(name, MAXFILE);     /* Name of original image */
  DECLARE_CHARACTER(type, 10);          /* HDS type of data */
  DECLARE_INTEGER(nelem);               /* Number of elements in slice */
  DECLARE_INTEGER(nx);                  /* Size of image in X */
  DECLARE_INTEGER(ny);                  /* Size of image in Y */
  DECLARE_INTEGER(status);              /* Starlink STATUS */
  DECLARE_INTEGER(xs1);                 /* Lower X value */
  DECLARE_INTEGER(xs2);                 /* Upper X value */
  DECLARE_INTEGER(ys1);                 /* Lower Y value */
  DECLARE_INTEGER(ys2);                 /* Upper Y value */
  DECLARE_POINTER(image);               /* Pointer to image data. */

  /* Local variables: */
  int i, j;
  int used;
  int result;
  char *ptr;
  char *atPtr;
  char *opStr;
  char *opPtr;
  char param[EMS__SZPAR];

#ifdef _DEBUG_
  printf( "Called writesliceCmd \n");
#endif

  /* Runtime initialisations. */
  opStr = (char *)NULL;
  xs1 = ys1 = xs2 = ys2 = 0;
  cnf_exprt( "gaiaslice", (char *)file, MAXFILE );
  cnf_exprt( "unknown", (char *)name, MAXFILE );
  nelem = 512;
  result = 1;

  /*  Parse the input arguments extracting the parameters which have
      been set. */
  atPtr = args;
  while ( ( ptr = strtok( atPtr, " " ) ) != NULL ) {
    atPtr = (char *) NULL;

    if ( strcmp( ptr,  "-file" ) == 0 ) {
      ptr = strtok( atPtr, " " );
      cnf_exprt( ptr, (char *)file, MAXFILE );
    } else if ( strcmp( ptr,  "-line" ) == 0 ) {
      ptr = strtok( atPtr, " " );
      xs1 = (F77_INTEGER_TYPE) atoi( ptr );
      ptr = strtok( atPtr, " " );
      ys1 = (F77_INTEGER_TYPE) atoi( ptr );
      ptr = strtok( atPtr, " " );
      xs2 = (F77_INTEGER_TYPE) atoi( ptr );
      ptr = strtok( atPtr, " " );
      ys2 = (F77_INTEGER_TYPE) atoi( ptr );
    } else if ( strcmp( ptr,  "-nelem" ) == 0 ) {
      ptr = strtok( atPtr, " " );
      nelem = (F77_INTEGER_TYPE) atoi( ptr );
    } else if ( strcmp( ptr,  "-name" ) == 0 ) {
      ptr = strtok( atPtr, " " );
      cnf_exprt( ptr, (char *)name, MAXFILE );
    }
  }
    /*  Determine HDS type of image. */
  switch ( info->type ) {
  case  BYTE_IMAGE:
    cnf_exprt( "_BYTE", (char *)type, 10 );
    break;
  case  X_IMAGE:
    cnf_exprt( "_UBYTE", (char *)type, 10 );
    break;
  case  SHORT_IMAGE:
    cnf_exprt( "_WORD", (char *)type, 10 );
    break;
  case  USHORT_IMAGE:
    cnf_exprt( "_UWORD", (char *)type, 10 );
    break;
  case  LONG_IMAGE:
    cnf_exprt( "_INTEGER", (char *)type, 10 );
    break;
  case  FLOAT_IMAGE:
    cnf_exprt( "_REAL", (char *)type, 10 );
    break;
  }

  /*  Set up the image information. */
  image = (F77_POINTER_TYPE) info->imageData;
  nx = (F77_INTEGER_TYPE) info->nx;
  ny = (F77_INTEGER_TYPE) info->ny;

  /*  Call Fortran routine to do the work. */
  ems_mark_c();
  F77_CALL(rtd_slice)( CHARACTER_ARG(name), POINTER_ARG(&image),
                       CHARACTER_ARG(type), INTEGER_ARG(&nx),
                       INTEGER_ARG(&ny), INTEGER_ARG(&xs1),
                       INTEGER_ARG(&ys1), INTEGER_ARG(&xs2),
                       INTEGER_ARG(&ys2), INTEGER_ARG(&nelem),
                       CHARACTER_ARG(file), INTEGER_ARG(&status)
                       TRAIL_ARG(name) TRAIL_ARG(type) TRAIL_ARG(file));
  if ( status != SAI__OK ) {

    /*  Get the error from EMS and return it as errStr. */
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
  return result;
}
