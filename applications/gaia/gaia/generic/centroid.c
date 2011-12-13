/*
 *  Name:
 *     centroidCmd

 *  Purpose:
 *     Interface from RTD to CENTROID routine.

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

 *  Notes:
 *     -  The -coords parameter should be paired doubles, these are
 *        read until the end of the string or until a new argument
 *        string is started.

 *  Arguments:
 *     StarImageInfo = struct * (Given)
 *        Pointer to an ImageInfo structure.
 *     args = char * (Given)
 *        Pointer to any arguments.
 *     errStr = char ** (Returned)
 *        Pointer to pointer to a string that contains an error
 *        message if appropriate. Only set if return is 0.

 *  Copyright:
 *     Copyright (C) 1998 Central Laboratory of the Research Councils
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *
 *  Authors:
 *     PWD: Peter W. Draper (STARLINK - Durham University)

 *  History:
 *     12-DEC-1997 (PWD):
 *        Original version.
 *     20-JAN-2000 (PWD):
 *        Added byte swap changes.
 *     30-MAY-2001 (PWD):
 *        Added double precision image support.
 */
#if HAVE_CONFIG_H
#include <config.h>
#endif

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
#include "gaiaUtils.h"

/*  Function prototypes */

extern F77_SUBROUTINE(rtd_cent)( CHARACTER(type), POINTER(image),
                                 LOGICAL(swap),
				 INTEGER(nx), INTEGER(ny),
				 DOUBLE_ARRAY(xin),
                                 DOUBLE_ARRAY(yin),
				 INTEGER(nin), INTEGER(isize),
				 DOUBLE(maxshf), INTEGER(maxit),
				 DOUBLE(toler),
                                 DOUBLE_ARRAY(xout),
				 DOUBLE_ARRAY(yout),
                                 INTEGER(nout),
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
   DECLARE_DOUBLE_ARRAY_DYN(fxin);       /* Pointer to X positions. */
   DECLARE_DOUBLE_ARRAY_DYN(fxout);      /* Pointer to X positions. */
   DECLARE_DOUBLE_ARRAY_DYN(fyin);       /* Pointer to Y positions. */
   DECLARE_DOUBLE_ARRAY_DYN(fyout);      /* Pointer to Y positions. */
   DECLARE_LOGICAL(swap);

   /* Local variables: */
   char **listArgv;
   char *opPtr;
   char *opStr;
   char buffer[TCL_DOUBLE_SPACE+2];
   double *xin;
   double *yin;
   double *xout;
   double *yout;
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
   int size;
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
   size = nin * sizeof(double);
   xin = cnfMalloc( size );
   yin = cnfMalloc( size );
   xout = cnfMalloc( size );
   yout = cnfMalloc( size );
   for ( i = istart, j = 0; i < iend; i += 2, j++ ) {
      xin[j] = atof( listArgv[i] );
      yin[j] = atof( listArgv[i+1] );
   }

   /* Export all arrays for use in Fortran */
   F77_CREATE_DOUBLE_ARRAY( fxin, size );
   F77_EXPORT_DOUBLE_ARRAY( xin, fxin, size );

   F77_CREATE_DOUBLE_ARRAY( fyin, size);
   F77_EXPORT_DOUBLE_ARRAY( yin, fyin, size );

   F77_CREATE_DOUBLE_ARRAY( fxout, size );
   F77_EXPORT_DOUBLE_ARRAY( xout, fxout, size );

   F77_CREATE_DOUBLE_ARRAY( fyout, size );
   F77_EXPORT_DOUBLE_ARRAY( yout, fyout, size );


   /*  Determine HDS type of image. */
   switch ( info->type ) {
      case  BYTE_IMAGE:
         cnf_exprt( "_UBYTE", (char *) type, 10 );
         break;
      case  X_IMAGE:
         cnf_exprt( "_BYTE", (char *) type, 10 );
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
      case  DOUBLE_IMAGE:
         cnf_exprt( "_DOUBLE", (char *) type, 10 );
         break;
   }

   /*  Set up the image information. */
   F77_EXPORT_POINTER( info->imageData, image );
   F77_EXPORT_INTEGER( info->nx, nx );
   F77_EXPORT_INTEGER( info->ny, ny );
   if ( info->swap ) {
      swap = F77_TRUE;
   } else {
      swap = F77_FALSE;
   }

   /*  Call Fortran routine to do the work. */
   emsMark();
   status = SAI__OK;
   F77_CALL(rtd_cent)( CHARACTER_ARG(type), POINTER_ARG(&image),
                       LOGICAL_ARG(&swap), INTEGER_ARG(&nx),
                       INTEGER_ARG(&ny), DOUBLE_ARRAY_ARG(fxin),
                       DOUBLE_ARRAY_ARG(fyin), INTEGER_ARG(&nin),
                       INTEGER_ARG(&isize), DOUBLE_ARG(&maxshift),
                       INTEGER_ARG(&maxit), DOUBLE_ARG(&toler),
                       DOUBLE_ARRAY_ARG(fxout), DOUBLE_ARRAY_ARG(fyout),
                       INTEGER_ARG(&nout), INTEGER_ARG(&status)
                       TRAIL_ARG(type));

   if ( status == SAI__OK ) {
      /*  Need to encode the return values as a list of x,y coordinate
          pairs and return them as the result. */
     F77_IMPORT_DOUBLE_ARRAY( fxout, xout, size );
     F77_IMPORT_DOUBLE_ARRAY( fyout, yout, size );

      opPtr = Tcl_Alloc( EMS__SZMSG * sizeof(char) );
      opPtr[0] = '\0';
      have = EMS__SZMSG;
      used = 0;
      for ( i = 0; i < nin; i++ ) {
         (void) sprintf( buffer, " %f %f ", xout[i], yout[i] );
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

   }
   else {
      /*  Centroid routine exited in error, so get the error from EMS
          and return it as errStr. */
       *errStr = gaiaUtilsErrMessage();

       /*  Set success of routine to false. */
       result = 0;
   }
   emsRlse();
   Tcl_Free( (char *) listArgv );
   cnfFree( xin );
   cnfFree( yin );
   cnfFree( xout );
   cnfFree( yout );
   return result;
}
