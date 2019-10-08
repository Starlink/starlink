#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "star/ndg.h"
#include "star/kaplibs.h"
#include "star/grp.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include <math.h>
#include <string.h>
#include <stdio.h>

#define CALCMODE_NONE 0
#define CALCMODE_TOPOL 1
#define CALCMODE_FROMPOL 2

F77_SUBROUTINE(complex_)( INTEGER(STATUS) ){
/*
*+
*  Name:
*     COMPLEX

*  Purpose:
*     Converts between representations of complex data.

*  Language:
*     C (designed to be called from Fortran)

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL COMPLEX( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts between various representations of complex
*     data, including complex NDFs.  The conversion may simply unpack or pack
*     real and imaginary parts of a complex NDF, or it may convert between
*     polar and cartesian representation.

*  Usage:
*     complex in1 in2 out1 out2 [intype] [outtype]

*  ADAM Parameters:
*     CALCMODE = LITERAL (Write)
*        Set to indicate the type of calculation which was performed:
*        "To polar", "From polar" or "None".
*     IN1 = NDF (Read)
*        The first input NDF.
*     IN2 = NDF (Read)
*        The second input NDF.
*     OUT1 = NDF (Write)
*        The first output NDF.
*     OUT2 = NDF (Write)
*        The second output NDF.
*     INTYPE = LITERAL (Read)
*        The nature of the input NDF(s).  Options are:
*
*        - "COMPLEX" -- IN1 is a complex NDF containing real
*        and imaginary parts.  (IN2 will not be accessed.)
*
*        - "REAL_IMAG" -- IN1 contains the real part and IN2
*        contains the imaginary parts.
*
*        - "MOD_ARG" -- IN1 contains the modulus and IN2 the
*        argument in radians.
*
*        [COMPLEX if IN1 is a complex NDF, otherwise REAL_IMAG]
*     OUTTYPE = LITERAL (Read)
*        The nature of the output NDF(s).  The same options are
*        available as for INTYPE but relate to NDFs OUT1 and OUT2
*        instead of IN1 and IN2.
*
*        [REAL_IMAG if IN1 is a complex NDF, otherwise COMPLEX]
*     TITLE1 = LITERAL (Read)
*        The title for the first output NDF.
*     TITLE2 = LITERAL (Read)
*        The title for the second output NDF.

*  Examples:
*     complex realmap imagmap cmplxmap
*        This example combines real and imaginary parts into a complex NDF.
*
*     complex cplxmap ! modulusmap ! OUTTYPE=MOD_ARG
*         This example would compute the modulus from a complex NDF.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2019 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     GSB: Graham S. Bell
*     {enter_new_authors_here}

*  History:
*     9-OCT-2019 (GSB):
*        Original version based on makesnr.
*     {enter_further_changes_here}

*-
*/

   GENPTR_INTEGER(STATUS)

/* Local Variables: */
   int indf_i1 = NDF__NOID;  /* Identifier for first input NDF */
   int indf_i2 = NDF__NOID;  /* Identifier for second input NDF */
   int indf_o1 = NDF__NOID;  /* Identifier for first output NDF */
   int indf_o2 = NDF__NOID;  /* Identifier for second output NDF */
   Grp *grp;                 /* GRP identifier for the input NDF. */
   size_t size;              /* Size of GRP group */
   char intype[32];          /* Input nature. */
   char outtype[32];         /* Output nature. */
   char *types = "COMPLEX,REAL_IMAG,MOD_ARG"; /* Input/output nature choices. */

   double *ipd_ir = NULL;    /* Pointer to input real part of Data array */
   double *ipd_ii = NULL;    /* Pointer to input imaginary part of Data array */
   double *ipd_im = NULL;    /* Pointer to input modulus Data array */
   double *ipd_ia = NULL;    /* Pointer to input argument Data array */

   double *ipd_or = NULL;    /* Pointer to output real part of Data array */
   double *ipd_oi = NULL;    /* Pointer to output imaginary part of Data array */
   double *ipd_om = NULL;    /* Pointer to output modulus Data array */
   double *ipd_oa = NULL;    /* Pointer to output argument Data array */

   int cmplx = 0;            /* Is the input data array complex? */
   int el_i1 = 0;            /* Numbers of input array elements mapped */
   int el_i2 = 0;            /* Numbers of input array elements mapped */
   int el_o1 = 0;            /* Numbers of output array elements mapped */
   int el_o2 = 0;            /* Numbers of output array elements mapped */
   int i;                    /* Loop count */
   int calcmode = CALCMODE_NONE; /* Integer identifying type of calculation */
   double real, imag, mod, arg; /* Complex value being processed */

/* Abort if an error has already occurred. */
   if( *STATUS != SAI__OK ) return;

/* Start an NDF context */
   ndfBegin();

/* Get an identifier for the NDF IN1. We use NDG (via kpg1_Rgndf)
   instead of calling ndfAssoc directly since NDF/HDS has problems with
   file names containing spaces, which NDG does not have. */
   if( *STATUS != SAI__OK ) goto L999;
   kpg1Rgndf( "IN1", 1, 1, "", &grp, &size, STATUS );
   ndgNdfas( grp, 1, "READ", &indf_i1, STATUS );
   grpDelet( &grp, STATUS );
   if( *STATUS == PAR__NULL ) {
      errAnnul( STATUS );
   }

   if( indf_i1 != NDF__NOID ) {
      ndfCmplx( indf_i1, "Data", &cmplx, STATUS );
   }

   if( cmplx ) {
      parChoic( "INTYPE", "COMPLEX", types, 1, intype, 31, STATUS );
      parChoic( "OUTTYPE", "REAL_IMAG", types, 1, outtype, 31, STATUS );
   } else {
      parChoic( "INTYPE", "REAL_IMAG", types, 0, intype, 31, STATUS );
      if( !strcmp( "COMPLEX", intype ) ) {
         *STATUS = SAI__ERROR;
         errRep( "", "The NDF IN1 does not appear to contain complex data", STATUS );
      }
      parChoic( "OUTTYPE", "COMPLEX", types, 1, outtype, 31, STATUS );
   }

/* Get an identifier for the NDF IN2, unless INTYPE=COMPLEX. */
   if( strcmp( "COMPLEX", intype ) ) {
      if( *STATUS != SAI__OK ) goto L999;
      kpg1Rgndf( "IN2", 1, 1, "", &grp, &size, STATUS );
      ndgNdfas( grp, 1, "READ", &indf_i2, STATUS );
      grpDelet( &grp, STATUS );
      if( *STATUS == PAR__NULL ) {
         errAnnul( STATUS );
      }
   }

/* Check we have at least one input NDF. */
   if( indf_i1 == NDF__NOID && indf_i2 == NDF__NOID ) {
      *STATUS = SAI__ERROR;
      errRep( "", "Neither input NDF IN1 nor IN2 was specified", STATUS );
   }

/* Get an identifier for the NDF OUT1. */
   if( *STATUS != SAI__OK ) goto L999;
   ndfProp( ( indf_i1 == NDF__NOID ? indf_i2 : indf_i1 ),
            "AXIS,WCS,QUALITY,UNITS", "OUT1", &indf_o1, STATUS );
   if( *STATUS == PAR__NULL ) {
      errAnnul( STATUS );
   }

/* Get an identifier for the NDF OUT2, unless OUTTYPE=COMPLEX.
   Units are not propagated if OUTTYPE=MOD_ARG. */
   if( strcmp( "COMPLEX", outtype ) ) {
      if( *STATUS != SAI__OK ) goto L999;
      ndfProp( ( indf_i1 == NDF__NOID ? indf_i2 : indf_i1 ),
               ( strcmp( "MOD_ARG", outtype ) ? "AXIS,WCS,QUALITY,UNITS" : "AXIS,WCS,QUALITY" ),
               "OUT2", &indf_o2, STATUS );
      if( *STATUS == PAR__NULL ) {
         errAnnul( STATUS );
      }
   }

/* Check we have at least one output NDF. */
   if( indf_o1 == NDF__NOID && indf_o2 == NDF__NOID ) {
      *STATUS = SAI__ERROR;
      errRep( "", "Neither output NDF OUT1 nor OUT2 was specified", STATUS );
   }

/* Set the titles using the TITLE1 and TITLE2 parameter
   and the OUT2 units to radians if OUTTYPE=MOD_ARG. */
   if( indf_o1 != NDF__NOID ) {
      ndfCinp( "TITLE1", indf_o1, "Title", STATUS );
   }

   if( indf_o2 != NDF__NOID ) {
      ndfCinp( "TITLE2", indf_o2, "Title", STATUS );

      if( !strcmp( "MOD_ARG", outtype ) ) {
         ndfCput( "radians", indf_o2, "Units", STATUS );
      }
   }

   if( *STATUS != SAI__OK ) goto L999;

/* Map the input DATA components. */
   if( !strcmp( "COMPLEX", intype ) ) {
      if( indf_i1 != NDF__NOID ) {
         ndfMapz( indf_i1, "Data", "_DOUBLE", "READ", (void *) &ipd_ir, (void *) &ipd_ii, &el_i1, STATUS );
      }
   } else if( !strcmp( "REAL_IMAG", intype ) ) {
      if( indf_i1 != NDF__NOID ) {
         ndfMap( indf_i1, "Data", "_DOUBLE", "READ", (void *) &ipd_ir, &el_i1, STATUS );
      }
      if( indf_i2 != NDF__NOID ) {
         ndfMap( indf_i2, "Data", "_DOUBLE", "READ", (void *) &ipd_ii, &el_i2, STATUS );
      }
   } else if( !strcmp( "MOD_ARG", intype ) ) {
      if( indf_i1 != NDF__NOID ) {
         ndfMap( indf_i1, "Data", "_DOUBLE", "READ", (void *) &ipd_im, &el_i1, STATUS );
      }
      if( indf_i2 != NDF__NOID ) {
         ndfMap( indf_i2, "Data", "_DOUBLE", "READ", (void *) &ipd_ia, &el_i2, STATUS );
      }
   } else {
      *STATUS = SAI__ERROR;
      errRep( "", "Unexpected INTYPE value (programming error)", STATUS );
   }

/* Check that the number of elements in each input match,
   or copy el_i2 to el_i1 if we only have IN1. */
   if( el_i2 && ! el_i1 ) {
      el_i1 = el_i2;
   } else if( el_i1 && el_i2 && ( el_i1 != el_i2 ) ) {
      *STATUS = SAI__ERROR;
      errRep( "", "Input NDFs IN1 and IN2 appear to be of different sizes", STATUS );
   }

/* Map the output DATA components. Set the type of the output components
   so that they are not complex unless OUTTYPE=COMPLEX. */
   if( !strcmp( "COMPLEX", outtype ) ) {
      if( indf_o1 != NDF__NOID ) {
         ndfStype( "COMPLEX_DOUBLE", indf_o1, "Data", STATUS );
         ndfMapz( indf_o1, "Data", "_DOUBLE", "WRITE", (void *) &ipd_or, (void *) &ipd_oi, &el_o1, STATUS );
      }
   } else if( !strcmp( "REAL_IMAG", outtype ) ) {
      if( indf_o1 != NDF__NOID ) {
         ndfStype( "_DOUBLE", indf_o1, "Data", STATUS );
         ndfMap( indf_o1, "Data", "_DOUBLE", "WRITE", (void *) &ipd_or, &el_o1, STATUS );
      }
      if( indf_o2 != NDF__NOID ) {
         ndfStype( "_DOUBLE", indf_o2, "Data", STATUS );
         ndfMap( indf_o2, "Data", "_DOUBLE", "WRITE", (void *) &ipd_oi, &el_o2, STATUS );
      }
   } else if( !strcmp( "MOD_ARG", outtype ) ) {
      if( indf_o1 != NDF__NOID ) {
         ndfStype( "_DOUBLE", indf_o1, "Data", STATUS );
         ndfMap( indf_o1, "Data", "_DOUBLE", "WRITE", (void *) &ipd_om, &el_o1, STATUS );
      }
      if( indf_o2 != NDF__NOID ) {
         ndfStype( "_DOUBLE", indf_o2, "Data", STATUS );
         ndfMap( indf_o2, "Data", "_DOUBLE", "WRITE", (void *) &ipd_oa, &el_o2, STATUS );
      }
   } else {
      *STATUS = SAI__ERROR;
      errRep( "", "Unexpected OUTTYPE value (programming error)", STATUS );
   }

/* Check that the number of elements mapped matches the input. */
   if( el_o1 && el_o1 != el_i1 ) {
      *STATUS = SAI__ERROR;
      errRep( "", "Output NDF OUT1's size doesn't match that of the input (programming error)", STATUS );
   }
   if( el_o2 && el_o2 != el_i1 ) {
      *STATUS = SAI__ERROR;
      errRep( "", "Output NDF OUT2's size doesn't match that of the input (programming error)", STATUS );
   }

   if( *STATUS != SAI__OK ) goto L999;

/* Determine the type of calculation being performed. This allows the
   operation to be reported via a parameter and simplifies bad value handling
   in the case that default values are provided (e.g. for mod and arg). */
   if( ( ipd_om || ipd_oa ) && ! ( ipd_im || ipd_ia ) ) {
      calcmode= CALCMODE_TOPOL;
   } else if( ( ipd_or || ipd_oi ) && ! ( ipd_ir || ipd_ii ) ) {
      calcmode = CALCMODE_FROMPOL;
   }
   parPut0c( "CALCMODE", ( calcmode == CALCMODE_NONE ? "None"
             : ( calcmode == CALCMODE_TOPOL ? "To polar"
             : ( calcmode == CALCMODE_FROMPOL ? "From polar"
             : "Unknown" ) ) ), STATUS );

/* Store appropriate values in the output DATA components. */
   for( i = 0; i < el_i1; i++ ) {
      switch( calcmode ) {
         case CALCMODE_TOPOL:
            real = ipd_ir ? ipd_ir[i] : 0.0;
            imag = ipd_ii ? ipd_ii[i] : 0.0;
            if( real != VAL__BADD && imag != VAL__BADD )  {
               mod = sqrt( (real * real) + (imag * imag) );
               arg = atan2( imag, real );
            } else {
               mod = VAL__BADD;
               arg = VAL__BADD;
            }
            break;
         case CALCMODE_FROMPOL:
            mod = ipd_im ? ipd_im[i] : 1.0;
            arg = ipd_ia ? ipd_ia[i] : 0.0;
            if( mod != VAL__BADD && arg != VAL__BADD ) {
               real = mod * cos( arg );
               imag = mod * sin( arg );
            } else {
               real = VAL__BADD;
               imag = VAL__BADD;
            }
            break;
         default:
            real = ipd_ir ? ipd_ir[i] : VAL__BADD;
            imag = ipd_ii ? ipd_ii[i] : VAL__BADD;
            mod = ipd_im ? ipd_im[i] : VAL__BADD;
            arg = ipd_ia ? ipd_ia[i] : VAL__BADD;
            break;
      }

      if( ipd_or ) {
         ipd_or[i] = real;
      }
      if( ipd_oi ) {
         ipd_oi[i] = imag;
      }
      if( ipd_om ) {
         ipd_om[i] = mod;
      }
      if( ipd_oa ) {
         ipd_oa[i] = arg;
      }
   }

L999:;

/* End the NDF context */
   ndfEnd( STATUS );

/* If an error has occurred, issue another error report identifying the
   program which has failed (i.e. this one). */
   if( *STATUS != SAI__OK ) {
      errRep( "COMPLEX_ERR", "COMPLEX: Failed to convert complex "
              "representation of NDF(s).", STATUS );
   }
}
