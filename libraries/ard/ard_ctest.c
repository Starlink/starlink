/*
 *+
 *  Name:
 *    ard_ctest

 *  Purpose:
 *    ARD installation test program

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of
 *     the License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful,but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE. See the GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  Authors:
 *     PWD: Peter W. Draper (JAC, Durham University)

 *  History:
 *     07-JUL-2006 (PWD):
 *        Original version.
 *     {enter_new_history_here}

 *-
 */
#include <stdio.h>
#include <stdlib.h>
#include "sae_par.h"
#include "prm_par.h"
#include "ard.h"
#include "star/grp.h"
#include "f77.h"

#define NDIM 2

int main( int argc, char *argv[] )
{
    Grp *grp;
    float c[NDIM+1*NDIM];
    int index;
    int *maskPtr;
    int el;
    int flag;
    int i;
    int lbnd[NDIM];
    int lbnde[NDIM];
    int lbndi[NDIM];
    int status;
    int ubnd[NDIM];
    int ubnde[NDIM];
    int ubndi[NDIM];

    lbnd[0] = 1;
    lbnd[1] = 1;
    ubnd[0] = 40;
    ubnd[1] = 40;

    /*  Initialise the inherited status. */
    status = SAI__OK;

    /*  Work out the number of pixels in the mask. */
    el = 1;
    for ( i = 0; i < NDIM; i++ ) {
        el *= ubnd[i] - lbnd[i] + 1;
    }

    /*  Get space to hold the mask. */
    maskPtr = (int *) cnfMalloc( el * sizeof( int ) );

    /*  Indicate that a unit application transformation is to be used. */
    c[0] = VAL__BADR;

    /*  Store the ARD description. */
    grp = NULL;
    ardGrpex( "CIR(0,0,20)OFF(10,0)CIR(0,0,20)", NULL, &grp, &flag, &status );

    /*  Create the mask. */
    index = 2;
    ardWork( grp, NDIM, lbnd, ubnd, c, 0, &index, maskPtr,
             lbndi, ubndi, lbnde, ubnde, &status );

    /*  Check the returned index is correct. */
    if ( index != 4 ) {
        printf( " ARD installation test failed...\n" );
        printf( " Region index returned by ARD_WORK was %d\n", index );
    }
    /*  Check the retuned bounding boxes are correct. */
    else if ( lbndi[0] != 1 || ubndi[0] != 30 ||
              lbndi[1] != 1 || ubndi[1] != 20 ||
              lbnde[0] != 1 || ubnde[0] != 40 ||
              lbnde[1] != 1 || ubnde[1] != 40 ) {

        /*  If the test has failed, display the bounding boxes. */
        printf( " ARD installation test failed...\n" );
        printf( " Internal bounding box:\n" );

        if ( lbndi[0] > ubndi[0] ) {
            printf( "... null\n" );
        }
        else {
            for ( i = 0; i < NDIM; i++ ) {
                printf( "  %d; %d:%d\n", i+1, lbndi[i], ubndi[i] );
            }
        }
        printf( "\n External bounding box:\n" );

        if ( lbnde[0] > ubnde[0] ) {
            printf( " ... NULL\n" );
        }
        else {
            for ( i = 0; i < NDIM; i++ ) {
                printf( "  %d; %d:%d\n", i+1, lbnde[i], ubnde[i] );
            }
        }
    }
    /* Otherwise, indicate that the test has been passed. */
    else {
        printf( " ARD installation test succeeded\n" );
    }

    /*  Delete the GRP group. */
    grpDelet( &grp, &status );

    /*  Free the mask. */
    cnfFree( maskPtr );

    return 0;
}
