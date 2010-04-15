/*
*+
*  Name:
*     smf_get_gridcoords

*  Purpose:
*     Get native (GRID) bolometer coordinates for SCUBA2 subarray

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_get_gridcoords ( double *row, double *col, int *status )

*  Arguments:
*     row = double* (Returned)
*        Row GRID coords of bolometers
*     col = double* (Returned)
*        Column GRID coords of bolometers
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Calculate the native GRID coordinates of each bolometer in the
*     subarray, in the same order they are stored in memory / identified
*     in framesets created by sc2ast_createwcs

*  Authors:
*     E.Chapin (UBC)
*     {enter_new_authors_here}

*  History :
*     2006-02-28 (EC):
*        Original
*     2006-07-20 (JB):
*        Split from dsim.c
*     2006-08-15 (EC):
*        - Fixed off-by-one errors
*        - Renamed to sc2ast_get_gridcoords from sc2sim_bolnatcoords
*     2006-08-17 (EC):
*        - Added nrow/ncol arguments
*        - Renamed to smf_get_gridcoords from sc2ast_get_gridcoords

*/

/* Starlink includes */
#include "sae_par.h"

/* SMURF includes */
#include "smf.h"

#define FUNC_NAME "smf_get_gridcoords"

void smf_get_gridcoords( double *row, double *col, int nrow, int ncol,
			 int *status ) {
  /* Local variables */
  int i;           /* row counter */
  int j;           /* column counter */
  int bol;         /* bolometer counter */

  /* Check status */
  if ( *status != SAI__OK ) return;

  /* Get the grid coordinates */
  bol = 0;
  for( j=1; j<=nrow; j++ ) {
    for( i=1; i<=ncol; i++ ) {
      row[bol] = (double)i;
      col[bol] = (double)j;
      bol++;
    }
  }
}
