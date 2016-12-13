/*
*+
*  Name:
*     smf_deskpike_wvm

*  Purpose:
*     Given an array of WVM-derived opacities, attempt to remove spikes.

*  Language:
*     C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_despike_wvm(double* data, int n, int width, double tol, int* status);

*  Arguments:
*     data = double* (Given)
*        Pointer to data array (which is to be modified).
*     n = int (Given)
*        Length of data array.
*     width = int (Given)
*        Width of despiking box.
*     tol = double (Given)
*        Fractional tolerance.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Attempts to remove spikes from an array of WVM-derived opacities.  These
*     are identified as those points more than a fractional tolerance, "tol",
*     away from the median of a moving box of points of width "width".
*
*     Points determined to comprise spikes are replaced with the bad value.

*  Authors:
*     GSB: Graham Bell (EAO)
*     DSB: David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     30-JAN-2016 (GSB):
*        Original version.
*     8-DEC-2016 (DSB):
*        - Add Authors and History sections to prologue.
*        - Check data is not bad before comparing it to the median. This
*        should not alter the results of this routine, but helps to avoid
*        unneccesary floating point exceptions when using feenableexcept to
*        track down other NaNs.

*  Copyright:
*     Copyright (C) 2016 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA
*-
*/

#include "sae_par.h"
#include "mers.h"

#include "smf.h"
#include "smurf_par.h"

#include <math.h>
#include <gsl/gsl_sort.h>

int smf_despike_wvm(double* data, int n, int width, double tol, int* status) {
    int i, j;
    int removed = 0;
    double* box;
    int prev_box_start = -1;
    int box_start;
    int box_end;
    int max_box_start;
    int box_slop;
    int box_n;
    int half_width;
    double median = 0;

    if (*status != SAI__OK) {
        return 0;
    }

    /* Ensure box width is odd (so that it's got a simple median). */
    width |= 1;

    /* Check box isn't wider than data array. */
    if (width > n) {
        return 0;
    }

    /* Allocate temporary array for box in which we will calculate median. */
    box = astMalloc(width * sizeof(*box));
    if (!box) {
        return 0;
    }

    /* Pre-calculate furthest position through the array we could place
       the box and other box parameters. */
    max_box_start = n - width;
    half_width = width / 2;
    box_slop = width / 10;

    for (i = 0; i < n; i ++) {
        /* Determine box position in data array. */
        box_start = i - half_width;
        if (box_start < 0) {
            box_start = 0;
        }
        if (box_start > max_box_start) {
            box_start = max_box_start;
        }

        /* If the box moved significantly, recalculate the median. */
        if ((prev_box_start == -1)
                || (abs(box_start - prev_box_start) > box_slop)) {
            prev_box_start = box_start;
            box_end = box_start + width;
            box_n = 0;
            for (j = box_start; j < box_end; j++) {
                if (data[j] != VAL__BADD) {
                    box[box_n ++] = data[j];
                }
            }

            /* Check we've got a resonable amount of data. */
            if (box_n > half_width) {
                /* (If there were an odd number of bad values, box_n may be
                    even, in which case this isn't the true median.) */
                gsl_sort(box, 1, box_n);
                median = box[box_n / 2];
            }
            else {
                median = 0;
            }
        }


        /* If we've got a valid median, compare the value to it. */
        if (median && data[i] != VAL__BADD) {
            if (fabs(data[i] - median) / median > tol) {
                data[i] = VAL__BADD;
                removed ++;
            }
        }
    }

    /* Free the temporary array and return the number of points removed. */
    astFree(box);

    return removed;
}
