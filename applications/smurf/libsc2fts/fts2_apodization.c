/*
*+
*  Name:
*     fts2_apodization.c

*  Purpose:
*     Provides support for common apodization functions

*  Language:
*     Starlink ANSI C

*  Type of Module:

*  Invocation:
*     fts2_apodization(signal, size, a, b, window, apodization, status);

*  Description:
*     Provides support for common apodization functions.

*  Arguments:
*     signal = double* (Given and Returned)
*        Pointer to the signal to be apodized.
*     size = int (Given)
*       Signal array length
*     a = double (Given)
*        Lower bound of the interval, must be in [-1, 1] interval.
*     b = double (Given)
*       Upper bound of the interval, must be in [-1, 1] interval.
*     window = double* (Given and Returned)
*        Pointer to window function
*     apodization = int (Given)
*       Apodization method, see smf_fts2apodizationmethod for supported methods.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Authors:
*     Coskun OBA (UoL)

*  History :
*     2011-09-26 (COBA):
*        Original version.

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
*     Copyright (C) 2010 University of Lethbridge. All Rights Reserved.

*  License:
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

*  Bugs:
*     {note_any_bugs_here}
*-
*/

// STANDARD INCLUDES
#include <math.h>

// STARLINK INCLUDES
#include "sae_par.h"

// SMURF INCLUDES
#include "fts2.h"

void fts2_apodization(
    double* signal,     // The signal to be apodized, modified and returned
    int size,           // Length of the signal
    double a,           // Lower limit of the interval
    double b,           // Upper limit of the interval
    double* window,     // The window function, returned
    int apodization,    // Apodization method, see smf_fts2apodizationmethod
    int* status)
{
  if(*status != SAI__OK) { return; }

  int i     = 0;
  double x  = 0.0;
  double dx = 0.0;
  double t  = 0.0;

  // Ensure that the interval is valid, if NOT, return.
  a = (a < b) ? a : b;
  b = (a < b) ? b : a;
  if(a == b || fabs(a) > 1 || fabs(b) > 1) {
    *status = SAI__ERROR;
    return;
  }

  x = a;
  dx = fabs(b - a) / (size - 1);
  switch(apodization) {
    case SMF__FTS2_APODIZATION_NONE:
      break;
    case SMF__FTS2_APODIZATION_GAUSS:
      for(i = 0; i < size; i++) {
        window[i] = exp(-pow(x, 2.0));
        signal[i] *= window[i];
        x += dx;
      }
      break;
    case SMF__FTS2_APODIZATION_NB11:
      for(i = 0; i < size; i++) {
        t = 1.0 - pow(x, 2.0);
        window[i] = 0.701551 - 0.639244 * t + 0.937693 * pow(t, 2.0);
        signal[i] *= window[i];
        x += dx;
      }
      break;
    case SMF__FTS2_APODIZATION_NB12:
      for(i = 0; i < size; i++) {
        t = 1.0 - pow(x, 2.0);
        window[i] = 0.396430 - 0.150902 * t + 0.754472 * pow(t, 2.0);
        signal[i] *= window[i];
        x += dx;
      }
      break;
    case SMF__FTS2_APODIZATION_NB13:
      for(i = 0; i < size; i++) {
        t = 1.0 - pow(x, 2.0);
        window[i] = 0.237413 - 0.065285 * t + 0.827872 * pow(t, 2.0);
        signal[i] *= window[i];
        x += dx;
      }
      break;
    case SMF__FTS2_APODIZATION_NB14:
      for(i = 0; i < size; i++) {
        t = 1.0 - pow(x, 2.0);
        window[i] = 0.153945 - 0.141765 * t + 0.987820 * pow(t, 2.0);
        signal[i] *= window[i];
        x += dx;
      }
      break;
    case SMF__FTS2_APODIZATION_NB15:
      for(i = 0; i < size; i++) {
        t = 1.0 - pow(x, 2.0);
        window[i] = 0.077112 + 0.703371 * pow(t, 2.0) + 0.219517 * pow(t, 4.0);
        signal[i] *= window[i];
        x += dx;
      }
      break;
    case SMF__FTS2_APODIZATION_NB16:
      for(i = 0; i < size; i++) {
        t = 1.0 - pow(x, 2.0);
        window[i] = 0.039234 + 0.630268 * pow(t, 2.0) + 0.234934 * pow(t, 4.0) + 0.095563 * pow(t, 6.0);
        signal[i] *= window[i];
        x += dx;
      }
      break;
    case SMF__FTS2_APODIZATION_NB17:
      for(i = 0; i < size; i++) {
        t = 1.0 - pow(x, 2.0);
        window[i] = 0.020078 + 0.48667 * pow(t, 2.0) + 0.386409 * pow(t, 4.0) + 0.112845 * pow(t, 6.0);
        signal[i] *= window[i];
        x += dx;
      }
      break;
    case SMF__FTS2_APODIZATION_NB18:
      for(i = 0; i < size; i++) {
        t = 1.0 - pow(x, 2.0);
        window[i] = 0.010172 + 0.344429 * pow(t, 2.0) + 0.451817 * pow(t, 4.0) + 0.193580 * pow(t, 6.0);
        signal[i] *= window[i];
        x += dx;
      }
      break;
    case SMF__FTS2_APODIZATION_NB19:
      for(i = 0; i < size; i++) {
        t = 1.0 - pow(x, 2.0);
        window[i] = 0.004773 + 0.232473 * pow(t, 2.0) + 0.464562 * pow(t, 4.0) + 0.298191 * pow(t, 6.0);
        signal[i] *= window[i];
        x += dx;
      }
      break;
    case SMF__FTS2_APODIZATION_NB20:
      for(i = 0; i < size; i++) {
        t = 1.0 - pow(x, 2.0);
        window[i] = 0.002267 + 0.140412 * pow(t, 2.0) + 0.487172 * pow(t, 4.0) + 0.256200 * pow(t, 6.0) + 0.113948 * pow(t, 8.0);
        signal[i] *= window[i];
        x += dx;
      }
      break;
    default:
      *status = SAI__ERROR; // UNKNOWN APODIZATION METHOD
  }
}
