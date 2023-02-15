/*
 *+
 *  Name:
 *     aztec_fill_smfHead

 *  Purpose:
 *     Populate smfHead with AzTEC specific information

 *  Language:
 *     ANSI C

 *  Invocation:
 *     void aztec_fill_smfHead( smfHead * hdr, int indf, int * status );

*  Arguments:
*     hdr = smfHead* (Given)
*        Pointer to the smfHead that will be filled with AzTEC information
*     indf = int (Given)
*        NDF locator for the input data file - currently ignored
*     status = int* (Given and Returned)
*        Pointer to global status.

 *  Description:
 *     This function populates a smfHead with AzTEC specific
 *     information.

 *  Authors:
 *     EC: Edward Chapin (UBC)

 *  History:
 *     6-SEPT-2006 (EC):
 *        Original version (based on acs_fill_smfHead)

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

#include "sae_par.h"
#include "mers.h"
#include "star/hds.h"
#include "ndf.h"

#include "libsmf/smf.h"
#include "smurf_par.h"
#include "aztec.h"

#define FUNC_NAME "aztec_fill_smfHead"

#define EXTENSION "AZTEC"

void aztec_fill_smfHead( smfHead * hdr, int indf __attribute__((unused)), int * status ) {

  double * fplanex = NULL; /* X coordinates in radians */
  double * fplaney = NULL; /* Y coordinates in radians */
  unsigned int i;          /* loop counter */

  /* azoff and eloff are the Azimuth and Elevation tangent plane offsets
   measured in radians for each bolometer */

  const double azoff[AZTEC_NBOLO] = {
    0.00015584515, 0.00044016678, 0.0000000, 0.00013625191,
    2.6843333e-05, -0.00013097957, -1.9124088e-05, 0.0000000,
    0.0000000, 0.00036971178, 0.00025056796, 0.00023470729,
    0.0000000, 0.0000000, -8.4848132e-05, 0.0000000,
    4.6944125e-05, 0.00030079267, 0.0000000, 0.00018390905,
    0.0000000, -6.3100381e-05, 0.0000000, -3.6014641e-05,
    0.0000000, 0.0000000, 0.00065203985, 0.0000000,
    0.00047405183, 0.00048893899, 0.0000000, 0.00034946889,
    0.00059329018, 0.00064665358, 0.00058673066, 0.0000000,
    0.00040872615, 0.0000000, 0.00042135408, 0.0000000,
    0.00066150825, 0.00052568298, 0.0000000, 0.0000000,
    0.0000000, 0.00060869755, 0.0000000, 0.00035502485,
    0.00065921024, 0.00043468014, 0.00053641190, 0.00047234286,
    0.00052814583, 0.00070891814, 0.00064816135, 0.00028889487,
    0.00058940683, 0.00036685429, 0.00041724287, 0.00023128015,
    0.00041045985, 0.00022550990, 0.00058566407, 0.00010899135,
    0.00071523041, 0.00029727293, 0.00048556953, 0.00035297409,
    0.0000000, 0.0000000, 0.00034791700, 0.0000000,
    4.3143384e-05, -0.00024616149, -2.4827673e-05, 2.0508908e-05,
    0.00013226723, 0.00031553729, 0.00019801517, 6.1844714e-06,
    8.6923618e-05, -0.00020258163, -9.1552614e-05, -0.00010628124,
    6.7810826e-05, 5.6080285e-05, 0.00024665164, 0.0000000,
    0.00015321310, -0.00015586939, -0.00013791433, -4.4229462e-05,
    -5.4786319e-05, 0.00026832036, 0.00011730832, 0.00017918600,
    -0.00053155407, -0.00058869416, 0.0000000, -0.00037771895,
    -0.00032198143, -0.00030939810, -0.00037054663, -0.00022098369,
    -0.00042796597, -0.00048400019, 0.0000000, -0.00027303954,
    -0.00027198361, -0.00011245825, -0.00026293506, 0.0000000,
    -0.00047410467, -0.00037779604, 0.0000000, -0.00032692847,
    -0.00016529513, -0.00041526580, -0.00016478753, -0.00021502630,
    -0.00046766975, -0.00023832691, 0.0000000, -0.00040664340,
    -0.00037313456, -0.00053493322, -0.00047905267, 0.0000000,
    0.0000000, -0.00019463021, 0.0000000, -9.7932756e-05,
    -0.00026609022, 0.0000000, -0.00042964536, 0.0000000,
    -0.00052604174, -0.00014839163, -0.00030101471, -0.00020724796,
    -4.7632851e-05, -0.00058126197, -0.00021738104, -0.00032317116 };

  const double eloff[AZTEC_NBOLO] = {
    -0.00078535379, -0.00067186871, 0.0000000, -0.00058146559,
    -0.00058484474, -0.00069347199, -0.00068632584, 0.0000000,
    0.0000000, -0.00057455700, -0.00057924030, -0.00038382809,
    0.0000000, 0.0000000, -0.00058864567, 0.0000000,
    -0.00078977044, -0.00047749691, 0.0000000, -0.00048195137,
    0.0000000, -0.00079570455, 0.0000000, -0.00048755727,
    0.0000000, 0.0000000, -0.00027810099, 0.0000000,
    -0.00037881269, -0.00057195840, 0.0000000, -0.00018692556,
    -0.00037519744, -8.7651807e-05, -0.00018189562, 0.0000000,
    -0.00028333648, 0.0000000, -0.00047632802, 0.0000000,
    -0.00047139311, -8.8891959e-05, 0.0000000, 0.0000000,
    0.0000000, -0.00056885075, 0.0000000, -0.00037977262,
    0.00030611059, 0.00051075558, 0.00030646353, 0.00020550021,
    9.9944731e-05, 7.6312511e-07, 9.9554941e-05, 0.00010149662,
    0.00020456355, 0.00040861028, 0.00030678739, 0.00020588127,
    0.00010033501, 4.7165443e-07, 2.9369161e-06, 1.4143456e-06,
    0.00020678108, 0.00030813177, 0.00040823213, 0.00020622743,
    0.0000000, 0.0000000, 1.5799821e-06, 0.0000000,
    0.00061445228, 0.00051194337, 0.00051403292, 0.00040974232,
    0.00040875815, 0.00051189974, 0.00051307784, 0.00020432696,
    0.00051317480, 0.00040680678, 0.00040853805, 0.00020379609,
    0.00030490728, 0.00010178169, 0.00041078176, 0.0000000,
    0.00061410806, 0.00030330982, 0.00051223911, 0.00030351393,
    0.00010064189, 0.00061331297, 0.00020631033, 0.00030642862,
    0.00020441277, -9.5773398e-05, 0.0000000, 0.00010170121,
    0.00020217536, 0.00040850459, 0.00030454658, 6.9932853e-07,
    0.00020428866, -9.5679345e-05, 0.0000000, -9.5683223e-05,
    0.00010082079, -4.4832521e-07, 0.00030382178, 0.0000000,
    0.00030362155, -9.4849829e-05, 0.0000000, 1.8799457e-06,
    -9.6008048e-05, 0.00040668606, 0.00010116840, 0.00020321625,
    -0.00050243619, -0.00069783046, 0.0000000, -0.00060241437,
    -0.00029169079, -0.00018939665, -0.00029467384, 0.0000000,
    0.0000000, -0.00059356653, 0.0000000, -0.00038990328,
    -0.00029232977, 0.0000000, -0.00018737740, 0.0000000,
    -0.00039944683, -0.00049010254, -0.00059893341, -0.00039211063,
    -0.00029023441, -0.00029600756, -0.00018811238, -0.00018732116 };

  if (*status != SAI__OK) return;

  /* allocate memory for LUTs and copy */
  if (*status == SAI__OK) {
    hdr->ndet = AZTEC_NBOLO;
    fplanex = astMalloc( AZTEC_NBOLO*sizeof(*fplanex) );
    fplaney = astMalloc( AZTEC_NBOLO*sizeof(*fplaney) );

    /* Copy using a for loop in case for some reason the type of fplane*
       is changed from double in the future */

    if (fplanex && fplaney) {
      for (i = 0; i < AZTEC_NBOLO; i++) {
	fplanex[i] = azoff[i];
	fplaney[i] = eloff[i];
      }
    }

    /* now store in the header */
    hdr->fplanex = fplanex;
    hdr->fplaney = fplaney;

  }

}
