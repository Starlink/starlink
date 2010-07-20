#include <string.h>
#include <stdio.h>
#include <ctype.h>

// SMURF includes
#include "fts2.h"
#include "libsmf/smf.h"

/* Gets the mirror scan rate in FSCAN mode, [mm/s]. */
double fts2_getScanVelocity(smfData* data, int* status)
{
  double scanVel = 0.0;
  smf_fits_getD(data->hdr, "SCANVEL", &scanVel, status);
  return scanVel;
}
