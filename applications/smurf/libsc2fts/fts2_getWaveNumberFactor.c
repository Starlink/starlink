#include <string.h>
#include <stdio.h>
#include <ctype.h>

// SMURF includes
#include "fts2.h"
#include "libsmf/smf.h"

/* Gets the wave number factor. */
double fts2_getWaveNumberFactor(smfData* data, int* status)
{
  double wnFactor = 0.0;
  HDSLoc* hdsLoc = smf_get_xloc(data, "FTS2DR", "EXT", "READ", 0, 0, status);
  HDSLoc* hdsLocFactor = NULL;
  datFind(hdsLoc, "FTS_WN_FACTOR", &hdsLocFactor, status);
  datGet0D(hdsLocFactor, &wnFactor, status);
  // FREE RESOURCES
  datAnnul(&hdsLocFactor, status);
  datAnnul(&hdsLoc, status);
  return wnFactor;
}
