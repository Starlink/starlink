#include <string.h>
#include <stdio.h>
#include <ctype.h>

// SMURF includes
#include "fts2.h"
#include "libsmf/smf.h"

/* Gets the mirror position when in STEPINT mode, [mm]. */
double fts2_getMirrorPosition(smfData* data, int* status)
{
  double position = 0.0;
  smf_fits_getD(data->hdr, "MIRPOS", &position, status);
  return position;
}
