#include <string.h>
#include <stdio.h>
#include <ctype.h>

// STARLINK includes
#include "ast.h"
#include "sae_par.h"

// SMURF includes
#include "fts2.h"
#include "libsmf/smf.h"

double* fts2_getPositions(smfData* data, int* status)
{
  double* positions = NULL;
  size_t count;
  HDSLoc* hdsLoc = smf_get_xloc(data, "JCMTSTATE", "EXT", "READ", 0, 0, status);
  HDSLoc* hdsLocPosition = NULL;
  datFind(hdsLoc, "FTS_POS", &hdsLocPosition, status);
  datSize(hdsLocPosition, &count, status);
  positions = (double*) astMalloc(count * sizeof(double));
  float* tmp = (float*) astMalloc(count * sizeof(float));
  datGetVR(hdsLocPosition, count, tmp, &count, status);
  if(*status == SAI__OK)
  {
    for(int i = 0; i < (int) count; i++)
    {
	  positions[i] = (double) tmp[i];
    }
  }
  // FREE RESOURCES
  astFree(tmp);
  datAnnul(&hdsLoc, status);
  datAnnul(&hdsLocPosition, status);
  return positions;
}
