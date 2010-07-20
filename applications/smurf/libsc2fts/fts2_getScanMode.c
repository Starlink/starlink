#include <string.h>
#include <stdio.h>
#include <ctype.h>

// SMURF includes
#include "fts2.h"
#include "libsmf/smf.h"

/* Gets the FTS-2 scanning mode. */
FTSMode fts2_getScanMode(smfData* data, int* status)
{
  int mode;
  char ftsMode[SZFITSCARD+1];
  smf_fits_getS(data->hdr, "FTS_MODE", ftsMode, sizeof(ftsMode), status);
  if(strncmp(ftsMode, "FSCAN", 5) == 0 )
  {
	  mode = FSCAN;
  }
  else if(strncmp(ftsMode, "STEPINT", 7) == 0)
  {
	  mode = STEPINT;
  }
  else
  {
    mode = UNKNOWN;
  }
  return mode;
}
