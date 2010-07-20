

#include <string.h>
#include <stdio.h>
#include <ctype.h>

// STARLINK includes
#include "ast.h"

// SMURF includes
#include "fts2.h"

/* Determines whether the FTS-2 is in the beam. */
bool fts2_isInBeam(smfData* data, int* status)
{
  char inbeam[SZFITSCARD+1];
  smf_fits_getS(data->hdr, "INBEAM", inbeam, sizeof(inbeam), status);

  char* upperCase = NULL;
  upperCase = inbeam;
  for(upperCase = inbeam; *upperCase; upperCase++)
  {
    *upperCase = toupper(*upperCase);
  }

  if(strncmp(upperCase, "FTS", 3) == 0 )
  {
    return TRUE;
  }
  else
  {
    return FALSE;
  }
}
