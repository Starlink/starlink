#include <string.h>                     /* String stuff from RTL */
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#include "asterix.h"                    /* Asterix definitions */

#include "aditypes.h"
#include "adimem.h"
#include "adikrnl.h"                    /* Internal ADI kernel */
#include "adimem.h"                     /* Allocation routines */
#include "adistrng.h"
#include "adicface.h"
#include "adiparse.h"
#include "adipkg.h"                   	/* Prototypes for this sub-package */

#include "adi_err.h"                    /* ADI error codes */

ADIobj		ADI_G_pkglist = ADI__nullid;
char		*ADI_G_ldpath = NULL;
ADIboolean	ADI_G_getenv = ADI__false;

void ADIpkgRequire( char *name, int nlen, ADIstatus status )
  {
  ADIobj	pname;

  if ( ! ADI_G_getenv ) {		/* Not got ADI_LOAD_PATH yet */
    ADI_G_ldpath =
	getenv( "ADI_LOAD_PATH" );

    ADI_G_getenv = ADI__true;
    }

  pname = adix_cmn( name, nlen, 	/* Locate name in common table */
		    status );
  }
