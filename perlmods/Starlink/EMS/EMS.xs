#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#ifdef __cplusplus
}
#endif

/* SAI defines -- more for testing than anything else */

#include "sae_par.h"


/* Pre-allocated space */

#define FCHAR 512       /* Size of Fortran character string */
static char str1[FCHAR];
static char str2[FCHAR];
static char str3[FCHAR];


MODULE = Starlink::EMS		PACKAGE = Starlink::EMS		


# Main routine

void
ems1_get_facility_error(inmsg_status, facility, ident, text)
  int inmsg_status
  char * facility = NO_INIT
  char * ident = NO_INIT
  char * text  = NO_INIT
 CODE:
  facility = str1;
  ident = str2;
  text  = str3;
  ems1_get_facility_error(inmsg_status, &facility, &ident, &text);
 OUTPUT:
  facility
  ident
  text

# Simple error codes so that the conversion facility can be tested

int
SAI__OK()
 CODE:
  RETVAL = SAI__OK;
 OUTPUT:
  RETVAL

int
SAI__ERROR()
 CODE:
  RETVAL = SAI__ERROR;
 OUTPUT:
  RETVAL

int
SAI__WARN()
 CODE:
  RETVAL = SAI__WARN;
 OUTPUT:
  RETVAL


  
