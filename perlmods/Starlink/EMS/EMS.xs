#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#ifdef __cplusplus
}
#endif

/* Pre-allocated space */

#define FCHAR 512       /* Size of Fortran character string */
static char str1[FCHAR];
static char str2[FCHAR];
static char str3[FCHAR];



static int
not_here(s)
char *s;
{
    croak("%s not implemented on this architecture", s);
    return -1;
}

static double
constant(name, arg)
char *name;
int arg;
{
    errno = 0;
    switch (*name) {
    }
    errno = EINVAL;
    return 0;

not_there:
    errno = ENOENT;
    return 0;
}


MODULE = Starlink::EMS		PACKAGE = Starlink::EMS		


double
constant(name,arg)
	char *		name
	int		arg


void
ems1_get_facility_error(inmsg_status, facility, ident, text)
  int inmsg_status
  char * facility = NO_INIT
  char * ident = NO_INIT
  char * text  = NO_INIT
 CODE:
  facility = str1;
  ident = str1;
  text  = str1;
  ems1_get_facility_error(inmsg_status, &facility, &ident, &text);
 OUTPUT:
  facility
  ident
  text

  
