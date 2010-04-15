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
#include "ems_par.h"
#include "ems_err.h"
#include "ems.h"

MODULE = Starlink::EMS		PACKAGE = Starlink::EMS

# Try some real EMS library calls as an experiment
# Could use a Starlink::Status object
# Use int for now

# ems_annul_c

void
emsAnnul( status )
  int status
 ALIAS:
  Starlink::EMS::ems_annul = 2
 CODE:
  emsAnnul( &status );
 OUTPUT:
  status

# ems_begin_c

void
emsBegin( status )
  int status
 ALIAS:
  Starlink::EMS::ems_begin = 2
 CODE:
  emsBegin( &status );
 OUTPUT:
  status

# ems_eload
# Return (param, opstr, status) as an array

void
emsEload()
 PREINIT:
  char param[EMS__SZPAR+1];
  int  parlen;
  char opstr[EMS__SZMSG+1];
  int  oplen;
  int  status;
 ALIAS:
  Starlink::EMS::ems_eload = 2
 PPCODE:
  emsEload(param, &parlen, opstr, &oplen, &status);
  EXTEND(sp,3);
  PUSHs(sv_2mortal( newSVpv( (char *)param, parlen ) ));
  PUSHs(sv_2mortal( newSVpv( (char *)opstr, oplen  ) ));
  PUSHs(sv_2mortal( newSViv(status) ));


# ems_end_c

void
emsEnd( status )
  int status
 ALIAS:
  Starlink::EMS::ems_end = 2
 CODE:
  emsEnd( &status );

void
emsErrno( token, errval )
  char * token
  int errval
 ALIAS:
  Starlink::EMS::ems_errno = 2
 CODE:
  emsErrno(token, errval);

# Note that we return the answer

char *
emsExpnd( msg, status )
  char * msg
  int status
 PREINIT:
  char result[EMS__SZTOK];
  int actual_length;
 CODE:
  RETVAL = result;
  emsExpnd(msg, RETVAL, EMS__SZTOK, 0, &actual_length, &status);
 OUTPUT:
  RETVAL


void
emsFacer( token, facerr )
  char * token
  int facerr
 ALIAS:
  Starlink::EMS::ems_facer = 2
 CODE:
   emsFacer(token, facerr);



# Assign formatted values to tokens - not really all that helpful
# since perl can just insert into the string

void
emsFmtc(token, format, value)
  char * token
  char * format
  char * value
 ALIAS:
  Starlink::EMS::ems_fmtc = 2
 PREINIT:
  int maxchar;
 CODE:
  Perl_croak(aTHX_ "emsFmtc no longer supported. Use sprintf instead\n");
  /* maxchar = EMS__SZTOK;
     emsFmtc(token, format, value, maxchar);*/

void
emsFmtd(token, format, value)
  char * token
  char * format
  double value
 ALIAS:
  Starlink::EMS::ems_fmtd = 2
 CODE:
  Perl_croak(aTHX_ "emsFmtd no longer supported. Use sprintf instead\n");
  /* emsFmtd(token, format, value);*/

void
emsFmti(token, format, value)
  char * token
  char * format
  int value
 ALIAS:
  Starlink::EMS::ems_fmti = 2
 CODE:
  Perl_croak(aTHX_ "emsFmti no longer supported. Use sprintf instead\n");
  /* emsFmti(token, format, value); */

void
emsFmtl(token, format, value)
  char * token
  char * format
  int value
 ALIAS:
  Starlink::EMS::ems_fmtl= 2
 CODE:
  Perl_croak(aTHX_ "emsFmtl no longer supported. Use sprintf instead\n");
  /* emsFmtl(token, format, value);*/

void
emsFmtr(token, format, value)
  char * token
  char * format
  float value
 ALIAS:
  Starlink::EMS::ems_fmtr = 2
 CODE:
  Perl_croak(aTHX_ "emsFmtr no longer supported. Use sprintf instead\n");
  /* emsFmtr(token, format, value); */


# Note that we return ems_level rather than use a variable

int
emsLevel()
 ALIAS:
  Starlink::EMS::ems_level = 2
 CODE:
  emsLevel(&RETVAL);
 OUTPUT:
  RETVAL


void
emsMark( )
 ALIAS:
  Starlink::EMS::ems_mark = 2
 CODE:
  emsMark();

char *
emsMload(param, text, status)
  char * param
  char * text
  int status
 ALIAS:
  Starlink::EMS::ems_mload = 2
 PREINIT:
  int oplen;
  char STR[256];
 CODE:
  RETVAL = STR;
  emsMload(param, text, RETVAL, &oplen, &status);
  *(RETVAL + oplen) = '\0';
 OUTPUT:
  RETVAL

void
emsRenew()
 ALIAS:
  Starlink::EMS::ems_renew = 2
 CODE:
  emsRenew();


void
emsRep( param, text, status )
  char * param
  char * text
  int status
 ALIAS:
  Starlink::EMS::ems_rep = 2
 CODE:
  emsRep(param, text, &status);
 OUTPUT:
  status



void
emsRlse( )
 ALIAS:
  Starlink::EMS::ems_rlse = 2
 CODE:
  emsRlse();


# Assign values to tokens - not really all that helpful
# since perl can just insert into the string

void
emsSetc(token, value)
  char * token
  char * value
 ALIAS:
  Starlink::EMS::ems_setc = 2
 CODE:
  emsSetc(token, value);

void
emsSetd(token, value)
  char * token
  double value
 ALIAS:
  Starlink::EMS::ems_setd = 2
 CODE:
  emsSetd(token, value);

void
emsSeti(token, value)
  char * token
  int value
 ALIAS:
  Starlink::EMS::ems_seti = 2
 CODE:
  emsSeti(token, value);

void
emsSetl(token, value)
  char * token
  int value
 ALIAS:
  Starlink::EMS::ems_setl = 2
 CODE:
  emsSetl(token, value);

void
emsSetr(token, value)
  char * token
  float value
 ALIAS:
  Starlink::EMS::ems_setr = 2
 CODE:
  emsSetr(token, value);



# Note return value

int
emsStat()
 ALIAS:
  Starlink::EMS::ems_stat = 2
 CODE:
  emsStat( &RETVAL );
 OUTPUT:
  RETVAL


void
emsSyser( token, systat )
  char * token
  int systat
 ALIAS:
  Starlink::EMS::ems_syser = 2
 CODE:
  emsSyser(token, systat);

void
emsTune( key, value, status )
  char * key
  int value
  int &status
 ALIAS:
  Starlink::EMS::ems_tune = 2
 OUTPUT:
  status


# Get facility error information

void
ems1_get_facility_error(inmsg_status, facility_name, error_ident, error_text)
  unsigned int inmsg_status
  char * facility_name = NO_INIT
  char * error_ident = NO_INIT
  char * error_text  = NO_INIT
 PREINIT:
  char facility[EMS__SZPAR+1], ident[EMS__SZPAR+1], text[EMS__SZMSG+1];
 CODE:
  /* copy pointer value into the arguments */
  facility_name = facility;
  error_ident = ident;
  error_text  = text;
  ems1_get_facility_error(inmsg_status, &facility_name,
			  &error_ident, &error_text);
 OUTPUT:
  facility_name
  error_ident
  error_text

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



int
EMS__NOMSG()
 CODE:
  RETVAL = EMS__NOMSG;
 OUTPUT:
  RETVAL
