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

/* Add my own prototype */
void ems1_get_facility_error( unsigned int errcode, char **facility_name,
			      char **error_ident, char **error_text);


MODULE = Starlink::EMS		PACKAGE = Starlink::EMS		

# Try some real EMS library calls as an experiment
# Could use a Starlink::Status object
# Use int for now

# ems_annul_c

void
ems_annul( status )
  int status
 CODE:
  ems_annul_c( &status );
 OUTPUT:
  status

# ems_begin_c

void
ems_begin( status )
  int status
 CODE:
  ems_begin_c( &status );
 OUTPUT:
  status

# ems_eload
# Return (param, opstr, status) as an array

void
ems_eload()
 PREINIT:
  char param[EMS__SZPAR+1];
  int  parlen;
  char opstr[EMS__SZMSG+1];
  int  oplen;
  int  status;
 PPCODE:
  ems_eload_c(param, &parlen, opstr, &oplen, &status);
  EXTEND(sp,3);
  PUSHs(sv_2mortal( newSVpv( (char *)param, parlen ) ));
  PUSHs(sv_2mortal( newSVpv( (char *)opstr, oplen  ) ));
  PUSHs(sv_2mortal( newSViv(status) ));


# ems_end_c

void
ems_end( status )
  int status
 CODE:
  ems_end_c( &status );

void
ems_errno( token, errval )
  char * token
  int errval
 CODE:
  ems_errno_c(token, errval);

void
ems_facer( token, facerr )
  char * token
  int facerr
 CODE:
   ems_facer_c(token, facerr);



# Assign formatted values to tokens - not really all that helpful
# since perl can just insert into the string

void
ems_fmtc(token, format, value)
  char * token
  char * format
  char * value
 PREINIT:
  int maxchar;
 CODE:
  maxchar = EMS__SZTOK;
  ems_fmtc_c(token, format, value, maxchar);

void
ems_fmtd(token, format, value)
  char * token
  char * format
  double value
 CODE:
  ems_fmtd_c(token, format, value);

void
ems_fmti(token, format, value)
  char * token
  char * format
  int value
 CODE:
  ems_fmti_c(token, format, value);

void
ems_fmtl(token, format, value)
  char * token
  char * format
  int value
 CODE:
  ems_fmtl_c(token, format, value);

void
ems_fmtr(token, format, value)
  char * token
  char * format
  float value
 CODE:
  ems_fmtr_c(token, format, value);


# Note that we return ems_level rather than use a variable

int
ems_level()
 CODE:
  ems_level_c(&RETVAL);
 OUTPUT:
  RETVAL


void
ems_mark( )
 CODE:
  ems_mark_c();

char *
ems_mload(param, text, status)
  char * param
  char * text
  int status
 PREINIT:
  int oplen;
 CODE:
  RETVAL = malloc(256);
  ems_mload_c(param, text, RETVAL, &oplen, &status);
  *(RETVAL + oplen) = '\0';
 OUTPUT:
  RETVAL

void
ems_renew()
 CODE:
  ems_renew_c();


void
ems_rep( param, text, status )
  char * param
  char * text
  int status
 CODE:
  ems_rep_c(param, text, &status);
 OUTPUT:
  status



void
ems_rlse( )
 CODE:
  ems_rlse_c();


# Assign values to tokens - not really all that helpful
# since perl can just insert into the string

void
ems_setc(token, value)
  char * token
  char * value
 PREINIT:
  int maxchar;
 CODE:
  maxchar = EMS__SZTOK;
  ems_setc_c(token, value, maxchar);

void
ems_setd(token, value)
  char * token
  double value
 CODE:
  ems_setd_c(token, value);

void
ems_seti(token, value)
  char * token
  int value
 CODE:
  ems_seti_c(token, value);

void
ems_setl(token, value)
  char * token
  int value
 CODE:
  ems_setl_c(token, value);

void
ems_setr(token, value)
  char * token
  float value
 CODE:
  ems_setr_c(token, value);



# Note return value

int
ems_stat()
 CODE:
  ems_stat_c( &RETVAL );
 OUTPUT:
  RETVAL


void
ems_syser( token, systat )
  char * token
  int systat
 CODE:
  ems_syser_c(token, systat);


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
