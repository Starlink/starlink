/*
*  Name:
*     grf_perl.c

*  Purpose:
*     Implement the grf module for the Perl interface

*  Description:
*     This file implements the low level graphics functions required
*     by the rest of AST, by reporting errors when called.
*
*     For the Perl interface, each of the standard GRF callbacks
*     is forwarded on to Perl callback. The call stack is adjusted
*     depending on which routine has been called.

*  Inheritance:
*     This module is not a class and does not inherit.

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils
*     Perl code Copyright (C) 2004 Particle Physics and Research Council.
*     All Rights Reserved.

*  Authors:
*     DSB: David S. Berry (Starlink)
*     TIMJ: Tim Jenness (JAC)

*  History:
*     23-OCT-1996 (DSB):
*        Original version.
*     13-NOV-1996 (DSB):
*        Modified to issue error messages using astError instead of printf.
*     25-FEB-2004 (TIMJ):
*        Convert from grf_null.c to grf_perl.c
*        Do all the hard stuff.
*/

/* Header files */
/* ============ */
#include "ast.h"
#include "grf.h"           /* Declare the functions in this module */

#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"     /* XSUB include */
#include "ppport.h"
#ifdef __cplusplus
}
#endif

#include "arrays.h"

/* Have one global hash that contains the SV* reference to the CV */

static SV * CurrentPlot;

/* Function Prototypes */
/* =================== */
static void Report( const char * );

/* Function definitions */
/* ==================== */

void Perl_storeGrfObject ( SV * plotobject ) {
  CurrentPlot = plotobject;
}

void Perl_clearGrfObject() {
  CurrentPlot = NULL;
}

/* An internal hash attribute name 
   return the relevant CVREF that can be called. Uses the global static
   object. Returns NULL if no callback is registered.
*/

SV* Perl_getcb ( char * attr ) {
  SV** elem;

  if (!astOK) return NULL;

  if (CurrentPlot == NULL ) {
    astError( AST__GRFER, 
	      "Massive internal inconsistency in AstPlot Grf infrastructure");
    return NULL;
  }
 
  /* we know this is already a hash ref */
  HV * hash_object = (HV*) SvRV( CurrentPlot );

  elem = hv_fetch( hash_object, attr, strlen(attr), 0);

  if (elem == NULL || !SvOK(*elem) ) {
    return NULL;
  } else {
    return *elem;
  }
}

int astGFlush( void ){
  dSP;
  SV * cb;
  AV * XX;
  AV * YY;
  int retval;

  if (!astOK) return 0;
  if (CurrentPlot == NULL ) {
    astError( AST__GRFER, "No Plot object stored. Should not happen." );
    return 0;
  }

  cb = Perl_getcb( "_gflush" );
 
  if (astOK) {
    if ( cb != NULL ) {
      int count;
      ENTER;
      SAVETMPS;

      count = perl_call_sv( SvRV(cb), G_NOARGS | G_SCALAR );

      SPAGAIN;

      if (astOK) {
	if (count != 1) {
	  astError( AST__GRFER, 
		    "Returned more than 1 arg from GFlush callback");
	  retval = 0;
	} else {
	  retval = POPi;
	}
      } else {
	retval = 0;
      }

      PUTBACK;

      FREETMPS;
      LEAVE;
    } else {
      retval = 0;
      Report("astGFlush");
    }
  } else {
    retval = 0;
  }
  return retval;
}

int astGLine( int n, const float *x, const float *y ){
  dSP;
  SV * cb;
  AV * XX;
  AV * YY;
  int retval;

  printf("Calling astGLine with %d points\n", n);
  if (n == 0 ) return 1;

  if (!astOK) return 0;
  if (CurrentPlot == NULL ) {
    astError( AST__GRFER, "No Plot object stored. Should not happen." );
    return 0;
  }

  cb = Perl_getcb( "_gline" );
 
  if (astOK) {
    if ( cb != NULL ) {
      int count;
      ENTER;
      SAVETMPS;

      PUSHMARK(sp);
    
      /* unpack is now reverse to XS norm */
      XX = newAV();
      unpack1D( newRV_noinc((SV*) XX), (float *)x, 'f', n);
      YY = newAV();
      unpack1D( newRV_noinc((SV*) YY), (float *)y, 'f', n);
    
      XPUSHs( sv_2mortal(newRV_noinc((SV*) XX )));
      XPUSHs( sv_2mortal(newRV_noinc((SV*) YY )));
    
      PUTBACK;

      count = perl_call_sv( SvRV(cb), G_SCALAR );

      SPAGAIN;

      if (astOK) {
	if (count != 1) {
	  astError( AST__GRFER, 
		    "Returned more than 1 arg from GLine callback");
	  retval = 0;
	} else {
	  retval = POPi;
	}
      } else {
	retval = 0;
      }

      PUTBACK;

      FREETMPS;
      LEAVE;
    } else {
      retval = 0;
      Report("astGLine");
    }
  } else {
    retval = 0;
  }
  return retval;
}

int astGQch( float *chv, float *chh ){
   Report( "astGQch" );
   return 0;
}

int astGMark( int n, const float *x, const float *y, int type ){
   Report( "astGMark" );
   return 0;
}

int astGText( const char *text, float x, float y, const char *just,
              float upx, float upy ){
   Report( "astGText" );
   return 0;
}               

int astGTxExt( const char *text, float x, float y, const char *just,
               float upx, float upy, float *xb, float *yb ){
   Report( "astGTxExt" );
   return 0;
}               

int astGAttr( int attr, double value, double *old_value, int prim ){
  dSP;
  SV * cb;
  int retval;
  double cache;

  if (!astOK) return 0;
  if (CurrentPlot == NULL ) {
    astError( AST__GRFER, "No Plot object stored. Should not happen." );
    return 0;
  }

  cb = Perl_getcb( "_gattr" );
 
  if (astOK) {
    if ( cb != NULL ) {
      int count;
      ENTER;
      SAVETMPS;

      PUSHMARK(sp);
    
      XPUSHs( sv_2mortal(newSViv(attr) ) );
      XPUSHs( sv_2mortal(newSVnv(value) ) );
      XPUSHs( sv_2mortal(newSViv(prim) ) );
    
      PUTBACK;

      count = perl_call_sv( SvRV(cb), G_ARRAY );

      SPAGAIN;

      if (astOK) {
	if (count != 2) {
	  astError( AST__GRFER,
		    "Must return 2 args from GAttr callback not %d",count);
	  retval = 0;
	} else {
	  /* The status will be on the stack furthest back so we 
	     need to read off old_val first */
	  cache = POPn;
	  if (old_value != NULL) *old_value = cache;
	  retval = POPi;
	}
      } else {
	retval = 0;
      }

      PUTBACK;

      FREETMPS;
      LEAVE;
    } else {
      retval = 0;
      Report("astGAttr");
    }
  } else {
    retval = 0;
  }
  return retval;
}

static void Report( const char *name ){
   astError( AST__GRFER, "%s: No graphics facilities are available.", name );
   astError( AST__GRFER, "Register one using eg Starlink::AST::PGPLOT "
             " ->pgplot method." );
}
