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
*     Perl code Copyright (C) 2004-2005 Particle Physics and Research Council.
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

#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"     /* XSUB include */
  /* #include "ppport.h" */
#ifdef __cplusplus
}
#endif

#include "astTypemap.h"
#include "arrays.h"
#include "grf.h"           /* Declare the functions in this module */

static SV * Perl_getPlotAttr ( const char * attr );

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

static SV * Perl_getPlotAttr ( const char * attr ) {
  if (!astOK) return NULL;

  if (CurrentPlot == NULL ) {
    astError( AST__GRFER,
	      "Massive internal inconsistency in AstPlot Grf infrastructure");
    return NULL;
  }

  return getPerlObjectAttr( CurrentPlot, attr );
}

int astGFlush( void ){
  dSP;
  SV * cb;
  SV * external;
  int retval;

  if (!astOK) return 0;
  if (CurrentPlot == NULL ) {
    astError( AST__GRFER, "No Plot object stored. Should not happen." );
    return 0;
  }

  cb = Perl_getPlotAttr( "_gflush" );

  if (astOK) {
    if ( cb != NULL ) {
      int count;
      int flags = G_SCALAR | G_EVAL;
      ENTER;
      SAVETMPS;

      /* Always need PUSHMARK/PUTBACK even if no args */
      PUSHMARK(sp);

      /* If we have a registered external object, push that on as
	 a first argument. */
      external = Perl_getPlotAttr( "_gexternal" );

      if ( external != NULL ) {
	XPUSHs( external );
      } else {
	/* No arguments */
	flags |= G_NOARGS;
      }

      PUTBACK;

      count = perl_call_sv( SvRV(cb), flags);

      retval = ReportPerlError( AST__GRFER );

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
  SV * external;
  int retval;

  if (n == 0 ) return 1;

  if (!astOK) return 0;
  if (CurrentPlot == NULL ) {
    astError( AST__GRFER, "No Plot object stored. Should not happen." );
    return 0;
  }

  cb = Perl_getPlotAttr( "_gline" );

  if (astOK) {
    if ( cb != NULL ) {
      int count;
      ENTER;
      SAVETMPS;

      PUSHMARK(sp);

      /* If we have a registered external object, push that on as
	 a first argument. */
      external = Perl_getPlotAttr( "_gexternal" );
      if ( external != NULL ) {
	XPUSHs( external );
      }

      /* unpack is now reverse to XS norm */
      XX = newAV();
      unpack1D( newRV_noinc((SV*) XX), (float *)x, 'f', n);
      YY = newAV();
      unpack1D( newRV_noinc((SV*) YY), (float *)y, 'f', n);

      XPUSHs( sv_2mortal(newRV_noinc((SV*) XX )));
      XPUSHs( sv_2mortal(newRV_noinc((SV*) YY )));

      PUTBACK;

      count = perl_call_sv( SvRV(cb), G_SCALAR | G_EVAL );
      retval = ReportPerlError( AST__GRFER );

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
  dSP;
  SV * cb;
  SV * external;
  int retval;

  if (!astOK) return 0;
  if (CurrentPlot == NULL ) {
    astError( AST__GRFER, "No Plot object stored. Should not happen." );
    return 0;
  }

  cb = Perl_getPlotAttr( "_gqch" );

  if (astOK) {
    if ( cb != NULL ) {
      int count;
      int flags = G_ARRAY | G_EVAL;
      ENTER;
      SAVETMPS;

      /* Always need PUSHMARK/PUTBACK even if no args */
      PUSHMARK(sp);

      /* If we have a registered external object, push that on as
	 a first argument. Else, in this case set NOARGS flag */
      external = Perl_getPlotAttr( "_gexternal" );

      if ( external != NULL ) {
	XPUSHs( external );
      } else {
	/* No arguments */
	flags |= G_NOARGS;
      }
      PUTBACK;

      count = perl_call_sv( SvRV(cb), flags);
      retval = ReportPerlError( AST__GRFER );

      SPAGAIN;

      if (astOK) {
	if (count != 3) {
	  astError( AST__GRFER,
		    "Must return 3 args from GQch callback");
	  retval = 0;
	} else {
	  /* pop results off the stack */
	  *chh = (float) POPn;
	  *chv = (float) POPn;
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
      Report("astGQch");
    }
  } else {
    retval = 0;
  }
  return retval;
}

int astGMark( int n, const float *x, const float *y, int type ){
  dSP;
  SV * cb;
  AV * XX;
  AV * YY;
  SV * external;
  int retval;

  if (n == 0 ) return 1;

  if (!astOK) return 0;
  if (CurrentPlot == NULL ) {
    astError( AST__GRFER, "No Plot object stored. Should not happen." );
    return 0;
  }

  cb = Perl_getPlotAttr( "_gmark" );

  if (astOK) {
    if ( cb != NULL ) {
      int count;
      ENTER;
      SAVETMPS;

      PUSHMARK(sp);

      /* If we have a registered external object, push that on as
	 a first argument. */
      external = Perl_getPlotAttr( "_gexternal" );
      if ( external != NULL ) {
	XPUSHs( external );
      }

      /* unpack is now reverse to XS norm */
      XX = newAV();
      unpack1D( newRV_noinc((SV*) XX), (float *)x, 'f', n);
      YY = newAV();
      unpack1D( newRV_noinc((SV*) YY), (float *)y, 'f', n);

      XPUSHs( sv_2mortal(newRV_noinc((SV*) XX )));
      XPUSHs( sv_2mortal(newRV_noinc((SV*) YY )));
      XPUSHs( sv_2mortal(newSViv(type) ) );

      PUTBACK;

      count = perl_call_sv( SvRV(cb), G_SCALAR | G_EVAL );
      retval = ReportPerlError( AST__GRFER );

      SPAGAIN;

      if (astOK) {
	if (count != 1) {
	  astError( AST__GRFER,
		    "Returned more than 1 arg from GMark callback");
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
      Report("astGMark");
    }
  } else {
    retval = 0;
  }
  return retval;
}

int astGText( const char *text, float x, float y, const char *just,
              float upx, float upy ){
  dSP;
  SV * cb;
  SV * external;
  int retval;

  if (!astOK) return 0;
  if (CurrentPlot == NULL ) {
    astError( AST__GRFER, "No Plot object stored. Should not happen." );
    return 0;
  }

  cb = Perl_getPlotAttr( "_gtext" );

  if (astOK) {
    if ( cb != NULL ) {
      int count;
      ENTER;
      SAVETMPS;

      PUSHMARK(sp);

      /* If we have a registered external object, push that on as
	 a first argument. */
      external = Perl_getPlotAttr( "_gexternal" );
      if ( external != NULL ) {
	XPUSHs( external );
      }

      XPUSHs( sv_2mortal(newSVpv(text, 0) ) );
      XPUSHs( sv_2mortal(newSVnv(x) ) );
      XPUSHs( sv_2mortal(newSVnv(y) ) );
      XPUSHs( sv_2mortal(newSVpv(just, 0) ) );
      XPUSHs( sv_2mortal(newSVnv(upx) ) );
      XPUSHs( sv_2mortal(newSVnv(upy) ) );
      PUTBACK;

      count = perl_call_sv( SvRV(cb), G_SCALAR | G_EVAL);
      retval = ReportPerlError( AST__GRFER );

      SPAGAIN;

      if (astOK) {
	if (count != 1) {
	  astError( AST__GRFER,
		    "Returned more than 1 arg from GText callback");
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
      Report("astGTExt");
    }
  } else {
    retval = 0;
  }
  return retval;
}

int astGTxExt( const char *text, float x, float y, const char *just,
               float upx, float upy, float *xb, float *yb ){

  dSP;
  SV * cb;
  SV * external;
  SV * retarg;
  AV * retarr;
  SV ** elem;
  int retval;
  int len = 0;
  int i;

  if (!astOK) return 0;
  if (CurrentPlot == NULL ) {
    astError( AST__GRFER, "No Plot object stored. Should not happen." );
    return 0;
  }

  cb = Perl_getPlotAttr( "_gtxext" );

  if (astOK) {
    if ( cb != NULL ) {
      int count;
      ENTER;
      SAVETMPS;

      PUSHMARK(sp);

      /* If we have a registered external object, push that on as
	 a first argument. */
      external = Perl_getPlotAttr( "_gexternal" );
      if ( external != NULL ) {
	XPUSHs( external );
      }

      XPUSHs( sv_2mortal(newSVpv(text, 0) ) );
      XPUSHs( sv_2mortal(newSVnv(x) ) );
      XPUSHs( sv_2mortal(newSVnv(y) ) );
      XPUSHs( sv_2mortal(newSVpv(just, 0) ) );
      XPUSHs( sv_2mortal(newSVnv(upx) ) );
      XPUSHs( sv_2mortal(newSVnv(upy) ) );
      PUTBACK;

      count = perl_call_sv( SvRV(cb), G_ARRAY | G_EVAL );
      retval = ReportPerlError( AST__GRFER );

      SPAGAIN;

      if (astOK) {
	if (count != 3) {
	  astError( AST__GRFER,
		    "Must return 3 args from GTxExt callback not %d",count);
	  retval = 0;
	} else {
	  /* The status will be on the stack furthest back so we
	     need to read off yb, then xb then status  */

	  /* yb. Read and copy into output array which was
	     allocated by the caller.
	     We can do this because we know there are always 4 numbers.
	  */
	  retarg = POPs;
	  if ( SvROK(retarg) && SvTYPE(SvRV(retarg))==SVt_PVAV) {
	    retarr = (AV*)SvRV(retarg);
	    len = av_len(retarr) + 1;
	    if (len != 4 ) {
	      astError( AST__GRFER,
			"yb must contain 4 elements not %d", len);
	      retval = 0;
	    } else {
	      for (i=0; i<len; i++) {
		elem = av_fetch( retarr, i, 0);
		if (elem == NULL ) {
		  yb[i] = 0.0;
		} else {
		  yb[i] = (float)SvNV(*elem);
		}
	      }
	    }
	  } else {
	    astError( AST__GRFER,
		      "Must return ref to array with values yb");
	    retval = 0;
	  }

	  /* xb. Read and copy into static storage. */
	  if (astOK) {
	    retarg = POPs;
	    if ( SvROK(retarg) && SvTYPE(SvRV(retarg))==SVt_PVAV) {
	      retarr = (AV*)SvRV(retarg);
	      if (len != 4 ) {
		astError( AST__GRFER,
			  "xb must contain 4 elements not %d", len);
		retval = 0;
	      } else {
		for (i=0; i<len; i++) {
		  elem = av_fetch( retarr, i, 0);
		  if (elem == NULL ) {
		    xb[i] = 0.0;
		  } else {
		    xb[i] = (float)SvNV(*elem);
		  }
		}
	      }
	    } else {
	      astError( AST__GRFER,
			"Must return ref to array with values xb");
	      retval = 0;
	    }
	  }

	  /* Status return */
	  if (astOK) {
	    retval = POPi;
	  }
	}
      } else {
	retval = 0;
      }

      PUTBACK;

      FREETMPS;
      LEAVE;
    } else {
      retval = 0;
      Report("astGTxExt");
    }
  } else {
    retval = 0;
  }
  return retval;
}

int astGAttr( int attr, double value, double *old_value, int prim ){
  dSP;
  SV * cb;
  SV * external;
  int retval;
  double cache;

  if (!astOK) return 0;

  /* Optimize away if we get both a bad value and null old_value */
  if (value == AST__BAD && old_value == NULL) return 1;

  /* Make sure we have a plot */
  if (CurrentPlot == NULL ) {
    astError( AST__GRFER, "No Plot object stored. Should not happen." );
    return 0;
  }

  cb = Perl_getPlotAttr( "_gattr" );

  if (astOK) {
    if ( cb != NULL ) {
      int count;
      ENTER;
      SAVETMPS;

      PUSHMARK(sp);

      /* If we have a registered external object, push that on as
	 a first argument. */
      external = Perl_getPlotAttr( "_gexternal" );
      if ( external != NULL ) {
	XPUSHs( external );
      }

      XPUSHs( sv_2mortal(newSViv(attr) ) );
      XPUSHs( sv_2mortal(newSVnv(value) ) );
      XPUSHs( sv_2mortal(newSViv(prim) ) );

      PUTBACK;

      count = perl_call_sv( SvRV(cb), G_ARRAY | G_EVAL );
      retval = ReportPerlError( AST__GRFER );

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

int astGScales( float *chv, float *chh ){
  dSP;
  SV * cb;
  SV * external;
  int retval;

  if (!astOK) return 0;
  if (CurrentPlot == NULL ) {
    astError( AST__GRFER, "No Plot object stored. Should not happen." );
    return 0;
  }

  cb = Perl_getPlotAttr( "_gscales" );

  if (astOK) {
    if ( cb != NULL ) {
      int count;
      int flags = G_ARRAY | G_EVAL;
      ENTER;
      SAVETMPS;

      /* Always need PUSHMARK/PUTBACK even if no args */
      PUSHMARK(sp);

      /* If we have a registered external object, push that on as
	 a first argument. Else, in this case set NOARGS flag */
      external = Perl_getPlotAttr( "_gexternal" );

      if ( external != NULL ) {
	XPUSHs( external );
      } else {
	/* No arguments */
	flags |= G_NOARGS;
      }
      PUTBACK;

      count = perl_call_sv( SvRV(cb), flags);
      retval = ReportPerlError( AST__GRFER );

      SPAGAIN;

      if (astOK) {
	if (count != 3) {
	  astError( AST__GRFER,
		    "Must return 3 args from GScales callback");
	  retval = 0;
	} else {
	  /* pop results off the stack */
	  *chh = (float) POPn;
	  *chv = (float) POPn;
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
      Report("astGScales");
    }
  } else {
    retval = 0;
  }
  return retval;
}


int astGCap( int cap, int value ) {
  dSP;
  SV * cb;
  SV * external;
  int retval;

  if (!astOK) return 0;
  if (CurrentPlot == NULL ) {
    astError( AST__GRFER, "No Plot object stored. Should not happen." );
    return 0;
  }

  cb = Perl_getPlotAttr( "_gcap" );

  if (astOK) {
    if ( cb != NULL ) {
      int count;
      ENTER;
      SAVETMPS;

      PUSHMARK(sp);

      /* If we have a registered external object, push that on as
	 a first argument. */
      external = Perl_getPlotAttr( "_gexternal" );
      if ( external != NULL ) {
	XPUSHs( external );
      }

      XPUSHs( sv_2mortal(newSViv(cap) ) );
      XPUSHs( sv_2mortal(newSViv(value) ) );
      PUTBACK;

      count = perl_call_sv( SvRV(cb), G_SCALAR | G_EVAL);
      retval = ReportPerlError( AST__GRFER );

      SPAGAIN;

      if (astOK) {
	if (count != 1) {
	  astError( AST__GRFER,
		    "Returned more than 1 arg from GCap callback");
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
      Report("astGCap");
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
