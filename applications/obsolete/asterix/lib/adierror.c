#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>

#include "adi.h"
#include "aditypes.h"
#include "adi_err.h"
#include "adierror.h"
#include "adiparse.h"
#include "adikrnl.h"

#ifndef NOEMS
#include "ems.h"
#endif

#define _ESTR(_x,_s) \
  char *_x = _s;

_ESTR(Estr__AdjObjRef, "adjusting object reference count")
_ESTR(Estr__AltObjShp, "altering object dimensions")
_ESTR(Estr__CloFilObj, "closing file object")
_ESTR(Estr__CloObjId,  "cloning object identifier")
_ESTR(Estr__CopObj,    "copying object")
_ESTR(Estr__CopObjCmp, "copying object component")
_ESTR(Estr__CreFilObj, "creating file object")
_ESTR(Estr__CreObjDat, "creating new object data")
_ESTR(Estr__CreRef,    "creating object reference")
_ESTR(Estr__DefCls,    "defining class")
_ESTR(Estr__DefClsCac, "defining class cluster size")
_ESTR(Estr__DefClsDes, "defining class destructor method")
_ESTR(Estr__DefClsPrt, "defining class print method")
_ESTR(Estr__DefCst,    "defining common string")
_ESTR(Estr__DefFilRep, "defining file representation")
_ESTR(Estr__DefGen,    "defining new generic function")
_ESTR(Estr__DefGenDis, "defining generic dispatch procedure")
_ESTR(Estr__DefMcf,    "defining new method combination form")
_ESTR(Estr__DefMth,    "defining new method")
_ESTR(Estr__DelObj,    "deleting object")
_ESTR(Estr__DelObjCmp, "deleting object component")
_ESTR(Estr__DelObjPrp, "deleting object property")
_ESTR(Estr__DelStrCmp, "deleting structure component")
_ESTR(Estr__ExeMth,    "executing method")
_ESTR(Estr__FshObjGrp, "flushing object group")
_ESTR(Estr__GetNumCmp, "getting number of structure components")
_ESTR(Estr__GetNumPrp, "getting number of object properties")
_ESTR(Estr__GetObjDat, "getting object data")
_ESTR(Estr__GetObjNam, "getting object name")
_ESTR(Estr__GetObjRef, "getting object reference count")
_ESTR(Estr__GetObjShp, "getting object dimensions")
_ESTR(Estr__GetObjTyp, "getting object type")
_ESTR(Estr__GetRefObj, "getting reference object id")
_ESTR(Estr__IndStrCmp, "indexing structure component")
_ESTR(Estr__IndObjPrp, "indexing object property")
_ESTR(Estr__LnkFilObj, "linking file objects")
_ESTR(Estr__LnkObjGrp, "linking object to group")
_ESTR(Estr__LodDefPkg, "loading definitions package")
_ESTR(Estr__LocObjCel, "locating object cell")
_ESTR(Estr__LocObjCmp, "locating object component")
_ESTR(Estr__LocObjPrp, "locating object property")
_ESTR(Estr__LocObjSli, "locating object slice")
_ESTR(Estr__LocStrCmp, "locating structure component")
_ESTR(Estr__MapObjDat, "mapping object data")
_ESTR(Estr__OpeFilObj, "opening file object")
_ESTR(Estr__PriObj,    "printing object")
_ESTR(Estr__PutObjDat, "putting object data")
_ESTR(Estr__PutRefObj, "putting reference object id")
_ESTR(Estr__SetObjDat, "setting object data")
_ESTR(Estr__TstObjDer, "testing object class derivation")
_ESTR(Estr__TstObjExi, "testing object existance")
_ESTR(Estr__TstObjSta, "testing object state")
_ESTR(Estr__UlnFilObj, "unlinking file objects")
_ESTR(Estr__UnmObjDat, "unmapping object data")


/*
 * Global variables, externally visible
 */
char	*ADI_G_err_rtn = NULL;		/* Current error routine */


/* Routines to support non-EMS error reporting
 */
#ifdef  NOEMS
#define	MAXTOK	10			/* Maximum number of tokens */
#define TOKLEN  80

typedef
  struct {
    char	name[20];
    char 	dat[TOKELEN];
    int		len;
    }
  etoken;

etoken  ADI_G_err_toks[MAXTOK];		/* Error tokens */
int	ADI_G_err_ntok = 0;		/* Token count */
char	ADI_G_err_ctx[200];		/* Current contextual error */
#endif

void adix_errcnl( ADIstatus status )
  {
#ifndef NOEMS
  ems_annul_c( status );
#else
  *status = SAI__OK;
#endif
  }

/*
 * Routine to expand message tokens for the error context string
 */
#ifdef  NOEMS
void adix_errexp( char *mes, char *obuf )
  {
  ADIboolean	found;
  int		itok;
  char		*mptr = obuf;
  char		*sptr = mes;
  int		tlen,vlen;

  for ( sptr = mes; *sptr; ) {
    if ( *sptr == '^' )
      {
      sptr++;
      for( tlen=0; isalnum(*(sptr+tlen)); tlen++ );
      if ( tlen )
	{
	for ( itok=0, found = ADI__false;
	      (itok<ADI_G_err_ntok) && !found ; itok++ )
	  found = !strncmp(sptr,ADI_G_err_toks[itok].name,tlen);
	if ( found )
	  {
	  itok--;
	  vlen = ADI_G_err_toks[itok].len;
	  memcpy( mptr, ADI_G_err_toks[itok].dat, vlen );
	  mptr += vlen;
	  }
	else {
	  *mptr++ = '<';
	  memcpy( mptr, sptr, tlen );
	  mptr += tlen;
	  *mptr++ = '>';
	  }
	sptr += tlen;
	}
      else
	*mptr++ = '^';
      }
    else
      *mptr++ = *sptr++;
    }

  *mptr = '\0';
  }
#endif


/*
 *  Set the contextual message string
 */
void adix_setes( char *ctx, int clen, ADIstatus status )
  {
  char		lctx[200];		/* Local contextual error */
  char		*cptr = NULL;		/* Context message */
  int		lrtn;			/* Length of named routine name */

  if ( clen <= 0 )			/* Import string */
    clen = strlen(ctx);

  if ( *ctx && (*ctx!=' ') ) {		/* User supplied a context string? */
    if ( ADI_G_err_rtn ) {		/* Error routine specified? */
      lrtn = strlen( ADI_G_err_rtn );
      strcpy( lctx, ADI_G_err_rtn );
      strcpy( lctx + lrtn, " : Error " );
      strncpy( lctx + lrtn + 9, ctx, clen );
      *(lctx+lrtn+9+clen) = 0;
      cptr = lctx;
      }
    else
      cptr = ctx;
    }

#ifndef NOEMS
  if ( cptr )				/* Any contextual error? */
    ems_rep_c( " ", cptr, status );

#else
  adix_errexp( cptr, ADI_G_err_ctx );	/* Expand tokens */

  ADI_G_err_ntok = 0;			/* Reset token count */
#endif
  }


/*
 *  Set the error and contextual message string
 */
void adix_setecs( ADIstatype code, char *ctx, int clen, va_list ap, ADIstatus status )
  {
  char 		buf[200];
  ADIobj	estr;

/* Load users contextual message to use tokens before they're cancelled */
  if ( ctx && (clen!=0) ) {
    estr = ADIstrmExtendCst( ADIstrmNew( "w", status ), buf, 200, status );
    ADIstrmVprintf( estr, ctx, ap, status );
    buf[_strm_data(estr)->dev->bnc] = 0;
    adix_erase( &estr, 1, status );

    *status = code;

    adix_setes( buf, _CSM, status );
    }

  else {
    *status = code;

    adix_setes( adix_errmsg( code, NULL, 0 ), _CSM, status );
    }
  }


#ifdef  NOEMS
void adix_errctx( char *buf, int buflen )
  {
  strncpy( buf, ADI_G_err_ctx, buflen );
  }
#endif


char *adix_errmsg( ADIstatype code, char *buf, int buflen )
  {
  static
    char *emsg = "Unrecognised ADI error code";

  static
    struct
      {
      ADIstatype	code;
      char		*msg;
      }
    etable[] =
      {
      {ADI__CONER,	"Data conversion error"},
      {ADI__TRUNC,	"Text truncated"},
      {ADI__INVARG,	"Invalid argument"},
      {ADI__SYMDEF,	"Symbol already defined"},
      {ADI__FATAL,	"Fatal internal ADI error"},
      {ADI__IDINV,	"Invalid ADI identifier"},
      {ADI__ILLKOP,	"Illegal operation on kernel object"},
      {ADI__NOTSET,	"Object data is not set"},
      {ADI__EXISTS,	"Object already exists"},
      {ADI__NOMTH,	"No applicable methods"},
      {ADI__MTHERR,	"Error in method"},
      {ADI__NOMEMB,	"No such class member"},
      {ADI__ISPRIM,	"Object is primitive"},
      {ADI__ILLOP,	"Illegal operation"},
      {ADI__OUTMEM,	"Unable to allocate dynamic memory"},
      {ADI__NOTACT,	"ADI is not active"},
      {ADI__NOPROP,	"No such property"},
      {ADI__NOCOMP,	"No such structure component"},
      {ADI__EXCEED,	"Storage space in buffer exceeded"},
      {ADI__NONAME,	"Object is not a named object"},
      {ADI__MAPPED,	"Object is already mapped"},
      {ADI__NOTMAP,	"Object is not mapped at this address"},
      {ADI__RDONLY,	"Object is readonly"},
      {ADI__SYNTAX,	"Syntax error"},
      {ADI__ITREND,	"No more items"},
      {SAI__ERROR,	"Error"},
      {SAI__OK,		""}
      };

  int		i;
  char 		*sptr;
  ADIlogical	found = ADI__false;

  for( i=0; etable[i].code && ! found ; )
    if ( code == etable[i].code )
      found = ADI__true;
    else
      i++;

  if ( found )
    sptr = etable[i].msg;
  else
    sptr = emsg;

  if ( buflen )
    strncpy( buf, sptr, buflen );

  return sptr;
  }

void adix_errout( char *rtn, char *str, ADIstatus status )
  {
  ADI_G_err_rtn = rtn;
  adix_setes( str, _CSM, status );
  ADI_G_err_rtn = NULL;
  }
