#include <limits.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "asterix.h"
#include "sae_par.h"

#include "aditypes.h"
#include "adiconv.h"
#include "adikrnl.h"
#include "adisyms.h"


typedef
  struct {
    ADIclassDef		*to;		/* Type procedure converts to */
    ADIconvertor	func;		/* The convertor procedure */
    ADIobj		next;	        /* Next convertor in list */
    }
  ADIcnvEntry;

#define         KT_CTYPE_cnv     ADIcnvEntry
#define		KT_DEFN_cnv      ADI_G_tdef_cnv
#define         KT_CODE_cnv      (-17)
#define         _cnv_data(_x)    ((ADIcnvEntry *) _ID_DATA(_x))
#define         _cnv_to(_x)      (_cnv_data(_x)->to)
#define         _cnv_func(_x)    (_cnv_data(_x)->func)
#define         _cnv_next(_x)    (_cnv_data(_x)->next)

/*
 *  Allocation control for convertor function list elements
 */
_DEF_STATIC_CDEF("_ConvertorFunction",cnv,128,NULL,NULL);


/*
 *  Look up convertor function from a class 'from' to a class 'to'
 */
ADIconvertor ADIcnvFind( ADIclassDef *from, ADIclassDef *to,
		ADIstatus status )
  {
  ADIobj       	crec = ADI__nullid;
  ADIobj	cur = from->cnvs;

  while ( _valid_q(cur) && _null_q(crec) ) {
    if ( _cnv_to(cur) == to )
      crec = cur;
    else
      cur = _cnv_next(cur);
    }

  return _ok(status) && _valid_q(crec) ? _cnv_func(crec) : NULL;
  }


void ADIcnvNew( ADIclassDef *from, ADIclassDef *to,
		ADIconvertor func, ADIstatus status )
  {
  ADIobj	newid = ADI__nullid;

  newid = adix_cls_alloc( &KT_DEFN_cnv, status );

/* Allocated ok */
  if ( _ok(status) ) {
    ADIobj	*ipoint = &from->cnvs;

/* Look for insertion point */
    while ( _valid_q(*ipoint) )
      ipoint = &_cnv_next(*ipoint);

/* Insert at end of list */
    *ipoint = newid;

/* Set the fields */
    _cnv_to(newid) = to;
    _cnv_func(newid) = func;
    _cnv_next(newid) = ADI__nullid;
    }
  }


/* Variables required to traverse character strings of the 3 types
 */
#define _declare_c_tr(_prefix,_mta) \
  int		_prefix##_is_adi = _valid_q(_mta->id); \
  int		_prefix##_nterm = (_mta->size == _CSM); \
  int		_prefix##_clen = _mta->size; \
  int		_prefix##_blen = _mta->size; \
  char          **_prefix##dptr; \
  char		*_prefix##_buf; \
  char          *_prefix##ptr; \
  ADIstring     *_prefix##sptr = NULL;\
  int		_prefix##_nulterm = _mta->nulterm

/* Macro sets up one of the 3 pointer forms to traverse character data
 */
#define _start_c_tr(_prefix,_data) \
  if ( _prefix##_nterm ) _prefix##dptr = (char **) _data; \
  else if ( _prefix##_is_adi ) _prefix##sptr = (ADIstring *) _data; \
  else _prefix##ptr = (char *) _data;

#define _setbuf_c_tr(_prefix) \
    if ( _prefix##_nterm ) \
      _prefix##_buf=*_prefix##dptr; \
    else if ( _prefix##_is_adi ) \
      {_prefix##_buf=_prefix##sptr->data;_prefix##_blen=_prefix##sptr->len;} \
    else \
      _prefix##_buf = _prefix##ptr;

#define _coppad_c_tr(_pref,_source) \
  {char *sptr;int ic; \
  if ( _pref##_nterm ) {_pref##_buf=*_pref##dptr;} \
  else if ( _pref##_is_adi ) {_pref##_buf=_pref##sptr->data;\
	_pref##_blen=_pref##sptr->len;} \
  else _pref##_buf = _pref##ptr; \
  for(sptr=_source,ic=0;*sptr && (ic<_pref##_blen);sptr++,ic++) \
    _pref##_buf[ic] = *sptr;\
  if ( *sptr ) (*nerr)++;\
  else \
    {if ( _pref##_nterm && ((ic+1)<_pref##_blen)) _pref##_buf[ic]=0; \
     else if ( (ic+1) < _pref##_blen ) \
       {if (_pref##_nulterm) _pref##_buf[ic]=0;else \
       memset(_pref##_buf+ic,' ',_pref##_blen-ic);}}\
  }

#define _advance_c_tr(_prefix) \
    if ( _prefix##_nterm ) _prefix##dptr++; \
    else { if ( _prefix##_is_adi ) _prefix##sptr++; \
    else _prefix##ptr += _prefix##_clen;}

/*
 *  Convert CHAR -> CHAR
 */
void ADIcnvCC( ADImta *idd, int nval, char *in, ADImta *odd,
		   char *out, int *nerr, ADIstatus status )
  {
  _declare_c_tr(i,idd);			/* Cursor over inputs */
  _declare_c_tr(o,odd);
  int           ic;                     /* Loop over converted data */
  int           ival = nval;            /* Loop over input values */

  _chk_stat;                            /* Check status on entry */

  _start_c_tr(i,in);
  _start_c_tr(o,out);

/* Loop over input strings */
  for( ; ival--; ) {
    _setbuf_c_tr(i);

    if ( o_is_adi ) {                 /* Allowed length for output */
      o_buf = osptr->data;
      o_blen = osptr->len;
      }
    else if ( o_nterm ) {
      o_buf = *odptr;
      o_blen = 999;
      }
    else
      o_buf = optr;

    for( ic=0; ic<_MIN(i_blen,o_blen); ic++ )
      o_buf[ic] = i_buf[ic];

    if ( i_blen < o_blen ) {              /* Null terminate or pad */
      if ( o_nulterm )
	o_buf[i_blen] = 0;
      else
	memset( o_buf + i_blen, ' ', o_blen - i_blen );
      }

/* Advance pointers */
    _advance_c_tr(i);
    _advance_c_tr(o);
    }
  }


#define _cnv_to_c(_T,_t,_caste,_fmt) \
void ADIcnv##_T##C( ADImta *idd, int n, char *in, ADImta *odd, \
		    char *out, int *nerr, ADIstatus status ) { \
  char          buf[30]; \
  _TM_ctype(_t)	*iptr = (_TM_ctype(_t) *) in; \
  int           ival = n; \
 _declare_c_tr(o,odd); \
  _chk_stat; \
  _start_c_tr(o,out); \
  for( ; ival--; ) { _caste val = *iptr++;  \
    sprintf( buf, _fmt, val ); \
    _setbuf_c_tr(o); _coppad_c_tr(o,buf); _advance_c_tr(o); } \
  }

_cnv_to_c(UB,ub,int,"%d")
_cnv_to_c(B,b,int,"%d")
_cnv_to_c(UW,uw,int,"%d")
_cnv_to_c(W,w,int,"%d")
_cnv_to_c(I,i,ADIinteger,"%ld")
_cnv_to_c(R,r,ADIdouble,"%g")
_cnv_to_c(D,d,ADIdouble,"%g")
_cnv_to_c(P,p,ADIpointer,"%x")


int ADIcnvCtoD( char *dat, int dlen, ADIdouble *val )
  {
  char		buf[30];
  char		*dptr = dat;
  int		ic;
  int		len = dlen;
  int		ok = 0;
  ADIdouble	r = 0;
  char		*uptr = dat;

/* Check string for Fortran type 'd' or 'D' characters. If found, copy */
/* the string to our buffer. Also copy to buffer if not null-terminated */
  if ( len > 0 ) {
    _CH_MOVE( buf, dat, len );
    buf[len] = 0;
    for( ic=0; ic<len; ic++ )
      if ( (buf[ic] == 'd') || (buf[ic] == 'D') )
	buf[ic] = 'e';
    uptr = buf;
    }
  else if ( len == _CSM ) {
    int	dp=0;

    for( ic=0; dptr[ic]; ic++ )
      if ( (buf[ic] == 'd') || (buf[ic] == 'D') )
	dp = ic;

    if ( dp ) {
      len = ic - 1;
      _CH_MOVE( buf, dat, len );
      buf[len] = 0;
      uptr = buf;
      }
    }

  if ( len ) {
    ok = sscanf( uptr, "%lg", val );
    }

  return ok;
  }


int ADIcnvCtoI( char *dat, int dlen, ADIinteger *val )
  {
  char		*dptr = dat;
  int		i = dlen;
  int		ok = 0;
  ADIinteger	r = 0;

  if ( dlen > 0 ) {
    while ( i-- && isdigit(*dptr) ) {
      r = r*10 + (*dptr - '0');
      dptr++;
      }
    ok = (!i);
    }
  else if ( dlen == _CSM ) {
    while ( isdigit(*dptr) ) {
      r = r*10 + (*dptr - '0');
      dptr++;
      }
    ok = ! *dptr;
    }

  *val = r;

  return ok;
  }

#define _chk_scan_2(_convt,_ctp,_ct) \
  {\
  _convt      out;\
  if ( ADIcnvCto##_ctp( i_buf, i_blen, &out ) ) {\
  if ( out<((_convt) _TM_min(_ct)) || out>((_convt) _TM_max(_ct)) )\
    {*optr = _TM_bad(_ct); (*nerr)++;}\
  else \
    *optr = (_TM_ctype(_ct)) out;} \
  else \
    {*optr = _TM_bad(_ct); (*nerr)++;} \
  }

#define _chk_scan_1(_convt,_ctp,_ct) {\
  _convt      out;\
  if ( ADIcnvCto##_ctp( i_buf, i_blen, &out ) ) \
    *optr = (_convt) out; \
  else \
    {*optr = _TM_bad(_ct); (*nerr)++;} \
  }

void ADIcnvCUB( ADImta *idd, int nval, char *in, ADImta *odd,
		char *out, int *nerr, ADIstatus status )
  {
  int           ival = nval;            /* Loop over input values */
  ADIubyte	*optr = (ADIubyte *)out;/* Output data pointer */

 _declare_c_tr(i,idd);
  _chk_stat;
  _start_c_tr(i,in);
  for( ; ival--; optr++ ) {
    _setbuf_c_tr(i);
    _chk_scan_2(ADIinteger,I,ub);
    _advance_c_tr(i);
    }
  }

void ADIcnvCB( ADImta *idd, int nval, char *in, ADImta *odd,
		char *out, int *nerr, ADIstatus status )
  {
  int           ival = nval;            /* Loop over input values */
  ADIbyte	*optr = (ADIbyte *)out;	/* Output data pointer */

 _declare_c_tr(i,idd);
  _chk_stat;
  _start_c_tr(i,in);
  for( ; ival--; optr++ ) {
    _setbuf_c_tr(i);
    _chk_scan_2(ADIinteger,I,b);
    _advance_c_tr(i);
    }
  }

void ADIcnvCUW( ADImta *idd, int nval, char *in, ADImta *odd,
		char *out, int *nerr, ADIstatus status )
  {
  int           ival = nval;            /* Loop over input values */
  ADIuword	*optr = (ADIuword *)out;/* Output data pointer */

 _declare_c_tr(i,idd);
  _chk_stat;
  _start_c_tr(i,in);
  for( ; ival--; optr++ ) {
    _setbuf_c_tr(i);
    _chk_scan_2(ADIinteger,I,uw);
    _advance_c_tr(i);
    }
  }

void ADIcnvCW( ADImta *idd, int nval, char *in, ADImta *odd,
	       char *out, int *nerr, ADIstatus status )
  {
  int           ival = nval;            /* Loop over input values */
  ADIword	*optr = (ADIword *)out;	/* Output data pointer */

 _declare_c_tr(i,idd);
  _chk_stat;
  _start_c_tr(i,in);
  for( ; ival--; optr++ ) {
    _setbuf_c_tr(i);
    _chk_scan_2(ADIinteger,I,w);
    _advance_c_tr(i);
    }
  }

void ADIcnvCI( ADImta *idd, int nval, char *in, ADImta *odd,
	       char *out, int *nerr, ADIstatus status )
  {
  int           ival = nval;            /* Loop over input values */
  ADIinteger	*optr = (ADIinteger *)out;	/* Output data pointer */

 _declare_c_tr(i,idd);
  _chk_stat;
  _start_c_tr(i,in);
  for( ; ival--; optr++ ) {
    _setbuf_c_tr(i);
    _chk_scan_2(ADIdouble,D,i);
    _advance_c_tr(i);
    }
  }

void ADIcnvCR( ADImta *idd, int nval, char *in, ADImta *odd,
	       char *out, int *nerr, ADIstatus status )
  {
  int           ival = nval;            /* Loop over input values */
  ADIreal	*optr = (ADIreal *)out;	/* Output data pointer */

 _declare_c_tr(i,idd);
  _chk_stat;
  _start_c_tr(i,in);
  for( ; ival--; optr++ ) {
    _setbuf_c_tr(i);
    _chk_scan_2(ADIdouble,D,r);
    _advance_c_tr(i);
    }
  }

void ADIcnvCD( ADImta *idd, int nval, char *in, ADImta *odd,
	       char *out, int *nerr, ADIstatus status )
  {
  int           ival = nval;            /* Loop over input values */
  ADIdouble	*optr = (ADIdouble *)out;	/* Output data pointer */

 _declare_c_tr(i,idd);
  _chk_stat;
  _start_c_tr(i,in);
  for( ; ival--; optr++ ) {
    _setbuf_c_tr(i);
    _chk_scan_1(ADIdouble,D,d);
    _advance_c_tr(i);
    }
  }



/*
 *  Convert LOGICAL -> LOGICAL
 */
#ifdef ADI_F77
void ADIcnvLL( ADImta *idd, int n, char *in, ADImta *odd,
		   char *out, int *nerr, ADIstatus status )
  {
  int		in_c = idd->nulterm;
  int		out_c = odd->nulterm;
  int           i = n;
  ADIlogical    *iptr = (ADIlogical *) in;
  ADIlogical    *optr = (ADIlogical *) out;

  if ( in_c && ! out_c ) {
    for( ; i-- ; )
      *optr++ = (*iptr++) ? F77_TRUE : F77_FALSE;
    }
  else if ( out_c && ! in_c ) {
    for( ; i-- ; )
      *optr++ = ((*iptr++) == F77_FALSE) ? ADI__false : ADI__true;
    }
  else
    _CH_MOVE( in, out, n*sizeof(ADIlogical) );
  }
#endif

#define _do_case_chk(_it,_ot) \
      _TM_ctype(_it) *iptr = (_TM_ctype(_it) *) in; \
      _TM_ctype(_ot) *optr = (_TM_ctype(_ot) *) out; \
      int	ival = n; \
      _chk_stat; \
      for( ; ival--; )\
	*optr++ = *iptr++;

#define _do_case_chk_min(_it,_ot) \
      _TM_ctype(_it) *iptr = (_TM_ctype(_it) *) in; \
      _TM_ctype(_ot) *optr = (_TM_ctype(_ot) *) out; \
      int	ival = n; \
      _chk_stat; \
      for( ; ival--; iptr++, optr++ ) {\
	if ( *iptr < ((_TM_ctype(_it)) _TM_min(_ot)) )\
	  {*optr = _TM_bad(_ot); (*nerr)++;}\
	else \
	  *optr = (_TM_ctype(_ot)) *iptr;} \

#define _do_case_chk_max(_it,_ot) \
      _TM_ctype(_it) *iptr = (_TM_ctype(_it) *) in; \
      _TM_ctype(_ot) *optr = (_TM_ctype(_ot) *) out; \
      int	ival = n; \
      _chk_stat; \
      for( ; ival--; iptr++, optr++ ) {\
	if ( *iptr > ((_TM_ctype(_it)) _TM_max(_ot)) )\
	  {*optr = _TM_bad(_ot); (*nerr)++;}\
	else \
	  *optr = (_TM_ctype(_ot)) *iptr;} \

#define _do_case_chk_bth(_it,_ot) \
      _TM_ctype(_it) *iptr = (_TM_ctype(_it) *) in; \
      _TM_ctype(_ot) *optr = (_TM_ctype(_ot) *) out; \
      int	ival = n; \
      _chk_stat; \
      for( ; ival--; iptr++, optr++ ) {\
	if ( *iptr < ((_TM_ctype(_it)) _TM_min(_ot)) || *iptr > ((_TM_ctype(_it)) _TM_max(_ot)) )\
	  {*optr = _TM_bad(_ot); (*nerr)++;}\
	else \
	  *optr = (_TM_ctype(_ot)) *iptr;} \

/*
 * Caste to _UBYTE
 */
void ADIcnvBUB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(b,ub);}
void ADIcnvWUB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_max(w,ub);}
void ADIcnvUWUB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_max(uw,ub);}
void ADIcnvIUB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(i,ub);}
void ADIcnvRUB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(r,ub);}
void ADIcnvDUB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(d,ub);}


/*
 * Caste to _BYTE
 */
void ADIcnvUBB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_max(ub,b);}
void ADIcnvWB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(w,b);}
void ADIcnvUWB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_max(uw,b);}
void ADIcnvIB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(i,b);}
void ADIcnvRB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(r,b);}
void ADIcnvDB( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(d,b);}


/*
 * Caste to _WORD
 */
void ADIcnvUBW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(ub,w);}
void ADIcnvBW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(b,w);}
void ADIcnvUWW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_max(uw,w);}
void ADIcnvIW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(i,w);}
void ADIcnvRW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(r,w);}
void ADIcnvDW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(d,w);}


/*
 * Caste to _UWORD
 */
void ADIcnvUBUW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(ub,uw);}
void ADIcnvBUW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_min(b,uw);}
void ADIcnvWUW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_max(w,uw);}
void ADIcnvIUW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(i,uw);}
void ADIcnvRUW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(r,uw);}
void ADIcnvDUW( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(d,uw);}

/*
 * Caste to _INTEGER
 */
void ADIcnvUBI( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(ub,i);}
void ADIcnvBI( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(b,i);}
void ADIcnvUWI( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(uw,i);}
void ADIcnvWI( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(w,i);}
void ADIcnvRI( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(r,i);}
void ADIcnvDI( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(d,i);}

/*
 * Caste to REAL
 */
void ADIcnvUBR( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(ub,r);}
void ADIcnvBR( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(b,r);}
void ADIcnvUWR( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(uw,r);}
void ADIcnvWR( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(w,r);}
void ADIcnvIR( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(i,r);}
void ADIcnvDR( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk_bth(d,r);}

/*
 * Caste to _DOUBLE
 */
void ADIcnvUBD( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(ub,d);}
void ADIcnvBD( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(b,d);}
void ADIcnvUWD( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(uw,d);}
void ADIcnvWD( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(w,d);}
void ADIcnvID( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(i,d);}
void ADIcnvRD( ADImta *idd, int n, char *in, ADImta *odd, char *out, int *nerr, ADIstatus status )
  {_do_case_chk(r,d);}


typedef
  enum {ub=0,b=1,uw=2,w=3,i=4,r=5,d=6,c=7,l=8,p=9}
  tcode;

void ADIcnvInit( ADIstatus status )
  {
  static struct {
    tcode t1,t2;
    ADIconvertor func;
    }
    ctable[] = {
    {ub, c, ADIcnvUBC },
    { b, c, ADIcnvBC },
    {uw, c, ADIcnvUWC },
    { w, c, ADIcnvWC },
    { i, c, ADIcnvIC },
    { r, c, ADIcnvRC },
    { d, c, ADIcnvDC },

    { c, c, ADIcnvCC },

    { c,ub, ADIcnvCUB },
    { c, b, ADIcnvCB },
    { c,uw, ADIcnvCUW },
    { c, w, ADIcnvCW },
    { c, i, ADIcnvCI },
    { c, r, ADIcnvCR },
    { c, d, ADIcnvCD },

    { b,ub, ADIcnvBUB },
    {uw,ub, ADIcnvUWUB },
    { w,ub, ADIcnvWUB },
    { i,ub, ADIcnvIUB },
    { r,ub, ADIcnvRUB },
    { d,ub, ADIcnvDUB },

    {ub, b, ADIcnvUBB },
    {uw, b, ADIcnvUWB },
    { w, b, ADIcnvWB },
    { i, b, ADIcnvIB },
    { r, b, ADIcnvRB },
    { d, b, ADIcnvDB },

    {ub,uw, ADIcnvUBUW },
    { b,uw, ADIcnvBUW },
    { w,uw, ADIcnvWUW },
    { i,uw, ADIcnvIUW },
    { r,uw, ADIcnvRUW },
    { d,uw, ADIcnvDUW },

    {ub, w, ADIcnvUBW },
    { b, w, ADIcnvBW },
    {uw, w, ADIcnvUWW },
    { i, w, ADIcnvIW },
    { r, w, ADIcnvRW },
    { d, w, ADIcnvDW },

    {ub, i, ADIcnvUBI },
    { b, i, ADIcnvBI },
    {uw, i, ADIcnvUWI },
    { w, i, ADIcnvWI },
    { r, i, ADIcnvRI },
    { d, i, ADIcnvDI },

    {ub, r, ADIcnvUBR },
    { b, r, ADIcnvBR },
    {uw, r, ADIcnvUWR },
    { w, r, ADIcnvIR },
    { i, r, ADIcnvIR },
    { d, r, ADIcnvDR },

    {ub, d, ADIcnvUBD },
    { b, d, ADIcnvBD },
    {uw, d, ADIcnvUWD },
    { w, d, ADIcnvID },
    { i, d, ADIcnvID },
    { r, d, ADIcnvRD },

#ifdef ADI_F77
    { l, l, ADIcnvLL },
#endif
    {ub,ub,NULL}
    };

  ADIclassDef	*cdef[10];
  int		it;

  cdef[ub] = _cdef_data(UT_ALLOC_ub);
  cdef[b]  = _cdef_data(UT_ALLOC_b);
  cdef[uw] = _cdef_data(UT_ALLOC_uw);
  cdef[w]  = _cdef_data(UT_ALLOC_w);
  cdef[i]  = _cdef_data(UT_ALLOC_i);
  cdef[r]  = _cdef_data(UT_ALLOC_r);
  cdef[d]  = _cdef_data(UT_ALLOC_d);
  cdef[c]  = _cdef_data(UT_ALLOC_c);
  cdef[l]  = _cdef_data(UT_ALLOC_l);
  cdef[p]  = _cdef_data(UT_ALLOC_p);

/* Define convertor functions */
  for( it=0; ctable[it].func; it++ )
    ADIcnvNew( cdef[ctable[it].t1], cdef[ctable[it].t2],
	       ctable[it].func, status );

  }
