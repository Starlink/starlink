#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <math.h>
#include <string.h>

#include "asterix.h"                    /* Asterix definitions */

#include "aditypes.h"
#include "adimem.h"                     /* Allocation routines */
#include "adikrnl.h"                    /* Kernel code */
#include "adicface.h"                   /* C programmer interface */
#include "adifuncs.h"                   /* Prototypes for this sub-package */
#include "adiparse.h"
#include "adierror.h"                   /* ADI error handling */
#include "adilist.h"
#include "adistrng.h"
#include "adiexpr.h"

/*
 * Macro to define a logical operation on an arbitrary data type
 */
#define _logp(_tcode,_ocode,_tacc,_oop) \
ADIobj ADI##_tcode##_ocode##_i( int narg, ADIobj args[], ADIstatus status ) \
  {\
  ADIobj	rval = ADI__nullid;\
  ADIlogical	res;\
  res = _tacc(args[0]) _oop _tacc(args[1]);\
  adic_newv0l( res ? ADI__true : ADI__false, &rval, status );\
  return rval;}

/*
 *  General purpose functions
 */
ADIobj ADIgenName_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	nid;
  ADIobj	rval = ADI__nullid;

  nid = adix_namei( args[0], status );

  if ( _valid_q(nid) )
    rval = adix_clone( nid, status );
  else
    adic_new0c( &rval, status );

  return rval;
  }

ADIobj ADIgenType_i( int narg, ADIobj args[], ADIstatus status )
  {
  return adix_clone( _DTDEF(args[0])->aname, status );
  }


/*
 *  Functions on INTEGER's
 */
ADIobj ADIintAbs_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIinteger	*dad;
  ADIobj	rval = ADI__nullid;

  rval = ADIexprOwnArg( args, status );

  dad = &_IVAL(rval);

  if ( *dad < 0 ) *dad = - *dad;

  return rval;
  }

ADIobj ADIintAnd_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIinteger	result;
  ADIobj	rval = ADI__nullid;

  result = _IVAL(args[0]);

  _ARGLOOP_2ND_TO_NTH(carg)
    result &= _IVAL(*carg);

  adic_newv0i( result, &rval, status );

  return rval;
  }

ADIobj ADIintCaste_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;
  ADIinteger	value;

  adic_get0i( args[0], &value, status );

  adic_newv0i( value, &rval, status );

  return rval;
  }

ADIobj ADIintDiv_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0i( _IVAL(args[0]) / _IVAL(args[1]), &rval, status );

  return rval;
  }

ADIobj ADIintEvenQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0l( ! (_IVAL(args[0]) % 2), &rval, status );

  return rval;
  }

ADIobj ADIintGCD_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIinteger	a,b;			/* Arguments */
  ADIinteger	c,d,r;			/* Work variables */
  ADIobj	rval = ADI__nullid;	/* Return value */

  a = _IVAL(args[0]);
  b = _IVAL(args[1]);

  c = _MAX(a,b); d = _MIN(a,b);

  while ( d != 0 ) {
    r = c % d; c = d; d = r;
    }

  adic_newv0i( c, &rval, status );

  return rval;
  }

ADIobj ADIintMax_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIinteger	ival, result;
  ADIobj	rval = ADI__nullid;

  result = _IVAL(args[0]);

  _ARGLOOP_2ND_TO_NTH(carg) {
    ival = _IVAL(*carg);
    if ( ival > result )
      result = ival;
    }

  adic_newv0i( result, &rval, status );

  return rval;
  }

ADIobj ADIintMin_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIinteger	ival, result;
  ADIobj	rval = ADI__nullid;

  result = _IVAL(args[0]);

  _ARGLOOP_2ND_TO_NTH(carg) {
    ival = _IVAL(*carg);
    if ( ival < result )
      result = ival;
    }

  adic_newv0i( result, &rval, status );

  return rval;
  }

ADIobj ADIintMod_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0i( _IVAL(args[0]) % _IVAL(args[1]), &rval, status );

  return rval;
  }

ADIobj ADIintMult_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIinteger	result = 1;
  ADIobj	rval = ADI__nullid;

  _ARGLOOP_1ST_TO_NTH(carg)
    result *= _IVAL(*carg);

  adic_newv0i( result, &rval, status );

  return rval;
  }

ADIobj ADIintNeg_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIinteger	*dad;
  ADIobj	rval = ADI__nullid;

  rval = ADIexprOwnArg( args, status );

  dad = &_IVAL(rval);

  *dad = - *dad;

  return rval;
  }

ADIobj ADIintNot_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIinteger	*dad;
  ADIobj	rval = ADI__nullid;

  rval = ADIexprOwnArg( args, status );

  dad = &_IVAL(rval);

  *dad = ~ *dad;

  return rval;
  }

ADIobj ADIintOddQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0l( (_IVAL(args[0]) % 2), &rval, status );

  return rval;
  }

ADIobj ADIintOr_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIinteger	result;
  ADIobj	rval = ADI__nullid;

  result = _IVAL(args[0]);

  _ARGLOOP_2ND_TO_NTH(carg)
    result |= _IVAL(*carg);

  adic_newv0i( result, &rval, status );

  return rval;
  }

ADIobj ADIintPlus_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIinteger	result = 0;
  ADIobj	rval = ADI__nullid;

  _ARGLOOP_1ST_TO_NTH(carg)
    result += _IVAL(*carg);

  adic_newv0i( result, &rval, status );

  return rval;
  }

ADIobj ADIintQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0l( _int_q(args[0]), &rval, status );

  return rval;
  }

ADIobj ADIintShift_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIinteger	val,shift,res;
  ADIobj	rval = ADI__nullid;

  val = _IVAL(args[0]);
  shift = _IVAL(args[1]);

  if ( shift > 0 )
    res = val >> shift;
  else if ( shift < 0 )
    res = val << (-shift);
  else
    res = val;

  adic_newv0i( res, &rval, status );

  return rval;
  }

ADIobj ADIintSub_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0i( _IVAL(args[0]) - _IVAL(args[1]), &rval, status );

  return rval;
  }

ADIobj ADIintXor_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0i( _IVAL(args[0]) ^ _IVAL(args[1]), &rval, status );

  return rval;
  }

_logp(int,EQ,_IVAL,==)
_logp(int,GE,_IVAL,>=)
_logp(int,GT,_IVAL,>)
_logp(int,LE,_IVAL,<=)
_logp(int,LT,_IVAL,<)
_logp(int,NE,_IVAL,!=)


/*
 *  Functions on REAL's
 */
#define _math_rop(_ocode,_oop) \
ADIobj ADIreal##_ocode##_i( int narg, ADIobj args[], ADIstatus status ) \
  {\
  ADIobj	rval = ADI__nullid;\
  adic_newv0r( (ADIreal) _oop((ADIdouble) _RVAL(args[0])), &rval, status );\
  return rval;}

_math_rop(ArcCos,acos)
_math_rop(ArcSin,asin)
_math_rop(ArcTan,atan)
_math_rop(Cos,cos)
_math_rop(Exp,exp)
_math_rop(Log,log)
_math_rop(Log10,log10)
_math_rop(Sin,sin)
_math_rop(Sqrt,sqrt)
_math_rop(Tan,tan)

ADIobj ADIrealAbs_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIreal	*dad;
  ADIobj	rval = ADI__nullid;

  rval = ADIexprOwnArg( args, status );

  dad = &_RVAL(rval);

  if ( *dad < 0 ) *dad = - *dad;

  return rval;
  }

ADIobj ADIrealArcTan2_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;
  ADIreal	value;

  value = atan2( _RVAL(args[0]), _RVAL(args[1]) );

  adic_newv0r( value, &rval, status );

  return rval;
  }

ADIobj ADIrealCaste_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;
  ADIreal	value;

  adic_get0r( args[0], &value, status );

  adic_newv0r( value, &rval, status );

  return rval;
  }

ADIobj ADIrealDiv_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0r( _RVAL(args[0]) / _RVAL(args[1]), &rval, status );

  return rval;
  }

ADIobj ADIrealMax_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIreal	ival, result;
  ADIobj	rval = ADI__nullid;

  result = _RVAL(args[0]);

  _ARGLOOP_2ND_TO_NTH(carg) {
    ival = _RVAL(*carg);
    if ( ival > result )
      result = ival;
    }

  adic_newv0r( result, &rval, status );

  return rval;
  }

ADIobj ADIrealMin_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIreal	ival, result;
  ADIobj	rval = ADI__nullid;

  result = _RVAL(args[0]);

  _ARGLOOP_2ND_TO_NTH(carg) {
    ival = _RVAL(*carg);
    if ( ival < result )
      result = ival;
    }

  adic_newv0r( result, &rval, status );

  return rval;
  }

ADIobj ADIrealMult_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIreal	result = 1.0;
  ADIobj	rval = ADI__nullid;

  _ARGLOOP_1ST_TO_NTH(carg)
    result *= _RVAL(*carg);

  adic_newv0r( result, &rval, status );

  return rval;
  }

ADIobj ADIrealNeg_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIreal	*dad;
  ADIobj	rval = ADI__nullid;

  rval = ADIexprOwnArg( args, status );

  dad = &_RVAL(rval);

  *dad = - *dad;

  return rval;
  }

ADIobj ADIrealPlus_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIreal	result = 0;
  ADIobj	rval = ADI__nullid;

  _ARGLOOP_1ST_TO_NTH(carg)
    result += _RVAL(*carg);

  adic_newv0r( result, &rval, status );

  return rval;
  }

ADIobj ADIrealQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0l( _real_q(args[0]), &rval, status );

  return rval;
  }

ADIobj ADIrealSub_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0r( _RVAL(args[0]) - _RVAL(args[1]), &rval, status );

  return rval;
  }

_logp(real,EQ,_RVAL,==)
_logp(real,GE,_RVAL,>=)
_logp(real,GT,_RVAL,>)
_logp(real,LE,_RVAL,<=)
_logp(real,LT,_RVAL,<)
_logp(real,NE,_RVAL,!=)


/*
 *  Functions on DOUBLE's
 */
#define _math_dop(_ocode,_oop) \
ADIobj ADIdble##_ocode##_i( int narg, ADIobj args[], ADIstatus status ) \
  {\
  ADIobj	rval = ADI__nullid;\
  adic_newv0d( _oop(_DVAL(args[0])), &rval, status );\
  return rval;}

_math_dop(ArcCos,acos)
_math_dop(ArcSin,asin)
_math_dop(ArcTan,atan)
_math_dop(Cos,cos)
_math_dop(Exp,exp)
_math_dop(Log,log)
_math_dop(Log10,log10)
_math_dop(Sin,sin)
_math_dop(Sqrt,sqrt)
_math_dop(Tan,tan)

ADIobj ADIdbleAbs_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIdouble	*dad;
  ADIobj	rval = ADI__nullid;

  rval = ADIexprOwnArg( args, status );

  dad = &_DVAL(rval);

  if ( *dad < 0 ) *dad = - *dad;

  return rval;
  }

ADIobj ADIdbleArcTan2_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;
  ADIdouble	value;

  value = atan2( _RVAL(args[0]), _RVAL(args[1]) );

  adic_newv0d( value, &rval, status );

  return rval;
  }

ADIobj ADIdbleCaste_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;
  ADIdouble	value;

  adic_get0d( args[0], &value, status );

  adic_newv0d( value, &rval, status );

  return rval;
  }

ADIobj ADIdbleDiv_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0d( _DVAL(args[0]) / _DVAL(args[1]), &rval, status );

  return rval;
  }

ADIobj ADIdbleMax_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIdouble	ival, result;
  ADIobj	rval = ADI__nullid;

  result = _DVAL(args[0]);

  _ARGLOOP_2ND_TO_NTH(carg) {
    ival = _DVAL(*carg);
    if ( ival > result )
      result = ival;
    }

  adic_newv0d( result, &rval, status );

  return rval;
  }

ADIobj ADIdbleMin_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIdouble	ival, result;
  ADIobj	rval = ADI__nullid;

  result = _DVAL(args[0]);

  _ARGLOOP_2ND_TO_NTH(carg) {
    ival = _DVAL(*carg);
    if ( ival < result )
      result = ival;
    }

  adic_newv0d( result, &rval, status );

  return rval;
  }

ADIobj ADIdbleMult_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIdouble	result = 1.0;
  ADIobj	rval = ADI__nullid;

  _ARGLOOP_1ST_TO_NTH(carg)
    result *= _DVAL(*carg);

  adic_newv0d( result, &rval, status );

  return rval;
  }

ADIobj ADIdbleNeg_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIdouble	*dad;
  ADIobj	rval = ADI__nullid;

  rval = ADIexprOwnArg( args, status );

  dad = &_DVAL(rval);

  *dad = - *dad;

  return rval;
  }

ADIobj ADIdblePlus_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIdouble	result = 0;
  ADIobj	rval = ADI__nullid;

  _ARGLOOP_1ST_TO_NTH(carg)
    result += _DVAL(*carg);

  adic_newv0d( result, &rval, status );

  return rval;
  }

ADIobj ADIdbleQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0l( _dble_q(args[0]), &rval, status );

  return rval;
  }

ADIobj ADIdbleSub_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0d( _DVAL(args[0]) - _DVAL(args[1]), &rval, status );

  return rval;
  }

_logp(dble,EQ,_DVAL,==)
_logp(dble,GE,_DVAL,>=)
_logp(dble,GT,_DVAL,>)
_logp(dble,LE,_DVAL,<=)
_logp(dble,LT,_DVAL,<)
_logp(dble,NE,_DVAL,!=)

/*
 *  Functions on LOGICAL's
 */
ADIobj ADIlogAnd_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIlogical	result = ADI__true;
  ADIobj	rval = ADI__nullid;

  _ARGLOOP_1ST_TO_NTH_AND( carg, result )
    if ( ! _LVAL(*carg) )
      result = ADI__false;

  adic_newv0l( result, &rval, status );

  return rval;
  }

ADIobj ADIlogNot_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0l( ! _LVAL(args[0]), &rval, status );

  return rval;
  }

ADIobj ADIlogOr_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIlogical	result = ADI__false;
  ADIobj	rval = ADI__nullid;

  _ARGLOOP_1ST_TO_NTH_AND( carg, (! result) )
    if ( _LVAL(*carg) )
      result = ADI__true;

  adic_newv0l( result, &rval, status );

  return rval;
  }

ADIobj ADIlogQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0l( _logical_q(args[0]), &rval, status );

  return rval;
  }

ADIobj ADIlogXor_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0l( _LVAL(args[0]) != _LVAL(args[1]), &rval, status );

  return rval;
  }

/*
 *  Functions on Strings's
 */

#define _strp(_ocode,_oop) \
ADIobj ADIstr##_ocode##_i( int narg, ADIobj args[], ADIstatus status ) \
  {\
  ADIobj	rval = ADI__nullid;\
  ADIlogical	res = ADI__false;\
  ADIstring	*sdat = _str_data(args[0]);\
  if ( sdat->len > 0 ) res = _oop( *(sdat->data) );\
  adic_newv0l( res ? ADI__true : ADI__false, &rval, status );\
  return rval;}

_strp(AlnumQ,isalnum)
_strp(AlphaQ,isalpha)
_strp(DigitQ,isdigit)
_strp(CntrlQ,iscntrl)
_strp(AsciiQ,isascii)
_strp(PrintQ,isprint)
_strp(LowerQ,islower)
_strp(UpperQ,isupper)
_strp(SpaceQ,isspace)
_strp(PunctQ,ispunct)
_strp(XdigitQ,isxdigit)

#define _slogp(_ocode,_oop) \
ADIobj ADIstr##_ocode##_i( int narg, ADIobj args[], ADIstatus status ) \
  {\
  ADIobj	rval = ADI__nullid;\
  ADIlogical	res;\
  res = strx_cmp( args[0], args[1] ) _oop 0;\
  adic_newv0l( res ? ADI__true : ADI__false, &rval, status );\
  return rval;}

_slogp(EQ,==)
_slogp(GE,>=)
_slogp(GT,>)
_slogp(LE,<=)
_slogp(LT,<)
_slogp(NE,!=)


ADIobj ADIstrCapit_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;
  ADIstring	*sdat;

  rval = ADIexprOwnArg( args, status );
  if ( _ok(status) ) {
    sdat = _str_data(rval);

    if ( sdat->len )
      if ( islower( *sdat->data ) )
	*sdat->data -= 'a' - 'A';
    }

  return rval;
  }

ADIobj ADIstrChar_i( int narg, ADIobj args[], ADIstatus status )
  {
  int		iarg;
  ADIobj	rval = ADI__nullid;
  ADIstring	*sdat;

  adic_new0c_n( narg, &rval, status );

  if ( _ok(status) ) {
    sdat = _str_data( rval );
    iarg = 0;
    while ( iarg < narg ) {
      sdat->data[iarg] = (char) _IVAL(args[iarg]);
      iarg++;
      }
    }

  return rval;
  }

ADIobj ADIstrChars_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;	/* Return object */
  int		i;			/* Loop over string */
  ADIobj	str;			/* A new string */
  ADIstring	*sdat = _str_data(args[0]);

  for ( i=sdat->len-1; i>=0; i-- ) {

/* Make single character string */
    adic_newv0c_n( sdat->data + i, 1, &str, status );

    rval = lstx_cell( str, rval, status );
    }

  return rval;
  }

ADIobj ADIstrConcat_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIinteger	len = 0;		/* Total output length */
  ADIobj	rval = ADI__nullid;	/* The output string */
  ADIstring	*sdat;
  char		*sptr;			/* Output data cursor */
  ADIobj	*vs_ptr;		/* Loop over stack */

/* Total length of all strings */
  _ARGLOOP_1ST_TO_NTH(vs_ptr)
    len += _str_len(*vs_ptr);

/* Allocate new string */
  adic_new0c_n( len, &rval, status );

/* Allocation went ok, and output string is of non-zero length */
  if ( _ok(status) && len ) {

/* Start of o/p string */
    sptr = _str_dat(rval);

    _ARGLOOP_1ST_TO_NTH(vs_ptr) {

      sdat = _str_data(*vs_ptr);

/* This string non-empty? */
      if ( sdat->len ) {

/* Copy to output */
	memcpy( sptr, sdat->data, sdat->len );

/* Advance data pointer */
	sptr += sdat->len;
	}
      }
    }

  return rval;
  }

ADIobj ADIstrIchar_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;
  ADIstring	*sdat = _str_data(args[0]);
  ADIinteger	res = 0;

  if ( sdat->len )
    res = sdat->data[0];

  adic_newv0i( res, &rval, status );

  return rval;
  }

ADIobj ADIstrLen_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0i( _str_len(args[0]), &rval, status );

  return rval;
  }

ADIobj ADIstrLower_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;
  ADIstring	*sdat;
  char		*ic;
  int		i;

  rval = ADIexprOwnArg( args, status );
  if ( _ok(status) ) {
    sdat = _str_data(rval);

    i = sdat->len; ic = sdat->data;
    while ( i-- ) {
      if ( isupper(*ic) )
	*ic += 'a' - 'A';
      ic++;
      }
    }

  return rval;
  }

ADIobj ADIstrRvrse_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;
  ADIstring	*sdat;
  char		*ic1,*ic2;
  char		tmp;
  int		i;

  rval = ADIexprOwnArg( args, status );
  if ( _ok(status) ) {
    sdat = _str_data(rval);

    i = sdat->len / 2;
    ic1 = sdat->data;
    ic2 = sdat->data + sdat->len - 1;
    while ( i-- ) {
      tmp = *ic1;
      *ic1 = *ic2;
      *ic2 = tmp;
      ic1++;
      ic2--;
      }
    }

  return rval;
  }

ADIobj ADIstrQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0l( _str_q(args[0]), &rval, status );

  return rval;
  }

ADIobj ADIstrSimQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;
  ADIlogical	res;

  res = (strx_cmpi( args[0], args[1] ) == 0);

  adic_newv0l( res ? ADI__true : ADI__false, &rval, status );

  return rval;
  }

ADIobj ADIstrTake_i( int narg, ADIobj args[], ADIstatus status )
  {
  char		*iptr;			/* Input data pointer */
  ADIinteger	itake;			/* Number of characters to take */
  ADIobj	rval = ADI__nullid;	/* The output string */
  ADIstring	*sdat = _str_data(args[0]);

/* Number of characters wanted */
  itake = _IVAL(args[1]);

/* Can't take more than the length - copy in this case */
  if ( abs(itake) > sdat->len )
    rval = ADIexprOwnArg( args, status );

/* Zero take */
  else if ( ! itake )
    adic_new0c( &rval, status );

/* Take first few characters */
  else if ( itake > 0 )
    adic_newv0c_n( sdat->data, itake, &rval, status );

/* Take last few characters */
  else if ( itake < 0 )
    adic_newv0c_n( sdat->data + sdat->len + itake, -itake, &rval, status );

  return rval;
  }

ADIobj ADIstrUpper_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;
  ADIstring	*sdat;
  char		*ic;
  int		i;

  rval = ADIexprOwnArg( args, status );
  if ( _ok(status) ) {
    sdat = _str_data(rval);

    i = sdat->len; ic = sdat->data;
    while ( i-- ) {
      if ( islower(*ic) )
	*ic -= 'a' - 'A';
      ic++;
      }
    }

  return rval;
  }

/*
 *  Functions on List's
 */
ADIobj ADIlstLen_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0i( lstx_len( args[0], status ), &rval, status );

  return rval;
  }

ADIobj ADIlstMk_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIobj	oarg;
  ADIobj	rval = ADI__nullid;
  ADIobj	*ipoint = &rval;

  _chk_stat_ret(ADI__nullid);

  _ARGLOOP_1ST_TO_NTH(carg) {

/* Ensure we own the base version of this data */
    oarg = ADIexprOwnArg( carg, status );

/* Insert into the list we're making */
    *ipoint = lstx_cell( oarg, ADI__nullid, status );
    ipoint = &_CDR(*ipoint);
    }

  return rval;
  }

ADIobj ADIlstQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_newv0l( _list_q(args[0]), &rval, status );

  return rval;
  }



void ADIfuncInit( ADIstatus status )
  {
  DEFINE_FUNC_TABLE(gen_funcs)
    FUNC_TENTRY( "Name(_)",				ADIgenName_i,	0 ),
    FUNC_TENTRY( "Type(_)",				ADIgenType_i,	0 ),
  END_FUNC_TABLE;

  DEFINE_FUNC_TABLE(int_funcs)
    FUNC_TENTRY( "Abs(_INTEGER)",			ADIintAbs_i,	0 ),
    FUNC_TENTRY( "And(__INTEGER)",			ADIintAnd_i,	0 ),
    FUNC_TENTRY( "Divide(_INTEGER,_INTEGER)",		ADIintDiv_i,	0 ),
    FUNC_TENTRY( "Equal(_INTEGER,_INTEGER)", 		ADIintEQ_i,	0 ),
    FUNC_TENTRY( "EvenQ(_INTEGER)",			ADIintEvenQ_i,	0 ),
    FUNC_TENTRY( "GCD(_INTEGER,_INTEGER)", 		ADIintGCD_i,	0 ),
    FUNC_TENTRY( "GreaterThan(_INTEGER,_INTEGER)", 	ADIintGT_i,	0 ),
    FUNC_TENTRY( "GreaterThanOrEqual(_INTEGER,_INTEGER)", ADIintGE_i,	0 ),
    FUNC_TENTRY( "Integer(_REAL)",			ADIintCaste_i,	0 ),
    FUNC_TENTRY( "Integer(_DOUBLE)",			ADIintCaste_i,	0 ),
    FUNC_TENTRY( "IntegerQ(_)",				ADIintQ_i,	0 ),
    FUNC_TENTRY( "LessThan(_INTEGER,_INTEGER)", 	ADIintLT_i,	0 ),
    FUNC_TENTRY( "LessThanOrEqual(_INTEGER,_INTEGER)", 	ADIintLE_i,	0 ),
    FUNC_TENTRY( "Max(__INTEGER)",			ADIintMax_i,	0 ),
    FUNC_TENTRY( "Min(__INTEGER)",			ADIintMin_i,	0 ),
    FUNC_TENTRY( "Mod(_INTEGER,_INTEGER)", 		ADIintMod_i,	0 ),
    FUNC_TENTRY( "Multiply(__INTEGER)",			ADIintMult_i,	0 ),
    FUNC_TENTRY( "Negate(_INTEGER)",			ADIintNeg_i,	0 ),
    FUNC_TENTRY( "Not(_INTEGER)",			ADIintNot_i,	0 ),
    FUNC_TENTRY( "NotEqual(_INTEGER,_INTEGER)", 	ADIintNE_i,	0 ),
    FUNC_TENTRY( "OddQ(_INTEGER)",			ADIintOddQ_i,	0 ),
    FUNC_TENTRY( "Or(__INTEGER)",			ADIintOr_i,	0 ),
    FUNC_TENTRY( "Plus(__INTEGER)",			ADIintPlus_i,	0 ),
    FUNC_TENTRY( "Shift(_INTEGER,_INTEGER)",		ADIintShift_i,	0 ),
    FUNC_TENTRY( "Subtract(_INTEGER,_INTEGER)", 	ADIintSub_i,	0 ),
    FUNC_TENTRY( "Xor(_INTEGER,_INTEGER)",		ADIintXor_i,	0 ),
  END_FUNC_TABLE;

  DEFINE_FUNC_TABLE(list_funcs)
    FUNC_TENTRY( "Length(_List)",			ADIlstLen_i,	0 ),
    FUNC_TENTRY( "List(__)",				ADIlstMk_i,	0 ),
    FUNC_TENTRY( "ListQ(_)",				ADIlstQ_i,	0 ),
  END_FUNC_TABLE;

  DEFINE_FUNC_TABLE(real_funcs)
    FUNC_TENTRY( "Abs(_REAL)",				ADIrealAbs_i,	0 ),
    FUNC_TENTRY( "ArcCos(_REAL)",			ADIrealArcCos_i,0 ),
    FUNC_TENTRY( "ArcSin(_REAL)",			ADIrealArcSin_i,0 ),
    FUNC_TENTRY( "ArcTan(_REAL)",			ADIrealArcTan_i,0 ),
    FUNC_TENTRY( "ArcTan(_REAL,_REAL)",			ADIrealArcTan2_i,0 ),
    FUNC_TENTRY( "Cos(_REAL)",				ADIrealCos_i,	0 ),
    FUNC_TENTRY( "Divide(_REAL,_REAL)",  		ADIrealDiv_i,   0 ),
    FUNC_TENTRY( "Equal(_REAL,_REAL)", 			ADIrealEQ_i,	0 ),
    FUNC_TENTRY( "Exp(_REAL)",				ADIrealExp_i,	0 ),
    FUNC_TENTRY( "GreaterThan(_REAL,_REAL)", 		ADIrealGT_i,	0 ),
    FUNC_TENTRY( "GreaterThanOrEqual(_REAL,_REAL)", 	ADIrealGE_i,	0 ),
    FUNC_TENTRY( "LessThan(_REAL,_REAL)", 		ADIrealLT_i,	0 ),
    FUNC_TENTRY( "LessThanOrEqual(_REAL,_REAL)", 	ADIrealLE_i,	0 ),
    FUNC_TENTRY( "Log(_REAL)",				ADIrealLog_i,	0 ),
    FUNC_TENTRY( "Log10(_REAL)",			ADIrealLog10_i,	0 ),
    FUNC_TENTRY( "Max(__REAL)",				ADIrealMax_i,	0 ),
    FUNC_TENTRY( "Min(__REAL)",				ADIrealMin_i,	0 ),
    FUNC_TENTRY( "Multiply(__REAL)",			ADIrealMult_i,	0 ),
    FUNC_TENTRY( "Negate(_REAL)",			ADIrealNeg_i,	0 ),
    FUNC_TENTRY( "NotEqual(_REAL,_REAL)",  		ADIrealNE_i,	0 ),
    FUNC_TENTRY( "Plus(__REAL)",			ADIrealPlus_i,	0 ),
    FUNC_TENTRY( "Real(_DOUBLE)",			ADIrealCaste_i,	0 ),
    FUNC_TENTRY( "Real(_INTEGER)",			ADIrealCaste_i,	0 ),
    FUNC_TENTRY( "RealQ(_)",				ADIrealQ_i,	0 ),
    FUNC_TENTRY( "Sin(_REAL)",				ADIrealSin_i,	0 ),
    FUNC_TENTRY( "Sqrt(_REAL)",				ADIrealSqrt_i,	0 ),
    FUNC_TENTRY( "Subtract(_REAL,_REAL)",  		ADIrealSub_i,	0 ),
    FUNC_TENTRY( "Tan(_REAL)",				ADIrealTan_i,	0 ),
  END_FUNC_TABLE;

  DEFINE_FUNC_TABLE(log_funcs)
    FUNC_TENTRY( "And(__LOGICAL)",			ADIlogAnd_i,	0 ),
    FUNC_TENTRY( "LogicalQ(_)",				ADIlogQ_i,	0 ),
    FUNC_TENTRY( "Not(_LOGICAL)",			ADIlogNot_i,	0 ),
    FUNC_TENTRY( "Or(__LOGICAL)",			ADIlogOr_i,	0 ),
    FUNC_TENTRY( "Xor(_LOGICAL,_LOGICAL)",		ADIlogXor_i,	0 ),
  END_FUNC_TABLE;

  DEFINE_FUNC_TABLE(string_funcs)
    FUNC_TENTRY( "AlnumQ(_)",				ADIstrAlnumQ_i, 0 ),
    FUNC_TENTRY( "AlphaQ(_)",				ADIstrAlphaQ_i, 0 ),
    FUNC_TENTRY( "AsciiQ(_)",				ADIstrAsciiQ_i, 0 ),
    FUNC_TENTRY( "Capitalise(_CHAR)",			ADIstrCapit_i,  0 ),
    FUNC_TENTRY( "Char(__INTEGER)",			ADIstrChar_i,	0 ),
    FUNC_TENTRY( "Characters(_CHAR)",			ADIstrChars_i,	0 ),
    FUNC_TENTRY( "CntrlQ(_)",				ADIstrCntrlQ_i, 0 ),
    FUNC_TENTRY( "Concat(__CHAR)",			ADIstrConcat_i, 0 ),
    FUNC_TENTRY( "DigitQ(_)",				ADIstrDigitQ_i, 0 ),
    FUNC_TENTRY( "Equal(_CHAR,_CHAR)",			ADIstrEQ_i,	0 ),
    FUNC_TENTRY( "GreaterThan(_CHAR,_CHAR)", 		ADIstrGT_i,	0 ),
    FUNC_TENTRY( "GreaterThanOrEqual(_CHAR,_CHAR)", 	ADIstrGE_i,	0 ),
    FUNC_TENTRY( "Ichar(_CHAR)",			ADIstrIchar_i,	0 ),
    FUNC_TENTRY( "LessThan(_CHAR,_CHAR)", 		ADIstrLT_i,	0 ),
    FUNC_TENTRY( "LessThanOrEqual(_CHAR,_CHAR)", 	ADIstrLE_i,	0 ),
    FUNC_TENTRY( "Length(_CHAR)",			ADIstrLen_i,	0 ),
    FUNC_TENTRY( "LowerCase(_CHAR)",			ADIstrLower_i,  0 ),
    FUNC_TENTRY( "LowerQ(_)",				ADIstrLowerQ_i, 0 ),
    FUNC_TENTRY( "NotEqual(_CHAR,_CHAR)",		ADIstrNE_i,	0 ),
    FUNC_TENTRY( "PrintQ(_)",				ADIstrPrintQ_i, 0 ),
    FUNC_TENTRY( "PunctQ(_)",				ADIstrPunctQ_i, 0 ),
    FUNC_TENTRY( "Reverse(_CHAR)",			ADIstrRvrse_i,  0 ),
    FUNC_TENTRY( "SimilarQ(_CHAR,_CHAR)",		ADIstrSimQ_i,	0 ),
    FUNC_TENTRY( "SpaceQ(_)",				ADIstrSpaceQ_i, 0 ),
    FUNC_TENTRY( "StringQ(_)",				ADIstrQ_i,	0 ),
    FUNC_TENTRY( "Take(_CHAR,_INTEGER)",		ADIstrTake_i,	0 ),
    FUNC_TENTRY( "UpperCase(_CHAR)",			ADIstrUpper_i,  0 ),
    FUNC_TENTRY( "UpperQ(_)",				ADIstrUpperQ_i, 0 ),
    FUNC_TENTRY( "XdigitQ(_)",				ADIstrXdigitQ_i,0 ),
  END_FUNC_TABLE;

  DEFINE_FUNC_TABLE(dble_funcs)
    FUNC_TENTRY( "Abs(_DOUBLE)",			ADIdbleAbs_i,	0 ),
    FUNC_TENTRY( "ArcCos(_DOUBLE)",			ADIdbleArcCos_i,0 ),
    FUNC_TENTRY( "ArcSin(_DOUBLE)",			ADIdbleArcSin_i,0 ),
    FUNC_TENTRY( "ArcTan(_DOUBLE)",			ADIdbleArcTan_i,0 ),
    FUNC_TENTRY( "ArcTan(_DOUBLE,_DOUBLE)",		ADIdbleArcTan2_i,0 ),
    FUNC_TENTRY( "Cos(_DOUBLE)",			ADIdbleCos_i,	0 ),
    FUNC_TENTRY( "Divide(_DOUBLE,_DOUBLE)",      	ADIdbleDiv_i,	0 ),
    FUNC_TENTRY( "Equal(_DOUBLE,_DOUBLE)", 		ADIdbleEQ_i,	0 ),
    FUNC_TENTRY( "Exp(_DOUBLE)",			ADIdbleExp_i,	0 ),
    FUNC_TENTRY( "GreaterThan(_DOUBLE,_DOUBLE)", 	ADIdbleGT_i,	0 ),
    FUNC_TENTRY( "GreaterThanOrEqual(_DOUBLE,_DOUBLE)", ADIdbleGE_i,	0 ),
    FUNC_TENTRY( "LessThan(_DOUBLE,_DOUBLE)", 		ADIdbleLT_i,	0 ),
    FUNC_TENTRY( "LessThanOrEqual(_DOUBLE,_DOUBLE)", 	ADIdbleLE_i,	0 ),
    FUNC_TENTRY( "Log(_DOUBLE)",			ADIdbleLog_i,	0 ),
    FUNC_TENTRY( "Log10(_DOUBLE)",			ADIdbleLog10_i,	0 ),
    FUNC_TENTRY( "Max(__DOUBLE)",			ADIdbleMax_i,	0 ),
    FUNC_TENTRY( "Min(__DOUBLE)",			ADIdbleMin_i,	0 ),
    FUNC_TENTRY( "Multiply(__DOUBLE)",			ADIdbleMult_i,	0 ),
    FUNC_TENTRY( "Negate(_DOUBLE)",			ADIdbleNeg_i,	0 ),
    FUNC_TENTRY( "NotEqual(_DOUBLE,_DOUBLE)",  		ADIdbleNE_i,	0 ),
    FUNC_TENTRY( "Plus(__DOUBLE)",		    	ADIdblePlus_i,	0 ),
    FUNC_TENTRY( "Double(_INTEGER)",			ADIdbleCaste_i,	0 ),
    FUNC_TENTRY( "Double(_REAL)",			ADIdbleCaste_i,	0 ),
    FUNC_TENTRY( "DoubleQ(_)",				ADIdbleQ_i,	0 ),
    FUNC_TENTRY( "Sin(_DOUBLE)",			ADIdbleSin_i,	0 ),
    FUNC_TENTRY( "Sqrt(_DOUBLE)",			ADIdbleSqrt_i,	0 ),
    FUNC_TENTRY( "Subtract(_DOUBLE,_DOUBLE)",  		ADIdbleSub_i,	0 ),
    FUNC_TENTRY( "Tan(_DOUBLE)",			ADIdbleTan_i,	0 ),
  END_FUNC_TABLE;

  _chk_stat;

/* Install functions */
  ADIkrnlAddFuncs( gen_funcs, status );
  ADIkrnlAddFuncs( int_funcs, status );
  ADIkrnlAddFuncs( list_funcs, status );
  ADIkrnlAddFuncs( real_funcs, status );
  ADIkrnlAddFuncs( dble_funcs, status );
  ADIkrnlAddFuncs( log_funcs, status );
  ADIkrnlAddFuncs( string_funcs, status );
  }
