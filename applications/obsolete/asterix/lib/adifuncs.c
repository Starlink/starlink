/*
 *
 *  Notes on functions :
 *
 *    Test:
 *
 *    New functions:
 *
 *	CardinalQ	- is object B|UB|W|UW|I
 *	NumberQ		- CardinalQ | R|D
 *	Shape/Size	-
 *	mk...		- series generators
 *	Round(x,np)	- round to specified number of places
 *	B|UB|W|UW 	- constructors
 *	Strip		- strip spaces from string
 *	Substr(s,b,n)	- sub-string op
 *	HMS2RAD		-
 *	RAD2HMS		- format radian angle to H,M,S
 *	FiniteQ		- number != infinity?
 *	BadQ		- number == bad value?
 *
 *    Other issues:
 *
 *    o And/Or shortcut evaluation
 *    o	Ensure handling of zero length strings/lists is consistent
 *    o Printing of non-prnt able types, eg. streams
 *    o precision of D.P. printing
 *    o Properties must be particular to a binding rather than a symbol. Affects
 *	evaluation logic. Needed for shortcut BOOLEAN ops. Find how MM does it
 *    o Casteing to support mixed mode maths
 *    o Coercion of bool values to T|F on input
 *    o Creation and of arrays
 *    o local and global statements
 */

#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <math.h>
#include <signal.h>
#include <time.h>
#include <sys/timeb.h>
#include <string.h>

#ifdef __MSDOS__
#include <process.h>
#else
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#endif

#include "asterix.h"                    /* Asterix definitions */

#include "aditypes.h"
#include "adimem.h"                     /* Allocation routines */
#include "adikrnl.h"                    /* Kernel code */
#include "adicface.h"                   /* C programmer interface */
#include "adifuncs.h"                   /* Prototypes for this sub-package */
#include "adiparse.h"
#include "adierror.h"                   /* ADI error handling */
#include "adilist.h"
#include "adisyms.h"
#include "adistrng.h"
#include "adiexpr.h"
#include "adiarray.h"
#include "aditable.h"
#include "adifsys.h"
#include "adipkg.h"

extern ADIstackFrame	ADI_G_fs[];

ADIclassDef	*cdef_B;
ADIclassDef	*cdef_UB;
ADIclassDef	*cdef_W;
ADIclassDef	*cdef_UW;
ADIclassDef	*cdef_I;
ADIclassDef	*cdef_R;
ADIclassDef	*cdef_D;
ADIclassDef	*cdef_L;

/*
 * Macro to define a logical operation on an arbitrary data type
 */
#define _logp(_tcode,_ocode,_tacc,_oop) \
ADIobj ADI##_tcode##_ocode##_i( int narg, ADIobj args[], ADIstatus status ) \
  {\
  ADIlogical	res;\
  res = _tacc(args[0]) _oop _tacc(args[1]);\
  return ADImkL( res ? ADI__true : ADI__false, status );}

#define _GEN_MK(_type,_T) \
ADIobj ADImk##_T( _type value, ADIstatus status ) { \
  return adix_cls_nallocd( cdef_##_T, 0, 0, (void *) &value, status );\
  }

_GEN_MK(ADIbyte,B)
_GEN_MK(ADIubyte,UB)
_GEN_MK(ADIword,W)
_GEN_MK(ADIuword,UW)
_GEN_MK(ADIinteger,I)
_GEN_MK(ADIreal,R)
_GEN_MK(ADIdouble,D)
_GEN_MK(ADIlogical,L)



void ADIexecCtrlC(int sig)
  {
  ADIstatype	lstatus = SAI__OK;

  if ( ADI_G_curint->exec.name != EXC_ControlC )
    ADIexecRaiseI( EXC_ControlC, SAI__OK, "CTRL-C detected", _CSM, &lstatus );

/* Re-start the handler */
  signal(SIGINT,ADIexecCtrlC);
  }


/*
 *  System interface functions
 */
ADIobj ADIsysGetEnv_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;
  char		*estr;
  ADIstring	*sdat;
  ADIobj	tstr;

/* Ensure null terminated string */
  tstr = ADIstrngEnsureNterm( args[0], status );
  sdat = _str_data(tstr);

  if ( sdat->data )
    estr = getenv( sdat->data );
  else
    estr = NULL;

/* Delete temporary string */
  adix_erase( &tstr, 1, status );

  if ( estr )
    adic_newv0c( estr, &rval, status );

  return rval;
  }

ADIobj ADIsysGetPid_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIinteger	pid;

  pid = (ADIinteger) getpid();

  return ADImkI( pid, status );
  }

/*
 *  General purpose functions
 */
ADIobj ADIgenAtomQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIlogical	res = ADI__false;

  if ( _valid_q(args[0]) )
    res = _DTDEF(args[0])->prim;

  return ADImkL( res, status );
  }

ADIobj ADIgenBreak_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIexecRaiseI( EXC_ScopeBreak, SAI__OK, NULL, 0, status );

  return ADI__nullid;
  }

ADIobj ADIgenDefProc_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	dhead,dargs,newid;
  ADImethod	mth;

/* Extract name and arguments from first input argument */
  _GET_HEDARG(dhead,dargs,args[0]);

/* Store method stuff */
  mth.args = adix_clone( dargs, status );
  mth.form = ADI__nullid;
  mth.exec = adix_clone( args[1], status );

/* Allocate the new method and fill in the fields */
  newid = ADIkrnlNewMth( &mth, status );

/* Create function symbol binding and add to symbol table */
  ADIsymAddBind( adix_clone( dhead, status ), -1,
		 ADIsbindNew( ADI__fun_sb, newid, status ),
		 status );

  return ADI__nullid;
  }

ADIobj ADIgenDefRep_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	dummy;
  ADIstring	*sptr = _str_data(args[0]);

  adix_defrep( sptr->data, sptr->len, &dummy, status );

  return ADI__nullid;
  }

ADIobj ADIgenDoWhile_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIlogical	isbool;
  ADIobj        robj;                   /* Return value */
  ADIobj        test;                   /* Result of test expression */
  ADIlogical    tval = ADI__true;       /* Test value */

  while ( _ok(status) && tval ) {

/* Execute actions */
    robj = ADIexprEvalList( vs_base[0], ADI__nullid, status );

/* Destroy result */
    if ( _valid_q(robj) )
      adix_erase( &robj, 1, status );

/* Evaluate test expression */
    test = ADIexprEval( vs_base[1], ADI__nullid, ADI__false, status );

    if ( _ok(status) ) {

      isbool = _logical_q(test);
      if ( isbool )
	tval = _LVAL(test);

      adix_erase( &test, 1, status );

      if ( ! isbool )
	ADIexecRaiseI( EXC_BoolExp, ADI__INVARG,
		     "Logical expression expected", _CSM, status );
      }
    }

  if ( *status == ADI__UNWIND )         /* Trap the break exception */
    ADIexecAcceptI( EXC_ScopeBreak, status );

  return ADI__nullid;
  }

ADIobj ADIgenGet_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	tstr;
  FILE	*fp;

/* Ensure null terminated string */
  tstr = ADIstrngEnsureNterm( args[0], status );

  fp = fopen( _str_dat(tstr), "r" );

  if ( fp )
    ADIstrmExtendFile( ADI_G_curint->StdIn, tstr, fp, status );
  else {
/* Delete temporary string */
    adix_erase( &tstr, 1, status );

    ADIexecRaiseI( EXC_InvalidArg, ADI__INVARG, "No such file %O\n", _CSM,
		status, args[0] );
    }

  return ADI__nullid;
  }

ADIobj ADIgenGlobal_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	dbind;
  ADIobj	name;
  ADIobj	*vs_ptr;

  _ARGLOOP_1ST_TO_NTH(vs_ptr) {

/* Extract symbol name */
    name = _etn_head(*vs_ptr);

/* Look for definition binding */
    dbind = ADIsymFind( name, -1, ADI__true, ADI__defn_sb, status );

/* If it doesn't exist, create it */
    if ( ! _valid_q(dbind) )
      ADIsymAddBind( name, -1,
		     ADIsbindNew( ADI__defn_sb, ADI__nullid, status ),
		     status );
    }

  return ADI__nullid;
  }

ADIobj ADIgenHst_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIstrmPrintf( "Common string table: \n\n", status );
  tblx_hstats( ADI_G_commonstrings, status );
  ADIstrmPrintf( "\nMaster symbol table: \n\n", status );
  tblx_hstats( ADI_G_fs[0].syms, status );

  return ADI__nullid;
  }

ADIobj ADIgenIf_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIlogical	isbool;
  int           nleft = narg;           /* Number of arguments left */
  ADIobj        robj;                   /* Return value */
  ADIobj        test;                   /* Result of test expression */
  ADIlogical	tval;
  ADIobj        *vptr = vs_base;        /* Loop over arguments */

  while ( (nleft>0) && _ok(status) ) {

    if ( nleft == 1 )
      robj = ADIexprEvalList( *vptr, ADI__nullid, status );

    else {

/* Evaluate test expression */
      test = ADIexprEval( *vptr, ADI__nullid, ADI__false, status );

      if ( _ok(status) ) {

/* Must be Boolean */
	isbool = _logical_q(test);
	if ( isbool )
	  tval = _LVAL(test);
	adix_erase( &test, 1, status );

	if ( isbool ) {

/* Advance to action for truth */
	  nleft--; vptr++;

/* Condition was true */
	  if ( tval ) {

/* To terminate loop */
	    nleft = 0;

/* True actions */
	    robj = ADIexprEvalList( *vptr, ADI__nullid, status );

	    if ( _valid_q(robj) )                 /* Destroy result */
	      adix_erase( &robj, 1, status );
	    }
	  }
	else
	  ADIexecRaiseI( EXC_BoolExp, ADI__INVARG,
		"Logical expression expected in if..else..endif", _CSM, status );

	adix_erase( &test, 1, status );       /* Remove test result protected */
	}
      }
    nleft--; vptr++;
    }

  return ADI__nullid;
  }

ADIobj ADIgenLocal_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	dbind;
  ADIobj	name;
  ADIobj	*vs_ptr;

  _ARGLOOP_1ST_TO_NTH(vs_ptr) {

/* Extract symbol name */
    name = _etn_head(*vs_ptr);

/* Look for definition binding */
    dbind = ADIsymFind( name, 1, ADI__true, ADI__defn_sb, status );

/* If it doesn't exist, create it */
    if ( ! _valid_q(dbind) )
      ADIsymAddBind( name, 1,
		   ADIsbindNew( ADI__defn_sb, ADI__nullid, status ),
		   status );
    }

  return ADI__nullid;
  }

ADIobj ADIgenLtime_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	res = ADI__nullid;
  struct tm	*tblock;
  time_t	timer;

/* Gets time of day */
  timer = time(NULL);

/* Get user supplied timezone offset */
  if ( narg )
    timer += _IVAL(args[0]) * 3600;

/* Converts date/time to a structure */
  tblock = localtime(&timer);

  res = lstx_cell( ADImkI( tblock->tm_sec, status ), res, status );
  res = lstx_cell( ADImkI( tblock->tm_min, status ), res, status );
  res = lstx_cell( ADImkI( tblock->tm_hour, status ), res, status );
  res = lstx_cell( ADImkI( tblock->tm_mday, status ), res, status );
  res = lstx_cell( ADImkI( tblock->tm_mon, status ), res, status );
  res = lstx_cell( ADImkI( tblock->tm_year, status ), res, status );

  return res;
  }


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

ADIobj ADIgenNullQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADImkL( _null_q(args[0]), status );
  }

ADIobj ADIgenPrint_i( int narg, ADIobj args[], ADIstatus status )
  {
  int		iarg = 0;
  ADIobj	*vs_ptr = args;

  while ( iarg < narg ) {
    if ( iarg )
      ADIstrmPrintf( " %O", status, *vs_ptr );
    else {
      ADIstrmPrintf( "%O", status, *vs_ptr );
      iarg++;
      }
    vs_ptr++;
    }

  ADIstrmPrintf( "\n", status );
  ADIstrmFlush( status );

  return ADI__nullid;
  }

ADIobj ADIgenRaise_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	edata;

  if ( narg > 1 )
    edata = ADIexprOwnArg( args+1, status );

  ADIexecRaiseI( ADIexprOwnArg( args, status ), SAI__OK, NULL, 0, status );

  if ( narg > 1 )
    ADI_G_curint->exec.errtext = edata;

  return ADI__nullid;
  }

ADIobj ADIgenRequire_i( int narg, ADIobj args[], ADIstatus status )
  {
  int		iarg;
  ADIstring	*sptr;

  for( iarg = 0; iarg < narg; iarg++ ) {
    sptr = _str_data(args[iarg]);
    ADIpkgRequire( sptr->data, sptr->len, status );
    }

  return ADI__nullid;
  }

ADIobj ADIgenReturn_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval;

/* Ensure ownership of return value */
  rval = ADIexprOwnArg( args, status );

  ADIexecRaiseI( EXC_ReturnValue, SAI__OK, NULL, 0, status );

  ADI_G_curint->exec.errtext = rval;

  return ADI__nullid;
  }

ADIobj ADIgenProbe_i( int narg, ADIobj args[], ADIstatus status )
  {
  adic_probe( status );

  return ADI__nullid;
  }

ADIobj ADIgenSet_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	dbind;
  ADIobj	name;
  ADIobj	rval = ADI__nullid;

  rval = ADIexprOwnArg( args+1, status );

/* Extract symbol name */
  name = _etn_head(args[0]);

/* Look for definition binding */
  dbind = ADIsymFind( name, 0, ADI__true, ADI__defn_sb, status );

/* If it exists, delete definition object from existing binding */
  if ( _valid_q(dbind) ) {
    ADIobj	*daddr = &_sbind_defn(dbind);
    adix_erase( daddr, 1, status );
    *daddr = rval;
    }

/* Otherwise create a new one */
  else
    ADIsymAddBind( name, 0,
		   ADIsbindNew( ADI__defn_sb, rval, status ),
		   status );

  return adix_clone( rval, status );
  }

ADIobj ADIgenTime_i( int narg, ADIobj args[], ADIstatus status )
  {
  time_t	timer;

  timer = time(NULL);

  if ( narg )
    timer += _IVAL(args[0])*3600;

  return ADImkI( timer, status );
  }

ADIobj ADIgenTimeIt_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIlogical	changed;
  double	diff;
  ADIinteger	nit = _IVAL(args[1]);
  ADIinteger	ic = nit;
  ADIobj	res;
#ifdef __MSDOS__
  struct timeb	start,stop;
#elif NO_GETTOD
  struct tms dummy2;
  long start, stop;
#else
  struct timeval start, stop;
  struct timezone tz;
  int micros;
#endif

#ifdef __MSDOS__
  ftime( &start );
#elif NO_GETTOD
  start = times(&dummy2);
#else
  gettimeofday(&start, &tz);
#endif

  while ( ic-- ) {
    res = ADIexprEval( args[0], ADI__nullid, ADI__false, status );
    adix_erase( &res, 1, status );
    }

#ifdef __MSDOS__
  ftime( &stop );
  diff = ((double) (stop.time - start.time))*1000000.0 +
	  ((double) (stop.millitm-start.millitm))*1000.0;
#elif NO_GETTOD
  stop = times(&dummy2);
  diff = (((double) (stop - start))*1000000.0)/CLK_TCK;
#else
  gettimeofday(&stop, &tz);
  micros = (stop.tv_sec - start.tv_sec)*1000000
	    + (stop.tv_usec - start.tv_usec);
  diff = micros;
#endif

  ADIstrmPrintf( "Time is %I microseconds per iteration\n", status,
		 (ADIinteger) (diff / ((double) nit)) );
  ADIstrmFlush( status );

  return ADI__nullid;
  }

ADIobj ADIgenType_i( int narg, ADIobj args[], ADIstatus status )
  {
  return adix_clone( _DTDEF(args[0])->aname, status );
  }

ADIobj ADIgenTzone_i( int narg, ADIobj args[], ADIstatus status )
  {
#ifdef __MSDOS__
  return ADImkI( _timezone / 3600, status );
#else
  return ADImkI( timezone / 3600, status );
#endif
  }

ADIobj ADIgenWhile_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIlogical	isbool;
  ADIobj        robj;                   /* Return value */
  ADIobj        test;                   /* Result of test expression */
  ADIlogical    tval = ADI__true;       /* Test value */

  while ( _ok(status) && tval ) {

/* Evaluate test expression */
    test = ADIexprEval( *vs_base, ADI__nullid, ADI__false, status );

/* Logical result? */
    isbool = _logical_q(test);
    if ( isbool )
      tval = _LVAL(test);

/* Destroy test result */
    adix_erase( &test, 1, status );

/* Result was correct type? */
    if ( isbool ) {

/* Execute actions if true */
      if ( tval ) {

	robj = ADIexprEvalList( vs_base[1], ADI__nullid, status );

	adix_erase( &robj, 1, status );
	}
      }
    else if ( _ok(status) ) {
*status = ADI__SYNTAX;
/*      EOSexecRaise( K_STR(EXC_BoolExp), NULL, status ); */
      }
    }

/*  if ( *status == EOS__UNWIND )         /* Trap the break exception */
/*    EOSexecAccept( K_STR(EXC_ScopeBreak),
				status ); */

  return ADI__nullid;
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

  result = _IVAL(args[0]);

  _ARGLOOP_2ND_TO_NTH(carg)
    result &= _IVAL(*carg);

  return ADImkI( result, status );
  }

ADIobj ADIintCaste_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIinteger	value;

  adic_get0i( args[0], &value, status );

  return ADImkI( value, status );
  }

ADIobj ADIintDiv_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIinteger	d1 = _IVAL(args[0]);
  ADIinteger	d2 = _IVAL(args[1]);
  ADIobj	rval = ADI__nullid;

  if ( d2 == 0 )
    ADIexecRaiseI( EXC_MathErr, ADI__INVARG, "Division by zero", _CSM,
		   status );
  else
    rval = ADImkI( d1 / d2, status );

  return rval;
  }

ADIobj ADIintEvenQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADImkL( ! (_IVAL(args[0]) % 2), status );
  }

ADIobj ADIintFact_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIinteger	inp = _IVAL(args[0]);

  ADIobj	rval = ADI__nullid;

  if ( inp < 0 )
    ;
  else if ( inp < 13 ) {
    ADIinteger	res = 1;
    while ( inp > 1 )
      res *= inp--;

    rval = ADImkI( res, status );
    }

  else {
    ADIdouble	res = 479001600.0;

    while ( inp > 12 )
      res *= inp--;

    rval = ADImkD( res, status );
    }

  return rval;
  }

ADIobj ADIintGCD_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIinteger	a,b;			/* Arguments */
  ADIinteger	c,d,r;			/* Work variables */

  a = _IVAL(args[0]);
  b = _IVAL(args[1]);

  c = _MAX(a,b); d = _MIN(a,b);

  while ( d != 0 ) {
    r = c % d; c = d; d = r;
    }

  return ADImkI( c, status );
  }

ADIobj ADIintMax_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIinteger	ival, result;

  result = _IVAL(args[0]);

  _ARGLOOP_2ND_TO_NTH(carg) {
    ival = _IVAL(*carg);
    if ( ival > result )
      result = ival;
    }

  return ADImkI( result, status );
  }

ADIobj ADIintMin_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIinteger	ival, result;

  result = _IVAL(args[0]);

  _ARGLOOP_2ND_TO_NTH(carg) {
    ival = _IVAL(*carg);
    if ( ival < result )
      result = ival;
    }

  return ADImkI( result, status );
  }

ADIobj ADIintMod_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADImkI( _IVAL(args[0]) % _IVAL(args[1]), status );
  }

ADIobj ADIintMult_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIinteger	result = 1;

  _ARGLOOP_1ST_TO_NTH(carg)
    result *= _IVAL(*carg);

  return ADImkI( result, status );
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
  return ADImkL( (_IVAL(args[0]) % 2), status );
  }

ADIobj ADIintOr_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIinteger	result;

  result = _IVAL(args[0]);

  _ARGLOOP_2ND_TO_NTH(carg)
    result |= _IVAL(*carg);

  return ADImkI( result, status );
  }



ADIobj ADIintPlus_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIinteger	result = 0;

  _ARGLOOP_1ST_TO_NTH(carg)
    result += _IVAL(*carg);

  return ADImkI( result, status );
  }

ADIobj ADIintPow2_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIinteger	res = _IVAL(args[0]);

  return ADImkI( res*res, status );
  }

ADIobj ADIintPower_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIinteger	a1 = _IVAL(args[0]);
  ADIinteger	a2 = _IVAL(args[1]);

  return ADImkD( pow((ADIdouble) a1,(ADIdouble) a2), status );
  }

ADIobj ADIintQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADImkL( _int_q(args[0]), status );
  }

ADIobj ADIintShift_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIinteger	val,shift,res;

  val = _IVAL(args[0]);
  shift = _IVAL(args[1]);

  if ( shift > 0 )
    res = val >> shift;
  else if ( shift < 0 )
    res = val << (-shift);
  else
    res = val;

  return ADImkI( res, status );
  }

ADIobj ADIintSign_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIinteger	sign = 1;

  if ( _IVAL(args[1]) < 0 )
    sign = -1;

  return ADImkI( abs(_IVAL(args[0]))*sign, status );
  }

ADIobj ADIintSub_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADImkI( _IVAL(args[0]) - _IVAL(args[1]), status );
  }

ADIobj ADIintSum_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADImkI( 999, status );
  }


ADIobj ADIintXor_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADImkI( _IVAL(args[0]) ^ _IVAL(args[1]), status );
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
  return ADImkR( (ADIreal) _oop((ADIdouble) _RVAL(args[0])), status );}\

ADIobj ADIrealArcCos_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIreal	arg = _RVAL(args[0]);
  ADIobj	rval = ADI__nullid;

  if ( (arg < -1.0) || (arg > 1.0) )
    ADIexecRaiseI( EXC_MathErr, SAI__OK, "Domain error in ArcCos, |arg| > 1", _CSM,
		   status, arg );
  else
    rval = ADImkR( (ADIreal) acos((ADIdouble) arg), status );

  return rval;
  }

ADIobj ADIrealArcSin_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIreal	arg = _RVAL(args[0]);
  ADIobj	rval = ADI__nullid;

  if ( (arg < -1.0) || (arg > 1.0) )
    ADIexecRaiseI( EXC_MathErr, SAI__OK, "Domain error in ArcSin, |arg| > 1", _CSM,
		   status, arg );
  else
    rval = ADImkR( (ADIreal) asin((ADIdouble) arg), status );

  return rval;
  }

_math_rop(ArcTan,atan)
_math_rop(Ceil,ceil)
_math_rop(Cos,cos)
_math_rop(Cosh,cosh)
_math_rop(Exp,exp)
_math_rop(Floor,floor)
_math_rop(Log,log)
_math_rop(Log10,log10)
_math_rop(Sin,sin)
_math_rop(Sinh,sinh)

ADIobj ADIrealSqrt_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIreal	arg = _RVAL(args[0]);
  ADIobj	rval = ADI__nullid;

  if ( arg < 0.0 )
    ADIexecRaiseI( EXC_MathErr, SAI__OK, "Square root of negative number %f", _CSM,
		   status, arg );
  else
    rval = ADImkR( (ADIreal) sqrt((ADIdouble) arg), status );

  return rval;
  }

_math_rop(Tan,tan)
_math_rop(Tanh,tanh)

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
  ADIreal	value;

  value = atan2( _RVAL(args[0]), _RVAL(args[1]) );

  return ADImkR( value, status );
  }

ADIobj ADIrealCaste_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIreal	value;

  adic_get0r( args[0], &value, status );

  return ADImkR( value, status );
  }

ADIobj ADIrealDiv_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIreal	d1 = _RVAL(args[0]);
  ADIreal	d2 = _RVAL(args[1]);
  ADIobj	rval = ADI__nullid;

  if ( d2 == 0.0 )
    ADIexecRaiseI( EXC_MathErr, ADI__INVARG, "Division by zero", _CSM,
		   status );
  else
    rval = ADImkR( d1 / d2, status );

  return rval;
  }

ADIobj ADIrealMax_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIreal	ival, result;

  result = _RVAL(args[0]);

  _ARGLOOP_2ND_TO_NTH(carg) {
    ival = _RVAL(*carg);
    if ( ival > result )
      result = ival;
    }

  return ADImkR( result, status );
  }

ADIobj ADIrealMin_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIreal	ival, result;

  result = _RVAL(args[0]);

  _ARGLOOP_2ND_TO_NTH(carg) {
    ival = _RVAL(*carg);
    if ( ival < result )
      result = ival;
    }

  return ADImkR( result, status );
  }

ADIobj ADIrealMod_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIreal	value;

  value = (ADIreal) fmod( _RVAL(args[0]), _RVAL(args[1]) );

  return ADImkR( value, status );
  }

ADIobj ADIrealMult_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIreal	result = 1.0;

  _ARGLOOP_1ST_TO_NTH(carg)
    result *= _RVAL(*carg);

  return ADImkR( result, status );
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

ADIobj ADIrealNint_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIreal	value = _RVAL(args[0]);
  ADIreal	fvalue;

  fvalue = floor( value );

  if ( value > 0.0 ) {
    if ( (value-fvalue) >= 0.5 )
      fvalue += 1.0;
    }
  else {
    if ( (value-fvalue) > 0.5 )
      fvalue += 1.0;
    }

  return ADImkR( fvalue, status );
  }

ADIobj ADIrealPlus_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIreal	result = 0;

  _ARGLOOP_1ST_TO_NTH(carg)
    result += _RVAL(*carg);

  return ADImkR( result, status );
  }

ADIobj ADIrealPower_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIreal	a1 = _RVAL(args[0]);
  ADIreal	a2 = _RVAL(args[1]);

  return ADImkR( (ADIreal) pow((ADIdouble) a1,(ADIdouble) a2), status );
  }

ADIobj ADIrealPowerI_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIreal	a1 = _RVAL(args[0]);
  ADIinteger	a2 = _IVAL(args[1]);

  return ADImkR( (ADIreal) pow((ADIdouble) a1,(ADIdouble) a2), status );
  }

ADIobj ADIrealQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADImkL( _real_q(args[0]), status );
  }

ADIobj ADIrealSign_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIreal	sign = 1.0;

  if ( _RVAL(args[1]) < 0.0 )
    sign = -1.0;

  return ADImkR( (ADIreal) fabs(_RVAL(args[0]))*sign, status );
  }

ADIobj ADIrealSub_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADImkR( _RVAL(args[0]) - _RVAL(args[1]), status );
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
  return ADImkD( _oop(_DVAL(args[0])), status );}

ADIobj ADIdbleArcCos_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIdouble	arg = _DVAL(args[0]);
  ADIobj	rval = ADI__nullid;

  if ( (arg < -1.0) || (arg > 1.0) )
    ADIexecRaiseI( EXC_MathErr, SAI__OK, "Domain error in ArcCos, |arg| > 1", _CSM,
		   status, arg );
  else
    rval = ADImkD( acos(arg), status );

  return rval;
  }

ADIobj ADIdbleArcSin_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIdouble	arg = _DVAL(args[0]);
  ADIobj	rval = ADI__nullid;

  if ( (arg < -1.0) || (arg > 1.0) )
    ADIexecRaiseI( EXC_MathErr, SAI__OK, "Domain error in ArcSin, |arg| > 1", _CSM,
		   status, arg );
  else
    rval = ADImkD( asin(arg), status );

  return rval;
  }

_math_dop(ArcTan,atan)
_math_dop(Ceil,ceil)
_math_dop(Cos,cos)
_math_dop(Cosh,cosh)
_math_dop(Exp,exp)
_math_dop(Floor,floor)
_math_dop(Log,log)
_math_dop(Log10,log10)
_math_dop(Sin,sin)
_math_dop(Sinh,sinh)

ADIobj ADIdbleSqrt_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIdouble	arg = _DVAL(args[0]);
  ADIobj	rval = ADI__nullid;

  if ( arg < 0.0 )
    ADIexecRaiseI( EXC_MathErr, SAI__OK, "Square root of negative number %f", _CSM,
		   status, arg );
  else
    rval = ADImkD( sqrt(arg), status );

  return rval;
  }

_math_dop(Tan,tan)
_math_dop(Tanh,tanh)

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
  ADIdouble	value;

  value = atan2( _RVAL(args[0]), _RVAL(args[1]) );

  return ADImkD( value, status );
  }

ADIobj ADIdbleCaste_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIdouble	value;

  adic_get0d( args[0], &value, status );

  return ADImkD( value, status );
  }

ADIobj ADIdbleDiv_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIdouble	d1 = _DVAL(args[0]);
  ADIdouble	d2 = _DVAL(args[1]);
  ADIobj	rval = ADI__nullid;

  if ( d2 == 0.0 )
    ADIexecRaiseI( EXC_MathErr, ADI__INVARG, "Division by zero", _CSM,
		   status );
  else
    rval = ADImkD( d1 / d2, status );

  return rval;
  }

ADIobj ADIdbleMax_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIdouble	ival, result;

  result = _DVAL(args[0]);

  _ARGLOOP_2ND_TO_NTH(carg) {
    ival = _DVAL(*carg);
    if ( ival > result )
      result = ival;
    }

  return ADImkD( result, status );
  }

ADIobj ADIdbleMin_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIdouble	ival, result;

  result = _DVAL(args[0]);

  _ARGLOOP_2ND_TO_NTH(carg) {
    ival = _DVAL(*carg);
    if ( ival < result )
      result = ival;
    }

  return ADImkD( result, status );
  }

ADIobj ADIdbleMod_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIdouble	result;

  result = fmod( _DVAL(args[0]), _DVAL(args[1]) );

  return ADImkD( result, status );
  }

ADIobj ADIdbleMult_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIdouble	result = 1.0;

  _ARGLOOP_1ST_TO_NTH(carg)
    result *= _DVAL(*carg);

  return ADImkD( result, status );
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

ADIobj ADIdbleNint_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIdouble	value = _DVAL(args[0]);
  ADIdouble	fvalue;

  fvalue = floor( value );

  if ( value > 0.0 ) {
    if ( (value-fvalue) >= 0.5 )
      fvalue += 1.0;
    }
  else {
    if ( (value-fvalue) > 0.5 )
      fvalue += 1.0;
    }

  return ADImkD( fvalue, status );
  }

ADIobj ADIdblePlus_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIdouble	result = 0;

  _ARGLOOP_1ST_TO_NTH(carg)
    result += _DVAL(*carg);

  return ADImkD( result, status );
  }

ADIobj ADIdblePower_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIdouble	a1 = _DVAL(args[0]);
  ADIdouble	a2 = _DVAL(args[1]);

  return ADImkD( pow(a1,a2), status );
  }

ADIobj ADIdblePowerI_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIdouble	a1 = _DVAL(args[0]);
  ADIinteger	a2 = _IVAL(args[1]);

  return ADImkD( pow(a1,(ADIdouble) a2), status );
  }

ADIobj ADIdbleQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADImkL( _dble_q(args[0]), status );
  }

ADIobj ADIdbleSign_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIdouble	sign = 1.0;

  if ( _DVAL(args[1]) < 0.0 )
    sign = -1.0;

  return ADImkD( fabs(_DVAL(args[0]))*sign, status );
  }

ADIobj ADIdbleSub_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADImkD( _DVAL(args[0]) - _DVAL(args[1]), status );
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

  _ARGLOOP_1ST_TO_NTH_AND( carg, result )
    if ( ! _LVAL(*carg) )
      result = ADI__false;

  return ADImkL( result, status );
  }

ADIobj ADIlogNot_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADImkL( ! _LVAL(args[0]), status );
  }

ADIobj ADIlogOr_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	*carg;
  ADIlogical	result = ADI__false;

  _ARGLOOP_1ST_TO_NTH_AND( carg, (! result) )
    if ( _LVAL(*carg) )
      result = ADI__true;

  return ADImkL( result, status );
  }

ADIobj ADIlogQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADImkL( _logical_q(args[0]), status );
  }

ADIobj ADIlogXor_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADImkL( _LVAL(args[0]) != _LVAL(args[1]), status );
  }

/*
 *  Functions on Strings's
 */

#define _strp(_ocode,_oop) \
ADIobj ADIstr##_ocode##_i( int narg, ADIobj args[], ADIstatus status ) \
  {\
  ADIlogical	res = ADI__false;\
  ADIstring	*sdat = _str_data(args[0]);\
  if ( sdat->len > 0 ) res = _oop( *(sdat->data) );\
  return ADImkL( res ? ADI__true : ADI__false, status );}

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
  ADIlogical	res;\
  res = strx_cmp( args[0], args[1] ) _oop 0;\
  return ADImkL( res ? ADI__true : ADI__false, status );}

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

ADIobj ADIstrCaste_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	rval = ADI__nullid;

  adic_new0c( &rval, status );

  adic_putid( rval, args[0], status );

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
	_CH_MOVE( sptr, sdat->data, sdat->len );

/* Advance data pointer */
	sptr += sdat->len;
	}
      }
    }

  return rval;
  }

ADIobj ADIstrIchar_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIstring	*sdat = _str_data(args[0]);
  ADIinteger	res = 0;

  if ( sdat->len )
    res = sdat->data[0];

  return ADImkI( res, status );
  }

ADIobj ADIstrLen_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADImkI( _str_len(args[0]), status );
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
  return ADImkL( _str_q(args[0]), status );
  }

ADIobj ADIstrSimQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIlogical	res;

  res = (strx_cmpi( args[0], args[1] ) == 0);

  return ADImkL( res ? ADI__true : ADI__false, status );
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
ADIobj ADIlstAppend_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIlist	*curl;
  ADIobj	rval;
  ADIobj	*ipoint;
  int		iarg = 1;
  ADIobj	*aptr = args + 1;
  ADIlogical	more;

/* Make copy of first argument */
  rval = ADIexprOwnArg( args, status );

/* Locate address of last CDR cell in the list */
  curl = _list_data(rval);
  more = ADI__true;
  do {
    if ( _valid_q(curl->cdr) )
      curl = _list_data(curl->cdr);
    else
      more = ADI__false;
    }
  while ( more );
  ipoint = &curl->cdr;

/* Loop over remaining arguments */
  while ( iarg++ < narg ) {

/* Iinsert an owned copy of the argument into the output list */
    lstx_inscel( ADIexprOwnArg( aptr++, status ), &ipoint, status );
    }

  return rval;
  }

ADIobj ADIlstConcat_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIlist	*curl = NULL;
  ADIobj	rval = ADI__nullid;
  ADIobj	*ipoint = &rval;
  ADIobj	ilst;
  int		iarg = 0;
  ADIobj	*aptr = args;
  ADIlogical	more;

/* Loop over supplied lists */
  while ( iarg++ < narg ) {

/* Make a copy */
    ilst = ADIexprOwnArg( aptr, status );

/* Locate address of last CDR cell in the list */
    curl = _list_data(ilst);
    more = ADI__true;
    do {
      if ( _valid_q(curl->cdr) )
	curl = _list_data(curl->cdr);
      else
	more = ADI__false;
      }
    while ( more );

/* Splice into list */
    *ipoint = ilst;
    ipoint = &curl->cdr;

/* Next argument */
    aptr++;
    }

  return rval;
  }

ADIobj ADIlstHead_i( int narg, ADIobj args[], ADIstatus status )
  {
  return adix_clone( _CAR(args[0]), status );
  }

ADIobj ADIlstLen_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADImkI( lstx_len( args[0], status ), status );
  }

ADIobj ADIlstMap_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADIetnNew( adix_clone( K_List, status ),
		ADIexprMapFun( args[0], args + 1, lstx_len( args[1], status ),
			narg - 1, status ), status );
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
    lstx_inscel( oarg, &ipoint, status );
    }

  return rval;
  }

ADIobj ADIlstNth_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIinteger	n = _IVAL(args[1]);
  ADIobj	*addr = NULL;

  if ( n > 0 )
    addr = lstx_nth( args[0], n, status );

  return adix_clone( addr ? *addr : ADIcvNulCons, status );
  }

ADIobj ADIlstQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADImkL( _list_q(args[0]), status );
  }

ADIobj ADIlstReverse_i( int narg, ADIobj args[], ADIstatus status )
  {
  return lstx_revrsi( ADIexprOwnArg( args, status ), status );
  }

ADIobj ADIlstTail_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	tail = _CDR(args[0]);

  return adix_clone( _valid_q(tail) ? tail : ADIcvNulCons, status );
  }

/*
 *  Functions on Stream's
 */

ADIobj ADIstrmQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  return ADImkL( _strm_q(args[0]), status );
  }

/*
 *  Functions on Array's
 */
ADIobj ADIaryMk1_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	car,curp;
  ADIinteger	nelm = 0;
  ADIclassDef	*ot = NULL;
  ADIobj	rval = ADI__nullid;
  ADIlogical	same = ADI__true;

/* Find length of list and determine whether types of objects in list are identical */
/* If the list contains null items the output array is always generic */
  curp = args[0];
  while ( _valid_q(curp) ) {
    _GET_CARCDR(car,curp,curp);
    if ( _valid_q(car) ) {
      if ( nelm++ )
	same &= (_DTDEF(car) == ot);
      else
	ot = _DTDEF(car);
      }
    else
      same = ADI__false;
    }

/* Create the output array */
  if ( nelm ) {
    adic_new1( same ? ot->name : "*", nelm, &rval, status );
    if ( _ok(status) ) {
      ADIarray	*ary = _ary_data(_han_id(rval));
      ADIobj	idata = ary->data;
      if ( same ) {
	curp = args[0];
	while ( _valid_q(curp) ) {
	  _GET_CARCDR(car,curp,curp);
	  adix_putid( idata, NULL, 0, car, status );
	  idata = ADImemIdAddOff( idata, 1, status );
	  }
	}
      else {
	curp = args[0];
	while ( _valid_q(curp) ) {
	  _GET_CARCDR(car,curp,curp);
	  *_obj_data(idata) = adix_copy( car, status );
	  idata = ADImemIdAddOff( idata, 1, status );
	  }
	}
      }
    }

  return rval;
  }

ADIobj ADIaryQ_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIlogical	res = ADI__false;

  if ( _han_q(args[0]) )
    res = _ary_q(_han_id(args[0]));

  return ADImkL( res, status );
  }

ADIobj ADIaryRank_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIarray	*ary = _ary_data(_han_id(args[0]));

  return ADImkI( ary->ndim, status );
  }

ADIobj ADIaryRef_i( int narg, ADIobj args[], ADIstatus status )
  {
  ADIarray	*ary = _ary_data(_han_id(args[0]));
  int		inds[ADI__MXDIM];
  ADIobj	idata;
  int		idim;
  ADIobj	rval = ADI__nullid;

  if ( (narg-1) != ary->ndim )
    ADIexecRaiseI( EXC_InvalidArg, ADI__INVARG,
		   "Number of indices does not match array dimensionality",
		   _CSM, status );
  else {
    for( idim=0; idim<(narg-1) && _ok(status); idim++ ) {
      inds[idim] = _IVAL(args[idim+1]);
      if ( (inds[idim] < 1) || (inds[idim]>ary->dims[idim]) )
	ADIexecRaiseI( EXC_InvalidArg, ADI__INVARG,
	"Array bound violation in dimension %d, index value %d vs. dimension %d",
	_CSM, status, idim+1, inds[idim], ary->dims[idim] );
      }

    if ( _ok(status) ) {
      idata = ADImemIdAddOff( ary->data,
			    ADIaryOffset( ary->ndim, ary->dims, inds ), status );

      if ( _obj_q(idata) )
	rval = adix_clone( *_obj_data(idata), status );
      else
	rval = adix_copy( idata, status );
      }
    }

  return rval;
  }

void ADIfuncInit( ADIstatus status )
  {
  DEFINE_FUNC_TABLE(sys_funcs)
    FUNC_TENTRY( "GetEnv(_CHAR)",			ADIsysGetEnv_i,	FA_L ),
    FUNC_TENTRY( "GetPid()",				ADIsysGetPid_i,	0 ),
  END_FUNC_TABLE;

  DEFINE_FUNC_TABLE(gen_funcs)
    FUNC_TENTRY( "AtomQ(_)",				ADIgenAtomQ_i,	FA_L ),
    FUNC_TENTRY( "Break()",				ADIgenBreak_i,	0 ),
    FUNC_TENTRY( "DefProc(_,_)",			ADIgenDefProc_i,FA_A ),
    FUNC_TENTRY( "DefRep(_CHAR)",			ADIgenDefRep_i, FA_A ),
    FUNC_TENTRY( "DoWhile(__)",				ADIgenDoWhile_i,FA_A ),
    FUNC_TENTRY( "Global(__Symbol)",			ADIgenGlobal_i,	FA_L1 ),
    FUNC_TENTRY( "Local(__Symbol)",			ADIgenLocal_i,	FA_L1 ),
    FUNC_TENTRY( "LocalTime()",				ADIgenLtime_i,	0 ),
    FUNC_TENTRY( "LocalTime(_INTEGER)",			ADIgenLtime_i,	0 ),
    FUNC_TENTRY( "Get(_CHAR)",				ADIgenGet_i,	FA_L ),
    FUNC_TENTRY( "HashStats()",				ADIgenHst_i,	0 ),
    FUNC_TENTRY( "If(__)",				ADIgenIf_i,	FA_A ),
    FUNC_TENTRY( "Name(_)",				ADIgenName_i,	FA_L ),
    FUNC_TENTRY( "NullQ(_)",				ADIgenNullQ_i,	FA_L ),
    FUNC_TENTRY( "Print(__)",				ADIgenPrint_i,	0 ),
    FUNC_TENTRY( "Probe()",				ADIgenProbe_i,	FA_L ),
    FUNC_TENTRY( "Raise(__)",				ADIgenRaise_i,	0 ),
    FUNC_TENTRY( "Require(__CHAR)",			ADIgenRequire_i,FA_L ),
    FUNC_TENTRY( "Return(_)",				ADIgenReturn_i,	0 ),
    FUNC_TENTRY( "Set(_Symbol,_)",			ADIgenSet_i,	FA_L1 ),
    FUNC_TENTRY( "Time()",				ADIgenTime_i,	0 ),
    FUNC_TENTRY( "Time(_INTEGER)",			ADIgenTime_i,	0 ),
    FUNC_TENTRY( "TimeIt(_,_INTEGER)",			ADIgenTimeIt_i,	FA_1 ),
    FUNC_TENTRY( "TimeZone()",				ADIgenTzone_i,	0 ),
    FUNC_TENTRY( "Type(_)",				ADIgenType_i,	FA_L ),
    FUNC_TENTRY( "While(__)",				ADIgenWhile_i,	FA_A ),
  END_FUNC_TABLE;

  DEFINE_FUNC_TABLE(int_funcs)
    FUNC_TENTRY( "Abs(_INTEGER)",			ADIintAbs_i,	FA_L ),
    FUNC_TENTRY( "And(__INTEGER)",			ADIintAnd_i,	FA_L ),
    FUNC_TENTRY( "Divide(_INTEGER,_INTEGER)",		ADIintDiv_i,	FA_L ),
    FUNC_TENTRY( "Equal(_INTEGER,_INTEGER)", 		ADIintEQ_i,	FA_L ),
    FUNC_TENTRY( "EvenQ(_INTEGER)",			ADIintEvenQ_i,	FA_L ),
    FUNC_TENTRY( "Factorial(_INTEGER)",			ADIintFact_i,	FA_L ),
    FUNC_TENTRY( "GCD(_INTEGER,_INTEGER)", 		ADIintGCD_i,	FA_L ),
    FUNC_TENTRY( "GreaterThan(_INTEGER,_INTEGER)", 	ADIintGT_i,	FA_L ),
    FUNC_TENTRY( "GreaterThanOrEqual(_INTEGER,_INTEGER)", ADIintGE_i,	FA_L ),
    FUNC_TENTRY( "Integer(_REAL)",			ADIintCaste_i,	FA_L ),
    FUNC_TENTRY( "Integer(_DOUBLE)",			ADIintCaste_i,	FA_L ),
    FUNC_TENTRY( "IntegerQ(_)",				ADIintQ_i,	FA_L ),
    FUNC_TENTRY( "LessThan(_INTEGER,_INTEGER)", 	ADIintLT_i,	FA_L ),
    FUNC_TENTRY( "LessThanOrEqual(_INTEGER,_INTEGER)", 	ADIintLE_i,	FA_L ),
    FUNC_TENTRY( "Max(__INTEGER)",			ADIintMax_i,	0 ),
    FUNC_TENTRY( "Min(__INTEGER)",			ADIintMin_i,	0 ),
    FUNC_TENTRY( "Mod(_INTEGER,_INTEGER)", 		ADIintMod_i,	FA_L ),
    FUNC_TENTRY( "Multiply(__INTEGER)",			ADIintMult_i,	FA_L ),
    FUNC_TENTRY( "Negate(_INTEGER)",			ADIintNeg_i,	FA_L ),
    FUNC_TENTRY( "Not(_INTEGER)",			ADIintNot_i,	FA_L ),
    FUNC_TENTRY( "NotEqual(_INTEGER,_INTEGER)", 	ADIintNE_i,	FA_L ),
    FUNC_TENTRY( "OddQ(_INTEGER)",			ADIintOddQ_i,	FA_L ),
    FUNC_TENTRY( "Or(__INTEGER)",			ADIintOr_i,	FA_L ),
    FUNC_TENTRY( "Plus(__INTEGER)",			ADIintPlus_i,	FA_L ),
    FUNC_TENTRY( "Power(_INTEGER,_INTEGER)", 		ADIintPower_i,	FA_L ),
    FUNC_TENTRY( "Power(_INTEGER,2)", 			ADIintPow2_i,	FA_L ),
    FUNC_TENTRY( "Shift(_INTEGER,_INTEGER)",		ADIintShift_i,	FA_L ),
    FUNC_TENTRY( "Sign(_INTEGER,_INTEGER)",		ADIintSign_i,	FA_L ),
    FUNC_TENTRY( "Subtract(_INTEGER,_INTEGER)", 	ADIintSub_i,	FA_L ),
    FUNC_TENTRY( "Sum(_INTEGER[])",			ADIintSum_i,	FA_L ),
    FUNC_TENTRY( "Xor(_INTEGER,_INTEGER)",		ADIintXor_i,	FA_L ),
  END_FUNC_TABLE;

  DEFINE_FUNC_TABLE(list_funcs)
    FUNC_TENTRY( "Lappend(_List,__)",			ADIlstAppend_i,	0 ),
    FUNC_TENTRY( "Lconcat(__List)",			ADIlstConcat_i,	0 ),
    FUNC_TENTRY( "Head(_List)",				ADIlstHead_i,	0 ),
    FUNC_TENTRY( "Llength(_List)",			ADIlstLen_i,	0 ),
    FUNC_TENTRY( "List(__)",				ADIlstMk_i,	0 ),
    FUNC_TENTRY( "ListQ(_)",				ADIlstQ_i,	FA_L ),
    FUNC_TENTRY( "Map(_Symbol,__List)",			ADIlstMap_i,	FA_1 ),
    FUNC_TENTRY( "Nth(_List,_INTEGER)",			ADIlstNth_i,	0 ),
    FUNC_TENTRY( "Lreverse(_List)",			ADIlstReverse_i,0 ),
    FUNC_TENTRY( "Tail(_List)",				ADIlstTail_i,	0 ),
  END_FUNC_TABLE;

  DEFINE_FUNC_TABLE(array_funcs)
    FUNC_TENTRY( "Array(_List)",			ADIaryMk1_i,	0 ),
    FUNC_TENTRY( "ArrayQ(_)",				ADIaryQ_i,	FA_L ),
    FUNC_TENTRY( "ArrayRef(_Array,__INTEGER)",		ADIaryRef_i,	0 ),
    FUNC_TENTRY( "Rank(_Array)",			ADIaryRank_i,	0 ),
  END_FUNC_TABLE;

  DEFINE_FUNC_TABLE(real_funcs)
    FUNC_TENTRY( "Abs(_REAL)",				ADIrealAbs_i,	FA_L ),
    FUNC_TENTRY( "ArcCos(_REAL)",			ADIrealArcCos_i,FA_L ),
    FUNC_TENTRY( "ArcSin(_REAL)",			ADIrealArcSin_i,FA_L ),
    FUNC_TENTRY( "ArcTan(_REAL)",			ADIrealArcTan_i,FA_L ),
    FUNC_TENTRY( "ArcTan(_REAL,_REAL)",			ADIrealArcTan2_i,FA_L ),
    FUNC_TENTRY( "Ceil(_REAL)",				ADIrealCeil_i,	FA_L ),
    FUNC_TENTRY( "Cos(_REAL)",				ADIrealCos_i,	FA_L ),
    FUNC_TENTRY( "Cosh(_REAL)",				ADIrealCosh_i,	FA_L ),
    FUNC_TENTRY( "Divide(_REAL,_REAL)",  		ADIrealDiv_i,   FA_L ),
    FUNC_TENTRY( "Equal(_REAL,_REAL)", 			ADIrealEQ_i,	FA_L ),
    FUNC_TENTRY( "Exp(_REAL)",				ADIrealExp_i,	FA_L ),
    FUNC_TENTRY( "Floor(_REAL)",			ADIrealFloor_i,	FA_L ),
    FUNC_TENTRY( "GreaterThan(_REAL,_REAL)", 		ADIrealGT_i,	FA_L ),
    FUNC_TENTRY( "GreaterThanOrEqual(_REAL,_REAL)", 	ADIrealGE_i,	FA_L ),
    FUNC_TENTRY( "LessThan(_REAL,_REAL)", 		ADIrealLT_i,	FA_L ),
    FUNC_TENTRY( "LessThanOrEqual(_REAL,_REAL)", 	ADIrealLE_i,	FA_L ),
    FUNC_TENTRY( "Log(_REAL)",				ADIrealLog_i,	FA_L ),
    FUNC_TENTRY( "Log10(_REAL)",			ADIrealLog10_i,	FA_L ),
    FUNC_TENTRY( "Max(__REAL)",				ADIrealMax_i,	0 ),
    FUNC_TENTRY( "Min(__REAL)",				ADIrealMin_i,	0 ),
    FUNC_TENTRY( "Mod(_REAL,_REAL)",  			ADIrealMod_i,	FA_L ),
    FUNC_TENTRY( "Multiply(__REAL)",			ADIrealMult_i,	FA_L ),
    FUNC_TENTRY( "Negate(_REAL)",			ADIrealNeg_i,	FA_L ),
    FUNC_TENTRY( "Nint(_REAL)",				ADIrealNint_i,	FA_L ),
    FUNC_TENTRY( "NotEqual(_REAL,_REAL)",  		ADIrealNE_i,	FA_L ),
    FUNC_TENTRY( "Plus(__REAL)",			ADIrealPlus_i,	FA_L ),
    FUNC_TENTRY( "Power(_REAL,_REAL)",  		ADIrealPower_i,	FA_L ),
    FUNC_TENTRY( "Power(_REAL,_INTEGER)",  		ADIrealPowerI_i,FA_L ),
    FUNC_TENTRY( "Real(_DOUBLE)",			ADIrealCaste_i,	FA_L ),
    FUNC_TENTRY( "Real(_INTEGER)",			ADIrealCaste_i,	FA_L ),
    FUNC_TENTRY( "RealQ(_)",				ADIrealQ_i,	FA_L ),
    FUNC_TENTRY( "Sign(_REAL,_REAL)",  			ADIrealSign_i,	FA_L ),
    FUNC_TENTRY( "Sin(_REAL)",				ADIrealSin_i,	FA_L ),
    FUNC_TENTRY( "Sinh(_REAL)",				ADIrealSinh_i,	FA_L ),
    FUNC_TENTRY( "Sqrt(_REAL)",				ADIrealSqrt_i,	FA_L ),
    FUNC_TENTRY( "Subtract(_REAL,_REAL)",  		ADIrealSub_i,	FA_L ),
    FUNC_TENTRY( "Tan(_REAL)",				ADIrealTan_i,	FA_L ),
    FUNC_TENTRY( "Tanh(_REAL)",				ADIrealTanh_i,	FA_L ),
  END_FUNC_TABLE;

  DEFINE_FUNC_TABLE(log_funcs)
    FUNC_TENTRY( "And(__LOGICAL)",			ADIlogAnd_i,	FA_L ),
    FUNC_TENTRY( "LogicalQ(_)",				ADIlogQ_i,	FA_L ),
    FUNC_TENTRY( "Not(_LOGICAL)",			ADIlogNot_i,	FA_L ),
    FUNC_TENTRY( "Or(__LOGICAL)",			ADIlogOr_i,	FA_L ),
    FUNC_TENTRY( "Xor(_LOGICAL,_LOGICAL)",		ADIlogXor_i,	FA_L ),
  END_FUNC_TABLE;

  DEFINE_FUNC_TABLE(string_funcs)
    FUNC_TENTRY( "AlnumQ(_)",				ADIstrAlnumQ_i, FA_L ),
    FUNC_TENTRY( "AlphaQ(_)",				ADIstrAlphaQ_i, FA_L ),
    FUNC_TENTRY( "AsciiQ(_)",				ADIstrAsciiQ_i, FA_L ),
    FUNC_TENTRY( "Capitalise(_CHAR)",			ADIstrCapit_i,  FA_L ),
    FUNC_TENTRY( "Char(__INTEGER)",			ADIstrChar_i,	FA_L ),
    FUNC_TENTRY( "Characters(_CHAR)",			ADIstrChars_i,	FA_L ),
    FUNC_TENTRY( "CntrlQ(_)",				ADIstrCntrlQ_i, FA_L ),
    FUNC_TENTRY( "Concat(__CHAR)",			ADIstrConcat_i, FA_L ),
    FUNC_TENTRY( "DigitQ(_)",				ADIstrDigitQ_i, FA_L ),
    FUNC_TENTRY( "Equal(_CHAR,_CHAR)",			ADIstrEQ_i,	FA_L ),
    FUNC_TENTRY( "GreaterThan(_CHAR,_CHAR)", 		ADIstrGT_i,	FA_L ),
    FUNC_TENTRY( "GreaterThanOrEqual(_CHAR,_CHAR)", 	ADIstrGE_i,	FA_L ),
    FUNC_TENTRY( "Ichar(_CHAR)",			ADIstrIchar_i,	FA_L ),
    FUNC_TENTRY( "Length(_CHAR)",			ADIstrLen_i,	FA_L ),
    FUNC_TENTRY( "LessThan(_CHAR,_CHAR)", 		ADIstrLT_i,	FA_L ),
    FUNC_TENTRY( "LessThanOrEqual(_CHAR,_CHAR)", 	ADIstrLE_i,	FA_L ),
    FUNC_TENTRY( "LowerCase(_CHAR)",			ADIstrLower_i,  FA_L ),
    FUNC_TENTRY( "LowerQ(_)",				ADIstrLowerQ_i, FA_L ),
    FUNC_TENTRY( "NotEqual(_CHAR,_CHAR)",		ADIstrNE_i,	FA_L ),
    FUNC_TENTRY( "PrintQ(_)",				ADIstrPrintQ_i, FA_L ),
    FUNC_TENTRY( "PunctQ(_)",				ADIstrPunctQ_i, FA_L ),
    FUNC_TENTRY( "Reverse(_CHAR)",			ADIstrRvrse_i,  FA_L ),
    FUNC_TENTRY( "SimilarQ(_CHAR,_CHAR)",		ADIstrSimQ_i,	FA_L ),
    FUNC_TENTRY( "SpaceQ(_)",				ADIstrSpaceQ_i, FA_L ),
    FUNC_TENTRY( "String(_LOGICAL)",			ADIstrCaste_i,	FA_L ),
    FUNC_TENTRY( "String(_INTEGER)",			ADIstrCaste_i,	FA_L ),
    FUNC_TENTRY( "String(_REAL)",			ADIstrCaste_i,	FA_L ),
    FUNC_TENTRY( "String(_DOUBLE)",			ADIstrCaste_i,	FA_L ),
    FUNC_TENTRY( "StringQ(_)",				ADIstrQ_i,	FA_L ),
    FUNC_TENTRY( "Take(_CHAR,_INTEGER)",		ADIstrTake_i,	FA_L ),
    FUNC_TENTRY( "UpperCase(_CHAR)",			ADIstrUpper_i,  FA_L ),
    FUNC_TENTRY( "UpperQ(_)",				ADIstrUpperQ_i, FA_L ),
    FUNC_TENTRY( "XdigitQ(_)",				ADIstrXdigitQ_i,FA_L ),
  END_FUNC_TABLE;

  DEFINE_FUNC_TABLE(dble_funcs)
    FUNC_TENTRY( "Abs(_DOUBLE)",			ADIdbleAbs_i,	FA_L ),
    FUNC_TENTRY( "ArcCos(_DOUBLE)",			ADIdbleArcCos_i,FA_L ),
    FUNC_TENTRY( "ArcSin(_DOUBLE)",			ADIdbleArcSin_i,FA_L ),
    FUNC_TENTRY( "ArcTan(_DOUBLE)",			ADIdbleArcTan_i,FA_L ),
    FUNC_TENTRY( "ArcTan(_DOUBLE,_DOUBLE)",		ADIdbleArcTan2_i,FA_L ),
    FUNC_TENTRY( "Ceil(_DOUBLE)",			ADIdbleCeil_i,	FA_L ),
    FUNC_TENTRY( "Cos(_DOUBLE)",			ADIdbleCos_i,	FA_L ),
    FUNC_TENTRY( "Cosh(_DOUBLE)",			ADIdbleCosh_i,	FA_L ),
    FUNC_TENTRY( "Divide(_DOUBLE,_DOUBLE)",      	ADIdbleDiv_i,	FA_L ),
    FUNC_TENTRY( "Double(_INTEGER)",			ADIdbleCaste_i,	FA_L ),
    FUNC_TENTRY( "Double(_REAL)",			ADIdbleCaste_i,	FA_L ),
    FUNC_TENTRY( "DoubleQ(_)",				ADIdbleQ_i,	FA_L ),
    FUNC_TENTRY( "Equal(_DOUBLE,_DOUBLE)", 		ADIdbleEQ_i,	FA_L ),
    FUNC_TENTRY( "Exp(_DOUBLE)",			ADIdbleExp_i,	FA_L ),
    FUNC_TENTRY( "Floor(_DOUBLE)",			ADIdbleFloor_i,	FA_L ),
    FUNC_TENTRY( "GreaterThan(_DOUBLE,_DOUBLE)", 	ADIdbleGT_i,	FA_L ),
    FUNC_TENTRY( "GreaterThanOrEqual(_DOUBLE,_DOUBLE)", ADIdbleGE_i,	FA_L ),
    FUNC_TENTRY( "LessThan(_DOUBLE,_DOUBLE)", 		ADIdbleLT_i,	FA_L ),
    FUNC_TENTRY( "LessThanOrEqual(_DOUBLE,_DOUBLE)", 	ADIdbleLE_i,	FA_L ),
    FUNC_TENTRY( "Log(_DOUBLE)",			ADIdbleLog_i,	FA_L ),
    FUNC_TENTRY( "Log10(_DOUBLE)",			ADIdbleLog10_i,	FA_L ),
    FUNC_TENTRY( "Max(__DOUBLE)",			ADIdbleMax_i,	0 ),
    FUNC_TENTRY( "Min(__DOUBLE)",			ADIdbleMin_i,	0 ),
    FUNC_TENTRY( "Mod(_DOUBLE,_DOUBLE)",  		ADIdbleMod_i,	FA_L ),
    FUNC_TENTRY( "Multiply(__DOUBLE)",			ADIdbleMult_i,	FA_L ),
    FUNC_TENTRY( "Negate(_DOUBLE)",			ADIdbleNeg_i,	FA_L ),
    FUNC_TENTRY( "Nint(_DOUBLE)",			ADIdbleNint_i,	FA_L ),
    FUNC_TENTRY( "NotEqual(_DOUBLE,_DOUBLE)",  		ADIdbleNE_i,	FA_L ),
    FUNC_TENTRY( "Plus(__DOUBLE)",		    	ADIdblePlus_i,	FA_L ),
    FUNC_TENTRY( "Power(_DOUBLE,_DOUBLE)",  		ADIdblePower_i,	FA_L ),
    FUNC_TENTRY( "Power(_DOUBLE,_INTEGER)",  		ADIdblePowerI_i,FA_L ),
    FUNC_TENTRY( "Sign(_DOUBLE,_DOUBLE)",  		ADIdbleSign_i,	FA_L ),
    FUNC_TENTRY( "Sin(_DOUBLE)",			ADIdbleSin_i,	FA_L ),
    FUNC_TENTRY( "Sinh(_DOUBLE)",			ADIdbleSinh_i,	FA_L ),
    FUNC_TENTRY( "Sqrt(_DOUBLE)",			ADIdbleSqrt_i,	FA_L ),
    FUNC_TENTRY( "Subtract(_DOUBLE,_DOUBLE)",  		ADIdbleSub_i,	FA_L ),
    FUNC_TENTRY( "Tan(_DOUBLE)",			ADIdbleTan_i,	FA_L ),
    FUNC_TENTRY( "Tanh(_DOUBLE)",			ADIdbleTanh_i,	FA_L ),
  END_FUNC_TABLE;

  DEFINE_FUNC_TABLE(strm_funcs)
    FUNC_TENTRY( "StreamQ(_)",				ADIstrmQ_i,	FA_L ),
  END_FUNC_TABLE;

  _chk_stat;

/* Install functions */
  ADIkrnlAddFuncs( sys_funcs, status );
  ADIkrnlAddFuncs( gen_funcs, status );
  ADIkrnlAddFuncs( int_funcs, status );
  ADIkrnlAddFuncs( list_funcs, status );
  ADIkrnlAddFuncs( array_funcs, status );
  ADIkrnlAddFuncs( real_funcs, status );
  ADIkrnlAddFuncs( dble_funcs, status );
  ADIkrnlAddFuncs( log_funcs, status );
  ADIkrnlAddFuncs( string_funcs, status );
  ADIkrnlAddFuncs( strm_funcs, status );

  cdef_B = _cdef_data(UT_cid_b);
  cdef_UB = _cdef_data(UT_cid_ub);
  cdef_W = _cdef_data(UT_cid_w);
  cdef_UW = _cdef_data(UT_cid_uw);
  cdef_I = _cdef_data(UT_cid_i);
  cdef_R = _cdef_data(UT_cid_r);
  cdef_D = _cdef_data(UT_cid_d);
  cdef_L = _cdef_data(UT_cid_l);
  }

