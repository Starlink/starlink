/*
 *
 */
#include <string.h>                     /* String stuff from RTL */
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "asterix.h"                    /* Asterix definitions */
#include "ems.h"

#include "aditypes.h"
#include "adimem.h"
#include "adikrnl.h"                    /* Internal ADI kernel */
#include "adimem.h"                     /* Allocation routines */
#include "adistrng.h"
#include "adicface.h"
#include "adiexpr.h"
#include "adisyms.h"
#include "adilist.h"
#include "adiparse.h"
#include "adipkg.h"                   	/* Prototypes for this sub-package */

#include "adi_err.h"                    /* ADI error codes */

#ifdef __MSDOS__
#define FILE_DELIMITER '\\'
#define PATH_SEPARATOR ';'
#else
#define FILE_DELIMITER '/'
#define PATH_SEPARATOR ':'
#endif

ADIobj		ADI_G_pkglist = ADI__nullid;
char		*ADI_G_ldpath = NULL;
size_t          ADI_G_ldpath_len = 0;
ADIlogical	ADI_G_getenv = ADI__false;



ADIobj adix_prs_cmd( ADIobj pstream, ADIstatus status );


ADIobj adix_prs_cmdlist( ADIobj pstream, char *termlist,
			      int *choice, ADIstatus status )
  {
  int           len;
  ADIlogical    more = ADI__true;            /* More statements in list? */
  ADIobj        robj = ADI__nullid;            /* Returned object */
  ADIobj  	state;			/* Parsed statement */
  char          *tcur;                  /* Cursor over terminator list */
  ADIobj	*ipoint = &robj;

/* Check inherited status on entry */
  _chk_stat_ret(ADI__nullid);

/* While more statements */
  while ( _ok(status) && more ) {

/* If statement starts with a symbol, test against terminal list */
    if ( ADIcurrentToken(pstream,status) == TOK__SYM ) {

      tcur = termlist;
      *choice = 1;
      while ( more && _ok(status) && (*tcur=='|') ) {
	len = 0; tcur++;
	while ( (tcur[len] != '|') && tcur[len] )
	  len++;

	if ( !strncmp(_strm_data(pstream)->ctok.dat,tcur,len) )
	  more = ADI__false;
	else
	  (*choice)++;

	tcur += len;
	}
      }

    if ( more ) {
      state = adix_prs_cmd( pstream, status );

      ADInextToken( pstream, status );

      if ( _valid_q(state) )
	lstx_inscel( state, &ipoint, status );
      }
    }

/* Use the null list to represent the null statement list */
  if ( _null_q(robj) )
    robj = adix_clone( ADIcvNulCons, status );

  return robj;
  }

ADIobj adix_prs_break( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	pstream = args[0];

/* Skip the BREAK keyword */
  ADInextToken( pstream, status );

/* Create the expression node */
  return ADIetnNew( adix_clone( K_Break, status ),
		    adix_clone( ADIcvNulCons, status ),
		    status );
  }


ADIobj adix_prs_defclass( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	pstream = args[0];
  ADIlogical	matchend = ADI__false;
  int		oflags;			/* Old stream flags */

  ADIobj	cargs[3] = {ADI__nullid, ADI__nullid, ADI__nullid};

/* Tell parser that end-of-lines can be ignored */
  oflags = ADIsetStreamAttr( pstream, ADI_STREAM__EOLISP, status );

  ADInextToken( pstream, status );

/* Get new class name from stream */
  cargs[0] = prsx_symname( pstream, status );
  ADInextToken( pstream, status );

/* Parse superclass list. This updates both the superclass list and the */
/* members list (due to inherited members) */
  if ( ADIcurrentToken(pstream,status) == TOK__SYM )
    ADIparseClassSupers( pstream, cargs+1, cargs+2, status );

/* Parse the class member list */
  if ( ADIifMatchToken( pstream, TOK__LBRACE, status ) ) {
    ADIparseClassMembers( pstream, cargs+2, status );

    if ( ADIcurrentToken(pstream,status) == TOK__RBRACE )
      matchend = ADI__true;
    else {
      adic_setecs( ADI__INVARG, "Closing brace expected", status );
      }
    }

/* Restore stream flags */
  ADIputStreamAttrs( pstream, oflags, status );

/* Match end token */
  if ( matchend )
    ADInextToken( pstream, status );

/* Define class, ignoring returned identifier */
  ADIdefClass_i( 3, cargs, status );

  return ADI__nullid;
  }


ADIobj adix_prs_defproc( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	pstream = args[0];
  ADIobj	actions;
  int		choice;
  ADIobj	defn;
  ADIobj	robj = ADI__nullid;

/* Skip the DEFPROC keyword */
  ADInextToken( pstream, status );

/* Get the definition expression */
  defn = ADIparseExpInt( pstream, 1, status );

/* Check it */
  if ( _ok(status) ) {
    if ( ! _etn_q(defn) ) {
      adic_setecs( ADI__SYNTAX, "Invalid procedure definition", status );
      }
    else {
      actions = adix_prs_cmdlist( pstream, "|endproc", &choice, status );

      ADInextToken( pstream, status );

      robj = ADIetnNew( adix_clone( K_DefProc, status ),
		     lstx_new2( defn, actions, status ),
		     status );
      }
    }

/* Create the expression node */
  return robj;
  }


ADIobj adix_prs_dowhile( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	pstream = args[0];
  int           choice;
  ADIobj        robj;                   /* Returned object */
  ADIobj        action;                 /* Action procedure */
  ADIobj        test;                   /* The WHILE test expression */

/* Skip the DO keyword */
  ADInextToken( pstream, status );

/* End of line with "do" on it */
  ADImatchToken( pstream, TOK__END, status );

/* Action list */
  action = adix_prs_cmdlist( pstream, "|while", &choice, status );

  ADInextToken( pstream, status );

/* Get the conditional expression */
  ADImatchToken( pstream, TOK__LPAREN, status );
  test = ADIparseExpInt( pstream, 1, status );
  ADImatchToken( pstream, TOK__RPAREN, status );

/* Construct argument list */
  robj = lstx_new2( action, test, status );

  return ADIetnNew( adix_clone( K_DoWhile, status ), robj, status );
  }


ADIobj adix_prs_global( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	pstream = args[0];
  ADIobj	cargs[3] = {ADI__nullid, ADI__nullid, ADI__nullid};
  ADIobj	lcell;
  ADIobj	rlist = ADI__nullid;
  ADIobj	*ipoint = &rlist;
  ADIlogical	more = ADI__true;

  ADInextToken( pstream, status );

/* Get new class name from stream */
  cargs[0] = prsx_symname( pstream, status );
  ADInextToken( pstream, status );

/* Parse the list of symbols and construct a list of expression nodes whose */
/* heads are the names of the symbols */
  while ( ADIcurrentToken(pstream,status) == TOK__SYM && more ) {

/* Get symbol name */
    lcell = lstx_cell(
		ADIetnNew(
		   prsx_symname( pstream, status ),
		   ADI__nullid,
		   status ),
		ADI__nullid,
		status );

    ADInextToken( pstream, status );

/* Add to list of symbols */
    *ipoint = lcell;
    ipoint = &_CDR(lcell);

/* End of list if not a comma */
    more = ADIifMatchToken( pstream, TOK__COMMA, status );
    }

  if ( ! _valid_q(rlist) )
    adic_setecs( ADI__SYNTAX, "Symbol name expected", status );

/* Define class, ignoring returned identifier */
/*  return ADIdefGlobal_i( 1, &rlist, status ); */
  return ADI__nullid;
  }


ADIobj adix_prs_if( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	pstream = args[0];
  int           choice;                 /* Keyword choice */
  ADIlogical	first = ADI__true;	/* First time through complex loop? */
  ADIlogical    more = ADI__true;       /* More ELSE clauses */
  ADIobj        robj = ADI__nullid;     /* Returned object */
  ADIobj        action;                 /* Action procedure */
  ADIobj	*ipoint = &robj;	/* List insertion point */

  ADInextToken( pstream, status );

/* Get the conditional expression */
  ADImatchToken( pstream, TOK__LPAREN, status );
  lstx_inscel( ADIparseExpInt( pstream, 1, status ), &ipoint, status );
  ADImatchToken( pstream, TOK__RPAREN, status );

/* There are 2 forms of 'if' statement. The simple form is simply
 *
 *	if ( expr ) statement
 *
 * which is trapped here on the presence of the 'then' keyword.
 */
  if ( ADIisTokenCstring( pstream, "then", status ) ) {

/* While more if..else if..endif clauses */
    while ( _ok(status) && more ) {

/* Get the conditional expression unless the first time through */
      if ( first )
	first = ADI__false;
      else {
	ADImatchToken( pstream, TOK__LPAREN, status );
	lstx_inscel( ADIparseExpInt( pstream, 1, status ), &ipoint, status );
	ADImatchToken( pstream, TOK__RPAREN, status );
	}

/* Skip the 'then' token if present */
      if ( ADIisTokenCstring( pstream, "then", status ) ) {
	ADInextToken( pstream, status );
	ADImatchToken( pstream, TOK__END, status );
	}
      else
	adic_setecs( ADI__SYNTAX, "THEN keyword expected", status );

/* Append truth action list */
      lstx_inscel( adix_prs_cmdlist( pstream, "|else|endif", &choice, status ),
		   &ipoint, status );

      if ( _ok(status) ) {

/* Match the ELSE or ENDIF */
	ADInextToken( pstream, status );

/* The keyword was ELSE */
	if ( choice == 1 ) {
	  if ( ADIcurrentToken(pstream,status) == TOK__SYM ) {

	    if ( ADIisTokenCstring( pstream, "if", status ) )
	      ADInextToken( pstream, status );
	    else
	      adic_setecs( ADI__SYNTAX, "Illegal token - can only be IF () THEN or end of line at this point", status );
	    }
	  else {

/* Terminal ELSE clause */
	    ADImatchToken( pstream, TOK__END, status );

	    lstx_inscel( adix_prs_cmdlist( pstream, "|endif", &choice, status ),
			 &ipoint, status );

/* Match the "endif" */
	    ADInextToken( pstream, status );

	    more = ADI__false;
	    }
	  }

/* The keyword was ENDIF. Flag end of loop */
	else
	  more = ADI__false;
	}
      }
    }
  else {

/* Parse a single statement, and put into the action list */
    action = lstx_cell( adix_prs_cmd( pstream, status ), ADI__nullid, status );

/* Add action list to output args */
    lstx_inscel( action, &ipoint, status );
    }

/* Return the expression tree */
  return ADIetnNew( adix_clone( K_If, status ), robj, status );
  }


ADIobj adix_prs_print( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	pstream = args[0];
  ADIobj	pargs = ADI__nullid;

/* Skip the command name */
  ADInextToken( pstream, status );

/* Gather arguments - separated by commas */
  pargs = ADIparseComDelList( pstream, TOK__END, ADI__false, status );

/* Return expression */
  return ADIetnNew( adix_clone( K_Print, status ), pargs, status );
  }


ADIobj adix_prs_raise( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	exname, exdata = ADI__nullid;
  ADIobj	pstream = args[0];

/* Skip the RAISE keyword */
  ADInextToken( pstream, status );

/* Get exception name */
  if ( ADIcurrentToken(pstream,status) == TOK__SYM ) {
    exname = prsx_symname( pstream, status );

    ADInextToken( pstream, status );

/* Optional data */
    if ( ADIcurrentToken(pstream,status) == TOK__CONST )
      exdata = prsx_cvalue( pstream, status );
    }
  else
    adic_setecs( ADI__SYNTAX, "Exception name expected", status );

/* Create the expression node */
  return ADIetnNew( adix_clone( K_Raise, status ),
		    _null_q(exdata) ?
		       lstx_cell( exname, ADI__nullid, status ) :
		       lstx_new2( exname, exdata, status ),
		    status );
  }


ADIobj adix_prs_return( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	pstream = args[0];
  ADIobj	defn;

/* Skip the RETURN keyword */
  ADInextToken( pstream, status );

/* Get the return expression */
  defn = ADIparseExpInt( pstream, 1, status );

  return ADIetnNew( adix_clone( K_Return, status ),
		    lstx_cell( defn, ADI__nullid, status ),
		    status );
  }


ADIobj adix_prs_try( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	pstream = args[0];
  int           choice;                 /* Keyword choice */
  ADIlogical	first = ADI__true;	/* First time through complex loop? */
  ADIlogical    more = ADI__true;       /* More ELSE clauses */
  ADIobj        robj = ADI__nullid;     /* Returned object */
  ADIobj        action;                 /* Action procedure */
  ADIobj	*ipoint = &robj;	/* List insertion point */

/* Skip the TRY keyword */
  ADInextToken( pstream, status );

/* End of line with TRY on it */
  ADImatchToken( pstream, TOK__END, status );

/* Get actions which might trigger exceptions */
  action = adix_prs_cmdlist( pstream, "|catch|finally|endtry", &choice, status );
  lstx_inscel( action, &ipoint, status );

  /* While more clauses*/
  do {

/* Command list */
    action = adix_prs_cmdlist( pstream, "|catch|finally|endtry", &choice, status );

    }
  while ( more && _ok(status) );

/* There are 2 forms of 'if' statement. The simple form is simply
 *
 *	if ( expr ) statement
 *
 * which is trapped here on the presence of the 'then' keyword.
 */
  if ( ADIisTokenCstring( pstream, "then", status ) ) {

/* While more if..else if..endif clauses */
    while ( _ok(status) && more ) {

/* Get the conditional expression unless the first time through */
      if ( first )
	first = ADI__false;
      else {
	ADImatchToken( pstream, TOK__LPAREN, status );
	lstx_inscel( ADIparseExpInt( pstream, 1, status ), &ipoint, status );
	ADImatchToken( pstream, TOK__RPAREN, status );
	}

/* Skip the 'then' token if present */
      if ( ADIisTokenCstring( pstream, "then", status ) ) {
	ADInextToken( pstream, status );
	ADImatchToken( pstream, TOK__END, status );
	}
      else
	adic_setecs( ADI__SYNTAX, "THEN keyword expected", status );

/* Append truth action list */
      lstx_inscel( adix_prs_cmdlist( pstream, "|else|endif", &choice, status ),
		   &ipoint, status );

      if ( _ok(status) ) {

/* Match the ELSE or ENDIF */
	ADInextToken( pstream, status );

/* The keyword was ELSE */
	if ( choice == 1 ) {
	  if ( ADIcurrentToken(pstream,status) == TOK__SYM ) {

	    if ( ADIisTokenCstring( pstream, "if", status ) )
	      ADInextToken( pstream, status );
	    else
	      adic_setecs( ADI__SYNTAX, "Illegal token - can only be IF () THEN or end of line at this point", status );
	    }
	  else {

/* Terminal ELSE clause */
	    ADImatchToken( pstream, TOK__END, status );

	    lstx_inscel( adix_prs_cmdlist( pstream, "|endif", &choice, status ),
			 &ipoint, status );

/* Match the "endif" */
	    ADInextToken( pstream, status );

	    more = ADI__false;
	    }
	  }

/* The keyword was ENDIF. Flag end of loop */
	else
	  more = ADI__false;
	}
      }
    }
  else {

/* Parse a single statement, and put into the action list */
    action = lstx_cell( adix_prs_cmd( pstream, status ), ADI__nullid, status );

/* Add action list to output args */
    lstx_inscel( action, &ipoint, status );
    }

/* Return the expression tree */
  return ADIetnNew( adix_clone( K_Try, status ), robj, status );
  }


ADIobj adix_prs_while( int narg, ADIobj args[], ADIstatus status )
  {
  ADIobj	pstream = args[0];
  int           choice;
  ADIobj        robj = ADI__nullid;     /* Returned object */
  ADIobj        action;                 /* Action procedure */
  ADIobj     	test;                   /* The WHILE test expression */

/* Skip the command name */
  ADInextToken( pstream, status );

  ADImatchToken( pstream, TOK__LPAREN, status );
  test = ADIparseExpInt( pstream, 1, status );
  ADImatchToken( pstream, TOK__RPAREN, status );

  ADImatchToken( pstream, TOK__END, status );

/* Action list */
  action = adix_prs_cmdlist( pstream, "|end", &choice, status );

  if ( _ok(status) ) {
    ADInextToken( pstream, status );        /* Match the "end" */

/* Construct argument list */
    robj = lstx_new2( test, action, status );
    }

  return ADIetnNew( adix_clone( K_While, status ), robj, status );
  }


/*
 * Parse a single statement. The token which signifies the end of valid statement
 * (an end-of-line or semicolon) is not matched by this routine. Null statements
 * are ignored, and the null identifier returned.
 *
 */
ADIobj adix_prs_cmd( ADIobj pstream, ADIstatus status )
  {
  ADIobj	cbind;
  ADItokenType	ctok;
  ADIobj	name;
  ADIobj	rval = ADI__nullid;

/* IF command starts with a symbol, look for a command binding */
  if ( ADIcurrentToken(pstream,status) == TOK__SYM ) {

    name = prsx_symname( pstream, status );

    cbind = ADIsymFind( name, ADI__true, ADI__command_sb, status );

/* Located a command symbol? */
    if ( _valid_q(cbind) ) {

/* Invoke parser procedure with stream as argument */
      rval = adix_exemth( ADI__nullid, _mthd_exec(_sbind_defn(cbind)),
		1, &pstream, status );
      }

    else
      rval = ADIparseExpInt( pstream, 1, status );
    }
  else
    rval = ADIparseExpInt( pstream, 1, status );

/* Check for garbage following statement */
  if ( _valid_q(rval) && _ok(status) ) {
    ctok = ADIcurrentToken(pstream,status);

    if ( ctok != TOK__END && ctok != TOK__SEMICOLON ) {
      char	*tstr;
      int	tlen;

      ADIdescribeToken( ctok, &tstr, &tlen );
      adic_setecs( ADI__SYNTAX,
		"Error reading statement - %*s found where semi-colon or end of line expected",
		status, tlen, tstr );
      }
    }

/*   == TOK__END
    ADInextToken( pstream, status ); */

  return rval;
  }

/*
 *  Parse and execute commands appearing on an input stream, sending output
 *  to the output stream if specified
 */
void ADIcmdExec( ADIobj istream, ADIobj ostream, ADIstatus status )
  {
  ADIobj	cmd;
  ADIobj	res;

  _chk_stat;

  do {
    ADInextToken( istream, status );
    cmd = adix_prs_cmd( istream, status );
    if ( _valid_q(cmd) ) {
      res = ADIexprEval( cmd, ADI__true, status );
      if ( _valid_q(ADI_G_curint->exec.name) ) {
	adic_setecs( ADI_G_curint->exec.code, "Break on unhandled exception %S", status,
		ADI_G_curint->exec.name );
	if ( _valid_q(ADI_G_curint->exec.errtext) ) {
	  adic_setecs( ADI_G_curint->exec.code, "%O", status,
			 ADI_G_curint->exec.errtext );
	  }
	ADIexecAcceptI( ADI_G_curint->exec.name, status );
	ems_annul_c( status ); *status = SAI__OK;
	}
      else if ( _valid_q(res) ) {
	if ( _valid_q(ostream) ) {
	  adix_print( ostream, res, 0, 1, status );
	  ADIstrmFprintf( ostream, "\n", status );
	  ADIstrmFflush( ostream, status );
	  }
	adic_erase( &res, status );
	}

/* Destroy the command */
      adic_erase( &cmd, status );
      }
    }
  while ( _ok(status) && (ADIcurrentToken(istream,status) != TOK__NOTATOK) );
  }


void ADIpkgRequire( char *name, int nlen, ADIstatus status )
  {
  char			fname[200];
  FILE			*fp;
  char			*pptr;
  ADIobj		pstream;
  int			ulen = 0, flen;

  _chk_init; _chk_stat;

  if ( ! ADI_G_getenv ) {		/* Not got ADI_LOAD_PATH yet */
    ADI_G_ldpath =
	getenv( "ADI_LOAD_PATH" );

    if ( ADI_G_ldpath )
      ADI_G_ldpath_len = strlen( ADI_G_ldpath );

    ADI_G_getenv = ADI__true;
    }

  _GET_STRING(name,nlen);

  pptr = ADI_G_ldpath;

  do {
    int             i;

    flen = 0;

    if ( pptr ) {
      for( ;pptr[ulen] == ' ' && (ulen<ADI_G_ldpath_len) ; ulen++ )
	{;}
      for( ;pptr[ulen] != PATH_SEPARATOR && (ulen<ADI_G_ldpath_len) ; ulen++ )
	fname[flen++] = pptr[ulen];
      fname[flen++] = FILE_DELIMITER;
      }

    for( i=0; i<nlen; i++ )
      fname[flen++] = name[i];

    strcpy( fname + flen, ".adi" );

    fp = fopen( fname, "r" );

    if ( pptr && ! fp )
      ulen++;
    }
  while ( pptr && (ulen<flen) && ! fp );

  if ( fp ) {

/* Set up parser stream */
    pstream = ADIstrmNew( "r", status );
    ADIstrmExtendFile( pstream, fp, status );

    ADIcmdExec( pstream, ADI_G_curint->StdOut, status );

/* Close stream and file */
    adic_erase( &pstream, status );
    fclose( fp );
    }
  else
    adic_setecs( ADI__INVARG, "Package /%*s/ not found", status,
	nlen, name );
  }


void ADIpkgInit( ADIstatus status )
  {
  DEFINE_CMDPAR_TABLE(ctable)
    CMDPAR_TENTRY( "break",	adix_prs_break ),
    CMDPAR_TENTRY( "defclass",	adix_prs_defclass ),
    CMDPAR_TENTRY( "defproc",	adix_prs_defproc ),
    CMDPAR_TENTRY( "do",	adix_prs_dowhile ),
    CMDPAR_TENTRY( "global",	adix_prs_global ),
    CMDPAR_TENTRY( "if",	adix_prs_if ),
    CMDPAR_TENTRY( "print",	adix_prs_print ),
    CMDPAR_TENTRY( "raise",	adix_prs_raise ),
    CMDPAR_TENTRY( "return",	adix_prs_return ),
    CMDPAR_TENTRY( "try",	adix_prs_try ),
    CMDPAR_TENTRY( "while",	adix_prs_while ),
  END_CMDPAR_TABLE;

  ADIkrnlAddCmdPars( ctable, status );
  }
