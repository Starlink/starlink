#include <string.h>                     /* String stuff from RTL */
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#include "asterix.h"                    /* Asterix definitions */

#include "aditypes.h"
#include "adimem.h"
#include "adikrnl.h"                    /* Internal ADI kernel */
#include "adimem.h"                     /* Allocation routines */
#include "adistrng.h"
#include "adicface.h"
#include "adiparse.h"
#include "adipkg.h"                   	/* Prototypes for this sub-package */

#include "adi_err.h"                    /* ADI error codes */

#ifdef __MSDOS__
#define FILE_DELIMITER '\\'
#else
#define FILE_DELIMITER '/'
#endif

ADIobj		ADI_G_pkglist = ADI__nullid;
char		*ADI_G_ldpath = NULL;
size_t          ADI_G_ldpath_len = 0;
ADIboolean	ADI_G_getenv = ADI__false;

ADIobj adix_prs_defcls( ADIstreamPtr pstream, ADIstatus status )
  {
  int		oflags;			/* Old stream flags */

  ADIobj	cargs[3] = {ADI__nullid, ADI__nullid, ADI__nullid};

/* Tell parser that end-of-lines can be ignored */
  oflags = ADIsetStreamAttr( pstream, ADI_STREAM__EOLISP, status );

  ADInextTokenFromStream( pstream, status );

/* Get new class name from stream */
  cargs[0] = prsx_symname( pstream, status );
  ADInextTokenFromStream( pstream, status );

/* Parse superclass list. This updates both the superclass list and the */
/* members list (due to inherited members) */
  if ( pstream->ctok.t == TOK__SYM )
    ADIparseClassSupers( pstream, cargs+1, cargs+2, status );

/* Parse the class member list */
  if ( ADImatchTokenFromStream( pstream, TOK__LBRACE, status ) ) {
    ADIparseClassMembers( pstream, cargs+2, status );

    if ( pstream->ctok.t == TOK__RBRACE )
      ADInextTokenFromStream( pstream, status );
    else {
      adic_setecs( ADI__INVARG, "Closing brace expected", status );
      }
    }

/* Restore stream flags */
  ADIputStreamAttrs( pstream, oflags, status );

/* Define class, ignoring returned identifier */
  return ADIdefClass_i( 3, cargs, status );
  }


void adix_prs_cmd( ADIstreamPtr pstream, ADIstatus status )
  {
  ADIobj	sdat = ADI__nullid;

  if ( pstream->ctok.t == TOK__SYM ) {
    if ( ADIisTokenCstring( pstream, "defclass", status ) ) {
      sdat = adix_prs_defcls( pstream, status );
      }
    else {
      adic_setetc( "CMD", pstream->ctok.dat, pstream->ctok.nc );
      adic_setecs( ADI__INVARG, "Unknown command name /^CMD/", status );
      }
    }

  else if ( pstream->ctok.t == TOK__END )
    ADInextTokenFromStream( pstream, status );

  if( _ok(status) && _valid_q(sdat) )
    adic_print( sdat, status );
  }


void adix_prs_cmds( ADIstreamPtr pstream, ADIstatus status )
  {
  _chk_stat;

  while ( _ok(status) && (pstream->ctok.t != TOK__NOTATOK) )
    adix_prs_cmd( pstream, status );
  }


void ADIpkgRequire( char *name, int nlen, ADIstatus status )
  {
  int			flen = 0;
  char			fname[200];
  FILE			*fp;
  ADIstream     	stream;
  char			*pptr;
  ADIstreamPtr  	pstream = &stream;
  int			ulen = 0;

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

    if ( pptr ) {
      for( ;pptr[ulen] == ' ' && (ulen<ADI_G_ldpath_len) ; ulen++ )
	{;}
      for( ;pptr[ulen] != ';' && (ulen<ADI_G_ldpath_len) ; ulen++ )
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
    ADIclearStream( pstream, status );    /* Set up a parser stream */
    ADIextendStreamFile( pstream, fp, status );

    ADInextTokenFromStream( pstream, status );

    adix_prs_cmds( pstream, status );

    ADIclearStream( pstream, status );    /* Set up a parser stream */
    }
  else {
    adic_setetc( "PKG", name, nlen );
    adic_setecs( ADI__INVARG, "Package /^PKG/ not found", status );
    }
  }
