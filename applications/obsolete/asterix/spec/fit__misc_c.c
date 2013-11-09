/*+
 *  Name:
 *     fit_mspecflat_int_c.c
 *
 *  Purpose:
 *     Takes an Asterix model system specification and flattens it (ie. removes
 *     all the brackets) to produce a list of additive terms which consist of
 *     one additive model component and up to (MAXCOMP-1) multiplicative model
 *     components. This routine does not perform any syntax checking other than
 *     that required for a successful parse (eg. it check that parentheses are
 *     balanced but not that you have supplied A1*A2 where A1 and A2 are both
 *     additive models.
 *
 *  Language:
 *
 *     Starlink ANSI C
 *
 *  Invokation :
 *
 *     CALL FIT_MSPECFLAT_INT( SPEC, NTERM, ATERM, ATSIGNS, STATUS )
 *
 *  Description:
 *
 *  Authors:
 *
 *     David J. Allan (ROSAT,University of Birmingham)
 *
 *  History:
 *
 *     24-Jan-1993 (DJA):
 *        Original version.
 *- */

/* Include Statements: */

#include "sae_par.h"			/* Starlink constants */
#include "cnf.h"                        /* Fortran <-> C interfacing */
#include "f77.h"			/* Fortran emulation */
#include "ems.h"          		/* Error handling */
#include <ctype.h>			/* Character processing */

#include "fit_par.h"			/* Asterix fitting constants */


/* Type declarations
 *
 */
typedef
  enum
    {TOK_PLUS,TOK_MINUS,TOK_TIMES,TOK_OPAREN,TOK_CPAREN,TOK_ID,TOK_END}
  LegalToken;

typedef
  struct Node_tag *NodeRef;

typedef
  struct Node_tag
    {
    LegalToken		t;
    int			data;
    NodeRef		lhs, rhs;
    }
  Node;


/* Macro definitions
 *
 */
#define NIL (void *)0
#define _ok(x) ((*x)==0)
#define MAXTERM 	MAXCOMP		/* Maximum number of additive terms */

static LegalToken curtok;
static int	  curtoklen;
static char	  *curtokstart;
static int	  curid=0;
static char *	  modpos[MAXCOMP];
static int	  modlen[MAXCOMP];

#define NullTerm	-1


typedef
  struct
    {
    LegalToken	  t;			/* Token linking terms (+ or -) */
    int		  next;			/* Number of next term */
    int		  len;			/* Number of items in term */
    int		  elem[MAXCOMP];	/* Elements of term */
    }
  TermLink;

static TermLink	  terms[MAXTERM];



static NodeRef NewNode( LegalToken t, int data, NodeRef lhs, NodeRef rhs,
		        int *status )
  {
  NodeRef		node;

  if ( !_ok(status) )
    return NIL;

  node = (NodeRef) malloc( sizeof(Node) );

  node->t = t;
  node->lhs = lhs;
  node->rhs = rhs;
  node->data = data;

  return node;
  }


static LegalToken NextToken( char **buf, int *status )
  {
  char 		ch = **buf;
  LegalToken	tok;

  curtokstart = *buf;
  (*buf)++;
  curtoklen = 1;
  switch ( ch )
    {
    case '+':
      tok = TOK_PLUS;
      break;
    case '-':
      tok = TOK_MINUS;
      break;
    case '*':
      tok = TOK_TIMES;
      break;
    case '(':
      tok = TOK_OPAREN;
      break;
    case ')':
      tok = TOK_CPAREN;
      break;
    case 0:
      tok = TOK_END;
      break;
    default:
      if ( isalnum(ch) )
	{
	curid++;
	modpos[curid-1] = curtokstart;
	do
	  {
	  curtoklen++; ch = *(*buf)++;
	  }
	while ( isalnum(ch) );
	modlen[curid-1] = curtoklen-1;
	(*buf)--;
	tok = TOK_ID;
	}
      else
        {
	*status = SAI__ERROR;
        ems_setc_c( "C", &ch, 1 );
        ems_rep_c( " ", "Illegal character /^C/", status );
        }
    }

  return curtok = tok;
  }


static NodeRef ParseExpr( char **spec, int *status );


static NodeRef ParsePrimary( char **spec, int *status )
  {
  NodeRef		node = NIL;

  if ( !_ok(status) )
    node = NIL;

  else if ( curtok == TOK_ID )
    {
    node = NewNode( TOK_ID, curid, NIL, NIL, status );
    NextToken( spec, status );
    }

  else if ( curtok == TOK_OPAREN )
    {
    NextToken( spec, status );
    node = ParseExpr( spec, status );
    if ( curtok == TOK_CPAREN )
      NextToken( spec, status );
    else
      {
      *status = SAI__ERROR;
      ems_rep_c( " ", "Syntax error - right parenthesis expected", status );
      }
    }
  else
    {
    *status = SAI__ERROR;
    ems_rep_c( " ", "Unexpected token in model specification", status );
    }

  return node;
  }


static NodeRef ParseTerm( char **spec, int *status )
  {
  NodeRef		node = NIL;

  node = ParsePrimary( spec, status );

  while ( (curtok == TOK_TIMES) && _ok(status) )
    {
    NextToken( spec, status );

    node = NewNode( TOK_TIMES, 0, node,
		     ParsePrimary(spec, status),
		     status );
    }

  return node;
  }


static NodeRef ParseExpr( char **spec, int *status )
  {
  NodeRef		node = NIL;
  LegalToken		optok;

  node = ParseTerm( spec, status );

  while ( ((curtok == TOK_PLUS) || (curtok==TOK_MINUS)) && _ok(status) )
    {
    optok = curtok;

    NextToken( spec, status );

    node = NewNode( optok, 0, node,
		    ParseTerm( spec, status),
		    status );
    }

  return node;
  }


static NodeRef TreeCreate( char **spec, int *status )
  {
  curid = 0;
  NextToken( spec, status );

  return ParseExpr( spec, status );
  }

static void TreeFree( NodeRef node, int *status )
  {
  if ( !_ok(status) || (! node) )
    return;

  if ( node->lhs )
    TreeFree( node->lhs, status );
  if ( node->rhs )
    TreeFree( node->rhs, status );

  free( node );
  }


static int GetNewTerm( int *status )
  {
  int		aterm = NullTerm;
  int		i;

  if ( !_ok(status) )
    return aterm;

  for( i=0; i<MAXTERM; i++ )
    if ( ! terms[i].len )
      {
      terms[i].next = NullTerm; aterm = i; terms[i].len = 1; break;
      }

  return aterm;
  }


static int Flatten( NodeRef node, int *status )
  {
  int		i,l_s,r_s,start,aterm,end,nterm,lterm;
  LegalToken	*opaddr;
  int		*linkaddr;
  LegalToken	lop,top,rop;

  if ( !_ok(status) || (! node) )
    return NullTerm;

  if ( node->t != TOK_ID )
    {
    l_s = Flatten( node->lhs, status );
    r_s = Flatten( node->rhs, status );
    }

  if ( (node->t == TOK_PLUS) || (node->t == TOK_MINUS) )
    {
    start = l_s;
    end = start;
    while ( terms[end].next != NullTerm )
      end = terms[end].next;

    terms[end].t = node->t;
    terms[end].next = r_s;
    }
  else if ( node->t == TOK_TIMES )
    {
    linkaddr = &start;
    opaddr = &top;

    lterm = l_s;                      	/* Loop over terms on lhs */
    lop = TOK_PLUS;			/* Implicit leading + on terms */
    do
      {
      aterm = r_s;            		/* Loop over terms on rhs */
      rop = TOK_PLUS;
      do
	{
	if ( terms[lterm].next == NullTerm )/* Last lhs term? */
	  nterm = aterm;
	else
	  {
	  nterm = GetNewTerm( status );
	  terms[nterm] = terms[aterm];
	  }

	if ( lop == rop )		/* Combine additive operators */
	  *opaddr = TOK_PLUS;
	else
	  *opaddr = TOK_MINUS;

	*linkaddr = nterm;		/* Make link to last term */
	linkaddr = &terms[nterm].next;
	opaddr = &terms[nterm].t;

	for ( i=0; i<terms[lterm].len; i++ )
	  terms[nterm].elem[terms[nterm].len++] = terms[lterm].elem[i];

	rop = terms[aterm].t;           /* Next rhs term */
	aterm = terms[aterm].next;
	}
      while ( aterm != NullTerm );	/* While more rhs terms */

      if ( terms[lterm].next == NullTerm )	/* Last lhs term? */
	terms[lterm].len = 0; 		/* Free it */

      lop = terms[lterm].t;           	/* Next lhs term */
      lterm = terms[lterm].next;
      }
    while ( lterm != NullTerm );	/* While more lhs terms */
    }
  else
    {
    start = GetNewTerm( status );
    terms[start].elem[0] = node->data;
    }

  return start;
  }


#ifdef DEBUG
static void FlatShow( int term, int *status )
  {
  int	aterm = term;
  int	i,word,j;

  if ( !_ok(status) )
    return;

  do
    {
    for( i=0; i<terms[aterm].len; i++ )
      {
      if ( i )
	putchar('*');
      word = terms[aterm].elem[i]-1;
      for(j=0; j<modlen[word];j++)
	putchar(modpos[word][j]);
      }

    if ( terms[aterm].next != NullTerm )
      {
      if ( terms[aterm].t == TOK_PLUS )
	printf( "+" );
      else
	printf( "-" );
      }

    aterm = terms[aterm].next;
    }
  while ( aterm != NullTerm );

  printf( "\n" );
  }
#endif


/*  FIT_MSPECFLAT
 *
 *
 */
F77_SUBROUTINE(fit_mspecflat_int)( CHARACTER(spec), INTEGER(nterm),
                                   INTEGER_ARRAY(aterm),
                                   INTEGER_ARRAY(atsign),
                                   INTEGER(status) TRAIL(spec) )
  {
  GENPTR_CHARACTER(spec)
  GENPTR_INTEGER(nterm)
  GENPTR_INTEGER_ARRAY(aterm)
  GENPTR_INTEGER_ARRAY(atsign)
  GENPTR_INTEGER(status)

  int			i;		/* Loop variable */
  LegalToken		lastop;		/* Linking operator between terms */
  char			*mcur,*mspec;		/* Model specification */
  NodeRef		mtree;		/* Parsed model specification */
  int			tlist;		/* Root of expanded expression */
  F77_INTEGER_TYPE	*odata;		/* Loop over output data */

  if ( *status != SAI__OK )		/* Check status on entry */
    return;

  mcur = mspec = cnf_creim( spec, 		/* Import Fortran model spec to C */
                     spec_length);

  mtree = TreeCreate( &mspec, status );	/* Parse expression */

  for (i=0; i<MAXCOMP; i++)		/* Initialise terms storage */
    terms[i].len = 0;

  tlist = Flatten( mtree, status );	/* Flatten expression */

  TreeFree( mtree, status );		/* Release parsed expression */

/*  FlatShow( tlist, status ); */

  cnf_free( mcur );			/* Free temporary string */

  if ( _ok(status) )			/* If all that went ok... */
    {
    odata = (F77_INTEGER_TYPE *) aterm; /* Output data */
    lastop = TOK_PLUS;
    *nterm = 0;

    do					/* Loop over all additive terms */
      {
      for( i=0; i<terms[tlist].len; 	/* Write out model id's */
	   i++ )
        *odata++ = terms[tlist].elem[i];

      for( i=terms[tlist].len; 		/* Fill in empty slots */
           i<MAXTERM; i++ )
         *odata++ = 0;

      if ( lastop == TOK_PLUS )		/* Which sign is this term */
        atsign[(*nterm)++] = 1;
      else
        atsign[(*nterm)++] = -1;

      lastop = terms[tlist].t;		/* The operator linking the next term */

      tlist = terms[tlist].next;	/* Advance to next term */
      }
    while ( tlist != NullTerm );	/* Reach last term yet? */
    }

  if ( *status != SAI__OK ) 		/* Output message if rename failed */
    {
    ems_setc_c( "SPEC", spec,
		spec_length );
    ems_rep_c( " ", "Error flattening model specification /^SPEC/", status );
    ems_rep_c( " ", "...from FIT_MSPECFLAT_INT", status);
    }
  }
