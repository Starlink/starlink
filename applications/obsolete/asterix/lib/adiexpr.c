#include <string.h>                     /* String stuff from RTL */
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>

#include "asterix.h"                    /* Asterix definitions */

#include "adi_err.h"
#include "aditypes.h"
#include "adimem.h"
#include "adisyms.h"
#include "adilist.h"
#include "adikrnl.h"                    /* Internal ADI kernel */
#include "adimem.h"                     /* Allocation routines */
#include "adicface.h"
#include "adiexpr.h"                    /* Prototypes for this sub-package */
#include "adiparse.h"

/*
 * The class definition object
 */
ADIobj	UT_ALLOC_etn = ADI__nullid;


/*
 * Construct a new expression tree node
 */
ADIobj ADIetnNew( ADIobj head, ADIobj args, ADIstatus status )
  {
  ADIobj    id = ADI__nullid;

  if ( _ok(status) ) {
    id = adix_cls_alloc( _cdef_data(UT_ALLOC_etn), status );

    if ( _ok(status) ) {
      _etn_head(id) = head; _etn_args(id) = args;
      }
    }

  return id;
  }


void ADIetnPrint( ADIobj stream, ADIobj node, ADIstatus status )
  {
  ADIobj          arglink;

  if ( _valid_q(node) && _ok(status) ) {
    adix_print( stream, _etn_head(node), 1, ADI__true, status );

    arglink = _etn_args(node);
    if ( arglink ) {
      ADIstrmPrintf( stream, "(", status );
      while ( _valid_q(arglink) && _ok(status) ) {
	adix_print( stream, _CAR(arglink), 1, ADI__true, status );
	arglink = _CDR(arglink);
	if ( _valid_q(arglink) )
	  ADIstrmPrintf( stream, ", ", status );
	}
      ADIstrmPrintf( stream, ")", status );
      }
    }
  }


void ADIetnInit( ADIstatus status )
  {
/* Define the expression node class */
  adic_defcls( "_Expression", "", "head,args", &UT_ALLOC_etn, status );

/* Define the printer */
  adix_def_prnt( UT_ALLOC_etn, ADIetnPrint, status );
  }

