#include <stdio.h>
#include "export.h"
/* Handy macro */
#define ARRLEN(arr) (sizeof(arr)/sizeof(arr[0]))

/* The functions and procedures */
IDL_VPTR hds2idl( int argc, IDL_VPTR argv[] );
void crehds( int argc, IDL_VPTR argv[] );

int IDL_Load(void)
{
/* These tables contain information on the functions and procedures that
 * make up the HDS2IDL DLM.  The information must be identical to that
 * in hds2idl.dlm
*/
static IDL_SYSFUN_DEF function_addr[] = {
  { hds2idl, "HDS2IDL", 1, 1, 0 },
};
static IDL_SYSFUN_DEF procedure_addr[] = {
  { (IDL_FUN_RET) crehds, "CREHDS", 2, 3, 0 },
};
/* Register our routine. The routines must be specified exactly the same way
 * as in hds2idl.dlm.
*/
return IDL_AddSystemRoutine(function_addr, IDL_TRUE, ARRLEN(function_addr))
   &&  IDL_AddSystemRoutine(procedure_addr, IDL_FALSE, ARRLEN(procedure_addr));
}


