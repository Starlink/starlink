/* This include file should be used instead of "ast.h" within NDF
   functions that use AST memory management routines. It defines the
   astCLASS macro before including "ast.h". This causes the AST memory
   management functions to use the local "status" variable within the
   NDF function as the global status pointer. This is cheaper than using
   astWatch to establish the global status pointer within every NDF call
   that uses AST. */

#define astCLASS ndf
#include "ast.h"

