/* This include file should be used instead of "ast.h" within ARY
   functions that use AST memory management routines. It defines the
   astCLASS macro before including "ast.h". This causes the AST memory
   management functions to use the local "status" variable within the
   ARY function as the global status pointer. This is cheaper than using
   astWatch to establish the global status pointer within every ARY call
   that uses AST. */

#define astCLASS ary
#include "ast.h"

