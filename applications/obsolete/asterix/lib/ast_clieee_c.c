/*  ast_clieee_c
 *
 *   Clear IEEE floating point exceptions on those machines which generate
 *   them
 *
 *
 */
#include "f77.h"

F77_SUBROUTINE(ast_clieee)()
  {

#if defined(sun4_Solaris) || defined(sun4)
  char *out;

  ieee_flags("clear","exception","inexact",&out);
  ieee_flags("clear","exception","underflow",&out);
#endif

  }
