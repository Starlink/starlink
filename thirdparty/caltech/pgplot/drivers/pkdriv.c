#include <stdio.h>

#ifndef convex
#include <string.h>
#endif

/*
 * VAX VMS includes etc..
 */
#ifdef VMS
#include <descrip.h>
#include <ssdef.h>
typedef struct dsc$descriptor_s VMS_string;
#define VMS_STRING(dsc, string) \
  dsc.dsc$w_length = strlen(string); \
  dsc.dsc$b_dtype = DSC$K_DTYPE_T; \
  dsc.dsc$b_class = DSC$K_CLASS_S; \
  dsc.dsc$a_pointer = string;
#endif

/*
 * Allow tkdriv to be calleable by FORTRAN using the two commonest
 * calling conventions. Both conventions append length arguments for
 * each FORTRAN string at the end of the argument list, and convert the
 * name to lower-case, but one post-pends an underscore to the function
 * name (PG_PPU) while the other doesn't. Note the VMS is handled
 * separately below. For other calling conventions you must write a
 * C wrapper routine to call tkdriv() or tkdriv_().
 */
#ifdef PG_PPU
#define PTKDRIV pkdriv_
#else
#define PTKDRIV pkdriv
#endif

/*.......................................................................
 * This is a stub version of the pTk PGPLOT widget device driver to
 * be included in the main PGPLOT library. The real driver resides in a
 * dedicated library, which when cited before libpgplot on the link line,
 * overrides this stub. The rational behind this is that if the real
 * driver were included in the PGPLOT library all applications that are
 * currently linked with PGPLOT would have to be changed to link with the
 * pTk libraries.
 */
#ifdef VMS
void pkdriv(ifunc, rbuf, nbuf, chrdsc, lchr)
     int *ifunc;
     float rbuf[];
     int *nbuf;
     struct dsc$descriptor_s *chrdsc; /* VMS FORTRAN string descriptor */
     int *lchr;
{
  int len = chrdsc->dsc$w_length;
  char *chr = chrdsc->dsc$a_pointer;
#else
void PTKDRIV(ifunc, rbuf, nbuf, chr, lchr, len)
 int   *ifunc, *nbuf, *lchr;
 int   len;
 float rbuf[];
 char  *chr;
{
#endif
  int i;
/*
 * Branch on the specified PGPLOT opcode.
 */
  switch(*ifunc) {

/*--- IFUNC=1, Return device name ---------------------------------------*/

  case 1:
    for(i=0; i < len; i++)
      chr[i] = ' ';
    *lchr = 0;
    break;
  default:
    fprintf(stderr, "/PTK: Unexpected opcode=%d in stub driver.\n", *ifunc);
    *nbuf = -1;
    break;
  };
  return;
}
