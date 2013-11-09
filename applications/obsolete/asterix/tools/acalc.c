/*
 *
 *    Description :
 *
 *     A calculator using the ADI expression parser. Reads from standard
 *     input and writes to standard output.
 */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include "asterix.h"
#include "adi.h"


main()
  {
  ADIstatype	     	gstatus = SAI__OK;
  ADIstatus  		status = &gstatus;

  adi_init( status );

  ADIcmdExec( ADI_G_curint->StdIn, ADI_G_curint->StdOut, status );

  return 1;
  }
