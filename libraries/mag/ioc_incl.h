/* Include file for the ioc package of low level routines
 * used to access magnetic tapes.
 *
 * K F Hartley    RAL   12 December 1991
 * Revised: B K McIlwrath RAL 13 January 1993
 *
 */

 
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <errno.h>
#include "iocerr.h"
