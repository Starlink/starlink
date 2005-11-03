#if !defined( NDG_ADAM_INCLUDED )  /* Include this file only once */
#define NDG_ADAM_INCLUDED
/*
*  Name:
*     ndg_adam.h

*  Purpose:
*     Define the ADAM C interface to the NDG library.

*  Description:
*     This module defines the C interface to the functions of the ADAM NDG
*     library. The file ndg_adam.c contains C wrappers for the Fortran 
*     GRP routines.

*  Notes:
*     - Given the size of the NDg library, providing a complete C
*     interface is probably not worth the effort. Instead, I suggest that 
*     people who want to use NDG from C extend this file (and
*     ndg_adam.c) to include any functions which they need but which are
*     not already included.

*  Authors:
*     DSB: David .S. Berry
*     TIM: Tim Jenness (JAC)

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*     02-NOV-2005 (TIMJ):
*        Copy from grp.h
*/

/* Need Grp type definitions */
#include "star/grp.h"

/* Public function prototypes */
/* -------------------------- */
void ndgAssoc( char * param, int verb, Grp **igrp, int *size, int * flag, int *status );
void ndgCreat( char * param, Grp *igrp0, Grp **igrp, int *size, int * flag, int *status);
void ndgNdfas( Grp *igrp, int index, char * mode, int * indf, int * status );
void ndgNdfpr( int indf1, char * clist, Grp *igrp, int index, int * indf2, int * status);

#endif
