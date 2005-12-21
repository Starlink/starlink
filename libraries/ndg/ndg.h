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
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*     02-NOV-2005 (TIMJ):
*        Copy from grp.h
*     20-DEC-2005 (TIMJ):
*        Add ndgAsexp

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*/

/* Need Grp type definitions */
#include "star/grp.h"

/* Public function prototypes */
/* -------------------------- */
void ndgAsexp( char * grpexp, int verb, Grp *igrp1, Grp ** igrp2, int *size, int * flag, int *status );
void ndgAssoc( char * param, int verb, Grp **igrp, int *size, int * flag, int *status );
void ndgCreat( char * param, Grp *igrp0, Grp **igrp, int *size, int * flag, int *status);
void ndgNdfas( Grp *igrp, int index, char * mode, int * indf, int * status );
void ndgNdfpr( int indf1, char * clist, Grp *igrp, int index, int * indf2, int * status);

#endif
