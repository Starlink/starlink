/*
*+
*  Name:
*     sc2store.h

*  Purpose:
*     Prototypes for the sc2store library

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Header File

*  Invocation:
*     #include "sc2store.h"

*  Description:
*     Prototypes used by the sc2store functions.

*  Authors:
*     B.D.Kelly (bdk@roe.ac.uk)
*     {enter_new_authors_here}

*  History:
*     2007-04-30 (BDK) this file now contains more than just the
*                prototypes, so include sc2store_pro.h. This allows the
*                prototypes to be created in the data acquisition
*                system, but this is hidden from smurf.
*     2008-05-15 (BDK) add SC2STORETelpar structure.
*     2008-05-28 (BDK) add sc2store_par.h.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#ifndef HEADGEN___src_sc2store_sc2store_h
#define HEADGEN___src_sc2store_sc2store_h 

 
/* Following include is for JCMTState definition */
#include "jcmt/state.h"


struct SC2STORETelpar
{
     double dut1;       /* difference UT1-UTC */
     double latdeg;     /* telescope latitude in degrees */
     double longdeg;    /* telescope east longitude in degrees */
};

typedef struct SC2STORETelpar SC2STORETelpar;

/* constants */

#include "sc2store_par.h"

/* The function prototypes */
#include "sc2store_pro.h"

#endif
