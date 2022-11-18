#if !defined( PRM_INCLUDED )   /* Protect against multiple inclusion*/
#define PRM_INCLUDED 1

/*
*  Name:
*     prm.h

*  Purpose:
*     Defines the public interface for the PRM library.

*  Description:
*     This file defines all the public function prototypes
*     provided by the C version of PRM.

*  Authors:
*     DSB: David S Berry (EAO)

*  History:
*     29-AUG-2017 (DSB):
*        Initial version.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*/

/* IEEE floating poitn exceptions ot be handled by PRM */
#define PRM__FPEXCEPTS ( FE_DIVBYZERO | FE_OVERFLOW | FE_INVALID )

/* Required by prm_cgen.h */
#include <stddef.h>
#include <stdint.h>

/* Include the expanded generic prototypes. */
#include "prm_cgen.h"

#endif
