/*
*+
*  Name:
*     ioc_incl.h

*  Purpose:
*     Include file for the ioc package

*  Description:
*     Include file for the ioc package of low level routines
*     used to access magnetic tapes.

*  Copyright:
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     KFH: K F Hartley (RAL)
*     BKM: Brian McIlwrath (RAL)

*  History:
*     12-DEC-1991 (KFH):
*        Original version
*     13-JAN-1993 (BKM):
*        Revised.

*-
*/


#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <errno.h>
#include "mio_err.h"
