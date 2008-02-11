/*
 *+
 *  Name:
 *     gsdac_struct.h

 *  Purpose:
 *     structure definitions for gsd2acsis 

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     C include file

 *  Description:
 *     Structures used by the gsd2acsis library.
 

 *  Authors:
 *     J. Balfour (j.balfour@jach.hawaii.edu)

 *  History:
 *     2008-01-28 (JB):
 *        Original.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

 *-
*/

#ifndef GSDAC_STRUCT_DEFINED
#define GSDAC_STRUCT_DEFINED

struct gsdac_gsd_struct     /* GSD file access parameters */
{
  char *dataPtr;            /* GSD data */
  void *fileDsc;            /* GSD file descriptor */
  void *itemDsc;            /* GSD item descriptors */
};
   
#endif /* GSDAC_STRUCT_DEFINED */
