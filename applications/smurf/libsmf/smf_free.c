/*
*+
*  Name:
*     smf_free

*  Purpose:
*     Low-level SMURF free

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Documentation for macro

*  Invocation:
*     pntr = smf_free( void * pntr, int * status );

*  Arguments:
*     pntr = void * (Given)
*        Memory location to be freed.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the SMURF free routine. It should be used in preference
*     to system free() and should be paired with a SMURF allocation
*     routine (smf_malloc). If the free was successful a null pointer
*     is returned, else the original.
*
*     This function is defined as a macro in smf.h.

*  Authors:
*     Tim Jenness (TIMJ)
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-01-25 (TIMJ):
*        Original version.
*     2006-06-01 (TIMJ):
*        Fix warning.
*     2007-12-14 (AGG):
*        Return null pointer if successful
*     2010-05-25 (TIMJ):
*        Replace with macro. Leave prologue in place for documentation.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

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

*  Bugs:
*     {note_any_bugs_here}
*-
*/
