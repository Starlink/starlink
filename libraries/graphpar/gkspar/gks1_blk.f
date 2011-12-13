      BLOCK DATA GKS1_BLK
*+
*  Name:
*     GKS1_BLK

*  Purpose:
*     Initialise the GKSGO Common Block so that implicit activation
*     of GKS can be done.

*  Language:
*     Starlink Fortran 77

*  Copyright:
*     Copyright (C) 1983, 1992 Science & Engineering Research Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     SLW: Sid Wright (UCL)
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     18-APR-1983 (SLW):
*        Original Version.
*     09-JAN-1992 (DLT):
*        Reformated comments
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global variables:
      INCLUDE 'gksgo_cmn'                     ! GKS Initialisation Switch

*  Global data:
      DATA GKSSLP /.TRUE./
*.
      END
