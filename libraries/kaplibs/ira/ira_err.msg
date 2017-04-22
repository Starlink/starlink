!+
!  Name:
!     ira_err.msg

!  Purpose:
!     IRA error codes.

!  Copyright:
!     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
!     All Rights Reserved.

!  Licence:
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be
!     useful,but WITHOUT ANY WARRANTY; without even the implied
!     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
!     PURPOSE. See the GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program; if not, write to the Free Software
!     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
!     02110-1301, USA

!  Authors:
!     {enter_authors_here}

!  History:
!     16-APR-2006 (TIMJ):
!         Add prolog.
!     20-APR-2017 (GSB):
!         Convert to messgen format.
!-

!  IRAS90 error numbers 1 - 30 and 242 - 300 are reserved for use
!  by IRA.

.FACILITY   IRA,1507/PREFIX=IRA__

.SEVERITY  ERROR

.BASE       1

AMBPR       <Ambiguous projection specified>
AMBSC       <Ambiguous sky coord system specified>
BAD2V       <2nd sky coord outside range +/- PI/2>
BADAS       <Bad HDS type for AS>
BADBJ       <Invalid epoch type specifier found>
BADEQ       <Invalid equinox specifier found>
BADNC       <Bad value for coordinate index>
BADPA       <BAD projection parameter value>
BADPR       <Unsupported projection specified>
BADSC       <Unsupported sky coordinate system specified>
BADST       <Illegal style value in IRA_RTOC>
BADTR       <Bad no. of variables in TRANSFORM structure>
EXFLD       <Too many fields in a formatted coordinate>
INVID       <Invalid IRA identifier>
MIXFL       <Field out of oder in formatted sky coord>
NOAS        <No AS found>
NOCNV       <Pixel size is becoming indefinately small>
NOEXT       <NDF extension doesn't exist>
NOFRM       <Unable to format floating pointr no>
NOMOR       <No free IRA identifiers are available>
NOPRO       <No projection information found in AS>
RANGE       <Out of range field value found in formatted sky coord>
REDFL       <Redundant field in formatted sky coord>
SCTER       <Invalid terminator found for a formatted>
SING        <Projection is singular>
TFEWP       <Too few projection parameters specified>
TIME        <Inappropriate formatted time value supplied>
TOOSH       <Supplied strings are too short>
UNDAS       <Undefined AS>

.BASE       242

SMALL       <Image coordinates cover too small an area>
ORDER       <Limits are in wrong order>
INIT        <IRA has not been initialised>

.END
