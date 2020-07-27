      SUBROUTINE CAT1_NLCKL (FI, INVAL, NULFLG, OUTVAL, STATUS)
*+
*  Name:
*     CAT1_NLCKL
*  Purpose:
*     Check whether a Boolean field value is null or not.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_NLCKL (FI, INVAL; NULFLG, OUTVAL; STATUS)
*  Description:
*     Check whether a Boolean field value is null or not.  Since the
*     Fortran standard provides no portable way to access three distinct
*     LOGICAL values (.TRUE., .FALSE. and NULL), there cannot be a null
*     logical value. So this routine always returns .FALSE. for NULFLG,
*     and OUTVAL is always a copy of INVAL.
*  Arguments:
*     FI  =  INTEGER (Given)
*        Column identifier for the column from which the field value
*        was extracted.
*     INVAL  =  LOGICAL (Given)
*        Field value which is to be checked to determine whether it is
*        null or not.
*     NULFLG  =  LOGICAL (Returned)
*        Flag indicating whether or not the field value is null.  It is
*        coded as follows:
*        .TRUE.  -  the value is null.
*        .FALSE. -  the value is not null; a genuine datum is available.
*     OUTVAL  =  LOGICAL (Returned)
*        The output value.  If INVAL is a genuine datum then OUTVAL is a
*        copy of it.  If INVAL is null then OUTVAL contains the
*        StarBase null value of the appropriate data type.  Note that
*        in this case OUTVAL *always* contains the appropriate StarBase
*        null value, *not* any null value which may have been specified
*        for the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Copyright:
*     Copyright (C) 2020 East Asian Observatory
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
*     DSB: David S Berry (EAO)
*  History:
*     27-JUL-2020 (DSB): Original version, based on cat1_nlckx.gen
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Arguments Given:
      INTEGER
     :  FI
      LOGICAL
     :  INVAL
*  Arguments Returned:
      LOGICAL
     :  NULFLG
      LOGICAL
     :  OUTVAL
*  Status:
      INTEGER STATUS             ! Global status
*.

*  No such thing as a null LOGICAL value.
      NULFLG = .FALSE.
      OUTVAL = INVAL

      END
