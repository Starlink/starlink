      SUBROUTINE CAT1_DFATT (FNAME, FGENUS, FEXP, FDTYPE, FCSIZE, FDIM,
     :  FSIZE, FNULL, FXCEPT, FSCALE, FZERO, FORDER, FDATE, FUNIT,
     :  FXFMT, FPDISP, FCOMM, STATUS)
*+
*  Name:
*     CAT1_DFATT
*  Purpose:
*     Generate default attributes for a column.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_DFATT (FNAME, FGENUS, FEXP, FDTYPE, FCSIZE, FDIM,
*       FSIZE, FNULL, FXCEPT, FSCALE, FZERO, FORDER, FDATE, FUNIT,
*       FXFMT, FPDISP, FCOMM; STATUS)
*  Description:
*     Generate a default set of attributes for a column.
*  Arguments:
*     FNAME  =  CHARACTER*(*) (Returned)
*        Name attribute.
*     FGENUS  =  INTEGER (Returned)
*        Genus attribute.
*     FEXP  =  CHARACTER*(*) (Returned)
*        Expression attribute.
*     FDTYPE  =  INTEGER (Returned)
*        Data type attribute.
*     FCSIZE  =  INTEGER (Returned)
*        Character size attribute (equals zero if the attribute is not
*        of data type character).
*     FDIM  =  INTEGER (Returned)
*        Dimensionality attribute.
*     FSIZE  =  INTEGER (Returned)
*        Size attribute.
*     FNULL  =  INTEGER (Returned)
*        Null flag attribute.
*     FXCEPT  =  CHARACTER*(*) (Returned)
*        Exception value attribute.
*     FSCALE  =  DOUBLE PRECISION (Returned)
*        Scale factor attribute.
*     FZERO  =  DOUBLE PRECISION (Returned)
*        Zero point attribute.
*     FORDER  =  INTEGER (Returned)
*        Order attribute.
*     FDATE  =  DOUBLE PRECISION (Returned)
*        Modification date attribute.
*     FUNIT  =  CHARACTER (Returned)
*        Units attribute.
*     FXFMT  =  CHARACTER*(*) (Returned)
*        External format attribute.
*     FPDISP  =  LOGICAL (Returned)
*        Preferential display flag attribute.
*     FCOMM  =  CHARACTER*(*) (Returned)
*        Comments attribute.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set each of the attributes to a default value.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     24/6/93 (ACD): Original version.
*     16/2/94 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Returned:
      INTEGER
     :  FGENUS,
     :  FDTYPE,
     :  FCSIZE,
     :  FDIM,
     :  FSIZE,
     :  FNULL,
     :  FORDER
      CHARACTER
     :  FNAME*(*),
     :  FEXP*(*),
     :  FXCEPT*(*),
     :  FUNIT*(*),
     :  FXFMT*(*),
     :  FCOMM*(*)
      DOUBLE PRECISION
     :  FSCALE,
     :  FZERO,
     :  FDATE
      LOGICAL
     :  FPDISP
*  Status:
      INTEGER STATUS             ! Global status
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Define a default value for each attribute of the column.

         FNAME = ' '
         FGENUS = CAT__GPHYS
         FEXP = ' '
         FDTYPE = CAT__TYPER
         FCSIZE = 20
         FDIM = CAT__SCALR
         FSIZE = 1
         FNULL = CAT__NULLD
         FXCEPT = ' '
         FSCALE = 1.0D0
         FZERO = 0.0D0
         FORDER = CAT__NOORD
         CALL CAT1_GTDAT (FDATE, STATUS)
         FUNIT = ' '
         FXFMT = ' '
         FPDISP = .TRUE.
         FCOMM = ' '

      END IF

      END
