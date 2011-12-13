      SUBROUTINE CAT6_CRPAR (CI, NUMPAR, PARNAM, PARVAL, STATUS)
*+
*  Name:
*     CAT6_CRPAR
*  Purpose:
*     Create the parameters in a tab-separated table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_CRPAR (CI, NUMPAR, PARNAM, PARVAL; STATUS)
*       STATUS)
*  Description:
*     Create the parameters in a tab-separated table.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     NUMPAR  =  INTEGER (Given)
*        Number of parameters.
*     PARNAM(NUMCOL)  =  CHARACTER*(*) (Given)
*        Parameter names.
*     PARVAL(NUMCOL)  =  CHARACTER*(*) (Given)
*        Parameter values.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each parameter
*       Generate the default attributes for each parameter.
*       Set the parameter name and convert it to upper case.
*       Set the data type and character width.
*       Set the parameter value.
*       Generate the external format.
*       Create the column.
*     end for
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     4/6/99 (ACD): Original version (from CAT1_CRPAR).
*     15/6/99 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      INTEGER
     :  CI,
     :  NUMPAR
      CHARACTER
     :  PARNAM(NUMPAR)*(*),
     :  PARVAL(NUMPAR)*(*)
*  Status:
      INTEGER STATUS         ! Global status.
*  Local Variables:
      INTEGER
     :  LOOP,    ! Loop index.
     :  QI,      ! Parameter identifier.
     :  LQXFMT   ! Length of QXFMT (excl. trail. blanks).
*
*    Attributes for a single parameter.

      CHARACTER
     :  QNAME*(CAT__SZCMP),  ! Name attribute.
     :  QUNIT*(CAT__SZUNI),  ! Units attribute.
     :  QXFMT*(CAT__SZEXF),  ! External format attribute.
     :  QCOMM*(CAT__SZCOM),  ! Comments attribute.
     :  QVALUE*(CAT__SZVAL)  ! Value attribute.
      INTEGER
     :  QDTYPE,  ! Data type attribute.
     :  QCSIZE,  ! Character size attribute.
     :  QDIM,    ! Dimensionality attribute.
     :  QSIZE    ! Size attribute.
      DOUBLE PRECISION
     :  QDATE    ! Modification date attribute.
      LOGICAL
     :  QPDISP   ! Preferential display flag attribute.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Process each parameter.

         DO LOOP = 1, NUMPAR

*
*          Generate the default attributes for a parameter.

            CALL CAT1_DPATT (QNAME, QDTYPE, QCSIZE, QDIM, QSIZE, QDATE,
     :        QUNIT, QXFMT, QPDISP, QCOMM, QVALUE, STATUS)

*
*          Set the parameter name and convert it to upper case.

            QNAME = PARNAM(LOOP)
            CALL CHR_UCASE (QNAME)

*
*          Set the data type and character width.

            QDTYPE = CAT__TYPEC
            QCSIZE = CAT__SZVAL

*
*          Set the parameter value.

            QVALUE = PARVAL(LOOP)

*
*          Generate the external format.

            LQXFMT = 0
            QXFMT = ' '

            CALL CHR_PUTC ('A', QXFMT, LQXFMT)
            CALL CHR_PUTI (QCSIZE, QXFMT, LQXFMT)

*
*          Create the parameter.

            CALL CAT1_ADDPR (CI, QNAME, QDTYPE, QCSIZE, QDIM, QSIZE,
     :        QDATE, QUNIT, QXFMT, QPDISP, QCOMM, QVALUE, QI, STATUS)

         END DO

      END IF

      END
