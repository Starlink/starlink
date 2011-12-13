      SUBROUTINE sgs_CLRBL (X1, X2, Y1, Y2)
*+
*  Name:
*     CLRBL

*  Purpose:
*     Clear a rectangle with sides parallel to the x and y axes if the
*     workstation can clear a selected area of the display surface.
*     Nothing is done if the display surface is already empty.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     X1 = REAL (Given)
*         X coordinate of bottom left corner
*     Y1 = REAL (Given)
*         Y      "      "    "     "     "
*     X2 = REAL (Given)
*         X      "      "   top  right   "
*     Y2 = REAL (Given)
*         Y      "      "    "     "

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Constants From GKS_PAR:
*     GNEMPT   i      display surface not empty
*     GSOLID   i      fill area interior style solid

*  Errors:
*     Error returned by GKS inquiry

*  Externals:
*     GQWKDU, GQASF, GSASF, GQFAIS, GQFACI, GSFAIS, GSFACI, GFA, GQLN,
*     GQLN, GQLWSC, GQPLCI, GSLN, GSLWSC, GSPLCI, sgs_1ERR, sgs_BOX,
*     sgs_OPOLY, sgs_OTEXT

*  Read From Common:
*     IZTW     i()    zone table - SGS workstation ID
*     ISZID    i      current zone ID
*     IWTID    i()    workstation table - GKS workstation ID
*     IBLKCL   i()    workstation descrition table - block clear mechanism
*     NPOLY    i      length of current polyline
*     NTEXT    i      length of current text string
*     WSNRCL   l()    workstation not really clear

*-

      IMPLICIT NONE

      REAL X1,X2,Y1,Y2

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'

      INCLUDE 'SGS_ERR'


      CHARACTER*5 RNAME
      PARAMETER (RNAME='CLRBL')
      REAL WIDTH,POLYX(4),POLYY(4)
      INTEGER IOLASF(13),NEWASF(13),IWKID,JSTAT
      INTEGER INTS,IERR,ICOLI,ICOL,LTYPE,IDEFMO,IREGMO,IEMPTY,NFRAME

*  Array of ASF flags for setting all ASFs to individual
      DATA NEWASF/13*GINDIV/



*  SGS workstation ID of current zone
      IWKID = ABS(IZTW(ISZID))

*  Flush all pending plotting
      IF (NTEXT.GT.0) CALL sgs_OTEXT
      IF (NPOLY.GT.1) CALL sgs_OPOLY

*  See if the display surface is not empty
      CALL GQWKDU(IWTID(IWKID),IERR,IDEFMO,IREGMO,IEMPTY,NFRAME)
      IF (IERR.NE.0) THEN
         CALL sgs_1ERR(SGS__INQER,RNAME,
     :                                 'Error returned by GQWKDU',JSTAT)
         GO TO 9999
      END IF

*  Only proceed if the display suface is not empty
      IF (IEMPTY.EQ.GNEMPT .OR. WSNRCL(ABS(IZTW(ISZID)))) THEN

*     Can we do it with a polygon fill?
         IF (IBLKCL(IWKID).EQ.1) THEN

*        Save current ASF settings
            CALL GQASF(IERR,IOLASF)
            IF (IERR.NE.0) THEN
               CALL sgs_1ERR(SGS__INQER,RNAME,
     :                                  'Error returned by GQASF',JSTAT)
               GO TO 9999
            END IF

*        Set all ASFs to individual
            CALL GSASF(NEWASF)

*        Save the current polygon fill style and colour index
            CALL GQFAIS(IERR,INTS)
            IF (IERR.NE.0) THEN
               CALL sgs_1ERR(SGS__INQER,RNAME,
     :                      'Error returned by GQFAIS',JSTAT)
               GO TO 9999
            END IF
            CALL GQFACI(IERR,ICOLI)
            IF (IERR.NE.0) THEN
               CALL sgs_1ERR(SGS__INQER,RNAME,
     :                      'Error returned by GQFACI',JSTAT)
               GO TO 9999
            END IF

*        Set to interior style solid and colour index zero
            CALL GSFAIS(GSOLID)
            CALL GSFACI(0)

*        Copy rectangle vertices into array
            POLYX(1) = X1
            POLYX(2) = X2
            POLYX(3) = X2
            POLYX(4) = X1
            POLYY(1) = Y1
            POLYY(2) = Y1
            POLYY(3) = Y2
            POLYY(4) = Y2

*        Fill the area
            CALL GFA(4,POLYX,POLYY)

*        Save the current polyline attributes
            CALL GQLN(IERR,LTYPE)
            IF (IERR.NE.0) THEN
               CALL sgs_1ERR(SGS__INQER,RNAME,
     :                                   'Error returned by GQLN',JSTAT)
               GO TO 9999
            END IF
            CALL GQLWSC(IERR,WIDTH)
            IF (IERR.NE.0) THEN
               CALL sgs_1ERR(SGS__INQER,RNAME,
     :                                 'Error returned by GQLWSC',JSTAT)
               GO TO 9999
            END IF
            CALL GQPLCI(IERR,ICOL)
            IF (IERR.NE.0) THEN
               CALL sgs_1ERR(SGS__INQER,RNAME,
     :                                 'Error returned by GQPLCI',JSTAT)
               GO TO 9999
            END IF

*        Set polyline to solid, normal width and background colour
            CALL GSLN(1)
            CALL GSLWSC(1.0)
            CALL GSPLCI(0)

*        Draw a box to erase the boundary
            CALL sgs_BOX(X1,X2,Y1,Y2)

*        Flush polyline before restoring line styles
            CALL sgs_OPOLY

*        Reset the fill area and polyline attributes back to their
*        original values
            CALL GSFAIS(INTS)
            CALL GSFACI(ICOL)
            CALL GSLN(LTYPE)
            CALL GSLWSC(WIDTH)
            CALL GSPLCI(ICOLI)
            CALL GSASF(IOLASF)
         END IF
      END IF

9999  CONTINUE

      END
