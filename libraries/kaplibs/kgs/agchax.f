C
C PWD: not used in KAPLIBS, so need dummy implementation for NCAR -user
C link flag support.
C
      SUBROUTINE AGCHAX( IFLG, IAXS, IPRT, VILS )
      END

      SUBROUTINE AGCHAX_DUMMY( IFLG, IAXS, IPRT, VILS )
*+
*  Name:
*     AGCHAX

*  Purpose:
*     Set pen number for axes of an NCAR (AUTOGRAPH) display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL AGCHAX( IFLG, IAXS, IPRT, VILS )

*  Description:
*     This is a user version of AGCHAX to replace the one in NCAR 
*     library. It is used, together with the routine IRM_STPEN, to set
*     the pen number for the axis lines, tick marks and numeric labels 
*     of an NCAR (AUTOGRPH) display. It is not called directly by the 
*     user program, but by AUTOGRAPH itself, just before and just after 
*     each of the objects making up an axis is drawn. This routine will
*     set the pen number for each axis portion according to the values 
*     of the NCAR pen setting variables in the common block IRM_COM 
*     which are set by IRM_STPEN.

*     To use this routine, it must be linked explicitly with the user
*     program to override the default version of AGCHAX in NCAR library.

*  Arguments:
*     See section 3.23.2 of AUTOGRAPH document.
     
*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     11-FEB-1991 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'IRM_COM'
*        MCM_OLDPX = INTEGER (Read)
*           The original GKS polyline index.
*        MCM_OLDTX = INTEGER (Read)
*           The original GKS text colour index.
*        MCM_AXPN = INTEGER (Read)
*           The pen number for axis lines.
*        MCM_TKPN = INTEGER (Read)
*           The pen number for tick marks.
*        MCM_NLBCL = INTEGER (Read)
*           The colour index for numeric labels.
*        MCM_STDAT = LOGICAL (Read)
*           The data setting flag. If it is true, the variables
*           in the common block have been assigned data by 
*           IRM_STPEN.

*  Arguments Given:
      INTEGER IFLG
      INTEGER IAXS
      INTEGER IPRT
      REAL VILS

*.

*  If the variable have been set values, use them to set pen number.
*  Otherwise do nothing.
      IF ( MCM_STDAT ) THEN

*  If an object is to be drawn, set the pen.
         IF ( IFLG .EQ. 0 ) THEN

*  Flush out the previous drawing.
            CALL PLOTIT( 0, 0, 2 )

*  If it is an axis line, set pen for the axis lines. 
            IF ( IPRT .EQ. 1 ) THEN
               CALL GSPLI( MCM_AXPN )
         
*  Else if the object to be drawn is a tick mark, 
*  set pen for the tick marks.
            ELSE IF ( IPRT .EQ. 2 .OR. IPRT .EQ. 3 ) THEN
               CALL GSPLI( MCM_TKPN )

*  Else if the object to be drawn is a numeric label, 
*  set colour for the numeric labels.
            ELSE IF ( IPRT .EQ. 4 .OR. IPRT .EQ. 5 ) THEN
               CALL GSTXCI( MCM_NLBCL )
            END IF

*  If an object has just been drawn, reset the pen number to the
*  original setting.
         ELSE

*  Flush out the previous drawing.
            CALL PLOTIT( 0, 0, 2 )
            IF ( IPRT .LE. 3 .AND. IPRT .GE. 1 ) THEN
               CALL GSPLI( MCM_OLDPX )
            ELSE IF ( IPRT .LE. 5 .AND. IPRT .GE. 4 ) THEN
               CALL GSTXCI( MCM_OLDTX )
            END IF
         END IF
      END IF     

      END
