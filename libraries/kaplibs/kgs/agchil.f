C
C PWD: not used in KAPLIBS, so need dummy implementation for NCAR -user
C link flag support.
C

      SUBROUTINE AGCHIL( IFLG, LBNM, LNNO )
      END

      SUBROUTINE AGCHIL_DUMMY( IFLG, LBNM, LNNO )
*+
*  Name:
*     AGCHIL

*  Purpose:
*     Set pen number for axis labels of an NCAR (AUTOGRAPH) display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL AGCHIL( IFLG, LBNM, LNNO )

*  Description:
*     This is a user version of AGCHIL to replace the one in NCAR 
*     library. It is used, together with the routine IRM_STPEN, to set
*     the pen number for the title and the axis labels of an NCAR 
*     (AUTOGRPH) display. It is not called directly by the user
*     program, but by AUTOGRAPH  itself, just before and just after 
*     each of the labels or title. This routine will set the pen number 
*     for the title and the axis labels according to the values of the 
*     NCAR pen setting variables in the common block IRM_NCAR which are 
*     set by IRM_STPEN.
*
*     The title refered to is the title witten by NCAR (AUTOGRAPH)
*     routines EZY, EZMY etc. which have the name of 'T' and line
*     number of 100. The axis labels have the name either 'T' or
*     'B' or 'R' or 'L'.
*
*     To use this routine, it must be linked explicitly with the user
*     program to override the default version of AGCHAX in NCAR library.

*  Arguments:
*     See section 3.25.2 of AUTOGRAPH document.
     
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
*           The Original GKS polyline idex.
*        MCM_OLDTX = INTEGER (Read)
*           The Original GKS text colour index.
*        MCM_TITCL = INTEGER (Read)
*           The colour index for numeric labels.
*        MCM_ALBCL = INTEGER (Read)
*           The colour index for numeric labels.
*        MCM_STDAT = LOGICAL (Read)
*           The data setting flag. If it is true, the variables
*           in the common block have been assigned data by 
*           IRM_STPEN.

*  Arguments Given:
      INTEGER IFLG
      INTEGER LNNO
      
      CHARACTER*(*) LBNM

*.

*  If the variables in the common block have been set values, use them
*  to set pen number. Otherwise do nothing.
      IF ( MCM_STDAT ) THEN

*  If a informational label is to be drawn, ...
         IF ( IFLG .EQ. 0 ) THEN

*  Flush out the previous drawing.
            CALL PLOTIT( 0, 0, 2 )

*  If the label to be drawn is the title of the display,
            IF ( LBNM( : 1 ) .EQ. 'T' .AND. LNNO .EQ. 100 ) THEN
               CALL GSTXCI( MCM_TITCL )

*  If the label to be draw is other axis label,
            ELSE IF ( ( LBNM( : 1 ) .EQ. 'T' .AND. LNNO .NE. 100 ) 
     :               .OR. LBNM( : 1 ) .EQ. 'B' 
     :               .OR. LBNM( : 1 ) .EQ. 'R'  
     :               .OR. LBNM( : 1 ) .EQ. 'L' ) THEN
               CALL GSTXCI( MCM_ALBCL )
            END IF

*  If a informational label has just been drawn, set the GKS colour
*  index to the original setting.
         ELSE

*  Flush out the previous drawing.
            CALL PLOTIT( 0, 0, 2 )

*  And then reset the text colour index.
            CALL GSTXCI( MCM_OLDTX )
         END IF

      END IF     

      END
