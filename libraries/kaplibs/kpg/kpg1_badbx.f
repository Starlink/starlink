      SUBROUTINE KPG1_BADBX( INDF1, OPER, INDF2, NGOOD, STATUS )
*+
*  Name:
*     KPG1_BADBX

*  Purpose:
*     Obtains an NDF section containing all good data in the supplied
*     NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_BADBX( INDF1, OPER, INDF2, NGOOD, STATUS )

*  Description:
*     This routine finds the pixel bounding box that encloses all good
*     data values in the DATA array of supplied NDF. It then either
*     creates and returns an NDF section corresponding to this bounding
*     box, or sets the pixel bounds of the supplied NDF to match the
*     bounding box.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        The input NDF identifier. Note, if OPER is 1 or 2 then any mapped
*        access to the NDF will be unmapped on exit. In addition, if
*        OPER is 2 then any mapped access to NDFs that are located within
*        an extension of the supplied NDF will be unmapped on exit. Also,
*        for OPER 1 and 2, "UPDATE" access is required to the NDF, and
*        (for OPER 2) any extension NDFs.
*     OPER = INTEGER (Given)
*        Indicates how the box should be used.
*
*        1 - the bounds of the supplied NDF will be modified to match the
*        bounding box enclosing the good data.
*
*        2 - the bounds of the supplied NDF will be modified to match the
*        bounding box enclosing the good data. In addition, any NDFs
*        found within the MORE component of the supplied NDF, which
*        have bounds equal to those of the supplied NDF, are changed to
*        match the bounds of the bounding box.
*
*        If any other value is supplied for OPER, INDF2 will be returned
*        holding an NDF identifier for a section of the supplied NDF
*        matching the bounding box.
*     INDF2 = INTEGER (Returned)
*        An identifier for the smallest NDF section that contains all
*        good DATA values in the the input NDF. Returned equal to
*        NDF__NOID if OPER is 1, or if an error occurs.
*     NGOOD = INTEGER (Returned)
*        The number of good DATA values in the supplied NDF. Returned
*        equal to zero if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009-2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S Berry (JACH)
*     MJC: Malcolm J. Currie (MJC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     9-MAR-2009 (DSB):
*        Original version.
*     10-MAR-2011 (DSB):
*        Added argument OPER.
*     9-MAY-2011 (DSB/MJC):
*        Set mandatory STATUS bad before issuing an ERR_REP.
*     2012-05-09 (TIMJ):
*        Add _INT64
*     19-DEC-2019 (DSB):
*        Now a wrapper for KPG1_BADBX8.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SAI__ constants

*  Arguments Given:
      INTEGER INDF1
      INTEGER OPER

*  Arguments Returned:
      INTEGER INDF2
      INTEGER NGOOD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER*8 NGOOD8
*.

*  Call the 8-byte routine.
      CALL KPG1_BADBX8( INDF1, OPER, INDF2, NGOOD8, STATUS )

*  Convert the 8 byte pixel count to 4-byte. Report an error if it
*  overflows.
      NGOOD = NGOOD8
      IF( STATUS .EQ. SAI__OK .AND. INT(NGOOD, 8) .NE. NGOOD8 ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'N', INDF1 )
         CALL ERR_REP( ' ', 'KPG1_BADBX: NDF ''^N'' has too many '//
     :                 'pixels.', STATUS )
      END IF

      END
