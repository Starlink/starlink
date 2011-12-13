      SUBROUTINE KPG1_LSTAR( DIM1, DIM2, ARRAY, XLOW, YLOW, XHIGH,
     :                       YHIGH, OPENF, FDI, FILNAM, STATUS )
*+
*  Name:
*     KPG1_LSTAR

*  Purpose:
*     Writes a section of a two-dimensional array to a text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_LSTAR( DIM1, DIM2, ARRAY, XLOW, YLOW, XHIGH, YHIGH,
*                      OPENF, FDI, FILNAM, STATUS )

*  Description:
*     This routine takes an input two-dimensional array and lists a
*     specified section of that image, as defined by the x-and-y
*     lower-and-upper bounds to a Fortran file. The file must either be
*     already opened and specified by the input file descriptor, or be
*     created in this routine and will be associated with the supplied
*     parameter name.  The first record in the file corresponds to the
*     lowest index row.

*  Arguments:
*     DIM1 = INTEGER (Given)
*         The first dimension of the two-dimensional array.
*     DIM2 = INTEGER (Given)
*         The second dimension of the two-dimensional array.
*     ARRAY( DIM1, DIM2 )  =  REAL (Given)
*         The two-dimensional array to be listed.
*     XLOW = INTEGER (Given)
*         x co-ord of lower-left corner of sub-array to be listed out.
*     YLOW = INTEGER (Given)
*         y co-ord of lower-left corner of sub-array to be listed out.
*     XHIGH = INTEGER (Given)
*         x co-ord of upper-right corner of sub-array to be listed out.
*     YHIGH = INTEGER (Given)
*         y co-ord of upper-right corner of sub-array to be listed out.
*     OPENF =  LOGICAL (Given)
*         If true the Fortran file is to be associated with %FILNAM and
*         opened. Otherwise the file is assumed to have been opened and
*         has descriptor %FD.
*     FDI = INTEGER (Given)
*         The descriptor associated with the previously opened Fortran
*         file.
*     FILNAM = CHARACTER * ( * ) (Given)
*         Parameter name of the file to be opened and to contain the
*         listing output.
*     STATUS = INTEGER (Given and Returned)
*         Global status value
*
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-OCT-2001 (DSB):
*        Original version.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'PAR_ERR'        ! PAR__ error constants

*  Arguments Given:
      INTEGER DIM1
      INTEGER DIM2
      REAL ARRAY( DIM1, DIM2 )
      INTEGER XLOW
      INTEGER YLOW
      INTEGER XHIGH
      INTEGER YHIGH
      LOGICAL OPENF
      INTEGER FDI
      CHARACTER FILNAM*(*)

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER BUFSIZ              ! maximum number of characters in an
      PARAMETER ( BUFSIZ = 132 )  ! output record

*  Local Variables:
      CHARACTER OUTLIN*( BUFSIZ ) ! Output line for listing
      INTEGER IAT                 ! Used length of string
      INTEGER FD                  ! File description
      INTEGER COL, ROW            ! Counter variables

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If we are to open a new text file...
      IF( OPENF ) THEN

*  Attempt to obtain and open a file to output listing.  A null
*  input forces an exit, but not an error.
         CALL FIO_ASSOC( FILNAM, 'WRITE', 'FORTRAN', BUFSIZ, FD,
     :                   STATUS )

         IF( STATUS .NE. SAI__OK ) THEN
            IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
            FD = -999
            GO TO 999
         END IF

      ELSE
         FD = FDI
      END IF

*  Loop round all the rows.
      DO ROW = YLOW, YHIGH

*  Reset the output string.
         OUTLIN = ' '
         IAT = 0

*  Loop round all the columns, appending the numbers to the buffer end,
*  separated by a space.
         DO COL = XLOW, XHIGH
            IF( BUFSIZ - IAT .GT. 14 ) THEN
               WRITE( OUTLIN( IAT + 1 :), '(G13.6)' ) ARRAY( COL, ROW )
               IAT = IAT + 14
            ELSE
               WRITE( OUTLIN( IAT + 1 :), * ) ' ...'
               GO TO 10
            END IF
         END DO
 10      CONTINUE

*  Write the buffer to the output file.
         CALL FIO_WRITE( FD, OUTLIN( : IAT ), STATUS )

      END DO

*  Close the file.
 999  CONTINUE
      IF( OPENF .AND. FD .NE. -999 ) CALL FIO_CANCL( FILNAM, STATUS )

      END
