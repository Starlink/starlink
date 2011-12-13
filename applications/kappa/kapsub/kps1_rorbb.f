      SUBROUTINE KPS1_RORBB( NUMRA, ROTSIZ, XLARGE, OFSETL, OFSETS,
     :                         INDEXL, INDEXS, IDIM1, IDIM2, ARRIN,
     :                         ODIM1, ODIM2, ARROUT, WORK, STATUS )
*+
*  Name:
*     KPS1_RORBx

*  Purpose:
*     Rotates square sections of input array into output array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL KPS1_RORBx( NUMRA, ROTSIZ, XLARGE, OFSETL, OFSETS, INDEXL,
*                       INDEXS, IDIM1, IDIM2, ARRIN, ODIM1, ODIM2,
*                       ARROUT, WORK, STATUS )

*  Description:
*     Rotates a ROTSIZ by ROTSIZ square section of ARRIN, pointed at by
*     INDEXL and INDEXS, into ARROUT.  The type of rotation is
*     determined from NUMRA:
*     NUMRA = 1 rotate by 90 degrees clockwise
*           = 3 rotate by 90 deg anticlockwise, equivalent to 270 deg
*               clockwise
*     An immediate return will occur if STATUS has an error value.

*  Arguments:
*     NUMRA = INTEGER (Given)
*        Number of right-angles through which the input data array
*        will be rotated.  This must be either 1 or 3.
*     ROTSIZ = INTEGER (Given)
*        Size of the square subsection for rotation.
*     XLARGE = LOGICAL (Given)
*        Should be .TRUE. if the first dimension of the input data
*        array is greater than the second dimension.
*     OFSETL = INTEGER (Given)
*        Gives offset to position of rotated subsection in the output
*        array along the longer dimension.
*     OFSETS = INTEGER (Given)
*        Gives offset to position of rotated subsection in the output
*        array along the shorter dimension.
*     INDEXL = INTEGER (Given)
*        Index to subsection for rotation along longer dimension of
*        input array.
*     INDEXS = INTEGER (Given)
*        Index to subsection for rotation along shorter dimension of
*        input array.
*     IDIM1 = INTEGER (Given)
*        The first dimension of the input 2-dimensional array.
*     IDIM2 = INTEGER (Given)
*        The second dimension of the input 2-dimensional array.
*     ARRIN( IDIM1, IDIM2 ) = ? (Given)
*        Data to be rotated.
*     ODIM1 = INTEGER (Given)
*        The first dimension of the output 2-dimensional array.
*     ODIM2 = INTEGER (Given)
*        The second dimension of the output 2-dimensional array.
*     ARROUT( ODIM1, ODIM2 ) = ? (Returned)
*        Will hold the rotated data.
*     WORK( ROTSIZ, ROTSIZ ) = ? (Returned)
*        Workspace to hold sub-sections for rotation.
*     STATUS = INTEGER( UPDATE )
*        The global status.

*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B or UB as appropriate.  The
*     ARRIN, ARROUT, and WORK arguments of the routine must have the
*     data type specified.
*     - If the number of right-angles for rotation is not 1 or 3 then
*     STATUS will be set to SAI__ERROR and an error reported.

*  Algorithm:
*     If no error on entry then
*        If number of clockwise 90 degree rotates is 1 or 3 then
*           Transfer square subsection of input image, pointed at by
*             INDEXL and INDEXS, into the work array
*           Rotate the work array using subroutine ROTAS2
*           Calculate the positon subsection should have in the output
*             array
*           Transfer work array to output array
*        Else
*           Set status and report error
*        Endif
*      Endif

*  Copyright:
*     Copyright (C) 1983-1984, 1986, 1988-1989 Science & Engineering
*     Research Council. Copyright (C) 1995 Central Laboratory of the
*     Research Councils. All Rights Reserved.

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
*     DB: Dave Baines (ROE)
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     27/07/1983 (DB):
*        Original version.
*     17/02/1984 (DB):
*        Documentation brought up to standard.
*     1986 September 9 (MJC):
*        Renamed parameters section to arguments and tidied.
*     1988 Jun 22 (MJC):
*        Added identification to error reporting plus an
*        extra status check.
*     1989 August 7 (MJC):
*        Passed array dimensions as separate variables.
*     1995 May 16 (MJC):
*        Made generic from ROTAS1.  Used an SST prologue and modern
*        variable declarations.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SAE global constants

*  Arguments Given:
      LOGICAL XLARGE
      INTEGER NUMRA
      INTEGER ROTSIZ
      INTEGER OFSETL
      INTEGER OFSETS
      INTEGER INDEXL
      INTEGER INDEXS
      INTEGER IDIM1
      INTEGER IDIM2
      BYTE ARRIN( IDIM1, IDIM2 )
      INTEGER ODIM1
      INTEGER ODIM2

*  Arguments Returned:
      BYTE ARROUT( ODIM1, ODIM2 )
      BYTE WORK( ROTSIZ, ROTSIZ )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DELTAX             ! Pointer to subsection in output
                                 ! array, first dimension
      INTEGER DELTAY             ! Pointer to subsection in output
                                 ! array, second dimension
      INTEGER X                  ! Index to element in subsection, first
                                 ! dimension
      INTEGER XIN                ! Index to element in input-array line
      INTEGER XOUT               ! Index to element in output-array line
      INTEGER Y                  ! Index to element in subsection, second
                                 ! dimension
      INTEGER YIN                ! Index to input-array line
      INTEGER YOUT               ! Index to output-array line

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check for the incorrect number of right angles to rotate.
      IF ( ( NUMRA .EQ. 1 ) .OR. ( NUMRA .EQ. 3 ) ) THEN

*  Transfer ROTSIZ by ROTSIZ section of the input array into workspace.
         IF ( XLARGE ) THEN
            DELTAX = INDEXL - 1
            DELTAY = INDEXS - 1

         ELSE
            DELTAX = INDEXS - 1
            DELTAY = INDEXL - 1

         END IF

         DO  Y = 1, ROTSIZ

*  Calculate the index to the input-array line.
            YIN = Y + DELTAY
            DO X = 1, ROTSIZ

*  Calculate the index to thepoint in the input-array line.
               XIN = X + DELTAX
               WORK( X, Y ) = ARRIN( XIN, YIN )
            END DO
         END DO

*  Perform the rotation on the workspace array.
         CALL KPS1_RORAB( NUMRA, ROTSIZ, ROTSIZ, ROTSIZ, ROTSIZ, WORK,
     :                      STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*  Calculate the offsets necessary to get workspace into the correct
*  place in the output array.
            IF ( XLARGE ) THEN
               IF ( NUMRA .EQ. 3 ) THEN
                  DELTAX = OFSETS - INDEXS
                  DELTAY = INDEXL - 1

               ELSE
                  DELTAX = INDEXS - 1
                  DELTAY = OFSETL - INDEXL

               END IF
            ELSE
               IF ( NUMRA .EQ. 3 ) THEN
                  DELTAX = OFSETL - INDEXL
                  DELTAY = INDEXS - 1

               ELSE
                  DELTAX = INDEXL - 1
                  DELTAY = OFSETS - INDEXS

               END IF
            END IF

*  Put the rotated workspace array into the output array.
            DO Y = 1, ROTSIZ

*  Calculate the index to the output-array line.
               YOUT = Y + DELTAY
               DO X = 1, ROTSIZ

*  Calculate the index to the point in the output-array line.
                  XOUT = X + DELTAX
                  ARROUT( XOUT, YOUT ) = WORK( X, Y )
               END DO
            END DO
         END IF
      ELSE

*  Report an error if number of right-angles for rotation is not 1 or 3.
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NUMRA', NUMRA )
         CALL ERR_REP( 'KPS1_RORBx_NORO',
     :     'KPS1_RORBx: The number of right-angle rotations cannot be '/
     :     /'^NUMRA (programming error).', STATUS )
      END IF

      END
