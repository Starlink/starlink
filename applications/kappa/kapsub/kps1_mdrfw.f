      SUBROUTINE KPS1_MDRFW( STEP, IDIM1, IDIM2, ARRIN, ODIM1, ODIM2,
     :                         ARROUT, STATUS )
*+
*  Name:
*     KPS1_MDRFx

*  Purpose:
*     Expands a 2-dimensional array by reflection about its edges.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MDRFx( STEP, IDIM1, IDIM2, ARRIN, ODIM1, ODIM2, ARROUT,
*                      STATUS )

*  Description:
*     The input array, ARRIN, is copied into the central section of the
*     output array, ARROUT.  The edges of ARROUT are then padded by
*     reflection about the edge pixels of ARRIN.  For example, with
*     STEP = 2 one corner of the input and output arrays would appear
*     as follows :
*
*                                              3 2 1 2 3 3 3
*                                              2 2 1 2 2 2 2
*     corner of     1 1 1 1 1   corresponding  1 1 1 1 1 1 1
*     input array : 1 2 2 2 2   corner of      2 2 1 2 2 2 2
*                   1 2 3 3 3   output array : 3 2 1 2 3 3 3
*                   1 2 3 4 4                  3 2 1 2 3 4 4
*                   1 2 3 4 5                  3 2 1 2 3 4 5
*
*     An immediate return will occur if STATUS has an error value on
*     entry.
*
*     Note: If ARROUT is not large enough to receive the array ARRIN,
*     together with its padding, only part of array ARRIN will be
*     copied.

*  Arguments:
*     STEP = INTEGER (Given)
*        The number of lines/columns in the output array which will be
*        padded by reflection.
*     IDIM1 = INTEGER (Given)
*        The first dimension of the input 2-dimensional array.
*     IDIM2 = INTEGER (Given)
*        The second dimension of the input 2-dimensional array.
*     ARRIN( IDIM1, IDIM2 ) = ? (Given)
*        Data to be copied into the central section of the output
*        array.
*     ODIM1 = INTEGER (Given)
*        The first dimension of the output 2-dimensional array.
*     ODIM2 = INTEGER (Given)
*        The second dimension of the output 2-dimensional array.
*     ARROUT( ODIM1, ODIM2 ) = ? (Returned)
*        Will contain input data array in central section and will be
*        padded at the edge of the array by a reflection about the
*        edges of the input array.
*     STATUS = INTEGER (Given)
*        This is the global status, if this variable has an error value
*        on entry then an immediate return will occur.

*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B or UB as appropriate.  The
*     ARRIN and ARROUT arguments to the routine must have the data
*     type specified.

*  Algorithm:
*     If no error then
*        Copy input array into central area of output array
*        For the first STEP lines of the output array
*           Calculate the index to the line in the output array which is
*             the same number of lines away from the line corresponding
*             to the first line of the input array but on the opposite
*             side of it
*           Copy all the points in this line corresponding to an input
*             array line into the current line
*        Endfor
*        For the last STEP lines of the output array
*           Calculate the index to the line in the output array which is
*             the same number of lines away from the line corresponding
*             to the last line of the input array but on the opposite
*             side of it
*           Copy all the points in this line corresponding to an input
*             array line into the current line
*        Endfor
*        For all lines of output array
*           For the first STEP points in the line
*              Copy the value of the point which is equidistant from the
*                point corresponding to the first point of an input
*                array line but on the opposite side of it
*           Endfor
*           For the last STEP points in the line
*              Copy the value of the point which is equidistant from the
*                point corresponding to the last point of an input
*                array line but on the opposite side of it
*           Endfor
*        Endfor
*     Endif

*  Copyright:
*     Copyright (C) 1983-1984, 1986, 1989, 1993 Science & Engineering
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
*     MJC: Malcolm J. Currie (STARLINK)
*     SMB: Steven Beard (ROE)
*     {enter_new_authors_here}

*  History:
*     26/10/83 (DB):
*        Original version.
*     17/02/84 (DB):
*        Documentation brought up to standard.
*     1986 September 9 (MJC):
*        Renamed parameters section to arguments and tidied.
*     1989 August 7 (MJC):
*        Passed array dimensions as separate variables.
*     1993 July 20 (SMB):
*        Modified to define the variables LASTX and LASTY in such a way
*        that the array ARROUT does not have to be exactly the right
*        size.  If it is too large it is only partly filled.  If it is
*        too small only part of the ARRIN array is copied.  This
*        modification was made to allow median filtering to be repeated
*        at different scale lengths while reusing the same work array.
*     1995 July 27 (MJC):
*        Made generic (from MEDREF).  Used a modern prologue and style.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit types

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SAE standard constants

*  Arguments Given:
      INTEGER STEP
      INTEGER IDIM1
      INTEGER IDIM2
      INTEGER*2 ARRIN( IDIM1, IDIM2 )
      INTEGER ODIM1
      INTEGER ODIM2

*  Arguments Returned:
      INTEGER*2 ARROUT( ODIM1, ODIM2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FIRST              ! Position in output array of first
                                 ! line/column of input array
      INTEGER LASTX              ! Position in output array of last
                                 ! column of input array
      INTEGER LASTY              ! Position in output array of last line
                                 ! of input array
      INTEGER X                  ! X index to output array elements
      INTEGER XREF               ! X index to array elements for
                                 ! 'reflection'
      INTEGER XIN                ! X index to input array elements
      INTEGER Y                  ! Y index to output array elements
      INTEGER YIN                ! Y index to input array elements
      INTEGER YREF               ! Y index to array elements for
                                 ! 'reflection'

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  FIRST is position in output array of line or column corresponding to
*  the first line or column of the input array.
      FIRST = STEP + 1

*  LASTX is position of column in output array corresponding to last
*  column of the input array which may be accommodated.
      LASTX = MIN( STEP + IDIM1, ODIM1 - STEP )

*  LASTY, position in output array of line corresponding to last line
*  of the input array which may be accommodated.
      LASTY = MIN( STEP + IDIM2, ODIM2 - STEP )

*  Copy the input frame into the central section of the output frame.
      DO Y = FIRST, LASTY
         YIN = Y - STEP

         DO X = FIRST, LASTX
            XIN = X - STEP
            ARROUT( X, Y ) = ARRIN( XIN, YIN )
         END DO
      END DO

*  Pad bottom STEP lines of output array by a reflection about the line
*  corresponding to the first line of the input array.
      DO Y = 1, STEP
         YREF = ( 2 * ( FIRST ) ) - Y

         DO  X = FIRST, LASTX
            ARROUT( X, Y ) = ARROUT( X, YREF )
         END DO
      END DO

*  Pad top STEP lines of output array by a reflection about the line
*  corresponding to the last line of the input array.
      DO Y = LASTY + 1, ODIM2
         YREF = ( 2 * LASTY ) - Y

         DO X = FIRST, LASTX
            ARROUT( X, Y ) = ARROUT( X, YREF )
         END DO
      END DO

*  Pad out ends of all lines of the output image.
      DO Y = 1, ODIM2

*  Pad first STEP points of output array line by a reflection about the
*  point corresponding to first point of the input image line.
         DO X = 1, STEP
            XREF = ( 2 * FIRST ) - X
            ARROUT( X, Y ) = ARROUT( XREF, Y )
         END DO

*  Pad last STEP points of output array line by a reflection about
*  point corresponding to the last point of the input image line.
         DO X = LASTX + 1, ODIM1
            XREF = ( 2 * LASTX ) - X
            ARROUT( X, Y ) = ARROUT( XREF, Y )
         END DO
      END DO

      END
