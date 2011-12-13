      SUBROUTINE MED3D ( WKARR, WKDIM1, WKDIM2, WKDIM3, WTARR, GDDATA,
     :                   ZNUMBR, MEDNAR, GODNUM, STATUS )
*+
*  Name:
*     MED3D

*  Purpose:
*     Outputs the z axis median for all pixels in the x,y plane

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     SUBROUTINE

*  Invocation:
*     CALL MED3D( WKARR, WKDIM1, WKDIM2, WKDIM3, WTARR, GDDATA, ZNUMBR,
*    :            MEDNAR, GODNUM, STATUS )

*  Description:
*     This routine takes as input a 3-d array with dimensions x,y,z.
*     Returned is a 2-d array, dimensions x,y, which holds the median
*     value for each pixel in the x,y plane, determined along the z
*     axis. Each x,y plane in the z axis direction of the 3-d array may
*     or may not have data in it, and this is indicated by the values
*     found in an input logical array. The final number of x,y planes
*     involved in deriving medians is also returned.

*  Arguments:
*     WKARR( WKDIM1, WKDIM2, WKDIM3 )  =  REAL( READ )
*         3-d array containing data to be statistically analysed
*     WKDIM1 = INTEGER( READ )
*         The first dimension of the input 3-d array.
*     WKDIM2 = INTEGER( READ )
*         The second dimension of the input 3-d array.
*     WKDIM3 = INTEGER( READ )
*         The third dimension of the input 3-d array.
*     WTARR( WKDIM3 )  =  INTEGER( READ )
*         Weighting of each z slice
*     GDDATA( WKDIM3 )  =  LOGICAL( READ )
*         Logical mask to indicate if z slice contains valid data or not
*     ZNUMBR  =  INTEGER( READ )
*         Number of x,y planes along z axis to be looked at
*     MEDNAR( WKDIM1, WKDIM2 )  =  REAL( WRITE )
*         Array to hold medians along z axis for each pixel in x,y plane
*     GODNUM  =  INTEGER( WRITE )
*         Number of z slices finally used in statistics
*     STATUS  =  INTEGER( UPDATE )
*         Global status parameter

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     Work out how many of the used z planes contain valid data
*     Set up variables for dummy array passed to stats subroutine
*     Do for all lines (y direction)
*        Do for all pixels in current line (x direction)
*           Do for all included planes (z direction)
*              If current plane has valid data then
*                 Increment dummy array position index by one
*                 Copy (x,y,z) value from 3-d array into dummy array
*              Endif
*           Endfor
*           Call stats subroutine to return median for current (x,y)
*             plane pixel along z axis
*           Set median output array pixels (x,y) to hold the returned
*            values
*        Endfor
*     Endfor
*     Return

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council. All
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
*     Malcolm Currie STARLINK (RAL::CUR)
*     {enter_new_authors_here}

*  History:
*     1989 May 26: First implementation (RAL::CUR)
*     1989 Aug  8: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*     {enter_further_changes_here}

*  Bugs:
*     None known.
*     {note_new_bugs_here}

*-

*  Type Definitions:

      IMPLICIT  NONE              ! no default typing allowed

*  Global Constants:

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*  Arguments Given:

      INTEGER
     :    WKDIM1, WKDIM2, WKDIM3,
     :    WTARR( WKDIM3 ),
     :    ZNUMBR

      REAL
     :    WKARR( WKDIM1, WKDIM2, WKDIM3 )

      LOGICAL
     :    GDDATA( ZNUMBR )

*  Arguments Returned:

      INTEGER
     :    GODNUM

      REAL
     :    MEDNAR( WKDIM1, WKDIM2 )

*  Status:

      INTEGER  STATUS

*  Local Constants:

      INTEGER
     :    MAXNUM                ! Maximum number of valid x,y planes
      PARAMETER( MAXNUM = 1000 )! allowed - same default as in calling
                                ! routine

*  Local Variables:

      INTEGER
     :    WORK2( MAXNUM ),      ! Work array used by MEDWV
     :    I, J, K, L, M         ! Counter variables

      REAL
     :    DUMARR( MAXNUM ),     ! Dummy array for passing data to median
                                ! s/r
     :    WORK1( MAXNUM ),      ! Work array used by MEDWV
     :    MEDIAN                ! median

*.
*    Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Initialise the good data counter

      GODNUM  =  0

*    Calculate the number of good x,y planes - do it once here, even
*    though we could calculate in the main loop before - this way saves
*    cpu - loop round all the elements of the logical mask to check

      DO  L  =  1, ZNUMBR

*       Check to see if current entry is set true, i.e. that the
*       corresponding x,y plane of the 3-d array contains good data

         IF ( GDDATA( L ) ) THEN

*          Increment counter by one

            GODNUM  =  GODNUM + 1

         END IF
      END DO

*    Now the main business - start by looping round each line (in y)

      DO  J  =  1, WKDIM2

*       Loop round each pixel in current line (in x)

         DO  I  =  1, WKDIM1

*          Reset the dummy array index

            M  =  0

*          Loop along z axis for the current x,y plane pixel

            DO  K  =  1, ZNUMBR

*             Check to see if the current x,y plane contains good data

               IF ( GDDATA( K ) ) THEN

*                Increment the dummy array counter

                  M  =  M + 1

*                Stick current pixel (x,y,z) value into dummy
*                array to be sent to the statistics subroutine

                  DUMARR( M )  =  WKARR( I, J, K )

*             End of if-current-(x,y,z)-value-is-valid check

               END IF

*          End of loop along z axis for current (x,y) pixel

            END DO

*          Send the dummy array off to obtain the median

            CALL MEDWV( WKDIM3, DUMARR, ZNUMBR, 1.0, MAXNUM,
     :                  WTARR, WORK1, WORK2, MEDIAN, STATUS )

*          On return, put the required values into the correct output
*          array positions

            MEDNAR( I, J )  =  MEDIAN

*       End of loop round pixels in current line

         END DO

*    End of loop round lines

      END DO

 999  CONTINUE

*    Return and end

      END
