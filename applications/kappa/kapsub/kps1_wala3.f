      SUBROUTINE KPS1_WALA3( MAP, LBNDX, UBNDX, LBNDY, UBNDY, ERRLIM,
     :                       XAMAP, YAMAP, STATUS )
*+
*  Name:
*     KPS1_WALA3

*  Purpose:
*     Find the input pixel coordinates corresponding to the centre of
*     each output pixel created by WCSALIGN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_WALA3( MAP, LBNDX, UBNDX, LBNDY, UBNDY, ERRLIM,
*                      XAMAP, YAMAP, STATUS )

*  Description:
*     In principle, one could fill the XAMAP and YAMAP arrays by simply
*     transforming every point using the supplied Mapping. However,
*     this would take a very long time to do for anything other than a
*     very small image. Much CPU time can be saved by approximating the
*     full projection mappings by six coefficient linear mappings. For
*     most uses, the mapping over the entire image will be describable
*     by a single linear transformation, but for large ones this may not
*     be the case. This routine adopts a recursive approach in which
*     checks are made on succesively smaller regions until a region is
*     found which can be described by a linear transformation.
*
*     This process is implemented in terms of an imaginary recursive
*     subroutine FILL. Fortran does not support recursive subroutines
*     (i.e.  subroutines which can call themselves). Therefore a
*     recursive subroutine calling mechanism must be simulated using
*     local arrays to store the local variables for each entry to FILL.
*
*     The specification for FILL is:
*
*     SUBROUTINE FILL( MAP, LBNDX, UBNDX, LBNDY, UBNDY, IB1, IB2, JB1,
*                      JB2, XAMAP, YAMAP, STATUS )
*
*     where MAP, LBNDX, UBNDX, LBNDY, UBNDY, XAMAP and YAMAP have just the
*     same meaning as for the "real" subroutine KPS1_WALA3. The additional
*     arguments IB1, IB2, JB1, JB2 define the section of the output arrays
*     XAMAP and YAMAP which are to be filled.
*
*     FILL first transforms a grid of 9 points spread evenly over the
*     section (IB1:IB2, JB1:JB2) of the output image, producing
*     corresponding input pixel coordinates. The supplied Mapping is used
*     for this purpose. A linear transformation is then calculated between
*     the 9 corresponding positions using a least squares criterion. If the
*     maximum error introduced by this fit at the 9 test points is acceptably
*     small, then the fit is used to fill the specified section of the output
*     arrays, and the subroutine FILL returns. If the linear fit is not
*     acceptable then the specified section is divide into 4 quarters, and
*     FILL is called to fill each quarter in turn.

*  Arguments:
*     MAP = INTEGER (Given)
*        AST Mapping from input pixel co-ordinates to reference (i.e.
*        output) pixel co-ordinates.
*     LBNDX = INTEGER (Given)
*        The lower X bound of the output image.
*     UBNDX = INTEGER (Given)
*        The upper X bound of the output image.
*     LBNDY = INTEGER (Given)
*        The lower Y bound of the output image.
*     UBNDY = INTEGER (Given)
*        The upper Y bound of the output image.
*     ERRLIM = DOUBLE PRECISION (Given)
*        The position accuracy required when re-sampling the input NDF.
*        Given as a number of pixels.
*     XAMAP( LBNDX:UBNDX, LBNDY:UBNDY ) = DOUBLE PRECISION (Returned)
*        Holds the input X pixel coordinate corresponding to the centre
*        of each output pixel.
*     YAMAP( LBNDX:UBNDX, LBNDY:UBNDY ) = DOUBLE PRECISION (Returned)
*        Holds the input Y pixel coordinate corresponding to the centre
*        of each output pixel.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Variables containing "A" in their names are used to store
*     information relating to the input NDF. Variables containing "B"
*     in their names are used to store information relating to the
*     output NDF.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-OCT-1998 (DSB):
*        Original version, based on IRAS90:SALIA3.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER MAP
      INTEGER LBNDX
      INTEGER UBNDX
      INTEGER LBNDY
      INTEGER UBNDY
      DOUBLE PRECISION ERRLIM

*  Arguments Returned:
      DOUBLE PRECISION XAMAP( LBNDX:UBNDX, LBNDY:UBNDY )
      DOUBLE PRECISION YAMAP( LBNDX:UBNDX, LBNDY:UBNDY )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXENT             ! Max. no. of recursive entries to the
      PARAMETER ( MAXENT = 5 )   ! imaginary subroutine FILL

*  Local Variables:
      INTEGER
     :         I,        ! Pixel index within a row of the output image.
     :         IB1,      ! Lower X bound of region being filled.
     :         IB2,      ! Upper X bound of region being filled.
     :         ICOL(3),  ! X index of each column of test points.
     :         IFIT,     ! Type of linear fit required.
     :         IIB1,     ! Value of IB1 to be used on next call to FILL.
     :         IIB2,     ! Value of IB2 to be used on next call to FILL.
     :         J,        ! Index of a row in the output image.
     :         JB1,      ! Lower Y bound of region being filled.
     :         JB2,      ! Upper Y bound of region being filled.
     :         JJB1,     ! Value of JB1 to be used on next call to FILL.
     :         JJB2,     ! Value of JB2 to be used on next call to FILL.
     :         JROW(3),  ! Y index of each row of test points.
     :         NBAD,     ! No. of bad test points.
     :         NENTRY,   ! Current no. of active entries to FILL.
     :         NK,       ! No. of distinct test point.
     :         QCOL,     ! Current quarter within row of quarters.
     :         QROW      ! Current row of quarters (1 or 2).

      INTEGER
     :         SIB1( MAXENT ),      ! Saved values of IB1
     :         SIB2( MAXENT ),      ! Saved values of IB2
     :         SICOL( 3, MAXENT ),  ! Saved values of ICOL
     :         SJB1( MAXENT ),      ! Saved values of JB1
     :         SJB2( MAXENT ),      ! Saved values of JB2
     :         SJROW( 3, MAXENT ),  ! Saved values of JROW
     :         SQCOL( MAXENT ),     ! Saved values of QCOL
     :         SQROW( MAXENT )      ! Saved values of QROW

      LOGICAL
     :         FILLED    ! True if current section has been filled.

      DOUBLE PRECISION
     :         C(6),     ! Co-efficients of linear transformation.
     :         MAXERR,   ! Max. error between linear fit and projection
     :                   ! mappings, at the test points.
     :         RMSERR,   ! RMS error between linear fit and projection
     :                   ! mappings, at the test points.
     :         X,        ! X pixel coordinate corresponding to index I.
     :         XA(9),    ! X coord. (within input image) of test points
     :         XB(9),    ! X coord. (within output image) of test points
     :         XTERM,    ! I/p X term independant of o/p X position.
     :         Y,        ! Y pixel coordinate corresponding to index J.
     :         YA(9),    ! Y coord. (within input image) of test points
     :         YB(9),    ! Y coord. (within output image) of test points
     :         YTERM     ! I/p Y term independant of o/p Y position.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Fill the returned X and Y maps with bad values.
      DO J = LBNDY, UBNDY
         DO I = LBNDX, UBNDX
            XAMAP( I, J ) = AST__BAD
            YAMAP( I, J ) = AST__BAD
         END DO
      END DO

*  Initialise the bounds of the area which is to be filled with X and Y
*  coordinate values, to be the area of the whole output image.
      IB1 = LBNDX
      IB2 = UBNDX
      JB1 = LBNDY
      JB2 = UBNDY

*  Initialise the number of active entries to the recursive subroutine
*  FILL.
      NENTRY = 0

*  This is the entry point to the recursive subroutine FILL. It is
*  equivalent to the statement...
*
*     SUBROUTINE FILL( IDAB, MAP, SCS, LBNDX, UBNDX, LBNDY, UBNDY,
*                      IB1, IB2, JB1, JB2, XAMAP, YAMAP, STATUS )

 10   CONTINUE
      NENTRY = NENTRY + 1

*  "Inherited status" check. Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Set a flag to indicate that  the current section has not yet been
*  filled with coordinate values.
      FILLED = .FALSE.

*  Tranform a grid of up to nine test points explicitly using the
*  supplied Mapping.
      CALL KPS1_WALA6( MAP, LBNDX, UBNDX, LBNDY, UBNDY, IB1, IB2,
     :                 JB1, JB2, XAMAP, YAMAP, XA, YA, XB, YB, NK,
     :                 ICOL, JROW, NBAD, STATUS )

*  If none of the test point positions were bad, find the linear
*  transformation from (X,Y) coordinates in the output image to (X,Y)
*  coordinates in the input image, which minimises the sum of squared
*  residuals at the test points.
      IF( NBAD .EQ. 0 ) THEN
         IFIT = 4
         CALL KPG1_LINTD( NK, XB, YB, XA, YA, IFIT, C, MAXERR, RMSERR,
     :                    STATUS )

*  If the maximum error between the positions of any of the test points
*  found using the linear fit, and using the full projections, is less
*  than a given fraction of a pixel, fill the current area of the output
*  X and Y maps using the linear transformation. The four corners pixels
*  are omitted so that they retain the values calculated from the full
*  projection mappings (this is done because the corner values may be
*  needed to define a linear fit in an adjacent area).
         IF( MAXERR .LE. ERRLIM ) THEN

*  Do the first row (omitting the first and last columns).
            Y = DBLE( JB1 ) - 0.5D0
            XTERM = C( 1 ) + C( 3 )*Y
            YTERM = C( 4 ) + C( 6 )*Y

            DO I = IB1 + 1, IB2 - 1
               X = DBLE( I ) - 0.5D0
               XAMAP( I, JB1 ) = XTERM + C( 2 )*X
               YAMAP( I, JB1 ) = YTERM + C( 5 )*X
            END DO

*  Do the remaining rows, excluding the last row.
            DO J = JB1 + 1, JB2 - 1
               Y = DBLE( J ) - 0.5D0
               XTERM = C( 1 ) + C( 3 )*Y
               YTERM = C( 4 ) + C( 6 )*Y

               DO I = IB1, IB2
                  X = DBLE( I ) - 0.5D0
                  XAMAP( I, J ) = XTERM + C( 2 )*X
                  YAMAP( I, J ) = YTERM + C( 5 )*X
               END DO

            END DO

*  Do the last row (omitting the first and last columns).
            Y = DBLE( JB2 ) - 0.5D0
            XTERM = C( 1 ) + C( 3 )*Y
            YTERM = C( 4 ) + C( 6 )*Y

            DO I = IB1 + 1, IB2 - 1
               X = DBLE( I ) - 0.5D0
               XAMAP( I, JB2 ) = XTERM + C( 2 )*X
               YAMAP( I, JB2 ) = YTERM + C( 5 )*X
            END DO

*  Set a flag to indicate that the current section has been filled.
            FILLED = .TRUE.

         END IF

*  If all the test point positions were bad, fill the current section
*  with bad values.
      ELSE IF( NBAD .EQ. NK ) THEN

         DO J = JB1, JB2
            DO I = IB1, IB2
               XAMAP( I, J ) = AST__BAD
               YAMAP( I, J ) = AST__BAD
            END DO
         END DO

*  Set a flag to indicate that the current section has been filled.
         FILLED = .TRUE.

      END IF

*  If the current section has not yet been filled, it is divided into
*  four quarters and the recursive subroutine FILL re-entered to fill
*  each quarter in turn. Note, use of DO loops is avoided because jumps
*  would need to be made out of and back into the DO loop, which may
*  cause problems with do loop termination criteria being lost. DO loops
*  are therefore simulated using GO TO and CONTINUE statements.
      IF( .NOT. FILLED ) THEN

*  If the max. number of entries has been reached, calculate the X
*  and Y coordinates explicitly using the full projection mappings.
         IF( NENTRY .GT. MAXENT ) THEN
            CALL KPS1_WALA5( MAP, LBNDX, UBNDX, LBNDY, UBNDY, IB1, IB2,
     :                       JB1, JB2, XAMAP, YAMAP, STATUS )

*  Otherwise, divide the current section into four quarters and fill
*  each quarter in turn using the recursive subroutine FILL. Note, use
*  of DO loops is avoided because jumps would need to be made out of
*  and back into the DO loop, which may cause problems with do loop
*  termination criteria being lost. DO loops are therefore simulated
*  using GO TO and CONTINUE statements.
         ELSE

*  Loop round each row of quarters. This is equivalent to...
*           DO QROW = 1, 2

            QROW = 1
 20         CONTINUE

*  Loop round each quarter in the current row. This is equivalent to...
*              DO QCOL = 1, 2

               QCOL = 1
 30            CONTINUE

*  Store the upper and lower Y bound of each quarter in the current row.
*  The bounds of each quarter are determined by the positions of the
*  nine test points used earlier.
                  JJB1 = JROW( QROW )
                  JJB2 = JROW( QROW + 1 )

*  Store the upper and lower X bound of the current quarter.
                  IIB1 = ICOL( QCOL )
                  IIB2 = ICOL( QCOL + 1 )

*  Now call the recursive subroutine FILL to fill the current quarter.
*  This is equivalent to the statement....

*                 CALL FILL( IDAB, MAP, SCS, LBNDX, UBNDX, LBNDY,
*                            UBNDY, IIB1, IIB2, JJB1, JJB2, XAMAP,
*                            YAMAP, STATUS )

*---------------------------------------------------------------------
*  The simulated calling mechanism requires all variables which will be
*  needed in future by the current entry to FILL, to be saved. Do this
*  first.
                  SIB1( NENTRY ) = IB1
                  SIB2( NENTRY ) = IB2
                  SJB1( NENTRY ) = JB1
                  SJB2( NENTRY ) = JB2
                  SQCOL( NENTRY ) = QCOL
                  SQROW( NENTRY ) = QROW
                  SICOL( 1, NENTRY ) = ICOL( 1 )
                  SICOL( 2, NENTRY ) = ICOL( 2 )
                  SICOL( 3, NENTRY ) = ICOL( 3 )
                  SJROW( 1, NENTRY ) = JROW( 1 )
                  SJROW( 2, NENTRY ) = JROW( 2 )
                  SJROW( 3, NENTRY ) = JROW( 3 )

*  Pass the supplied argument values IIB1, IIB2, JJB1, JJB2 to the
*  "dummy" arguments IB1, IB2, JB1, JB2.
                  IB1 = IIB1
                  IB2 = IIB2
                  JB1 = JJB1
                  JB2 = JJB2

*  Now jump back to re-enter FILL.
                  GO TO 10

*  This is the point to which the above simulated call to FILL returns.
 40               CONTINUE

*  This is the end of the simulated CALL statement.
*----------------------------------------------------------------------

*  End the simulated do loop for the quarters in the current row...
*              END DO

               QCOL = QCOL + 1
               IF( QCOL .LE. 2 ) GO TO 30

*  End the simulated do loop for the rows of quarters...
*           END DO

            QROW = QROW + 1
            IF( QROW .LE. 2 ) GO TO 20

         END IF

      END IF

*  This is the end of the recursive subroutine FILL, i.e. it is
*  equivalent to...

*     RETURN

*----------------------------------------------------------------------
*  Reduce the number of active entries by one.
      NENTRY = NENTRY - 1

*  If any active entries remain, restore the saved variable values for
*  this entry. If no active entries remain, return to the calling
*  routine.
      IF( NENTRY .GT. 0 ) THEN
         IB1 = SIB1( NENTRY )
         IB2 = SIB2( NENTRY )
         JB1 = SJB1( NENTRY )
         JB2 = SJB2( NENTRY )
         QCOL = SQCOL( NENTRY )
         QROW = SQROW( NENTRY )
         ICOL( 1 ) = SICOL( 1, NENTRY )
         ICOL( 2 ) = SICOL( 2, NENTRY )
         ICOL( 3 ) = SICOL( 3, NENTRY )
         JROW( 1 ) = SJROW( 1, NENTRY )
         JROW( 2 ) = SJROW( 2, NENTRY )
         JROW( 3 ) = SJROW( 3, NENTRY )

*  Jump back to the point from which the call to FILL was made.
         GO TO 40

      END IF

*  This is the end of the simulated RETURN statement.
*----------------------------------------------------------------------

 999  CONTINUE

      END
