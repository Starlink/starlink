      SUBROUTINE CAP_POPG2 (CNORML, XMIN, XBIN, YMIN, YBIN, PTS,
     :  XARRAY, YARRAY, XSIZE, YSIZE, GRID, STATUS)
*+
*  Name:
*     CAP_POPG2
*  Purpose:
*     Populate a two-dimensional grid.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_POPG2 (CNORML, XMIN, XBIN, YMIN, YBIN, PTS, XARRAY, YARRAY,
*       XSIZE, YSIZE; GRID; STATUS)
*  Description:
*     Populate a two-dimensional grid.
*  Arguments:
*     CNORML  =  LOGICAL (Given)
*        Flag; is the grid to be normalised, coded as follows:
*        .TRUE.  -  Normalise,
*        .FALSE. -  do not normalise.
*     XMIN  =  REAL (Given)
*        Minimum X value in the grid.
*     XBIN  =  REAL (Given)
*        Size of each X bin in the grid.
*     YMIN  =  REAL (Given)
*        Minimum Y value in the grid.
*     YBIN  =  REAL (Given)
*        Size of each Y bin in the grid.
*     PTS  =  INTEGER (Given)
*        Number of points in the arrays to be binned.
*     XARRAY(PTS)  =  REAL (Given)
*        Array of X values.
*     YARRAY(PTS)  =  REAL (Given)
*        Array of Y values.
*     XSIZE  =  INTEGER (Given)
*        X size of the grid.
*     YSIZE  =  INTEGER (Given)
*        Y size of the grid.
*     GRID  =  REAL (Returned)
*        Output grid.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the grid to zero.
*     If there are more than zero points to be gridded then
*       For each point
*         Compute the X and Y, indices.
*         Force the indices to be within the grid.
*         Increment the appropriate grid element.
*       end for
*       If normalisation is required then
*         Normalise the grid.
*       end if
*     end if
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     24/6/99 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      LOGICAL
     :  CNORML
      INTEGER
     :  PTS,
     :  XSIZE,
     :  YSIZE
      REAL
     :  XMIN,
     :  XBIN,
     :  YMIN,
     :  YBIN,
     :  XARRAY(PTS),
     :  YARRAY(PTS)
*  Arguments Returned:
      REAL
     :  GRID(XSIZE, YSIZE)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  PT,    ! Current point.
     :  XELEM, ! Current X grid element.
     :  YELEM  !    "    Y  "      "   .
      REAL
     :  RPTS   ! Number of points as a REAL number.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Initialise the grid to zero.

         DO YELEM = 1, YSIZE
            DO XELEM = 1, XSIZE
               GRID(XELEM, YELEM) = 0.0E0
            END DO
         END DO

*
*       Proceed if there are some points to be gridded.

         IF (PTS .GT. 0) THEN

*
*          Process each point.

            DO PT = 1, PTS

*
*             Compute the X and Y grid indices for the point.

               XELEM = INT( (XARRAY(PT) - XMIN) / XBIN) + 1
               YELEM = INT( (YARRAY(PT) - YMIN) / YBIN) + 1

*
*             Force the indices to be within the grid.

               XELEM = MAX(1, XELEM)
               YELEM = MAX(1, YELEM)

               XELEM = MIN(XELEM, XSIZE)
               YELEM = MIN(YELEM, YSIZE)

*
*             Increment the appropriate grid element.

               GRID(XELEM, YELEM) = GRID(XELEM, YELEM) + 1
            END DO

*
*          If required then normalise the grid.

            IF (CNORML) THEN
               RPTS = REAL(PTS)

               DO YELEM = 1, YSIZE
                  DO XELEM = 1, XSIZE
                     GRID(XELEM, YELEM) = GRID(XELEM, YELEM) / RPTS
                  END DO
               END DO
            END IF

         END IF

      END IF

      END
