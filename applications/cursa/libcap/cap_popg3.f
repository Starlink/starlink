      SUBROUTINE CAP_POPG3 (CNORML, XMIN, XBIN, YMIN, YBIN, ZMIN, ZBIN,
     :  PTS, XARRAY, YARRAY, ZARRAY, XSIZE, YSIZE, ZSIZE, GRID, STATUS)
*+
*  Name:
*     CAP_POPG3
*  Purpose:
*     Populate a three-dimensional grid.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_POPG3 (CNORML, XMIN, XBIN, YMIN, YBIN, ZMIN, ZBIN,
*       PTS, XARRAY, YARRAY, ZARRAY, XSIZE, YSIZE, ZSIZE; GRID; STATUS)
*  Description:
*     Populate a three-dimensional grid.
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
*     ZMIN  =  REAL (Given)
*        Minimum Z value in the grid.
*     ZBIN  =  REAL (Given)
*        Size of each Z bin in the grid.
*     PTS  =  INTEGER (Given)
*        Number of points in the arrays to be binned.
*     XARRAY(PTS)  =  REAL (Given)
*        Array of X values.
*     YARRAY(PTS)  =  REAL (Given)
*        Array of Y values.
*     ZARRAY(PTS)  =  REAL (Given)
*        Array of Z values.
*     XSIZE  =  INTEGER (Given)
*        X size of the grid.
*     YSIZE  =  INTEGER (Given)
*        Y size of the grid.
*     ZSIZE  =  INTEGER (Given)
*        Z size of the grid.
*     GRID  =  REAL (Returned)
*        Output grid.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the grid to zero.
*     If there are more than zero points to be gridded then
*       For each point
*         Compute the X, Y, Z indices.
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
     :  YSIZE,
     :  ZSIZE
      REAL
     :  XMIN,
     :  XBIN,
     :  YMIN,
     :  YBIN,
     :  ZMIN,
     :  ZBIN,
     :  XARRAY(PTS),
     :  YARRAY(PTS),
     :  ZARRAY(PTS)
*  Arguments Returned:
      REAL
     :  GRID(XSIZE, YSIZE, ZSIZE)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  PT,    ! Current point.
     :  XELEM, ! Current X grid element.
     :  YELEM, !    "    Y  "      "   .
     :  ZELEM  !    "    Z  "      "   .
      REAL
     :  RPTS   ! Number of points as a REAL number.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Initialise the grid to zero.

         DO ZELEM = 1, ZSIZE
            DO YELEM = 1, YSIZE
               DO XELEM = 1, XSIZE
                  GRID(XELEM, YELEM, ZELEM) = 0.0E0
               END DO
            END DO
         END DO

*
*       Proceed if there are some points to be gridded.

         IF (PTS .GT. 0) THEN

*
*          Process each point.

            DO PT = 1, PTS

*
*             Compute the X, Y, Z grid indices for the point.

               XELEM = INT( (XARRAY(PT) - XMIN) / XBIN) + 1
               YELEM = INT( (YARRAY(PT) - YMIN) / YBIN) + 1
               ZELEM = INT( (ZARRAY(PT) - ZMIN) / ZBIN) + 1

*
*             Force the indices to be within the grid.

               XELEM = MAX(1, XELEM)
               YELEM = MAX(1, YELEM)
               ZELEM = MAX(1, ZELEM)

               XELEM = MIN(XELEM, XSIZE)
               YELEM = MIN(YELEM, YSIZE)
               ZELEM = MIN(ZELEM, ZSIZE)

*
*             Increment the appropriate grid element.

               GRID(XELEM, YELEM, ZELEM) = GRID(XELEM, YELEM, ZELEM) + 1
            END DO

*
*          If required then normalise the grid.

            IF (CNORML) THEN
               RPTS = REAL(PTS)

               DO ZELEM = 1, ZSIZE
                  DO YELEM = 1, YSIZE
                     DO XELEM = 1, XSIZE
                        GRID(XELEM, YELEM, ZELEM) =
     :                    GRID(XELEM, YELEM, ZELEM) / RPTS
                     END DO
                  END DO
               END DO
            END IF

         END IF

      END IF

      END
