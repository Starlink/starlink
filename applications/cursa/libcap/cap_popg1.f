      SUBROUTINE CAP_POPG1 (CNORML, XMIN, XBIN, PTS, XARRAY, XSIZE,
     :  GRID, STATUS)
*+
*  Name:
*     CAP_POPG1
*  Purpose:
*     Populate a two-dimensional grid (or histogram).
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_POPG1 (CNORML, XMIN, XBIN, PTS, XARRAY, XSIZE; GRID;
*       STATUS)
*  Description:
*     Populate a two-dimensional grid (or histogram).
*  Arguments:
*     CNORML  =  LOGICAL (Given)
*        Flag; is the grid to be normalised, coded as follows:
*        .TRUE.  -  Normalise,
*        .FALSE. -  do not normalise.
*     XMIN  =  REAL (Given)
*        Minimum X value in the grid.
*     XBIN  =  REAL (Given)
*        Size of each X bin in the grid.
*     PTS  =  INTEGER (Given)
*        Number of points in the arrays to be binned.
*     XARRAY(PTS)  =  REAL (Given)
*        Array of X values.
*     XSIZE  =  INTEGER (Given)
*        X size of the grid.
*     GRID  =  REAL (Returned)
*        Output grid.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the grid to zero.
*     If there are more than zero points to be gridded then
*       For each point
*         Compute the X index.
*         Force the index to be within the grid.
*         Increment the appropriate grid element.
*       end for
*       If normalisation is required then
*         Normalise the grid.
*       end if
*     end if
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     25/6/99 (ACD): Original version.
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
     :  XSIZE
      REAL
     :  XMIN,
     :  XBIN,
     :  XARRAY(PTS)
*  Arguments Returned:
      REAL
     :  GRID(XSIZE)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  PT,    ! Current point.
     :  XELEM  ! Current X grid element.
      REAL
     :  RPTS   ! Number of points as a REAL number.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Initialise the grid to zero.

         DO XELEM = 1, XSIZE
            GRID(XELEM) = 0.0E0
         END DO

*
*       Proceed if there are some points to be gridded.

         IF (PTS .GT. 0) THEN

*
*          Process each point.

            DO PT = 1, PTS

*
*             Compute the X grid index for the point.

               XELEM = INT( (XARRAY(PT) - XMIN) / XBIN) + 1

*
*             Force the index to be within the grid.

               XELEM = MAX(1, XELEM)
               XELEM = MIN(XELEM, XSIZE)

*
*             Increment the appropriate grid element.

               GRID(XELEM) = GRID(XELEM) + 1
            END DO

*
*          If required then normalise the grid.

            IF (CNORML) THEN
               RPTS = REAL(PTS)

               DO XELEM = 1, XSIZE
                  GRID(XELEM) = GRID(XELEM) / RPTS
               END DO
            END IF

         END IF

      END IF

      END
