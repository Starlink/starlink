      SUBROUTINE CATGRID (STATUS)
*+
*  Name:
*     CATGRID
*  Purpose:
*     Generate an NDF grid from up to three columns in a catalogue.
*  Language:
*     Fortran 77.
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL (STATUS)
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Description:
*     Generate a grid, formatted as a Starlink NDF file, from up to
*     three columns in a catalogue.  If one column is specified the
*     output grid corresponds to a histogram, two columns correspond
*     to a two-dimensional 'image' and three columns to a data cube.
*
*     The dimensionality of the grid (1, 2 or 3) is specified.  Then,
*     for each axis of the grid, the name of the corresponding column
*     in the catalogue and the number of elements in the grid along the
*     axis are given.  The limits of the grid along each axis correspond
*     to the range of values of the corresponding catalogue column.  The
*     value of each element in grid is set to the number of points which
*     lie within it.  Optionally the grid may be normalised by dividing
*     by the total number of points in the catalogue.
*
*     The grids generated can be displayed and manipulated using Starlink
*     software such as GAIA (SUN/214), KAPPA (SUN/95) and Figaro (SUN/86)
*     or visualisation packages such as DX (SUN/203 and SC/2).
*  Usage:
*     catgrid
*  ADAM Parameters:
*     CATIN  =  CHARACTER (read)
*        Name of the input catalogue.
*     NDIM  =  INTEGER (read)
*        Number of dimensions in the output grid.  The permitted values
*        are 1 - 3.
*     COLX  =  CHARACTER (read)
*        Name of the column to be used for the X-axis of the grid.
*     XBINS  =  INTEGER (read)
*        Number of bins in the grid along the X-axis.
*     COLY  =  CHARACTER (read)
*        Name of the column to be used for the Y-axis of the grid.
*     YBINS  =  INTEGER (read)
*        Number of bins in the grid along the Y-axis.
*     COLZ  =  CHARACTER (read)
*        Name of the column to be used for the Z-axis of the grid.
*     ZBINS  =  INTEGER (read)
*        Number of bins in the grid along the Z-axis.
*     GRID  =  NDF (Write)
*        The name of the output data grid or histogram.
*     NORMAL  =  LOGICAL (read)
*        Flag indicating whether the grid of values is to be normalised
*        or not.  If NORMAL is set to TRUE the grid will be normalised
*        (that is, the value of each grid element will be the number of
*        points occupying the element divided by the total number of
*        points in the catalogue); if it is set to FALSE it will not.
*        The default is FALSE.
*     QUIET  =  LOGICAL (read)
*        Operate in quiet mode where warnings are suppressed.  The
*        permitted values are:
*        TRUE  - quiet mode,
*        FALSE - verbose mode.
*  Examples:
*     catgrid
*        The input catalogue will be prompted for, followed by the
*        dimensionality (1, 2 or 3) of the output grid.  For each axis
*        the name of the corresponding catalogue column and the number of
*        elements along the axis are prompted for.  Finally, the name of
*        the NDF file to hold will be prompted for.  An un-normalised
*        grid will be generated.
*     catgrid  normal=true
*        The input catalogue, dimensionality, details of each axis and
*        output NDF file are all prompted for.  A normalised grid will
*        be generated.
*  Algorithm:
*     Obtain and set the quiet mode.
*     Attempt to open the catalogue.
*     If ok then
*       Get the number of rows in the catalogue.
*       Get the name of the catalogue.
*       Get the dimensionality of the grid.
*       If the dimensionality is valid then
*         For each dimension
*           Get the name of the corresponding column.
*           Get an identifier for this column.
*           Get the units of the column.
*           Map a work array for the column.
*           If ok then
*             Get the number of bins for the column.
*           else
*             Set the termination flag.
*           end if
*         end for
*         Read the columns into the work arrays.
*         If ok and more than one row is available then
*           Report the number of rows which will be used.
*           For each column
*             Determine the range of the column.
*             Compute the bin size.
*           end for
*           Begin an NDF context.
*           Attempt to create the NDF grid.
*           If ok then
*             Get the count normalisation flag.
*             Populate the grid.
*             Set the NDF title, label and units.
*             Set the axes.
*             Close the NDF.
*           else
*             Report error creating the NDF.
*           end if
*           End the NDF context.
*         else
*           If the failure is too few rows then
*             Set the status.
*             Report error; the catalogue contains zero or one rows.
*           end if
*         end if
*         Release the work arrays.
*       else
*         Report error: invalid dimensionality.
*       end if
*       Attempt to close the catalogue.
*       Report any error.
*     else
*       Report error; failed to open the catalogue.
*     end if
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     22/6/99 (ACD): Original version.
*     25/6/99 (ACD): First stable version.
*     5/4/01  (ACD): Added the quiet mode.
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CAT_PAR'   ! CAT symbolic constants.
      INCLUDE 'CNF_PAR'   ! CNF functions
*  Status:
      INTEGER STATUS      ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER MAXDIM      ! Maximum dimensionality of the NDF grid.
      PARAMETER (MAXDIM = 3)
*  Local Variables:
      LOGICAL
     :  QUIET,   ! Flag; operate in quiet or verbose (normal) mode?
     :  MORE,    ! Flag; more columns to be processed?
     :  CNORML   ! Flag; are the grid counts to be normalised?
      INTEGER
     :  CI,      ! Catalogue identifier.
     :  ROWS,    ! No. of rows in the catalogue.
     :  NDIM,    ! Dimensionality of the output grid.
     :  CURCOL,  ! Number of the current column.
     :  CURDIM,  ! The currnet dimension.
     :  DEFBIN,  ! Default number of bins.
     :  PTS,     ! Number of rows (or data points) read from catalogue.
     :  NULLS,   ! Number of rows omitted because of null values.
     :  GRIDID,  ! Identifier to NDF grid.
     :  GRDPTR   ! Pointer to NDF grid.
      INTEGER
     :  LCATNM,  ! Length of CATNAM (excl. trail. blanks).
     :  ELEM,    ! Total number of elements in the grid.
     :  FID(MAXDIM),      ! Identifiers to the specified columns.
     :  COLPTR(MAXDIM),   ! Pointers to arrays for data from columns.
     :  BINS(MAXDIM),     ! Number of bins in each column.
     :  LBND(MAXDIM),     ! Lower bounds of NDF grid axes.
     :  UBND(MAXDIM)      ! Upper   "    "   "   "    "  .
      REAL
     :  MAXVAL,           ! Maximum value in current column.
     :  MINVAL(MAXDIM),   ! Minimum value in each column.
     :  BINSIZ(MAXDIM)    ! Bin size for each column.
      CHARACTER
     :  CATNAM*(CAT__SZVAL),         ! Catalogue name.
     :  COLNAM(MAXDIM)*(CAT__SZCMP), ! Column names.
     :  COLUNT(MAXDIM)*(CAT__SZUNI), !   "    units.
     :  PARCOL(MAXDIM)*4, ! ADAM Parameter names for columns.
     :  PARBIN(MAXDIM)*5  !  "       "       "    "  bins.
*  Local Data:
      DATA
     :  PARCOL /'COLX',  'COLY',  'COLZ'/,
     :  PARBIN /'XBINS', 'YBINS', 'ZBINS'/
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Obtain and set the quiet mode.

         CALL PAR_GET0L ('QUIET', QUIET, STATUS)

         IF (QUIET) THEN
            CALL CAT_TUNES ('QUIET', 'YES', STATUS)
         ELSE
            CALL CAT_TUNES ('QUIET', 'NO', STATUS)
         END IF

*
*       Attempt to open the catalogue and proceed if ok.

         CALL CAT_ASSOC ('CATIN', 'READ', CI, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Get the number of rows in the catalogue.

            CALL CAT_TROWS (CI, ROWS, STATUS)

*
*          Get the name of the catalogue.

            CALL CAT_TIQAC (CI, 'NAME', CATNAM, STATUS)

*
*          Get the dimensionality of the grid and proceed if it is valid.

            CALL PAR_GET0I ('NDIM', NDIM, STATUS)
            CALL PAR_CANCL ('NDIM', STATUS)

            IF (NDIM .GE. 1  .AND.  NDIM .LE. 3  .AND.
     :        STATUS .EQ. SAI__OK) THEN

*
*             Get the details of each column which is to constitute
*             the axes (or dimensions) of the grid.

               CURCOL = 0
               DEFBIN = 50

               MORE = .TRUE.

               DO WHILE (MORE)
                  CURCOL = CURCOL + 1

*
*                Get the name of the column and get an identifier for it.

                  CALL PAR_GET0C (PARCOL(CURCOL), COLNAM(CURCOL),
     :              STATUS)
                  CALL PAR_CANCL (PARCOL(CURCOL), STATUS)

                  CALL CAT_TIDNT (CI, COLNAM(CURCOL), FID(CURCOL),
     :              STATUS)

*
*                Get the units of the column.

                  CALL CAT_TIQAC (FID(CURCOL), 'UNITS', COLUNT(CURCOL),
     :              STATUS)

*
*                Map a work array for the column.

                  CALL CAP_CRTAR (ROWS, '_REAL', COLPTR(CURCOL), STATUS)

*
*                If all is ok then get the number of bins into which the
*                column is to be divided.  Otherwise set the termination
*                flag.

                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL PAR_DEF0I (PARBIN(CURCOL), DEFBIN, STATUS)
                     CALL PAR_GET0I (PARBIN(CURCOL), BINS(CURCOL),
     :                 STATUS)
                     CALL PAR_CANCL (PARBIN(CURCOL), STATUS)

                     DEFBIN = BINS(CURCOL)
                  ELSE
                     MORE = .FALSE.
                  END IF

*
*                If all the columns have been processed then set the
*                termination flag.

                  IF (CURCOL .EQ. NDIM) THEN
                     MORE = .FALSE.
                  END IF
               END DO

*
*             Read the columns into the work arrays, report the number
*             of rows rejected because of null values and proceed if ok.

               CALL CAP_GTGDC (CI, ROWS, NDIM, FID, COLPTR, PTS, NULLS,
     :           STATUS)

               IF (NULLS .GT. 0) THEN
                  CALL MSG_SETI ('NULLS', NULLS)
                  CALL MSG_OUT (' ', '^NULLS rows were rejected '/
     :              /'because of null values.', STATUS)
               END IF

               IF (STATUS .EQ. SAI__OK  .AND.  PTS .GT. 0) THEN

*
*                Report the number of rows which will be used.

                  CALL MSG_SETI ('PTS', PTS)
                  CALL MSG_OUT (' ', 'The grid or histogram will '/
     :              /'be constructed from ^PTS points.', STATUS)

*
*                For each column find the range and compute the bin size.

                  DO CURCOL = 1, NDIM
                     CALL CAP_PRNG (PTS, %VAL(CNF_PVAL(COLPTR(CURCOL))),
     :                 MINVAL(CURCOL), MAXVAL, STATUS)

                     BINSIZ(CURCOL) = (MAXVAL - MINVAL(CURCOL) ) /
     :                                  REAL(BINS(CURCOL) - 1)
                  END DO

*
*                Begin the NDF context.

                  CALL NDF_BEGIN

*
*                Attempt to create and map the NDF grid and proceed if ok.

                  DO CURDIM = 1, NDIM
                     LBND(CURDIM) = 1
                     UBND(CURDIM) = BINS(CURDIM)
                  END DO

                  CALL NDF_CREAT ('GRID', '_REAL', NDIM, LBND, UBND,
     :              GRIDID, STATUS)

                  CALL NDF_MAP (GRIDID, 'DATA', '_REAL', 'WRITE',
     :              GRDPTR, ELEM, STATUS)

                  IF (STATUS .EQ. SAI__OK) THEN

*
*                   Get the count normalisation flag.

                     CALL PAR_GET0L ('NORMAL', CNORML, STATUS)
                     CALL PAR_CANCL ('NORMAL', STATUS)

*
*                   Populate the grid.

                     IF (NDIM .EQ. 1) THEN
                        CALL CAP_POPG1 (CNORML, MINVAL(1), BINSIZ(1),
     :                    PTS, %VAL(CNF_PVAL(COLPTR(1))), BINS(1),
     :                    %VAL(CNF_PVAL(GRDPTR)),
     :                    STATUS)
                     ELSE IF (NDIM .EQ. 2) THEN
                        CALL CAP_POPG2 (CNORML, MINVAL(1), BINSIZ(1),
     :                    MINVAL(2), BINSIZ(2), PTS,
     :                    %VAL(CNF_PVAL(COLPTR(1))),
     :                    %VAL(CNF_PVAL(COLPTR(2))), BINS(1), BINS(2),
     :                    %VAL(CNF_PVAL(GRDPTR)), STATUS)
                     ELSE IF (NDIM .EQ. 3) THEN
                        CALL CAP_POPG3 (CNORML, MINVAL(1), BINSIZ(1),
     :                    MINVAL(2), BINSIZ(2), MINVAL(3), BINSIZ(3),
     :                    PTS, %VAL(CNF_PVAL(COLPTR(1))),
     :                    %VAL(CNF_PVAL(COLPTR(2))),
     :                    %VAL(CNF_PVAL(COLPTR(3))),
     :                    BINS(1), BINS(2), BINS(3),
     :                    %VAL(CNF_PVAL(GRDPTR)), STATUS)
                     END IF

*
*                   Set the NDF title, label and comments.

                     IF (CATNAM .NE. ' ') THEN
                        LCATNM = CHR_LEN(CATNAM)
                     ELSE
                        LCATNM = 1
                     END IF

                     CALL NDF_CPUT (CATNAM(1 : LCATNM), GRIDID,
     :                 'TITLE', STATUS)

                     IF (NDIM .GT. 1) THEN
                        CALL NDF_CPUT ('Grid', GRIDID, 'LABEL', STATUS)
                     ELSE
                        CALL NDF_CPUT ('Histogram', GRIDID, 'LABEL',
     :                    STATUS)
                     END IF

                     IF (CNORML) THEN
                        CALL NDF_CPUT ('Normalised counts', GRIDID,
     :                    'UNITS', STATUS)
                     ELSE
                        CALL NDF_CPUT ('Counts', GRIDID, 'UNITS',
     :                    STATUS)
                     END IF

*
*                   Set the axes.

                     DO CURDIM = 1, NDIM
                        CALL CAP_GSTAX (GRIDID, CURDIM, COLNAM(CURDIM),
     :                    COLUNT(CURDIM), MINVAL(CURDIM),
     :                    BINSIZ(CURDIM), STATUS)
                     END DO

*
*                   Close the NDF: unmap it and annul the identifier.

                     CALL NDF_UNMAP (GRIDID, 'DATA', STATUS)
                     CALL NDF_ANNUL (GRIDID, STATUS)

                  ELSE

*
*                   Failed to create the NDF grid; Report an error.

                     CALL ERR_REP ('CATGRID_GRD', 'Failed to '/
     :                 /'create the output grid.', STATUS)

                  END IF

*
*                End the NDF context.

                  CALL NDF_END (STATUS)

               ELSE

*
*                If the failure was caused by insufficient points being
*                obtained from the catalogue then set the status and
*                report an error.

                  IF (STATUS .EQ. SAI__OK  .AND.  PTS .LE. 1) THEN
                     STATUS = SAI__ERROR

                     CALL ERR_REP ('CATGRID_PTS', 'There were too '/
     :                 /'few points to compute a grid or histogram.',
     :                 STATUS)
                  END IF
               END IF

*
*             Release the work arrays.

               DO CURCOL = 1, NDIM
                  CALL CAP_FREAR (COLPTR(CURCOL), STATUS)
               END DO

            ELSE

*
*             Report an error if an invalid dimensionality was given.

               IF (STATUS .EQ. SAI__OK) THEN
                  STATUS = SAI__ERROR

                  CALL MSG_SETI ('NDIM', NDIM)
                  CALL ERR_REP ('CATGRID_DIM', 'Invalid '/
     :              /'dimensionality given: ^NULLS (permitted values '/
     :              /'are 1 to 3).', STATUS)
               END IF
            END IF

*
*          Attempt to close the catalogue.

            CALL CAT_TRLSE (CI, STATUS)

*
*          Report any error.

            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_REP ('CATGRID_ERR', 'Failed to bin the '/
     :           /'catalogue into a grid or histogram.', STATUS)
            END IF

         ELSE

*
*          Failed to open the catalogue; report an error.

            CALL ERR_REP ('CATGRID_OPN', 'Failed to open the '/
     :        /'input catalogue.', STATUS)

         END IF

      END IF

      END
