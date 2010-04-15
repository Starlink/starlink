      SUBROUTINE CAP_GHSRN (AUTOSCL, CXMIN, CXMAX, BINSP, BINDET, NORML,
     :  STATUS)
*+
*  Name:
*     CAP_GHSRN
*  Purpose:
*     Set the histogram X range and other details.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GHSRN (AUTOSCL, CXMIN, CXMAX, BINSP, BINDET, NORML; STATUS)
*  Description:
*     Set the histogram X range and other details.
*  Arguments:
*     AUTOSCL  =  LOGICAL (Given)
*        Flag determining whether or not the plot is to be auto-scaled.
*        It is coded as follows:
*        .TRUE.  -  auto-scale the plot,
*        .FALSE. -  do not auto-scale the plot.
*     CXMIN  =  CHARACTER*(*) (Given)
*        Minimum of the X axis.
*     CXMAX  =  CHARACTER*(*) (Given)
*        Maximum of the X axis.
*     BINSP  =  LOGICAL (Given)
*        Flag indicating how the details of the histogram bins are
*        to be specified, coded as follows:
*        .TRUE.  - by bin width,
*        .FALSE. - by the number of bins.
*     BINDET  =  REAL (Given)
*        Detail defining the histogram bins: either the bin width or
*        number of bins, depending on the value of BINSP.
*     NORML  =  LOGICAL (Given)
*        Flag indicating whether the histogram is to be normalised,
*        coded as follows:
*        .TRUE.  - normalise the histogram,
*        .FALSE. - do not normalise the histogram.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is a catalogue open then
*       If there is a plot open then
*         If not auto-scaling then
*           Attempt to obtain REAL values for the ranges.
*           If ok then
*             If the range is ok then
*               Set the range
*             end if
*               set the bad range flag.
*             end if
*           else
*             set the bad range flag.
*           end if
*         end if
*         If the range is ok then
*           Set the bin width or number of bins.
*           Set the scaling flags.
*         else
*           Report error; bad range.
*         end if
*       else
*         Report warning; no plot open.
*       end if
*     else
*       Report a warning; there is no open catalogue.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     14/9/99 (ACD): Original version (from CAP_GSCRN).
*     16/9/99 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'SGZ_PAR'           ! catview parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! catview common block.
      INCLUDE 'SPLOT_CMN'         ! catview scatterplot common block.
*  Arguments Given:
      LOGICAL
     :  AUTOSCL,
     :  BINSP,
     :  NORML
      CHARACTER
     :  CXMIN*(*),
     :  CXMAX*(*)
      REAL
     :  BINDET
*  Status:
      INTEGER STATUS   ! Global status.
*  Local Variables:
      LOGICAL
     :  RNGOK       ! Flag; is the range ok?
      CHARACTER
     :  CLXMIN*40,  ! Local copy of CXMIN.
     :  CLXMAX*40   !   "    "   "  CXMAX.
      REAL
     :  XMIN,       ! X minimum of range.
     :  XMAX        ! X maximum "    "  .
      INTEGER
     :  CSTAT       ! Local status reading REAL from CHARACTER.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Initialise the 'range ok' flag.

         RNGOK = .TRUE.

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          Check that there is a histogram open.

            IF (OPEN__HIST) THEN

*
*             Check whether auto-scaling is required.

               IF (.NOT. AUTOSCL) THEN

*
*                The range is not to be auto-scaled.  Attempt to obtain
*                REAL values for the range extents.

                  CSTAT = SAI__OK

                  CLXMIN = CXMIN
                  CALL CHR_LDBLK (CLXMIN)
                  CLXMIN(1 : 1) = ' '
                  CALL CHR_LDBLK (CLXMIN)
                  CALL CHR_CTOR (CLXMIN, XMIN, CSTAT)

                  CLXMAX = CXMAX
                  CALL CHR_LDBLK (CLXMAX)
                  CLXMAX(1 : 1) = ' '
                  CALL CHR_LDBLK (CLXMAX)
                  CALL CHR_CTOR (CLXMAX, XMAX, CSTAT)

*
*                Proceed if values obtained ok.

                  IF (CSTAT .EQ. SAI__OK) THEN

*
*                   Check that a valid X range has been given.

                     IF (XMIN .LT. XMAX) THEN

*
*                      Set the X range.

                        XMIN__HIST = XMIN
                        XMAX__HIST = XMAX

                     ELSE
                        RNGOK = .FALSE.

                     END IF

                  ELSE
                     RNGOK = .FALSE.

                  END IF
               END IF

*
*             If the range is ok then set the scaling flags and
*             histogram bin details.  Otherwise report a warning.
*
*             Note that the bin width and number of bins are variously
*             derived from the 'bin specification' and 'bin details'.

               IF (RNGOK) THEN
                  AUTO__HIST = AUTOSCL
                  AXPL__HIST = .FALSE.
                  NORM__HIST = NORML

                  BINSP__HIST = BINSP

                  IF (BINSP) THEN
                     BINWD__HIST = BINDET
                     NBINS__HIST = 0
                  ELSE
                     BINWD__HIST = 0.0E0
                     NBINS__HIST = NINT(BINDET)
                  END IF

               ELSE
                  CALL MSG_SETC ('CLXMIN', CLXMIN)
                  CALL MSG_SETC ('CLXMAX', CLXMAX)

                  CALL CAP_WARN (GUI__SGZ, ' ', 'Bad X axis range '/
     :              /'(^CLXMIN to ^CLXMAX) will be ignored.', STATUS)
               END IF

            ELSE
               CALL CAP_WARN (GUI__SGZ, ' ', 'There is no plot '/
     :           /'open.', STATUS)

            END IF

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

C        print444, auto__hist, xmin__hist, xmax__hist
C 444    format(1x, 'cap_ghsrn: auto__hist, xmin__hist, xmax__hist: ',
C    :     l5, 0pf10.2, 0pf10.2)


      END IF

      END
