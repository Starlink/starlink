      SUBROUTINE CAP_GSCRN (AUTOSCL, CXMIN, CXMAX, CYMIN, CYMAX, STATUS)
*+
*  Name:
*     CAP_GSCRN
*  Purpose:
*     Set the plotting range.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSCRN (AUTOSCL, CXMIN, CXMAX, CYMIN, CYMAX; STATUS)
*  Description:
*     Set the plotting range.
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
*     CYMIN  =  CHARACTER*(*) (Given)
*        Minimum of the Y axis.
*     CYMAX  =  CHARACTER*(*) (Given)
*        Maximum of the Y axis.
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
*     9/7/98   (ACD): Original version.
*     20/11/98 (ACD): First stable version.
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
     :  AUTOSCL
      CHARACTER
     :  CXMIN*(*),
     :  CXMAX*(*),
     :  CYMIN*(*),
     :  CYMAX*(*)
*  Status:
      INTEGER STATUS   ! Global status.
*  Local Variables:
      LOGICAL
     :  RNGOK       ! Flag; is the range ok?
      CHARACTER
     :  CLXMIN*40,  ! Local copy of CXMIN.
     :  CLXMAX*40,  !   "    "   "  CXMAX.
     :  CLYMIN*40,  !   "    "   "  CYMIN.
     :  CLYMAX*40   !   "    "   "  CYMAX.
      REAL
     :  XMIN,       ! X minimum of range.
     :  XMAX,       ! X maximum "    "  .
     :  YMIN,       ! Y minimum "    "  .
     :  YMAX        ! Y maximum "    "  .
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
*          Check that there is a plot open.

            IF (OPEN__SPLOT) THEN

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

                  CLYMIN = CYMIN
                  CALL CHR_LDBLK (CLYMIN)
                  CLYMIN(1 : 1) = ' '
                  CALL CHR_LDBLK (CLYMIN)
                  CALL CHR_CTOR (CLYMIN, YMIN, CSTAT)

                  CLYMAX = CYMAX
                  CALL CHR_LDBLK (CLYMAX)
                  CLYMAX(1 : 1) = ' '
                  CALL CHR_LDBLK (CLYMAX)
                  CALL CHR_CTOR (CLYMAX, YMAX, CSTAT)

*
*                Proceed if values obtained ok.

                  IF (CSTAT .EQ. SAI__OK) THEN

*
*                   Check that a valid range has been given.

                     IF (XMIN .LT. XMAX  .AND.  YMIN .LT. YMAX) THEN

*
*                      Set the range.

                        XMIN__SPLOT = XMIN
                        XMAX__SPLOT = XMAX
                        YMIN__SPLOT = YMIN
                        YMAX__SPLOT = YMAX

                     ELSE
                        RNGOK = .FALSE.

                     END IF

                  ELSE
                     RNGOK = .FALSE.

                  END IF
               END IF

*
*             If the range is ok then set the scaling flags.  Otherwise
*             report a warning.

               IF (RNGOK) THEN
                  AUTO__SPLOT = AUTOSCL
                  AXPL__SPLOT = .FALSE.

               ELSE
                  CALL MSG_SETC ('CLXMIN', CLXMIN)
                  CALL MSG_SETC ('CLXMAX', CLXMAX)
                  CALL MSG_SETC ('CLYMIN', CLYMIN)
                  CALL MSG_SETC ('CLYMAX', CLYMAX)

                  CALL CAP_WARN (GUI__SGZ, ' ', 'Bad axis range (X '/
     :              /'^CLXMIN to ^CLXMAX, Y ^CLYMIN to ^CLYMAX) '/
     :              /'will be ignored.', STATUS)
               END IF

            ELSE
               CALL CAP_WARN (GUI__SGZ, ' ', 'There is no plot '/
     :           /'open.', STATUS)

            END IF

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

      END IF

      END
