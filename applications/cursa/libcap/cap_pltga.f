      SUBROUTINE CAP_PLTGA (GRPLST, MCENTR, TITLE, GRPLCT, STATUS)
*+
*  Name:
*     CAP_PLTGA
*  Purpose:
*     Plot a graphics attributes list.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PLTGA (GRPLST, MCENTR, TITLE; GRPLCT; STATUS)
*  Description:
*     Plot a graphics attributes list.
*  Arguments:
*     GRPLST  =  CHARACTER*(*) (Given)
*        Name of the graphics attributes list.
*     MCENTR  =  LOGICAL (Given)
*        Flag; is a central cross to be plotted, coded as follows:
*        .TRUE.  - plot central cross,
*        .FALSE. - no central cross.
*     TITLE  =  CHARACTER*(*) (Given)
*        Title for the plot.
*     GRPLCT  =  INTEGER (Given and Returned)
*        Number of graphics attributes lists currently plotted.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to open the graphics attributes list.
*     If ok then
*       If this is the first list then
*         Get the description of the region to be plotted.
*         Draw the border and annotation and set up the window and
*         viewport for plotting the target lists.
*       else (not the first list)
*         Get the equinox and epoch of the list.
*         If the equinox and epoch do not match those of the first
*         plot then
*           Report message.
*         end if
*       end if
*       If all is ok then
*         Set the viewport and window for the chart.
*         Plot the entries in the list.
*         Plot the legend for the list.
*       end if
*       Close the graphics attributes list.
*     else
*       Report error opening the graphics attributes list.
*     end if
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     20/8/96 (ACD): Original version.
*     3/6/97  (ACD): Converted for CURSA.
*     4/6/97  (ACD): First stable version.
*     25/7/00 (ACD): Changed the way the region description is obtained.
*        If it cannot be read from catalogue parameters it is computed
*        from the table of values (rather than being prompted for).
*     20/4/01 (ACD): Added the title for the plot.  Made the checks
*        That the equinox and epoch are both (a) present and (b) the same
*        for all lists result in warnings rather than errors.
*     24/4/01 (ACD): Fixed bug in calls to CAP_OPGTC.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'CHART_PAR'         ! CATCHART constants.
*  Global Variables:
      INCLUDE 'CHART_CMN'         ! CATCHART common block.
*  Arguments Given:
      CHARACTER
     :  GRPLST*(*),
     :  TITLE*(*)
      LOGICAL
     :  MCENTR
*  Arguments Given and Returned:
      INTEGER
     :  GRPLCT
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  GAI        ! Identifier for the graphics attributes list.
      CHARACTER
     :  EPOCH*10,  ! Epoch   of the list.
     :  EQUNX*10,  ! Equinox "   "   "  .
     :  EQ1T*1,    ! Time system for first equinox.
     :  EP1T*1,    ! Time system for first epoch.
     :  EQCT*1,    ! Time system for current equinox.
     :  EPCT*1     ! Time system of current epoch.
      DOUBLE PRECISION
     :  EQ1,       ! First equinox.
     :  EP1,       ! First epoch.
     :  EQC,       ! Current equinox.
     :  EPC        ! Current epoch.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Attempt to open the graphics attributes list and proceed if ok.

         CALL CAT_TOPEN (GRPLST, 'OLD', 'READ', GAI, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            GRPLCT = GRPLCT + 1

*
*          Check whether this is the first list to be plotted.

            IF (GRPLCT .EQ. 1) THEN

*
*             Get the description of the region to be plotted as both
*             sexagesimal values and radians.

               CALL CAP_PLTDT (GAI, RA__CIO, DEC__CIO,
     :           RAC__CIO, DECC__CIO, STATUS)

               CALL CAP_OPGTC (GAI, 'EQUINOX', ' ', EQUNX__CIO, STATUS)
               CALL CAP_OPGTC (GAI, 'EPOCH', ' ', EPOCH__CIO, STATUS)

*
*             Draw the border and annotation and set up the window and
*             viewport for plotting the target lists.

               CALL CAP_PLTST (GAI, MCENTR, TITLE, STATUS)

            ELSE

*
*             Check that the equinox and epoch of the current list
*             match those of the first list (which was used to define
*             the axes).

               IF (EQUNX__CIO .NE. ' ') THEN
                  CALL CAP_OPGTC (GAI, 'EQUINOX', ' ', EQUNX, STATUS)
                  IF (EQUNX .NE. ' ') THEN
                     CALL CAP_DCEQP (EQUNX__CIO, EQ1T, EQ1, STATUS)
                     CALL CAP_DCEQP (EQUNX, EQCT, EQC, STATUS)
                     IF (EQ1 .NE. EQC  .OR.  EQ1T .NE. EQCT) THEN
                        CALL CAP_WARN (.TRUE., ' ', 'Equinox of the '/
     :                    /'current list does not match that of the '/
     :                    /'first list.', STATUS)
                     END IF
                  END IF
               END IF

               IF (EPOCH__CIO .NE. ' ') THEN
                  CALL CAP_OPGTC (GAI, 'EPOCH', ' ', EPOCH, STATUS)
                  IF (EPOCH .NE. ' ') THEN
                     CALL CAP_DCEQP (EPOCH__CIO, EP1T, EP1, STATUS)
                     CALL CAP_DCEQP (EPOCH, EPCT, EPC, STATUS)
                     IF (EP1 .NE. EPC  .OR.  EP1T .NE. EPCT) THEN
                        CALL CAP_WARN (.TRUE., ' ', 'Equinox of the '/
     :                    /'current list does not match that of the '/
     :                    /'first list.', STATUS)
                     END IF
                  END IF
               END IF

            END IF

*
*          Proceed if all is ok.

            IF (STATUS .EQ. SAI__OK) THEN

*
*             Set the viewport and window for the chart.

               CALL PGVPORT (CVXMN__CIO, CVXMX__CIO, CVYMN__CIO,
     :           CVYMX__CIO)
               CALL PGWNAD (CWXMN__CIO, CWXMX__CIO, CWYMN__CIO,
     :           CWYMX__CIO)

*
*             Plot all the entries in the graphics attributes list.

               CALL CAP_PLTGR (GRPLST, GAI, STATUS)

*
*             Plot the legend for the graphics attributes list.

               CALL CAP_PLTLG (GAI, STATUS)
            END IF

*
*          Attempt to close the graphics attributes list.

            CALL CAT_TRLSE (GAI, STATUS)

*
*          Decrement the number of lists if an error ocurred.

            IF (STATUS .NE. SAI__OK) THEN
               GRPLCT = GRPLCT - 1
            END IF

         ELSE

*
*          Report an error opening the graphics attributes list.

            CALL MSG_SETC ('GRPLST', GRPLST)
            CALL ERR_REP ('CAP_PLTGA_OPN', 'Unable to open graphics '/
     :        /'attributes list ^GRPLST.', STATUS)
         END IF

      END IF

      END
