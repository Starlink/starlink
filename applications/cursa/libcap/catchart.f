      SUBROUTINE CATCHART (STATUS)
*+
*  Name:
*     CATCHART
*  Purpose:
*     Plot a one or more target lists as a finding chart.
*  Language:
*     Fortran 77.
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL CATCHART (STATUS)
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Description:
*     Plot a one or more target lists as a finding chart.  The lists
*     are plotted using a tangent plane projection.  If several lists
*     are plotted they are superimposed on a single finding chart.  In
*     this case the coordinates for all the lists must be for the same
*     equinox and epoch.
*  Usage:
*     catchart
*  ADAM Parameters:
*     GRPHDV  =  CHARACTER (read)
*        The name of the graphics device on which the plot will be
*        produced.
*     MCENTRE  =  LOGICAL (read)
*        A flag indicating whether the centre of the plot will be marked
*        with a 'gun sight' cross.  It is coded as follows:
*        .TRUE.  -  mark with a cross,
*        .FALSE. -  do not mark with a cross.
*     MULTIPLE  =  LOGICAL (read)
*        A flag indicating whether more than target list is to be
*        plotted.  It is coded as follows:
*        .TRUE.  -  plot several target lists,
*        .FALSE. -  plot a single target list.
*     GRPLST  =  CHARACTER (read)
*        The name of the target list to be plotted.
*     TITLE  =  CHARACTER (read)
*        Title for the plot.
*     QUIET  =  LOGICAL (read)
*        Operate in quiet mode where warnings are suppressed.  The
*        permitted values are:
*        TRUE  - quiet mode,
*        FALSE - verbose mode.
*  Examples:
*     catchart
*        A graphics device and target list will be prompted for and then
*        the target list will be plotted.  Most of the other parameters
*        will only be prompted for if they cannot be read from the
*        target list.  The centre of the plot will be marked with a
*        'gun sight' cross.
*     catchart  multiple=yes
*        Plot several target lists superimposed as a single finding
*        chart.  You will be prompted in sequence for the required
*        target lists.  When you have entered all the required lists
*        reply QUIT.
*     catchart  mcentre=no
*        A graphics device and target list will be prompted for and then
*        the target list will be plotted without a central cross.
*     catchart  multiple=yes  mcentre=no
*        Plot several target lists superimposed on a single finding
*        chart with no central cross.
*  Algorithm:
*     Obtain and set the quiet mode.
*     Obtain the name of the graphics device.
*     Determine whether a central mark is required.
*     Determine whether multiple lists are to be plotted.
*     Obtain the title.
*     If all is ok then
*       Attempt to open the graphics device.
*       If ok then
*         While (more graphics attributes lists are to be plotted)
*           Get the name of the graphics attributes list.
*           If (not null and not termination flag) then
*             Plot the graphics attributes list.
*           end if
*           If (termination requested)
*             Set the termination flag.
*           end if
*           If only one list is to be plotted then
*             Set the termination flag.
*           end if
*           If any error has occurred then
*             Flush the error.
*             Annul the error.
*           end if
*         end do
*         Close the graphics device.
*         If all is ok then
*           Report the number of lists plotted.
*         end if
*       else
*         Report error opening the graphics device.
*       end if
*     end if
*     Report any error.
*  Implementation Deficiencies:
*     Will not overlay onto a photographic plate.
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     18/8/96 (ACD): Original version.
*     3/6/97  (ACD): Converted for CURSA.
*     4/6/97  (ACD): First stable version.
*     26/7/00 (ACD): Removed documentation of deleted parameters QUERYRA,
*        QUERYDEC and QUERYRAD.
*     5/4/01  (ACD): Added the quiet mode.
*     20/4/01 (ACD): Added option to include a title for the plot.
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER PGBEGIN
*  Local Constants:
      INTEGER PGOK   ! Success status for PGBEGIN.
      PARAMETER (PGOK = 1)
*  Local Variables:
      INTEGER
     :  PGSTAT       ! Status returned from PGBEGIN.
      CHARACTER
     :  GRPHDV*75,   ! Graphics device.
     :  GRPLST*75,   ! Current graphics attributes list.
     :  GRPLSU*75,   ! GRPLST converted to upper case.
     :  TITLE*75     ! Title for the plot.
      LOGICAL
     :  QUIET,       ! Flag; operate in quiet or verbose (normal) mode?
     :  MCENTR,      ! Flag; plot central cross?
     :  MULT,        ! Flag; plot multiple lists?
     :  MORE         ! Flag; more graphics attributes lists to plot?
      INTEGER
     :  GRPLCT       ! Number of graphics attributes lists plotted.
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
*       Obtain the name of the graphics device.

         CALL PAR_GET0C ('GRPHDV', GRPHDV, STATUS)
         CALL PAR_CANCL ('GRPHDV', STATUS)

*
*       Determine whether a central mark is required.

         CALL PAR_GET0L ('MCENTRE', MCENTR, STATUS)
         CALL PAR_CANCL ('MCENTRE', STATUS)

*
*       Determine whether multiple lists are to be plotted.

         CALL PAR_GET0L ('MULTIPLE', MULT, STATUS)
         CALL PAR_CANCL ('MULTIPLE', STATUS)

*
*       Obtain the title.

         CALL PAR_GET0C ('TITLE', TITLE, STATUS)
         CALL PAR_CANCL ('TITLE', STATUS)

*
*       Proceed if all is ok.

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Attempt to open the graphics device and proceed if ok.

            PGSTAT = PGBEGIN (0, GRPHDV, 1, 1)
            IF (PGSTAT .EQ. PGOK) THEN

*
*             Plot the graphics attributes lists.

               MORE = .TRUE.
               GRPLCT = 0

               DO WHILE (MORE)

*
*                Get the name of the graphics attributes list.

                  CALL PAR_GET0C ('GRPLST', GRPLST, STATUS)
                  CALL PAR_CANCL ('GRPLST', STATUS)

                  GRPLSU = GRPLST
                  CALL CHR_UCASE (GRPLSU)

                  IF (STATUS .EQ. SAI__OK   .AND.
     :                GRPLSU  .NE.  'END'   .AND.
     :                GRPLSU  .NE.  'STOP'  .AND.
     :                GRPLSU  .NE.  'QUIT'  .AND.
     :                GRPLSU  .NE.  'EXIT')  THEN

*
*                   Plot the current graphics attributes list.

                     CALL CAP_PLTGA (GRPLST, MCENTR, TITLE, GRPLCT,
     :                 STATUS)

                  ELSE
                     MORE = .FALSE.

                  END IF

*
*                Set the termination flag if only one list is to be
*                plotted.

                  IF (.NOT. MULT) THEN
                     MORE = .FALSE.
                  END IF

*
*                If any error has occurred then flush the error and annul
*                the status.

                  IF (STATUS .NE. SAI__OK) THEN
                     CALL ERR_FLUSH (STATUS)
                     CALL ERR_ANNUL (STATUS)
                  END IF
               END DO

*
*             Close the graphics device.

               CALL PGEND

*
*             If all is ok then report the number of lists plotted.

               IF (STATUS .EQ. SAI__OK) THEN
                  IF (GRPLCT .EQ. 1) THEN
                     CALL MSG_OUT (' ', 'One target list plotted '/
     :                 /'successfully.', STATUS)
                  ELSE
                     CALL MSG_SETI ('GRPLCT', GRPLCT)
                     CALL MSG_OUT (' ', '^GRPLCT target lists plotted '/
     :                 /'successfully.', STATUS)
                  END IF
               END IF

            ELSE

*
*             Report error opening the graphics device.

               STATUS = SAI__ERROR

               CALL MSG_SETC ('GRPHDV', GRPHDV)
               CALL MSG_SETI ('PGSTAT', PGSTAT)

               CALL ERR_REP ('CATCHART_OPG', 'PGPLOT error opening '/
     :           /'graphics device  ^GRPHDV (code: ^PGSTAT).', STATUS)

            END IF
         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CATCHART_ERR', 'Error plotting '/
     :        /'finding chart.', STATUS)
         END IF

      END IF

      END
