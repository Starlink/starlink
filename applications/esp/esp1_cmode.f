

      SUBROUTINE ELF1_CMODE(STATUS)
*+
*  Name:
*     ELF1_CMODE

*  Purpose:
*     The routine allows the user to input most the information required
*     (position, background value etc) for the galaxy profile to be
*     performed/displayed  and then calls other routines as necessary.
*
*     The routine operates using a combination of keyboard and cursor
*     inputs and examines the latest DATA image in the AGI database
*     for the device specified.
*
*     Information such as pixel size, background count value and its
*     standard deviation are also input.
*

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_CMODE(STATUS)

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*
*     Profiling is continued until the mean profile value is less than
*     a user defined number of standard deviations above sky (LIM2) or
*     until the mean profile value increases by a user defined amount
*     compared to the previous profile (LIM1). The difference between the
*     sizes of profile semi-major axis values is controlled using the
*     FINE parameter.
*
*     The initial estimate of the galaxy position may be improved by
*     selecting the AUTOL option which performs a weighted maximum
*     analysis of the image area immediately surrounding the input value.
*
*     Contaminating parts of the image may be defined using an ARD file.

*  Implementation Status:
*     Under development

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-Mar-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'ELF_PAR'               ! ELLFOU constants
      INCLUDE 'MSG_PAR'               ! MSG constants
      INCLUDE 'SUBPAR_PAR'            ! SUBPAR constants
      INCLUDE 'PAR_ERR'		      ! PAR constants
      INCLUDE 'CNF_PAR'

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      CHARACTER *(MSG__SZMSG) FILE    ! NDF file name
      LOGICAL AGAIN                   ! Determine another profile?
      LOGICAL ANGCON                  ! Position angle convention
      LOGICAL AUTOL                   ! Is an estimate of the galaxy centre
                                      ! position to be made?
      LOGICAL EXCLAIM                 ! Was the filename an exclaimation
      LOGICAL FRZORI                  ! Is the galaxy origin frozen?
      LOGICAL GRAPH                   ! Is a graph to be plotted
      LOGICAL INOKAY                  ! Was the most recent input value okay
      LOGICAL SAME                    ! Use the display device the image is
                                      ! on to show the result graphs?
      LOGICAL WRITECAT                ! Output a catalogue?
      INTEGER COLOUR                  ! Pen colour used to show galaxy centre
      INTEGER AGIID                   ! AGI identifier
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER FIOD                    ! FIO file descriptor
      INTEGER FIRST                   ! First time the NDF identifier has been
                                      ! determined
      INTEGER I                       ! Loop variable
      INTEGER LBND(NDF__MXDIM)        ! Lower limit for image index
      INTEGER NDF1                    ! Identifier for the source NDF
      INTEGER NDIM                    ! Number of dimensions in the
                                      ! image
      INTEGER POINT0(1)               ! Pointer to the data component of
                                      ! the source NDF
      INTEGER POINT1(1)               ! Pointer to the data component of
                                      ! for the output NDF
      INTEGER POINT3(1)               ! Pointer to ARD mask
      INTEGER PRANGE(2)               ! Length of the x and y axes
      INTEGER STLEN                   ! File name length
      INTEGER UBND(NDF__MXDIM)        ! Upper limit for image index
      INTEGER VALIDP                  ! Number of radii for which a
                                      ! was found
      REAL ANGOFF                     ! Position angle offset
      REAL BACK                       ! Background count value
      REAL FINE                       ! Determines how closely spaced the
                                      ! chosen radii values are
      REAL LIM1                       ! Maximum permitted count increase factor
      REAL LIM2                       ! Lower limit on ellipse count
      REAL PSIZE                      ! Size of the image pixels in arc sec
      REAL RESULT(17,ELF__RESUL)      ! Ellipse parameters
      REAL RLIM                       ! Sampling radius maximum
      REAL SIGMA                      ! Standard deviation of the background value
      REAL X(10)                      ! Indices of the co-ordinates input
      REAL XCO                        ! X index of the galaxy origin
      REAL Y(10)                      ! Indices of the co-ordinates input
      REAL YCO                        ! Y index of the galaxy origin
      REAL ZEROP                      ! Zero point of the surface
                                      ! brightness graphs
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Get a different COLOUR value if one is to be found on the command line.
      CALL PAR_STATE('COLOUR',I,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0I('COLOUR',COLOUR,STATUS)
         CALL MSG_OUT(' ','Command line COLOUR value used.',STATUS)
      ELSE
         COLOUR=1
      END IF

*   Look on the command line for an ANGCON input.
*   Otherwise, use the default value.
      CALL PAR_STATE('ANGCON',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0L('ANGCON',ANGCON,STATUS)
         CALL MSG_OUT(' ','Command line ANGCON value used.',STATUS)
      ELSE
         ANGCON=.TRUE.
      END IF

*   Look on the command line for an ANGOFF input.
*   Otherwise, use the default value.
      CALL PAR_STATE('ANGOFF',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('ANGOFF',ANGOFF,STATUS)
         CALL MSG_OUT(' ','Command line ANGOFF value used.',STATUS)
      ELSE
         ANGOFF=0.0
      END IF

*   Are we to write a catalogue output file?
      CALL PAR_STATE ('OUTCAT',I,STATUS)
      IF (I .EQ. SUBPAR__ACTIVE) THEN
*      Parameter was present on the command line
         WRITECAT = .TRUE.
      ELSE
         WRITECAT = .FALSE.
      ENDIF

*   Begin an NDF context.
      CALL NDF_BEGIN
      IF (STATUS.NE.SAI__OK) GOTO 9999

*     Loop around looking at different parts for the same image.
      AGAIN=.TRUE.
      FIRST=0
      DO WHILE ((AGAIN).AND.(STATUS.EQ.SAI__OK))

*      Get the cursor position for the galaxy origin. At the same time
*      obtain the NDF identifier for the most recent 'DATA' picture.
         INOKAY=.FALSE.
         DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))

*         Get the value from the image. Taking care to ensure that
*         the NDF is only obtained by ELF1_CURSO only the first time
*         a co-ordinate is provided.
            CALL ELF1_CURSO('IMGDEV',FIRST,0,COLOUR,NDF1,X,Y,
     :                      RLIM,STATUS)
            XCO=X(10)
            YCO=Y(10)

*         Get the image bounds and also the size of the axes in pixels.
            CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
            PRANGE(1)=UBND(1)-LBND(1)+1
            PRANGE(2)=UBND(2)-LBND(2)+1

*         Check that the co-ordinate values input are legal and
*         annul the parameter if not.
            IF ((XCO.LT.1.0).OR.(XCO.GT.PRANGE(1)).OR.(YCO.LT.0.0).
     :           OR.(YCO.GT.PRANGE(2))) THEN
               CALL MSG_OUT(' ','The position supplied, is not '//
     :                      'within the image.',STATUS)
               CALL PAR_CANCL('ORIGIN',STATUS)
            ELSE
               INOKAY=.TRUE.
            END IF
            FIRST=1

         END DO
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the sampling radius value from the image. Taking care to ensure
*      that the NDF is only obtained by ELF1_CURSO only the first time
*      a co-ordinate is provided.
         CALL ELF1_CURSO('IMGDEV',2,0,COLOUR,NDF1,X,Y,
     :                   RLIM,STATUS)

*      Calculate maximum permitted radius for the ellipse.
         RLIM=SQRT((X(1)-X(2))*(X(1)-X(2))+(Y(1)-Y(2))*(Y(1)-Y(2)))
         IF (RLIM.LT.5.) RLIM=ELF__RLIM

*      Display the circle showing the radius limit.
         CALL ELF1_CURSO('IMGDEV',3,0,COLOUR,NDF1,X,Y,
     :                    RLIM,STATUS)
         CALL MSG_BLANK(STATUS)

*      Determine whether or not the origin given is to be used throughout the
*      profiling.
         CALL PAR_GET0L('FRZORI',FRZORI,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the background count value.
         CALL PAR_GET0R('BACK',BACK,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the background count standard deviation value.
         INOKAY=.FALSE.
         DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))
            CALL PAR_GET0R('SIGMA',SIGMA,STATUS)
            IF (SIGMA.LE.0.0) THEN
*            Display message and annul the parameter.
               CALL MSG_OUT(' ','Sigma supplied, is not '//
     :                      'feasible.',STATUS)
               CALL PAR_CANCL('SIGMA',STATUS)
            ELSE
               INOKAY=.TRUE.
            END IF
         END DO
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Is the default ellipse isophotal level separation to be used?
*      Check the command line for an input.
*      Otherwise, use the value specified in elf_par.
         CALL PAR_STATE('FINE',I,STATUS )
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
            CALL PAR_GET0R('FINE',FINE,STATUS)
            CALL MSG_OUT(' ','Command line FINE value used.',STATUS)
         ELSE
            FINE=ELF__FINE
         END IF

*      Get the pixel size value.
         CALL ESP1_GTPSZ(NDF1,PSIZE,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the zero point for the surface brightness scale/graphs.
         CALL PAR_GET0R('ZEROP',ZEROP,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Is the galaxy centre to be determined by a weighted search around
*      the coords provided?
         CALL PAR_GET0L('AUTOL',AUTOL,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Check the state of the parameter LIM1, to see if there is a
*      suggested value on the command line.
*      Otherwise, use the value specified in elf_par.
         CALL PAR_STATE('LIM1',I, STATUS )
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
            CALL PAR_GET0R('LIM1',LIM1,STATUS)
            CALL MSG_OUT(' ','Command line LIM1 value used.',STATUS)
         ELSE
            LIM1=ELF__LIM1
         END IF

*      Check the state of the parameter LIM2, to see if there is a
*      suggested value on the command line.
*      Otherwise, use the value specified in elf_par.
         CALL PAR_STATE('LIM2',I,STATUS )
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
            CALL PAR_GET0R('LIM2',LIM2,STATUS)
            CALL MSG_OUT(' ','Command line LIM2 value used.',STATUS)
         ELSE
            LIM2=ELF__LIM2
         END IF

*      Map the input NDF data array as _REAL values for reading.
         CALL NDF_MAP(NDF1,'DATA','_REAL','READ',POINT0(1),ELEMS,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the name of the file being mapped.
         CALL NDF_MSG('FILE',NDF1)
         CALL MSG_LOAD(' ','^FILE',FILE,STLEN,STATUS)

*      Allocate dynamic memory on which to map the NDF.
         CALL PSX_CALLOC(ELEMS,'_REAL',POINT1(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Transfer values from the mapped NDF to the allocated memory.
         CALL ELF1_TRANS(ELEMS,%VAL(CNF_PVAL(POINT0(1))),
     :                   %VAL(CNF_PVAL(POINT1(1))),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Un-map the source NDF. Helps to reduce the resources being used.
         CALL NDF_UNMAP(NDF1,'DATA',STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Allocate the memory needed for the logical mask array.
         CALL PSX_CALLOC(ELEMS,'_INTEGER',POINT3(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Transfer to the ARD driver control routine.
         NDIM=2
         CALL ESP1_ARD_DRIVER(NDIM,ELEMS,LBND,UBND,POINT1,POINT3,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Free the dynamic array space of the logical mask.
         CALL PSX_FREE(POINT3(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Look for a better (though crude) estimate of the galaxy core position.
         IF (AUTOL) CALL ELF1_AUTOL(ELEMS,PRANGE,BACK,
     :                   %VAL(CNF_PVAL(POINT1(1))),XCO,YCO,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Call the routine that profiles the galaxy and sets up the values
*      in the results arrays.
         CALL ELF1_PRO(1,ANGCON,ANGOFF,FRZORI,FINE,LIM2,
     :             PSIZE,RLIM,BACK,SIGMA,ELEMS,POINT1,PRANGE,
     :             XCO,YCO,VALIDP,RESULT,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Only bother with the graphs if there is more than one data point.
         IF (VALIDP.GT.1) THEN

*         Get the user selection of using the same display as the image
*         to display the results.
            CALL PAR_GET0L('SAME',SAME,STATUS)

*         Display the results graphs on the device used to show the image.
            IF (SAME) THEN

*            Find limits for the window to be used to display the graph.
               CALL ELF1_CURSO('IMGDEV',6,0,COLOUR,NDF1,X,Y,
     :                         RLIM,STATUS)

*            Set up the new window.
               CALL ELF1_CURSO('IMGDEV',7,0,COLOUR,NDF1,X,Y,
     :                         RLIM,STATUS)

*            Set up the AGI/PGPLOT interface.
               CALL ELF1_AGICO(0,1,0,AGIID,STATUS)

*            Display the un-analysed data as a graphical plot of radius
*            (in pixels) versus intensity.
               CALL ELF1_GRAPH(PSIZE,ZEROP,RESULT,VALIDP,
     :                         STATUS)
               IF (STATUS.NE.SAI__OK) GOTO 9999

*            Turn off the AGI/PGPLOT interface.
               CALL ELF1_AGICO(1,1,0,AGIID,STATUS)

            END IF

         END IF

*      Display the results graphs on a device specified by the user.
         IF (.NOT.SAME) THEN

*      Only display the graphs if there is more than 1 data point.
             IF (VALIDP.GT.1) THEN

*            Display the graph of the data points i.e. radius versus brightness.
*            Determine if graphical histogram output is required. Set the
*            value for GRAPH accordingly.
               AGIID=0
               GRAPH=.TRUE.
               CALL ERR_MARK
               CALL ELF1_AGICO(0,0,0,AGIID,STATUS)
               IF (STATUS.NE.SAI__OK) THEN
                  GRAPH=.FALSE.
                  CALL ERR_ANNUL(STATUS)
               END IF
               CALL ERR_RLSE

*            Display the un-analysed data as a graphical plot of radius
*            (in pixels) versus intensity.
               IF (GRAPH) THEN

                  CALL ELF1_GRAPH(PSIZE,ZEROP,RESULT,VALIDP,
     :                            STATUS)
                  IF (STATUS.NE.SAI__OK) GOTO 9999

*               Turn off the AGI/PGPLOT interface.
                  CALL ELF1_AGICO(1,0,0,AGIID,STATUS)

               END IF

            END IF

         END IF

*      Output a text file containing results if required.
         CALL ELP1_TEXTO(0,NDF1,VALIDP,ZEROP,
     :        RESULT,17,ELF__MXPOI,XCO,YCO,BACK,
     :        SIGMA,PSIZE,LBND,.FALSE.,FIOD,EXCLAIM,STATUS)

         CALL ERR_MARK
         IF (WRITECAT) THEN
            CALL ESP1_CATO(0, NDF1,VALIDP,ZEROP,
     :           RESULT,17,ELF__MXPOI,XCO,YCO,BACK,
     :           SIGMA,PSIZE,LBND,.FALSE.,STATUS)
            IF (STATUS .EQ. PAR__NULL) THEN
               CALL ERR_ANNUL (STATUS)
            ENDIF
         ENDIF
         CALL ERR_RLSE

*      An appropriate place to exit to if the dynamic memory has already
*      been allocated.
 9998    CONTINUE

*      De-allocate the dynamic memory used.
         CALL PSX_FREE(POINT1(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the user selection of preparing another galaxy profile
*      or not.
         CALL PAR_GET0L('AGAIN',AGAIN,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Cancel the parameters so that they must be reinput when
*      looping round.
         IF (AGAIN) CALL ELF1_CANCL(0,STATUS)

      END DO

      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

*   End the NDF context.
      CALL NDF_END(STATUS)

      END



      SUBROUTINE ELP1_CMODE(STATUS)
*+
*  Name:
*     ELP1_CMODE

*  Purpose:
*     The routine allows the user to input most the information required
*     (position, background value etc) for the galaxy profile to be
*     performed/displayed  and then calls other routines as necessary.
*
*     The routine operates using a combination of keyboard and cursor
*     inputs and examines the latest DATA image in the AGI database
*     for the device specified.
*
*     Information such as pixel size, background count value and its
*     std. dev. are also input.
*

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELP1_CMODE(STATUS)

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*
*     Profiling is continued until the mean profile value is less than
*     a user defined number of standard deviations above sky (LIM2) or
*     until the mean profile value increases by a user defined amount
*     compared to the previous profile (LIM1). The difference between the
*     sizes of profile semi-major axis values is controlled using the
*     FINE parameter.
*
*     When profiling an ellipse with a small radius one of two methods may
*     be employed to determine the values of points located around each
*     trial ellipse. These are are bi-linear interpolation or a surface
*     interpolation based on a 8x8 grid of surrounding pixels. The latter
*     method is slower but may yield better results at small radii. It
*     is selected by setting the parameter FAST to false.
*
*     The initial estimate of the galaxy position may be improved by
*     selecting the AUTOL option which performs a centroid
*     analysis of the image area immediately surrounding the input value.
*
*     Contaminating parts of the image may be defined using an ARD file.

*  Implementation Status:
*     Under development

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-Mar-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'ELP_PAR'               ! ELLPRO constants
      INCLUDE 'MSG_PAR'               ! MSG constants
      INCLUDE 'SUBPAR_PAR'            ! SUBPAR constants
      INCLUDE 'PAR_ERR'               ! PAR constants
      INCLUDE 'CNF_PAR'


*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      CHARACTER *(MSG__SZMSG) FILE    ! NDF file name
      LOGICAL ANGCON                  ! Position angle convention
      LOGICAL AGAIN                   ! Determine another profile?
      LOGICAL AUTOL                   ! Is an estimate of the galaxy centre
                                      ! position to be made?
      LOGICAL EXCLAIM                 ! Was an exclaimation mark given for
                                      ! an output file name?
      LOGICAL FAST                    ! Use fast method of profiling?
      LOGICAL FRZORI                  ! Is the galaxy origin frozen?
      LOGICAL GRAPH                   ! Is a graph to be plotted
      LOGICAL INOKAY                  ! Was the most recent input value okay
      LOGICAL SAME                    ! Use the display device the image is
                                      ! on to show the result graphs?
      LOGICAL WRITECAT                ! Output a catalogue?
      INTEGER AGIID                   ! AGI identifier
      INTEGER COLOUR                  ! Pen colour used for galaxy marker
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER FIOD                    ! FIO file descriptor
      INTEGER FIRST                   ! First time the NDF identifier has been
                                      ! determined
      INTEGER I                       ! Loop variable
      INTEGER LBND(NDF__MXDIM)        ! Lower limit for image index
      INTEGER MINMOD                  ! Which residual to use in minimisation
      INTEGER NDF1                    ! Identifier for the source NDF
      INTEGER NDIM                    ! Number of dimensions in the
                                      ! image
      INTEGER POINT0(1)               ! Pointer to the data component of
                                      ! the source NDF
      INTEGER POINT1(1)               ! Pointer to the data component of
                                      ! for the output NDF
      INTEGER POINT3(1)               ! Pointer to the ARD mask
      INTEGER PRANGE(2)               ! Length of the x and y axes
      INTEGER STLEN                   ! File name length
      INTEGER UBND(NDF__MXDIM)        ! Upper limit for image index
      INTEGER VALIDP                  ! Number of radii for which a
                                      ! was found
      REAL ANGOFF                     ! Position angle offset
      REAL BACK                       ! Background count value
      REAL FINE                       ! Determines how closely spaced the
                                      ! chosen radii values are
      REAL FRACT                      ! Fraction of the ellipse points that
                                      ! must be available for the profile
                                      ! at a given radius to be kept
      REAL LIM1                       ! Maximum permitted count increase factor
      REAL LIM2                       ! Lower limit on ellipse count
      REAL LIM3                       ! Radius at which the position angle,
                                      ! ellipticity and origin are
                                      ! no longer adjusted
      REAL PSIZE                      ! Size of the image pixels in arc sec
      REAL RESULT(ELP__NRES,ELP__RESUL)      ! Ellipse parameters
      REAL RLIM                       ! Ellipse maximum size
      REAL SIGMA                      ! Std. dev. of the background value
      REAL X(10)                      ! Indices of the co-ordinates input
      REAL XCO                        ! X index of the galaxy origin
      REAL Y(10)                      ! Indices of the co-ordinates input
      REAL YCO                        ! Y index of the galaxy origin
      REAL ZEROP                      ! Zero point of the surface
                                      ! brightness graphs
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the pen colour required.
      CALL PAR_STATE('COLOUR',I, STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0I('COLOUR',COLOUR,STATUS)
         CALL MSG_OUT(' ','Command line COLOUR value used.',STATUS)
      ELSE
         COLOUR=1
      END IF

*   Look at the command line value for ANGCON.
*   Otherwise, use the default.
      CALL PAR_STATE('ANGCON',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0L('ANGCON',ANGCON,STATUS)
         CALL MSG_OUT(' ','Command line ANGCON value used.',STATUS)
      ELSE
         ANGCON=.TRUE.
      END IF

*   Look at the command line value for ANGOFF.
*   Otherwise, use the default.
      CALL PAR_STATE('ANGOFF',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('ANGOFF',ANGOFF,STATUS)
         CALL MSG_OUT(' ','Command line ANGOFF value used.',STATUS)
      ELSE
         ANGOFF=0.0
      END IF

*   Get the minimisation mode (which residual to use)
C      CALL PAR_STATE('MINMOD',I, STATUS)
C      IF (STATUS.NE.SAI__OK) GOTO 9999
C      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
C         CALL PAR_GET0I('MINMOD',MINMOD,STATUS)
C         CALL MSG_OUT(' ','Command line MINMOD value used.',STATUS)
C      ELSE
C         MINMOD=0
C      END IF
      CALL PAR_GET0I('MINMOD',MINMOD,STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 9999

*   Are we to write a catalogue output file?
      CALL PAR_STATE ('OUTCAT',I,STATUS)
      IF (I .EQ. SUBPAR__ACTIVE) THEN
*      Parameter was present on the command line
         WRITECAT = .TRUE.
      ELSE
         WRITECAT = .FALSE.
      ENDIF



*   Begin an NDF context.
      CALL NDF_BEGIN
      IF (STATUS.NE.SAI__OK) GOTO 9999

*     Loop around looking at different parts for the same image.
      AGAIN=.TRUE.
      FIRST=0
      DO WHILE ((AGAIN).AND.(STATUS.EQ.SAI__OK))

*      Get the cursor position for the galaxy origin. At the same time
*      obtain the NDF identifier for the most recent 'DATA' picture.
         INOKAY=.FALSE.
         DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))

*         Get the value from the image. Taking care to ensure that
*         the NDF is only obtained by ELP1_CURSO only the first time
*         a co-ordinate is provided.
            CALL ELP1_CURSO('IMGDEV',FIRST,0,COLOUR,NDF1,X,Y,
     :                      RLIM,STATUS)
            XCO=X(10)
            YCO=Y(10)

*         Get the image bounds and also the size of the axes in pixels.
            CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
            PRANGE(1)=UBND(1)-LBND(1)+1
            PRANGE(2)=UBND(2)-LBND(2)+1

*         Check that the co-ordinate values input are legal and
*         annul the parameter if not.
            IF ((XCO.LT.1.0).OR.(XCO.GT.PRANGE(1)).OR.(YCO.LT.0.0).
     :           OR.(YCO.GT.PRANGE(2))) THEN
               CALL MSG_OUT(' ','The position supplied, is not '//
     :                      'within the image.',STATUS)
               CALL PAR_CANCL('ORIGIN',STATUS)
            ELSE
               INOKAY=.TRUE.
            END IF
            FIRST=1

         END DO
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the radius value from the image. Taking care to ensure that
*      the NDF is only obtained by ELP1_CURSO only the first time
*      a co-ordinate is provided.
         CALL ELP1_CURSO('IMGDEV',2,0,COLOUR,NDF1,X,Y,RLIM,STATUS)

*      Calculate maximum permitted radius for the ellipse.
         RLIM=SQRT((X(1)-X(2))*(X(1)-X(2))+(Y(1)-Y(2))*(Y(1)-Y(2)))
         IF (RLIM.LT.5.) RLIM=ELP__RLIM

*      Display the circle showing the radius limit.
         CALL ELP1_CURSO('IMGDEV',3,0,COLOUR,NDF1,X,Y,RLIM,STATUS)
         CALL MSG_BLANK(STATUS)

*      Determine whether or not the origin given is to be used throughout
*      the profiling.
         CALL PAR_GET0L('FRZORI',FRZORI,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the background count value.
         CALL PAR_GET0R('BACK',BACK,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the background count standard deviation value.
         INOKAY=.FALSE.
         DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))
            CALL PAR_GET0R('SIGMA',SIGMA,STATUS)
            IF (SIGMA.LE.0.0) THEN
*            Display message and annul the parameter.
               CALL MSG_OUT(' ','Sigma supplied, is not '//
     :                      'feasible.',STATUS)
               CALL PAR_CANCL('SIGMA',STATUS)
            ELSE
               INOKAY=.TRUE.
            END IF
         END DO
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Is the default radii separation to be used?
*      Check the command line for an input.
*      Otherwise, use the value specified in elp_par.
         CALL PAR_STATE('FINE',I,STATUS )
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
            CALL PAR_GET0R('FINE',FINE,STATUS)
            CALL MSG_OUT(' ','Command line FINE value used.',STATUS)
         ELSE
            FINE=ELP__FINE
         END IF

*      Get the pixel size value.
         CALL ESP1_GTPSZ(NDF1,PSIZE,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the zero point for the surface brightness scale/graphs.
         CALL PAR_GET0R('ZEROP',ZEROP,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Is the galaxy centre to be determined by a centroid around
*      the coords provided?
         CALL PAR_GET0L('AUTOL',AUTOL,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Calculate the profiles without using the bi-cubic spline at
*      low radii? Check the command line for the an input.
*      Otherwise, use the value specified in elp_par.
         CALL PAR_STATE('FAST',I,STATUS )
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
            CALL PAR_GET0L('FAST',FAST,STATUS)
            CALL MSG_OUT(' ','Command line FAST value used.',STATUS)
         ELSE
            FAST=ELP__FAST
         END IF

*      Check the state of the parameter LIM1, to see if there is a
*      suggested value on the command line.
*      Otherwise, use the value specified in elp_par.
         CALL PAR_STATE('LIM1',I, STATUS )
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
            CALL PAR_GET0R('LIM1',LIM1,STATUS)
            CALL MSG_OUT(' ','Command line LIM1 value used.',STATUS)
         ELSE
            LIM1=ELP__LIM1
         END IF

*      Check the state of the parameter LIM2, to see if there is a
*      suggested value on the command line.
*      Otherwise, use the value specified in elp_par.
         CALL PAR_STATE('LIM2',I,STATUS )
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
            CALL PAR_GET0R('LIM2',LIM2,STATUS)
            CALL MSG_OUT(' ','Command line LIM2 value used.',STATUS)
         ELSE
            LIM2=ELP__LIM2
         END IF

*      Check the state of the parameter LIM3, to see if there is a
*      suggested value on the command line.
*      Otherwise, use the value specified in elp_par.
         CALL PAR_STATE('LIM3',I,STATUS )
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
            CALL PAR_GET0R('LIM3',LIM3,STATUS)
            CALL MSG_OUT(' ','Command line LIM3 value used.',STATUS)
         ELSE
            LIM3=ELP__LIM3
         END IF

*      Check the state of the parameter FRACT, to see if there is a
*      suggested value on the command line.
*      Otherwise, use the value specified in elp_par.
         CALL PAR_STATE('FRACT',I,STATUS )
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
            CALL PAR_GET0R('FRACT',FRACT,STATUS)
            CALL MSG_OUT(' ','Command line FRACT value used.',STATUS)
         ELSE
            FRACT=ELP__FRACT
         END IF

*      Map the input NDF data array as _REAL values for reading.
         CALL NDF_MAP(NDF1,'DATA','_REAL','READ',POINT0(1),ELEMS,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the name of the file being mapped.
         CALL NDF_MSG('FILE',NDF1)
         CALL MSG_LOAD(' ','^FILE',FILE,STLEN,STATUS)

*      Allocate dynamic memory on which to map the NDF.
         CALL PSX_CALLOC(ELEMS,'_REAL',POINT1(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Transfer values from the mapped NDF to the allocated memory.
         CALL ELP1_TRANS(ELEMS,%VAL(CNF_PVAL(POINT0(1))),
     :                   %VAL(CNF_PVAL(POINT1(1))),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Un-map the source NDF. Helps to reduce the resources being used.
         CALL NDF_UNMAP(NDF1,'DATA',STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Allocate the memory needed for the logical mask array.
         CALL PSX_CALLOC(ELEMS,'_INTEGER',POINT3(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Transfer to the ARD driver control routine.
         NDIM=2
         CALL ESP1_ARD_DRIVER(NDIM,ELEMS,LBND,UBND,POINT1,POINT3,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Free the dynamic array space of the logical mask.
         CALL PSX_FREE(POINT3(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Look for a better (though crude) estimate of the galaxy core position.
         IF (AUTOL) CALL ELP1_AUTOL(BACK,ELEMS,PRANGE,
     :                   %VAL(CNF_PVAL(POINT1(1))),XCO,YCO,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Call the routine that profiles the galaxy and sets up the values
*      in the results arrays.
         CALL ELP1_PRO(1,MINMOD,ANGCON,ANGOFF,FRZORI,FINE,
     :             LIM1,LIM2,LIM3,
     :             FRACT,PSIZE,FAST,RLIM,BACK,SIGMA,ELEMS,POINT1,
     :             PRANGE,XCO,YCO,VALIDP,RESULT,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Only bother with the graphs if there is more than one data point.
         IF (VALIDP.GT.1) THEN

*         Get the user selection of using the same display as the image
*         to display the results.
            CALL PAR_GET0L('SAME',SAME,STATUS)

*         Display the results graphs on the device used to show the image.
            IF (SAME) THEN

*            Find limits for the window to be used to display the graph.
               CALL ELP1_CURSO('IMGDEV',6,0,COLOUR,NDF1,X,Y,
     :                         RLIM,STATUS)

*            Set up the new window.
               CALL ELP1_CURSO('IMGDEV',7,0,COLOUR,NDF1,X,Y,
     :                         RLIM,STATUS)

*            Set up the AGI/PGPLOT interface.
               CALL ELP1_AGICO(0,1,0,AGIID,STATUS)

*            Display the un-analysed data as a graphical plot of radius
*            (in pixels) versus intensity.
               CALL ELP1_GRAPH(PSIZE,ZEROP,RESULT,VALIDP,
     :                         STATUS)
               IF (STATUS.NE.SAI__OK) GOTO 9999

*            Turn off the AGI/PGPLOT interface.
               CALL ELP1_AGICO(1,1,0,AGIID,STATUS)

            END IF

         END IF

*      Display the results graphs on a device specified by the user.
         IF (.NOT.SAME) THEN

*      Only display the graphs if there is more than 1 data point.
             IF (VALIDP.GT.1) THEN

*            Display the graph of the data points i.e. radius versus brightness.
*            Determine if graphical histogram output is required. Set the
*            value for GRAPH accordingly.
               AGIID=0
               GRAPH=.TRUE.
               CALL ERR_MARK
               CALL ELP1_AGICO(0,0,0,AGIID,STATUS)
               IF (STATUS.NE.SAI__OK) THEN
                  GRAPH=.FALSE.
                  CALL ERR_ANNUL(STATUS)
               END IF
               CALL ERR_RLSE

*            Display the un-analysed data as a graphical plot of radius
*            (in pixels) versus intensity.
               IF (GRAPH) THEN

                  CALL ELP1_GRAPH(PSIZE,ZEROP,RESULT,VALIDP,
     :                            STATUS)
                  IF (STATUS.NE.SAI__OK) GOTO 9999

*               Turn off the AGI/PGPLOT interface.
                  CALL ELP1_AGICO(1,0,0,AGIID,STATUS)

               END IF

            END IF

         END IF

*      Output a text file containing results if required.  This is done
*      only once, rather than once for each source as in elp1_fmode.
         CALL ELP1_TEXTO(0,NDF1,VALIDP,ZEROP,
     :        RESULT,ELP__NRES,ELP__MXPOI,XCO,YCO,BACK,
     :        SIGMA,PSIZE,LBND,.TRUE.,FIOD,EXCLAIM,STATUS)

         CALL ERR_MARK
         IF (WRITECAT) THEN
            CALL ESP1_CATO(0, NDF1,VALIDP,ZEROP,
     :           RESULT,ELP__NRES,ELP__MXPOI,XCO,YCO,BACK,
     :           SIGMA,PSIZE,LBND,.TRUE.,STATUS)
            IF (STATUS .EQ. PAR__NULL) THEN
               CALL ERR_ANNUL (STATUS)
            ENDIF
         ENDIF
         CALL ERR_RLSE

*      An appropriate place to exit to if the dynamic memory has already
*      been allocated.
 9998    CONTINUE

*      De-allocate the dynamic memory used.
         CALL PSX_FREE(POINT1(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the user selection of preparing another galaxy profile
*      or not.
         CALL PAR_GET0L('AGAIN',AGAIN,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Cancel the parameters so that they must be reinput when
*      looping round.
         IF (AGAIN) CALL ELP1_CANCL(0,STATUS)

      END DO

      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

*   End the NDF context.
      CALL NDF_END(STATUS)

      END


      SUBROUTINE GAU1_CMODE(STATUS)
*+
*  Name:
*     GAU1_CMODE

*  Purpose:
*     The routine allows the user to select the sources
*     by using a cursor to indicate their position and extent via
*     on image already displayed.
*
*     The routine operates using a combination of keyboard and cursor
*     inputs and examines the latest DATA image in the AGI database
*     for the device specified.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_CMODE(STATUS)

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Accesses the AGI database to find the name of the image
*     most recently displayed on the image device. The image
*     file is read to determine the size of the image. The
*     routines are then employed to deduce source coordinates.

*     Routines are then called that make a first guess at the
*     properties of the sources. These values are then used as
*     a starting point for the minimisation routines.

*     The final parameter values are saved to a text file and an
*     output image created.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     NG: Norman Gray (Starlink, GLA)
*     {enter_new_authors_here}

*  History:
*     10-Mar-1996 (GJP)
*     (Original version)
*     25-Feb-1998 (NG)
*     (add psize parameter to control pixel size, and display of FWHM/sigma
*     3-JUN-1998 (NG)
*     (incorporate routines in gaufit2.f)
*
*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'MSG_PAR'               ! MSG constants
      INCLUDE 'SUBPAR_PAR'            ! SUBPAR constants
      INCLUDE 'PAR_ERR'		      ! defines PAR__NULL
      include 'GAU_PAR'
      INCLUDE 'CNF_PAR'

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      CHARACTER *(MSG__SZMSG) FILE    ! NDF file name
      CHARACTER *(256) MODTYC         ! Type of output image
      INTEGER MODTYP                  ! Integer representation of MODTYC
      LOGICAL ANGCON                  ! Position angle convention
      LOGICAL AUTOL                   ! Better origin?
      INTEGER COLOUR                  ! Pen colour used for source marker
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER FIRST                   ! First time the NDF identifier has been
                                      ! determined
      INTEGER ISTAT                   ! Cursor error value returned
      INTEGER LBND(NDF__MXDIM)        ! Lower limit for image index
      INTEGER NDF1                    ! Identifier for the source NDF
      INTEGER NDF2                    ! identifier for the output NDF
      INTEGER NDIM                    ! Number of dimensions in the
                                      ! image
      INTEGER NITER                   ! Number of iterations
      INTEGER NSOUR                   ! Number of sources
      INTEGER POINT(6)                ! Pointers to the image data
                                      ! and workspace arrays
      INTEGER PRANGE(2)               ! Length of the x and y axes
      INTEGER MODEL                   ! Pointer to output image
      INTEGER STLEN                   ! File name length
      INTEGER UBND(NDF__MXDIM)        ! Upper limit for image index
      INTEGER UPIX                    ! Number of used pixels in the output
                                      ! image
      integer i,j                     ! loop counts!
      REAL ANGOFF                     ! Position angle offset
      REAL ANGINC                     ! Size of angle change during minimisation
      REAL BACK                       ! Background value
      REAL HINT(4,10)                 ! User angle, Sa, Sb and peak values
      REAL GUESS(10,7)                ! Initial source parameters
      real guesserrs(10,7)            ! Uncertainties on source parameters
      REAL NSIGMA                     ! Pixel count threshold factor
      REAL PINC                       ! Size of angle change during minimisation
      REAL PSIZE		      ! Pixel size, units of arcsec
      REAL RLIM(10)                   ! Source maximum size
      REAL SAINC                      ! Size of change factor in Sa
      REAL SBINC                      ! Size of change factor in Sb
      REAL SIGMA                      ! Std. dev. of the background value
      REAL X(10)                      ! Indices of the co-ordinates input
      REAL XCO(10,2)                  ! X index of the source origin
      REAL XINC                       ! Size of X movement in minimisation
      REAL YINC                       ! Size of Y movement in minimisation
      REAL Y(10)                      ! Indices of the co-ordinates input
      REAL YCO(10,2)                  ! Y index of the source origin
      LOGICAL DISPFWHM		      ! Display FWHM rather than sigma
      logical lsqfit                   ! Use the NSG fit method, not original
      logical calcsd            ! calculate and display SD errors (NSG only)
      logical fitback           ! background to be fitted, rather than given
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the pen colour required.
      CALL PAR_GET0I('COLOUR',COLOUR,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Look at the command line value for ANGCON.
*   Otherwise, use the default.
      CALL PAR_GET0L('ANGCON',ANGCON,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Look at the command line value for ANGOFF.
*   Otherwise, use the default.
      CALL PAR_GET0R('ANGOFF',ANGOFF,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Begin an NDF context.
      CALL NDF_BEGIN
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Initialise variables.
      FIRST=0
      NSOUR=0
      ISTAT=0

      DO WHILE ( (ISTAT.EQ.0).AND.(NSOUR.LT.10) )

*      Get the co-ordinates from the image. Taking care to ensure that
*      the NDF is obtained by GAU1_CURSO only the first time
*      a co-ordinate is provided.
         CALL GAU1_CURSO('IMGDEV',FIRST,COLOUR,NDF1,X,Y,
     :                   ISTAT,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*    Test the ISTAT of the return.
         IF (ISTAT.EQ.0) THEN

*         Increment number of sources.
            NSOUR=NSOUR+1
            XCO(NSOUR,1)=X(1)
            YCO(NSOUR,1)=Y(1)

*         Indicate that the maximum number of sources has been reached.
            IF(NSOUR.EQ.10) THEN
               CALL MSG_BLANK(STATUS)
               CALL MSG_OUT(' ','WARNING!',STATUS)
               CALL MSG_OUT(' ',
     :          'Only 10 sources allowed. This is the last.',STATUS)
            END IF

*         Get the image bounds and also the size of the axes in pixels.
*         Need only be done once.
            IF (NSOUR.EQ.1) THEN
               CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
               IF (STATUS.NE.SAI__OK) GOTO 9999
               PRANGE(1)=UBND(1)-LBND(1)+1
               PRANGE(2)=UBND(2)-LBND(2)+1
            END IF

*         Get the radius value from the image.
            CALL GAU1_CURSO('IMGDEV',2,COLOUR,NDF1,X,Y,
     :                     ISTAT,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9999

*         Test the ISTAT returned.
            IF(ISTAT.EQ.0) THEN

*            Calculate radius of the source.
               RLIM(NSOUR)=SQRT
     :               ((X(1)-X(2))*(X(1)-X(2))+(Y(1)-Y(2))*(Y(1)-Y(2)))
               IF (RLIM(NSOUR).LT.3.) THEN
                  RLIM(NSOUR)=3.
                  CALL MSG_OUT(' ','Radius corrected to 2.0',STATUS)
               END IF

*            Set default values for the HINTs.
               HINT(1,NSOUR)=VAL__BADR
               HINT(2,NSOUR)=VAL__BADR
               HINT(3,NSOUR)=VAL__BADR
               HINT(4,NSOUR)=VAL__BADR

            ELSE

*            Remove last selection.
               IF(NSOUR.GT.0) NSOUR=NSOUR-1

            END IF

         END IF

      END DO

*   Abort if no sources defined.
      IF (NSOUR.EQ.0) THEN
         CALL MSG_OUT(' ',
     :                'No sources were defined. Aborting!',STATUS)
         GOTO 9999
      ELSE
*      Echo the input coordinates, to reassure the user
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','Read source positions:',STATUS)
         CALL MSG_OUT(' ','  Source     X      Y   Radius-limit',
     :        STATUS)
         DO I=1,NSOUR
            CALL MSG_FMTI('I','I2',I)
            CALL MSG_FMTR('XCO','F6.1',XCO(I,1))
            CALL MSG_FMTR('YCO','F6.1',YCO(I,1))
            CALL MSG_FMTR('RL','F6.1',RLIM(I))
            CALL MSG_OUT(' ','    ^I    ^XCO ^YCO ^RL',STATUS)
         END DO
      END IF

*   Look at the command line value for PSIZE.
*   A NULL value (`!') may be entered here - indicates sizes should be
*   shown in pixels, rather than converted to arcsec
*   Require that the size be strictly positive - 1 micro-arcsec is
*   Infeasibly small, and non-zero.
*
*   The PSIZE parameter is used internally to encode both the pixel size,
*   and whether FWHM or sigma is to be displayed.
*   psize positive means display FWHM rather than sigma.
*   abs(psize) is pixel size in arcsec.
*   abs(psize)<1e-6 means display in units of pixels
      CALL ERR_MARK
      CALL ESP1_GTPSZ (NDF1, PSIZE, STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
*      Negative pixel size flags `no size known'
         PSIZE = 1e-7
         CALL ERR_ANNUL (STATUS)
      ENDIF
      CALL ERR_RLSE

*   Do we display FWHM or sigma?  (see psize in gau1_cmode for discussion)
      CALL PAR_GET0L ('FWHM', DISPFWHM, STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (.NOT.DISPFWHM) PSIZE = -PSIZE

*   Get which fit method we're to use.
      call par_get0l ('LSQFIT', lsqfit, status)
      if (status .ne. sai__ok) goto 9999

*   Should we calculate (and display) uncertainties
*   Only available if lsqfit is true.
      if (lsqfit) then
         call par_get0l ('CALCSD', calcsd, status)
         if (status .ne. sai__ok) goto 9999
      endif

*   Get the background count value.
*   A negative or NULL value (`!') indicates that the fitting
*   routine should determine the background, rather than be told it.
*   This only works for the non-linear (GN) fitting method implemented in
*   gau2_pro
      fitback = .false.
      if (lsqfit) then
         call par_promt('BACK',
     :        'Background count value ( < 0 to have it fitted)',status)
         call par_get0r('back',back,status)
         if (back .lt. 0.) then
            fitback = .true.    ! tells gau1_texto to note that it was fitted
            back = -1.          ! tells gau2_pro to fit background
         endif
      else
         call par_minr('BACK', 0.0, status)
         call par_get0r('BACK', back, status)
      endif

      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the background count standard deviation (only if BACK was specified)
      if (.not. fitback) then
         CALL PAR_GET0R('SIGMA',SIGMA,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the number of sigma above sky at which a point becomes
*      significant.
         CALL PAR_GET0R('NSIGMA',NSIGMA,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
      endif

*   Get the number of minimisation iterations to perform.  In the LSQ
*   case, get this from the parameter MAXITER (it's an upper limit on
*   the number); in the original case, from the parameter NITER (it
*   specifies a fixed number of iterations.  In either case, put the
*   result in the variable NITER (venial overloading).
      if (lsqfit) then
         call par_get0i ('MAXITER', niter, status)
         if (niter .lt. 0) then
*         Ensure this is -1 (the range should guarantee this, and we
*         don't depend on it, but it's good to be consistent)
            niter = -1          ! tells gau2_pro to use default (150)
         endif
      else
         CALL PAR_GET0I('NITER',NITER,STATUS)
      end if
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL MSG_BLANK(STATUS)

*   Map the input NDF data array as _REAL values for reading.
      CALL NDF_MAP(NDF1,'DATA','_REAL','READ',POINT(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the name of the file being mapped.
      CALL NDF_MSG('FILE',NDF1)
      CALL MSG_LOAD(' ','^FILE',FILE,STLEN,STATUS)

*   Allocate dynamic memory on which to map the NDF.
      CALL PSX_CALLOC(ELEMS,'_REAL',POINT(2),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Allocate more dynamic memory on which to map the NDF.
      CALL PSX_CALLOC(ELEMS,'_REAL',POINT(3),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Transfer values from the mapped NDF to the allocated memory.
      CALL GAU1_TRANS(NSOUR,BACK,SIGMA,NSIGMA,ELEMS,
     :               %VAL(CNF_PVAL(POINT(1))),
     :               %VAL(CNF_PVAL(POINT(2))),XCO,YCO,RLIM,PRANGE,
     :               UPIX,GUESS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Allocate more dynamic memory on which to map the addresses of good pixels.
      CALL PSX_CALLOC(UPIX,'_INTEGER',POINT(4),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Allocate more dynamic memory on which to map the X index of good pixels.
      CALL PSX_CALLOC(UPIX,'_INTEGER',POINT(5),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Allocate more dynamic memory on which to map the Y index of good pixels.
      CALL PSX_CALLOC(UPIX,'_INTEGER',POINT(6),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Store locations of the pixels being used. Will help speed things up
*   later.
      CALL GAU1_TRAN2(ELEMS,%VAL(CNF_PVAL(POINT(2))),UPIX,PRANGE(1),
     :                %VAL(CNF_PVAL(POINT(4))),
     :                %VAL(CNF_PVAL(POINT(5))),
     :                %VAL(CNF_PVAL(POINT(6))),
     :                STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Propogate the bits of the source NDF required.
      CALL NDF_PROP(NDF1,'DATA,WCS','MODEL',NDF2,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Get the type of image to be created and convert to upper case.
      if (lsqfit) then
         call par_promt ('MODTYP',
     :    'Whole image model (W)/Residuals (R)/reGression diag. (G)',
     :        status)
      endif
      CALL PAR_GET0C('MODTYP',MODTYC,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL CHR_UCASE(MODTYC)
      if (modtyc(:1) .eq. 'W') then
         modtyp = gau2whole
      elseif (modtyc(:1) .eq. 'R') then
         modtyp = gau2residual
      elseif (modtyc(:1) .eq. 'G' .and. lsqfit) then
         modtyp = gau2regdiag
      else
         call msg_out (' ',
     :        'Unrecognised model type - W assumed', status)
         modtyp = gau2whole
      endif
      CALL MSG_BLANK(STATUS)

*   Map the output NDF data array as _REAL values for updating.
      CALL NDF_MAP(NDF2,'DATA','_REAL','UPDATE',MODEL,ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Un-map the source NDF. Helps to reduce the resources being used.
      CALL NDF_UNMAP(NDF1,'DATA',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Slightly different preparations for the two fitting methods
      if (lsqfit) then

*      Get an estimate for the first position angles, sigmas and
*      peak values to try.
         call gau1_guess(nsour,angcon,angoff,psize,sigma,
     :        nsigma,back,xco,yco,rlim,
     :        elems,%val(cnf_pval(point(2))),prange,guess,
     :        hint,status)
         if (status.ne.sai__ok) goto 9998

         call gau2_pro (nsour, modtyp, angcon, angoff, psize, niter,
     :        rlim, back, sigma, elems, upix, point, prange,
     :        guess, guesserrs, calcsd, status)

*      Indicate the final parameter values.
         call msg_blank (status)
         call msg_out (' ', 'Fitted parameter values:', status)
         do i=1,nsour
            call gau1_disp(i,angcon,angoff,psize,guess,status)
         enddo
*      ...and uncertainties
         call msg_blank (status)
         call msg_out (' ', 'Parameter uncertainties: (-ve values '//
     :        'indicate no estimate made)', status)
         do i=1,nsour
            call gau1_disp(i,angcon,angoff,psize,guesserrs,status)
         enddo
         call msg_blank (status)

         if (fitback) then
            call msg_setr ('BG', back)
            call msg_out (' ', 'Background (fitted) = ^BG', status)
            call msg_blank (status)
         endif

      else

*      Get the size of the movement in the X direction
*      permitted during minimisation.
         CALL PAR_GET0R('XINC',XINC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the size of the movement in the Y direction
*      permitted during minimisation.
         CALL PAR_GET0R('YINC',YINC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the size of the change factor in Sa
*      permitted during minimisation.
         CALL PAR_GET0R('SAINC',SAINC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the size of the change factor in Sb
*      permitted during minimisation.
         CALL PAR_GET0R('SBINC',SBINC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the size of the change factor in peak
*      permitted during minimisation.
         CALL PAR_GET0R('PINC',PINC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the size of the change factor in angle
*      permitted during minimisation.
         CALL PAR_GET0R('ANGINC',ANGINC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
         CALL MSG_BLANK(STATUS)

*      Look for a better (though crude) estimate of the source core position.
         CALL PAR_GET0L('AUTOL',AUTOL,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF (AUTOL) CALL GAU1_AUTOL(NSOUR,ELEMS,PRANGE,
     :                              %VAL(CNF_PVAL(POINT(2))),
     :                              XCO,YCO,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Get an estimate for the first position angles, sigmas and
*      peak values to try.
         CALL GAU1_GUESS(NSOUR,ANGCON,ANGOFF,PSIZE,SIGMA,
     :        NSIGMA,BACK,XCO,YCO,RLIM,
     :        ELEMS,%VAL(CNF_PVAL(POINT(2))),PRANGE,GUESS,
     :        HINT,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Call the routine that profiles the source and sets up the values
*      in the results arrays.
         CALL GAU1_PRO(NSOUR,MODTYP,XINC,YINC,SAINC,SBINC,
     :        ANGINC,PINC,ANGCON,ANGOFF,PSIZE,NITER,RLIM,
     :        BACK,SIGMA,ELEMS,UPIX,POINT,
     :        PRANGE,GUESS,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      This route doesn't calculate guesserrs(), so set all the elements
*      there to flag values (negative)
         do i=1,7
            do j=1,10
               guesserrs(j,i) = -1.0
            enddo
         enddo

      endif

*   [ orig comment: Copy the errors generated to an output image. ]
*   No - it copies the working image to the result image.  No errors
*   are calculated anywhere... [NG]
      CALL GAU1_OUTIM(ELEMS,%VAL(CNF_PVAL(POINT(3))),
     :                %VAL(CNF_PVAL(MODEL)),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Output a text file containing results if required.
      CALL GAU1_TEXTO(NSOUR,ANGCON,ANGOFF,PSIZE,LBND,NDF1,GUESS,
     :     guesserrs, BACK, fitback, SIGMA,lsqfit,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   An appropriate place to exit to if the dynamic memory has already
*   been allocated.
 9998 CONTINUE

*   De-allocate the dynamic memory used.
      CALL PSX_FREE(POINT(6),STATUS)
      CALL PSX_FREE(POINT(5),STATUS)
      CALL PSX_FREE(POINT(4),STATUS)
      CALL PSX_FREE(POINT(3),STATUS)
      CALL PSX_FREE(POINT(2),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

*   End the NDF context.
      CALL NDF_END(STATUS)

      END


      SUBROUTINE SEC1_CMODE(STATUS)
*+
*  Name:
*     SEC1_CMODE

*  Purpose:
*     May be used to display the average pixel values within a wedge shaped
*     sector of the image. The sector is in the form of a wedge (of user
*     defined size) drawn out from an origin point.
*
*     The results are displayed as pixel value (in terms of level relative
*     to sky or surface brightness) versus origin distance. Pixel values
*     are summed over all the points at a given distance from the origin.
*
*     Options include summing results from two equal sized diametrically
*     opposite sectors, displaying data using a number of possible radius
*     transformations, the use of an approximate local maximum location as
*     origin if required, automatic selection of the maximum radius out
*     from the origin to be considered and the use of a graphics cursor
*     to select the image object to be examined.
*
*     This routine operates using a combination of keyboard and cursor
*     inputs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_CMODE(STATUS)

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Allows the pixels count values within a wedge/sector drawn from a
*     user specified point(s) to be plotted as a function of distance
*     from the origin point.
*
*     Also, allows the results from two diametrically opposite sectors
*     to be plotted as an option. The results are displayed with
*     radii as arc seconds and mean pixel values expressed in terms of
*     sigma or surface brightness.
*
*     The length of the slice, it's angular width and position angle are
*     all user defined. The length may also be selected automatically by
*     the software if desired.
*
*     The user is allowed to define how the radius values will be displayed
*     i.e. as R, R**0.25, Log R or R x R.
*
*     Contaminating parts of the image may be defined using an ARD file.
*
*     An option is present allowing the input object location to be
*     adjusted by the software to employ an approximate (1 pixel
*     accuracy) estimate of the location of the weighted intensity maximum.

*  Implementation Status:
*     Under development

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-Nov-1992 (GJP)
*     (Original version)
*     20-FEB-1997 (GJP)
*     Modified to avoid cursor selection of points when
*     GRAPH=FALSE.

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'SEC_PAR'               ! SECTOR constants
      INCLUDE 'SUBPAR_PAR'            ! SUBPAR constants
      INCLUDE 'CNF_PAR'

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      CHARACTER *(256) RADISP         ! Option choice defining how the
                                      ! radius data is to be displayed
      LOGICAL AGAIN                   ! Determine another profile?
      LOGICAL AUTOL                   ! Is an estimate of the galaxy centre
                                      ! position to be made?
      LOGICAL EXCLAIM                 ! File name was '!'?
      LOGICAL GRAPH                   ! Is a graph to be plotted
      LOGICAL INOKAY                  ! Was the most recent input value okay
      LOGICAL INOK2                   ! Same as above
      LOGICAL MIRROR                  ! Sum the pixels over a single slice
                                      ! or two diametrically opposite slices
      LOGICAL SAME                    ! Use the display device the image is
                                      ! on to show the result graphs?
      LOGICAL SURF                    ! Pixel values expressed as sigma
                                      ! or surface brightness
      INTEGER AGIID                   ! AGI identifier
      INTEGER COUNT(2)                ! The number of data points used in the
                                      ! scale length regression
      INTEGER COLOUR                  ! Colour of the galaxy centre point
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER FIOD2                   ! Output file identifier
      INTEGER FIRST                   ! First time the NDF identifier has been
                                      ! determined
      INTEGER FLAG                    ! Can the central pixel value be found?
      INTEGER I                       ! Loop variable
      INTEGER LBND(NDF__MXDIM)        ! Lower limit for image index
      INTEGER RLIM                    ! The length of the slice to be taken
      INTEGER LEN2                    ! Temporary storage of RLIM
      INTEGER NDF1                    ! Identifier for the source NDF
      INTEGER NDIM                    ! Number of dimensions in the
                                      ! image
      INTEGER NVP                     ! The number of valid data points found
      INTEGER PFLAG                   ! can the position angle be found?
      INTEGER POINT0(1)               ! Pointer to the data component of
                                      ! the source NDF
      INTEGER POINT1(1)               ! Pointer to the data component of
                                      ! for the output NDF
      INTEGER POINT3(1)               ! Pointer to the mapped ARD mask
      INTEGER PRANGE(2)               ! Length of the x and y axes
      INTEGER UBND(NDF__MXDIM)        ! Upper limit for image index
      REAL ANGLE                      ! Angle between a line between two
                                      ! points
      REAL ANGWID                     ! Angular width of the sector
      REAL BACK                       ! Background count value
      REAL CONS(2)                    ! Constant terms of the curves used
                                      ! to find the scale length
      REAL DISTAN                     ! Distance between two points on
                                      ! the image
      REAL GRAD(2)                    ! Gradients of the curves used
                                      ! to find out the scale length
      REAL HIR                        ! Highest radius value used in the fit
                                      ! calculated
      REAL LOR                        ! Lowest radius value employed in
                                      ! the fit calculated
      REAL NUMBER(SEC__RESUL)         ! The number of pixels found at a given
                                      ! distance from the origin.
      REAL OCOUNT                     ! Pixel count for the origin pixel
      REAL POSANG                     ! Position angle of the slice
      REAL PSIZE                      ! Size of the image pixels in arc sec
      REAL RADIUS                     ! radius of the arc to be used when
                                      ! drawing the sector
      REAL SIGMA                      ! Standard deviation of the background value
      REAL SLEN(2)                    ! Scale length of the galaxy
      REAL SUMMAT(SEC__RESUL)         ! Sum of the pixel counts for all pixels
                                      ! at a given distance from the origin
      REAL TEMP                       ! Temporary storage
      REAL X(10)                      ! Array indices of the sector to be used
      REAL XCO                        ! X index of the sector origin
      REAL Y(10)                      ! Array indices of the sector to be used
      REAL YCO                        ! Y index of the sector origin
      REAL ZEROP                      ! Zero point of the surface
                                      ! brightness graphs
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Get the pen colour.
      CALL PAR_STATE('COLOUR',I,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0I('COLOUR',COLOUR,STATUS)
         CALL MSG_OUT(' ','Command line COLOUR value used.',STATUS)
      ELSE
         COLOUR=1
      END IF

*   Begin an NDF context.
      CALL NDF_BEGIN
      IF (STATUS.NE.SAI__OK) GOTO 9999

*     Loop around looking at different parts for the same image.
      AGAIN=.TRUE.
      FIRST=0
      DO WHILE (AGAIN.AND.(STATUS.EQ.SAI__OK))

*      Get the cursor positon for the galaxy origin. At the same time
*      obtain the NDF identifier for the most recent 'DATA' picture.
         INOKAY=.FALSE.
         DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))

*         Get the value from the image. Taking care to ensure that
*         the NDF is obtained by SEC1_CURSO only the first time
*         a co-ordinate is provided.
            CALL SEC1_CURSO('IMGDEV',FIRST,0,POSANG,COLOUR,NDF1,
     :                      X,Y,RADIUS,ANGWID,STATUS)
            XCO=X(10)
            YCO=Y(10)

*         Get the image bounds and also the size of the axes in pixels.
            CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9999
            PRANGE(1)=UBND(1)-LBND(1)+1
            PRANGE(2)=UBND(2)-LBND(2)+1

*         Check that the co-ordinate values input are legal and
*         annul the parameter if not.
            IF ((XCO.LT.1.0).OR.(XCO.GT.PRANGE(1)).OR.(YCO.LT.0.0).
     :           OR.(YCO.GT.PRANGE(2))) THEN
               CALL MSG_OUT(' ','The position supplied, is not '//
     :                      'within the image.',STATUS)
               CALL PAR_CANCL('ORIGIN',STATUS)
            ELSE
               INOKAY=.TRUE.
            END IF
            FIRST=1

         END DO
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get slice position angle.

*      Get the position indicating the sector direction and extent.
         CALL SEC1_CURSO('IMGDEV',2,0,POSANG,COLOUR,NDF1,
     :                   X,Y,RADIUS,ANGWID,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Calculate the position angle in radians.
         CALL SEC1_POSAN(X(1),Y(1),X(2),Y(2),ANGLE,DISTAN,PFLAG,STATUS)
         POSANG=ANGLE
         RADIUS=DISTAN

*      Get the pixel position defining the angular width of the sector.
         CALL SEC1_CURSO('IMGDEV',3,0,POSANG,COLOUR,NDF1,
     :                   X,Y,RADIUS,ANGWID,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Calculate the angular width of the sector required.
         CALL SEC1_POSAN(X(1),Y(1),X(3),Y(3),ANGLE,DISTAN,PFLAG,STATUS)
         IF (POSANG-ANGLE.GT.SEC__PIVAL) ANGLE=ANGLE+SEC__PIVAL
         IF (ANGLE-POSANG.GT.SEC__PIVAL) ANGLE=ANGLE-SEC__PIVAL
         ANGWID=2.*ABS(POSANG-ANGLE)

*      Display the sector.
         CALL SEC1_CURSO('IMGDEV',4,0,POSANG,COLOUR,NDF1,
     :                   X,Y,RADIUS,ANGWID,STATUS)
         CALL MSG_BLANK(STATUS)

*      Are pixels on both sides of the origin to be used.
         CALL PAR_GET0L('MIRROR',MIRROR,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Display the second half of the sector.
         IF (MIRROR) CALL SEC1_CURSO('IMGDEV',5,0,POSANG,COLOUR,NDF1,
     :                               X,Y,RADIUS,ANGWID,STATUS)

*      Convert position angle and angular width from radians
*      to degrees format.
         POSANG=POSANG*SEC__RADS
         ANGWID=ANGWID*SEC__RADS
         RLIM=NINT(RADIUS)

*      Get the background count value.
         CALL PAR_GET0R('BACK',BACK,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the background count standard deviation value.
         INOKAY=.FALSE.
         DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))
            CALL PAR_GET0R('SIGMA',SIGMA,STATUS)
            IF (SIGMA.LE.0.0) THEN
*            Display message and annul the parameter.
               CALL MSG_OUT(' ','Sigma supplied, is not '//
     :                      'feasible.',STATUS)
               CALL PAR_CANCL('SIGMA',STATUS)
            ELSE
               INOKAY=.TRUE.
            END IF
         END DO
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the pixel size value.
         CALL ESP1_GTPSZ(NDF1,PSIZE,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Convert the length in arc secs to length in pixels.
         RLIM=NINT(REAL(RLIM)/PSIZE)
         IF (RLIM.GT.SEC__RESUL) THEN
            STATUS=SAI__ERROR
            CALL ERR_REP(' ','The size of the sector is '//
     :                   'too big.',STATUS)
            GOTO 9999
         END IF

*      Get the user selection of values shown in sigma or surface brightness.
         CALL PAR_GET0L('SURF',SURF,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the radius display type mode and convert to upper case.
         CALL PAR_GET0C('RADISP',RADISP,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
         CALL CHR_UCASE(RADISP)

*      Get the zero point for the surface brightness scale/graphs.
         CALL PAR_GET0R('ZEROP',ZEROP,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the user selection of automatic location of the galaxy centre
*      or not.
         CALL PAR_GET0L('AUTOL',AUTOL,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Map the input NDF data array as _REAL values for reading.
         CALL NDF_MAP(NDF1,'DATA','_REAL','READ',POINT0(1),ELEMS,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Allocate dynamic memory on which to map the NDF.
         CALL PSX_CALLOC(ELEMS,'_REAL',POINT1(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Transfer values from the mapped NDF to the allocated memory.
         CALL SEC1_TRANS(ELEMS,%VAL(CNF_PVAL(POINT0(1))),
     :                   %VAL(CNF_PVAL(POINT1(1))),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Un-map the source NDF. Helps to reduce the resources being used.
         CALL NDF_UNMAP(NDF1,'DATA',STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Allocate the memory needed for the logical mask array.
         CALL PSX_CALLOC(ELEMS,'_INTEGER',POINT3(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Transfer to the ARD driver control routine.
         CALL ESP1_ARD_DRIVER(NDIM,ELEMS,LBND,UBND,POINT1,POINT3,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Free the dynamic array space of the logical mask.
         CALL PSX_FREE(POINT3(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Look for a better (though crude) estimate of the galaxy core position.
         CALL SEC1_AUTOL(AUTOL,ELEMS,PRANGE,OCOUNT,FLAG,
     :                   %VAL(CNF_PVAL(POINT1(1))),XCO,YCO,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Call the routine that fills the arrays with the summation of all
*      the data points within the required slice.
         CALL SEC1_PIE(1,BACK,ELEMS,XCO,YCO,PRANGE,POSANG,ANGWID,NVP,
     :                 NUMBER,SUMMAT,RLIM,%VAL(CNF_PVAL(POINT1(1))),
     :                 STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
         LEN2=RLIM


*      Continue the radius/brightness summation using pixels on the other
*      side of the origin.
         IF (MIRROR) THEN

*          Pass the position angle for the other side of the origin and
*          then repeat the summation.
            TEMP=POSANG+180.0

*         Perform the count summation for the opposite side of the object.
            CALL SEC1_PIE(0,BACK,ELEMS,XCO,YCO,PRANGE,TEMP,ANGWID,NVP,
     :                    NUMBER,SUMMAT,RLIM,%VAL(CNF_PVAL(POINT1(1))),
     :                    STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9999

         END IF

*      Ensure that the length value used reflects the highest value found
*      even when MIRROR is true.
         IF (RLIM.LT.LEN2) RLIM=LEN2

*      Only bother with the graphs if there is more than one data point.
         IF (NVP.GT.1) THEN

*         Get the user selection of using the same display as the image
*         to display the results.
            CALL PAR_GET0L('SAME',SAME,STATUS)

*         Display the results graphs on the device used to show the image.
            IF (SAME) THEN

*            Find limits for the window to be used to display the graph.
               CALL SEC1_CURSO('IMGDEV',6,0,POSANG,COLOUR,NDF1,
     :                         X,Y,RADIUS,ANGWID,STATUS)

*            Set up the new window.
               CALL SEC1_CURSO('IMGDEV',7,0,POSANG,COLOUR,NDF1,
     :                         X,Y,RADIUS,ANGWID,STATUS)

*            Set up the AGI/PGPLOT interface.
               CALL SEC1_AGICO(0,1,0,AGIID,STATUS)

*            Display the raw results graph.
               CALL SEC1_GRAPH(1,ZEROP,RADISP,SURF,RLIM,BACK,
     :                         NUMBER,PSIZE,SIGMA,SUMMAT,CONS,
     :                         GRAD,STATUS)

*            Turn off the AGI/PGPLOT interface.
               CALL SEC1_AGICO(1,1,0,AGIID,STATUS)

*            Only get back the radius limits if there are more
*            than two data points.
               IF (NVP.GT.2) THEN

*               Loop around until two reasonable value have been selected
*               for the minimum and maximum radii.
                  INOK2=.FALSE.
                  DO WHILE ((.NOT.INOK2).AND.(STATUS.EQ.SAI__OK))

*                  Display a cursor in the new window and get back the
*                  range of radius values the user is interested in.
                     CALL SEC1_CURSO('IMGDEV',8,1,POSANG,COLOUR,NDF1,
     :                               X,Y,RADIUS,ANGWID,STATUS)
                     CALL SEC1_CURSO('IMGDEV',9,1,POSANG,COLOUR,NDF1,
     :                               X,Y,RADIUS,ANGWID,STATUS)
                     LOR=MIN(X(8),X(9))
                     HIR=MAX(X(8),X(9))

*                 Transfer the radius values for conversion.
                     CALL SEC1_UNCON(RADISP,LOR,HIR,STATUS)

*                  Check to see if it is possible for there to be two
*                  data points in the radius range required.
                     IF ((HIR-LOR)/PSIZE.LT.2.0) THEN
                        CALL MSG_BLANK(STATUS)
                        CALL MSG_OUT(' ','The radius range supplied,'//
     :                              ' is too narrow to be used.',STATUS)
                     ELSE
                        INOK2=.TRUE.
                     END IF

                  END DO

*               Calculate the scale length assuming spiral or elliptical.

*               Obtain the 'fit' parameters for linear fits to the
*               brightness versus radius data (suitably transformed).
                  CALL SEC1_LINRE(LOR,HIR,NUMBER,SUMMAT,PSIZE,BACK,
     :                            RLIM,GRAD,CONS,COUNT,SLEN,STATUS)

*               Open up the AGI/PGPLOT interface again.
                  CALL SEC1_AGICO(0,1,1,AGIID,STATUS)

*               Display the fit plots if a graphics device was selected.
                  CALL SEC1_GRAPH(2,ZEROP,RADISP,SURF,RLIM,BACK,
     :                            NUMBER,PSIZE,SIGMA,SUMMAT,CONS,
     :                            GRAD,STATUS)

*               Turn off the AGI/PGPLOT interface.
                  CALL SEC1_AGICO(1,1,1,AGIID,STATUS)

               END IF

            END IF

         END IF


*      Display the results graphs on a device specified by the user.
         IF (.NOT.SAME) THEN

*      Only display the graphs if there is more than 1 data point.
             IF (NVP.GT.1) THEN

*            Display the graph of the data points i.e. radius versus brightness.
*            Determine if graphical histogram output is required. Set the
*            value for GRAPH accordingly.
               AGIID=0
               GRAPH=.TRUE.
               CALL ERR_MARK
               CALL SEC1_AGICO(0,0,0,AGIID,STATUS)
               IF (STATUS.NE.SAI__OK) THEN
                  GRAPH=.FALSE.
                  CALL ERR_ANNUL(STATUS)
               END IF
               CALL ERR_RLSE

*            Display the un-analysed data as a graphical plot of radius
*            (in some form) versus intensity (in some form).
               IF (GRAPH) THEN
                  CALL SEC1_GRAPH(1,ZEROP,RADISP,SURF,RLIM,BACK,
     :                            NUMBER,PSIZE,SIGMA,SUMMAT,
     :                         CONS,GRAD,STATUS)
*               Turn off the AGI/PGPLOT interface.
                  CALL SEC1_AGICO(1,0,0,AGIID,STATUS)
               END IF

*            Only determine the scale lengths if there are more
*            than 2 data points.
               IF (NVP.GT.2) THEN

                  IF(GRAPH) THEN
*                  Display a cursor in the new window and get back the range of
*                  radius values the user is interested in.
                     CALL SEC1_CURSO('DEVICE',8,1,POSANG,COLOUR,NDF1,
     :                               X,Y,RADIUS,ANGWID,STATUS)
                     CALL SEC1_CURSO('DEVICE',9,1,POSANG,COLOUR,NDF1,
     :                               X,Y,RADIUS,ANGWID,STATUS)
                     LOR=MIN(X(8),X(9))
                     HIR=MAX(X(8),X(9))

*                 Transfer the radius values for conversion.
                     CALL SEC1_UNCON(RADISP,LOR,HIR,STATUS)

                  ELSE

*                  Define range when no device selected.
                     LOR=0.0
                     HIR=RADIUS

                  END IF

*               Check to see if it is possible for there to be two
*               data points in the radius range required.
                  IF ((HIR-LOR)/PSIZE.LT.2.0) THEN
                     STATUS=SAI__ERROR
                     CALL ERR_REP(' ','The radius range supplied,'//
     :                            ' is too narrow to be used.',STATUS)
                     GOTO 9999
                  END IF

*               Calculate the scale length assuming spiral or elliptical.

*               Obtain the 'fit' parameters for linear fits to the
*               brightness versus radius data (suitably transformed).
                  CALL SEC1_LINRE(LOR,HIR,NUMBER,SUMMAT,PSIZE,BACK,
     :                            RLIM,GRAD,CONS,COUNT,SLEN,STATUS)


*               Display the fit plots if a graphics device was selected.
                  IF (GRAPH) THEN

*                  Open up the AGI/PGPLOT interface again.
                     CALL SEC1_AGICO(0,0,1,AGIID,STATUS)

*                  Show the fit.
                     CALL SEC1_GRAPH(2,ZEROP,RADISP,SURF,RLIM,BACK,
     :                               NUMBER,PSIZE,SIGMA,SUMMAT,
     :                               CONS,GRAD,STATUS)

*                  Turn off the AGI/PGPLOT interface.
                     CALL SEC1_AGICO(1,0,0,AGIID,STATUS)

                  END IF

               END IF

            END IF

         END IF


*      Display the results on the default display.
*      Output a text file containing results if required.
         IF (NVP.GT.2) THEN
            CALL SEC1_TEXTD(FLAG,NDF1,XCO,YCO,OCOUNT,BACK,SIGMA,CONS,
     :                      GRAD,RLIM,PSIZE,COUNT,ZEROP,SLEN,
     :                      LOR,HIR,LBND,STATUS)

            CALL SEC1_TEXTO(NDF1,SUMMAT,NUMBER,XCO,YCO,BACK,
     :                      SIGMA,CONS,RLIM,PSIZE,ZEROP,SLEN,LBND,
     :                      FIOD2,EXCLAIM,STATUS)
         END IF

*      De-allocate the dynamic memory used.
         CALL PSX_FREE(POINT1(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the user selection of preparing another galaxy profile
*      or not.
         CALL PAR_GET0L('AGAIN',AGAIN,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

         IF (AGAIN) THEN

*         Cancel the parameters so that they must be reinput when
*         looping round.
            CALL SEC1_CANCL(0,STATUS)

         END IF

      END DO

      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

*   End the NDF context.
      CALL NDF_END(STATUS)

      END
