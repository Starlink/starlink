

      SUBROUTINE ELF1_KMODE(STATUS)
*+
*  Name:
*     ELF1_KMODE

*  Purpose:
*     The routine allows the user to input most of the information required
*     (position, background value etc) for the galaxy profile to be
*     performed/displayed and then calls other routines as necessary.
*
*     Information such as pixel size, background count value and its
*     standard deviation are also input.
*
*     The routine operates using a combination of keyboard inputs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_KMODE(STATUS)

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Profiling is continued until the mean profile value is less than
*     a user defined number of standard deviation above sky (LIM2) or
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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-MAR-1993 (GJP)
*     (Original version)
*     26-OCT-1999 (MBT)
*     Modified to cope with COSYS=C.
*     8-NOV-1999 (MBT)
*     Removed COSYS altogether (use WCS instead).

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
      LOGICAL AGAIN                   ! Look at another part of the image?
      LOGICAL ANGCON                  ! Position angle convention
      LOGICAL AUTOL                   ! Is an estimate of the galaxy centre
                                      ! position to be made?
      LOGICAL EXCLAIM                 ! Was the filename an exclaimation
      LOGICAL FRZORI                  ! Is the galaxy origin frozen?
      LOGICAL GRAPH                   ! Is a graph to be plotted
      LOGICAL INOKAY                  ! Was the most recent input value okay
      LOGICAL WRITECAT                ! Write a catalogue?
      INTEGER AGIID                   ! AGI identifier
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER FIOD                    ! FIO file descriptor
      INTEGER I                       ! Loop variable
      INTEGER LBND(NDF__MXDIM)        ! Lower limit for image index
      INTEGER NDF1                    ! Identifier for the source NDF
      INTEGER NDIM                    ! Number of dimensions in the
                                      ! image
      INTEGER POINT0(1)               ! Pointer to the data component of
                                      ! the source NDF
      INTEGER POINT1(1)               ! Pointer to the data component after
                                      ! its been mapped to dynamic memory
      INTEGER POINT3(1)               ! Pointer for ARD mask
      INTEGER PRANGE(2)               ! Length of the x and y axes
      INTEGER STLEN                   ! NDF file name length
      INTEGER UBND(NDF__MXDIM)        ! Upper limit for image index
      INTEGER VALIDP                  ! Number of radii for which
                                      ! a fit was obtained
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
      REAL XCO                        ! X index of the galaxy origin
      REAL YCO                        ! Y index of the galxay origin
      REAL ZEROP                      ! Zero point of the surface
                                      ! brightness graphs
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Begin an NDF context.
      CALL NDF_BEGIN
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Obtain an identifier for the NDF structure to be examined.
      CALL NDF_ASSOC('IN','READ',NDF1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the image bounds and also the size of the axes in pixels.
      CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      PRANGE(1)=UBND(1)-LBND(1)+1
      PRANGE(2)=UBND(2)-LBND(2)+1

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

*   Look at another location on the image.
      AGAIN=.TRUE.
      DO WHILE ((AGAIN).AND.(STATUS.EQ.SAI__OK))

*      Get the pixel to be used as the galaxy centre.
         CALL ESP1_INPOS(NDF1,'ORIGIN',XCO,YCO,STATUS)

*      Determine whether or not the origin given is to be used throughout the
*      profiling.
         CALL PAR_GET0L('FRZORI',FRZORI,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the background count value.
         CALL PAR_GET0R('BACK',BACK,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the background count  value.
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

*      Use the default ellipse isophotal separation value?
*      Look on the command line for an output.
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

*      Get the sampling radius.
         CALL PAR_GET0R('RLIM',RLIM,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF (RLIM.LE.PSIZE) RLIM=ELF__RLIM

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

*      Map the NDF data array as _REAL values for reading.
         CALL NDF_MAP(NDF1,'DATA','_REAL','READ',POINT0(1),
     :                ELEMS,STATUS)
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
     :                 PSIZE,RLIM,BACK,SIGMA,ELEMS,POINT1,
     :                 PRANGE,XCO,YCO,VALIDP,RESULT,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Only allow graphical output if VALIDP is greater than 1. Since
*      otherwise there are too few points.
         IF (VALIDP.GT.1) THEN

*         Ask user for device name.
            AGIID=0
            GRAPH=.TRUE.
            CALL ERR_MARK
            CALL ELF1_AGICO(0,0,0,AGIID,STATUS)
            IF (STATUS.NE.SAI__OK) THEN
               GRAPH=.FALSE.
               CALL ERR_ANNUL(STATUS)
            END IF
            CALL ERR_RLSE

*         Display the un-analysed data as a graphical plot of radius (in
*         pixels) versus intensity.
            IF (GRAPH) THEN

               CALL ELF1_GRAPH(PSIZE,ZEROP,RESULT,VALIDP,
     :                         STATUS)
               IF (STATUS.NE.SAI__OK) GOTO 9998

*            Turn off the AGI/PGPLOT interface.
               CALL ELF1_AGICO(1,0,0,AGIID,STATUS)

            END IF

         END IF

*      Create a text file containing the latest profile/fit results
*      (if required).
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

*      Get the user selection of preparing a galaxy profile
*      or not.
         CALL PAR_GET0L('AGAIN',AGAIN,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

         IF (AGAIN) THEN

*         Spacing to make things tidy.
            CALL MSG_BLANK(STATUS)

*         Cancel the parameters so that they must be reinput when
*         looping round.
            CALL ELF1_CANCL(1,STATUS)

         END IF

      END DO

 9999 CONTINUE

*   End the NDF context.
      CALL NDF_END(STATUS)

      END


      SUBROUTINE ELP1_KMODE(STATUS)
*+
*  Name:
*     ELP1_KMODE

*  Purpose:
*     The routine allows the user to input most of the information required
*     (position, background value etc) for the galaxy profile to be
*     performed/displayed and then calls other routines as necessary.
*
*     Information such as pixel size, background count value and its
*     standard deviation are also input.
*
*     The routine operates using a combination of keyboard inputs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELP1_KMODE(STATUS)

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-MAR-1993 (GJP)
*     (Original version)
*     26-OCT-1999 (MBT):
*     Modified to cope with COSYS=C.
*     8-NOV-1999 (MBT):
*     Removed COSYS altogether.

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
      LOGICAL AGAIN                   ! Look at another part of the image?
      LOGICAL ANGCON                  ! Position angle convention
      LOGICAL AUTOL                   ! Is an estimate of the galaxy centre
                                      ! position to be made?
      LOGICAL EXCLAIM                 ! Was an exclaimation mark given for
                                      ! an output file name?
      LOGICAL FAST                    ! Use fast method of profiling?
      LOGICAL FRZORI                  ! Is the galaxy origin frozen?
      LOGICAL GRAPH                   ! Is a graph to be plotted
      LOGICAL INOKAY                  ! Was the most recent input value okay
      LOGICAL WRITECAT		      ! Write a catalogue?
      INTEGER AGIID                   ! AGI identifier
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER FIOD                    ! FIO file descriptor
      INTEGER I                       ! Loop variable
      INTEGER LBND(NDF__MXDIM)        ! Lower limit for image index
      INTEGER MINMOD                  ! Which residual to use in minimisation
      INTEGER NDF1                    ! Identifier for the source NDF
      INTEGER NDIM                    ! Number of dimensions in the
                                      ! image
      INTEGER POINT0(1)               ! Pointer to the data component of
                                      ! the source NDF
      INTEGER POINT1(1)               ! Pointer to the data component after
                                      ! its been mapped to dynamic memory
      INTEGER POINT3(1)               ! Pointer to the ARD mask
      INTEGER PRANGE(2)               ! Length of the x and y axes
      INTEGER STLEN                   ! NDF file name length
      INTEGER UBND(NDF__MXDIM)        ! Upper limit for image index
      INTEGER VALIDP                  ! Number of radii for which
                                      ! a fit was obtained
      REAL ANGOFF                     ! Position angle offset
      REAL BACK                       ! Background count value
      REAL FINE                       ! Determines how closely spaced the
                                      ! chosen radii values are
      REAL FRACT                      ! Fraction of the ellipse points that
                                      ! must be available for the profile
                                      ! at a given radius to be kept
      REAL LIM1                       ! Maximum permitted count increase factor
      REAL LIM2                       ! Lower limit on ellipse count
      REAL LIM3                       ! Radius value beyond which the
                                      ! position angle, origin and ellipticity
                                      ! are no longer adjusted
      REAL PSIZE                      ! Size of the image pixels in arc sec
      REAL RESULT(ELP__NRES,ELP__RESUL)      ! Ellipse parameters
      REAL RLIM                       ! Maximum ellipse radius
      REAL SIGMA                      ! Std. dev. of the background value
      REAL XCO                        ! X index of the galaxy origin
      REAL YCO                        ! Y index of the galxay origin
      REAL ZEROP                      ! Zero point of the surface
                                      ! brightness graphs
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Begin an NDF context.
      CALL NDF_BEGIN
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Obtain an identifier for the NDF structure to be examined.
      CALL NDF_ASSOC('IN','READ',NDF1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the image bounds and also the size of the axes in pixels.
      CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      PRANGE(1)=UBND(1)-LBND(1)+1
      PRANGE(2)=UBND(2)-LBND(2)+1

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

*   Are we to write a catalogue output file?
      CALL PAR_STATE ('OUTCAT',I,STATUS)
      IF (I .EQ. SUBPAR__ACTIVE) THEN
*      Parameter was present on the command line
         WRITECAT = .TRUE.
      ELSE
         WRITECAT = .FALSE.
      ENDIF

*   Look at another location on the image.
      AGAIN=.TRUE.
      DO WHILE ((AGAIN).AND.(STATUS.EQ.SAI__OK))

*      Get the pixel to be used as the galaxy centre.
         CALL ESP1_INPOS(NDF1,'ORIGIN',XCO,YCO,STATUS)

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

*      Use the default radii separation.
*      Look on the command line for an output.
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

*      Get the maximum profile radius.
         CALL PAR_GET0R('RLIM',RLIM,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF (RLIM.LE.PSIZE) RLIM=ELP__RLIM

*      Get the zero point for the surface brightness scale/graphs.
         CALL PAR_GET0R('ZEROP',ZEROP,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Is the galaxy centre to be determined by a centroid around
*      the coords provided?
         CALL PAR_GET0L('AUTOL',AUTOL,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the minimisation mode (which residual to use)
         CALL PAR_GET0I('MINMOD',MINMOD,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Calculate the profiles without using the bi-cubic spline at
*      low radii? Look on the command line for an input.
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

*      Map the NDF data array as _REAL values for reading.
         CALL NDF_MAP(NDF1,'DATA','_REAL','READ',POINT0(1),
     :                ELEMS,STATUS)
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

*      Only allow graphical output if VALIDP is greater than 1. Since
*      otherwise there are too few points.
         IF (VALIDP.GT.1) THEN

*         Ask user for device name.
            AGIID=0
            GRAPH=.TRUE.
            CALL ERR_MARK
            CALL ELP1_AGICO(0,0,0,AGIID,STATUS)
            IF (STATUS.NE.SAI__OK) THEN
               GRAPH=.FALSE.
               CALL ERR_ANNUL(STATUS)
            END IF
            CALL ERR_RLSE

*         Display the un-analysed data as a graphical plot of radius (in
*         pixels) versus intensity.
            IF (GRAPH) THEN

               CALL ELP1_GRAPH(PSIZE,ZEROP,RESULT,VALIDP,
     :                        STATUS)
               IF (STATUS.NE.SAI__OK) GOTO 9998

*            Turn off the AGI/PGPLOT interface.
               CALL ELP1_AGICO(1,0,0,AGIID,STATUS)

            END IF


         END IF

*      Create a text file containing the latest profile/fit results
*      (if required).
         CALL ELP1_TEXTO(0,NDF1,VALIDP,ZEROP,
     :        RESULT,ELP__NRES,ELP__MXPOI,XCO,YCO,BACK,SIGMA,
     :        PSIZE,LBND,.TRUE.,FIOD,EXCLAIM,STATUS)

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

*      Get the user selection of preparing a galaxy profile
*      or not.
         CALL PAR_GET0L('AGAIN',AGAIN,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

         IF (AGAIN) THEN

*         Spacing to make things tidy.
            CALL MSG_BLANK(STATUS)

*         Cancel the parameters so that they must be reinput when
*         looping round.
            CALL ELP1_CANCL(1,STATUS)

         END IF

      END DO

 9999 CONTINUE

*   End the NDF context.
      CALL NDF_END(STATUS)

      END


      SUBROUTINE SEC1_KMODE(STATUS)
*+
*  Name:
*     SEC1_KMODE
*
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
*  Language:
*     Starlink Fortran 77
*
*  Invocation:
*     CALL SEC1_KMODE(STATUS)
*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
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
*
*     This routine operates by keyboard inputs exclusively.
*
*  Implementation Status:
*     Under development
*
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}
*
*  History:
*     16-Nov-1992 (GJP)
*       (Original version)
*
*  Bugs:
*     None known.
*
*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'SEC_PAR'               ! SECTOR constants
      INCLUDE 'SUBPAR_PAR'            ! SUBPAR constant
      INCLUDE 'CNF_PAR'

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      CHARACTER *(256) RADISP         ! Option choice defining how the
                                      ! radius data is to be displayed
*      CHARACTER *(256) STRINP         ! String array for character input
      LOGICAL AGAIN                   ! Look at another part of the image?
      LOGICAL AUTOL                   ! Is an estimate of the galaxy centre
                                      ! position to be made?
      LOGICAL EXCLAIM                 ! File name was '!'?
      LOGICAL GRAPH                   ! Is a graph to be plotted
      LOGICAL INOKAY                  ! Was the most recent input value okay
      LOGICAL MIRROR                  ! Sum the pixels over a single slice
                                      ! or two diametrically opposite slices
      LOGICAL SURF                    ! Pixel values expressed as sigma
                                      ! or surface brightness
      INTEGER AGIID                   ! AGI identifier
      INTEGER COUNT(2)                ! The number of data points used in the
                                      ! scale length regression
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER FIOD2                   ! Output file identifier
      INTEGER FLAG                    ! Can the central pixel value be found?
*      INTEGER IND                     ! The number of origin indices to
                                      ! be input at one go i.e. 2
      INTEGER IND2                    ! Number of indices returned
      INTEGER LBND(NDF__MXDIM)        ! Lower limit for image index
      INTEGER RLIM                    ! The length of the slice to be taken
      INTEGER LEN2                    ! Temporary storage of RLIM
      INTEGER NDF1                    ! Identifier for the source NDF
      INTEGER NDIM                    ! Number of dimensions in the
                                      ! image
      INTEGER NVP                     ! The number of valid data points found
      INTEGER POINT0(1)               ! Pointer to the data component of
                                      ! the source NDF
      INTEGER POINT1(1)               ! Pointer to the data component after
                                      ! its been mapped to dynamic memory
      INTEGER POINT3(1)               ! Pointer to the ARD mask
      INTEGER PRANGE(2)               ! Length of the x and y axes
      INTEGER UBND(NDF__MXDIM)        ! Upper limit for image index
      REAL ANGWID                     ! Angular width of the slice
      REAL BACK                       ! Background count value
      REAL CONS(2)                    ! Constant terms of the curves used
                                      ! to find the scale length
      REAL GRAD(2)                    ! Gradients of the curves used
                                      ! to find out the scale length
      REAL HIR                        ! Highest radius value used in the fit
                                      ! calculated
      REAL INP(2)                     ! Value input by the user
      REAL LOR                        ! Lowest radius value employed in
                                      ! the fit calculated
      REAL NUMBER(SEC__RESUL)         ! The number of pixels found at a given
                                      ! distance from the origin.
      REAL OCOUNT                     ! Pixel count for the origin pixel
      REAL POSANG                     ! Position angle of the slice
      REAL PSIZE                      ! Size of the image pixels in arc sec
      REAL SIGMA                      ! Standard deviation of the background value
      REAL SLEN(2)                    ! Scale length of the galaxy
      REAL SUMMAT(SEC__RESUL)         ! Sum of the pixel counts for all pixels
                                      ! at a given distance from the origin
      REAL TEMP                       ! Temporary storage
      REAL XCO                        ! X index of the sector origin
      REAL YCO                        ! Y index of the sector origin
      REAL ZEROP                      ! Zero point of the surface
                                      ! brightness graphs
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Begin an NDF context.
      CALL NDF_BEGIN
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Obtain an identifier for the NDF structure to be examined.
      CALL NDF_ASSOC('IN','READ',NDF1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the image bounds and also the size of the axes in pixels.
      CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      PRANGE(1)=UBND(1)-LBND(1)+1
      PRANGE(2)=UBND(2)-LBND(2)+1

*   Look at another location on the image.
      AGAIN=.TRUE.
      DO WHILE (AGAIN.AND.(STATUS.EQ.SAI__OK))

*      Get the pixel to be used as the galaxy centre.  Try the PORIGIN
*      parameter first.
         CALL PAR_GET1R('PORIGIN',2,INP,IND2,STATUS)
         IF (STATUS .EQ. SAI__OK) THEN
            XCO=INP(1)
            YCO=INP(2)
         ELSE
*         Parameter wasn't present -- use ORIGIN instead
            CALL ESP1_INPOS(NDF1,'ORIGIN',XCO,YCO,STATUS)
         ENDIF

*      Get the position angle for the sector.
         CALL PAR_GET0R('POSANG',POSANG,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get angular width of the slice.
         CALL PAR_GET0R('ANGWID',ANGWID,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get a value for the length of the slice. 0=automatic.
         CALL PAR_GET0I('RLIM',RLIM,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Are pixels on both sides of the origin to be used.
         CALL PAR_GET0L('MIRROR',MIRROR,STATUS)
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

*      Map the output NDF data array as _REAL values for reading.
         CALL NDF_MAP(NDF1,'DATA','_REAL','READ',POINT0(1),
     :                ELEMS,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Allocate dynamic memory on which to map the NDF.
         CALL PSX_CALLOC(ELEMS,'_REAL',POINT1(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Transfer values from the mapped NDF to the allocated memory.
         CALL SEC1_TRANS(ELEMS,%VAL(CNF_PVAL(POINT0(1))),
     :                   %VAL(CNF_PVAL(POINT1(1))),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Un-map the source NDF. Helps to reduce the resourcse being used.
         CALL NDF_UNMAP(NDF1,'DATA',STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Allocate the memory needed for the logical mask array.
         CALL PSX_CALLOC(ELEMS,'_INTEGER',POINT3(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Transfer to the ARD driver control routine.
         CALL ESP1_ARD_DRIVER(NDIM,ELEMS,LBND,UBND,POINT1,POINT3,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Free the dynamic array space of the logical mask.
         CALL PSX_FREE(POINT3(1),STATUS)
        IF (STATUS.NE.SAI__OK) GOTO 9998

*      Look for a better (though crude) estimate of the galaxy core position.
         CALL SEC1_AUTOL(AUTOL,ELEMS,PRANGE,OCOUNT,FLAG,
     :                   %VAL(CNF_PVAL(POINT1(1))),XCO,YCO,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Call the routine that fills the arrays with the summation of all
*      the data points within the required slice.
         CALL SEC1_PIE(1,BACK,ELEMS,XCO,YCO,PRANGE,POSANG,ANGWID,NVP,
     :                 NUMBER,SUMMAT,RLIM,%VAL(CNF_PVAL(POINT1(1))),
     :                 STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998
         LEN2=RLIM

*      Continue the summation of radius/brightness points using data on
*      the other side of the origin.
         IF (MIRROR) THEN

*          Pass the position angle for the other side of the origin and
*          then repeat the summation.
            TEMP=POSANG+180.

*         Perform the count summation for the opposite side of the object.
            CALL SEC1_PIE(0,BACK,ELEMS,XCO,YCO,PRANGE,TEMP,ANGWID,NVP,
     :                    NUMBER,SUMMAT,RLIM,%VAL(CNF_PVAL(POINT1(1))),
     :                    STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9998

         END IF

*      Ensure that the length value used reflects the highest value found
*      even when MIRROR is true.
         IF (RLIM.LT.LEN2) RLIM=LEN2


*      Only allow graphical output if NVP is greater than 1. Since
*      otherwise there are too few points.
         IF (NVP.GT.1) THEN

*         Ask user for device name.
            AGIID=0
            GRAPH=.TRUE.
            CALL ERR_MARK
            CALL SEC1_AGICO(0,0,0,AGIID,STATUS)
            IF (STATUS.NE.SAI__OK) THEN
               GRAPH=.FALSE.
               CALL ERR_ANNUL(STATUS)
            END IF
            CALL ERR_RLSE

*         Display the un-analysed data as a graphical plot of radius (in
*         some form) versus intensity (in some form).
            IF (GRAPH) THEN
               CALL SEC1_GRAPH(1,ZEROP,RADISP,SURF,RLIM,BACK,NUMBER,
     :                         PSIZE,SIGMA,SUMMAT,CONS,GRAD,STATUS)
               IF (STATUS.NE.SAI__OK) GOTO 9998
            END IF

*         Only allow fitting when more than two data points are present.
            IF (NVP.GT.2) THEN

*            Loop round until two sensible radius values are input.
               INOKAY=.FALSE.
               DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))

*               Get the two values from the keyboard
*                  CALL PAR_GET1R('FITLIM',IND,INP,IND2,STATUS)
                  CALL PAR_GET1R('FITLIM',2,INP,IND2,STATUS)

*               Check that the values were not inadvertantly reversed
*               and swop them round so that the result can be sensible.
                  IF (INP(1).LT.INP(2)) THEN
                     LOR=INP(1)
                     HIR=INP(2)
                  ELSE
                     CALL MSG_BLANK(STATUS)
                     CALL MSG_OUT(' ','WARNING!!!',STATUS)
                     CALL MSG_OUT(' ','The low and high values were'/
     :                            /' swapped.',STATUS)
                     CALL MSG_BLANK(STATUS)
                     LOR=INP(2)
                     HIR=INP(1)
                  END IF

*               Check to see if it is possible for there to be two
*               data points in the radius range required.
                  IF ((HIR-LOR)/PSIZE.LT.2.0) THEN
                     CALL MSG_OUT(' ','The radius range supplied, is '/
     :                           /' too narrow to be used.',STATUS)
                  ELSE
                     INOKAY=.TRUE.
                  END IF

               END DO
               IF (STATUS.NE.SAI__OK) GOTO 9999

*            Calculate the scale length assuming spiral or elliptical.

*            Obtain the 'fit' parameters for linear fits to the brightness
*            versus radius data (suitably transformed).
                CALL SEC1_LINRE(LOR,HIR,NUMBER,SUMMAT,PSIZE,BACK,
     :                          RLIM,GRAD,CONS,COUNT,SLEN,STATUS)
                IF (STATUS.NE.SAI__OK) GOTO 9998

*            Display the fit plots if a graphics device was selected.
               IF (GRAPH) THEN

                  CALL SEC1_GRAPH(2,ZEROP,RADISP,SURF,RLIM,BACK,
     :                            NUMBER,PSIZE,SIGMA,SUMMAT,CONS,
     :                            GRAD,STATUS)
                  IF (STATUS.NE.SAI__OK) GOTO 9998

*               Turn off the AGI/PGPLOT interface.
                  CALL SEC1_AGICO(1,0,0,AGIID,STATUS)

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

 9998    CONTINUE

*      De-allocate the dynamic memory used.
         CALL PSX_FREE(POINT1(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the user selection of preparing a galaxy profile
*      or not.
         CALL PAR_GET0L('AGAIN',AGAIN,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

         IF (AGAIN) THEN

*         Spacing to make things look more tidy.
            CALL MSG_BLANK(STATUS)

*         Cancel the parameters so that they must be reinput when
*         looping round.
            CALL SEC1_CANCL(1,STATUS)

         END IF

      END DO

 9999 CONTINUE

*   End the NDF context.
      CALL NDF_END(STATUS)

      END
