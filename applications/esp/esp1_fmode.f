

      SUBROUTINE ELF1_FMODE(STATUS)
*+
*  Name:
*     ELF1_FMODE
*
*  Purpose:
*     The routine obtains the user inputs required to perform
*     galaxy profiling for a given image. The co-ordinates of the
*     points on the image denoting the suggested galaxy centres are
*     defined via an ASCII text file.
*
*  Language:
*     Starlink Fortran 77
*
*  Invocation:
*     CALL ELF1_FMODE(STATUS)
*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Description:
*     Information such as pixel size, background count value and its
*     standard deviation (SIGMA) etc are input.
*
*     Profiling is continued until the mean profile value is less than
*     a user defined number of standard deviations above sky (LIM2) or
*     until the mean profile value increases by a user defined ratio
*     compared to the previous profile (LIM1). The difference between the
*     sizes of profile semi-major axis values is controlled using the
*     FINE parameter.
*
*     Contaminating parts of the image may be defined using an ARD file.
*
*     The initial estimate of the galaxy position may be improved by
*     modifying the AUTOL option which performs a weighted maximum
*     analysis of the image area immediately surrounding the input value.
*
*  Implementation Status:
*     Under development
*
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     NG: Norman Gray (Starlink, Glasgow)
*
*  History:
*     17-JUL-1993 (GJP)
*     (Original version)
*     21-Nov-1999 (NG)
*       Use new elp1_filer instead of (identical!) elf1_filer
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
      INCLUDE 'ELF_PAR'               ! ELLFOU constants
      INCLUDE 'MSG_PAR'               ! MSG constants
      INCLUDE 'SUBPAR_PAR'            ! SUBPAR constants
      INCLUDE 'PAR_ERR'		      ! PAR constants
      INCLUDE 'CNF_PAR'

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      CHARACTER *(MSG__SZMSG) FILE    ! NDF file name
      LOGICAL ANGCON                  ! Position angle convention
      LOGICAL AUTOL                   ! Is an estimate of the galaxy centre
                                      ! position to be made?
      LOGICAL EXCLAIM                 ! Was the filename an exclaimation
      LOGICAL FILINP                  ! Was a file name input?
      LOGICAL FRZORI                  ! Is the galaxy origin frozen?
      LOGICAL INOKAY                  ! Was the most recent input value okay
      LOGICAL WRITECAT                ! Output a catalogue?
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER FIOD                    ! Output FIO file descriptor
      INTEGER FIOID                   ! Input FIO file descriptor
      INTEGER I                       ! Loop variable
      INTEGER IWCS                    ! AST pointer to NDF's WCS frameset
      INTEGER LBND(NDF__MXDIM)        ! Lower limit for image index
      INTEGER NDF1                    ! Identifier for the source NDF
      INTEGER NDIM                    ! Number of dimensions in the
                                      ! image
      INTEGER NGALS                   ! Number of galaxy centres co-ordinate
                                      ! pairs found in the text file
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
      REAL BACK                       ! Global background count value
      REAL BACKS(ELF__NGALS)          ! Local background values
      REAL FINE                       ! Determines how closely spaced the
                                      ! chosen radii values are
      REAL LIM1                       ! Maximum permitted count increase factor
      REAL LIM2                       ! Lower limit on ellipse count
      REAL PSIZE                      ! Size of the image pixels in arc sec
      REAL RESULT(17,ELF__RESUL)      ! Ellipse parameters
      REAL RLIMDEF                    ! Sampling radius maximum
      REAL RLIM(ELF__NGALS)           ! Maximum ellipse radii
      REAL SIGMA                      ! Standard deviation of the background value
      REAL XC(ELF__NGALS)             ! X co-ordinates of the galaxy positions
                                      ! found from the file
      REAL XCO                        ! X index of the galaxy origin
      REAL YC(ELF__NGALS)             ! Y co-ordinates of the galaxy positions
                                      ! found from the file
      REAL YCO                        ! Y index of the galaxy origin
      REAL ZEROP                      ! Zero point of the surface
                                      ! brightness graphs
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Determine the input text file name.
      CALL FIO_ASSOC('INFILE','READ','LIST',80,FIOID,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (STATUS.EQ.SAI__OK) FILINP=.TRUE.

*   Begin an NDF context.
      CALL NDF_BEGIN
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Begin an AST context.
      CALL AST_BEGIN(STATUS)

*   Obtain an identifier for the NDF structure to be examined.
      CALL NDF_ASSOC('IN','READ',NDF1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the WCS component for the NDF.
      CALL NDF_GTWCS(NDF1,IWCS,STATUS)

*   Are we to write a catalogue output file?
      CALL PAR_STATE ('OUTCAT',I,STATUS)
      IF (I .NE. SUBPAR__NULL) THEN
         WRITECAT = .TRUE.
      ELSE
         WRITECAT = .FALSE.
      ENDIF

*   Get the image bounds and also the size of the axes in pixels.
      CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      PRANGE(1)=UBND(1)-LBND(1)+1
      PRANGE(2)=UBND(2)-LBND(2)+1

*   Determine whether or not the origin given is to be used throughout
*   the profiling.
      CALL PAR_GET0L('FRZORI',FRZORI,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

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

*   Get the global background count value.
      CALL PAR_GET0R('BACK',BACK,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the background count standard deviation value.
      INOKAY=.FALSE.
      DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))
         CALL PAR_GET0R('SIGMA',SIGMA,STATUS)
         IF (SIGMA.LE.0.0) THEN
*         Display message and annul the parameter.
            CALL MSG_OUT(' ','Sigma supplied, is not '//
     :                   'feasible.',STATUS)
            CALL PAR_CANCL('SIGMA',STATUS)
         ELSE
            INOKAY=.TRUE.
         END IF
      END DO
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Should the default ellipse isophotal settings be used.
*   Look on the command line for an input.
*   Otherwise, use the value specified in elf_par.
      CALL PAR_STATE('FINE',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('FINE',FINE,STATUS)
         CALL MSG_OUT(' ','Command line FINE value used.',STATUS)
      ELSE
         FINE=ELF__FINE
      END IF

*   Get the pixel size value.
      CALL ESP1_GTPSZ(NDF1,PSIZE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the sampling radius maximum.
      CALL PAR_GET0R('RLIM',RLIMDEF,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (RLIMDEF.LE.PSIZE) RLIMDEF=ELF__RLIM

*   Get the zero point for the surface brightness scale/graphs.
      CALL PAR_GET0R('ZEROP',ZEROP,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*      Is the galaxy centre to be determined by a weighted search around
*      the coords provided?
         CALL PAR_GET0L('AUTOL',AUTOL,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*   Check the state of the parameter LIM1, to see if there is a
*   suggested value on the command line.
*   Otherwise, use the value specified in elf_par.
      CALL PAR_STATE('LIM1',I, STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('LIM1',LIM1,STATUS)
         CALL MSG_OUT(' ','Command line LIM1 value used.',STATUS)
      ELSE
         LIM1=ELF__LIM1
      END IF

*   Check the state of the parameter LIM2, to see if there is a
*   suggested value on the command line.
*   Otherwise, use the value specified in elf_par.
      CALL PAR_STATE('LIM2',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('LIM2',LIM2,STATUS)
         CALL MSG_OUT(' ','Command line LIM2 value used.',STATUS)
      ELSE
         LIM2=ELF__LIM2
      END IF

*   Obtain the co-ordinates of the galaxies required.
      CALL ELP1_FILER(FIOID,BACK,RLIMDEF,NDF1,NGALS,
     :     XC,YC,BACKS,RLIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Abort if the number of galaxies is zero.
      IF (NGALS.EQ.0) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','No viable data points were found!!!',STATUS)
         CALL MSG_BLANK(STATUS)
         GOTO 9999
      END IF

*   Map the NDF data array as _REAL values for reading.
      CALL NDF_MAP(NDF1,'DATA','_REAL','READ',POINT0(1),
     :             ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the name of the file being mapped.
      CALL NDF_MSG('FILE',NDF1)
      CALL MSG_LOAD(' ','^FILE',FILE,STLEN,STATUS)

*   Allocate dynamic memory on which to map the NDF.
      CALL PSX_CALLOC(ELEMS,'_REAL',POINT1(1),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Transfer values from the mapped NDF to the allocated memory.
      CALL ELF1_TRANS(ELEMS,%VAL(CNF_PVAL(POINT0(1))),
     :                %VAL(CNF_PVAL(POINT1(1))),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Un-map the source NDF. Helps to reduce the resources being used.
      CALL NDF_UNMAP(NDF1,'DATA',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Allocate the memory needed for the logical mask array.
      CALL PSX_CALLOC(ELEMS,'_INTEGER',POINT3(1),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Transfer to the ARD driver control routine.
      NDIM=2
      CALL ESP1_ARD_DRIVER(NDIM,ELEMS,LBND,UBND,POINT1,POINT3,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Free the dynamic array space of the logical mask.
      CALL PSX_FREE(POINT3(1),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Open a text output file.
      CALL ELP1_TEXTO(1,NDF1,VALIDP,ZEROP,
     :     RESULT,17,ELF__MXPOI,XCO,YCO,BACK,SIGMA,
     :     PSIZE,LBND,.FALSE.,FIOD,EXCLAIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   If requested, open a catalogue output file, too
      IF (WRITECAT) THEN
         CALL ESP1_CATO(1, NDF1, 0, 0.0,
     :        RESULT, 0, 0, 0.0, 0.0, BACK,
     :        0.0, 0.0, 0, .FALSE., STATUS)
      ENDIF

*   Look at each of the image location found in the text file.
      CALL MSG_BLANK(STATUS)
      DO 20 I=1,NGALS

*      Read the stored array values.
         XCO=XC(I)
         YCO=YC(I)

*      Display the current co-ordinates.
         CALL ESP1_XYFMT(IWCS,XCO,YCO,'X','Y','DOM',STATUS)
         CALL MSG_OUT(' ','Working on ^DOM co-ordinates:  ^X  ^Y',
     :                STATUS)

*      Look for a better (though crude) estimate of the galaxy core position.
         IF (AUTOL) CALL ELF1_AUTOL(ELEMS,PRANGE,BACK,
     :                              %VAL(CNF_PVAL(POINT1(1))),
     :                              XCO,YCO,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Call the routine that profiles the galaxy and sets up the values
*      in the results arrays.
*         CALL ELF1_PRO(0,ANGCON,ANGOFF,FRZORI,FINE,LIM2,
         CALL ELF1_PRO(1,ANGCON,ANGOFF,FRZORI,FINE,LIM2,
     :                 PSIZE,RLIM(I),BACKS(I),SIGMA,ELEMS,POINT1,
     :                 PRANGE,XCO,YCO,VALIDP,RESULT,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Place in the opened file the heading, the co-ordinates of the galaxy
*      being considered and the profiling results.
         IF (.NOT.EXCLAIM) THEN
            CALL ELP1_TEXTO(2,NDF1,VALIDP,ZEROP,
     :           RESULT,17,ELF__MXPOI,XCO,YCO,BACKS(I),
     :           SIGMA,PSIZE,LBND,.FALSE.,FIOD,EXCLAIM,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9998
         END IF

         IF (WRITECAT) THEN
            CALL ESP1_CATO(2, NDF1, VALIDP, ZEROP,
     :           RESULT, 17, ELF__MXPOI, XCO, YCO, BACK,
     :           SIGMA, PSIZE, LBND, .FALSE.,STATUS)
         ENDIF

*      Tell the user what happened.
         IF (VALIDP.LT.1) THEN
            CALL MSG_OUT(' ','FAILED!!!',STATUS)
         ELSE
            IF (VALIDP.GT.0) THEN
               CALL MSG_FMTI('FOUND','I4',VALIDP)
               CALL MSG_OUT(' ','^FOUND ellipses determined.',STATUS)
            END IF
         END IF

 20   CONTINUE

*   Close the opened file.
      IF (.NOT.EXCLAIM) THEN
         CALL ELP1_TEXTO(3,NDF1,VALIDP,ZEROP,
     :        RESULT,17,ELF__MXPOI,XCO,YCO,BACKS(I),
     :        SIGMA,PSIZE,LBND,.FALSE.,FIOD,EXCLAIM,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998
      END IF

      IF (WRITECAT) THEN
         CALL ESP1_CATO(3, NDF1, 0, 0.0,
     :        RESULT, 0, 0, 0.0, 0.0, BACK,
     :        0.0, 0.0, 0, .FALSE., STATUS)
      ENDIF

*   An appropriate place to exit to if the dynamic memory has already
*   been allocated.

 9998 CONTINUE

*   De-allocate the dynamic memory used.
      CALL PSX_FREE(POINT1(1),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

*   Close the file input ASCII files.
      CALL FIO_ANNUL(FIOID,STATUS)

*   End the AST context.
      CALL AST_END(STATUS)

*   End the NDF context.
      CALL NDF_END(STATUS)

      END




      SUBROUTINE ELP1_FMODE(STATUS)
*+
*  Name:
*     ELP1_FMODE
*
*  Purpose:
*     The routine obtains the user inputs required to perform
*     galaxy profiling for a given image. The co-ordinates of the
*     points on the image denoting the suggested galaxy centres are
*     defined via an ASCII text file.
*
*  Language:
*     Starlink Fortran 77
*
*  Invocation:
*     CALL ELP1_FMODE(STATUS)
*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Description:
*     Information such as pixel size, background count value and its
*     standard deviation etc are input.
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
*     Contaminating parts of the image may be defined using an ARD file.
*
*     The initial estimate of the galaxy position may be improved by
*     modifying the AUTOL option which calculate the centroid.
*     of the image area immediately surrounding the input value.
*
*  Implementation Status:
*     Under development
*
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     NG: Norman Gray (Starlink, Glasgow)
*     {enter_new_authors_here}
*
*  History:
*     17-May-1992 (GJP)
*       (Original version)
*     21-Nov-1999 (NG)
*       Use new elp1_filer instead of (identical!) elf1_filer
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
      INCLUDE 'ELP_PAR'               ! ELLPRO constants
      INCLUDE 'MSG_PAR'               ! MSG constants
      INCLUDE 'SUBPAR_PAR'            ! SUBPAR constants
      INCLUDE 'CNF_PAR'

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      CHARACTER *(MSG__SZMSG) FILE    ! NDF file name
      LOGICAL ANGCON                  ! Position angle convention
      LOGICAL AUTOL                   ! Is an estimate of the galaxy centre
                                      ! position to be made?
      LOGICAL EXCLAIM                 ! Was an exclaimation mark given for
                                      ! an output file name?
      LOGICAL FAST                    ! Use fast method of profiling?
      LOGICAL FILINP                  ! Was a file name input?
      LOGICAL FRZORI                  ! Is the galaxy origin frozen?
      LOGICAL INOKAY                  ! Was the most recent input value okay
                                      ! scale length regression
      LOGICAL WRITECAT                ! Output a catalogue?
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER FIOD                    ! Output FIO file descriptor
      INTEGER FIOID                   ! Input FIO file descriptor
      INTEGER I                       ! Loop variable
      INTEGER IWCS                    ! AST pointer to NDF's WCS frameset
      INTEGER LBND(NDF__MXDIM)        ! Lower limit for image index
      INTEGER MINMOD                  ! Which residual to use in minimisation
      INTEGER NDF1                    ! Identifier for the source NDF
      INTEGER NDIM                    ! Number of dimensions in the
                                      ! image
      INTEGER NGALS                   ! Number of galaxy centres co-ordinate
                                      ! pairs found in the text file
      INTEGER POINT0(1)               ! Pointer to the data component of
                                      ! the source NDF
      INTEGER POINT1(1)               ! Pointer to the data component after
                                      ! its been mapped to dynamic memory
      INTEGER POINT3(1)               ! Pointer to ARD mask
      INTEGER PRANGE(2)               ! Length of the x and y axes
      INTEGER STLEN                   ! NDF file name length
      INTEGER UBND(NDF__MXDIM)        ! Upper limit for image index
      INTEGER VALIDP                  ! Number of radii for which
                                      ! a fit was obtained
      REAL ANGOFF                     ! Position angle offset
      REAL BACK                       ! Global background count value
      REAL BACKS(ELP__NGALS)          ! Local background values
      REAL FINE                       ! Determines how closely spaced the
                                      ! chosen radii values are
      REAL FRACT                      ! Limit to fraction of bad points in
                                      ! an ellipse
      REAL LIM1                       ! Maximum permitted count increase factor
      REAL LIM2                       ! Lower limit on ellipse count
      REAL LIM3                       ! Radius at which the galaxy position
                                      ! angle, centre and ellipticity are
                                      ! frozen
      REAL PSIZE                      ! Size of the image pixels in arc sec
      REAL RESULT(ELP__NRES,ELP__RESUL)      ! Ellipse parameters
      REAL DEFRLIM                    ! Default maximum ellipse radius
      REAL RLIM(ELP__NGALS)           ! Maximum ellipse radii
      REAL SIGMA                      ! Std. dev. of the background value
      REAL XC(ELP__NGALS)             ! X co-ordinates of the galaxy positions
                                      ! found from the file
      REAL XCO                        ! X index of the galaxy origin
      REAL YC(ELP__NGALS)             ! Y co-ordinates of the galaxy positions
                                      ! found from the file
      REAL YCO                        ! Y index of the galaxy origin
      REAL ZEROP                      ! Zero point of the surface
                                      ! brightness graphs
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Determine the input text file name.
      CALL FIO_ASSOC('INFILE','READ','LIST',80,FIOID,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (STATUS.EQ.SAI__OK) FILINP=.TRUE.

*   Begin an NDF context.
      CALL NDF_BEGIN
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Begin an AST context.
      CALL AST_BEGIN(STATUS)

*   Obtain an identifier for the NDF structure to be examined.
      CALL NDF_ASSOC('IN','READ',NDF1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the WCS frameset for the NDF.
      CALL NDF_GTWCS(NDF1,IWCS,STATUS)

*   Are we to write a catalogue output file?
      CALL PAR_STATE ('OUTCAT',I,STATUS)
      IF (I .EQ. SUBPAR__ACTIVE) THEN
*      Parameter was present on the command line
         WRITECAT = .TRUE.
      ELSE
         WRITECAT = .FALSE.
      ENDIF

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

*   Determine whether or not the origin given is to be used throughout
*   the profiling.
      CALL PAR_GET0L('FRZORI',FRZORI,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the global background count value.
      CALL PAR_GET0R('BACK',BACK,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the background count std. dev. value.
      INOKAY=.FALSE.
      DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))
         CALL PAR_GET0R('SIGMA',SIGMA,STATUS)
         IF (SIGMA.LE.0.0) THEN
*         Display message and annul the parameter.
            CALL MSG_OUT(' ','Sigma supplied, is not '//
     :                   'feasible.',STATUS)
            CALL PAR_CANCL('SIGMA',STATUS)
         ELSE
            INOKAY=.TRUE.
         END IF
      END DO
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Should the default radii settings be used.
*   Look on the command line for an input.
*   Otherwise, use the value specified in elp_par.
      CALL PAR_STATE('FINE',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('FINE',FINE,STATUS)
         CALL MSG_OUT(' ','Command line FINE value used.',STATUS)
      ELSE
         FINE=ELP__FINE
      END IF

*   Get the pixel size value.
      CALL ESP1_GTPSZ(NDF1,PSIZE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the maximum profile radius.
      CALL PAR_GET0R('RLIM',DEFRLIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (DEFRLIM.LE.PSIZE) DEFRLIM=ELP__RLIM

*   Get the zero point for the surface brightness scale/graphs.
      CALL PAR_GET0R('ZEROP',ZEROP,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*      Is the galaxy centre to be determined by a centroid around
*      the coords provided?
         CALL PAR_GET0L('AUTOL',AUTOL,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*   Calculate the profiles without using the bi-cubic spline at
*   low radii? Look on the command line for an input.
*   Otherwise, use the value specified in elp_par.
      CALL PAR_STATE('FAST',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0L('FAST',FAST,STATUS)
         CALL MSG_OUT(' ','Command line FAST value used.',STATUS)
      ELSE
         FAST=ELP__FAST
      END IF

*   Check the state of the parameter LIM1, to see if there is a
*   suggested value on the command line.
*   Otherwise, use the value specified in elp_par.
      CALL PAR_STATE('LIM1',I, STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('LIM1',LIM1,STATUS)
         CALL MSG_OUT(' ','Command line LIM1 value used.',STATUS)
      ELSE
         LIM1=ELP__LIM1
      END IF

*   Check the state of the parameter LIM2, to see if there is a
*   suggested value on the command line.
*   Otherwise, use the value specified in elp_par.
      CALL PAR_STATE('LIM2',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('LIM2',LIM2,STATUS)
         CALL MSG_OUT(' ','Command line LIM2 value used.',STATUS)
      ELSE
         LIM2=ELP__LIM2
      END IF

*   Check the state of the parameter LIM3, to see if there is a
*   suggested value on the command line.
*   Otherwise, use the value specified in elp_par.
      CALL PAR_STATE('LIM3',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('LIM3',LIM3,STATUS)
         CALL MSG_OUT(' ','Command line LIM3 value used.',STATUS)
      ELSE
         LIM3=ELP__LIM3
      END IF

*   Check the state of the parameter FRACT, to see if there is a
*   suggested value on the command line.
*   Otherwise, use the value specified in elp_par.
      CALL PAR_STATE('FRACT',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('FRACT',FRACT,STATUS)
         CALL MSG_OUT(' ','Command line FRACT value used.',STATUS)
      ELSE
         FRACT=ELP__FRACT
      END IF

*   Get the minimisation mode (which residual to use)
      CALL PAR_GET0I('MINMOD',MINMOD,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Obtain the co-ordinates of the galaxies required.
      CALL ELP1_FILER(FIOID,BACK,DEFRLIM,NDF1,NGALS,
     :     XC,YC,BACKS,RLIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Abort if the number of galaxies is zero.
      IF (NGALS.EQ.0) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','No viable data points were found!!!',STATUS)
         CALL MSG_BLANK(STATUS)
         GOTO 9999
      END IF

*   Map the NDF data array as _REAL values for reading.
      CALL NDF_MAP(NDF1,'DATA','_REAL','READ',POINT0(1),
     :             ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the name of the file being mapped.
      CALL NDF_MSG('FILE',NDF1)
      CALL MSG_LOAD(' ','^FILE',FILE,STLEN,STATUS)

*   Allocate dynamic memory on which to map the NDF.
      CALL PSX_CALLOC(ELEMS,'_REAL',POINT1(1),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Transfer values from the mapped NDF to the allocated memory.
      CALL ELP1_TRANS(ELEMS,%VAL(CNF_PVAL(POINT0(1))),
     :                %VAL(CNF_PVAL(POINT1(1))),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Un-map the source NDF. Helps to reduce the resources being used.
      CALL NDF_UNMAP(NDF1,'DATA',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Allocate the memory needed for the logical mask array.
      CALL PSX_CALLOC(ELEMS,'_INTEGER',POINT3(1),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Transfer to the ARD driver control routine.
      NDIM=2
      CALL ESP1_ARD_DRIVER(NDIM,ELEMS,LBND,UBND,POINT1,POINT3,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Free the dynamic array space of the logical mask.
      CALL PSX_FREE(POINT3(1),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Open a text output file.
      CALL ELP1_TEXTO(1,NDF1,VALIDP,ZEROP,
     :     RESULT,ELP__NRES,ELP__MXPOI,XCO,YCO,BACK,
     :     SIGMA,PSIZE,LBND,.TRUE.,FIOD,EXCLAIM,STATUS)

*   If requested, open a catalogue output file, too
      IF (WRITECAT) THEN
         CALL ESP1_CATO(1, NDF1, 0, 0.0,
     :        RESULT, 0, 0, 0.0, 0.0, BACK,
     :        0.0, 0.0, 0, .TRUE., STATUS)
      ENDIF

      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Look at each of the image location found in the text file.
      CALL MSG_BLANK(STATUS)
      DO 20 I=1,NGALS

*      Read the stored array values.
         XCO=XC(I)
         YCO=YC(I)

*      Display the current co-ordinates.
         CALL ESP1_XYFMT(IWCS,XCO,YCO,'X','Y','DOM',STATUS)
         CALL MSG_OUT(' ','Working on ^DOM co-ordinates:  ^X  ^Y',
     :                STATUS)

*      Look for a better (though crude) estimate of the galaxy core position.
         IF (AUTOL) CALL ELP1_AUTOL(BACK,ELEMS,PRANGE,
     :                              %VAL(CNF_PVAL(POINT1(1))),
     :                              XCO,YCO,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Call the routine that profiles the galaxy and sets up the values
*      in the results arrays.
         CALL ELP1_PRO(0,MINMOD,ANGCON,ANGOFF,FRZORI,FINE,
     :                 LIM1,LIM2,LIM3,
     :                 FRACT,PSIZE,FAST,RLIM(I),BACKS(I),SIGMA,ELEMS,
     :                 POINT1,PRANGE,XCO,YCO,VALIDP,RESULT,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Place in the opened file a heading, the co-ordinates of the galaxy being
*      considered and the profiling results.
         IF (.NOT.EXCLAIM) THEN
            CALL ELP1_TEXTO(2,NDF1,VALIDP,ZEROP,
     :           RESULT,ELP__NRES,ELP__MXPOI,XCO,YCO,
     :           BACKS(I),SIGMA,PSIZE,LBND,.TRUE.,
     :           FIOD,EXCLAIM,STATUS)
         END IF

         IF (WRITECAT) THEN
            CALL ESP1_CATO(2, NDF1, VALIDP, ZEROP,
     :           RESULT, ELP__NRES, ELP__MXPOI, XCO, YCO, BACK,
     :           SIGMA, PSIZE, LBND, .TRUE.,STATUS)
         ENDIF
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Tell the user what happened.
         IF (VALIDP.LT.1) THEN
            CALL MSG_OUT(' ','FAILED!!! No valid ellipses.',STATUS)
         ELSE
            IF (VALIDP.GT.0) THEN
               CALL MSG_FMTI('FOUND','I4',VALIDP)
               CALL MSG_OUT(' ','^FOUND ellipses determined.',STATUS)
            END IF
         END IF

 20   CONTINUE

*   Close the opened file.
      IF (.NOT.EXCLAIM) THEN
*      The only required arguments in mode=3 are mode, fiod and status, so
*      pass suitably-typed dummys for the others.  This would work as
*      well if we passed meaningful values, but doing it this way
*      seems safer.
        CALL ELP1_TEXTO(3,0,0,0.0,RESULT,0,0,0.0,0.0,0.0,
     :        0.0,0.0,0,.TRUE.,FIOD,.FALSE.,STATUS)
      END IF

      IF (WRITECAT) THEN
         CALL ESP1_CATO(3, NDF1, 0, 0.0,
     :        RESULT, 0, 0, 0.0, 0.0, BACK,
     :        0.0, 0.0, 0, .TRUE., STATUS)
      ENDIF
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   An appropriate place to exit to if the dynamic memory has already
*   been allocated.

 9998 CONTINUE

*   De-allocate the dynamic memory used.
      CALL PSX_FREE(POINT1(1),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

*   Close the file input ASCII files.
      CALL FIO_ANNUL(FIOID,STATUS)

*   End the AST context.
      CALL AST_END(STATUS)

*   End the NDF context.
      CALL NDF_END(STATUS)

C*   Debug the HDS system
C      CALL HDS_SHOW ('FILES', STATUS)
C      CALL HDS_SHOW ('LOCATORS', STATUS)

      END


      SUBROUTINE GAU1_FMODE(STATUS)
*+
*  Name:
*     GAU1_FMODE

*  Purpose:
*     The routine obtains the user inputs required to perform
*     source profiling for a given image. The co-ordinates of the
*     points on the image denoting the suggested source centres are
*     defined via an ASCII text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_FMODE(STATUS)

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Reads a text file to find the source coordinates.
*     The image file is read to determine the size of the image.

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
*     17-May-1992 (GJP)
*     (Original version)
*     04-JUN-1998 (NG)
*     Add psize parameter to control pixel sizes, and display of FWHM/sigma.
*     Incorporate routines in gaufit2.f

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
      include 'PAR_ERR'
      include 'GAU_PAR'
      INCLUDE 'CNF_PAR'

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      CHARACTER *(256) MODTYC         ! Type of output image
      integer modtyp                  ! ...as integer
      LOGICAL ANGCON                  ! Position angle convention
      LOGICAL AUTOL                   ! Better origin?
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER FIOID                   ! FIO file descriptor
      INTEGER LBND(NDF__MXDIM)        ! Lower limit for image index
      INTEGER NDF1                    ! Identifier for the source NDF
      INTEGER NDF2                    ! Output image identifier
      INTEGER NDIM                    ! Number of dimensions in the
                                      ! image
      INTEGER NITER                   ! Number of iterations
      INTEGER NSOUR                   ! Number of sources
      INTEGER POINT(6)                ! Pointers to the image data
                                      ! and workspace
      INTEGER PRANGE(2)               ! Length of the x and y axes
      INTEGER MODEL                   ! Pointer to output image
      INTEGER UBND(NDF__MXDIM)        ! Upper limit for image index
      INTEGER UPIX                    ! Number of good pixels in the output
                                      ! image
      integer i,j                     ! temporary variables
      REAL ANGOFF                     ! Position angle offset
      REAL ANGINC                     ! Size of angle variation during minimisation
      REAL PSIZE		      ! pixel size in arcsec
      REAL BACK                       ! Background count value
      REAL HINT(4,10)                 ! User angle, Sa, Sb and peak values

      REAL GUESS(10,7)                ! Initial source parameters
      real guesserrs(10,7)
      REAL NSIGMA                     ! Pixel count Threshold
      REAL PINC                       ! Size of peak variation during minimisation
      REAL RLIM(10)                   ! Source maximum size
      REAL SAINC                      ! Size of change factor in Sa
      REAL SBINC                      ! Size of change factor in Sb
      REAL SIGMA                      ! Std deviation of the background value
      REAL XCO(10,2)                  ! X index of the source origin
      REAL XINC                       ! Size of X movement in minimisation
      REAL YCO(10,2)                  ! Y index of the source origin
      REAL YINC                       ! Size of Y movement in minimisation
      logical fwhm                    ! Work in FWHM, rather than sigma
      logical lsqfit                  ! Use the NSG fit method, not original
      logical calcsd                  ! calculate and display SD errors (LSQ)
      logical fitback                 ! background to be fitted, not given
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Determine the input text file name.
      CALL FIO_ASSOC('INFILE','READ','LIST',80,FIOID,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

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

*   Look at the value for PSIZE.
*   Require that the pixel size be greater than 1 micro-as, as we use
*   psize<1e-6 to mean `undefined', so that the application will display
*   sizes in pixels, rather than arcsec.  A NULL value (`!') indicates
*   sizes should be shown in pixels.
*   Otherwise, use the default.
      CALL ERR_MARK
      CALL ESP1_GTPSZ (NDF1, PSIZE, STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
         PSIZE = 1E-7
         CALL ERR_ANNUL (STATUS)
      END IF
      CALL ERR_RLSE

*   Do we work in FWHM or sigma?  (see psize in gau1_cmode for discussion)
      CALL PAR_GET0L ('FWHM', FWHM, STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (.NOT.FWHM) PSIZE = -PSIZE

*   Obtain the co-ordinates of the sources required.
      CALL GAU1_FILER(FIOID,NDF1,NSOUR,XCO,YCO,RLIM,HINT,FWHM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Abort if the number of sources is zero.
      IF (NSOUR.EQ.0) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','No viable data points were found!',STATUS)
         CALL MSG_BLANK(STATUS)
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

*   Indicate that the maximum number of sources has been reached.
      IF(NSOUR.EQ.10) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','WARNING!',STATUS)
         CALL MSG_OUT(' ',
     :    'Only 10 sources allowed. Extras rejected.',STATUS)
      END IF

*   Look at the command line value for ANGCON.
*   Otherwise, use the default.
      CALL PAR_GET0L('ANGCON',ANGCON,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Look at the value for ANGOFF.
*   Otherwise, use the default.
      CALL PAR_GET0R('ANGOFF',ANGOFF,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

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
     :        'Background count value ( < 0 to have it fitted)', status)
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

*   Get the background count standard deviation (only if BACK was specified).
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

*   Propogate the bits of the source NDF required.
      CALL NDF_PROP(NDF1,'DATA,WCS','MODEL',NDF2,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the type of image to be created and convert to upper case.
      if (lsqfit) then
         call par_promt ('MODTYP',
     :      'Whole image model (W)/Residuals (R)/reGression diag. (G)',
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

*   Map the input NDF data array as _REAL values for reading.
      CALL NDF_MAP(NDF1,'DATA','_REAL','READ',POINT(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Allocate dynamic memory on which to map the NDF.
      CALL PSX_CALLOC(ELEMS,'_REAL',POINT(2),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Allocate more dynamic memory on which to map the NDF.
      CALL PSX_CALLOC(ELEMS,'_REAL',POINT(3),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Transfer values from the mapped NDF to the allocated memory.
      CALL GAU1_TRANS(NSOUR,BACK,SIGMA,NSIGMA,ELEMS,
     :                %VAL(CNF_PVAL(POINT(1))),
     :                %VAL(CNF_PVAL(POINT(2))),XCO,YCO,RLIM,PRANGE,
     :                UPIX,GUESS,STATUS)
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

*   Map the output NDF data array as _REAL values for updating.
      CALL NDF_MAP(NDF2,'DATA','_REAL','UPDATE',MODEL,ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

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

*      Look for a better (though crude) estimate of the source core
*      position.
         CALL PAR_GET0L('AUTOL',AUTOL,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF (AUTOL) CALL GAU1_AUTOL(NSOUR,ELEMS,PRANGE,
     :                              %VAL(CNF_PVAL(POINT(2))),
     :                              XCO,YCO,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Get an estimate for the first position angles, sigmas and
*      peak values to try.
         CALL GAU1_GUESS(NSOUR,ANGCON,ANGOFF,PSIZE,SIGMA,NSIGMA,BACK,
     :        XCO,YCO,RLIM,
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

*   Store the output image.
      CALL GAU1_OUTIM(ELEMS,%VAL(CNF_PVAL(POINT(3))),
     :                %VAL(CNF_PVAL(MODEL)),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Output a text file containing results.
      CALL GAU1_TEXTO(NSOUR,ANGCON,ANGOFF,PSIZE,LBND,NDF1,GUESS,
     :     guesserrs, BACK, fitback, SIGMA,lsqfit,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   An appropriate place to exit to if the dynamic memory has already
*   been allocated

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


      SUBROUTINE GRA1_FMODE(STATUS)
*+
*  Name:
*     GRA1_FMODE

*  Purpose:
*     Performs scale length calculations on profile data
*     contained in an input text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRA1_FMODE(STATUS)

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Reads through an input text file and identifies valid ESP
*     format files created by ELLFOU, ELLPRO or SECTOR.
*
*     Processes the information it finds in such a way as to
*     determine the scale lnegth of the profiles it finds.
*     the result generated is output to another text file
*     with a suitable header.


*  Implementation Status:
*     Under development

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     06-OCT-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'GRA_PAR'               ! GRAPHS constants
      INCLUDE 'SUBPAR_PAR'            ! SUBPAR constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      CHARACTER *(80) CURCO           ! Current coordinates of galaxy centre
      CHARACTER *(80) FILEN           ! Image on which the galaxy was found
      CHARACTER *(80) FILEN2          ! Temporary storage for FILEN
      CHARACTER *(3) FTYPE            ! The record type being read
                                      ! radius data is to be displayed
      LOGICAL ANGCON                  ! Position angle convention
      LOGICAL EXCLAIM                 ! Is the file name a !?
      LOGICAL RRANGE                  ! Is the limit selection automatic?
      LOGICAL INOKAY                  ! Was the most recent input value okay
      LOGICAL OPENF                   ! Was the text output file opened okay?
      INTEGER FIOD2                   ! File identifier
      INTEGER NUMB                    ! The number of data points within
                                      ! the given radius range
      INTEGER NUMBP(2)                ! The number of data points used in the
                                      ! scale length regression
      INTEGER FIOID                   ! FIO file descriptor
      INTEGER FLAG                    ! Can the central pixel value be found?
      INTEGER FSTAT                   ! Has the file end been reached?
      INTEGER I                       ! Temporary storage
      INTEGER IND                     ! The number of origin indices to
                                      ! be input at one go i.e. 2
      INTEGER IND2                    ! Number of indices returned
      INTEGER POINTS                  ! The number of valid data points found
      REAL ANGOFF                     ! Position angle offsets
      REAL BACK                       ! Background count value
      REAL CONS(2)                    ! Constant terms of the curves used
                                      ! to find the scale length
      REAL GRAD(2)                    ! Gradients of the curves used
                                      ! to find out the scale length
      REAL HIR                        ! Highest radius value used in the fit
                                      ! calculated
      REAL HIRPERM                    ! Permanent version of HIR
      REAL INP(2)                     ! Value input by the user
      REAL LOR                        ! Lowest radius value employed in
                                      ! the fit calculated
      REAL LORPERM                    ! Permanent version of LOR
      REAL LOWLIM                     ! Lower fit radius limit
      REAL PSIZE                      ! Size of the image pixels in arc sec
      REAL REG(2)                     ! Regression coefficient squared
      REAL SIGMA                      ! Standard deviation of the background value
      REAL SLEN(2)                    ! Scale length of the galaxy
      REAL RESULT(GRA__RESUL,17)      ! Sum of the pixel counts for all pixels
                                      ! at a given distance from the origin
      REAL XCO                        ! X index of the sector origin
      REAL YCO                        ! Y index of the sector origin
      REAL ZEROP                      ! Zero point of the surface
                                      ! brightness graphs
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Determine the input ELLPRO, SECTOR or ELLFOU file name.
      CALL FIO_ASSOC('INFILE','READ','LIST',80,FIOID,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Look at the command line value for LOWLIM.
*   Otherwise, use the default.
      CALL PAR_STATE('LOWLIM',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('LOWLIM',LOWLIM,STATUS)
         CALL MSG_OUT(' ','Command line LOWLIM value used.',STATUS)
      ELSE
         LOWLIM=0.5
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

*   Is the radius range selection to be automatic.
      CALL PAR_GET0L('RRANGE',RRANGE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the radius range limits from the user.
      IF (.NOT.RRANGE) THEN

*      Get the two values from the keyboard.
         IND=2
         CALL PAR_GET1R('FITLIM',IND,INP,IND2,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Check that the values were not inadvertantly reversed
*      and swop them round so that the result can be sensible.
         IF (INP(1).LT.INP(2)) THEN
             LORPERM=INP(1)
             HIRPERM=INP(2)
         ELSE
             LORPERM=INP(2)
             HIRPERM=INP(1)
         END IF

      ELSE

*      Set arbitrary low and high limits for the scale length calculation.
         LORPERM=0
         HIRPERM=1E5

      END IF
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Create a text file containing the latest profile/fit results.
      CALL GRA1_TEXTO(0,FILEN,REG,XCO,YCO,CURCO,SLEN,CONS,ZEROP,
     :                OPENF,FIOD2,EXCLAIM,STATUS)
      FILEN2=' '

*   Look at all the entries in the text file.
      FSTAT=-1
      FLAG=0
      DO WHILE (FLAG.LT.1)

*      Obtain the results from the file.
         CALL GRA1_FILER(FSTAT,FIOID,FLAG,POINTS,RESULT,
     :                   BACK,SIGMA,PSIZE,XCO,YCO,CURCO,
     :                   FILEN,FTYPE,ZEROP,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Only continue if you are sure everything is okay.
         IF(FSTAT.EQ.0) THEN

*      Set up the radius limits for this loop.
            LOR=LORPERM
            HIR=HIRPERM

*         Check to see if it is possible for there to be two
*         data points in the radius range required.
            CALL GRA1_RANGE(POINTS,PSIZE,RESULT,LOR,HIR,NUMB,STATUS)
            IF (NUMB.GT.2) INOKAY=.TRUE.

*         Clear the values for scale length, regression coeff,
*         gradient and slope.
            DO 27 I=1,2
               SLEN(I)=0.0
               CONS(I)=0.0
               GRAD(I)=0.0
               REG(I)=0.0
 27         CONTINUE

*         Only allow fitting when more than two data points are present.
            IF (INOKAY) THEN

*            Calculate the scale length assuming spiral or elliptical.

*            Obtain the 'fit' parameters for linear fits to the brightness
*            versus radius data (suitably transformed).
                CALL GRA1_LINRE(POINTS,LOWLIM,FTYPE,RRANGE,LOR,
     :                          HIR,RESULT,PSIZE,GRAD,CONS,SLEN,
     :                          NUMBP,REG,STATUS)
                IF (STATUS.NE.SAI__OK) GOTO 9999

*            Display the results on the default display.
               CALL GRA1_TEXTD(FLAG,POINTS,XCO,YCO,CURCO,BACK,SIGMA,
     :                         CONS,GRAD,PSIZE,NUMBP,ZEROP,SLEN,
     :                         LOR,HIR,REG,STATUS)

*            Create a text file header (if the filename has changed).
               IF (FILEN.NE.FILEN2) THEN
                  CALL GRA1_TEXTO(1,FILEN,REG,XCO,YCO,CURCO,SLEN,
     :                            CONS,ZEROP,OPENF,
     :                            FIOD2,EXCLAIM,STATUS)
                  FILEN2=FILEN
               END IF

*            Store the latest profile/fit results
               IF (.NOT.EXCLAIM)  THEN
                  CALL GRA1_TEXTO(2,FILEN,REG,XCO,YCO,CURCO,SLEN,
     :                            CONS,ZEROP,OPENF,FIOD2,
     :                            EXCLAIM,STATUS)
               END IF

            ELSE
               CALL MSG_BLANK(STATUS)
               CALL MSG_OUT(' ','Too few isophotes to calculate'//
     :                      ' scale length.',STATUS)
               CALL MSG_BLANK(STATUS)
            END IF

         END IF

      END DO

*   Close the results file.
      IF (.NOT.EXCLAIM) THEN
         CALL GRA1_TEXTO(3,FILEN,REG,XCO,YCO,CURCO,SLEN,
     :                   CONS,ZEROP,OPENF,
     :                   FIOD2,EXCLAIM,STATUS)
      END IF

 9999 CONTINUE

      END
