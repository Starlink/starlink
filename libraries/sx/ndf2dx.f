       SUBROUTINE NDF2DX( STATUS )

*+
*  Name:
*     NDF2DX

*  Purpose:
*     Converts a 1,2 or 3-D NDF into a file compatible with
*     IBM's Data Explorer (DX) format.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Allows the user to input the name of an NDF image file and
*     then constructs an equivalent DX file.
*
*     Bad pixels are represented by their own object.

*  Usage:
*     NDF2DX IN OUT (BMODE) (DEP) (PVAR) (PBAD) (AXES)

*  ADAM Parameters:
*     IN = _NDF (Read)
*        The name of the NDF data structure/file that is to be
*        converted.
*     OUT = _CHAR (Write)
*        The name of the output DX structure.
*     BMODE = _LOGICAL (Read)
*        Should the data be written as binary?
*     DEP = _CHAR (Read)
*        Is the data position dependant?
*     PVAR = _LOGICAL (Read)
*        Should the variance section be written if found?
*     PBAD = _LOGICAL (Read)
*        Should the BAD pixels be associated with an invalid
*        position item in the output file?
*     AXES = _LOGICAL (Read)
*        Should any axis information be copied?

*  Examples:
*     ndf2dx in=galaxy out=galaxy.dx
*
*     The NDF image "galaxy" is converted to a DX file
*     named "galaxy.dx"

*  Implementation Status:
*     Current version does not deal with complex variance or data
*     components.
*
*     This version uses PSX_UNAME to identify the opsys used
*     and thereby determine if binary data should be read in lsb or
*     msb format. This will need to be modified if other operating
*     systems are to be supported.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     ACD: A C Davenhall (Edinburgh)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     02-OCT-1995 (GJP)
*        Original version.
*     15-SEP-1997 (ACD)
*        Added option AXES to control whether axis any information in
*        the NDF is copied to the DX dataset.
*     29-AUG-2004 (TIMJ)
*        Do not directly compare logicals with TRUE/FALSE
*     28-SEP-2006 (DSB)
*        Store correct value (1) for unused 2nd or 3rd axis dimensions.
*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PAR_ERR'               ! Parameter system error constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      CHARACTER TDEP*11               ! Temporary variable
      CHARACTER* (NDF__SZTYP) VTSETD  ! Mapping type for data
      LOGICAL BMODE                   ! Save data as binary?
      LOGICAL CDATA                   ! Is complex data present?
      LOGICAL CVARI                   ! Is complex variance present?
      LOGICAL AXES                    ! Copy any axis information?
      LOGICAL FAXIS                   ! Are the axes to be copied?
      LOGICAL FBAD                    ! Are Bad pixels present?
      LOGICAL FDATA                   ! Is data present?
      LOGICAL FVARI                   ! Is variance present?
      LOGICAL DEP                     ! Is the data position dependant?
      LOGICAL TEMPL                   ! Override the FVARI/FBAD value?
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER I                       ! Loop variable
      INTEGER LBND(NDF__MXDIM)        ! Lower bounds for each image axis
      INTEGER NDF1                    ! Identifier for the source NDF
      INTEGER NDIM                    ! Number of dimensions in the
                                      ! image
      INTEGER POINTD(NDF__MXDIM)      ! Pointer to the data component of
                                      ! the source NDF
      INTEGER POINTV(NDF__MXDIM)      ! Pointer to the variance component of
                                      ! the source NDF
      INTEGER PRANGE(NDF__MXDIM)      ! Number of pixels in the image x
      INTEGER UBND(NDF__MXDIM)        ! Upper bounds for each image axis

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Check to see if the command line defines the value of BMODE
*   ie is the data to be stored as binary or ASCII.
      CALL PAR_GET0L('BMODE',BMODE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Check to see if the command line defines the value of DEP
*   ie is the data position endant?
      CALL PAR_CHOIC( 'DEP', 'POSITIONS', 'POSITIONS,CONNECTIONS',
     :                .FALSE., TDEP, STATUS )
      DEP = ( TDEP .EQ. 'POSITIONS' )
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Begin an NDF context.
      CALL NDF_BEGIN
      CALL MSG_BLANK(STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Obtain an identifier for the NDF structure to be examined.
      CALL NDF_ASSOC('IN','READ',NDF1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Check the state of the components.
      CALL NDF_STATE(NDF1,'Data',FDATA,STATUS)
      CALL NDF_STATE(NDF1,'Variance',FVARI,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Stop here if the Data component is undefined.
      IF(.NOT.FDATA) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','No data component found.',STATUS)
         GOTO 9999
      END IF

*   Check for complex components.
      CALL NDF_CMPLX(NDF1,'Data',CDATA,STATUS)
      CALL NDF_CMPLX(NDF1,'Variance',CVARI,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Stop here if the Data component is complex.
      IF ((CDATA).OR.(CVARI)) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Complex data/variance values found.'
     :                    ,STATUS)
         GOTO 9999
      END IF

*   Check to see if the command line defines the value of FVARI.
      CALL PAR_GET0L('PVAR',TEMPL,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (TEMPL) FVARI=.FALSE.

*   Get the pixel-index bounds of the NDF and store in LBND and UBND.
      CALL NDF_BOUND(NDF1,3,LBND,UBND,NDIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Test the dimensionality.
      IF(NDIM.GT.3) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Cannot cope with images of 4',STATUS)
         CALL ERR_REP(' ','or more dimensions.',STATUS)
         GOTO 9999
      END IF

*   Store the size (in pixels) of the image dimensions.
      DO 10 I=1,3
         PRANGE(I)=UBND(I)-LBND(I)+1
 10   CONTINUE

*   Sort out the data component type to use.
      CALL NDF_TYPE(NDF1,'Data',VTSETD,STATUS)

*   Make sure that _UBYTE, _BYTE, _UWORD and _WORD
*   are mapped as integer.
      IF ((VTSETD.NE.'_INTEGER')
     :     .AND.(VTSETD.NE.'_REAL')
     :     .AND.(VTSETD.NE.'_DOUBLE')) THEN
         VTSETD='_INTEGER'
      END IF

*   Map the data component..
      CALL NDF_MAP(NDF1,'Data',VTSETD,'READ',POINTD(1),
     :             ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Is there a variance component to map.
      IF (FVARI) THEN

*      Map the variance component using the same type as the data.
         CALL NDF_MAP(NDF1,'Variance',VTSETD,'READ',POINTV(1),
     :                ELEMS,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

      ELSE

*      Since no variance was found make sure the data array is
*      passed twice to the code that writes the invalid pixel info.
         POINTV(1)=POINTD(1)

      END IF

*   Check to see if BAD pixels are present.
      CALL NDF_BAD(NDF1,'Data',.FALSE.,FBAD,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Check to see if the command line defines the value of PBAD.
      CALL PAR_GET0L('PBAD',TEMPL,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (.NOT.TEMPL) FBAD=.FALSE.

*   Check to see if the command line defines the value of AXES.
*   This variable controls whether any axis information which may
*   be in the NDF is to be copied to the DX dataset.
      CALL PAR_GET0L('AXES',AXES,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Look at the axis information.

*   If any axes which may be present are to be copied then check
*   whether any actually exist.  Otherwise set the flag indicating
*   that they are not to be copied.
      IF (AXES) THEN
         CALL NDF_STATE(NDF1,'Axis',FAXIS,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
      ELSE
         FAXIS = .FALSE.
      END IF

*   Write to the DX file.
      CALL NDF2DX_TEXT(DEP,BMODE,POINTD,POINTV,
     :                  VTSETD,FAXIS,FBAD,
     :                  FVARI,ELEMS,LBND,NDIM,NDF1,PRANGE,
     :                  STATUS)

*   Close down all resources.
 9999 CONTINUE

*   End the NDF context.
      CALL NDF_END(STATUS)

      END


      SUBROUTINE NDF2DX_TEXT(DEP,BMODE,POINTD,POINTV,
     :                       VTSETD,FAXIS,FBAD,FVARI,ELEMS,LBND,
     :                       NDIM,NDF1,PRANGE,STATUS)
*+
*  Name:
*     NDF2DX_TEXT

*  Purpose:
*     Creates the DX file required in ASCII.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL NDF2DX_TEXT(DEP,BMODE,VTSETD,FAXIS,FBAD,
*                       FVARI,ELEMS,LBND,NDIM,NDF1,PRANGE,
*                       STATUS)

*  Description:
*     Creates a DX file and places in it data from the
*     open NDF file. ASCII data only will be created.

*  Arguments:
*     DEP = LOGICAL (Given)
*        Is the data position dependant?
*     BMODE = LOGICAL (Given)
*        Is the data to be saved as binary?
*     POINTD(NDF__MXDIM) = INTEGER (Given)
*        Pointer to the data component of the source NDF.
*     POINTV(NDF__MXDIM) = INTEGER (Given)
*        Pointer to the variance component of the source NDF.
*     VTSETD = CHARACTER *(NDF__SZTYP) (Given)
*        Type of data in data.
*     FAXIS = LOGICAL (Given)
*        Is the axis component present?
*     FBAD = LOGICAL (Given)
*        Are there bad pixels present.
*     FVARI = LOGICAL (Given)
*        Is the variance component present?
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     LBND = INTEGER (Given)
*        Image co-ordinates origin.
*     NDIM = INTEGER (Given)
*        Number of dimensions of the image.
*     NDF1 = INTEGER (Given)
*        NDF identifier for the image.
*     PRANGE ( NDF__MXDIM ) = INTEGER (Given)
*        The length of each image axis in pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     12-OCT-1993 (GJP)
*       (Original version)
*     29-AURG-2004 (TIMJ)
*       Declare logical variables as LOGICAL and not INTEGER
*       Use LOGICALs are logicals and do not compare with .TRUE./.FALSE.
*

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'MSG_PAR'               ! Parameter system constants
      INCLUDE 'NDF_PAR'               ! NDF public constants
      INCLUDE 'FIO_ERR'               ! FIO error constants

*  Arguments Given:
      CHARACTER *(NDF__SZTYP) VTSETD  ! Type of data in data
      LOGICAL BMODE                   ! Is data to be stored as binary
      LOGICAL FAXIS                   ! Is the axis information present
      LOGICAL FBAD                    ! Are bad pixels present
      LOGICAL FVARI                   ! Variance component present?
      INTEGER ELEMS                   ! Number of pixels
      INTEGER LBND(NDF__MXDIM)        ! Lower limits of image world
                                      ! co-ordinate system
      INTEGER NDF1                    ! NDF indentifier
      INTEGER NDIM                    ! Image dimensionality
      LOGICAL DEP                   ! Is data position dependant
      INTEGER POINTD(NDF__MXDIM)      ! Pointer to the data component of
                                      ! the source NDF
      INTEGER POINTV(NDF__MXDIM)      ! Pointer to the variance component of
                                      ! the source NDF
      INTEGER PRANGE(NDF__MXDIM)      ! Number of pixels in the image x

*  Status:
      INTEGER STATUS                  ! Global status

*  External functions:
      INTEGER FOPEN                   ! C function
      INTEGER FCLOSE                  ! C function

*  Local variables:
      CHARACTER *(3)  BLANK           ! A blank line
      CHARACTER *(256) LINE           ! FIO line output length
      CHARACTER *(MSG__SZMSG) NAME    ! NDF name
      CHARACTER *(256) OUT            ! Output file name
      CHARACTER *(7) SHAPE            ! A useful string
      INTEGER FIOD                    ! File identifier
      INTEGER NC                      ! Length of output string
      INTEGER PNTRX(3)                ! Pointer to the X axis information
      INTEGER PNTRY(3)                ! Pointer to the Y axis information
      INTEGER PNTRZ(3)                ! Pointer to the Z axis information
      INTEGER STATUS2                 ! C function status
      INTEGER SZX                     ! Number of pixels in x axis
      INTEGER SZY                     ! Number of pixels in y axis
      INTEGER SZZ                     ! Number of pixels in z axis
      LOGICAL OVER                    ! Over-write existing output file?
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Empty string.
      BLANK='#'

*   Set default C function status value.
      STATUS2=1

*   Define shape of data/variance string.
      SHAPE='shape '//CHAR(48+NDIM)

*   Get the image name.
      CALL PAR_GET0C('OUT',OUT,STATUS)
      IF (STATUS.NE.SAI__OK) THEN
         CALL ERR_REP(' ','Failed naming output file.',
     :                   STATUS)
         GOTO 9999
      END IF

*   Open the output file.
      IF (BMODE) THEN

*      Open the binary output file.
         STATUS2=FOPEN(OUT,"w")
         IF (STATUS2.EQ.0) THEN
            STATUS=SAI__ERROR
            CALL ERR_REP(' ','Failed opening output file.',
     :                   STATUS)
            GOTO 9999
         END IF

      ELSE

*      Open the ASCII FIO file.
         CALL FIO_OPEN(OUT,'WRITE','LIST',200,FIOD,STATUS)

*  If it failed because the file already exists, delete the existing file
*  and opening a new one.
         IF( STATUS .EQ. FIO__NFEXI ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL FIO_ERASE( OUT, STATUS )
            CALL FIO_OPEN( OUT, 'WRITE', 'LIST', 200, FIOD, STATUS )

         ELSE IF (STATUS.NE.SAI__OK) THEN
            GOTO 9999

         END IF

      END IF

*   Output the main heading.

      NC=0
      CALL CHR_PUTC('# STARLINK NDF2DX output file',LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:1),STATUS)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:1),STATUS)

*   Output the file name.
      CALL NDF_MSG('NAME',NDF1)
      CALL MSG_LOAD(' ','^NAME',NAME,NC,STATUS)
      NC=0
      CALL CHR_PUTC('# NDF filename: ',LINE,NC)
      CALL CHR_PUTC(NAME,LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:1),STATUS)
      IF(STATUS.NE.SAI__OK) GOTO 9999

*   Map the axis information arrays.
*   Will be needed if FAXIS is TRUE or FBAD is true.
      CALL NDF2DX_AXIS(NDF1,NDIM,PRANGE,PNTRX,PNTRY,PNTRZ,
     :                 SZX,SZY,SZZ,STATUS)
      IF(STATUS.NE.SAI__OK) GOTO 9999


*   Output the regular positions grid information. Object 1.

      IF (.NOT.FAXIS) THEN

*      No axis information defined.
         CALL NDF2DX_POSNA(DEP,BMODE,FIOD,NDF1,NDIM,PRANGE,
     :                     LBND,STATUS)

      ELSE

*      Axis information defined.
         CALL NDF2DX_POSYA(DEP,ELEMS,BMODE,FIOD,NDF1,NDIM,PRANGE,
     :                     LBND,PNTRX,PNTRY,PNTRZ,
     :                     SZX,SZY,SZZ,STATUS)

      END IF
      IF(STATUS.NE.SAI__OK) GOTO 9999

*   Output the regular grid connections information. Object 2.

      CALL NDF2DX_REGG(DEP,BMODE,FIOD,NDIM,PRANGE,STATUS)
      IF(STATUS.NE.SAI__OK) GOTO 9999

*   Create the data object. Object 3.

      CALL NDF2DX_DATA(DEP,FVARI,POINTD,POINTV,ELEMS,BMODE,
     :                 FIOD,PRANGE,VTSETD,STATUS)
      IF(STATUS.NE.SAI__OK) GOTO 9999


*   Deal with the BAD pixels as invalid data.
*   This is object 4 or 5 (depending on value of FVARI).

      IF (FBAD) THEN

         CALL NDF2DX_BAD(DEP,NDIM,VTSETD,ELEMS,POINTD,POINTV,
     :                   PRANGE,NDF1,FVARI,FBAD,BMODE,
     :                   FIOD,STATUS)
         IF(STATUS.NE.SAI__OK) GOTO 9999

      END IF


*   Add the field creation.
      CALL NDF2DX_FIELD(NDF1,FVARI,FBAD,BMODE,FIOD,NDIM,DEP,STATUS)
      IF(STATUS.NE.SAI__OK) GOTO 9999

*   Close down the file.
      IF (BMODE) THEN

*      Binary.
         STATUS2=FCLOSE()

      ELSE

*      ASCII.
         CALL FIO_CLOSE(FIOD,STATUS)

      END IF

 9999 CONTINUE

      END



      SUBROUTINE NDF2DX_LABS( NDF1, NDIM, BMODE, FIOD, STATUS )
*+
*  Name:
*     NDF2DX_LABS

*  Purpose:
*     Writes out an attribute clause defining the axis units and labels.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_LABS( NDF1, NDIM, BMODE, FIOD, STATUS )

*  Description:
*     This subroutine is called to add an "axis labels" attribute to
*     the field object. This attribute can be used by the "AutoAxes"
*     DX module. It consists of a string list (one string for each axis).
*     Each string contains an axis label followed by the associated units
*     in parenthesise.

*  Arguments:
*     NDF1 = INTEGER (Given)
*        NDF identifier of input file.
*     NDIM = INTEGER (Given)
*        Number of dimensions.
*     BMODE = LOGICAL (Given)
*        Save data as binary?
*     FIOD = INTEGER (Given)
*        Identifier for the opened FIO output file (if BMODE=.FALSE.).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     DSB: David Berry (STARLINK)

*  History:
*     12-OCT-1995 (GJP)
*        (Original version)
*     12-DEC-1995 (DSB)
*        Re-written to create an attribute instead of an object.

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      LOGICAL BMODE
      INTEGER FIOD
      INTEGER NDF1
      INTEGER NDIM

*  Status:
      INTEGER STATUS                  ! Global status

*  External Functions:
      INTEGER CHR_LEN                 ! Used length of a string

*  Local variables:
      CHARACTER LINE*256              ! FIO line output length
      CHARACTER TEXT*80               ! AXIS character strings
      INTEGER I                       ! Loop variable
      INTEGER NC                      ! Number of characters

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Create the attribute statement for the axis labels
      NC = 0
      CALL CHR_PUTC( 'attribute "axis labels" string', LINE, NC )

      DO I = 1, NDIM
         TEXT = ' '
         CALL NDF_ACGET( NDF1, 'Label', I, TEXT, STATUS )
         CALL CHR_PUTC( ' "'//TEXT( :CHR_LEN( TEXT ) ), LINE, NC )

         TEXT = ' '
         CALL NDF_ACGET( NDF1, 'Units', I, TEXT, STATUS )
         IF( TEXT .NE. ' ' ) THEN
            CALL CHR_PUTC( ' ('//TEXT( :CHR_LEN( TEXT ) )//')', LINE,
     :                     NC )
         END IF

         CALL CHR_PUTC( '"', LINE, NC )

      END DO

      CALL NDF2DX_WRICH( BMODE, FIOD, LINE( :NC ), STATUS )

      END


      SUBROUTINE NDF2DX_BAD(DEP,NDIM,VTSETD,ELEMS,POINTD,
     :                      POINTV,PRANGE,NDF1,FVARI,FBAD,
     :                      BMODE,FIOD,STATUS)
*+
*  Name:
*     NDF2DX_BAD

*  Purpose:
*     Writes out the bad pixel information

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_BAD(DEP,NDIM,VTSETD,ELEMS,POINTD,POINTV,PRANGE,
*                     NDF1,FVARI,FBAD,BMODE,FIOD,STATUS)

*  Description:
*     Writes out the bad pixel information.

*  Arguments:
*     DEP = LOGICAL (Given)
*        Is the data position dependant?
*     NDIM = INTEGER (Given)
*        The number of dimensions
*     VTSETD = CHARACTER *(NDF__SZTYP) (Given)
*        Type of data in data.
*     ELEMS = INTEGER (Given)
*        Number of pixels.
*     POINTD(NDF__MXDIM) = INTEGER (Given)
*        Pointer to the data component of the source NDF.
*     POINTV(NDF__MXDIM) = INTEGER (Given)
*        Pointer to the variance component of the source NDF.
*     PRANGE ( NDF__MXDIM ) = INTEGER (Given)
*        The length of each image axis in pixels.
*     NDF1 = INTEGER (Given)
*        NDF identifier of input file.
*     FVARI = LOGICAL (Given)
*        Is variance to be stored.
*     FBAD = LOGICAL (Given)
*        Is variance to be stored.
*     BMODE = LOGICAL (Given)
*        Save data as binary?
*     FIOD = INTEGER (Given)
*        Identifier for the opened FIO file (if used).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     12-OCT-1995 (GJP)
*        (Original version)
*     31-AUG-2004 (TIMJ)
*        Use CNF_PVAL


*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF public constants
      INCLUDE 'CNF_PAR'               ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER *(NDF__SZTYP) VTSETD  ! Type of data in data
      LOGICAL BMODE                   ! Is data saved as binary
      LOGICAL DEP                     ! Is the data position dependant?
      INTEGER ELEMS                   ! Number of pixels
      LOGICAL FBAD                    ! Is the bad data stored
      LOGICAL FVARI                   ! Is variance to be stored
      INTEGER FIOD                    ! FIO identifier
      INTEGER NDF1                    ! The NDF identifier
      INTEGER NDIM                    ! Number of dimensions
      INTEGER POINTD(NDF__MXDIM)      ! Pointer to the data component of
                                      ! the source NDF
      INTEGER POINTV(NDF__MXDIM)      ! Pointer to the variance component of
                                      ! the source NDF
      INTEGER PRANGE(NDF__MXDIM)      ! Number of pixels in the image x

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      CHARACTER *(1) BLANK            ! A blank
      CHARACTER *(3)  BORD            ! Byte order
      CHARACTER *(256) LINE           ! FIO line output length
      CHARACTER *(20) MACH            ! Machine name
      CHARACTER *(20) NODE            ! Node name
      CHARACTER*(13) PORC             ! Positions or connections
      CHARACTER *(20) REL             ! Opsys version
      CHARACTER *(20) SYS             ! System name
      CHARACTER *(256) TEXT1          ! Text message
      CHARACTER *(256) TEXT2          ! Text message
      CHARACTER *(20) VER             ! Opsys subversion name
      INTEGER COUNT                   ! Number of bad pixels
      INTEGER NC                      ! Number of characters
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Blank.
      BLANK='#'

*   Find out what system type we are running on.
      CALL PSX_UNAME(SYS,NODE,REL,VER,MACH,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      BORD=' '
      IF (SYS(1:5).EQ.'SunOS') BORD='msb'
      IF (SYS(1:4).EQ.'OSF1') BORD='lsb'
      IF (SYS(1:5).EQ.'Linux') BORD='lsb'
      IF (BORD.EQ.' ') THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Unsupported operating system.',
     :                STATUS)
         GOTO 9999
      END IF

*   Is data position or connection dependant?
      IF (DEP) THEN
         PORC='"positions"'
      ELSE
         PORC='"connections"'
      END IF

*   Look to see how many bad pixels there are.
      IF (VTSETD.EQ.'_INTEGER') CALL NDF2DX_CBADI
     :   (%VAL(CNF_PVAL(POINTD(1))),%VAL(CNF_PVAL(POINTV(1))),
     :    ELEMS,COUNT,STATUS)
      IF (VTSETD.EQ.'_REAL') CALL NDF2DX_CBADR
     :   (%VAL(CNF_PVAL(POINTD(1))),%VAL(CNF_PVAL(POINTV(1))),
     :    ELEMS,COUNT,STATUS)
      IF (VTSETD.EQ.'_DOUBLE') CALL NDF2DX_CBADD
     :   (%VAL(CNF_PVAL(POINTD(1))),%VAL(CNF_PVAL(POINTV(1))),
     :    ELEMS,COUNT,STATUS)

*   Bailout if there were none found.
      IF (COUNT.EQ.0) THEN
         FBAD=.FALSE.
         GOTO 9999
      END IF

*   Create and write the object heading.
      CALL MSG_FMTI('COUNT','I10',COUNT)
      CALL NDF2DX_WRICH(BMODE,FIOD,BLANK(1:1),STATUS)
      IF (FVARI) THEN
         TEXT1='# object 5 is the invalid '//PORC
         IF (BMODE) THEN
            TEXT2='object 5 class array type int '//
     :         'rank 0 items ^COUNT '//BORD//
     :         ' binary data follows'
         ELSE
            TEXT2='object 5 class array type int '//
     :         'rank 0 items ^COUNT '//
     :         'data follows'
         END IF
      ELSE
         TEXT1='# object 4 is the invalid '//PORC
         IF (BMODE) THEN
            TEXT2='object 4 class array type int '//
     :         'rank 0 items ^COUNT '//BORD//
     :         ' binary data follows'
         ELSE
            TEXT2='object 4 class array type int '//
     :         'rank 0 items ^COUNT '//
     :         'data follows'
         END IF
      END IF
      NC=0
      CALL CHR_PUTC(TEXT1,LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)
      NC=0
      CALL MSG_LOAD(' ',TEXT2,LINE,NC,STATUS)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

*   Save the invalid points.
      IF (VTSETD.EQ.'_INTEGER') CALL NDF2DX_WRIBADI
     :                         (BMODE,FIOD,%VAL(CNF_PVAL(POINTD(1))),
     :                         %VAL(CNF_PVAL(POINTV(1))),ELEMS,STATUS)
      IF (VTSETD.EQ.'_REAL') CALL NDF2DX_WRIBADR
     :                       (BMODE,FIOD,%VAL(CNF_PVAL(POINTD(1))),
     :                       %VAL(CNF_PVAL(POINTV(1))),ELEMS,STATUS)
      IF (VTSETD.EQ.'_DOUBLE') CALL NDF2DX_WRIBADD
     :                         (BMODE,FIOD,%VAL(CNF_PVAL(POINTD(1))),
     :                         %VAL(CNF_PVAL(POINTV(1))),ELEMS,STATUS)

*   Add the attribute reference at the end.
      NC=0
      CALL CHR_PUTC('attribute "ref" string '//PORC,
     :              LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

 9999 CONTINUE

      END


      SUBROUTINE NDF2DX_FIELD(NDF1,FVARI,FBAD,BMODE,FIOD,NDIM,DEP,
     :                        STATUS)
*+
*  Name:
*     NDF2DX_FIELD

*  Purpose:
*     Writes out the field object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_FIELD(NDF1,FVARI,FBAD,BMODE,FIOD,NDIM,DEP,STATUS)

*  Description:
*     Creates the field object at the end of the DX file.

*  Arguments:
*     NDF1 = INTEGER (Given)
*        NDF identifier of input file.
*     FVARI = LOGICAL (Given)
*        Is variance to be stored.
*     FBAD = LOGICAL (Given)
*        Is variance to be stored.
*     BMODE = LOGICAL (Given)
*        Save data as binary?
*     FIOD = INTEGER (Given)
*        Identifier for the opened FIO file (if used).
*     NDIM = INTEGER (Given)
*        No. of dimensions in NDF
*     DEP = LOGICAL (Given)
*        Is the data positions-dependant?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-OCT-1995 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF public constants

*  Arguments Given:
      LOGICAL BMODE                   ! Is data saved as binary
      LOGICAL FBAD                    ! Is the bad data stored
      LOGICAL FVARI                   ! Is variance to be stored
      INTEGER FIOD                    ! FIO identifier
      INTEGER NDF1                    ! The NDF identifier
      INTEGER NDIM
      LOGICAL DEP

*  Status:
      INTEGER STATUS                  ! Global status

*  External References:
      INTEGER CHR_LEN                 ! Used length of a string

*  Local variables:
      CHARACTER *(1) BLANK            ! A blank
      CHARACTER *(256) LINE           ! FIO line output length
      CHARACTER NAME*80               ! Field name
      INTEGER NC                      ! Number of characters
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the value of blank.
      BLANK='#'

*   Show the comment.
      CALL NDF2DX_WRICH(BMODE,FIOD,BLANK(1:1),STATUS)
      NC=0
      CALL CHR_PUTC('# A multi component field is created'
     :              ,LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

*   Output the object class.
      NAME = 'NDF'
      CALL NDF_CGET( NDF1, 'TITLE', NAME, STATUS )

      NC = 0
      CALL CHR_PUTC( 'object "', LINE, NC )
      CALL CHR_PUTC( NAME( :CHR_LEN( NAME ) ), LINE, NC )
      CALL CHR_PUTC( '" class field ', LINE, NC )
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

*   Output the components.
      NC=0
      CALL CHR_PUTC('component "positions" value 1'
     :              ,LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)
      NC=0
      CALL CHR_PUTC('component "connections" value 2'
     :              ,LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)
      NC=0
      CALL CHR_PUTC('component "data" value 3'
     :              ,LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

*   Output conditional components.

*   The variance.
      IF (FVARI) THEN
         NC=0
         CALL CHR_PUTC('component "variance" value 4'
     :                 ,LINE,NC)
         CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)
      END IF

*   The invalid pixel positions or connections.
      IF( FBAD ) THEN
         NC = 0
         CALL CHR_PUTC( 'component "invalid ', LINE, NC )

         IF( DEP ) THEN
            CALL CHR_PUTC( 'positions" value ', LINE, NC )
         ELSE
            CALL CHR_PUTC( 'connections" value ', LINE, NC )
         END IF

         IF( FVARI ) THEN
            CALL CHR_PUTI( 5, LINE, NC )
         ELSE
            CALL CHR_PUTI( 4, LINE, NC )
         END IF

         CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

      END IF

*   Axis labels attributes...
      CALL NDF2DX_LABS( NDF1, NDIM, BMODE, FIOD, STATUS )

*   Obtain/write the NDF title component.
      NC=0
      CALL NDF_CMSG('TITLE',NDF1,'Title',STATUS)
      CALL MSG_LOAD(' ','attribute "name" string "^TITLE"',
     :              LINE,NC,STATUS)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

*   Obtain/write the NDF label component.
      NC=0
      CALL NDF_CMSG('LABEL',NDF1,'Label',STATUS)
      CALL MSG_LOAD(' ','attribute "ndf_label" string "^LABEL"',
     :              LINE,NC,STATUS)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

*   Obtain/write the NDF units component.
      NC=0
      CALL NDF_CMSG('UNITS',NDF1,'Units',STATUS)
      CALL MSG_LOAD(' ','attribute "ndf_units" string "^UNITS"',
     :              LINE,NC,STATUS)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

*   Insert the end of file marker and a user message.
      CALL NDF2DX_WRICH(BMODE,FIOD,BLANK(1:1),STATUS)
      NC=0
      CALL CHR_PUTC('end',LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

 9999 CONTINUE

      END


      SUBROUTINE NDF2DX_DATA(DEP,FVARI,POINTD,POINTV,ELEMS,
     :                       BMODE,FIOD,PRANGE,VTSETD,STATUS)
*+
*  Name:
*     NDF2DX_DATA

*  Purpose:
*     Writes out the data and variance information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_DATA(DEP,FVARI,ELEMS,BMODE,FIOD,NDIM,
*                      PRANGE,VTSETD,STATUS)

*  Description:
*     Creates the data and variance objects.

*  Arguments:
*     DEP = LOGICAL (Given)
*        Is the data position dependant?
*     FVARI = LOGICAL (Given)
*        Is variance to be stored.
*     ELEMS = INTEGER (Given)
*        Number of pixels.
*     BMODE = LOGICAL (Given)
*        Save data as binary?
*     FIOD = INTEGER (Given)
*        Identifier for the opened FIO file (if used).
*     NDIM = INTEGER (Given)
*        The number of dimensions.
*     PRANGE ( NDF__MXDIM ) = INTEGER (Given)
*        The length of each image axis in pixels.
*     POINTD(NDF__MXDIM) = INTEGER (Given)
*        Pointer to the data component of the source NDF.
*     POINTV(NDF__MXDIM) = INTEGER (Given)
*        Pointer to the variance component of the source NDF.
*     VTSETD = CHARACTER *(NDF__SZTYP) (Given)
*        Type of data in data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     12-OCT-1995 (GJP)
*       (Original version)
*     31-AUG-2004 (TIMJ)
*       Use CNF_PVAL

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF public constants
      INCLUDE 'CNF_PAR'               ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER* (NDF__SZTYP) VTSETD  ! Mapping type for data
      LOGICAL BMODE                   ! Is data saved as binary?
      LOGICAL DEP                     ! Is the data position dependant?
      LOGICAL FVARI                   ! Is variance to be stored?
      INTEGER ELEMS                   ! Number of pixels
      INTEGER FIOD                    ! FIO identifier
      INTEGER POINTD(NDF__MXDIM)      ! Pointer to the data component of
                                      ! the source NDF
      INTEGER POINTV(NDF__MXDIM)      ! Pointer to the variance component of
                                      ! the source NDF
      INTEGER PRANGE(NDF__MXDIM)      ! Expected size of axis

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      CHARACTER *(1) BLANK            ! A blank
      CHARACTER *(3)  BORD            ! Byte order
      CHARACTER *(256) LINE           ! FIO line output length
      CHARACTER *(20) MACH            ! Machine name
      CHARACTER *(20) NODE            ! Node name
      CHARACTER*(13) PORC             ! Positions or connection dependant
      CHARACTER *(20) REL             ! Opsys version
      CHARACTER *(20) SYS             ! System name
      CHARACTER *(256) TEXT1          ! Text message
      CHARACTER *(20) VER             ! Opsys subversion name
      INTEGER NC                      ! Number of characters
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Blank.
      BLANK='#'

*   Is data position or connection dependant?
      IF (DEP) THEN
         PORC='"positions"'
      ELSE
         PORC='"connections"'
      END IF

*   Find out what system type we are running on.
      CALL PSX_UNAME(SYS,NODE,REL,VER,MACH,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      BORD=' '
      IF (SYS(1:5).EQ.'SunOS') BORD='msb'
      IF (SYS(1:4).EQ.'OSF1') BORD='lsb'
      IF (SYS(1:5).EQ.'Linux') BORD='lsb'
      IF (BORD.EQ.' ') THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Unsupported operating system.',
     :                STATUS)
         GOTO 9999
      END IF

*   Heading.
      CALL NDF2DX_WRICH(BMODE,FIOD,BLANK(1:1),STATUS)
      NC=0
      CALL CHR_PUTC('# object 3 is the data ',LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

*   Output the data object header.
      NC=0
      CALL MSG_FMTI('ELEMS','I10',ELEMS)
      IF (BMODE) THEN

*      Binary.
         IF (VTSETD.EQ.'_INTEGER') TEXT1=
     :          'object 3 class array type int rank 0 '//
     :          'items ^ELEMS '//BORD// ' binary data follows'
         IF (VTSETD.EQ.'_REAL') TEXT1=
     :         'object 3 class array type float rank 0 '//
     :         'items ^ELEMS '//BORD//' binary data follows'

         IF (VTSETD.EQ.'_DOUBLE') TEXT1=
     :         'object 3 class array type double rank 0 '//
     :         'items ^ELEMS '//BORD//' binary data follows'
      ELSE

*     ASCII.
         IF (VTSETD.EQ.'_INTEGER') TEXT1=
     :          'object 3 class array type int rank 0 '//
     :          'items ^ELEMS data follows'
         IF (VTSETD.EQ.'_REAL') TEXT1=
     :         'object 3 class array type float rank 0 '//
     :         'items ^ELEMS data follows'

         IF (VTSETD.EQ.'_DOUBLE') TEXT1=
     :         'object 3 class array type double rank 0 '//
     :         'items ^ELEMS data follows'

      END IF

      CALL MSG_LOAD(' ',TEXT1,LINE,NC,STATUS)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)
      IF(STATUS.NE.SAI__OK) GOTO 9999

*   Save the data.
      IF (VTSETD.EQ.'_INTEGER')
     :   CALL NDF2DX_WRIDATI(BMODE,FIOD,%VAL(CNF_PVAL(POINTD(1))),ELEMS,
     :                       STATUS)
      IF (VTSETD.EQ.'_REAL')
     :   CALL NDF2DX_WRIDATR(BMODE,FIOD,%VAL(CNF_PVAL(POINTD(1))),ELEMS,
     :                       STATUS)
      IF (VTSETD.EQ.'_DOUBLE')
     :   CALL NDF2DX_WRIDATD(BMODE,FIOD,%VAL(CNF_PVAL(POINTD(1))),ELEMS,
     :                       STATUS)
      IF( STATUS.NE.SAI__OK) GOTO 9999

*   Append the "dep" attribute.
      NC=0
      CALL CHR_PUTC('attribute "dep" string '//PORC,LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

*   Create the variance object. Object 4.

      IF(FVARI) THEN

         CALL NDF2DX_WRICH(BMODE,FIOD,BLANK(1:1),STATUS)
         NC=0
         CALL CHR_PUTC('# object 4 is the variance ',LINE,NC)
         CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

*      Output the variance object.
         NC=0
         CALL MSG_FMTI('ELEMS','I10',ELEMS)

         IF (BMODE) THEN

*         Binary.
            IF (VTSETD.EQ.'_INTEGER') TEXT1=
     :         'object 4 class array type int rank 0 '//
     :         ' items ^ELEMS '//BORD//' binary data follows'
            IF (VTSETD.EQ.'_REAL') TEXT1=
     :         'object 4 class array type float rank 0 '//
     :         ' items ^ELEMS '//BORD//' binary data follows'
            IF (VTSETD.EQ.'_DOUBLE') TEXT1=
     :         'object 4 class array type double rank 0 '//
     :         ' items ^ELEMS '//BORD//' binary data follows'

         ELSE

*         ASCII.
            IF (VTSETD.EQ.'_INTEGER') TEXT1=
     :         'object 4 class array type int rank 0 '//
     :         ' items ^ELEMS data follows'
            IF (VTSETD.EQ.'_REAL') TEXT1=
     :         'object 4 class array type float rank 0 '//
     :         ' items ^ELEMS data follows'
            IF (VTSETD.EQ.'_DOUBLE') TEXT1=
     :         'object 4 class array type double rank 0 '//
     :         ' items ^ELEMS data follows'

         END IF

         CALL MSG_LOAD(' ',TEXT1,LINE,NC,STATUS)
         CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

*      Save the variance.
         IF (VTSETD.EQ.'_INTEGER')
     :     CALL NDF2DX_WRIDATI(BMODE,FIOD,%VAL(CNF_PVAL(POINTV(1))),
     :                         ELEMS,
     :                         STATUS)
         IF (VTSETD.EQ.'_REAL')
     :     CALL NDF2DX_WRIDATR(BMODE,FIOD,%VAL(CNF_PVAL(POINTV(1))),
     :                         ELEMS,
     :                         STATUS)
         IF (VTSETD.EQ.'_DOUBLE')
     :     CALL NDF2DX_WRIDATD(BMODE,FIOD,%VAL(CNF_PVAL(POINTV(1))),
     :                         ELEMS,
     :                         STATUS)
         IF( STATUS.NE.SAI__OK) GOTO 9999

*      Append the "dep" attribute.
         NC=0
         CALL CHR_PUTC('attribute "dep" string '//PORC,LINE,NC)
         CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

      END IF

 9999 CONTINUE

      END



      SUBROUTINE NDF2DX_REGG(DEP,BMODE,FIOD,NDIM,PRANGE,STATUS)
*+
*  Name:
*     NDF2DX_REGG

*  Purpose:
*     Writes out the regular grid definition.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_REGG(DEP,BMODE,FIOD,NDIM,PRANGE,STATUS)

*  Description:
*     Writes out the regular grid definition.

*  Arguments:
*     DEP = LOGICAL (Given)
*        Is the data position dependant?
*     BMODE = LOGICAL (Given)
*        Save data as binary?
*     FIOD = INTEGER (Given)
*        Identifier for the opened FIO file (if used).
*     NDIM = INTEGER (Given)
*        The number of dimensions.
*     PRANGE ( NDF__MXDIM ) = INTEGER (Given)
*        The length of each image axis in pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-OCT-1995 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF public constants

*  Arguments Given:
      LOGICAL DEP                   ! Is the data position dependant?
      LOGICAL BMODE                   ! Is data saved as binary
      INTEGER FIOD                    ! FIO identifier
      INTEGER NDIM                    ! Number of dimensions
      INTEGER PRANGE(NDF__MXDIM)      ! Expected size of axis

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      CHARACTER *(1) BLANK            ! A blank
      CHARACTER *(256) LINE           ! FIO line output length
      CHARACTER *(256) TEXT1          ! Text message
      CHARACTER *(256) TEXT2          ! Text message
      INTEGER NC                      ! Number of characters
      INTEGER SUB                     ! Temporary variable
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Blank.
      BLANK='#'

*   Heading.
      CALL NDF2DX_WRICH(BMODE,FIOD,BLANK(1:1),STATUS)
      NC=0
      CALL CHR_PUTC('# object 2 is the regular '//
     :              'grid connections',LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

*   Setup appropriate strings
      IF (NDIM.EQ.1) THEN
         TEXT1='object 2 class gridconnections counts ^X'
         TEXT2='attribute "element type" string "line"'
      END IF
      IF (NDIM.EQ.2) THEN
         TEXT1='object 2 class gridconnections counts ^Y ^X'
         TEXT2='attribute "element type" string "quads"'
      END IF
      IF (NDIM.EQ.3) THEN
         TEXT1='object 2 class gridconnections counts ^Z ^Y ^X'
         TEXT2='attribute "element type" string "cubes"'
      END IF

*   Format image size information.
      IF (DEP) THEN
         SUB=0
      ELSE
         SUB=1
      END IF
      CALL MSG_FMTI('X','I6',PRANGE(1)+SUB)
      CALL MSG_FMTI('Y','I6',PRANGE(2)+SUB)
      CALL MSG_FMTI('Z','I6',PRANGE(3)+SUB)

*   Write size data.
      NC=0
      CALL MSG_LOAD(' ',TEXT1,LINE,NC,STATUS)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

*   Output the object class.
      NC=0
      CALL CHR_PUTC(TEXT2,LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

*   Output the cross reference.
      NC=0
      CALL CHR_PUTC('attribute "ref" string "positions"'
     :              ,LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)
      IF(STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

      END


      SUBROUTINE NDF2DX_POSYA(DEP,ELEMS,BMODE,FIOD,NDF1,NDIM,
     :                        PRANGE,LBND,PNTRX,PNTRY,PNTRZ,
     :                        SZX,SZY,SZZ,STATUS)
*+
*  Name:
*     NDF2DX_POSYA

*  Purpose:
*     Writes out position information when axis information
*     is available.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_POSYA(DEP,ELEMS,BMODE,FIOD,NDF1,NDIM,PRANGE,LBND,
*                       PNTRX,PNTRY,PNTRY,SZX,SZY,SZZ,STATUS)

*  Description:
*     Writes out position information when axis information
*     is available, as a product array. Each term is a regular
*     array if the axis is linear, and a standard array otherwise.

*  Arguments:
*     DEP = LOGICAL (Given)
*        Is data position dependant?
*     ELEMS = INTEGER (Given)
*        Number of pixels.
*     BMODE = LOGICAL (Given)
*        Save data as binary?
*     FIOD = INTEGER (Given)
*        Identifier for the opened FIO file (if used).
*     NDF1 = INTEGER (Given)
*        Identifier for the opened NDF.
*     NDIM = INTEGER (Given)
*        The number of dimensions.
*     PRANGE ( NDF__MXDIM ) = INTEGER (Given)
*        The length of each image axis in pixels.
*     LBND (NDF__MXDIM) = INTEGER (Given)
*         Lower bounds for each image axis.
*     PNTRX(3) =INTEGER (Returned)
*        Pointer to the axis information
*     PNTRY(3) =INTEGER (Returned)
*        Pointer to the axis information
*     PNTRZ(3) =INTEGER (Returned)
*        Pointer to the axis information.
*     SZX = INTEGER (Returned)
*        Sixe of the x array.
*     SZY = INTEGER (Returned)
*        Sixe of the y array.
*     SZZ = INTEGER (Returned)
*        Sixe of the z array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     12-OCT-1995 (GJP)
*       (Original version)
*     31-AUG-2004 (TIMJ)
*       Use CNF_PVAL

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF public constants
      INCLUDE 'CNF_PAR'               ! For CNF_PVAL function

*  Arguments Given:
      LOGICAL BMODE                   ! Is data saved as binary
      LOGICAL DEP                   ! Is data position dependant
      INTEGER ELEMS                   ! Number of pixels
      INTEGER FIOD                    ! FIO identifier
      INTEGER LBND(NDF__MXDIM)        ! Lower bounds for each image axis
      INTEGER NDF1                    ! NDF identifier
      INTEGER NDIM                    ! Number of dimensions
      INTEGER PRANGE(NDF__MXDIM)      ! Expected size of axis
      INTEGER PNTRX(3)                ! Pointer to the X axis information
      INTEGER PNTRY(3)                ! Pointer to the Y axis information
      INTEGER PNTRZ(3)                ! Pointer to the Z axis information
      INTEGER SZX                     ! Number of pixels in x axis
      INTEGER SZY                     ! Number of pixels in y axis
      INTEGER SZZ                     ! Number of pixels in z axis


*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      CHARACTER *(256) LINE           ! FIO line output length
      INTEGER I                       ! Loop variable
      INTEGER NC                      ! Number of characters
      INTEGER NP                      ! Number of elems
      LOGICAL LIN(3)                  ! Linearity flags for each axis
      REAL LVAL(3)                    ! Axis values at first element
      REAL UVAL(3)                    ! Axis values at last element

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Create each required term of the product array defining the positions.
      CALL NDF2DX_PTERM( 1, 'xaxis', PRANGE(1),
     :                   %VAL( CNF_PVAL( PNTRX(1) ) ), NDIM,
     :                   BMODE, FIOD, DEP, STATUS )

      IF( NDIM .GT. 1 ) THEN
         CALL NDF2DX_PTERM( 2, 'yaxis', PRANGE(2),
     :                      %VAL( CNF_PVAL( PNTRY(1) ) ),
     :                      NDIM, BMODE, FIOD, DEP, STATUS )

         IF( NDIM .GT. 2 ) THEN
            CALL NDF2DX_PTERM( 3, 'zaxis', PRANGE(3),
     :                         %VAL( CNF_PVAL( PNTRZ(1) ) ),
     :                         NDIM, BMODE, FIOD, DEP,  STATUS )
         END IF

      END IF

*   The header comments for the product array itself.
      CALL NDF2DX_WRICH(BMODE,FIOD,'#',STATUS)
      NC=0
      CALL CHR_PUTC('# object 1 is the positions',LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

*   Create the product array, with NDIM terms.
      NC=0
      CALL CHR_PUTC('object 1 class productarray',LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

      IF( NDIM .GT. 2 ) THEN
         NC=0
         CALL CHR_PUTC('term "zaxis"',LINE,NC)
         CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)
      END IF

      IF( NDIM .GT. 1 ) THEN
         NC=0
         CALL CHR_PUTC('term "yaxis"',LINE,NC)
         CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)
      END IF

      NC=0
      CALL CHR_PUTC('term "xaxis"',LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

      END


      SUBROUTINE NDF2DX_POSNA(DEP,BMODE,FIOD,NDF1,NDIM,
     :                        PRANGE,LBND,STATUS)
*+
*  Name:
*     NDF2DX_POSNA

*  Purpose:
*     Writes out position information when there is no axis
*     information available ie default grid.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_POSNA(DEP,BMODE,FIOD,NDF1,NDIM,PRANGE,
*                       LBND,STATUS)

*  Description:
*     Writes out position information when there is no axis
*     information available setting up a regular grid using default
*     information from the NDF..

*  Arguments:
*     DEP = LOGICAL (Given)
*        Is the data position dependant?
*     BMODE = LOGICAL (Given)
*        Save data as binary?
*     FIOD = INTEGER (Given)
*        Identifier for the opened FIO file (if used).
*     NDF1 = INTEGER (Given)
*        Identifier for the opened NDF.
*     NDIM = INTEGER (Given)
*        The number of dimensions.
*     PRANGE ( NDF__MXDIM ) = INTEGER (Given)
*        The length of each image axis in pixels.
*     LBND (NDF__MXDIM) = INTEGER (Given)
*         Lower bounds for each image axis.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-OCT-1995 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF public constants

*  Arguments Given:
      LOGICAL DEP                   ! Is the data position dependant?
      LOGICAL BMODE                   ! Is data saved as binary
      INTEGER FIOD                    ! FIO identifier
      INTEGER LBND(NDF__MXDIM)        ! Lower bounds for each image axis
      INTEGER NDF1                    ! NDF identifier
      INTEGER NDIM                    ! Number of dimensions
      INTEGER PRANGE(NDF__MXDIM)      ! Expected size of axis

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      CHARACTER *(1) BLANK            ! A blank
      CHARACTER *(256) LINE           ! FIO line output length
      CHARACTER *(256) TEXT1          ! Text message
      CHARACTER *(256) TEXT2          ! Text message
      CHARACTER *(256) TEXT3          ! Text message
      CHARACTER *(256) TEXT4          ! Text message
      CHARACTER *(256) TEXT5          ! Text message
      INTEGER NC                      ! Number of characters
      INTEGER SUB                     ! Is the data position dependant?
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   A useful string.
      BLANK='#'

*   Heading.
      CALL NDF2DX_WRICH(BMODE,FIOD,BLANK(1:1),STATUS)
      NC=0

      CALL CHR_PUTC('# object 1 is the regular '//
     :              'grid positions',LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

      IF(STATUS.NE.SAI__OK) GOTO 9999

*   Setup appropriate strings
      IF (NDIM.EQ.1) THEN
         TEXT1='object 1 class gridpositions counts ^X'
         TEXT2='origin ^XO'
         TEXT3='delta       1'
      END IF
      IF (NDIM.EQ.2) THEN
         TEXT1='object 1 class gridpositions counts ^Y ^X'
         TEXT2='origin ^XO ^YO '
         TEXT3='delta       0      1'
         TEXT4='delta       1      0'
      END IF
      IF (NDIM.EQ.3) THEN
         TEXT1='object 1 class gridpositions counts ^Z ^Y ^X'
         TEXT2='origin ^XO ^YO ^ZO'
         TEXT3='delta       0      0      1'
         TEXT4='delta       0      1      0'
         TEXT5='delta       1      0      0'
      END IF

*   Format image size information.
      IF (DEP) THEN
         SUB=0
      ELSE
         SUB=1
      END IF
      CALL MSG_FMTI('X','I6',PRANGE(1)+SUB)
      CALL MSG_FMTI('Y','I6',PRANGE(2)+SUB)
      CALL MSG_FMTI('Z','I6',PRANGE(3)+SUB)

*   Write size data.
      NC=0
      CALL MSG_LOAD(' ',TEXT1,LINE,NC,STATUS)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

*   Format grid origin information.
      IF (DEP) THEN
         CALL MSG_FMTR('XO','F9.4',LBND(1)-0.5)
         CALL MSG_FMTR('YO','F9.4',LBND(2)-0.5)
         CALL MSG_FMTR('ZO','F9.4',LBND(3)-0.5)
      ELSE
         CALL MSG_FMTR('XO','F9.4',LBND(1)-1.0)
         CALL MSG_FMTR('YO','F9.4',LBND(2)-1.0)
         CALL MSG_FMTR('ZO','F9.4',LBND(3)-1.0)
      END IF

*   Write origin information.
      NC=0
      CALL MSG_LOAD(' ',TEXT2,LINE,NC,STATUS)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)

*   Write the axis edge increments.
      NC=0
      CALL CHR_PUTC(TEXT3,LINE,NC)
      CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)
      IF (NDIM.GT.1) THEN
         NC=0
         CALL CHR_PUTC(TEXT4,LINE,NC)
         CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)
      END IF
      IF (NDIM.EQ.3) THEN
         NC=0
         CALL CHR_PUTC(TEXT5,LINE,NC)
         CALL NDF2DX_WRICH(BMODE,FIOD,LINE(1:NC),STATUS)
      END IF
      IF(STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

      END


      SUBROUTINE NDF2DX_AXIS(NDF1,NDIM,PRANGE,PNTRX,PNTRY,PNTRZ,
     :                       SZX,SZY,SZZ,STATUS)


*+
*  Name:
*     NDF2DX_AXIS

*  Purpose:
*     Map the axis information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_AXIS(NDF1,NDIM,PRANGE,PNTRX,PNTRY,PNTRZ,
*                      SZX,SZY,SZZ,STATUS)

*  Description:
*     Maps the axis arrays for the correct number of dimensions
*     and then checks to see if the values for axis length returned
*     agree with those expected.

*  Arguments:
*     NDF1 = INTEGER (Given)
*        Identifier for the opened NDF.
*     NDIM = INTEGER (Given)
*        The number of dimensions.
*     PRANGE ( NDF__MXDIM ) = INTEGER (Given)
*        The length of each image axis in pixels.
*     PNTRX(3) =INTEGER (Returned)
*        Pointer to the axis information
*     PNTRY(3) =INTEGER (Returned)
*        Pointer to the axis information
*     PNTRZ(3) =INTEGER (Returned)
*        Pointer to the axis information.
*     SZX = INTEGER (Returned)
*        Sixe of the x array.
*     SZY = INTEGER (Returned)
*        Sixe of the y array.
*     SZZ = INTEGER (Returned)
*        Sixe of the z array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-OCT-1995 (GJP)
*        Original version.
*     28-SEP-2006 (DSB)
*        Store correct values (1) for unused 2nd or 3rd axis lengths.

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF public constants

*  Arguments Given:
      INTEGER NDF1                    ! NDF identifier
      INTEGER NDIM                    ! Number of dimensions
      INTEGER PRANGE(NDF__MXDIM)      ! Expected size of axis

*  Arguments Returned:
      INTEGER PNTRX(3)                ! Pointer to mapped axis
      INTEGER PNTRY(3)                ! Pointer to mapped axis
      INTEGER PNTRZ(3)                ! Pointer to mapped axis
      INTEGER SZX                     ! Size of X axis
      INTEGER SZY                     ! Size of Y axis
      INTEGER SZZ                     ! Size of Z axis

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Initialise.
      SZX = 1
      SZY = 1
      SZZ = 1

*   One dimensional.
      IF (NDIM.EQ.1) THEN
         CALL NDF_AMAP(NDF1,'Centre,Width',1,'_REAL','READ',
     :                 PNTRX,SZX,STATUS)
      END IF
      IF(STATUS.NE.SAI__OK) GOTO 9999

*   Two dimensional.
      IF (NDIM.EQ.2) THEN
         CALL NDF_AMAP(NDF1,'Centre,Width',1,'_REAL','READ',
     :                 PNTRX,SZX,STATUS)
         CALL NDF_AMAP(NDF1,'Centre,Width',2,'_REAL','READ',
     :                 PNTRY,SZY,STATUS)
      END IF
      IF(STATUS.NE.SAI__OK) GOTO 9999

*   Three dimensional.
      IF (NDIM.EQ.3) THEN
         CALL NDF_AMAP(NDF1,'Centre,Width',1,'_REAL','READ',
     :                 PNTRX,SZX,STATUS)
         CALL NDF_AMAP(NDF1,'Centre,Width',2,'_REAL','READ',
     :                 PNTRY,SZY,STATUS)
         CALL NDF_AMAP(NDF1,'Centre,Width',3,'_REAL','READ',
     :                 PNTRZ,SZZ,STATUS)
      END IF
      IF(STATUS.NE.SAI__OK) GOTO 9999

*   Check to see if the dimensions of each axis found
*   agree with those anticipated.
      IF ( (SZX.NE.PRANGE(1))
     :   .OR.(SZY.NE.PRANGE(2)).OR.(SZZ.NE.PRANGE(3)) ) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Axis component corrupted.',STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END



      SUBROUTINE NDF2DX_WRIAX1(DEP,BMODE,FIOD,AXISX,AXWIDX,
     :                         SZX,STATUS)
*+
*  Name:
*     NDF2DX_WRIAX1

*  Purpose:
*     Writes the co-ordinate information in a 1-D file to the DX file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_WRIAX1(DEP,BMODE,FIOD,AXISX,AXWIDX,SZX,STATUS)

*  Description:
*     Uses the values in the axis arrays to provide a position
*     for each pixel.

*  Arguments:
*     DEP = LOGICAL (Given)
*        Is the data position dependant?
*     BMODE = LOGICAL (Given)
*        Is the data to be saved in binary mode?
*     FIOD = INTEGER (Given)
*        The device number.
*     AXISX(SZX) = REAL (Given)
*        The x axis information.
*     AXWIDX(SZX) = REAL (Given)
*        The x axis width information.
*     SZX = INTEGER (Given)
*        Sixe of the x array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-OCT-1995 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'MSG_PAR'               ! Parameter system constants

*  Arguments Given:
      INTEGER FIOD                    ! Device number
      INTEGER SZX                     ! Number of X axis elements
      REAL AXISX(SZX)                 ! X axis information
      REAL AXWIDX(SZX)                ! X axis width information

*  Status:
      INTEGER STATUS                  ! Global status

*  External functions:
      INTEGER FWRITER                 ! C function

*  Local variables:
      CHARACTER *(200) LINE           ! FIO line output length
      LOGICAL BMODE                   ! Is the data to be saved as binary?
      LOGICAL DEP                   ! Is the data position dependant?
      INTEGER I                       ! Loop counter
      INTEGER NC                      ! Character counter
      INTEGER NV                      ! Number of values written
      INTEGER STATUS2                 ! C function status
      INTEGER SUB                     ! Tmporary variable
      REAL OUTP(1)                    ! Output array
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set temporary variable.
      IF (DEP) THEN
         SUB=0
      ELSE
         SUB=1
      END IF

*   Set default C function status value.
      STATUS2=1

*   Set number of points to be written.
      NV=1

*   Write out the axis information.
      DO 500 I=1,SZX+SUB

*      Setup the output array.
         IF (I.LE.SZX) THEN
            OUTP(1)=AXISX(I)-AXWIDX(I)/2.*SUB
         ELSE
            OUTP(1)=AXISX(I)+AXWIDX(I)/2.
         END IF

*      Choose the manner in which data is to be stored.

         IF (BMODE) THEN

*         Binary.
            STATUS2=FWRITER(OUTP,NV)

         ELSE

*         ASCII.

            NC=0
            CALL CHR_PUTR(OUTP(1),LINE,NC)
            CALL FIO_WRITE(FIOD,LINE(1:NC),STATUS)

         END IF

 500  CONTINUE

*   Cope with the C function error message.
      IF ((BMODE).AND.(STATUS2.EQ.0)) THEN
         STATUS=SAI__ERROR
            CALL ERR_REP(' ','Failed writing axis information.',
     :                   STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE NDF2DX_WRIAX2(DEP,BMODE,FIOD,AXISX,AXWIDX,SZX,
     :                         AXISY,AXWIDY,SZY,STATUS)
*+
*  Name:
*     NDF2DX_WRIAX2

*  Purpose:
*     Writes the co-ordinate information in a 2-D file to the DX file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_WRIAX2(DEP,BMODE,FIOD,AXISX,AXWIDX,SZX,
*                        AXISY,AXWIDY,SZY,STATUS)

*  Description:
*     Uses the values in the axis arrays to provide a position
*     for each pixel.

*  Arguments:
*     DEP = LOGICAL (Given)
*        Is the data position dependant?
*     BMODE = LOGICAL (Given)
*        Is the data to be saved in binary mode?
*     FIOD = INTEGER (Given)
*        Device number.
*     AXISX(SZX) = REAL (Given)
*        The x axis information.
*     AXISY(SZY) = REAL (Given)
*        The x axis information.
*     AXWIDX(SZX) = REAL (Given)
*        The x axis width information.
*     AXWIDY(SZY) = REAL (Given)
*        The x axis width information.
*     SZX = INTEGER (Given)
*        Sixe of the x array.
*     SZY = INTEGER (Given)
*        Sixe of the y array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-OCT-1995 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'MSG_PAR'               ! Parameter system constants

*  Arguments Given:
      LOGICAL BMODE                   ! Is data to be saved as binary
      LOGICAL DEP                   ! Is data position dependant
      INTEGER SZX                     ! Number of X axis elements
      INTEGER SZY                     ! Number of Y axis elements
      REAL AXISX(SZX)                 ! X axis information
      REAL AXISY(SZY)                 ! Y axis information
      REAL AXWIDX(SZX)                ! X axis width information
      REAL AXWIDY(SZY)                ! Y axis width information

*  Status:
      INTEGER STATUS                  ! Global status

*  External functions:
      INTEGER FWRITER                 ! C function

*  Local variables:
      CHARACTER *(200) LINE           ! Final output
      CHARACTER *(256) LINEX          ! FIO line output
      CHARACTER *(256) LINEY          ! FIO line output
      INTEGER FIOD                    ! Device number
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Loop variable
      INTEGER NCX                     ! Character counter
      INTEGER NCY                     ! Character counter
      INTEGER NV                      ! Number of values to output
      INTEGER STATUS2                 ! C function status
      INTEGER SUB                     ! Temporary variable
      REAL OUTP(2)                    ! Output array
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set default C function status value.
      STATUS2=1

*   Set temporary variable.
      IF (DEP) THEN
         SUB=0
      ELSE
         SUB=1
      END IF

*   Choose the manner in which data is to be stored.

      IF(BMODE) THEN

*      Binary.

*      Loop through all x and y values.
         NV=2
         DO 50 I=1,SZY+SUB

*         Y axis value.
            IF (I.LE.SZY) THEN
               OUTP(2)=AXISY(I)-AXWIDY(I)/2.*SUB
            ELSE
               OUTP(2)=AXISY(I)+AXWIDY(I)/2.
            END IF

            DO 60 J=1,SZX+SUB

*            X axis value.
              IF (J.LE.SZX) THEN
                 OUTP(1)=AXISX(J)-AXWIDX(J)/2.*SUB
               ELSE
                 OUTP(1)=AXISX(J+AXWIDX(J))/2.
               END IF
               STATUS2=FWRITER(OUTP,NV)

 60         CONTINUE
 50      CONTINUE

      ELSE

*      ASCII

*      Loop through all x and y values.
         DO 500 I=1,SZY+SUB

*         Get a string for the Y value.
            NCY=0
            IF (I.LE.SZY) THEN
                CALL CHR_PUTR((AXISY(I)-AXWIDY(I)/2.*SUB),
     :                         LINEY,NCY)
            ELSE
                CALL CHR_PUTR(AXISY(I),LINEY,NCY)
            END IF

            DO 600 J=1,SZX+SUB

*            Get a string for the X value.
               NCX=0
               IF (J.LE.SZX) THEN
                  CALL CHR_PUTR((AXISX(J)-AXWIDX(J)/2.*SUB),
     :                          LINEX,NCX)
               ELSE
                  CALL CHR_PUTR(AXISX(J)+AXWIDX(J)/2.,LINEX,NCX)
               END IF

*            Write out the results.
               LINE=LINEX(1:NCX)//' '//LINEY(1:NCY)
               CALL FIO_WRITE(FIOD,LINE(1:(NCX+NCY+1)),
     :                        STATUS)

 600        CONTINUE
 500     CONTINUE

      END IF

*   Cope with the C function error message.
      IF ((BMODE).AND.(STATUS2.EQ.0)) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Failed writing axis information.',
     :                STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE NDF2DX_WRIAX3(DEP,BMODE,FIOD,
     :                         AXISX,AXWIDX,SZX,
     :                         AXISY,AXWIDY,SZY,
     :                         AXISZ,AXWIDZ,SZZ,
     :                         STATUS)
*+
*  Name:
*     NDF2DX_WRIAX

*  Purpose:
*     Writes the co-ordinate information in a 3-D file to the DX file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_WRIAX3(DEP,BMODE,FIOD,AXISX,AXWIDX,SZX,
*                        AXISY,AXWIDY,SZY,
*                        AXISZ,AXWIDZ,SZZ,
*                        STATUS)

*  Description:
*     Uses the values in the axis arrays to provide a position
*     for each pixel.

*  Arguments:
*     DEP = LOGICAL (Given)
*        Is the data position dependant?
*     BMODE = LOGICAL (Given)
*        Is the data to be saved in binary mode?
*     FIOD = INTEGER (Given)
*        Device number.
*     AXISX(SZX) = REAL (Given)
*        The x axis information.
*     AXISY(SZY) = REAL (Given)
*        The x axis information.
*     AXISY(SZZ) = REAL (Given)
*        The x axis information.
*     AXISWIDX(SZX) = REAL (Given)
*        The x axis width information.
*     AXISWIDY(SZY) = REAL (Given)
*        The x axis width information.
*     AXISWIDY(SZZ) = REAL (Given)
*        The x axis width information.
*     SZX = INTEGER (Given)
*        Sixe of the x array.
*     SZY = INTEGER (Given)
*        Sixe of the y array.
*     SZZ = INTEGER (Given)
*        Sixe of the z array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-OCT-1995 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'MSG_PAR'               ! Parameter system constants

*  Arguments Given:
      LOGICAL BMODE                   ! Is the data to be saved as binary
      LOGICAL DEP                   ! Is the data position dependant
      INTEGER FIOD                    ! Device number
      INTEGER SZX                     ! Number of X axis elements
      INTEGER SZY                     ! Number of Y axis elements
      INTEGER SZZ                     ! Number of Z axis elements
      REAL AXISX(SZX)                 ! X axis information
      REAL AXISY(SZY)                 ! Y axis information
      REAL AXISZ(SZZ)                 ! Z axis information
      REAL AXWIDX(SZX)                ! X axis information
      REAL AXWIDY(SZY)                ! Y axis information
      REAL AXWIDZ(SZZ)                ! Z axis information

*  Status:
      INTEGER STATUS                  ! Global status

*  External functions:
      INTEGER FWRITER                 ! C function

*  Local variables:
      CHARACTER *(200) LINE           ! Output line
      CHARACTER *(256) LINEX          ! FIO line output
      CHARACTER *(256) LINEY          ! FIO line output
      CHARACTER *(256) LINEZ          ! FIO line output
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Loop variable
      INTEGER K                       ! Loop variable
      INTEGER NCX                     ! Character counter
      INTEGER NCY                     ! Character counter
      INTEGER NCZ                     ! Character counter
      INTEGER NV                      ! Number of values
      INTEGER STATUS2                 ! C function status
      INTEGER SUB                     ! Temporary variable
      REAL OUTP(3)                    ! Output array

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set default C function status value.
      STATUS2=1

*   Format image size information.
      IF (DEP) THEN
         SUB=0
      ELSE
         SUB=1
      END IF

*   Choose the manner in which data is to be stored.

      IF (BMODE) THEN

*      Binary
*      Loop through all x, y and z values.
         NV=3
         DO 50 I=1,SZZ+SUB

*         Z axis.
            IF (I.LE.SZZ) THEN
               OUTP(3)=AXISZ(I)-AXWIDZ(I)/2.*SUB
            ELSE
               OUTP(3)=AXISZ(I)+AXWIDZ(I)/2.
            END IF

            DO 60 J=1,SZY+SUB

*            Y axis.
               IF (J.LE.SZY) THEN
                  OUTP(2)=AXISY(J)-AXWIDY(J)/2.*SUB
               ELSE
                  OUTP(2)=AXISY(J)+AXWIDY(J)/2.
               END IF

               DO 70 K=1,SZX+SUB

*               X axis.
                  IF (K.LE.SZX) THEN
                     OUTP(1)=AXISX(K)-AXWIDX(K)/2.*SUB
                  ELSE
                     OUTP(1)=AXISX(K)+AXWIDX(K)/2.
                  END IF

                  STATUS2=FWRITER(OUTP,NV)

 70            CONTINUE
 60         CONTINUE
 50      CONTINUE

      ELSE

*      ASCII

*      Loop through all x,y and z values.
         DO 500 I=1,SZZ+SUB

*         Get a string for the Z value.
            NCZ=0
            IF (I.LE.SZZ) THEN
               CALL CHR_PUTR((AXISZ(I)-AXWIDZ(I)/2.*SUB),
     :                       LINEZ,NCZ)
            ELSE
               CALL CHR_PUTR(AXISZ(I)+AXWIDZ(I)/2.,LINEZ,NCZ)
            END IF

            DO 600 J=1,SZY+SUB

*            Get a string for the Y value.
               NCY=0
               IF (J.LE.SZY) THEN
                  CALL CHR_PUTR((AXISY(J)-AXWIDY(J)/2.*SUB),
     :                           LINEY,NCY)
               ELSE
                  CALL CHR_PUTR(AXISY(J)+AXWIDY(J)/2.,LINEY,NCY)
               END IF

               DO 700 K=1,SZX+SUB

*               Get a string for the X value.
                  NCX=0
                  IF (K.LE.SZX) THEN
                     CALL CHR_PUTR((AXISX(K)-AXWIDX(K)/2.*SUB),
     :                             LINEX,NCX)
                  ELSE
                     CALL CHR_PUTR(AXISX(K)+AXWIDX(K)/2.,LINEX,NCX)
                  END IF

*               Write out the results.
                  LINE=LINEX(1:NCX)//' '//LINEY(1:NCY)//
     :                 ' '//LINEZ(1:NCZ)
                  CALL FIO_WRITE(FIOD,LINE(1:
     :                 (NCX+NCY+NCZ+2)),STATUS)

 700           CONTINUE
 600        CONTINUE
 500     CONTINUE

      END IF

*   Cope with the C function error message.
      IF ((BMODE).AND.(STATUS2.EQ.0)) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Failed writing axis information.',
     :                STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE NDF2DX_WRIDATI(BMODE,FIOD,DARRAY,ELEMS,STATUS)
*+
*  Name:
*     NDF2DX_WRIDATI

*  Purpose:
*     Writes the data/variance component information to the file.
*     Used for data mapped as INTEGER.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_WRIDATI(BMODE,FIOD,DARRAY,ELEMS,STATUS)

*  Description:
*     Writes the component values to the file as ASCII or binary values.
*     ASCII sent in bunches of 10.

*  Arguments:
*     BMODE = LOGICAL (Given)
*        Is the data to be saved in binary mode?
*     FIOD = INTEGER (Given)
*        The device number.
*     DARRAY(ELEMS) = INTEGER (Given)
*        The x axis information.
*     ELEMS= INTEGER (Given)
*        Sixe of the x array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-OCT-1995 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'MSG_PAR'               ! Parameter system constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      LOGICAL BMODE                   ! Is the data to be saved as binary
      INTEGER FIOD                    ! Device number
      INTEGER ELEMS                   ! Number of elements
      INTEGER DARRAY(ELEMS)           ! X axis information

*  Status:
      INTEGER STATUS                  ! Global status

*  External functions:
      INTEGER FWRITEI                 ! C function

*  Local variables:
      CHARACTER *(200) LINE           ! FIO line output length
      INTEGER I                       ! Loop counter
      INTEGER J                       ! Loop counter
      INTEGER NC                      ! Character counter
      INTEGER NV                      ! Number of values written
      INTEGER OUTP(1)                 ! Output array
      INTEGER STATUS2                 ! C function status
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set default C function status value.
      STATUS2=1

*   Choose the manner in which data is to be stored.

      IF(BMODE) THEN

*      Binary
         NV=1
         DO 10 I=1,ELEMS

*         Check for bad values.
            IF(DARRAY(I).EQ.VAL__BADI) THEN
               OUTP(1)=0
            ELSE
               OUTP(1)=DARRAY(I)
            END IF

*         Write the value.
            STATUS2=FWRITEI(OUTP,NV)

 10      CONTINUE

      ELSE

*      ASCII

*      Loop for all values.
         I=1
         DO WHILE((I.LE.ELEMS).AND.(STATUS.EQ.SAI__OK))

*         Output 10 values at a time.
            LINE=' '
            NC=0
            J=1
            DO WHILE((J.LE.10).AND.(I.LE.ELEMS))

*            Construct a line.
               IF (DARRAY(I).EQ.VAL__BADI) THEN
                 CALL CHR_PUTI(0,LINE,NC)
               ELSE
                 CALL CHR_PUTI(DARRAY(I),LINE,NC)
               END IF
               LINE=LINE(1:NC)//' '
               NC=NC+1
               I=I+1
               J=J+1

            END DO

*         Write out the resultant string.
            CALL FIO_WRITE(FIOD,LINE(1:NC-1),STATUS)

         END DO

      END IF

*   Cope with the C function error message.
      IF ((BMODE).AND.(STATUS2.EQ.0)) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Failed writing data.',
     :                STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE NDF2DX_WRIDATR(BMODE,FIOD,DARRAY,ELEMS,STATUS)
*+
*  Name:
*     NDF2DX_WRIDATR

*  Purpose:
*     Writes the data/variance component information to the file.
*     Used for data mapped as REAL.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_WRIDATR(BMODE,FIOD,DARRAY,ELEMS,STATUS)

*  Description:
*     Writes the component values to the file as ASCII or binary values.
*     ASCII sent in bunches of 10.

*  Arguments:
*     BMODE = LOGICAL (Given)
*        Is the data to be saved in binary mode?
*     FIOD = INTEGER (Given)
*        The device number.
*     DARRAY(ELEMS) = REAL (Given)
*        The x axis information.
*     ELEMS= INTEGER (Given)
*        Sixe of the x array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-OCT-1995 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'MSG_PAR'               ! Parameter system constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      LOGICAL BMODE                   ! Is the data to be saved as binary
      INTEGER FIOD                    ! Device number
      INTEGER ELEMS                   ! Number of elements
      REAL DARRAY(ELEMS)              ! X axis information

*  Status:
      INTEGER STATUS                  ! Global status

*  External functions:
      INTEGER FWRITER                 ! C function

*  Local variables:
      CHARACTER *(200) LINE           ! FIO line output length
      INTEGER I                       ! Loop counter
      INTEGER J                       ! Loop counter
      INTEGER NC                      ! Character counter
      INTEGER NV                      ! Number of values written
      INTEGER STATUS2                 ! C function status
      REAL OUTP(1)                    ! Output array
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set default C function status value.
      STATUS2=1

*   Choose the manner in which data is to be stored.

      IF(BMODE) THEN

*      Binary
         NV=1
         DO 10 I=1,ELEMS

*         Check for bad values.
            IF(DARRAY(I).EQ.VAL__BADR) THEN
               OUTP(1)=0.0
            ELSE
               OUTP(1)=DARRAY(I)
            END IF

*         Write out the value
            STATUS2=FWRITER(OUTP,NV)

 10      CONTINUE

      ELSE

*      ASCII

*      Loop for all values.
         I=1
         DO WHILE((I.LE.ELEMS).AND.(STATUS.EQ.SAI__OK))

*         Output 10 values at a time.
            LINE=' '
            NC=0
            J=1
            DO WHILE((J.LE.10).AND.(I.LE.ELEMS))

*            Construct a line.
               IF (DARRAY(I).EQ.VAL__BADR) THEN
                  CALL CHR_PUTR(0.0,LINE,NC)
               ELSE
                  CALL CHR_PUTR(DARRAY(I),LINE,NC)
               END IF
               LINE=LINE(1:NC)//' '
               NC=NC+1
               I=I+1
               J=J+1

            END DO

*         Write out the resultant string.
            CALL FIO_WRITE(FIOD,LINE(1:NC-1),STATUS)

         END DO

      END IF

*   Cope with the C function error message.
      IF ((BMODE).AND.(STATUS2.EQ.0)) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Failed writing data.',
     :                STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE NDF2DX_WRIDATD(BMODE,FIOD,DARRAY,ELEMS,STATUS)
*+
*  Name:
*     NDF2DX_WRIDATD

*  Purpose:
*     Writes the data/variance component information to the file.
*     Used for data mapped as DOUBLE PRECISION.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_WRIDATD(BMODE,FIOD,DARRAY,ELEMS,STATUS)

*  Description:
*     Writes the component values to the file as ASCII or binary values.
*     ASCII sent in bunches of 10.

*  Arguments:
*     BMODE = LOGICAL (Given)
*        Is the data to be saved in binary mode?
*     FIOD = INTEGER (Given)
*        The device number.
*     DARRAY(ELEMS) = REAL (Given)
*        The x axis information.
*     ELEMS= INTEGER (Given)
*        Sixe of the x array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     12-OCT-1995 (GJP)
*       (Original version)
*     29-AUG-2004 (TIMJ)
*       Make sure that CHR_PUTD takes a DOUBLE argument.

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'MSG_PAR'               ! Parameter system constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      LOGICAL BMODE                   ! Is the data to be saved as binary
      INTEGER FIOD                    ! Device number
      INTEGER ELEMS                   ! Number of elements
      DOUBLE PRECISION DARRAY(ELEMS)  ! X axis information

*  Status:
      INTEGER STATUS                  ! Global status

*  External functions:
      INTEGER FWRITED                 ! C function

*  Local variables:
      CHARACTER *(200) LINE           ! FIO line output length
      INTEGER I                       ! Loop counter
      INTEGER J                       ! Loop counter
      INTEGER NC                      ! Character counter
      INTEGER NV                      ! Number of values written
      INTEGER STATUS2                 ! C function status
      DOUBLE PRECISION OUTP(1)        ! Output array
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set default C function status value.
      STATUS2=1

      IF(BMODE) THEN

*      Binary
         NV=1
         DO 10 I=1,ELEMS

*         Check for bad values.
            IF (DARRAY(I).EQ.VAL__BADD) THEN
               OUTP(1)=0.0
            ELSE
               OUTP(1)=DARRAY(I)
            END IF

*         Write out the value.
            STATUS2=FWRITED(OUTP,NV)

 10      CONTINUE

      ELSE

*      ASCII

*      Loop for all values.
         I=1
         DO WHILE((I.LE.ELEMS).AND.(STATUS.EQ.SAI__OK))

*         Output 10 values at a time.
            LINE=' '
            NC=0
            J=1
            DO WHILE((J.LE.10).AND.(I.LE.ELEMS))

*            Construct a line.
               IF (DARRAY(I).EQ.VAL__BADD) THEN
                  CALL CHR_PUTD(0.0D0,LINE,NC)
               ELSE
                  CALL CHR_PUTD(DARRAY(I),LINE,NC)
               END IF
               LINE=LINE(1:NC)//' '
               NC=NC+1
               I=I+1
               J=J+1

            END DO

*         Write out the resultant string.
            CALL FIO_WRITE(FIOD,LINE(1:NC-1),STATUS)

         END DO

      END IF

*   Cope with the C function error message.
      IF ((BMODE).AND.(STATUS2.EQ.0)) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Failed writing data.',
     :                STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE NDF2DX_WRIBADI(BMODE,FIOD,DARRAY,VARRAY,ELEMS,
     :                          STATUS)
*+
*  Name:
*     NDF2DX_WRIBADI

*  Purpose:
*     Writes the invalid position information to the file.
*     Used for data mapped as INTEGER.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_WRIBADI(BMODE,FIOD,DARRAY,VARRAY,ELEMS,STATUS)

*  Description:
*     Writes out a position for an invalid pixel.

*  Arguments:
*     BMODE = LOGICAL (Given)
*        Is the data to be saved in binary mode?
*     FIOD = INTEGER (Given)
*        The device number.
*     DARRAY(ELEMS) = INTEGER (Given)
*        The data information.
*     VARRAY(ELEMS) = INTEGER (Given)
*        The variance information.
*     ELEMS= INTEGER (Given)
*        Number of pixels in the image.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-OCT-1995 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'MSG_PAR'               ! Parameter system constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      LOGICAL BMODE                   ! Is the data to be stored as binary
      INTEGER FIOD                    ! Device number
      INTEGER ELEMS                   ! Number of elements
      INTEGER DARRAY(ELEMS)           ! Data information
      INTEGER VARRAY(ELEMS)           ! Variance information

*  Status:
      INTEGER STATUS                  ! Global status

*  External functions:
      INTEGER FWRITEI                 ! C function

*  Local variables:
      CHARACTER *(200) LINE           ! FIO line output length
      INTEGER I                       ! Loop counter
      INTEGER NC                      ! Character counter
      INTEGER STATUS2                 ! C function status
      INTEGER OUTP(1)                 ! Output values
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set default C function status value.
      STATUS2=1

*   Write out the axis information for bad pixels.
      DO 500 I=1,ELEMS

*      Test for bad pixels.
         IF ((DARRAY(I).EQ.VAL__BADI).OR.(VARRAY(I).EQ.VAL__BADI))
     :                                    THEN

*         Check how to save the data.
            IF (BMODE) THEN

*            Binary.
               OUTP(1)=I-1
               STATUS2=FWRITEI(OUTP,1)

            ELSE

*            ASCII.

*            Create the string.
               NC=0
               CALL CHR_PUTI((I-1),LINE,NC)

*            Write it.
               CALL FIO_WRITE(FIOD,LINE(1:NC),STATUS)

            END IF

         END IF

 500  CONTINUE

*   Cope with the C function error message.
      IF ((BMODE).AND.(STATUS2.EQ.0)) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Failed writing bad pixel data.',
     :                STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE NDF2DX_WRIBADR(BMODE,FIOD,DARRAY,VARRAY,ELEMS,
     :                          STATUS)
*+
*  Name:
*     NDF2DX_WRIBADR

*  Purpose:
*     Writes the invalid position information to the file.
*     Used for data mapped as REAL.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_WRIBADR(BMODE,FIOD,DARRAY,VARRAY,ELEMS,STATUS)

*  Description:
*     Writes out a position for an invalid pixel.

*  Arguments:
*     BMODE = LOGICAL (Given)
*        Is the data to be saved in binary mode?
*     FIOD = INTEGER (Given)
*        The device number.
*     DARRAY(ELEMS) = REAL (Given)
*        The data information.
*     VARRAY(ELEMS) = REAL (Given)
*        The variance information.
*     ELEMS= INTEGER (Given)
*        Number of pixels in the image.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-OCT-1995 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'MSG_PAR'               ! Parameter system constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      LOGICAL BMODE                   ! Is the data to be stored as binary
      INTEGER FIOD                    ! Device number
      INTEGER ELEMS                   ! Number of elements
      REAL DARRAY(ELEMS)              ! Data information
      REAL VARRAY(ELEMS)              ! Variance information

*  Status:
      INTEGER STATUS                  ! Global status

*  External functions:
      INTEGER FWRITEI                 ! C function

*  Local variables:
      CHARACTER *(200) LINE           ! FIO line output length
      INTEGER I                       ! Loop counter
      INTEGER NC                      ! Character counter
      INTEGER STATUS2                 ! C function status
      INTEGER OUTP(1)                 ! Output values
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set default C function status value.
      STATUS2=1

*   Write out the axis information for bad pixels.
      DO 500 I=1,ELEMS

*      Test for bad pixels.
         IF ((DARRAY(I).EQ.VAL__BADR).OR.(VARRAY(I).EQ.VAL__BADR))
     :                                    THEN

*         Check how to save the data.
            IF (BMODE) THEN

*            Binary.
               OUTP(1)=I-1
               STATUS2=FWRITEI(OUTP,1)

            ELSE

*            ASCII.

*            Create the string.
               NC=0
               CALL CHR_PUTI((I-1),LINE,NC)

*            Write it.
               CALL FIO_WRITE(FIOD,LINE(1:NC),STATUS)

            END IF

         END IF

 500  CONTINUE

*   Cope with the C function error message.
      IF ((BMODE).AND.(STATUS2.EQ.0)) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Failed writing bad pixel data.',
     :                STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE NDF2DX_WRIBADD(BMODE,FIOD,DARRAY,VARRAY,ELEMS,
     :                          STATUS)
*+
*  Name:
*     NDF2DX_WRIBADD

*  Purpose:
*     Writes the invalid position information to the file.
*     Used for data mapped as DOUBLE PRECISION.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_WRIBADR(BMODE,FIOD,DARRAY,VARRAY,ELEMS,STATUS)

*  Description:
*     Writes out a position for an invalid pixel.

*  Arguments:
*     BMODE = LOGICAL (Given)
*        Is the data to be saved in binary mode?
*     FIOD = INTEGER (Given)
*        The device number.
*     DARRAY(ELEMS) = DOUBLE PRECISION (Given)
*        The data information.
*     VARRAY(ELEMS) = DOUBLE PRECISION (Given)
*        The variance information.
*     ELEMS= INTEGER (Given)
*        Number of pixels in the image.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-OCT-1995 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'MSG_PAR'               ! Parameter system constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      LOGICAL BMODE                   ! Is the data to be stored as binary
      INTEGER FIOD                    ! Device number
      INTEGER ELEMS                   ! Number of elements
      DOUBLE PRECISION DARRAY(ELEMS)  ! Data information
      DOUBLE PRECISION VARRAY(ELEMS)  ! Variance information

*  Status:
      INTEGER STATUS                  ! Global status

*  External functions:
      INTEGER FWRITEI                 ! C function

*  Local variables:
      CHARACTER *(200) LINE           ! FIO line output length
      INTEGER I                       ! Loop counter
      INTEGER NC                      ! Character counter
      INTEGER STATUS2                 ! C function status
      INTEGER OUTP(1)                 ! Output values
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set default C function status value.
      STATUS2=1

*   Write out the axis information for bad pixels.
      DO 500 I=1,ELEMS

*      Test for bad pixels.
         IF ((DARRAY(I).EQ.VAL__BADD).OR.(VARRAY(I).EQ.VAL__BADD))
     :                                    THEN

*         Check how to save the data.
            IF (BMODE) THEN

*            Binary.
               OUTP(1)=I-1
               STATUS2=FWRITEI(OUTP,1)

            ELSE

*            ASCII.

*            Create the string.
               NC=0
               CALL CHR_PUTI((I-1),LINE,NC)

*            Write it.
               CALL FIO_WRITE(FIOD,LINE(1:NC),STATUS)

            END IF

         END IF

 500  CONTINUE

*   Cope with the C function error message.
      IF ((BMODE).AND.(STATUS2.EQ.0)) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Failed writing bad pixel data.',
     :                STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE NDF2DX_CBADI(DARRAY,VARRAY,ELEMS,COUNT,STATUS)
*+
*  Name:
*     NDF2DX_CBADI

*  Purpose:
*     Count the number of bad pixels in the file.
*     Used for data mapped as INTEGER.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_CBADI(DARRAY,VARRAY,ELEMS,COUNT,STATUS)

*  Description:
*     Looks through the array and checks for the BAD value
*     incrementing a counter when it is found.

*  Arguments:
*     DARRAY(ELEMS) = INTEGER (Given)
*        The data information.
*     VARRAY(ELELMS) = INTEGER (Given)
*        The variance information.
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     COUNT = INTEGER (Returned)
*        Number of bad pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-OCT-1995 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER ELEMS                   ! Number of elements
      INTEGER DARRAY(ELEMS)           ! The image
      INTEGER VARRAY(ELEMS)           ! The variance

*  Arguments returned:
      INTEGER COUNT                   ! Number of bad pixels

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Loop counter


*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Initialise the counter.
      COUNT=0

*   Write out the axis information for bad pixels.
      DO 100 I=1,ELEMS

         IF ((DARRAY(I).EQ.VAL__BADI).OR.(VARRAY(I).EQ.VAL__BADI))
     :        COUNT=COUNT+1

 100  CONTINUE

 9999 CONTINUE

      END



      SUBROUTINE NDF2DX_CBADR(DARRAY,VARRAY,ELEMS,COUNT,STATUS)
*+
*  Name:
*     NDF2DX_CBADR

*  Purpose:
*     Count the number of bad pixels in the file.
*     Used for data mapped as REAL.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_CBADR(DARRAY,VARRAY,ELEMS,COUNT,STATUS)

*  Description:
*     Looks through the array and checks for the BAD value
*     incrementing a counter when it is found.

*  Arguments:
*     DARRAY(ELEMS) = REAL (Given)
*        The data information.
*     VARRAY(ELEMS) = REAL (Given)
*        The variance information.
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     COUNT = INTEGER (Returned)
*        Number of bad pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-OCT-1995 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER ELEMS                   ! Number of elements
      REAL DARRAY(ELEMS)              ! The image
      REAL VARRAY(ELEMS)              ! The image

*  Arguments returned:
      INTEGER COUNT                   ! Number of bad pixels

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Loop counter


*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Initialise the counter.
      COUNT=0

*   Write out the axis information for bad pixels.
      DO 100 I=1,ELEMS

         IF ((DARRAY(I).EQ.VAL__BADR).OR.(DARRAY(I).EQ.VAL__BADR))
     :        COUNT=COUNT+1

 100  CONTINUE

 9999 CONTINUE

      END



      SUBROUTINE NDF2DX_CBADD(DARRAY,VARRAY,ELEMS,COUNT,STATUS)
*+
*  Name:
*     NDF2DX_CBADD

*  Purpose:
*     Count the number of bad pixels in the file.
*     Used for data mapped as DOUBLE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_CBADD(DARRAY,VARRAY,ELEMS,COUNT,STATUS)

*  Description:
*     Looks through the array and checks for the BAD value
*     incrementing a counter when it is found.

*  Arguments:
*     DARRAY(ELEMS) = DOUBLE (Given)
*        The data information.
*     VARRAY(ELEMS) = DOUBLE (Given)
*        The variance information.
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     COUNT = INTEGER (Returned)
*        Number of bad pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-OCT-1995 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER ELEMS                   ! Number of elements
      DOUBLE PRECISION DARRAY(ELEMS)  ! The image
      DOUBLE PRECISION VARRAY(ELEMS)  ! The variance

*  Arguments returned:
      INTEGER COUNT                   ! Number of bad pixels

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Loop counter


*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Initialise the counter.
      COUNT=0

*   Write out the axis information for bad pixels.
      DO 100 I=1,ELEMS

         IF ((DARRAY(I).EQ.VAL__BADD).OR.(DARRAY(I).EQ.VAL__BADD))
     :        COUNT=COUNT+1

 100  CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE NDF2DX_WRICH(BMODE,FIOD,TEXT,STATUS)
*+
*  Name:
*     NDF2DX_WRICH

*  Purpose:

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_WRICH(BMODE,FIOD,TEXT,STATUS)

*  Description:

*  Arguments:
*     BMODE = LOGICAL (Given)
*        Is text to be written using FIO or fopen.
*     FIOD = INTEGER (Given)
*        FIO file identifier.
*     TEXT *(*) = CHARACTER (Given)
*        The string to be written.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-OCT-1995 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      CHARACTER*(*) TEXT              ! Output string
      LOGICAL BMODE                   ! How is data to be written out
      INTEGER FIOD                    ! FIO file identifier

*  External functions:
      INTEGER FWRITEC                 ! C function

*  Status:
      INTEGER STATUS                  ! Global status

*  External functions:
      INTEGER CHR_LEN                 ! String length

*  Local variables:
      INTEGER NC                      ! Number of characters
      INTEGER STATUS2                 ! C function status

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set default C function status value.
      STATUS2=1

*   Find the string length.
      NC=CHR_LEN(TEXT)

*   Select method to be used to output data.
      IF (BMODE) THEN

*      Binary ie fopen.
         STATUS2=FWRITEC(TEXT(1:NC))
         IF (STATUS2.EQ.0) STATUS=SAI__ERROR

      ELSE

*      ASCII ie FIO_WRITE.
         CALL FIO_WRITE(FIOD,TEXT(1:NC),STATUS)

      END IF

*   Cope with the C function error message.
      IF ((BMODE).AND.(STATUS2.EQ.0)) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Failed writing output string.',
     :                STATUS)
         GOTO 9999
      END IF

 9999 CONTINUE

      END







      SUBROUTINE NDF2DX_PTERM( AXIS, NAME, N, CENTRE, NDIM, BMODE, FIOD,
     :                         DEP,  STATUS )
*+
*  Name:
*     NDF2DX_PTERM

*  Purpose:
*     Write out an axis centre array as a regular or standard DX array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2DX_PTERM( AXIS, NAME, N, CENTRE, NDIM, BMODE, FIOD,
*                        DEP,  STATUS )

*  Description:
*     Create a regular or standard array as a term of the positions array.
*     NB, the current version assumes that pixels are contiguous and
*     therefore pixels widths are assumed to be equal to the distance
*     between pixel centres.

*  Arguments:
*     AXIS = INTEGER (Given)
*        The NDF axis number
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the DX array object to create
*     N = INTEGER (Given)
*        The number of values in the NDF axis centre array
*     CENTRE( N ) = REAL (Given)
*        The AXIS CENTRE array from the NDF
*     NDIM = INTEGER (Given)
*        The number of dimensions.
*     BMODE = LOGICAL (Given)
*        Save data as binary?
*     FIOD = INTEGER (Given)
*        Identifier for the opened FIO file (if used).
*     DEP = LOGICAL (Given)
*        Is data position dependant?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)

*  History:
*     1-DEC-1995 (DSB)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF public constants

*  Arguments Given:
      INTEGER AXIS                    ! The NDF AXIS number
      CHARACTER NAME*(*)              ! The name of the term
      INTEGER N                       ! Size of NDF axis arrays
      REAL CENTRE(N)                  ! The axis centre array
      INTEGER NDIM                    ! Number of dimensions
      LOGICAL BMODE                   ! Is data saved as binary
      INTEGER FIOD                    ! FIO identifier
      LOGICAL DEP                     ! Is data position dependant

*  Status:
      INTEGER STATUS                  ! Global status

*  External functions:
      INTEGER FWRITER                 ! C function
      INTEGER CHR_LEN                 ! used length of a string

*  Local variables:
      CHARACTER *(3)  BORD            ! Byte order
      CHARACTER *(256) LINE           ! FIO line output length
      CHARACTER *(20) MACH            ! Machine name
      CHARACTER *(20) NODE            ! Node name
      CHARACTER *(20) REL             ! Opsys version
      CHARACTER *(20) SYS             ! System name
      CHARACTER *(20) VER             ! Opsys subversion name
      INTEGER ELEMS                   ! No. of items in DX array
      INTEGER I                       ! Loop variable
      INTEGER ISTAT                   ! C i/o status
      INTEGER NC                      ! Number of characters
      LOGICAL LIN                     ! Is axis linear?
      REAL DELTA                      ! Increment along axis
      REAL LVAL                       ! Axis value at first element
      REAL UVAL                       ! Axis value at last element
      REAL TEMP(3)                    ! Buffer for origin and delta values

      DATA TEMP/3*0.0/

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

      CALL NDF2DX_WRICH( BMODE, FIOD, '#', STATUS )

*  If the data are connections dependant, we need an extra position.
      IF( DEP ) THEN
         ELEMS = N
      ELSE
         ELEMS = N + 1
      END IF

*  See if the axis is linear.
      CALL KPG1_AXLIR( N, CENTRE, LVAL, UVAL, LIN, STATUS )

*  If it is linear, find the increment between adjacent pixels.
      IF( LIN ) THEN
         DELTA = ( UVAL - LVAL )/REAL( N - 1 )

*  If the data are connections dependant, each position should refer to
*  the boundary between two pixels, so adjust the first one to refer to
*  the lower bound of the first pixel.
         IF( .NOT. DEP ) LVAL = LVAL - 0.5*DELTA

*   Create the regular array object
         NC = 0
         CALL CHR_PUTC( 'object "', LINE, NC )
         CALL CHR_PUTC( name, LINE, NC )
         CALL CHR_PUTC( '" class regulararray items ', LINE, NC )
         CALL CHR_PUTI( ELEMS, LINE, NC )
         CALL CHR_PUTC( ' type float', LINE, NC )
         CALL NDF2DX_WRICH( BMODE, FIOD, LINE(1:NC), STATUS )

*   Now add the origin clause.
         TEMP( AXIS ) = LVAL
         NC = 0
         CALL CHR_PUTC( 'origin ', LINE, NC )
         DO I = 1, NDIM
            CALL CHR_PUTR( TEMP( I ), LINE, NC )
            CALL CHR_PUTC( '  ', LINE, NC )
         END DO
         CALL NDF2DX_WRICH( BMODE, FIOD, LINE(1:NC), STATUS )

*   Now add the delta clause.
         TEMP( AXIS ) = DELTA
         NC = 0
         CALL CHR_PUTC( 'delta ', LINE, NC )
         DO I = 1, NDIM
            CALL CHR_PUTR( TEMP( I ), LINE, NC )
            CALL CHR_PUTC( '  ', LINE, NC )
         END DO
         CALL NDF2DX_WRICH( BMODE, FIOD, LINE(1:NC), STATUS )

*  Now deal with cases where the axis is not linear. In these cases we
*  store the axis values explicitly as a normal array.
      ELSE


*  Find out what system type we are running on.
         CALL PSX_UNAME( SYS, NODE, REL, VER, MACH, STATUS )
         IF( STATUS .NE. SAI__OK ) GO TO 9999

         BORD = ' '
         IF( SYS(1:5) .EQ. 'SunOS' ) BORD = 'msb'
         IF( SYS(1:4) .EQ. 'OSF1' ) BORD = 'lsb'
         IF( SYS(1:5) .EQ. 'Linux' ) BORD = 'lsb'
         IF( BORD .EQ. ' ' ) THEN
            CALL MSG_SETC( 'OS', SYS )
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Unsupported operating system: ''^OS''.',
     :                   STATUS)
            GO TO 9999
         END IF

*  Create the header for the array object.
         NC = 0
         CALL CHR_PUTC( 'object "', LINE, NC )
         CALL CHR_PUTC( name, LINE, NC )
         CALL CHR_PUTC( '" class array type float rank 1 shape ', LINE,
     :                  NC )
         CALL CHR_PUTI( NDIM, LINE, NC )
         CALL CHR_PUTC( ' items ', LINE, NC )
         CALL CHR_PUTI( ELEMS, LINE, NC )

         IF( BMODE ) THEN
            CALL CHR_PUTC( ' '//BORD//' binary data follows', LINE, NC )
         ELSE
            CALL CHR_PUTC( ' text data follows', LINE, NC )
         END IF

         CALL NDF2DX_WRICH( BMODE, FIOD, LINE(1:NC), STATUS )

*  Write out the data values. Each axis position is encoded as a vector
*  in ndim space. First do binary data.
         IF( BMODE ) THEN

*  Write out the first point. If the data is connection dependant, alter
*  the axis value to make it refer to the lower edge of the cell.
            IF( DEP ) THEN
               TEMP( AXIS ) = CENTRE(1)
            ELSE
               TEMP( AXIS ) = 0.5*( 3.0*CENTRE(1)-CENTRE(2) )
            END IF

            ISTAT = FWRITER( TEMP, NDIM )
            IF( ISTAT .NE. 1 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'Unable to write binary data '//
     :                       'to output file', STATUS )
               GO TO 9999
            END IF

*  Now do the remaining points, correcting connections-dependant values
*  so that they refer to the edges of the cells instead of the centres.
            DO I = 2, N

               IF( DEP ) THEN
                  TEMP( AXIS ) = CENTRE(I)
               ELSE
                  TEMP( AXIS ) = 0.5*( CENTRE(I) + CENTRE( I-1 ) )
               END IF

               ISTAT = FWRITER( TEMP, NDIM )
               IF( ISTAT .NE. 1 ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'Unable to write binary data '//
     :                          'to output file', STATUS )
                  GO TO 9999
               END IF

            END DO

*  Connections-dependant data requires an extra point at the end.
            IF( .NOT. DEP ) THEN
               TEMP( AXIS ) = 0.5*( 3.0*CENTRE(N)-CENTRE(N-1) )
               ISTAT = FWRITER( TEMP, NDIM )

               IF( ISTAT .NE. 1 ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'Unable to write binary data '//
     :                          'to output file', STATUS )
                  GO TO 9999
               END IF

            END IF

*  Now do text data.
         ELSE

*  Write out the first point. If the data is connection dependant, alter
*  the axis value to make it refer to the lower edge of the cell.
            NC = 0

            IF( DEP ) THEN
               TEMP( AXIS ) = CENTRE(1)
            ELSE
               TEMP( AXIS ) = 0.5*( 3.0*CENTRE(1)-CENTRE(2) )
            END IF

            LINE = ' '
            WRITE( LINE, * ) TEMP
            CALL FIO_WRITE( FIOD, LINE( : CHR_LEN( LINE ) ), STATUS )

*  Now do the remaining points, correcting connections-dependant values
*  so that they refer to the edges of the cells instead of the centres.
            DO I = 2, N
               NC = 0

               IF( DEP ) THEN
                  TEMP( AXIS ) = CENTRE( I )
               ELSE
                  TEMP( AXIS ) = 0.5*( CENTRE( I ) + CENTRE( I - 1 ) )
               END IF

               LINE = ' '
               WRITE( LINE, * ) TEMP
               CALL FIO_WRITE( FIOD, LINE( : CHR_LEN( LINE ) ), STATUS )

               IF( STATUS .NE. SAI__OK ) GO TO 9999

            END DO

*  Connections-dependant data requires an extra point at the end.
            IF( .NOT. DEP ) THEN
               NC = 0
               TEMP( AXIS )  = 0.5*( 3.0*CENTRE( N ) - CENTRE( N-1 ) )

               LINE = ' '
               WRITE( LINE, * ) TEMP
               CALL FIO_WRITE( FIOD, LINE( : CHR_LEN( LINE ) ), STATUS )

            END IF

         END IF

      END IF

      TEMP( AXIS ) = 0.0

 9999 CONTINUE

      END


      SUBROUTINE KPG1_AXLIR( EL, ARRAY, LVAL, UVAL, LINEAR, STATUS )
*+
*  Name:
*     KPG1_AXLIx

*  Purpose:
*     Determines whether an array's values are equally spaced.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_AXLIx( EL, ARRAY, LVAL, UVAL, LINEAR, STATUS )

*  Description:
*     This routine determines whether or not adjacent elements of a
*     1-d array have values that are equally spaced, i.e. it tests for
*     linearity.  It simply checks if the intervals between all
*     successive pairs of elements are the same within the machine
*     precision.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of elements in the array.  It must be at least
*        two.
*     ARRAY( EL ) = ? (Given)
*        The array to be tested.
*     LVAL = ? (Returned)
*        Value of the first array element.  If this is bad an estimated
*        value is substituted when the array is linear.
*     UVAL = ? (Returned)
*        Value of the last array element.  If this is bad an estimated
*        value is substituted when the array is linear.
*     LINEAR = LOGICAL (Returned)
*        True if the array is linear.
*     STATUS = INTEGER (Given)
*        The global status.

*  Notes:
*     -  There is a routine for most numeric data types: replace "x" in
*     the routine name by D, R, I, W, or UW as appropriate.  The array
*     (and the variables for the first and last array elements) supplied
*     to the routine must have the data type specified.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 April 4 (MJC):
*        Original version based on JM's CON_LNEAR.
*     1992 March 13 (MJC):
*        Improved the test for linearity by using a longer baseline.
*        Allowed for bad values.  Performed all tests in double
*        precision.  Set the maximum difference between the actual
*        and predicted values to be 0.5 for integer data.
*     1993 may 27 (MJC):
*        Used improved algorithm for the linearity test scaling the
*        maximum deviation by maximum absolute value in the array.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT    NONE         ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE constants
      INCLUDE 'PRM_PAR'     ! PRIMDAT constants

*  Arguments Given:
      INTEGER    EL            ! Array size
      REAL     ARRAY( EL )   ! Array to be tested.

*  Arguments Returned:
      LOGICAL    LINEAR        ! True if array is linear
      REAL     LVAL          ! Value of first array element.
      REAL     UVAL          ! Value of last array element.

*  Status:
      INTEGER    STATUS        ! Global status

*  Local Variables:
      DOUBLE PRECISION CURENT  ! Current array value
      DOUBLE PRECISION DIFLIM  ! Tolerance used for comparing intervals
      DOUBLE PRECISION FIRST   ! First non-bad array value
      INTEGER    HIGH          ! Index of last non-bad array value
      INTEGER    I             ! Loop variable
      DOUBLE PRECISION INCREM  ! Value of step size
      INTEGER    LOW           ! Index of first non-bad array value
      DOUBLE PRECISION MAXVAL  ! Maximum absolute value in the array

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'    ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'    ! NUM definitions for conversions

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Assume that the array is not linear until proven otherwise.

      LINEAR = .FALSE.

*    Set start, and end values using first and last elements assuming
*    these are not bad.

      LVAL = ARRAY( 1 )
      UVAL = ARRAY( EL )

*    Find the increment assuming for the moment that it is a linear
*    array by using the longest baseline.  Exclude any bad data at
*    the ends of the array.

      LOW = 1
      DO WHILE ( ARRAY( LOW ) .EQ. VAL__BADR .AND. LOW .LT. EL )
         LOW = LOW + 1
      END DO
      HIGH = EL
      DO WHILE ( ARRAY( HIGH ) .EQ. VAL__BADR .AND. HIGH .GT. LOW )
         HIGH = HIGH - 1
      END DO

*    Report an error when there is one or no good values.

      IF ( LOW .GE. HIGH ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_AXLIx_BADAT',
     :     'The test for linear spacing within an array has failed '/
     :     /'because the array has one or no good values.', STATUS )
         GOTO 999
      END IF

*    Evaluate the linear increment.

      INCREM = ( NUM_RTOD( ARRAY( HIGH ) )
     :         - NUM_RTOD( ARRAY( LOW ) ) ) / NUM_ITOR( HIGH - LOW )

*    Find the biggest absolute value in the array.

      MAXVAL = -1.0D0
      DO I = LOW, HIGH
         IF ( ARRAY( I ) .NE. VAL__BADR ) THEN
            MAXVAL = MAX( MAXVAL, ABS( NUM_RTOD( ARRAY( I ) ) ) )
         END IF
      END DO

*    Find the smallest allowed difference in the intervals.  Integer
*    intervals need only be different by half of one---the machine
*    precision by definition---for non-linearity.  Floating-point uses
*    an arbitrary factor times the floating-point machine precision in
*    units of the increment to test for linearity.  The factor should
*    allow for rounding errors and a full decade of values.

      IF ( VAL__EPSR .EQ. 1.0E0 ) THEN
         DIFLIM = 0.5D0
      ELSE
         DIFLIM = 11.0E0 * VAL__EPSR * MAXVAL
      END IF

*    Check that each pair of successive elements are the same interval
*    apart in value as the previous pair.  If not, the array is
*    non-linear.  There must be at least two valid values to test for
*    linearity.

      FIRST = NUM_RTOD( ARRAY( LOW ) )
      IF ( ( HIGH - LOW ) .GT. 1 ) THEN
         DO I = LOW + 1, HIGH

*          Ignore bad values from the test.

            IF ( ARRAY( I ) .NE. VAL__BADR ) THEN

*             The tolerance for deciding that the intervals are the
*             same is achieved by comparing the difference between the
*             predicted value---given linear data---and the actual
*             value, against a few times the machine precision at the
*             maximum absolute value.

               CURENT = NUM_RTOD( ARRAY( I ) )
               IF ( ABS( CURENT - FIRST - INCREM * DBLE( I - LOW ) )
     :              .GT. DIFLIM ) GO TO 999

            END IF
         END DO
      END IF

*    The loop was completed therefore the interval in array values is
*    constant.

      LINEAR = .TRUE.

*    Set the limiting values if either was bad by extrapolating the
*    linear array.

      IF ( ARRAY( 1 ) .EQ. VAL__BADR ) THEN
         LVAL = NUM_DTOR( NUM_RTOD( ARRAY( LOW ) ) -
     :          INCREM * DBLE( LOW - 1 ) )
      END IF

      IF ( ARRAY( EL ) .EQ. VAL__BADR ) THEN
         UVAL = NUM_DTOR( NUM_RTOD( ARRAY( HIGH ) ) +
     :          INCREM * DBLE( EL - HIGH ) )
      END IF

  999 CONTINUE

      END
