*+ PSF_AXAF_MARX - Wrapper program for PSF_AXAF_MARX_ARRAY
      SUBROUTINE PSF_AXAF_MARX( PSID, X01, Y01, QX1, QY1, DX1, DY1,
     :                               INTEG, NX, NY, ARRAY, STATUS )
*    Description :
*     Wrapper program for PSF_AXAF_MARX_ARRAY.
*     Adds the size of the PSF array required by the PSF cube to the standard PSF system call
*     This allows the PSF system to retain it form.
*
*    Deficiencies :
*
*    Authors :
*     David Geddes
*
*    History :
*     31 Aug 99 : Original (DGED)
*
*    Type Definitions :
      IMPLICIT NONE
*
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_HCUBE_CMN'
      INCLUDE 'MATH_PAR'
*
*    Import :
      INTEGER PSID
      INTEGER NX,NY
*
      LOGICAL INTEG
*
      REAL X01, Y01, QX1, QY1
      REAL DX1, DY1
*
*    Export :
      REAL ARRAY(NX,NY)                             ! Array to be returned
*
*    Status :
      INTEGER STATUS                                ! Error status
*
*    Local variables :
      INTEGER PSF_ARRAY
*
*    Map PSF array
      CALL DYN_MAPR(1, HX_PSF_ARRAY_X * HX_PSF_ARRAY_Y, PSF_ARRAY,
     :                                                      STATUS)

      CALL PSF_AXAF_MARX_ARRAY( PSID, X01, Y01, QX1, QY1, DX1, DY1,
     :  INTEG, NX, NY, ARRAY, HX_PSF_ARRAY_X, HX_PSF_ARRAY_Y,
     :  %val(PSF_ARRAY), STATUS )

      CALL DYN_UNMAP( PSF_ARRAY, STATUS )

*    Exit
 99   IF (STATUS .NE. SAI__OK) THEN
         CALL AST_REXIT('PSF_AXAF_MARX',STATUS)
      ENDIF

      END
*
*
*+ PSF_AXAF_MARX_ARRAY.F - Create an AXAF MARX PSF array
      SUBROUTINE PSF_AXAF_MARX_ARRAY( PSID, X01, Y01, QX1, QY1, DX1,
     :  DY1, INTEG, NX, NY, ARRAY, PX, PY, PSF_ARRAY, STATUS )
*
*    Description :
*     Features
*     - Creates an AXAF PSF array
*     - As a  part of the PSF system conforms to the PSF system interface.
*     - Can deal with RADIANS or DEGREES
*
*     Method
*     - Work out if values are PIXEL or RADIAN
*     - Calculate roll angle
*     - Initialise arrays
*     - Convert the X Y radian or pixel value into a chip position
*     - Relate chip position to hypercube & locate XY position on the cube model
*     - Search locator cube for PSF array number and PSF size
*     - Convert array number to array name
*     - Check if PSF required is repeated
*     - Check for delta function
*     - Read in the PSF array
*     - If pixel size is CCD size - copy the PSF directly
*     - Else subpixallate array
*
*     The ASTERIX PSF SYSTEM represents PSFs in arrays representing PROBABILITY PER
*     PIXEL. ROSAT choose to store information about probability per area and transform
*     this into pixel probabilties when the pixel size was known. Because AXAF uses a CCD
*     it is anticpated that the pixel size required will commonly be multiples of the
*     CCDPIX and consequently the AXAF cube stores information in the required, probability
*     per pixel, form.
*
*     This routine has been constrained to have this form because of a requirement
*     to conform to the PSF system. By conforming with the current PSF it was hoped
*     that the PSF might more easy be integrated into existing ASTERIX programs. In
*     practice some modifications have had to be made.
*
*     This routine deals with PSF information in the form in which it is passed in.
*     If the values are given as a standard CDD pixel size then the routine is able
*     to read the PSF directly from the PSF cube. If radians are passed then the
*     routine convert the PSF array values into probabilty per arcsecond sq and
*     repixellates at the required pixel size. The first approach is very fast and
*     the second allows flexibilty.
*
*     The PSF Hyper Cube model - for AXAF : ACIS I
*     The unorthodox chip layout and gap between the chips mean that conversion bet
*     ween chip position and an offset from center in radians or degrees is not
*     straight forwards. The model used here treats the four chips as a single array
*     of 2070 * 2070. The purpose of the model is to enable an offset (eg. 0,0( to be
*     converted into a chip position (eg 963,1109) [the nominal aimpoint]. The chip array
*     is therefore taken to be 2070*2070 ~ including a 22 ccdpix gap.
*
*           (0,2070) Y
*           ^
*           |   [0|1]       Chip orientation used on PSF model for ACIS-I
*           |   [2|3]
*          (0,0)--------> (2070,0) X
*
*     Note - the decision to treat ACIS I and ACIS S separately was driven by the
*     observation that this was how they were deal with in the MARX simulation package.
*
*     See - The AXAF Proposers Guide. Oct 97. p41.
*
*     AXAF provides
*
*    Deficiencies :
*     Doesn't check that its an ACIS-I image.
*     Roll angle is input by the user
*
*    Environment Parameters
*
*    Authors :
*     David Geddes
*
*    History :
*     31 Aug 99 : Original (DGED)
*
*    Type Definitions :
      IMPLICIT NONE
*
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_HCUBE_CMN'
      INCLUDE 'MATH_PAR'
      INCLUDE 'PSS_PAR'
      INCLUDE 'PSS_CMN'
*
*    Import :
      INTEGER PSID                                      ! PSF handle
      INTEGER NX,NY                                     ! Array size
      INTEGER PX,PY
*
      LOGICAL INTEG
*
      REAL X01, Y01                                     ! X Y position
      REAL QX1, QY1
      REAL DX1, DY1                                     ! Pixel size
      REAL PSF_ARRAY(PX,PY)
*
*    Export :
      REAL                      ARRAY(NX,NY)            ! Array to be returned
*
*     Status :                                          ! Error status
      INTEGER                   STATUS
*
*    Functions
      REAL                      AXAF_SUBPIXELING        ! Breaks down pixels
*
*    Local variables :
      CHARACTER*(DAT__SZLOC)    HCLOC                   ! Hyper cube locator
      CHARACTER*(80)            PSF_ARRAY_NAME          !
      CHARACTER*(80)            TEMP                    !

      INTEGER                   ARPTR                   ! Array pointer
      INTEGER                   CHIPX,CHIPY             ! Chip position
      INTEGER                   ARR_X,ARR_Y
      INTEGER                   I, J                    ! Major pixel loops
      INTEGER                   II, JJ                  ! Sub-pixel loops
      INTEGER                   MX, MN                  ! Max,min
      INTEGER                   MNX, MNY                ! Local calc bounds
      INTEGER                   N1,N2                   ! Any integer
      INTEGER                   NVAL                    ! Number of Values
      INTEGER                   PSF_X,PSF_Y
      INTEGER                   SPA                     ! Size of PSF Array
      INTEGER                   XPOS, YPOS              ! Model position locators
      INTEGER                   XSUB, YSUB              ! Sub-pixel factors

      REAL                      ANGLE                   ! Any angle
      REAL                      DX, DY                  ! Pixel size
      REAL                      NORM               	! Normalisation constant
      REAL                      QX, QY
      REAL                      ROLL_ANGLE              ! Roll angle
      REAL                      RATIO                   ! Ratio of lengths
      REAL                      SDX, SDY ! Sub-pixel bin sizes
      REAL                      SUM                     ! Cumulative value
      REAL                      XNOR, YNOR              ! X,Y bits to NORM
      REAL                      XP0, YP0                ! Major pixel centre
      REAL                      XPA0,YPA0               ! Corner of pixel array
      REAL                      XPS, YPS                ! Sub-pixel pixel centre
      REAL                      X0, Y0                  ! X and Y values in seconds
      REAL                      XH                      ! Hypothenuese in seconds
      REAL                      XRVAL,YRVAL             ! X and Y roll values


      REAL TOTAL
*-
*    Pixel sizes passed in as radians are required in seconds
      X0 = X01 * MATH__RTOD * 3600
      Y0 = Y01 * MATH__RTOD * 3600
      DX = ABS(DX1 * MATH__RTOD * 3600)
      DY = ABS(DY1 * MATH__RTOD * 3600)
      QX = QX1 * MATH__RTOD * 3600
      QY = QY1 * MATH__RTOD * 3600

*    Get roll angle from the user
      CALL USI_GET0R('ROLL',ROLL_ANGLE,STATUS)

      IF (ROLL_ANGLE .EQ. 360.0) ROLL_ANGLE = 0.0

      IF (ROLL_ANGLE .LT. 0.0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Error - roll angle less than zero')
         GOTO 999
      ENDIF

      IF (ROLL_ANGLE .GT. 0.0) THEN

*       Radains
         XH = SQRT( X01*X01 + Y01*Y01)

         IF (X01 .EQ. 0.0) THEN
            ANGLE = 0.0
         ELSE
            RATIO = X01/XH
            ANGLE = ASIN(RATIO)
         ENDIF

*       Add user input and offset in data to find total angle
*       Record this total angle as radian value
         ANGLE = (ROLL_ANGLE * MATH__DTOR) + ANGLE
*       Record as degree
         ROLL_ANGLE = ANGLE * MATH__RTOD

         IF (DB) PRINT *,'DEBUG : ROLL ANGLE DEG = ',ROLL_ANGLE

         IF (ROLL_ANGLE .GT. 360.0) THEN
            ROLL_ANGLE = ROLL_ANGLE - 360.0
            ANGLE = ROLL_ANGLE * MATH__DTOR
         ENDIF

*       Roll angle - included in header
         IF (ROLL_ANGLE .GE. 0.0 .AND. ROLL_ANGLE .LT. 90.0) THEN

            YRVAL = COS(ANGLE)*XH
            XRVAL = SIN(ANGLE)*XH
*         Seconds
            X0 = XRVAL * 3600 * MATH__RTOD
            Y0 = YRVAL * 3600 * MATH__RTOD

*       90 to 180
         ELSE IF (ROLL_ANGLE .GE. 90.0 .AND. ROLL_ANGLE .LT. 180.0) THEN

            ANGLE = ANGLE - ( 90 * MATH__DTOR )

            XRVAL = COS(ANGLE)*XH
            YRVAL = - SIN(ANGLE)*XH

            X0 = XRVAL * 3600 * MATH__RTOD
            Y0 = YRVAL * 3600 * MATH__RTOD

*       180 to 270
         ELSEIF (ROLL_ANGLE .GE. 180.0 .AND. ROLL_ANGLE .LT. 270.0) THEN

            ANGLE = ANGLE - (180 * MATH__DTOR)

            YRVAL = - COS(ANGLE)*XH
            XRVAL = - SIN(ANGLE)*XH

            X0 = XRVAL * 3600 * MATH__RTOD
            Y0 = YRVAL * 3600 * MATH__RTOD

*       270 to 360
         ELSEIF (ROLL_ANGLE .GE. 270.0 .AND. ROLL_ANGLE .LT. 360.0) THEN

            ANGLE = ANGLE - (270 * MATH__DTOR)

            XRVAL = - COS(ANGLE)*XH
            YRVAL = SIN(ANGLE)*XH

            X0 = XRVAL * 3600 * MATH__RTOD
            Y0 = YRVAL * 3600 * MATH__RTOD

         ENDIF
      ENDIF

*    Initialise arrays
      CALL ARR_INIT1R(0.0,HX_PSF_ARRAY_X*HX_PSF_ARRAY_Y,PSF_ARRAY,
     :                                                       STATUS)
       CALL ARR_INIT1R(0.0,NY*NX,ARRAY,STATUS)

      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','Error initialising arrays')
         GOTO 999
      ENDIF

*    Convert the X Y position into a chip position

*    Decide whether the values passed as pixels
      If (AX_UNITS(1)(1:6) .EQ. 'pixels') THEN
         IF (DX1 .EQ. 1.0 .AND. DY1 .EQ. 1.0) THEN
*          Change to pixel size
            DX =  0.00000242
            DY =  0.00000242
         ENDIF
      ENDIF

*    Work out if values are PIXEL or RADIAN

*    Radians
      If (DX1 .LE. 0.05) THEN ! Value passed in < a tenth of a CCD pix

*       Convert radian value into chip position

*       In seconds - sign retained
         IF ( X0 .EQ. 0 ) THEN
            CHIPX=0
         ELSE
            CHIPX = INT(X0/AXAF_CPIX_SIZE)
         ENDIF
         IF (Y0 .EQ. 0 ) THEN
            CHIPY=0
         ELSE
            CHIPY = INT(Y0/AXAF_CPIX_SIZE)
         ENDIF
*       See method (above) for a description of chip order and model
*       Transform these into values on a single array.

*       Off axis value are inverted hence subtract
         CHIPX = NOMINAL_X - CHIPX
         CHIPY = NOMINAL_Y - CHIPY

*       Check bounds
         IF (CHIPX .GT. 2070 .OR. CHIPY .GT. 2070 .OR.
     :       CHIPX .LT. 1 .OR. CHIPY .LT. 1) THEN
              If (DB) PRINT *,'DB : ERROR - chip position out of bounds'
         ENDIF
         IF (CHIPX .GT. 2070) CHIPX = 2070
         IF (CHIPY .GT. 2070) CHIPY = 2070
         IF (CHIPX .LT. 1) CHIPX = 1
         IF (CHIPY .LT. 1) CHIPY = 1
      ELSE
*    Pixels
*       Value given equal chip position upon a 2070 square pixel CHANDRA image
         CHIPX = X01
         CHIPY = Y01
         DX =  0.00000242
         DY =  0.00000242
      ENDIF

*    Relate chip position to hypercube

*    Find scale
      HX_SCALE_X = 2048/HX_CB_NRBINX

      HX_CB_GAP_X    = HX_CB_NRBINX/2
      HX_CB_GAP_Y    = HX_CB_NRBINY/2

*    Locate XY position on the cube model
      XPOS = HX_SCALE_X
      HX_LOCATOR_POSX = 1
      DO WHILE (CHIPX .GT. XPOS)
         XPOS = XPOS + HX_SCALE_X
         HX_LOCATOR_POSX = HX_LOCATOR_POSX + 1
         IF ( HX_LOCATOR_POSX .EQ. HX_CB_GAP_X) XPOS = XPOS + 22
      ENDDO

      YPOS = HX_SCALE_X
      HX_LOCATOR_POSY = 1
      DO WHILE (CHIPY .GT. YPOS)
         YPOS = YPOS + HX_SCALE_X
         HX_LOCATOR_POSY = HX_LOCATOR_POSY + 1
         IF ( HX_LOCATOR_POSY .EQ. HX_CB_GAP_Y) YPOS = YPOS + 22
      ENDDO

*    NOTE - gap values currently default here.

*    Mean energy values HX_LOCATOR_POSE is already supplied by PSF_AXAF_INT

*    Now have energy and X & Y position

*    Search locator cube for PSF array number
      NVAL = HX_CB_NRBINX*HX_CB_NRBINY*HX_CB_NEBIN

*    Calculate position in the locator cube. [4 = size of integer.]
      N1 = 0
      N2= ((HX_LOCATOR_POSY-1)*HX_CB_NRBINX + HX_LOCATOR_POSX) * 4 - 4
     : + (((HX_LOCATOR_POSE-1)*(HX_CB_NRBINX*HX_CB_NRBINX))*4)

*    Get the number of the required PSF array
      CALL ARR_COP1I(1,%val(HX_CB_LPTR+N2),N1,STATUS)
      IF (N1 .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','ERROR - Value in locator cube')
         GOTO 999
      ENDIF

*    Get the size of the PSf array
      CALL ARR_COP1I(1,%val(HX_PSF_PTR+(N1*4)-4),HX_PSF_SIZE,STATUS)
      SPA = 2*HX_PSF_SIZE+1

      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','ERROR - Searching locator cube')
         GOTO 999
      ENDIF

*  Create array name

*    Initialise array. Messy CALL ARR_INIT1C ?
      DO I=1,80
         TEMP(I:I) = ' '
      ENDDO

*    Convert array number to array name
      WRITE(TEMP,*) N1
      MX = 0
      MN = 80
      DO N1 = 1,80
         IF ( TEMP(N1:N1) .NE. ' ') THEN
           MX = N1
           IF ( N1 .LT. MN) MN = N1
         ENDIF
      ENDDO
      PSF_ARRAY_NAME = 'PSF_ARRAY_'//TEMP(MN:MX)
      IF (DB) PRINT *,'PSF = '//PSF_ARRAY_NAME

*    Is value repeated?
      IF (TEMP_STRING .NE. PSF_ARRAY_NAME .OR. HX_KEEP_NX .NE. NX) THEN
         TEMP_STRING = PSF_ARRAY_NAME
      ELSE
         DO I = 1,NX
            DO J = 1,NY
               ARRAY(I,J) = HX_KEEP_ARRAY(I,J)
            END DO
         END DO
*       Return
         GOTO 999
      ENDIF

*    If the PSF falls within one pix then a delta function is used.
      N1 = ABS(DX1/0.00000242)
      IF ( N1 .GT. SPA) THEN
         IF (DB) PRINT *,'Using a delta function'
         N1 = (NX/2) + 1
         ARRAY(N1,N1)=1
         GOTO 998
      ENDIF

*  New PSF

*    Read in the PSF array
      CALL ADI1_GETLOC(HX_CB_ID,HCLOC,STATUS)

      CALL CMP_MAPV(HCLOC,PSF_ARRAY_NAME,'_REAL','READ',ARPTR,
     :                                             SPA*SPA,STATUS)

*    Copy to full size array
      CALL ARRAY_OFFSET(PSF_ARRAY,HX_PSF_ARRAY_X,%val(ARPTR),SPA,STATUS)

*    Required well calls are repeated.
      CALL CMP_UNMAP(HCLOC,PSF_ARRAY_NAME,STATUS)

*    Check status
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','ERROR - Reading in PSF array ')
         GOTO 999
      ENDIF

*    If required size = CCD pixel size, then the PSF can be copied directly

*    If pixel size is CCD size - copy the PSF directly.
      IF (DX .EQ. 0.00000242) THEN

         IF (NX .GT. HX_PSF_ARRAY_X .OR. NY .GT. HX_PSF_ARRAY_Y) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP(' ','ERROR - Requested Array Larger '
     : //                 'than PSF')
            GOTO 999
         END IF

*       Work out the offsets within the array
         ARR_X = NX/2 + 1
         ARR_Y = NY/2 + 1
         PSF_X = HX_PSF_ARRAY_X/2 + 1
         PSF_Y = HX_PSF_ARRAY_Y/2 + 1
         PSF_X = PSF_X - ARR_X
         PSF_Y = PSF_Y - ARR_Y

*       Copying across PSF array
         DO I = 1, NX
            DO J = 1, NY
               ARRAY(I,J) = PSF_ARRAY(PSF_X+I,PSF_Y+J)
            END DO
         END DO

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP(' ','ERROR - copying across PSF array')
            GOTO 999
         ENDIF

*     Finished.
         GOTO 998
      END IF

*    Debug information
      IF (DB) THEN
         TOTAL=0
         DO I = 1, HX_PSF_ARRAY_X
            DO J = 1, HX_PSF_ARRAY_X
               TOTAL = TOTAL + PSF_ARRAY(I,J)
            END DO
         END DO
         PRINT *,' DEBUG : PRE ARRAY TOTAL', TOTAL
      ENDIF

*    The following code uses the array in the hypercube to create a PSF with
*    a pixel size which is other than the CDD pix size. This code essentially
*    is a simplification of the subpixelling mechanism which was widely used
*    in ROSAT PSPC. See file ~ PSFRTN.F routine PSF_XRT_PSF.

*    The offest value is not used as the relevant array has already been found
      X0 = 0.0
      Y0 = 0.0

*    Pixel array base coordinates
      XPA0 = ( - REAL(NX)/2.0 ) * DX
      YPA0 = ( - REAL(NY)/2.0 ) * DY

*    Base coordinates
      XP0 = ( - REAL(NX)/2.0 ) * DX + X0 + QX
      YP0 = ( - REAL(NY)/2.0 ) * DY + Y0 + QY

*    The whole array. PSFs are not symmetrical.
      MNX = NX
      MNY = NY

*    For each point requiring calculation
      DO J = 1, MNY

*      Find Y sub-pixelling
        YSUB =  MAX(12 - ABS(ABS(INT(NY/2)) + 1 - J), 3 )
        SDY = DY / YSUB

*      Y contribution to normalisation
        YNOR = SDY

        DO I = 1, MNX

*        Zero
          SUM = 0.0

*        Find X sub-pixelling
          XSUB = MAX(12 - ABS(ABS(INT(NX/2)) + 1 - I), 3 )
          SDX = DX / XSUB

*        X contribution to normalisation - hence total normalisation
          XNOR = SDX

*        Absolute value required.
          NORM = ABS(XNOR*YNOR)

*        Y position of first sub-pixel centre
          YPS = YP0 + DY*(J-1) + 0.5*SDY

*        For each sub-pixel row
          DO JJ = 0, YSUB-1

*          X position of first sub-pixel centre assign dx1=2.4e
            XPS = XP0 + DX*(I-1) + 0.5*SDX


*          For each sub-pixel
            DO II = 0, XSUB-1

*            Pass in PSF array from the cube.
              SUM = SUM + AXAF_SUBPIXELING(PSF_ARRAY,PX,PY,XPS,YPS)

*            Next sub-pixel
              XPS = XPS + SDX

            END DO

*          Next row of sub-pixels
            YPS = YPS + SDY
          END DO

*        Set returning ARRAY value
          ARRAY(I,J) = ARRAY(I,J) + (SUM * NORM)
        END DO

      END DO

 998  CONTINUE

*    Ensure that the total is 1
      TOTAL=0

      DO I = 1,NX
         DO J = 1,NY
            TOTAL = TOTAL + ARRAY(I,J)
         END DO
      END DO

      IF (DB) PRINT *,' DEBUG : ARRAY TOTAL = ', TOTAL

*    Normalise
      DO I = 1,NX
         DO J = 1,NY
            ARRAY(I,J) = ARRAY(I,J)*(1.0/TOTAL)
         END DO
      END DO

*     Initialise array to zero
      CALL ARR_INIT1R(0.0,300*300,HX_KEEP_ARRAY,STATUS)
      DO I = 1,NX
         DO J = 1,NY
            HX_KEEP_ARRAY(I,J) = ARRAY(I,J)
         END DO
      END DO
      HX_KEEP_NX = NX

*    Error
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','ERROR - subpixelling PSF array')
         GOTO 999
      ENDIF

*    Exit
 999  CONTINUE
      IF (STATUS .NE. SAI__OK) THEN
         CALL AST_REXIT('PSF_AXAF_MARX',STATUS)
      ENDIF
      END


*+ AXAF_SUBPIXELING
      REAL FUNCTION AXAF_SUBPIXELING(PSF_ARRAY,NX,NY,XOFF,YOFF)
*
*    Description :
*    - Returns a values of probability per square seconds
*
*    - The PSF is stored in the cube in an array of with a ccd
*    pixel size. This value, AXAF_CPIX_SIZE, is used to convert in prob
*    per square seconds at the required offset.
*
*    Deficiencies :
*
*    Authors :
*     David Geddes
*
*    History :
*     31 Aug 99 : Original (DGED)
*
*    Type Definitions :
      IMPLICIT NONE
*
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_HCUBE_CMN'
*
*    Import :
      INTEGER                         NX,NY             ! PSF array size
*
      REAL                            XOFF,YOFF         ! Offset from center of PSF
      REAL                            PSF_ARRAY(NX,NY)  ! PSF read from cube
*
*    Local variables :
      INTEGER                         XAPOS             ! X array position
      INTEGER                         YAPOS             ! Y array position
*
      REAL                            R                 ! Return value
      REAL                            XPOS0,YPOS0       ! Origin position

      INTEGER                  FTP                      ! File id
        PARAMETER (FTP = 20)


*    Locate position in the array
      XPOS0 = REAL (NX/2.0) * AXAF_CPIX_SIZE
      YPOS0 = REAL (NY/2.0) * AXAF_CPIX_SIZE

      XAPOS = INT((XPOS0 + XOFF)/0.495)+1
      YAPOS = INT((YPOS0 + YOFF)/0.495)+1

*    Check request is not outside the array bounds
      IF (INT(XAPOS) .GT. NX .OR. INT(YAPOS) .GT. NY .OR.
     :    INT(XAPOS) .LT. 1  .OR. INT(YAPOS) .LT. 1) THEN
         AXAF_SUBPIXELING = 0
         RETURN
      ENDIF

      IF (PSF_ARRAY(XAPOS,YAPOS) .GT. 0.0) THEN
         R = PSF_ARRAY(XAPOS,YAPOS) / 0.245025  ! pixsize squared
      ELSE
         R = 0.0
      ENDIF

      AXAF_SUBPIXELING = R

      END


*+ ARRAY_OFFSET - Copies a small array into the middle of a large array
      SUBROUTINE ARRAY_OFFSET(L_ARRAY,LX,S_ARRAY,SX,STATUS)
*
*    Description :
*     Copies a small array into the middle of a large array
*
*    Authors :
*     David Geddes
*
*    History :
*     31 Aug 99 : Original (DGED)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global variables :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSF_HCUBE_CMN'
*
*    Import/Export :
*
      INTEGER LX,SX
      INTEGER NX,NY
      REAL    L_ARRAY(LX,LX)
      REAL    S_ARRAY(SX,SX)
*
*    Status
      INTEGER STATUS
*
*    Function definitions :
*
*    Local constants :
*
*    Local variables :
      INTEGER L_MID
      INTEGER S_MID

*-
      IF (LX .LT. SX) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Error - Array sizes incorrect')
         GOTO 999
      ENDIF

      L_MID = INT(LX/2)
      S_MID = INT(SX/2)

      L_MID = L_MID - S_MID

      DO NX = 1,SX
         DO NY = 1,SX
             L_ARRAY(L_MID+NX-1, L_MID+NY-1) = S_ARRAY(NX,NY)
         END DO
      END DO

*    Exit
 999  CONTINUE
      IF (STATUS .NE. SAI__OK) THEN
         CALL AST_REXIT('ARRAY_OFFSET',STATUS)
      ENDIF
      END


*+ CPA_PROFILE : Provides a % per pixel profile across a PSF
      SUBROUTINE CPA_PROFILE(PSF_ARRAY,PSF_PROFILE,PSF_RAD,ODD,STATUS)
*
*    Description :
*      CPA_PROFILE : Provides a % per pixel profile across a PSF.
*
*    Deficiencies :
*     This currently requires that the PSF be square. However could be modified by
*     passing in the pixel size and calculating the area and calculating the area of circle.
*
*    Environment Parameters
*
*    Authors :
*     David Geddes
*
*    History :
*     Original version.
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Global variables :
*
*    Import :
      INTEGER PSF_RAD
      INTEGER ODD
*    Export:
      REAL    PSF_ARRAY(2*PSF_RAD+ODD,2*PSF_RAD+ODD) !X & Y ?
      REAL    PSF_PROFILE(PSF_RAD+ODD)
*
*    Status :
*
      INTEGER STATUS
*
*    Function definitions :
*
*    Local constants :
*
*    Local variables :
      INTEGER N1
      INTEGER PRFL_CNT
      INTEGER STEP
      INTEGER X0,Y0
      INTEGER XPOS,YPOS

      REAL TOTAL
*-

*  Center
      X0 = PSF_RAD+1
      Y0 = PSF_RAD+1

      XPOS = X0
      YPOS = Y0

      STEP = 0

      PRFL_CNT = 0
      TOTAL = 0
      TOTAL = TOTAL + PSF_ARRAY(XPOS,YPOS)

      PRFL_CNT = PRFL_CNT + 1
      PSF_PROFILE(PRFL_CNT) =  TOTAL

*    Determine if PSF is  odd or even
      DO WHILE ((XPOS .NE. 1 .AND. YPOS .NE. 1) .AND.
     :          (XPOS .NE. 2 .AND. ODD  .NE. 0))

*       Move left one
         XPOS = XPOS - 1
         TOTAL = TOTAL + PSF_ARRAY(XPOS,YPOS)

*       Increase step
         STEP = STEP + 1

*       Move up
         DO N1 = 1, STEP
            YPOS = YPOS + 1
            TOTAL = TOTAL + PSF_ARRAY(XPOS,YPOS)
         END DO

*       Increase step
         STEP = STEP + 1

*       Move right
         DO N1 = 1, STEP
            XPOS = XPOS + 1
            TOTAL = TOTAL + PSF_ARRAY(XPOS,YPOS)
         END DO

*       Move down
         DO N1 = 1, STEP
            YPOS = YPOS - 1
            TOTAL = TOTAL + PSF_ARRAY(XPOS,YPOS)
         END DO

*       Move left
         DO N1 = 1, STEP
            XPOS = XPOS - 1
            TOTAL = TOTAL + PSF_ARRAY(XPOS,YPOS)
         END DO

         PRFL_CNT = PRFL_CNT + 1
         PSF_PROFILE(PRFL_CNT) =  TOTAL
      END DO

*    Exit

 9000 CONTINUE
      IF (STATUS .NE. SAI__OK) THEN
         CALL AST_REXIT('CPA_PROFILE',STATUS)
      ENDIF

      END
