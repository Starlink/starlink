*+ USER - User Commands for DIPSO
      LOGICAL FUNCTION USER(CMD, PARAMS, MAXPT, MAXBRK, X, Y, NPT,
     :  BRKS, NBRK, TITLE, OK)
*    Description :
*     Allow user-defined commands in DIPSO.   The routine is passed
*     the name of a command and the associated parameter string.
*     If the command can be handled then the parameter string is
*     decoded and any modifications to the current spectrum made.
*     The current spectrum and associated break points are assumed
*     to be handled PROPERLY, and the argument list should be
*     reproduced exactly as given!    Any units that are OPENed
*     should be in the range 20-29.
*    Invocation :
*     <FOUND> = USER(CMD, PARAMS, MAXPT, MAXBRK, X, Y, NPT, BRKS, NBRK,
*    :  TITLE, OK)
*    Parameters :
*     CMD*(*)=CHARACTER(READ)
*           Expression specifying the command name.   This is always
*           in upper case.
*     PARAMS*(*)=CHARACTER(READ)
*           Expression specifying the parameter values associated
*           with the command.   This character string is in a
*           form suitable for decoding with the DECODE routine.
*     MAXPT=INTEGER(READ)
*           Expression specifying the maximum size of the X and Y
*           arrays.
*     MAXBRK=INTEGER(READ)
*           Expression specifying the maximum number of break points
*           in the X and Y arrays.
*     X(MAXPT)=REAL(UPDATE)
*           Array containing the x-values for the current spectrum.
*     Y(MAXPT)=REAL(UPDATE)
*           Array containing the y-values for the current spectrum.
*     NPT=INTEGER(UPDATE)
*           Variable containing the number of points in the X and
*           Y arrays.
*     BRKS(MAXBRK)=INTEGER(UPDATE)
*           Array containing the break point positions in the X and
*           Y arrays.
*     NBRK=INTEGER(UPDATE)
*           Variable containing the number of break points stored
*           in the BRKS array.   (The minimum legitimate value
*           is 1 if there are any data in the X and Y arrays).
*     TITLE*(*)=CHARACTER(UPDATE)
*           Expression containing the title associated with
*           the current dataset.
*     OK=LOGICAL(WRITE)
*           Variable to receive success (OK=.TRUE.) or failure
*           (OK=.FALSE.) of DECODE call.
*    Result :
*     <FOUND>=LOGICAL
*           Whether command was found (.TRUE.) or not (.FALSE.).
*    Method :
*     See if the CMD string matches one of the commands available
*     from this routine.   If so then act accordingly.
*     The parameter string, PARAMS, may be decoded as appropriate for
*     the particular command.
*     The Y array can be modified arbitrarily.   However, if the
*     X array is modified or NPT is changed, then the break
*     points may need to be adjusted.   The spectrum is considered
*     to be broken up into a set of regions separated by "break
*     points":
*
*           START          END            REGION
*           -----          ---            ------
*
*           1              BRKS(1)        1
*           BRKS(1)+1      BRKS(2)        2
*
*           ..
*
*           BRKS(NBRK-1)+1 BRKS(NBRK)     NBRK
*
*     The significance of breaking the spectrum up into regions comes
*     when it is plotted:  each region is plotted separately, without
*     any connecting line.   Thus the various regions COULD be contours
*     (although they would not be very well distinguished).
*    Authors :
*     Jack Giddings (UCL::JRG)
*    History :
*     5-JUL-1983:  Original.   (UCL::JRG)
*     20-SEP-1983: SWITCH added.   (UCL::anon)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      CHARACTER*(*) CMD               ! Command name
      CHARACTER*(*) PARAMS            ! Parameter value string
      INTEGER MAXPT                   ! Maximum number of spectrum points
      INTEGER MAXBRK                  ! Maximum number of break points
*    Import-Export :
      REAL X(*)                       ! X-values
      REAL Y(*)                       ! Y-values
      INTEGER NPT                     ! Number of spectrum points
      INTEGER BRKS(*)                 ! Break points
      INTEGER NBRK                    ! Number of break points
      CHARACTER*(*) TITLE             ! Associated character string
*    Status :
      LOGICAL OK! Whether DECODE completed OK
*    Local constants :
      INTEGER MAXVAL                  ! Maximum number of parameter values
      PARAMETER(MAXVAL=10)
*    Local variables :
      REAL VALUES(MAXVAL)             ! Array for parameter values
      REAL RADIUS                     ! CIRCLE radius
      REAL DA                         ! Angle increment in radians
      REAL AMP                        ! SIN function amplitude
      REAL PHASE                      ! SIN function phase
      REAL WORV                       ! Wavelength OR Velocity
      INTEGER I                       ! Loop index
      INTEGER INDEX                   ! Stack index for SWITCH
      INTEGER NPTS, NBRKS             ! Temporary SWITCH variables
*-

*    Assume OK until error is detected

      OK = .TRUE.

*    Assume command found until it is not

      USER = .TRUE.

*    Compare command string to those handled here;  act if found.

*    TEST(P1,P2) - print parameter values

      IF (CMD .EQ. 'TEST') THEN

         VALUES(1) = 1.0
         VALUES(2) = 2.0
         VALUES(3) = 3.0
         CALL DECODE(CMD, PARAMS, 2, 3, VALUES, 'P1 P2 ', OK)
         IF (OK) THEN
            PRINT *, 'TEST VALUES: ',
     :      VALUES(1), VALUES(2), VALUES(3)
         ELSE
            PRINT *, 'Error in TEST parameters'
         ENDIF

*    CIRCLE(RADIUS) - create a circular spectrum

      ELSEIF (CMD .EQ. 'CIRCLE') THEN

         VALUES(1) = 1.0
         CALL DECODE(CMD, PARAMS, 1, 1, VALUES, 'RADIUS ', OK)
         IF (OK) THEN
            RADIUS = VALUES(1)
            NPT = MIN(128,MAXPT)
            DA = 2.0*3.141593/REAL(NPT-1)
            DO I = 1, NPT
               X(I) = RADIUS*COS(REAL(I-1)*DA)
               Y(I) = RADIUS*SIN(REAL(I-1)*DA)
            ENDDO
            NBRK = 1
            BRKS(NBRK) = NPT
         ELSE
            PRINT *, 'Error in CIRCLE parameters'
         ENDIF

*    SIN(AMP,PHASE) - create sine function spectrum

      ELSEIF (CMD .EQ. 'SIN') THEN

         VALUES(1) = 1.0
         VALUES(2) = 0.0
         CALL DECODE(CMD, PARAMS, 2, 2, VALUES, 'AMP PHASE ', OK)
         IF (OK) THEN
            AMP = VALUES(1)
            PHASE = VALUES(2)
            NPT = MIN(128,MAXPT)
            DA = 2.0*3.141593/REAL(NPT-1)
            DO I = 1, NPT
               X(I) = REAL(I-1)*DA + PHASE*2.0*3.141593
               Y(I) = AMP*SIN(X(I))
            ENDDO
            NBRK = 1
            BRKS(NBRK) = NPT
         ELSE
            PRINT *, 'Error in SIN parameters'
         ENDIF

*   SWITCH(INDEX) - switch STACK entry into current arrays, as
!                   an example of GETSTK

      ELSEIF (CMD .EQ. 'SWITCH') THEN

         CALL DECODE(CMD, PARAMS, 1, 1, VALUES, 'ENTRY ', OK)
         IF (OK) THEN
            INDEX = NINT(VALUES(1))
            NPTS  = MAXPT
            NBRKS = MAXBRK
            CALL GETSTK(INDEX, NPTS, X, Y, NBRKS, BRKS, TITLE,
     :      WORV, OK)
            IF (.NOT.OK) RETURN
            NPT = NPTS
            NBRK = NBRKS
            PRINT *, 'SWITCH completed'
         ELSE
            PRINT *, 'Error in SWITCH parameter'
         ENDIF

*    Command not found

      ELSE

         USER = .FALSE.

      ENDIF

      END
