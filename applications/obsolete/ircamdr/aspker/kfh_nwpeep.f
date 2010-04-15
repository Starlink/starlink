
*+  KFH_NWPEEP - Displays part of an image on a terminal
      SUBROUTINE KFH_NWPEEP(PIC,NX,NY,IX0,IY0,STATUS)
*    Description :
*     This routine displays a formatted listing of a 9x9
*     section of an image on the terminal. The data is
*     automatically scaled to give a pleasing format. The
*     central pixel is passed over as a parameter.
*    Invocation :
*     CALL KFH_NWPEEP(PIC,NX,NY,IX0,IY0,STATUS)
*    Parameters :
*     PIC(NX,NY) = REAL
*           The array which holds the image data.
*     NX = INTEGER
*           The X dimension of the image.
*     NY = INTEGER
*           The Y dimension of the image.
*     IX0 = INTEGER
*           The X coordinate of the centre of the
*           selected 9x9 section of the image.
*     IY0 = INTEGER
*           The Y coordinate of the centre of the
*           selected 9x9 section of the image.
*     STATUS = INTEGER
*           The status value on entry to this
*           subroutine.
*    Method :
*     A 9x9 section from an image (defined by the
*     central pixel) is selected , and a suitable
*     scaling factor found. The chosen pixels are
*     then scaled and written (in F9.0 format) to
*     the terminal. The coordinates of the vertices
*     and the scaling factor are also reported.
*    Authors :
*     K.F.Hartley (RGVAD::KFH)
*     S.Chan (RGVAD::KFH)
*    History :
*     Pre-historic: Original interim environment version (PTW)
*     7 July 1983: First SAI version (KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER NX                         ! The X-dimension of the image.
      INTEGER NY                         ! The Y-dimension of the image.
      REAL PIC(NX,NY)                    ! Array containing the image
*                                        ! data.
      REAL FACTOR                        ! Scaling factor for the section
*                                        ! of data to be looked at.
      INTEGER IFACEXP                    ! Variable used in the calculation
*                                        ! of the scaling factor.
      REAL DMAX                          ! The maximum value in the section
*                                        ! of data.
      INTEGER IX0                        ! The X-coordinate of the selected
*                                        ! 9x9 section of the image.
      INTEGER IY0                        ! The Y-coordinate of the selected
*                                        ! 9x9 section of the image.
      INTEGER IX1                        ! Variable used in scaling.
      INTEGER IY1                        ! Variable used in scaling.
      INTEGER IXR                        ! X-coordinate for the right hand
*                                        ! corner of the 9x9 section.
      INTEGER IYT                        ! Y-coordinate for the top
*                                        ! corner of the 9x9 section.
      INTEGER IXL                        ! X-coordinate for the left hand
*                                        ! corner of the 9x9 section.
      INTEGER IYB                        ! Y-coordinate for the bottom
*                                        ! corner of the 9x9 section.
      INTEGER IX                         ! General variable.
      INTEGER IY                         ! General variable.
      INTEGER I                          ! General variable.
      INTEGER J                          ! General variable.
      REAL V                             ! Variable holding the result of
*                                        ! multiplying a data value and
*                                        ! the scaling factor.
      INTEGER NPTR                       ! Pointer to characters in the
*                                        ! output buffer.
      CHARACTER*9 CWK                    ! Variable holding a string of
*                                        ! characters which are output
*                                        ! whenever an incomplete section
*                                        ! is chosen (e.g. if (1,1) is the
*                                        ! central pixel)
      CHARACTER*72 CLINE                 ! Output buffer.
*-

*
*    If the status value is bad, then return to the main program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       X,Y coordinates for left , right , top , bottom.
*

         IXR = IX0+4
         IYT = IY0+4
         IXL = IX0-4
         IYB = IY0-4

*
*       Report X,Y for top right hand corner.
*

         WRITE (CLINE,'(61X,''('',I4,'','',I4,'')'')') IXR,IYT
         CALL MSG_OUT('OUT1',' ',STATUS)
         CALL MSG_OUT('OUT2',CLINE,STATUS)
         CALL MSG_OUT('OUT3',' ',STATUS)

*
*       Auto scaling
*

         IF (IX0.LT.1) THEN

            IX1 = 1

         ELSEIF (IX0.GT.NX) THEN

            IX1 = NX

         ELSE

            IX1 = IX0

         ENDIF

         IF (IY0.LT.1) THEN

            IY1 = 1

         ELSEIF (IY0.GT.NY) THEN

            IY1 = NY

         ELSE

            IY1 = IY0

         ENDIF

         DMAX = PIC(IX1,IY1)

         DO J = IYB,IYT

            IF (J.GE.1.AND.J.LE.NY) THEN

               DO I = IXL,IXR

                  IF (I.GE.1.AND.I.LE.NX) THEN

                     IF ( PIC(I,J).GT.DMAX) THEN

                        DMAX = PIC(I,J)

                     ENDIF

                  ENDIF

               END DO

            ENDIF

         END DO

         IF (DMAX.GT.999999.0.OR.DMAX.LE.99.0) THEN

            FACTOR = 999999.0/DMAX
            FACTOR = ALOG10(ABS(FACTOR))
            IFACEXP = INT(FACTOR)
            FACTOR = 10**INT(FACTOR)

         ELSE

            FACTOR = 1.0

         ENDIF

*
*       Main report.
*

         DO J = 1,9

            IY = IY0-J+6

            DO I = 1,9

               IX = IX0+I-4

               IF (IX.LT.1.OR.IX.GT.NX.OR.IY.LT.1.OR
     :          .IY.GT.NY) THEN

                  CWK = ' ........ '

               ELSE

                  V = NINT(PIC(IX,IY)*FACTOR)

                  IF (V.GT.9999999.0) THEN

                     CWK = ' +++++++ '

                  ELSEIF (V.LT.-999999.0) THEN

                     CWK = ' ------- '

                  ELSE

                     WRITE (CWK,'(F9.0)') V

                  ENDIF

               ENDIF

               NPTR = (I-1)*8+1
               CLINE(NPTR:NPTR+7) = CWK(1:8)

            END DO

            CALL MSG_OUT('OUT4',CLINE,STATUS)

         END DO

*
*       Report bottom left hand corner.
*

         WRITE (CLINE,'(''('',I4,'','',I4,'')'')') IXL,IYB
         CALL MSG_OUT('OUT5',' ',STATUS)
         CALL MSG_OUT('OUT6',CLINE,STATUS)
         CALL MSG_OUT('OUT7',' ',STATUS)

*
*       Report auto-scaling factor.
*

         IF (FACTOR.NE.1.0) THEN

            WRITE (CLINE,'(''Values have been multiplied by 10**'',
     :             I3)') IFACEXP
            CALL MSG_OUT('OUT8',CLINE,STATUS)

         ELSE

            CALL MSG_OUT('OUT9','Scaling factor of 1 used',STATUS)

         ENDIF

         CALL MSG_OUT('LINE',' ',STATUS)

      ENDIF

      END
