*+  KFH_NHPLOT - Plots a histogram.
      SUBROUTINE KFH_NHPLOT(HIST,NHIST,HMIN,HMAX,STATUS)
*    Description :
*     This routine plots the histogram of an image on
*     the specified graphics device. The histogram is
*     calculated from the NWHIST subroutine.
*    Invocation :
*     CALL KFH_NHPLOT(HIST,NHIST,HMIN,HMAX,STATUS)
*    Parameters :
*     HIST(NHIST) = INTEGER
*           This array contains the histogram of the image.
*     NHIST = INTEGER
*           This is the number of bins in the histogram.
*     HMIN = REAL
*           This is the minimum value of the image data.
*     HMAX = REAL
*           This is the maximum value of the image data.
*     STATUS = INTEGER
*           This is the status value on entry to this
*           subroutine.
*    Method :
*     First a copy of the histogram is made so that it
*     can be altered by this subroutine. The spanning
*     range of the X-axis is set to the minimum and
*     maximum values of the data. The user is asked
*     whether a change in the range is required. If so ,
*     the user-given values are taken , otherwise the
*     values are used. This is repeated for the range
*     of the Y-axis. The type of axes of the plot is set
*     as LINEAR , but a change in this has been
*     accommodated. If the user changes the axes , his
*     axis types are taken. The types are then checked to
*     determine whether they conflict with the X and Y
*     ranges given (e.g. LOG axes produce an error when
*     the scale contains a zero). If everything is OK ,
*     the histogram is plotted (using SIMPLEPLOT).
*    Authors :
*     S.Chan
*    History :
*     28 September 1983
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER NHIST                     ! The number of bins in
*                                       ! the histogram.
      REAL BINSZ                        ! The size of the bins.
      REAL CHIST(5000)                  ! Copy of the histogram.
      INTEGER HIST(NHIST)               ! The array containing the
*                                       ! histogram of the image.
      REAL HMAX                         ! The maximum value of the
*                                       ! image.
      REAL HMIN                         ! The minimum value of the
*                                       ! image.
      INTEGER I                         ! General variable.
      REAL MAX                          ! Value of largest bin.
      REAL RANGE                        ! The range of pixel values.
      REAL X(5000)                      ! Variable used in the plotting
*                                       ! of the histogram.
*-

*
*    If the status is bad, then return to the main program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Calculate the size of each bin.
*

         RANGE = HMAX-HMIN
         BINSZ = RANGE/NHIST

*
*       Make a copy of the histogram.
*

         DO I = 1,NHIST

            CHIST(I) = REAL(HIST(I))

         END DO

*
*       Find the largest bin.
*

         MAX = CHIST(1)

         DO I = 1,NHIST

            IF (CHIST(I).GT.MAX) THEN

               MAX = CHIST(I)

            ENDIF

         END DO

*
*       Set the X-axis.
*

         DO I = 1,NHIST

            X(I) = REAL(I)

         END DO

*
*       Associate a device with the diagram.
*

*         CALL DIA_ASSOC('HDEVICE','WRITE',DIA,STATUS)

*
*       If something is wrong , then exit from the program.
*

*         IF (STATUS.NE.SAI__OK) THEN

*            RETURN

*         ENDIF

*
*       Set the value limits (XVMIN,XVMAX,YVMIN,YVMAX).
*

*         CALL DIA_PUTWL('V',X(1),X(NHIST),HIST(1),HIST(NHIST),
*     :     STATUS)

         CALL DIA_PUTAL('XV',0.0,REAL(NHIST),STATUS)
         CALL DIA_PUTAL('YV',0.0,MAX,STATUS)

*
*       Begin the design.
*

         CALL DIA_BDES(STATUS)
         CALL DIA_ADESL(NHIST,X,CHIST,STATUS)

*
*       Define the axis labels.
*

         CALL DIA_PUT0C('XLABEL','BIN NUMBER',STATUS)
         CALL DIA_PUT0C('YLABEL','NUMBER',STATUS)

*
*       Set the major tick marks to be 0.5 character height.
*

         CALL DIA_PUT0R('XTICK',0.5,STATUS)
         CALL DIA_PUT0R('YTICK',0.5,STATUS)

*
*       Set the title of the plot.
*

         CALL DIA_PUT0C('TITLE','HISTOGRAM OF CURRENT REGION',
     :     STATUS)

*
*       Design and draw the axes.
*

         CALL DIA_DRAW(STATUS)

*
*       Plot the histogram.
*

         CALL DIA_HISTL(NHIST,X,CHIST,STATUS)

*
*       Reset all attributes.
*

         CALL DIA_RESET('*',STATUS)

*
*       Exit the program.
*

*         CALL DIA_ANNUL(DIA,STATUS)

      ENDIF

      END
