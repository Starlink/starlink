
*+  KFH_NLPLOT - Plots a slice on the specified graphics device.
      SUBROUTINE KFH_NLPLOT(LINE,NPTS,STATUS)
*    Description :
*     This routine produces a plot of the slice produced
*     by KFH_NWSLICE.
*    Invocation :
*     CALL KFH_NLPLOT(LINE,NPTS,STATUS)
*    Parameters :
*     LINE(0:NPTS-1) = REAL
*           The array which holds the data of the slice.
*     NPTS = INTEGER
*           The number of points in the slice.
*     STATUS = INTEGER
*           The status value on entry to this subroutine.
*    Method :
*     This subroutine first takes the slice data
*     previously determined by KFH_NWSLICE and finds
*     its minimum and maximum values. These are then
*     used to calculate the scales of the plot. The
*     slice is plotted on the device chosen using
*     SIMPLEPLOT.
*    Authors :
*     S.Chan
*    History :
*     3 November 1983
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local constants :
      INTEGER MAXPTS
      PARAMETER(MAXPTS=4096)
*    Local variables :
      INTEGER NPTS                       ! The number of points which
*                                        ! make up the slice.
      REAL CLINE(MAXPTS)                 ! Copy of LINE.
      INTEGER I                          ! General variable.
      REAL LINE(0:NPTS-1)                ! The array containing the slice
*                                        ! data.
      REAL X(MAXPTS)                     ! Data points.
*-

*
*    If the status on entry is bad, then return to the calling
*    program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Make a copy of the line data.
*

         DO I = 1,NPTS

            CLINE(I) = LINE(I-1)

         END DO

*
*       Initialise the X-axis.
*

         DO I = 1,NPTS

            X(I) = REAL(I)

         END DO

*
*       Associate a device with the diagram.
*

*         CALL DIA_ASSOC('SDEVICE','WRITE',DIA,STATUS)

*
*       If something goes wrong , then exit from the
*       program.
*

*         IF (STATUS.NE.SAI__OK) THEN

*            RETURN

*         ENDIF

*
*       Set the value limits.
*

*         CALL DIA_PUTWL('V',X(1),X(NPTS),CLINE(1),
*     :     CLINE(NPTS),STATUS)

         CALL DIA_PUTAL('XV',0.0,REAL(NPTS),STATUS)

*
*       Begin the design.
*

         CALL DIA_BDES(STATUS)
         CALL DIA_ADESL(NPTS,X,CLINE,STATUS)

*
*       Define axis labels .
*

         CALL DIA_PUT0C('XLABEL','PIXEL',STATUS)
         CALL DIA_PUT0C('YLABEL','VALUE',STATUS)

*
*       Set major tick marks to be 0.5 character
*       height.
*

         CALL DIA_PUT0R('XTICK',0.5,STATUS)
         CALL DIA_PUT0R('YTICK',0.5,STATUS)

*
*       Set the title of the plot.
*

         CALL DIA_PUT0C('TITLE','SLICE',STATUS)

*
*       Set the size , position and length of the key.
*

         CALL DIA_PUT0I('KEYS',1,STATUS)
         CALL DIA_PUT0C('KEYPOS','INSIDE_TR',STATUS)
         CALL DIA_PUT0I('KEYLEN',5,STATUS)

*
*       Design and draw the axes.
*

         CALL DIA_DRAW(STATUS)

*
*       Plot the slice.
*

         CALL DIA_POLYL(NPTS,X,CLINE,STATUS)
         CALL DIA_KEY(1,.TRUE.,0,'SLICE',STATUS)

*
*       Reset all attributes.
*

         CALL DIA_RESET('*',STATUS)

*
*       Exit.
*

*         CALL DIA_ANNUL(DIA,STATUS)

      ENDIF

      END
