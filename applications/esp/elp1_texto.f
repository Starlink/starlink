      SUBROUTINE ELP1_TEXTO(MODE,NDF1,VALIDP,ZEROP,
     :     RESULT,RESNRES,RESNPOI,XCO,YCO,BACK,SIGMA,PSIZE,LBND,
     :     ISELLPRO,
     :     FIOD,EXCLAIM,STATUS)
*+
*  Name:
*     ELP1_TEXTO
*
*  Purpose:
*     Puts the most recent galaxy 'fit' results into a text format 
*     ASCII output file.
*     
*  Language:
*     Starlink Fortran 77
*
*  Invocation:
*      CALL ELP1_TEXTO(MODE,NDF1,VALIDP,ZEROP,
*                      RESULT,RESNRES,RESNPOI,XCO,YCO,BACK,SIGMA,PSIZE,LBND,
*                      ISELLPRO,
*                      FIOD,EXCLAIM,STATUS)    
*
*  Description:
*     Creates a text file (if required) and places in it data from the
*     most recent galaxy profile/fit generated.
*
*     The parameter MODE is used as follows:
*         MODE=0  Do all the actions in modes 1, 2 and 3.
*         MODE=1  Open file
*         MODE=2  Save headings and data.
*         MODE=3  Close the file.
*
*     All radii values output are measured in pixels.
*
*  Arguments:               
*     MODE = INTEGER (Given)
*        Used to show which part of the text file is to be created. 
*     NDF1 = INTEGER (Given)
*        NDF identifier for the image.
*     VALIDP = INTEGER (Given)
*        Number of ellipse radii fitted successfully
*     ZEROP = REAL (Given)
*        The magnitude scale zero point. Units magnitudes.
*     RESULT(RESNRES,RESNPOI) = REAL (Given)
*        Array containing the results.
*     RESNRES = INTEGER (Given)
*        The number of results in the RESULT array.
*     RESNPOI = INTEGER (Given)
*        The maximum number of points in the RESULT array.
*     XCO = REAL (Given)
*        The X index of the origin used. Units pixels.
*     YCO = REAL (Given)
*        The Y index of the origin used. Units pixels.
*     BACK = REAL (Given)
*        Image background value employed. Units counts.
*     SIGMA = REAL (Given)
*        Standard deviation of the background value. Units counts.
*     PSIZE = REAL (Given)
*        The image pixels size. Units arc secs.
*     LBND(10) = INTEGER (Given)
*        Lower limits of the image world co-ordinate system.
*     ISELLPRO = LOGICAL (Given)
*        Is this being called by ellpro rather than ellfou (the output
*        format is minutely different)?
*     FIOD = INTEGER (Given and Returned)
*        Output file FIO descriptor.
*     EXCLAIM = LOGICAL (Returned)
*        An exclaimation mark was returned for the file name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     NG: Norman Gray (Starlink, Glasgow)
*
*  History:
*     12-MAR-1993 (GJP)
*       (Original version)
*     20-FEB-1997 (GJP)
*       Output format modified.
*     20-Aug-1998 (NG)
*       Output format modified.
*     11-Nov-1999 (NG)
*       Output format modified again!  The previous version, by adding a
*       column, managed to confuse the graphs application.
*     15-Dec-1999 (NG)
*       ELP1_TEXTO and ELF1_TEXTO merged into this routine, which was
*       extracted from the ellpro.f monolithic source file.
*
*
*  Bugs:
*     None known.
*
*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'elp_par'               ! ELLPRO constants
      INCLUDE 'MSG_PAR'               ! Parameter system constants
      INCLUDE 'NDF_PAR'               ! NDF public constants

*  Arguments Given:                              
      INTEGER LBND(NDF__MXDIM)        ! Lower limits of image world
                                      ! co-ordinate system
      INTEGER MODE                    ! Defines which part of the file saving
                                      ! is to be performed.
      INTEGER NDF1                    ! NDF indentifier
      INTEGER VALIDP                  ! Number of radii fitted successfully
      INTEGER RESNRES                 ! number of results in RESULT array.
      INTEGER RESNPOI                 ! maximum number of points in RESULT.
      REAL BACK                       ! Background count value
      REAL PSIZE                      ! The size of each pixel in
                                      ! arc seconds
      REAL RESULT(RESNRES,RESNPOI)  ! Array containing the profiling
				      ! results.
      REAL SIGMA                      ! Standard deviation of the background
      REAL XCO                        ! X index of the origin
      REAL YCO                        ! Y index of the origin
      REAL ZEROP                      ! Magnitude scale zero point
      LOGICAL ISELLPRO		      ! Is this being called by ellpro?

*  Arguments Given and Returned:
      LOGICAL EXCLAIM                 ! Was an exclamation mark given 
                                      ! for the file name?
      INTEGER FIOD                    ! Output file FIO descriptor
      
*  Status:     
      INTEGER STATUS                  ! Global status

*   Local constants:
      INTEGER LINSIZ            ! Maximum line size
      PARAMETER (LINSIZ=120)

*  Local variables:
      CHARACTER *(LINSIZ) TEXT        ! The heading
      CHARACTER *(LINSIZ) LINE        ! FIO line output length
      CHARACTER *(LINSIZ) PREFIX      ! Output prefix
      CHARACTER *(MSG__SZMSG) NAME    ! NDF name
      LOGICAL OPENF                   ! Was the output file opened?
      INTEGER I                       ! Temporary variable
      INTEGER IWCS                    ! AST pointer to NDF's WCS frameset
      INTEGER J                       ! Temporary variable
      INTEGER NCHAR                   ! Length of output string

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Open the FIO file.
      IF ((MODE.EQ.0).OR.(MODE.EQ.1)) THEN

*      Determine the output text file name. If the file name chosen fails, 
*      the user is reprompted
         IF (MODE.EQ.0) CALL MSG_BLANK(STATUS)
         OPENF=.FALSE.             
         EXCLAIM=.FALSE.   
         CALL ERR_MARK
         DO WHILE((.NOT.OPENF).AND.(.NOT.EXCLAIM)
     :             .AND.(STATUS.EQ.SAI__OK))
            CALL ELP1_AIF_ASFIO('OUT','WRITE','LIST',LINSIZ,FIOD,OPENF,
     :                          EXCLAIM,STATUS)
            IF ((.NOT.OPENF).AND.(.NOT.EXCLAIM)) THEN
               CALL ERR_REP(' ','Bad file name.',STATUS)
               CALL ERR_REP(' ','For no file, type !',STATUS)
               CALL ERR_ANNUL(STATUS)
            END IF
         END DO
         CALL ERR_RLSE
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Inform the user if a difficulty was encountered and that an
*      an output file will not be used. 
         IF (EXCLAIM) THEN  
            CALL MSG_BLANK(STATUS)
            CALL MSG_OUT(' ','WARNING!!!',STATUS)
            CALL MSG_OUT(' ','No output text file created.',STATUS)
            CALL MSG_BLANK(STATUS)
            GOTO 9999
         END IF

      END IF

*   Output the heading, galaxy co-ordinates used and the profiling results.
      IF ((MODE.EQ.0).OR.(MODE.EQ.2)) THEN

*      Output a heading.
         NCHAR=0
         IF (ISELLPRO) THEN
            CALL CHR_PUTC('## ESP ELLPRO V1.1 OUTPUT FILE',
     :           LINE,NCHAR)
         ELSE
            CALL CHR_PUTC('## ESP ELLFOU V1.1 OUTPUT FILE',
     :           LINE,NCHAR)
         ENDIF
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTC('##',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output the file name.
         NCHAR=0
         CALL CHR_PUTC('## Filename: ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL NDF_MSG('NAME',NDF1)
         CALL MSG_LOAD(' ','^NAME',NAME,NCHAR,STATUS)
         CALL CHR_CLEAN(NAME)
         CALL FIO_WRITE(FIOD,NAME(:NCHAR),STATUS)
         
*      Output the standard deviation value that was used.
         NCHAR=0

         CALL CHR_PUTC('## Sigma (counts): ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(SIGMA,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output the image pixel size.
         NCHAR=0
         CALL CHR_PUTC('## Pixel size (arc secs): ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(PSIZE,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output X and Y data co-ordinates.
         NCHAR=0
         CALL CHR_PUTC('## X/Y co-ordinates (Base):',
     :        LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(1:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(XCO,LINE,NCHAR)
         CALL CHR_PUTC(' ',LINE,NCHAR)
         CALL CHR_PUTR(YCO,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output X and Y world co-ordinates.
         CALL NDF_GTWCS(NDF1,IWCS,STATUS)
         CALL ESP1_XYFMT(IWCS,XCO,YCO,'X','Y','DOM',STATUS)
         CALL MSG_LOAD(' ','## X/Y co-ordinates (^DOM):',LINE,NCHAR,
     :        STATUS)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         CALL ESP1_XYFMT(IWCS,XCO,YCO,'X','Y','DOM',STATUS)
         CALL MSG_LOAD(' ','^X ^Y',LINE,NCHAR,STATUS)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         CALL AST_ANNUL(IWCS,STATUS)
         
*      Output the background value that was used.
         NCHAR=0
         CALL CHR_PUTC('## Background (counts): ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(BACK,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output the magnitude zero point.
         NCHAR=0
         CALL CHR_PUTC('## Zero point of magnitude:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(ZEROP,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output the number of points determined.
         NCHAR=0
         CALL CHR_PUTC('## Number of points:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTI(VALIDP,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output the an ellipse parameters heading.
         NCHAR=0
         CALL CHR_PUTC('## Ellipse Parameters:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output a data description.
         NCHAR=0
         IF (ISELLPRO) THEN
*         Write the statistic in column 10 rather than next to the
*         `count'. The latter would be more logical, but confuses the
*         graphs application. 
            TEXT='X       Y     Points   MeanRad    Count     '//
     :           'PA    1/Ellipt     Dev  PPU    Statistic'
         ELSE
            TEXT='X       Y     Points   MeanRad    Count     '//
     :           'PA    1/Ellipt     Dev  PPU'
         ENDIF
         CALL CHR_PUTC('!! '//TEXT,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(1:NCHAR),STATUS)
         NCHAR=0
C      TEXT='                                     Mean    Statistic'
C      CALL CHR_PUTC('!! '//TEXT,LINE,NCHAR)
C      CALL FIO_WRITE(FIOD,LINE(1:NCHAR),STATUS)

*      Output the actual values.
         DO 400 I=1,VALIDP

*         Create an appropriately formatted output string.
            CALL MSG_FMTR('X','F6.1',RESULT(1,I))
            CALL MSG_FMTR('Y','F6.1',RESULT(2,I))
            CALL MSG_FMTI('N','I3',INT(RESULT(8,I)))
            CALL MSG_FMTR('RAD','F8.2',RESULT(4,I))
            CALL MSG_FMTR('VAL','F10.1',RESULT(6,I))
            CALL MSG_FMTR('POS','F6.1',RESULT(5,I))
            CALL MSG_FMTR('ELL','F5.3',RESULT(3,I))
            CALL MSG_FMTR('DEV','F8.1',RESULT(7,I))
            CALL MSG_FMTR('POI','F4.0',RESULT(9,I))
            IF (ISELLPRO) THEN
               CALL MSG_FMTR('MEDN','F10.1',RESULT(ELP__STAT,I))
               TEXT='^X  ^Y    ^N   ^RAD  ^VAL  ^POS   ^ELL'//
     :              '  ^DEV  ^POI  ^MEDN'
            ELSE
               TEXT='^X  ^Y    ^N   ^RAD  ^VAL  ^POS   ^ELL'//
     :              '  ^DEV  ^POI'
            ENDIF
            
*         Output the results in suitably formatted form.
            NCHAR=0
            CALL MSG_LOAD(' ',TEXT,NAME,J,STATUS)
            NAME=NAME(1:J)
            CALL CHR_CLEAN(NAME)
            CALL CHR_PUTC(NAME,LINE,NCHAR)
            CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

 400     CONTINUE

*      Output the a Fourier descriptor heading.
         NCHAR=0
         CALL CHR_PUTC('## Fourier Descriptors:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output a data description.
         NCHAR=0
         TEXT='MeanRad  1xSin   1xCos   2xSin   2xCos'//
     :        '   3xSin   3xCos   4xSin   4xCos'
         CALL CHR_PUTC('!! '//TEXT,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(1:NCHAR),STATUS)

*      Output the dimensions of the results
         NCHAR=0
         CALL MSG_SETI('VALIDP',VALIDP)
         CALL MSG_LOAD(' ','fourier (^VALIDP,9)=',
     :           LINE,NCHAR,STATUS)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output the actual values.
         DO 500 I=1,VALIDP

*         Create an appropriately formatted output string.
            CALL MSG_FMTR('RAD','F8.2',RESULT(4,I))
            CALL MSG_FMTR('FDS1','F6.3',RESULT(10,I))
            CALL MSG_FMTR('FDC1','F6.3',RESULT(11,I))
            CALL MSG_FMTR('FDS2','F6.3',RESULT(12,I))
            CALL MSG_FMTR('FDC2','F6.3',RESULT(13,I))
            CALL MSG_FMTR('FDS3','F6.3',RESULT(14,I))
            CALL MSG_FMTR('FDC3','F6.3',RESULT(15,I))
            CALL MSG_FMTR('FDS4','F6.3',RESULT(16,I))
            CALL MSG_FMTR('FDC4','F6.3',RESULT(17,I))
            TEXT=' ^RAD   ^FDS1  ^FDC1  ^FDS2  ^FDC2'// 
     :           '  ^FDS3  ^FDC3  ^FDS4  ^FDC4'
 
*         Output the results in suitably formatted form.
            NCHAR=0
            CALL MSG_LOAD(' ',TEXT,NAME,NCHAR,STATUS)
            CALL CHR_CLEAN(NAME)
            CALL FIO_WRITE(FIOD,NAME(:NCHAR),STATUS)

 500     CONTINUE

*      Add message describing storage units for radius.
         NCHAR=0
         TEXT='!! NOTE: Radii values are stored on file as semi-'/
     :        /'major axis length' 
         CALL CHR_PUTC(TEXT,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         TEXT='!!       measured in pixels but on screen as '/
     :        /'equivalent radii in arc secs.' 
         CALL CHR_PUTC(TEXT,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Add message describing position angle.
         NCHAR=0
         TEXT='!! NOTE: Position angles are stored on file with'/
     :        /' origin upward and clockwise rotation positive.' 
         CALL CHR_PUTC(TEXT,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Add file terminator.
         NCHAR=0
         TEXT='## END'
         CALL CHR_PUTC(TEXT,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

      END IF

*  Close down the file output.
      IF ((MODE.EQ.0).OR.(MODE.EQ.3)) CALL FIO_CLOSE(FIOD,STATUS)

 9999 CONTINUE

      END 


