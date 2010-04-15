      SUBROUTINE SURF_WRITE_PHOTOM_HEADER (ODF, OBS_DATE, OBS_TIME,
     :     ANALYSIS, RUN_NUMBER, OBJECT, SUB_INSTRUMENT, FILTER,
     :     CENTRE_COORDS, LAT, LONG, LAT2, LONG2, MJD1, MJD2,
     :     OFFSET_COORDS, MAP_X, MAP_Y, SAMPLE_COORDS, SAMPLE_PA,
     :     SKY_SUBTRACTION, FD, STATUS)
*+
*  Name:
*     SURF_WRITE_PHOTOM_HEADER

*  Purpose:
*     Write photom information to text file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL SURF_WRITE_PHOTOM_HEADER (ODF, OBS_DATE, OBS_TIME,
*     :     ANALYSIS, RUN_NUMBER, OBJECT, SUB_INSTRUMENT, FILTER,
*     :     CENTRE_COORDS, LAT, LONG, LAT2, LONG2, MJD1, MJD2,
*     :     OFFSET_COORDS, MAP_X, MAP_Y, SAMPLE_COORDS, SAMPLE_PA,
*     :     SKY_SUBTRACTION, FD, STATUS)

*  Description:
*     This routine writes out the header for 1 sub-instrument of a PHOTOM
*     observation.
*        If status is good on entry the routine will call FIO_ASSOC to open
*     the ASCII file to hold the results; the filename will be read from
*     parameter 'FILE'. FIO_WRITE will then be called to write out the results
*     in the following format:-
*
*      Output from SURF reduction of a PHOTOM observation
*      Reduction date        : <date>
*      Observation definition: <ODF name>
*      Date of observation   : <date>
*      Time of observation   : <time>
*      Run number            : <number>
*      Object                : <object>
*      Sub-instrument        : <name>
*      Filter                : <name>
*      Centre coords         : <coord system of centre>
*      Latitude              : <latitude>
*      Longitude             : <longitude>
*
*     If the centre coordinate system is PLANET then:-
*      2nd latitude          : <latitude on MJD2>
*      2nd longitude         : <longitude on MJD2>
*      date of 1st position  : <modified Julian day when source at lat,long>
*      date of 2nd position  : <modified Julian day when source at lat2,long2>
*     end if
*
*      Offset coords         : <coord system of source offset>
*      x offset              : <x offset of source>
*      y offset              : <y offset of source>
*      Sampe coords          : <coord system of jiggle offsets>
*      Sample position angle : <angle that x axis of jiggle offsets is rotated
*                              anticlockwise from the x axis of the sample
*                              coord system>
*      Sky error removal     : <TRUE if the SURF REMSKY application has been
*                              run on the data>
*      Analysis mode         : AVERAGE or PARABOLA
*

*  Arguments:
*     ODF                    = CHARACTER*(*) (Given)
*           the name of the observation definition file
*     OBS_DATE               = CHARACTER*(*) (Given)
*           the date of the observation
*     OBS_TIME               = CHARACTER*(*) (Given)
*           the UT of the observation
*     RUN_NUMBER             = INTEGER (Given)
*           the observation number
*     OBJECT                 = CHARACTER*(*) (Given)
*           the name of the object observed
*     SUB_INSTRUMENT         = CHARACTER*(*) (Given)
*           the name of the sub-instrument used
*     FILTER                 = CHARACTER*(*) (Given)
*           the name of the filter used
*     CENTRE_COORDS          = CHARACTER*(*) (Given)
*           the coordinate system of the telescope centre coords
*     LAT                    = CHARACTER*(*) (Given)
*           the latitude of the telescope centre
*     LONG                   = CHARACTER*(*) (Given)
*           the longitude of the telescope centre
*     LAT2                   = CHARACTER*(*) (Given)
*           the second source latitude if CENTRE_COORDS is PLANET
*     LONG2                  = CHARACTER*(*) (Given)
*           the second source longitude if CENTRE_COORDS is PLANET
*     MJD1                   = DOUBLE PRECISION (Given)
*           the modified Julian date of LAT,LONG if CENTRE_COORDS is PLANET
*     MJD2                   = DOUBLE PRECISION (Given)
*           the modified Julian date of LAT2,LONG2 if CENTRE_COORDS is PLANET
*     OFFSET_COORDS          = CHARACTER*(*) (Given)
*           the coordinate system of the map centre offsets
*     MAP_X                  = REAL (Given)
*           the x offset of the map centre
*     MAP_Y                  = REAL (Given)
*           the y offset of the map centre
*     SAMPLE_COORDS          = CHARACTER*(*) (Given)
*           the coordinate system of the jiggle offsets
*     SAMPLE_PA              = REAL (Given)
*           the position angle by which the x-axis of the jiggle offsets is
*           rotated (anti-clockwise) from the x-axis of the SAMPLE_COORDS
*           system
*     SKY_SUBTRACTION        = LOGICAL (Given)
*           .TRUE. if the sky error has been removed
*     MAX_BEAM               = INTEGER (Given)
*           the maximum number of bolometers that can observe the source
*           in a single observation
*     PHOT_BB (MAX_BEAM)     = INTEGER (Given)
*           the indices of the bolometers used to observe the source in
*           each beam in the BOL_CHAN and BOL_ADC arrays
*     FD                     = INTEGER (Returned)
*           File descriptor of output file
*     STATUS                 = INTEGER (Given and returned)
*           global status


*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)
*     T.Jenness (JAC)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Bugs:

*  History:
*     $Id$
*     $Log$
*     Revision 1.6  1999/08/19 03:37:46  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.5  1999/08/03 20:01:45  timj
*     Add copyright message to header.
*     Minor fixes to header style.
*
*     13-MAR-1996: original version
*    endhistory

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'SURF_PAR'               ! SURF constants

*  Arguments Given:
      CHARACTER*(*) ODF
      CHARACTER*(*) OBS_DATE
      CHARACTER*(*) OBS_TIME
      INTEGER       RUN_NUMBER
      CHARACTER*(*) OBJECT
      CHARACTER*(*) SUB_INSTRUMENT
      CHARACTER*(*) FILTER
      CHARACTER*(*) ANALYSIS
      CHARACTER*(*) CENTRE_COORDS
      CHARACTER*(*) LAT
      CHARACTER*(*) LONG
      CHARACTER*(*) LAT2
      CHARACTER*(*) LONG2
      DOUBLE PRECISION MJD1
      DOUBLE PRECISION MJD2
      CHARACTER*(*) OFFSET_COORDS
      REAL          MAP_X
      REAL          MAP_Y
      CHARACTER*(*) SAMPLE_COORDS
      REAL          SAMPLE_PA
      LOGICAL       SKY_SUBTRACTION

*  Arguments Given & Returned:

*  Arguments Returned:
      INTEGER            FD              ! FIO file identifier

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:
      INTEGER RECLEN                     ! length of record written to file
      PARAMETER (RECLEN = 80)            !

*  Local variables:
      INTEGER            ITEMP           ! scratch integer
      CHARACTER*(RECLEN) LINE            ! line to be written to file
      INTEGER            NTICKS          ! number of ticks since some date

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  open the file

      CALL FIO_ASSOC ('FILE', 'WRITE', 'LIST', 0, FD, STATUS)

      IF (STATUS .NE. SAI__OK) RETURN

*  write header information

      LINE = 'Output from '//PACKAGE//
     :     ' reduction of a PHOTOM observation'
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Reduction date        : '
      CALL PSX_TIME (NTICKS, STATUS)
      CALL PSX_CTIME (NTICKS, LINE(25:), STATUS)
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Observation definition: '//ODF
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Date of observation   : '//OBS_DATE
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Time of observation   : '//OBS_TIME
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Run number            : '
      CALL CHR_ITOC (RUN_NUMBER, LINE(25:), ITEMP)
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Object                : '//OBJECT
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Sub-instrument        : '//SUB_INSTRUMENT
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Filter                : '//FILTER
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Centre coords         : '//CENTRE_COORDS
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Latitude              : '//LAT
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Longitude             : '//LONG
      CALL FIO_WRITE (FD, LINE, STATUS)

      CALL CHR_UCASE (CENTRE_COORDS)
      IF (CENTRE_COORDS .EQ. 'PLANET') THEN
         LINE = '2nd latitude          : '//LAT2
         CALL FIO_WRITE (FD, LINE, STATUS)

	 LINE = '2nd longitude         : '//LONG2
         CALL FIO_WRITE (FD, LINE, STATUS)

         LINE = 'date of 1st position  : '
         CALL CHR_DTOC (MJD1, LINE(25:), ITEMP)
         CALL FIO_WRITE (FD, LINE, STATUS)

         LINE = 'date of 2nd position  : '
         CALL CHR_DTOC (MJD1, LINE(25:), ITEMP)
         CALL FIO_WRITE (FD, LINE, STATUS)
      END IF

      LINE = 'Offset coords         : '//OFFSET_COORDS
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'x offset              : '
      CALL CHR_RTOC (MAP_X, LINE(25:), ITEMP)
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'y offset              : '
      CALL CHR_RTOC (MAP_Y, LINE(25:), ITEMP)
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Sample coords         : '//SAMPLE_COORDS
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Sample position angle : '
      CALL CHR_RTOC (SAMPLE_PA, LINE(25:), ITEMP)

      LINE = 'Sky error removal     : '
      CALL CHR_LTOC (SKY_SUBTRACTION, LINE(25:), ITEMP)
      CALL FIO_WRITE (FD, LINE, STATUS)
      CALL FIO_WRITE (FD, ' ', STATUS)

      LINE = 'Photometric analysis  : '//ANALYSIS
      CALL FIO_WRITE (FD, LINE, STATUS)


  10  FORMAT (I4, '  ', 2E11.3, ' ', F8.3, 2E11.3)
  15  FORMAT (I4, ' Bad integration')
  20  FORMAT ('      ', 2E11.3, ' ', F8.3, 2E11.3)
  30  FORMAT ('      ', 2E11.3, ' ', F8.3)

      END
