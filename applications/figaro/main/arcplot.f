      SUBROUTINE ARCPLOT(XPLOT,YPLOT,COEFF,RESULTS,WAVES,ARC,RESVAR
     :     ,XSECT,IOPT,ERROR,WEIGHT,ORDER,LINNAM,POLYDATA,POLYTAB
     :     ,USENAGERR,STATUS)
*+
*  Name:
*     ARCPLOT

*  Purpose:
*     Plot arc dispersion information and control fitting

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARCPLOT(XPLOT,YPLOT,COEFF,RESULTS,WAVES,ARC,RESVAR,XSECT,IOPT
*           ,ERROR,WEIGHT,ORDER,LINNAM,POLYDATA,POLYTAB,USENAGERR,
*           STATUS)

*  Description:
*     Plots of the dispersion relation and residuals on the fit are
*    made, and the user can alter the settings, request another fit,
*    etc.

*  Arguments:
*     XPLOT = REAL ARRAY (Given)
*        X axis array data
*     YPLOT = REAL ARRAY (Workspace)
*        Must be of dimension at least WAVDIM
*     COEFF = DOUBLE PRECISION ARRAY (Given)
*        Coefficients of fit
*     YPOSIT = REAL ARRAY (Given)
*        Fitted positions of lines
*     WAVES = REAL ARRAY (Given)
*        Wavelengths of lines
*     ARC(NSLCT,LINE_COUNT) = INTEGER*2 ARRAY (Given)
*        Arc useage array
*     YVAR = REAL ARRAY (Given)
*        Variances on YPOSIT
*     XSECT = INTEGER (Given)
*        Current cross-section
*     IOPT = INTEGER (Returned)
*        Option selected on exit:
*            1 - Accept current parameters, and fit whole frame
*            2 - Quit
*            3 - Try fitting with current parameters etc.
*            4 - More plots (dja_arcurve)
*     ERROR = INTEGER (Given)
*        0 if we have a successful fit to plot
*     WEIGHT = LOGICAL (Given and returned)
*        If to weight fits
*     ORDER = INTEGER (Given)
*        Order for fits
*     LINNAM = CHARACTER*10 ARRAY (Given)
*        Names of lines
*     STATUS = INTEGER (Given and returned)
*        Global status

*  Global variables:
*     WAVDIM = INTEGER (Given)
*        Number of bins in wavelength direction of arc frame (in
*        arc_dims)
*     LINE_COUNT = INTEGER (Given)
*        Number of lines identified (in arc_dims)
*     SPDIM1 = INTEGER (Given)
*        2nd dimension of arc frame (in arc_dims)

*  Authors:
*     TNW: T.N.Wilkins (Durham)
*     AJH: A.J.Holloway (Manchester)
*     ACD: A C Davenhall (Edinburgh)
*     {enter_new_authors_here}

*  History:
*     26-AUG-1993 (TNW):
*        Original version.
*     8-OCT-1993 (TNW):
*        Converted from 2df to twodspec
*     13-DEC-1993 (TNW):
*        Improvements to scrollbars
*     24-JAN-1994 TNW:
*        Use TEST_AREAS
*     28-JAN-1994 TNW:
*        Minor optimisations to code
*     9-FEB-1994 TNW:
*        Use SLIDER, change top plot to dispersion, etc.
*     Nov-1997 AJH:
*        Fixed intial data table production under linux
*     18-DEC-2000 ACD:
*        Removed the spurious argument NBAD in the calls to GR_RANGE.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing
      INCLUDE 'PRM_PAR'
      INCLUDE 'SAE_PAR'
      INCLUDE 'arc_dims'
      INCLUDE 'gr_inc'
      INTEGER STATUS,XSECT,IOPT
      INTEGER ERROR,ORDER,MAX_ORDER
      PARAMETER (MAX_ORDER = 10)
      LOGICAL WEIGHT,POLYDATA,POLYTAB,USENAGERR
      CHARACTER*10 LINNAM(LINE_COUNT)
      REAL XPLOT(WAVDIM),YPLOT(WAVDIM),YMAX,YMIN,XMIN,XMAX,YVMAX,YVMIN
      PARAMETER (YVMIN = 0.05)
      REAL RESULTS(MXPARS,NYP,NXP)
      REAL RESVAR(MXPARS,NYP,NXP)
      REAL WAVES(LINE_COUNT),VALUE
      REAL YPOS,XERR(2),YERR(2),SIGMA,DIST,TDIST,X,Y,Y1,Y2,YD,XCUR,
     :     YCUR
      DOUBLE PRECISION COEFF(*),DCOEFF(MAX_ORDER)
      INTEGER*2 ARC(NSLCT,LINE_COUNT)
      INTEGER I,PGCURSE,LINE,LEN1,NXSECT,K1,NSCRN,SLINE,ELINE,SCREEN
      CHARACTER*80 CHARS
      CHARACTER*20 WSTRING,NSTRING
      INTEGER WLEN,NLEN,DELETE,CHR_LEN,PPOSD,PPOSC,GET_PARNUM,
     :     PPOS
      INTEGER TNW_CPUTR

* Define geometry

*  X limits of main bit of plot

      REAL XMAIN1,XMAIN2
      PARAMETER (XMAIN1 = 0.07, XMAIN2 = 0.84)

*  Limits of weights indicator

      REAL WEIX1,WEIX2,WEIY1,WEIY2
      PARAMETER (WEIX1 = 0.915, WEIX2 = 1.0, WEIY1 = 0.89, WEIY2 = 0.96)

*  Limits of used data indicator

      REAL DATX1,DATX2,DATY1,DATY2
      PARAMETER (DATX1 = WEIX1, DATX2 = WEIX2, DATY1 = 0.82, DATY2 =
     :     WEIY1)

*  Limits of tables indicator

      REAL TABX1,TABX2,TABY1,TABY2
      PARAMETER (TABX1 = WEIX1, TABX2 = WEIX2, TABY1 = 0.75, TABY2 =
     :     DATY1)

*  Limits of indicator if using Nag errors for fits.

      REAL NAGX1,NAGX2,NAGY1,NAGY2
      PARAMETER (NAGX1 = WEIX1, NAGX2 = WEIX2, NAGY1 = 0.68, NAGY2 =
     :     TABY1)

*  Limits of xsect bar

      REAL XSBX1,XSBX2,XSBY1,XSBY2,XSBAY1,XSBAY2
      PARAMETER (XSBX1 = 0.9, XSBX2 = 0.91, XSBY1 = 0.05, XSBY2 = 0.91,
     :     XSBAY1 = XSBY1 - 0.04, XSBAY2 = XSBY2 + 0.04)

*  Limits of order bar

      REAL ORBX1,ORBX2,ORBY1,ORBY2,ORBAY1, ORBAY2
      PARAMETER (ORBX1 = 0.96, ORBX2 = 0.97, ORBY1 = 0.05, ORBY2 = 0.55,
     :     ORBAY1 = ORBY1 - 0.04, ORBAY2 = ORBY2 + 0.04)

* Char 127 is returned for delete, but others too (probably any control)
* we'll use it as delete though

      PARAMETER (DELETE = 127)
      CHARACTER CUR,CHR_UPPER
      LOGICAL LOOP,INLOOP,TABLE,EDIT
      REAL YTEST,XTEST0,YTEST0,YTEST1

* We have 2 lists, LIST is in normalised device coordinates, WLIST is in
* world (data) coordinates

      INTEGER NLIST,NWLIST,MAXWLIST
      PARAMETER (NLIST = 2, MAXWLIST = 20)
      REAL LIST(6,NLIST),WLIST(4,MAXWLIST),YSPTEST,ORDMAX
      INTEGER ID,ID2,MARKER,OLDVAL,COUNT,IORDMAX

      IORDMAX = MIN(MAX_ORDER,LINE_COUNT-1)

      LIST(1,1) = ORBX1
      LIST(2,1) = ORBX2
      LIST(3,1) = ORBAY1
      LIST(4,1) = ORBAY2
      LIST(5,1) = 0.0
      LIST(6,1) = REAL(IORDMAX)

      ORDMAX = LIST(6,1) + 0.5

      LIST(1,2) = XSBX1
      LIST(2,2) = XSBX2
      LIST(3,2) = XSBAY1
      LIST(4,2) = XSBAY2
      LIST(5,2) = 1.0
      LIST(6,2) = REAL(SPDIM1)

* Open graphics (if not already open)

      CALL GR_SOFT(STATUS)

* Get position of centres

      PPOSD = GET_PARNUM('Centre_1')
      PPOSC = GET_PARNUM('Contincent')

      NSCRN = (LINE_COUNT+14)/15
      SCREEN = 1
      NXSECT = XSECT
      K1 = ORDER + 1
      TABLE = ERROR.NE.SAI__OK

      LOOP = STATUS.EQ.SAI__OK
      DO WHILE(LOOP)

*  Start plots, with order indicator

         CALL PGBBUF
         CALL PGPAGE

* First indicate order

         CALL SLIDER('ORDER','D',LIST(1,1),ORDER,OLDVAL)

*  Then whether we're using weights

         CALL PGVPORT(WEIX1,WEIX2,WEIY1,WEIY2)
         CALL PGWINDOW(0.0,1.0,0.0,1.0)
         CALL PGSCI(1)
         CALL PGTEXT(0.01,0.6,'Weights')
         IF(WEIGHT) THEN
            CALL PGTEXT(0.01,0.2,'ON')
         ELSE
            CALL PGTEXT(0.01,0.2,'OFF')
         ENDIF

*  Which data we're using

         CALL PGVPORT(DATX1,DATX2,DATY1,DATY2)
         CALL PGWINDOW(0.0,1.0,0.0,1.0)
         CALL PGSCI(1)
         CALL PGTEXT(0.01,0.6,'Data')
         IF(POLYDATA) THEN
            CALL PGTEXT(0.01,0.2,'Polyfit')
            PPOS = PPOSC
         ELSE
            CALL PGTEXT(0.01,0.2,'Original')
            PPOS = PPOSD
         ENDIF

*  Whether we give tables

         CALL PGVPORT(TABX1,TABX2,TABY1,TABY2)
         CALL PGWINDOW(0.0,1.0,0.0,1.0)
         CALL PGSCI(1)
         CALL PGTEXT(0.01,0.6,'Tables')
         IF(POLYTAB) THEN
            CALL PGTEXT(0.01,0.2,'ON')
         ELSE
            CALL PGTEXT(0.01,0.2,'OFF')
         ENDIF

*  Whether we use fits with Nag errors

         IF(.NOT.POLYDATA) THEN
            CALL PGVPORT(NAGX1,NAGX2,NAGY1,NAGY2)
            CALL PGWINDOW(0.0,1.0,0.0,1.0)
            CALL PGSCI(1)
            CALL PGTEXT(0.01,0.6,'Nag fail')
            IF(USENAGERR) THEN
               CALL PGTEXT(0.01,0.2,'USED')
            ELSE
               CALL PGTEXT(0.01,0.2,'NOT USED')
            ENDIF
         ENDIF

         IF(SPDIM1.GT.1) THEN

*  Then current xsect

            CALL SLIDER('XSECT','D',LIST(1,2),NXSECT,OLDVAL)
         ENDIF
         NWLIST = 0
         IF(.NOT.TABLE) THEN
            DO I = 1, K1 - 1
               DCOEFF(I) = COEFF(I) * REAL(K1 - I)
           ENDDO
            CALL EPOLYA(WAVDIM,K1-1,DCOEFF,XPLOT,YPLOT)
            CALL GR_RANGE(YPLOT,1,WAVDIM,YMIN,YMAX,STATUS)

* First plot, of calculated dispersion against position

            CALL PGVPORT(XMAIN1,XMAIN2,0.53,0.96)
            CALL PGWINDOW(XPLOT(1),XPLOT(WAVDIM),YMIN,YMAX)
            CALL PGBOX('BCNST',0.0,0,'BCNST',0.0,0)
            CALL PGLINE(WAVDIM,XPLOT,YPLOT)

*  Calculate residuals

            COUNT = 0
            DO I = 1, LINE_COUNT
               IF(RESULTS(PPOS,I,XSECT).NE.VAL__BADR) THEN
                  COUNT = COUNT + 1
                  CALL EPOLYA(1,K1,COEFF,RESULTS(PPOS,I,XSECT),YPOS)
                  YPLOT(COUNT) = WAVES(I) - YPOS
               ENDIF
            ENDDO
            CALL PGMTEXT('T',0.5,0.5,0.5,'Latest fit (dispersion)')
            CALL PGMTEXT('T',0.5,1.0,1.0,'Hit ? for help')

* 2nd plot, of residuals

            CALL GR_RANGE(YPLOT,1,COUNT,YMIN,YMAX,STATUS)
            YVMAX = 0.45
            CALL PGVPORT(XMAIN1,XMAIN2,YVMIN,YVMAX)
            CALL PGWINDOW(0.0,1.0,0.0,1.0)
            CALL PGPOINT(1,0.88,0.97,16)
            CALL PGTEXT(0.89,0.965,'Used')
            CALL PGPOINT(1,0.88,0.92,0)
            CALL PGTEXT(0.89,0.915,'Not used')
            XMIN = XPLOT(1)
            XMAX = XPLOT(WAVDIM)
            CALL PGWINDOW(XMIN,XMAX,YMIN,YMAX)
            CALL PGBOX('BCNST',0.0,0,'BCNST',0.0,0)
            CALL PGMTEXT('T',0.5,0.5,0.5,'Residuals/line editing')
            COUNT = 0
            DO I = 1, LINE_COUNT
               IF(RESULTS(PPOS,I,XSECT).NE.VAL__BADR) THEN
                  COUNT = COUNT + 1
                  IF(((ARC(1,I).EQ.ARC_OK).OR.(POLYDATA.AND.ARC(1,I).EQ
     :                 .1))) THEN
                     MARKER = 16
                  ELSE
                     MARKER = 0
                  ENDIF
                  CALL PGPOINT(1,RESULTS(PPOS,I,XSECT),YPLOT(COUNT)
     :                 ,MARKER)

*         Put error bars on so user can see if residuals significant

                  SIGMA = SQRT(RESVAR(PPOS,I,XSECT))
                  XERR(1) = RESULTS(PPOS,I,XSECT) - SIGMA
                  XERR(2) = XERR(1) + SIGMA + SIGMA
                  CALL EPOLYA(2,K1,COEFF,XERR,YERR)
                  YERR(1) = WAVES(I) - YERR(1)
                  YERR(2) = WAVES(I) - YERR(2)
                  CALL PGERRY(1,RESULTS(PPOS,I,XSECT),YERR(1),YERR(2),1
     :                 .0)
               ENDIF
            ENDDO
            CALL PGSLS(2)
            CALL PGMOVE(XPLOT(1),0.0)
            CALL PGDRAW(XPLOT(WAVDIM),0.0)
            CALL PGSLS(1)
         ELSE

* There was a problem with the fitting, so we have to provide user
* interaction without the dispersion plots, etc. Also can be selected if
* required.

* Write list of lines to plotting device

            YVMAX = 0.95
            CALL PGVPORT(XMAIN1,XMAIN2,YVMIN,YVMAX)
            XMIN = 0.0
            XMAX = 1.0
            YMIN = REAL(SCREEN * 15) + 0.5
            YMAX = YMIN - 17.0
            CALL PGWINDOW(XMIN,XMAX,YMIN,YMAX)
            CALL PGBOX('BC',0.0,0,'BC',0.0,0)
            CALL PGMTEXT('T',0.5,1.0,1.0,'Hit ? for help')
            YD = YMAX * 0.95 + YMIN * 0.05
            CALL PGTEXT(0.45,YD,'Centre')
            CALL PGTEXT(0.6,YD,'Error')
            CALL PGTEXT(0.8,YD,'Used')
            SLINE = (SCREEN - 1) * 15 + 1
            ELINE = MIN(SLINE + 14,LINE_COUNT)
            LEN1 = 0
            CALL CHR_PUTC('Lines ',CHARS,LEN1)
            CALL CHR_PUTI(SLINE,CHARS,LEN1)
            CALL CHR_PUTC(' to ',CHARS,LEN1)
            CALL CHR_PUTI(ELINE,CHARS,LEN1)
            CALL CHR_PUTC(' (total ',CHARS,LEN1)
            CALL CHR_PUTI(LINE_COUNT,CHARS,LEN1)
            CALL CHR_PUTC(')',CHARS,LEN1)
            CALL PGMTEXT('T',0.5,0.0,0.0,CHARS(:LEN1))
            DO LINE = SLINE, ELINE
               LEN1 = 0
               CALL CHR_FILL(' ',CHARS)
               STATUS = TNW_CPUTR('(f14.2)',WAVES(LINE),CHARS,LEN1)
               CALL CHR_LDBLK(CHARS)
               LEN1 = CHR_LEN(CHARS)
               Y = REAL(LINE)
               CALL PGTEXT(0.05,Y,CHARS(:LEN1))
               LEN1 = 0
               CALL CHR_APPND(LINNAM(LINE),CHARS,LEN1)
               CALL PGTEXT(0.22,Y,CHARS(:LEN1))
               LEN1 = 0
               VALUE = RESULTS(PPOS,LINE,XSECT)
               IF(VALUE.EQ.VAL__BADR) THEN
                  CHARS = 'BAD'
                  LEN1 = 3
               ELSE
                  CALL CHR_PUTR(VALUE,CHARS,LEN1)
               ENDIF
               CALL PGTEXT(0.43,Y,CHARS(:LEN1))
               LEN1 = 0
               VALUE = RESVAR(PPOS,LINE,XSECT)
               IF(VALUE.EQ.VAL__BADR) THEN
                  CHARS = 'BAD'
                  LEN1 = 3
               ELSE
                  CALL CHR_PUTR(SQRT(ABS(VALUE)),CHARS,LEN1)
               ENDIF
               CALL PGTEXT(0.58,Y,CHARS(:LEN1))
               IF(ARC(1,LINE).LT.ARC_NO_FITS) THEN
                  CALL PGSFS(1)
               ELSE IF(ARC(1,LINE).GT.ARC_NO_FITS) THEN
                  CALL PGSFS(2)
                  CALL PGPOINT(1,0.835,Y-0.35,5)
               ENDIF
               CALL PGRECT(0.82,0.85,Y,Y-0.7)

*    Indicate if the current data is unuseable for continuity-corrected
* data, but could be used for original data

* CHANGE I to LINE
               IF(POLYDATA.AND.ARC(1,LINE).EQ.ARC_ORIG)THEN
                  CALL PGSCI(0)
                  CALL PGRECT(0.83,0.84,Y-0.2,Y-0.5)
                  CALL PGSCI(1)
               ENDIF
               NWLIST = NWLIST + 1
               WLIST(1,NWLIST) = 0.82
               WLIST(2,NWLIST) = 0.85
               WLIST(3,NWLIST) = Y-0.7
               WLIST(4,NWLIST) = Y
            ENDDO
         ENDIF
         CALL PGEBUF
         INLOOP = .TRUE.
         DO WHILE(INLOOP)
            IF(STATUS.EQ.SAI__OK) STATUS = PGCURSE(XCUR,YCUR,CUR) - 1
            X = XCUR
            Y = YCUR
            CUR = CHR_UPPER(CUR)
            YTEST0 = (Y - YMIN)/ (YMAX - YMIN)
            YTEST1 = YTEST0 * (YVMAX - YVMIN) + 0.04
            YTEST = YTEST1 / (ORBAY2 - ORBAY1)
            XTEST0 = (X - XMIN) * (XMAIN2 - XMAIN1)/ (XMAX - XMIN) +
     :           XMAIN1
            YSPTEST = YTEST0 * (YVMAX - YVMIN) + YVMIN
            ID = 0
            ID2 = 0
            CALL TEST_AREAS(NLIST,6,LIST,XTEST0,YSPTEST,ID)
            IF(ID.EQ.0) THEN
               CALL TEST_AREAS(NWLIST,4,WLIST,X,Y,ID2)
            ENDIF
            IF(ID.EQ.1) THEN

*  Change current order

               OLDVAL = ORDER
               Y = YTEST0 * (YVMAX - YVMIN)/(ORBY2 - ORBY1)
               YTEST = Y*(ORDMAX+0.5)-0.5
               IF(YTEST.LT.-0.5) THEN
                  ORDER = ORDER - 1
               ELSE IF(YTEST.GT.ORDMAX) THEN
                  ORDER = ORDER + 1
               ELSE
                  ORDER = NINT(YTEST)
               ENDIF
               ORDER = MAX(0,ORDER)
               ORDER = MIN(ORDER,IORDMAX)
               CALL SLIDER('ORDER','M',LIST(1,1),ORDER,OLDVAL)
            ELSE IF((ID.EQ.2).AND.(SPDIM1.GT.1)) THEN

*  Change current cross-section

               OLDVAL = NXSECT
               Y = YTEST0 * (YVMAX - YVMIN)/(XSBY2 - XSBY1)
               YTEST = Y*REAL(SPDIM1) + 0.5
               IF(YTEST.LT.0.5) THEN
                  NXSECT = NXSECT - 1
               ELSE IF(YTEST.GT.(REAL(SPDIM1)+0.5)) THEN
                  NXSECT = NXSECT + 1
               ELSE
                  NXSECT = NINT(YTEST)
               ENDIF
               NXSECT = MIN(SPDIM1,MAX(1,NXSECT))
               CALL SLIDER('XSECT','M',LIST(1,2),NXSECT,OLDVAL)
            ELSE IF(ID2.NE.0) THEN

*     Purge line from list
*     or Return line to list

               LINE = NINT(Y)
               LINE = MAX(1,MIN(LINE_COUNT,LINE))
               IF(ARC(1,LINE).LT.ARC_NO_FITS) THEN
                  ARC(1,LINE) = ARC(1,LINE) + 10
                  Y = REAL(LINE)
                  CALL PGSFS(1)
                  CALL PGSCI(0)
                  CALL PGRECT(0.82,0.85,Y,Y-0.7)
                  CALL PGSCI(1)
                  CALL PGSFS(2)
                  CALL PGRECT(0.82,0.85,Y,Y-0.7)
                  CALL PGPOINT(1,0.835,Y-0.35,5)
               ELSE IF(ARC(1,LINE).GT.ARC_NO_FITS) THEN
                  ARC(1,LINE) = ARC(1,LINE) - 10
                  Y = REAL(LINE)
                  CALL PGSFS(1)
                  CALL PGRECT(0.82,0.85,Y,Y-0.7)
                  IF(POLYDATA.AND.ARC(1,I).EQ.ARC_ORIG)THEN
                     CALL PGSCI(0)
                     CALL PGRECT(0.83,0.84,Y-0.2,Y-0.5)
                     CALL PGSCI(1)
                  ENDIF
               ENDIF
            ELSE IF(CUR.EQ.'Q') THEN

*   Quit

               IOPT = 2
               LOOP = .FALSE.
               INLOOP = .FALSE.
            ELSE IF(CUR.EQ.'F') THEN

*   Perform (another) fit

               LOOP = .FALSE.
               IOPT = 3
               INLOOP = .FALSE.
            ELSE IF(CUR.EQ.'?') THEN

*    Help

               CALL PGENV(0.0,1.0,0.0,1.0,0,-1)
               IF(TABLE) THEN
                  CALL PGTEXT(0.1,0.85
     :                 ,'Click on box to purge/return line')
               ELSE
                  CALL PGTEXT(0.1,0.9,'P - Purge line from list')
                  CALL PGTEXT(0.1,0.85,'R - Return line to list')
               ENDIF
               CALL PGTEXT(0.1,0.8,'Q - Quit')
               CALL PGTEXT(0.1,0.75,'F - Try another fit')
               CALL PGTEXT(0.1,0.7
     :              ,'Click on slider to set order')
               CALL PGTEXT(0.1,0.65,'? - Help')
               CALL PGTEXT(0.1,0.6,
     :              'C - Plot of arc with lines labeled')
               CALL PGTEXT(0.1,0.55
     :              ,'S - Toggle using continuity-corrected data')
               CALL PGTEXT(0.1,0.5
     :              ,'H - Toggle tables (to alpha terminal/screen)')
               IF(.NOT.TABLE) THEN
                  CALL PGTEXT(0.1,0.45,
     :                 'T - Show table (can edit lines)')
                  CALL PGTEXT(0.1,0.4,'A - Accept fits')
                  IF(.NOT.POLYDATA) THEN
                     CALL PGTEXT(0.1,0.35,'M - More plots')
                     YD = 0.3
                  ELSE
                     YD = 0.35
                  ENDIF
               ELSE
                  CALL PGTEXT(0.1,0.45,'E - Edit line wavelength/name')
                  YD = 0.4
                  IF(NSCRN.GT.1) THEN
                     CALL PGTEXT(0.1,YD,'D - Go down line list')
                     CALL PGTEXT(0.1,0.35,'U - Go up line list')
                     YD = 0.3
                  ENDIF
                  IF(ERROR.EQ.SAI__OK) THEN
                     CALL PGTEXT(0.1,YD,
     :                    'G - Goto dispersion plots')
                     YD = YD - 0.05
                  ENDIF
               ENDIF
               IF(.NOT.POLYDATA) THEN
                  CALL PGTEXT(0.1,YD
     :                 ,'N - Toggle use of fits with Nag errors')
                  YD = YD - 0.05
               ENDIF
               IF(SPDIM1.GT.1) THEN
                  CALL PGTEXT(0.1,YD
     :                 ,'Click on slider to set cross-section')
               ENDIF
               CALL PGTEXT(0.1,0.1,
     :              'Hit any key to return to arc display')
               IF(STATUS.EQ.SAI__OK) STATUS = PGCURSE(XCUR,YCUR,CUR) - 1
               INLOOP = .FALSE.
            ELSE IF(CUR.EQ.'W') THEN

*     Toggle use of weights

               WEIGHT=.NOT.WEIGHT
               CALL PGVPORT(WEIX1,WEIX2,WEIY1,WEIY2)
               CALL PGWINDOW(0.0,1.0,0.0,1.0)
               CALL PGSCI(0)
               CALL PGSFS(1)
               CALL PGRECT(0.0,1.0,0.0,1.0)
               CALL PGSCI(1)
               CALL PGTEXT(0.01,0.6,'Weights')
               IF(WEIGHT) THEN
                  CALL PGTEXT(0.01,0.2,'ON')
               ELSE
                  CALL PGTEXT(0.01,0.2,'OFF')
               ENDIF
            ELSE IF(CUR.EQ.'S') THEN
               POLYDATA = .NOT.POLYDATA
               CALL PGVPORT(DATX1,DATX2,DATY1,DATY2)
               CALL PGWINDOW(0.0,1.0,0.0,1.0)
               CALL PGSCI(0)
               CALL PGSFS(1)
               CALL PGRECT(0.0,1.0,0.0,1.0)
               CALL PGSCI(1)
               CALL PGTEXT(0.01,0.6,'Data')
               IF(POLYDATA) THEN
                  CALL PGTEXT(0.01,0.2,'Polyfit')
                  PPOS = PPOSC
               ELSE
                  CALL PGTEXT(0.01,0.2,'Original')
                  PPOS = PPOSD
               ENDIF
               CALL PGVPORT(NAGX1,NAGX2,NAGY1,NAGY2)
               CALL PGWINDOW(0.0,1.0,0.0,1.0)
               IF(.NOT.POLYDATA) THEN
                  CALL PGTEXT(0.01,0.6,'Nag fail')
                  IF(POLYTAB) THEN
                     CALL PGTEXT(0.01,0.2,'USED')
                  ELSE
                     CALL PGTEXT(0.01,0.2,'NOT USED')
                  ENDIF
               ELSE
                  CALL PGSCI(0)
                  CALL PGRECT(0.0,1.0,0.0,1.0)
                  CALL PGSCI(1)
               ENDIF

            ELSE IF(CUR.EQ.'H') THEN

*  Whether we give tables

               POLYTAB = .NOT.POLYTAB
               CALL PGVPORT(TABX1,TABX2,TABY1,TABY2)
               CALL PGWINDOW(0.0,1.0,0.0,1.0)
               CALL PGSCI(0)
               CALL PGSFS(1)
               CALL PGRECT(0.0,1.0,0.0,1.0)
               CALL PGSCI(1)
               CALL PGTEXT(0.01,0.6,'Tables')
               IF(POLYTAB) THEN
                  CALL PGTEXT(0.01,0.2,'ON')
               ELSE
                  CALL PGTEXT(0.01,0.2,'OFF')
               ENDIF

            ELSE IF((CUR.EQ.'N').AND.(.NOT.POLYDATA)) THEN

               USENAGERR = .NOT.USENAGERR
               CALL PGVPORT(NAGX1,NAGX2,NAGY1,NAGY2)
               CALL PGWINDOW(0.0,1.0,0.0,1.0)
               CALL PGSCI(0)
               CALL PGSFS(1)
               CALL PGRECT(0.0,1.0,0.0,1.0)
               CALL PGSCI(1)
               CALL PGTEXT(0.01,0.6,'Nag fail')
               IF(USENAGERR) THEN
                  CALL PGTEXT(0.01,0.2,'USED')
               ELSE
                  CALL PGTEXT(0.01,0.2,'NOT USED')
               ENDIF
            ELSE IF(CUR.EQ.'C') THEN
               CALL PLOT_ARC(YPLOT,ARC,WAVES,RESULTS,STATUS)
               INLOOP = .FALSE.
            ELSE IF(CUR.EQ.'C') THEN
               POLYTAB = .NOT.POLYTAB
            ELSE
               IF(.NOT.TABLE) THEN
                  IF(CUR.EQ.'P') THEN

*  Purge line from list

                     TDIST = VAL__MAXR
                     DO I = 1, LINE_COUNT
                        DIST = ABS(RESULTS(PPOS,I,XSECT) - X)
                        IF(DIST.LT.TDIST) THEN
                           LINE = I
                           TDIST = DIST
                        ENDIF
                     ENDDO
                     IF(ARC(1,LINE).LT.ARC_NO_FITS) ARC(1,LINE) = ARC(1
     :                    ,LINE) + 10
                     CALL PGSCI(0)
                     CALL PGPOINT(1,RESULTS(PPOS,LINE,XSECT),YPLOT(LINE)
     :                    ,16)
                     CALL PGSCI(1)
                     CALL PGPOINT(1,RESULTS(PPOS,LINE,XSECT),YPLOT(LINE)
     :                    ,0)
                  ELSE IF(CUR.EQ.'R') THEN

*    Return line to list

                     TDIST = VAL__MAXR
                     DO I = 1, LINE_COUNT
                        DIST = ABS(RESULTS(PPOS,I,XSECT) - X)
                        IF(DIST.LT.TDIST) THEN
                           LINE = I
                           TDIST = DIST
                        ENDIF
                     ENDDO
                     IF(ARC(1,LINE).GT.ARC_NO_FITS) ARC(1,LINE) = ARC(1
     :                    ,LINE) - 10
                     CALL PGSCI(0)
                     CALL PGPOINT(1,RESULTS(PPOS,LINE,XSECT),YPLOT(LINE)
     :                    ,0)
                     CALL PGSCI(1)
                     CALL PGPOINT(1,RESULTS(PPOS,LINE,XSECT),YPLOT(LINE)
     :                    ,16)
                  ELSE IF(CUR.EQ.'A') THEN

*    Accept current settings-produce output file

                     LOOP = .FALSE.
                     IOPT = 1
                     INLOOP = .FALSE.
                  ELSE IF(CUR.EQ.'T') THEN


*    Display tables-use for editing line wavelengths/names

                     TABLE = .TRUE.
                     INLOOP = .FALSE.
                  ELSE IF((CUR.EQ.'M').AND.(.NOT.POLYDATA)) THEN

*   More plots

                     LOOP = .FALSE.
                     IOPT = 4
                     INLOOP = .FALSE.
                  ELSE
                     CALL MSG_OUT(' ','Invalid key',STATUS)
                  ENDIF
               ELSE

*   We've got a table displayed

                  IF((ERROR.EQ.SAI__OK).AND.(CUR.EQ.'G')) THEN

*     If we do have a successful fit, but just wanted to display table,
*     then return to plots of fit

                     TABLE = .FALSE.
                     INLOOP = .FALSE.
                  ELSE IF(CUR.EQ.'E') THEN

*      Edit line names/wavelengths
*      This loop continues until the user hits a position outside the
*      current line

                     LINE = NINT(Y)
                     LINE = MAX(1,MIN(LINE_COUNT,LINE))
                     YD = REAL(LINE)
                     CALL PGSFS(2)
                     Y1 = YD - 0.4
                     Y2 = YD + 0.4
                     CALL PGRECT(0.03,0.2,Y1,Y2)
                     CALL PGRECT(0.2,0.42,Y1,Y2)
                     CALL PGMTEXT('B',1.0,0.5,0.5,
     :                    'Hit on another line when done')
                     NSTRING = LINNAM(LINE)
                     NLEN = CHR_LEN(NSTRING)
                     WLEN = 0
                     STATUS = TNW_CPUTR('(f14.2)',WAVES(LINE),WSTRING
     :                    ,WLEN)
                     CALL CHR_LDBLK(WSTRING)
                     WLEN = CHR_LEN(WSTRING)
                     EDIT = .TRUE.
                     DO WHILE(EDIT)
                        IF(STATUS.NE.SAI__OK) THEN
                           Y = Y1 - 1.0
                        ELSE
                           STATUS = PGCURSE(XCUR,YCUR,CUR) - 1
                           X = XCUR
                           Y = YCUR
                        ENDIF
                        IF((Y.LT.Y1).OR.(Y.GT.Y2)) THEN
                           EDIT = .FALSE.
                        ELSE IF(X.LT.0.2) THEN

*         Line wavelength

                           IF(WLEN.GT.0) THEN
                              CALL PGSCI(0)
                              CALL PGTEXT(0.05,YD,WSTRING(:WLEN))
                              IF(ICHAR(CUR).EQ.DELETE) WLEN = WLEN - 1
                           ENDIF
                           IF(ICHAR(CUR).NE.DELETE) THEN
                              WLEN = WLEN + 1
                              WSTRING(WLEN:WLEN) = CUR
                           ENDIF
                           CALL PGSCI(1)
                           IF(WLEN.GT.0) THEN
                              CALL PGTEXT(0.05,YD,WSTRING(:WLEN))
                           ENDIF
                        ELSE IF(X.LT.0.42) THEN

*          Line name

                           IF(NLEN.GT.0) THEN
                              CALL PGSCI(0)
                              CALL PGTEXT(0.22,YD,NSTRING(:NLEN))
                              IF(ICHAR(CUR).EQ.DELETE) NLEN = NLEN - 1
                           ENDIF
                           IF(ICHAR(CUR).NE.DELETE) THEN
                              NLEN = NLEN + 1
                              NSTRING(NLEN:NLEN) = CUR
                           ENDIF
                           CALL PGSCI(1)
                           IF(NLEN.GT.0) THEN
                              CALL PGTEXT(0.22,YD,NSTRING(:NLEN))
                           ENDIF
                        ENDIF
                     ENDDO
                     CALL PGSCI(0)
                     CALL PGRECT(0.03,0.2,Y1,Y2)
                     CALL PGRECT(0.2,0.42,Y1,Y2)
                     CALL PGMTEXT('B',1.0,0.5,0.5,
     :                    'Hit on another line when done')
                     CALL PGTEXT(0.05,YD,WSTRING(:WLEN))
                     CALL PGSCI(1)
                     LINNAM(LINE) = NSTRING(:NLEN)
                     CALL CHR_CTOR(WSTRING(:WLEN),VALUE,STATUS)
                     IF(STATUS.NE.SAI__OK) THEN
                        STATUS = SAI__OK
                     ELSE
                        WAVES(LINE) = VALUE
                     ENDIF
                     WLEN = 0
                     CALL CHR_PUTR(WAVES(LINE),WSTRING,WLEN)
                     CALL PGTEXT(0.05,YD,WSTRING(:WLEN))
                  ELSE IF((CUR.EQ.'U').AND.(NSCRN.GT.1)) THEN
                     SCREEN = SCREEN + 1
                     IF(SCREEN.GT.NSCRN) SCREEN = 1
                     INLOOP = .FALSE.
                  ELSE IF((CUR.EQ.'D').AND.(NSCRN.GT.1)) THEN
                     SCREEN = SCREEN - 1
                     IF(SCREEN.EQ.0) SCREEN = NSCRN
                     INLOOP = .FALSE.
                  ELSE
                     CALL MSG_OUT(' ','Invalid key',STATUS)
                  ENDIF
               ENDIF
            ENDIF
            IF(INLOOP) THEN
               CALL PGVPORT(XMAIN1,XMAIN2,YVMIN,YVMAX)
               CALL PGWINDOW(XMIN,XMAX,YMIN,YMAX)
            ENDIF
            IF(STATUS.NE.SAI__OK) RETURN
         ENDDO
      ENDDO
      XSECT = NXSECT
      END
