; XRT ICL procedures definitions file
;
;
;
  PROC XRTBOX 
;
        PRINT 'XRTBOX Version 2.2-0'
;
; Initialise variables
        BOX_RA=0.0
        BOX_DEC=0.0
        X1=0.0
        X2=0.0
        RADIUS=0.0
;
; Allows a circular box to be selected on an XRT image and used to
; extract events in the XRTSORT program.
;
        CVAR='C'
;
;  Select the box shape and size
        ICIRCLE 
;
;  Retrieve the current position from the system
        ICURRENT RA=(BOX_RA) DEC=(BOX_DEC) X1=(X1) X2=(X2)
;
        RADIUS = ABS(X2-X1) / 2.0
;
;  Sort the XRT data within this box.
        PRINT 'Radius chosen : ' (RADIUS)
;
        XRTSORT SHAPE=(CVAR) RA=(BOX_RA+0.0) DEC=(BOX_DEC+0.0)~
                 RAD=(RADIUS+0.0) 
;
  END PROC
;
; Sorts XRT data using a radius optimisation algorithm
;
; XRT ICL procedures definitions file
;
  PROC XRTBCKBOX PARAM
;
        PRINT 'XRTBCKBOX Version 2.2-0'
;
;   The new version does not optimise by default
;   To optimise type OPT no the command line, this will then
;   optimise using an energy of 0.5 keV to determine the PSPC
;   PSF. To optimise at a different energy, a number sould be
;   entered on the command line, e.g. 0.2
;
        IF UNDEFINED(PARAM)
           OPT = FALSE
        ELSE 
           TEST = UPCASE(PARAM)
;
           IF TEST = 'OPT'
              OPT = TRUE 
              ENERGY = 0.5
           ELSE
              ENERGY = TEST
              OPT = TRUE
           ENDIF
        ENDIF
;
; Print a warning message if the radius is being optimised
;        IF OPT
;           PRINT ' '
;           PRINT 'WARNING: DO NOT OPTIMISE THE RADIUS IF A '
;           PRINT ' SPECTRUM IS BEING EXTRACTED FROM AN ON-AXIS SOURCE'
;           PRINT ' '
;        ENDIF
;
; Initialise variables
        BOX_RA=0.0
        BOX_DEC=0.0
        X1=0.0
        X2=0.0
        RADIUS=0.0
        SBTOT=0.0
        BFLUX=0.0
        SRCXA=0.0
        SRCYA=0.0
        NEWRAD=0.0
        FRAC=0.0
	SRCX=0.0
	SRCY=0.0
	STOT=0.0
	BCK_RA=0.0
	BCK_DEC=0.0
	BXC=0.0
	BYC=0.0
	IXP=0.0
	IYP=0.0
	BTOT=0.0
	IBTOT=0.0
	OBTOT=0.0
;
; Allows a circular box to be selected on an XRT image and used to
; extract events in the XRTSORT program.
;
        CVAR='C'
;
; Set mode to cursor mode
        IMODE CURS
;
        IF OPT
           PRINT 'Select a large circle around the source'
        ELSE
           PRINT 'Select a circle around the source'
        ENDIF
;
;  Select the box shape and size
        ICIRCLE 
;
;  Retrieve the current position from the system
        ICURRENT RA=(BOX_RA) DEC=(BOX_DEC) X=(SRCX) Y=(SRCY) X1=(X1) X2=(X2) SUPPRESS=YES
;
        SRAD = ABS(X2-X1) / 2.0
;
;  Find the area in arcmins. NB: XRT images are in degrees
        SAREA = 3.14 * SRAD * SRAD * 3600.
;
;  Convert src position in to arcmins
        SRCXA= SRCX * 60.
        SRCYA= SRCY * 60.
;
;  Find the counts in this circle
        ISTATS TOT=(STOT) SUPPRESS=YES
;
;  Select a background region
        PRINT 'Select centre of background region'
;
        IPOSIT
        IMARK CURR \

;
;  Get RA/DEC of bckgnd box centre
        ICURRENT RA=(BCK_RA) DEC=(BCK_DEC) X=(BXC) Y=(BYC) SUPPRESS=YES

        OFFSET = SQRT((BXC-SRCX)*(BXC-SRCX) + (BYC-SRCY)*(BYC-SRCY))




;
;  Is it going to be an annulus around the source box ?
        IF OFFSET < SRAD
;
           PRINT 'Select inner radius of background region'
           IPOSIT
           ICURRENT X=(IXP) Y=(IYP) SUPPRESS=YES
           IRAD = SQRT((IXP-BXC)*(IXP-BXC) + (IYP-BYC)*(IYP-BYC))
;
;     Change mode to keyboard mode
           IMODE KEY
;
           GSET DEF COLOUR=2 \
;
;     Draw inner circle and get stats
           ICIRCLE XCENT=(BXC) YCENT=(BYC) RAD=(IRAD)
           ISTATS TOT=(IBTOT) SUPPRESS=YES
;          
;     Change mode to cursor mode
           IMODE CURS
;
           PRINT 'Select outer radius of background region'
           IPOSIT
           ICURRENT X=(IXP) Y=(IYP) SUPPRESS=YES
           ORAD = SQRT((IXP-BXC)*(IXP-BXC) + (IYP-BYC)*(IYP-BYC))
;
;     Change mode to keyboard mode
           IMODE KEY
;
;     Draw outer circle and get stats
           ICIRCLE XCENT=(BXC) YCENT=(BYC) RAD=(ORAD)
           ISTATS TOT=(OBTOT) SUPPRESS=YES
;
;     Calculate background flux per arcmin
           BAREA = 3.14 * 3600 * (ORAD*ORAD - IRAD*IRAD)
           BTOT = OBTOT - IBTOT
           BFLUX = BTOT / BAREA
;
;  Not an annulus around the source box
        ELSE

;
           IMARK CURR \

;
           GSET DEF COLOUR=2 \
;
           PRINT 'Select radius of background region'
           IPOSIT
           ICURRENT X=(IXP) Y=(IYP) SUPPRESS=YES
           ORAD = SQRT((IXP-BXC)*(IXP-BXC) + (IYP-BYC)*(IYP-BYC))
;
;     Change mode to keyboard mode
           IMODE KEY
;
           ICIRCLE XCENT=(BXC) YCENT=(BYC) RAD=(ORAD)
           ISTATS TOT=(BTOT) SUPPRESS=YES
;
           IRAD = 0.0
;
;    Find the area in arcmins. NB: XRT images are in degrees
           BAREA = 3.14 * ORAD * ORAD * 3600.
;
;    Calculate the bckgnd flux
           BFLUX = BTOT / BAREA
;
        ENDIF
;
;  Background subtract source
        SBTOT = STOT - BFLUX * SAREA
;
;  Show counts in both boxes
        PRINT 'Source area '(SAREA) 'bckgnd area '(BAREA)
        PRINT 'Source counts '(STOT) 'bckgnd counts '(BTOT)
        PRINT 'Subtracted counts '(SBTOT) 
;
;  Radopt wont work if subtracted counts are negative.
        IF  SBTOT < 0.0
           SIGNAL SRCNEG
        ENDIF
;  
;  Calculate the optimum radius
        IF OPT
;
          RADOPT INSTR='XRT' SCOUNT=(SBTOT) BCOUNT=(BFLUX) AZIMUTH=(SRCXA+0.0)~
 ELEV=(SRCYA+0.0) RADIUS=(NEWRAD) MAXFRAC=(FRAC) DISP=NO ENERGY=(ENERGY+0.0)
;
          PRINT ' '
          PRINT 'Optimum radius : ' (NEWRAD:5:2) 'arcmin'
          PRINT '  - contains ' (FRAC*100:5:1)'% of the source counts'
          PRINT ' '
;
;     Draw circle on the image
          GSET DEF COLOUR=3 STYLE=2 \
          IMODE KEY
          ICIRCLE (SRCX) (SRCY) RAD=(NEWRAD/60.0)
;
;     Reset linestyle to solid.
          GSET DEF COLOUR=1 STYLE=1 \
;
        ELSE
          NEWRAD = SRAD * 60.
          PRINT 'Using selected radius : ' (NEWRAD:5:2) 'arcmin'
        ENDIF
;
;  Sort the XRT data within this box.
        RADIUS = NEWRAD / 60.
;
;  Sort data
        XRTSORT SHAPE='C' RA=(BOX_RA+0.0) DEC=(BOX_DEC+0.0) BACK=Y~
   RAD=(RADIUS+0.0) RAB=(BCK_RA+0.0) DECB=(BCK_DEC+0.0)~
   BIRAD=(IRAD+0.0) BORAD=(ORAD+0.0)
;
;
   EXCEPTION SRCNEG
     { Handles the case of negative subtracted source counts }
       PRINT ' ** Source counts are negative !! '
       PRINT '    cannot calculate optimum radius - exiting **'
   END EXCEPTION

  END PROC


PROC WFCOPT
;
; Initialise variables
        BOX_RA=0.0
        BOX_DEC=0.0
        X1=0.0
        X2=0.0
        RADIUS=0.0
        SBTOT=0.0
        BFLUX=0.0
        SRCXA=0.0
        SRCYA=0.0
        NEWRAD=0.0
        FRAC=0.0
	SRCX=0.0
	SRCY=0.0
	SRAD=0.0
	BCK_RA=0.0
	BCK_DEC=0.0
	BRAD=0.0
	BTOT=0.0
	BAREA=0.0
	STOT=0.0
	SAREA=0.0
	FILE=0.0
	DMJD=0.0
	STRING=0.0
	STRING2=0.0
	IFILT=0.0
;
; Allows a circular box to be selected on an XRT image and used to
; extract events in the XRTSORT program.
;
        CVAR='C'
;
; Set mode to cursor mode
        IMODE CURS
;
        PRINT 'Select a large circle around the source'
;
;  Select the box shape and size
        ICIRCLE 
;
;  Retrieve the current position from the system
        ICURRENT RA=(BOX_RA) DEC=(BOX_DEC) X=(SRCX) Y=(SRCY) X1=(X1) X2=(X2)~
 SUPPRESS=YES
;
        SRAD = ABS(X2-X1) / 2.0
;
;  Find the area in arcmins. NB: WFC images are in arcmins 
        SAREA = 3.14 * SRAD * SRAD
;
;  Find the counts in this circle
        ISTATS TOT=(STOT)
;
;  Select a background region
        PRINT 'Select a background region'
;
        ICIRCLE 
;
;  Retrieve the current position from the system
        ICURRENT RA=(BCK_RA) DEC=(BCK_DEC) X1=(X1) X2=(X2) SUPPRESS=YES
;
        BRAD = ABS(X2-X1) / 2.0
;
;  Find the area in arcmins. NB: WFC images are in degrees
        BAREA = 3.14 * BRAD * BRAD 
;
        ISTATS TOT=(BTOT)
;
;  Calculate background flux per square arcmin
        IF (SQRT((BCK_RA-BOX_RA)**2 + (BCK_DEC-BOX_DEC)**2)) > SRAD
;
           BFLUX = BTOT / BAREA
;
        ELSE
;
           BFLUX = (BTOT - STOT) / (BAREA - SAREA)
;
        ENDIF
;
;  Background subtract source
        SBTOT = STOT - BFLUX * SAREA
;
;  DEBUG
        PRINT 'Source area '(SAREA) 'bckgnd area '(BAREA)
        PRINT 'Source counts '(STOT) 'bckgnd counts '(BTOT)
        PRINT 'Subtracted counts '(SBTOT) 'bckgnd flux '(BFLUX)
;
;  Find the MJD of this file and the filter number
        GETPAR ILOAD INP (FILE)
;
        STRING='@' & (FILE) & '.MORE.ASTERIX.HEADER.BASE_MJD' 
        HGET (STRING) VALUE ATTR=(DMJD)
;
        STRING2='@' & (FILE) & '.MORE.ASTERIX.INSTRUMENT.SORT.FILTER'
        HGET (STRING2) VALUE ATTR=(IFILT)
  =IFILT
  =DMJD
  =SBTOT
  =BFLUX
  =SRCX
  =SRCY
;
        RADOPT INSTR='WFC' SCOUNT=(SBTOT) BCOUNT=(BFLUX) AZIMUTH=(SRCX)~
 ELEV=(SRCY) DMJD=(DMJD) FILTER=(IFILT) RADIUS=(NEWRAD) MAXFRAC=(FRAC)
;
        RADIUS = NEWRAD 
;
;  Draw circle on the image
        GSET DEF STYLE=2 \
        IMODE KEY
        ICIRCLE (SRCX) (SRCY) RAD=(RADIUS)
        GSET DEF STYLE=1 \
;
        PRINT  (BOX_RA) (BOX_DEC) (RADIUS)
;
        PRINT '** Now run WFCSORT with these parameters **'
;
  END PROC

PROC XPSF90
;
; Procedure to display 90% radii circles on an XRT image
;
; initialise variables
     SRCX=0.0
     SRCY=0.0
     OFFAX=0.0
     RAD=0.0
; Get the off-axis position (arcmins)
     IPOSIT
;
     ICURRENT X=(SRCX) Y=(SRCY) SUPPRESS=YES
;
     OFFAX = SQRT( SRCX**2 + SRCY**2 ) * 60.
;
; Get the 90% radius
     XRAD90 OFFAX=(OFFAX) PSFRAD=(RAD)
;
; Draw the circle on the image
     IMODE KEY
     ICIRCLE (SRCX) (SRCY) RAD=(RAD)
;
END PROC

PROC XPSF
;
; Procedure to display the PSF radius on an XRT image
;
; initialise variables
     SRCX=0.0
     SRCY=0.0
     MODE=0.0
     OFFAX=0.0
     RAD=0.0
; Get the off-axis position (arcmins)
     IPOSIT
;
     ICURRENT X=(SRCX) Y=(SRCY) SUPPRESS=YES MODE=(MODE)
;
     OFFAX = SQRT( SRCX**2 + SRCY**2 ) * 60.
;
; Get the radius
     XRAD OFFAX=(OFFAX) PSFRAD=(RAD)
;
; Draw the circle on the image
     IMODE KEY
     ICIRCLE (SRCX) (SRCY) RAD=(RAD)
;
; Reset the mode
     IF MODE = 'CURS'
        IMODE CURS
     ENDIF
;
END PROC
