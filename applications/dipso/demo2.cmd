
!  This CMD file illustrates the use of FRAME to design
!  plots in `real' co-ordinates (i.e. cm), and the use
!  of FRZONE to specfify subzones within the frame

!  Set plotting options according to taste
!  No `rotation' of line styles, colours...

NROT

!  Continuous lines

TLINE 1

!  Plot `histogram' style

HIST

!  Set colour to white (for appropriate devices)

CSET 1

!  Fancy labels

FONT 2

!  Obtain and plot data

READ DIPSODIR:DEMO2

!   Set X and Y ranges;  `trim' Y axis

XR 1530 1570
YR -0.5 4.5,YT

!   Make sure that the screen is cleared before plotting

BOX

!   Design the size of the plotting area, and tick-mark spacing

FRAME 16 8 2
TICKS 10 2 3 3

!   No labelling

NUMOFF,NLAB


!   Select a subzone within the plotting frame, and plot

FRZONE 0.0 0.333 0.0 0.5,PM

!   Make sure the screen ISN'T cleared between plots,
!   select subzones, and plot.   Reverse and invert
!   some plots to make it more interesting.

NB
FRZONE 0.0 0.333 0.5 1.0,PM
PLOTREV
FRZONE 0.333 0.667 0.0 0.5,PM
FRZONE 0.333 0.667 0.5 1.0,PM
PLOTINV
FRZONE 0.667 1.000 0.0 0.5,PM
FRZONE 0.667 1.000 0.5 1.0,PM

!   Make sure the screen is cleared before the next plot

BOX

!   Undo previous reversal and inversion of plots

PLOTREV,PLOTINV

!   Turn labelling back on

NUMON,LABON

!   Select new X range, and adjust tickmark spacing accordingly

XR 1545 1555
TICKS 2 1 4 4

!   Change axis labels

YLAB "Residual Intensity"
XLAB "Wavelength ('PRU''.A')"


!   Choose a different plotting area, 14x14 cm, top left

FRAME 14 14 7
PM

!   Turn off all labelling, do an inset plot (new X range)

NLAB
NUMOFF
NB
XR 1530 1570
TICKS 10 5 1 1
FRZONE 0.65 0.95 0.15 0.4,PM

!   Return to reasonable defaults

TZONE 0
FONT 0
BOX,NUMON,LABON,TICKS,YJ,NXY
YLAB Flux
XLAB Wavelength
