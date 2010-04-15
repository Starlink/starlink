(new-routine "PGADVANCE"
             "Advance to new page"
             ( ))

(new-routine "PGASK"
             "Control new page prompting"
             '("FLAG"))

(new-routine "PGBBUF"
             "Begin batch of output (buffer)"
             ( ))

(new-routine "PGBEGIN"
             "Begin PGPLOT, open output device"
             '("UNIT" "FILE" "NXSUB" "NYSUB"))

(new-routine "PGBIN"
             "Histogram of binned data"
             '("NBIN" "X" "DATA" "CENTER"))

(new-routine "PGBOX"
             "Draw labeled frame around viewport"
             '("XOPT" "XTICK" "NXSUB" "YOPT" "YTICK" "NYSUB"))

(new-routine "PGCONB"
             "Contour map of a 2D data array, with blanking"
             '("A" "IDIM" "JDIM" "I1" "I2" "J1" "J2" "C" "NC" "TR" "BLANK"))

(new-routine "PGCONS"
             "Contour map of a 2D data array (fast algorithm)"
             '("A" "IDIM" "JDIM" "I1" "I2" "J1" "J2" "C" "NC" "TR"))

(new-routine "PGCONT"
             "Contour map of a 2D data array (contour-following)"
             '("A" "IDIM" "JDIM" "I1" "I2" "J1" "J2" "C" "NC" "TR"))

(new-routine "PGCONX"
             "Contour map of a 2D data array (non-rectangular)"
             '("A" "IDIM" "JDIM" "I1" "I2" "J1" "J2" "C" "NC" "PLOT"))

(new-routine "PGCURSE"
             "Read cursor position"
             '("X" "Y" "CH"))

(new-routine "PGDRAW"
             "Draw a line from the current pen position to a point"
             '("X" "Y"))

(new-routine "PGEBUF"
             "End batch of output (buffer)"
             ( ))

(new-routine "PGEND"
             "Terminate PGPLOT"
             ( ))

(new-routine "PGENV"
             "Set window and viewport and draw labeled frame"
             '("XMIN" "XMAX" "YMIN" "YMAX" "JUST" "AXIS"))

(new-routine "PGERRX"
             "Horizontal error bar"
             '("N" "X1" "X2" "Y" "T"))

(new-routine "PGERRY"
             "Vertical error bar"
             '("N" "X" "Y1" "Y2" "T"))

(new-routine "PGETXT"
             "Erase text from graphics display"
             ( ))

(new-routine "PGFUNT"
             "Function defined by X = F(T), Y = G(T)"
             '("FX" "FY" "N" "TMIN" "TMAX" "PGFLAG"))

(new-routine "PGFUNX"
             "Function defined by Y = F(X)"
             '("FY" "N" "XMIN" "XMAX" "PGFLAG"))

(new-routine "PGFUNY"
             "Function defined by X = F(Y)"
             '("FX" "N" "YMIN" "YMAX" "PGFLAG"))

(new-routine "PGGRAY"
             "Gray-scale map of a 2D data array"
             '("A" "IDIM" "JDIM" "I1" "I2" "J1" "J2" "FG" "BG" "TR"))

(new-routine "PGHI2D"
             "Cross-sections through a 2D data array"
             '("DATA" "NXV" "NYV" "IX1" "IX2" "IY1" "IY2" "X" "IOFF" "BIAS" "CENTER" "YLIMS"))

(new-routine "PGHIST"
             "Histogram of unbinned data"
             '("N" "DATA" "DATMIN" "DATMAX" "NBIN" "PGFLAG"))

(new-routine "PGIDEN"
             "Write username, date, and time at bottom of plot"
             ( ))

(new-routine "PGLABEL"
             "Write labels for x-axis, y-axis, and top of plot"
             '("XLBL" "YLBL" "TOPLBL"))

(new-routine "PGLCUR"
             "Draw a line using the cursor"
             '("MAXPT" "NPT" "X" "Y"))

(new-routine "PGLDEV"
             "List available device types"
             ( ))

(new-routine "PGLEN"
             "Find length of a string in a variety of units"
             '("UNITS" "STRING" "XL" "YL"))

(new-routine "PGLINE"
             "Draw a polyline (curve defined by line-segments)"
             '("N" "XPTS" "YPTS"))

(new-routine "PGMOVE"
             "Move pen (change current pen position)"
             '("X" "Y"))

(new-routine "PGMTEXT"
             "Write text at position relative to viewport"
             '("SIDE" "DISP" "COORD" "FJUST" "TEXT"))

(new-routine "PGNCURSE"
             "Mark a set of points using the cursor"
             '("MAXPT" "NPT" "X" "Y" "SYMBOL"))

(new-routine "PGNUMB"
             "Convert a number into a plottable character string"
             '("MM" "PP" "FORM" "STRING" "NC"))

(new-routine "PGOLIN"
             "Mark a set of points using the cursor"
             '("MAXPT" "NPT" "X" "Y" "SYMBOL"))

(new-routine "PGPAGE"
             "Advance to new page"
             ( ))

(new-routine "PGPAPER"
             "Change the size of the view surface"
             '("WIDTH" "ASPECT"))

(new-routine "PGPOINT"
             "Draw one or more graph markers"
             '("N" "XPTS" "YPTS" "SYMBOL"))

(new-routine "PGPOLY"
             "Fill a polygonal area with shading"
             '("N" "XPTS" "YPTS"))

(new-routine "PGPTEXT"
             "Write text at arbitrary position and angle"
             '("X" "Y" "ANGLE" "FJUST" "TEXT"))

(new-routine "PGQCF"
             "Inquire character font"
             '("IF"))

(new-routine "PGQCH"
             "Inquire character height"
             '("SIZE"))

(new-routine "PGQCI"
             "Inquire color index"
             '("CI"))

(new-routine "PGQCOL"
             "Inquire color capability"
             '("CI1" "CI2"))

(new-routine "PGQCR"
             "Inquire color representation"
             ( ))

(new-routine "PGQFS"
             "Inquire fill-area style"
             '("FS"))

(new-routine "PGQINF"
             "Inquire PGPLOT general information"
             '("ITEM" "VALUE" "LENGTH"))

(new-routine "PGQLS"
             "Inquire line style"
             '("LS"))

(new-routine "PGQLW"
             "Inquire line width"
             '("LW"))

(new-routine "PGQVP"
             "Inquire viewport size and position"
             '("UNITS" "X1" "X2" "Y1" "Y2"))

(new-routine "PGQWIN"
             "Inquire window boundary coordinates"
             '("X1" "X2" "Y1" "Y2"))

(new-routine "PGRECT"
             "Draw a rectangle, using fill-area attributes"
             '("X1" "X2" "Y1" "Y2"))

(new-routine "PGRND"
             "Find the smallest ROUND number greater than x"
             '("X" "NSUB"))

(new-routine "PGRNGE"
             "Choose axis limits"
             '("X1" "X2" "XLO" "XHI"))

(new-routine "PGSCF"
             "Set character font"
             '("IF"))

(new-routine "PGSCH"
             "Set character height"
             '("SIZE"))

(new-routine "PGSCI"
             "Set color index"
             '("CI"))

(new-routine "PGSCR"
             "Set color representation"
             '("CI" "CR" "CG" "CB"))

(new-routine "PGSFS"
             "Set fill-area style"
             '("FS"))

(new-routine "PGSHLS"
             "Set color representation using HLS system"
             '("CI" "CH" "CL" "CS"))

(new-routine "PGSLS"
             "Set line style"
             '("LS"))

(new-routine "PGSLW"
             "Set line width"
             '("LW"))

(new-routine "PGTBOX"
             "Draw a box and optionally write HH MM SS style numeric labelling"
             '("XOPT" "XTICKD" "NXSUBD" "YOPT" "YTICKD" "NYSUBD"))

(new-routine "PGTEXT"
             "Write text (horizontal, left-justified)"
             '("X" "Y" "TEXT"))

(new-routine "PGUPDT"
             "Update display"
             ( ))

(new-routine "PGVPORT"
             "Set viewport (normalized device coordinates)"
             '("XLEFT" "XRIGHT" "YBOT" "YTOP"))

(new-routine "PGVSIZE"
             "Set viewport (inches)"
             '("XLEFT" "XRIGHT" "YBOT" "YTOP"))

(new-routine "PGVSTAND"
             "Set standard (default) viewport"
             ( ))

(new-routine "PGWINDOW"
             "Set window"
             '("X1" "X2" "Y1" "Y2"))

(new-routine "PGWNAD"
             "Set window and adjust viewport to same aspect ratio"
             '("X1" "X2" "Y1" "Y2"))
