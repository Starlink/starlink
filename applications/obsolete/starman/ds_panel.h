/* DS_PANEL.H */

extern struct
{
      F77_INTEGER_TYPE pnsnx;		/* Panel X size */
      F77_INTEGER_TYPE pnsny;		/* Panel Y size */
      F77_INTEGER_TYPE pnnum;		/* Panel number of options */
      F77_INTEGER_TYPE pnjcon[200];	/* Panel option order */
      F77_INTEGER_TYPE pnnrow;		/* Panel number of rows */
      F77_INTEGER_TYPE pnncol;		/* Panel number of columns */
      F77_INTEGER_TYPE pdstype;
      F77_INTEGER_TYPE pdnuml;		/* Panel option list code number -
					(which `option list' loaded at present) */
      F77_INTEGER_TYPE pnhposx;		/* Panel X location of help button */
      F77_INTEGER_TYPE pnhposy;		/* Panel Y location of help button */
      F77_INTEGER_TYPE pncposx;		/* Panel X location of command button */
      F77_INTEGER_TYPE pncposy;		/* Panel Y location of command button */
      F77_INTEGER_TYPE pnvposx;		/* Panel X location of Posn, Value box */
      F77_INTEGER_TYPE pnvposy;		/* Panel Y location of Posn, Value box */
      F77_INTEGER_TYPE pnnums[200];	/* Panel number of options (sections) */
      F77_INTEGER_TYPE pnx[200];	/* Panel X locations of buttons */
      F77_INTEGER_TYPE pny[200];	/* Panel Y locations of buttons */
      F77_INTEGER_TYPE pnbhposx;	/* Panel X location of button help button */
      F77_INTEGER_TYPE pnbhposy;	/* Panel Y location of button help button */
} F77_NAMED_COMMON(ds_panel);


extern struct
{
      F77_LOGICAL_TYPE dopanel;		/* Panel use? */
      F77_LOGICAL_TYPE dohpanel;	/* Help Panel use? */
      F77_LOGICAL_TYPE pdsopen;		/* Panel opened? */
      F77_LOGICAL_TYPE pds_dum1;	 /* Dummy to make up 4 */
} F77_NAMED_COMMON(ds_panelb);

