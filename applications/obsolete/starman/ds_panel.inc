!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! DS_PANEL.INC
!

      integer	pnsnx			! Panel X size
      integer	pnsny			! Panel Y size
      integer	pnnum			! Panel number of options
      integer	pnjcon(200)		! Panel option order
      integer	pnnrow			! Panel number of rows
      integer	pnncol			! Panel number of columns
      integer	pdstype			! Panel display type
      integer	pdnuml			! Panel option list code number -
					!  (which `option list' loaded at present)
      integer	pnhposx			! Panel X location of help button
      integer	pnhposy			! Panel Y location of help button
      integer	pncposx			! Panel X location of command button
      integer	pncposy			! Panel Y location of command button
      integer	pnvposx			! Panel X location of Posn, Value box
      integer	pnvposy			! Panel Y location of Posn, Value box 
      integer	pnnums(200)		! Panel number of options (sections)
      integer   pnx(200)		! Panel button X locations
      integer   pny(200)		! Panel button Y locations
      integer	pnbhposx		! Panel X location of button help button
      integer	pnbhposy		! Panel Y location of button help button

      logical	dopanel			! Panel use?
      logical	dohpanel		! Help Panel use?
      logical	pdsopen			! Panel opened?

      logical   pds_dum1		! Dummy to make up 4

      common /ds_panel/ pnsnx,   pnsny,   pnnum,    pnjcon,   pnnrow,
     +                  pnncol,  pdstype, pdnuml,  pnhposx,  pnhposy,
     +                  pncposx, pncposy, pnvposx, pnvposy,   pnnums,
     +                     pnx,      pny, pnbhposx, pnbhposy

      common /ds_panelb/ dopanel,  dohpanel,  pdsopen,   pds_dum1

