/*============================================================================
*
*   WCSLIB - an implementation of the FITS WCS proposal.
*   Copyright (C) 1995, Mark Calabretta
*
*   This library is free software; you can redistribute it and/or modify it
*   under the terms of the GNU Library General Public License as published
*   by the Free Software Foundation; either version 2 of the License, or (at
*   your option) any later version.
*
*   This library is distributed in the hope that it will be useful, but
*   WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library
*   General Public License for more details.
*
*   You should have received a copy of the GNU Library General Public License
*   along with this library; if not, write to the Free Software Foundation,
*   Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*
*   Correspondence concerning WCSLIB may be directed to:
*      Internet email: mcalabre@atnf.csiro.au
*      Postal address: Dr. Mark Calabretta,
*                      Australia Telescope National Facility,
*                      P.O. Box 76,
*                      Epping, NSW, 2121,
*                      AUSTRALIA
*
*=============================================================================
*
* C interfaces to FORTRAN PGPLOT routines used by the WCSLIB test programs.
* These are provided to allow call-by-value semantics particularly in relation
* to the use of constant arguments.
*
* $Id: tpgc.c,v 2.0 1995/09/12 06:23:56 mcalabre Exp $
*---------------------------------------------------------------------------*/

void pgbeg (unit, file, nxsub, nysub)

char  file[80];
int   nxsub, nysub, unit;

{
   register int j;

   for (j = 0; j < 80; j++) {
      if (file[j] == '\0') {
         for (; j < 80; file[j++] = ' ');
      }
   }

   pgbeg2_(&unit, file, &nxsub, &nysub);
}

/*--------------------------------------------------------------------------*/

void pgend ()

{
   pgend_();
}

/*--------------------------------------------------------------------------*/

void pgenv (dxmin, dxmax, dymin, dymax, just, axis)

int axis, just;
double dxmax, dxmin, dymax, dymin;

{
   float  fxmax, fxmin, fymax, fymin;

   fxmax = (float)dxmax;
   fxmin = (float)dxmin;
   fymax = (float)dymax;
   fymin = (float)dymin;
   pgenv_(&fxmin, &fxmax, &fymin, &fymax, &just, &axis);
}

/*--------------------------------------------------------------------------*/

void pgline (n, xpts, ypts)

int n;
float xpts[], ypts[];

{
   pgline_(&n, xpts, ypts);
}

/*--------------------------------------------------------------------------*/

void pgpage ()

{
   pgpage_();
}

/*--------------------------------------------------------------------------*/

void pgpt (n, xpts, ypts, symbol)

int n;
float xpts[], ypts[];
int symbol;

{
   pgpt_(&n, xpts, ypts, &symbol);
}

/*--------------------------------------------------------------------------*/

void pgsci (ci)

int ci;

{
   pgsci_(&ci);
}

/*--------------------------------------------------------------------------*/

void pgscr (ci, dcr, dcg, dcb)

int    ci;
double dcr, dcg, dcb;

{
   float  fcr, fcg, fcb;

   fcr = (float)dcr;
   fcg = (float)dcg;
   fcb = (float)dcb;
   pgscr_(&ci, &fcr, &fcg, &fcb);
}

/*--------------------------------------------------------------------------*/

void pgsls (ls)

int ls;

{
   pgsls_(&ls);
}

/*--------------------------------------------------------------------------*/

void pgslw (lw)

int lw;

{
   pgslw_(&lw);
}

/*--------------------------------------------------------------------------*/

void pgtext (dx, dy, text)

char text[80];
double dx, dy;

{
   register int j;
   float  fx, fy;

   fx = (float)dx;
   fy = (float)dy;

   for (j = 0; j < 80; j++) {
      if (text[j] == '\0') {
         for (; j < 80; text[j++] = ' ');
      }
   }

   pgtxt2_(&fx, &fy, text);
}
