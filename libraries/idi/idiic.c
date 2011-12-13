/*
*+
*  Name :
*     IDIIC.C
*
*  Purpose :
*     FORTRAN to C interface layer
*
*  Description :
*     These routines interface the top level FORTRAN routines
*     to the C routines running the X driver.
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink, RAL)
*
*  History :
*     27-FEB-1991 (NE):
*        Orignal version
*     16-MAY-1991 (NE):
*        Added conditional compilation
*     19-SEP-1991 (NE):
*        Added CNF interface
*     20-JAN-1992 (DLT):
*        Made all subroutine names lower case
*/

/* System definitions */

#include <stdio.h>
#include <string.h>

/* Package definitions */

#include "idi_err.h"
#include "f77.h"
#include "cnf.h"
#include "device.dep"
#include "idi.h"
#include "idifuncs.h"

/******************************************************************************/

F77_SUBROUTINE(ixwopn) ( CHARACTER(devnam), INTEGER(lendisp),
                         INTEGER(displayid), INTEGER(idierr)
                         TRAIL(devnam) )

/*
*+
*  Name:
*     IXWOPN
*
*  Purpose:
*     F77 to C interface used to open a display
*
*  Invocation:
*     CALL IXWOPN( devnam, lendisp, displayid, idierr )
*
*  Description:
*     F77 to C interface used to open a display
*
*  Arguments:
*     devnam = char
*        Device name
*     lendisp = int
*        Length of device name string
*     displayid = int
*        Display identifier
*     idierr = int
*        Status
*
*  Algorithm:
*     Convert the FORTRAN string into a C string.
*     Call the C routine.
*
*  Copyright:
*     Copyright (C) 1991, 1992, 1994 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink, RAL)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*      9-MAR-1994 (DLT):
*        Free temporary string after use
*-
*/

{
GENPTR_CHARACTER(devnam)
GENPTR_INTEGER(lendisp)
GENPTR_INTEGER(displayid)
GENPTR_INTEGER(idierr)

/* Local variables */
char *devnam0;

/* Copy the FORTRAN string to a local C string */
devnam0 = cnf_creim( devnam, *lendisp );

/* Call the C routine */
*idierr = IIDOPN_C( devnam0, displayid );

/* Free temporary string */
cnf_free( devnam0 );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwclo) ( INTEGER(display), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWCLO
*
*  Purpose:
*     F77 to C interface used to close a display
*
*  Invocation:
*     CALL IXWCLO( display, idierr )
*
*  Description:
*     F77 to C interface used to close a display
*
*  Arguments:
*     display = int
*        Display identifier
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIDCLO_C( *display );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwrst) ( INTEGER(display), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWRST
*
*  Purpose:
*     F77 to C interface used to reset a display
*
*  Invocation:
*     CALL IXWRST( display, idierr )
*
*  Description:
*     F77 to C interface used to reset a display
*
*  Arguments:
*     display = int
*        Display identifier
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIDRST_C( *display );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwupd) ( INTEGER(display), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWUPD
*
*  Purpose:
*     F77 to C interface used to update (refresh) display
*
*  Invocation:
*     CALL IXWUPD( display, idierr )
*
*  Description:
*     F77 to C interface used to update (refresh) display
*
*  Arguments:
*     display = int
*        Display identifier
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIDUPD_C( *display );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwerr) ( INTEGER(errn), CHARACTER(errtxt),
                         INTEGER(txtlen) TRAIL(errtxt) )

/*
*+
*  Name:
*     IXWERR
*
*  Purpose:
*     F77 to C interface used to get error text
*
*  Invocation:
*     CALL IXWERR( errn, errtxt, txtlen )
*
*  Description:
*     F77 to C interface used to get error text
*
*  Arguments:
*     errn = int
*        Error number
*     errtxt = char
*        Error text
*     txtlen = int
*        Error text length
*
*  Algorithm:
*     Call the C routine.
*     Convert the C string into a FORTRAN string.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(errn)
GENPTR_CHARACTER(errtxt)
GENPTR_INTEGER(txtlen)

/* Local variables */
void IIDERR_C( int errn, char errtxt[], int *txtlen );
char lerrtxt[256];

/* Call the C routine */
IIDERR_C( *errn, lerrtxt, txtlen );

/* Copy the local C string to a FORTRAN string */
cnf_exprt( lerrtxt, errtxt, *txtlen );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwqdv) ( INTEGER(display), INTEGER(nconf), INTEGER(xdev),
                         INTEGER(ydev), INTEGER(depthdev),
                         INTEGER(maxlutn), INTEGER(maxittn),
                         INTEGER(maxcurn), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWQDV
*
*  Purpose:
*     F77 to C interface used to query display characteristics
*
*  Invocation:
*     CALL IXWQDV( display, nconf, xdev, ydev, depthdev, maxlutn,
*                  maxittn, maxcurn, idierr )
*
*  Description:
*     F77 to C interface used to query display characteristics
*
*  Arguments:
*     display = int
*        Display identifier
*     nconf = int
*        Number of configurations
*     xdev = int
*        X display size
*     ydev = int
*        Y display size
*     depthdev = int
*        Display depth (bits/pixel)
*     maxlutn = int
*        Max number of available LUT
*     maxittn = int
*        Max number of available ITT
*     maxcurn = int
*        Max number of cursors
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(nconf)
GENPTR_INTEGER(xdev)
GENPTR_INTEGER(ydev)
GENPTR_INTEGER(depthdev)
GENPTR_INTEGER(maxlutn)
GENPTR_INTEGER(maxiitn)
GENPTR_INTEGER(maxcurn)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIDQDV_C( *display, nconf, xdev, ydev, depthdev, maxlutn,
                    maxittn, maxcurn);

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwqci) ( INTEGER(display), INTEGER(cap),
                         INTEGER(size), INTEGER(capdata),
                         INTEGER(ncap), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWQCI
*
*  Purpose:
*     F77 to C interface used to query capability (integer values)
*
*  Invocation:
*     CALL IXWQCI( display, cap, size, capdata, ncap, idierr )
*
*  Description:
*     F77 to C interface used to query capability (integer values)
*
*  Arguments:
*     display = int
*        Display identifier
*     cap = int
*        Integer capability
*     size = int
*        Output buffer size
*     capdata = int
*        Output buffer
*     ncap = int
*        Number of output values
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(cap)
GENPTR_INTEGER(size)
GENPTR_INTEGER(capdata)
GENPTR_INTEGER(ncap)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIDQCI_C( *display, *cap, *size, capdata, ncap );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwqcr) ( INTEGER(display), INTEGER(cap),
                         INTEGER(size), REAL(capdata),
                         INTEGER(ncap), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWQCR
*
*  Purpose:
*     F77 to C interface used to query capability (real values)
*
*  Invocation:
*     CALL IXWQCR( display, cap, size, capdata, ncap, idierr )
*
*  Description:
*     F77 to C interface used to query capability (real values)
*
*  Arguments:
*     display = int
*        Ddisplay identifier
*     cap = int
*        Capability
*     size = int
*        Output buffer size
*     capdata = float
*        Output buffer
*     ncap = int
*        Number of output values
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(cap)
GENPTR_INTEGER(size)
GENPTR_REAL(capdata)
GENPTR_INTEGER(ncap)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIDQCR_C( *display, *cap, *size, capdata, ncap );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwqdc) ( INTEGER(display), INTEGER(confn),
                         INTEGER(memtype), INTEGER(maxmem),
                         INTEGER(confmode), INTEGER(memlist),
                         INTEGER(memxsize), INTEGER(memysize),
                         INTEGER(memdepth), INTEGER(ittdepth),
                         INTEGER(nmem), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWQDC
*
*  Purpose:
*     F77 to C interface used to query device characteristics
*
*  Invocation:
*     CALL IXWQDC ( display, confn, memtype, maxmem, confmode, memlist,
*                   memxsize, memysize, memdepth, ittdepth, nmem, idierr )
*
*  Description:
*     F77 to C interface used to query device characteristics
*
*  Arguments:
*     display = int
*        Display identifier
*     confn = int
*        Configuration number
*     memtype = int
*        Memory type
*     maxmem = int
*        Max number of memories
*     confmode = int
*        Configuration mode
*     memlist = int
*        Memory identifier list
*     memxsize = int
*        Memory X sizes
*     memysize = int
*        Memory Y sizes
*     memdepth = int
*        Memory depth list
*     memittdepth = int
*        ITT depth list
*     nmem = int
*        Number of memories
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(confn)
GENPTR_INTEGER(memtype)
GENPTR_INTEGER(maxmem)
GENPTR_INTEGER(confmode)
GENPTR_INTEGER(memlist)
GENPTR_INTEGER(memxsize)
GENPTR_INTEGER(memysize)
GENPTR_INTEGER(memdepth)
GENPTR_INTEGER(ittdepth)
GENPTR_INTEGER(nmem)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIDQDC_C( *display, *confn, *memtype, *maxmem, confmode, memlist,
                    memxsize, memysize, memdepth, ittdepth, nmem );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwsel) ( INTEGER(display), INTEGER(confn),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWSEL
*
*  Purpose:
*     F77 to C interface used to select configuration
*
*  Invocation:
*     CALL IXWSEL( display, confn, idierr )
*
*  Description:
*     F77 to C interface used to select configuration
*
*  Arguments:
*     display = int
*        Display identifier
*     confn = int
*        Configuration number
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(confn)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIDSEL_C( *display, *confn );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwsmv) ( INTEGER(display), INTEGER(memlist),
                         INTEGER(nmem), INTEGER(vis),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWSMV
*
*  Purpose:
*     F77 to C interface used to set memory visibility
*
*  Invocation:
*     CALL IXWSMV( display, memlist, nmem, vis, idierr )
*
*  Description:
*     F77 to C interface used to set memory visibility
*
*  Arguments:
*     display = int
*        Display identifier
*     memlist = int
*        Memory identifier list
*     nmem = int
*        Number of memories
*     vis = int
*        Visibility
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memlist)
GENPTR_INTEGER(nmem)
GENPTR_INTEGER(vis)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIMSMV_C( *display, memlist, *nmem, *vis );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwwsc) ( INTEGER(display), INTEGER(memlist),
                         INTEGER(nmem), INTEGER(xscr),
                         INTEGER(yscr), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWWSC
*
*  Purpose:
*     F77 to C interface used to write memory scroll
*
*  Invocation:
*     CALL IXWWSC( display, memlist, nmem, xscr, yscr, idierr )
*
*  Description:
*     F77 to C interface used to write memory scroll
*
*  Arguments:
*     display = int
*        Display identifier
*     memlist = int
*        Memory identifier list
*     nmem = int
*        Number of memories
*     xscr = int
*        X scroll
*     yscr = int
*        Y scroll
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memlist)
GENPTR_INTEGER(nmem)
GENPTR_INTEGER(xscr)
GENPTR_INTEGER(yscr)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIZWSC_C( *display, memlist, *nmem, *xscr, *yscr );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwwzm) ( INTEGER(display), INTEGER(memlist),
                         INTEGER(nmem), INTEGER(zoom),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWWZM
*
*  Purpose:
*     F77 to C interface used to write memory zoom
*
*  Invocation:
*     CALL IXWWZM ( display, memlist, nmem, zoom, idierr )
*
*  Description:
*     F77 to C interface used to write memory zoom
*
*  Arguments:
*     display = int
*        Display identifier
*     memlist = int
*        Memory identifier list
*     nmem = int
*        Number of memories
*     zoom = int
*        Zoom factor
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memlist)
GENPTR_INTEGER(nmem)
GENPTR_INTEGER(zoom)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIZWZM_C( *display, memlist, *nmem, *zoom );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwrsz) ( INTEGER(display), INTEGER(memid),
                         INTEGER(xscr), INTEGER(yscr),
                         INTEGER(zoom), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWRSZ
*
*  Purpose:
*     F77 to C interface used to read scroll and zoom
*
*  Invocation:
*     CALL IXWRSZ ( display, memid, xscr, yscr, zoom, idierr )
*
*  Description:
*     F77 to C interface used to read scroll and zoom
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     xscr = int
*        X scroll
*     yscr = int
*        Y scroll
*     zoom = int
*        Zoom factor
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memid)
GENPTR_INTEGER(xscr)
GENPTR_INTEGER(yscr)
GENPTR_INTEGER(zoom)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIZRSZ_C( *display, *memid, xscr , yscr , zoom );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwslt) ( INTEGER(display), INTEGER(memid),
                         INTEGER(lutn), INTEGER(ittn),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWSLT
*
*  Purpose:
*     F77 to C interface used to select look-up table
*
*  Invocation:
*     CALL IXWSLT( display, memid, lutn, ittn, idierr )
*
*  Description:
*     F77 to C interface used to select look-up table
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     lutn = int
*        LUT number
*     ittn = int
*        ITT number
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memid)
GENPTR_INTEGER(lutn)
GENPTR_INTEGER(ittn)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIMSLT_C( *display, *memid, *lutn, *ittn );

return;
}


/******************************************************************************/

F77_SUBROUTINE(ixwsdp) ( INTEGER(display), INTEGER(memlist),
                         INTEGER(nmem), INTEGER(lutf),
                         INTEGER(ittf), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWSDP
*
*  Purpose:
*     F77 to C interface used to select display path
*
*  Invocation:
*     CALL IXWSDP( display, memlist, nmem, lutf, ittf, idierr )
*
*  Description:
*     F77 to C interface used to select display path
*
*  Arguments:
*     display = int
*        Display identifier
*     memlist = int
*        Memory identifier list
*     nmem = int
*        Number of memories
*     lutf = int
*        LUT flag
*     ittf = int
*        ITT flag
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memlist)
GENPTR_INTEGER(nmem)
GENPTR_INTEGER(lutf)
GENPTR_INTEGER(ittf)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIDSDP_C( *display, memlist, *nmem, lutf, ittf );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwwmy) ( INTEGER(display), INTEGER(memid),
                         INTEGER(data), INTEGER(npixel),
                         INTEGER(depth), INTEGER(packf),
                         INTEGER(x0), INTEGER(y0), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWWMY
*
*  Purpose:
*     F77 to C interface used to write memory
*
*  Invocation:
*     CALL IXWWMY( display, memid, data, npixel, depth, packf, x0, y0, idierr )
*
*  Description:
*     F77 to C interface used to write memory
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     data = int
*        Image data
*     npixel = int
*        Number of pixels
*     depth = int
*        Data depth (bits/pixel)
*     packf = int
*        Packing factor
*     x0 = int
*        X offset
*     y0 = int
*        Y offset
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memid)
GENPTR_INTEGER(data)
GENPTR_INTEGER(npixel)
GENPTR_INTEGER(depth)
GENPTR_INTEGER(packf)
GENPTR_INTEGER(x0)
GENPTR_INTEGER(y0)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIMWMY_C( *display, *memid, data, *npixel, *depth, *packf,
                    *x0, *y0);

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwcmy) ( INTEGER(display), INTEGER(memlist),
                         INTEGER(nmem), INTEGER(bck), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWCMY
*
*  Purpose:
*     F77 to C interface used to clear memory
*
*  Invocation:
*     CALL IXWCMY( display, memlist, nmem, bck, idierr )
*
*  Description:
*     F77 to C interface used to clear memory
*
*  Arguments:
*     display = int
*        Display identifier
*     memlist = int
*        Memory identifier list
*     nmem = int
*        Number of memories
*     bck = int
*        Background value
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memlist)
GENPTR_INTEGER(nmem)
GENPTR_INTEGER(bck)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIMCMY_C( *display, memlist, *nmem, *bck );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwrmy) ( INTEGER(display), INTEGER(memid),
                         INTEGER(npixel), INTEGER(x0),
                         INTEGER(y0), INTEGER(depth),
                         INTEGER(packf), INTEGER(ittf),
                         INTEGER(data), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWRMY
*
*  Purpose:
*     F77 to C interface used to read memory
*
*  Invocation:
*     CALL IXWRMY ( display, memid, npixel, x0, y0, depth, packf,
*                   ittf, data, idierr )
*
*  Description:
*     F77 to C interface used to write memory
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     npixel = int
*        Number of pixels
*     x0 = int
*        X offset
*     y0 = int
*        Y offset
*     depth = int
*        Data depth (bits/pixel)
*     packf = int
*        Packing factor
*     ittf = int
*        ITT flag
*     data = int
*        Image data
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memid)
GENPTR_INTEGER(npixel)
GENPTR_INTEGER(x0)
GENPTR_INTEGER(y0)
GENPTR_INTEGER(depth)
GENPTR_INTEGER(packf)
GENPTR_INTEGER(ittf)
GENPTR_INTEGER(data)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIMRMY_C( *display, *memid, *npixel, *x0, *y0, *depth,
                    *packf, *ittf, data );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwstw) ( INTEGER(display), INTEGER(memid),
                         INTEGER(loaddir), INTEGER(xwdim),
                         INTEGER(ywdim), INTEGER(depth),
                         INTEGER(xwoff), INTEGER(ywoff),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWSTW
*
*  Purpose:
*     F77 to C interface used to select transfer window
*
*  Invocation:
*     CALL IXWSTW ( display, memid, loaddir, xwdim, ywdim, depth,
*                   xwoff, ywoff, idierr )
*
*  Description:
*     F77 to C interface used to select transfer window
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     loaddir = int
*        Load direction
*     xwdim = int
*        X size
*     ywdim = int
*        Y size
*     depth = int
*        Data depth
*     xwoff = int
*        X offset
*     ywoff = int
*        Y offset
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memid)
GENPTR_INTEGER(loaddir)
GENPTR_INTEGER(xwdim)
GENPTR_INTEGER(ywdim)
GENPTR_INTEGER(depth)
GENPTR_INTEGER(xwoff)
GENPTR_INTEGER(ywoff)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIMSTW_C( *display, *memid, *loaddir, *xwdim, *ywdim,
                    *depth, *xwoff, *ywoff );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwply) ( INTEGER(display), INTEGER(memid),
                         INTEGER(x), INTEGER(y),
                         INTEGER(np), INTEGER(color),
                         INTEGER(style), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWPLY
*
*  Purpose:
*     F77 to C interface used to plot polyline
*
*  Invocation:
*     CALL IXWPLY( display, memid, x, y, np, color, style, idierr )
*
*  Description:
*     F77 to C interface used to plot polyline
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     x = int
*        X positions
*     y = int
*        Y positions
*     np = int
*        Number of positions
*     color = int
*        Draw color
*     style = int
*        Draw style
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memid)
GENPTR_INTEGER(x)
GENPTR_INTEGER(y)
GENPTR_INTEGER(np)
GENPTR_INTEGER(color)
GENPTR_INTEGER(style)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIGPLY_C( *display, *memid, x, y, *np, *color, *style );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwtxt) ( INTEGER(display), INTEGER(memid),
                         CHARACTER(txt), INTEGER(lentxt),
                         INTEGER(x0), INTEGER(y0),
                         INTEGER(path), INTEGER(orient),
                         INTEGER(color), INTEGER(txtsize),
                         INTEGER(idierr) TRAIL(txt) )

/*
*+
*  Name:
*     IXWTXT
*
*  Purpose:
*     F77 to C interface used to plot text
*
*  Invocation:
*     CALL IXWTXT ( display, memid, txt, lentxt, x0, y0, path, orient,
*                   color, txtsize, idierr )
*
*  Description:
*     F77 to C interface used to plot text
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     txt = char
*        Text
*     lentxt = int
*        Text length
*     x0 = int
*        X text position
*     y0 = int
*        Y text position
*     path = int
*        Text path
*     orient = int
*        Text orientation
*     color = int
*        Text color
*     txtsize = int
*        Text size
*     idierr = int
*        Status
*
*  Algorithm:
*     Convert the FORTRAN string into a C string.
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink, RAL)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*      9-MAR_1994 (DlT):
*        Free temporary string after use
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memid)
GENPTR_CHARACTER(txt)
GENPTR_INTEGER(lentxt)
GENPTR_INTEGER(x0)
GENPTR_INTEGER(y0)
GENPTR_INTEGER(path)
GENPTR_INTEGER(orient)
GENPTR_INTEGER(color)
GENPTR_INTEGER(txtsize)
GENPTR_INTEGER(idierr)

/* Local variables */
char *txt0;

/* Copy the FORTRAN string to a local C string */
txt0 = cnf_creim( txt, *lentxt );

/* Call the C routine */
*idierr = IIGTXT_C( *display, *memid, txt0, *x0, *y0, *path, *orient,
                    *color, *txtsize );

/* Free the temporary string */
cnf_free( txt0 );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwwit) ( INTEGER(display), INTEGER(memid),
                         INTEGER(ittn), INTEGER(ittstart),
                         INTEGER(ittlen), REAL(ittdata),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWWIT
*
*  Purpose:
*     F77 to C interface used to write intensity transformation table
*
*  Invocation:
*     CALL IXWWIT( display, memid, ittn, ittstart, ittlen, ittdata, idierr )
*
*  Description:
*     F77 to C interface used to write intensity transformation table
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     ittn = int
*        ITT number
*     ittstart = int
*        ITT offset
*     ittlen = int
*        ITT length
*     ittdata = float
*        ITT data
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memid)
GENPTR_INTEGER(ittn)
GENPTR_INTEGER(ittstart)
GENPTR_INTEGER(ittlen)
GENPTR_REAL(ittdata)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IILWIT_C( *display, *memid, *ittn, *ittstart, *ittlen, ittdata );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwrit) ( INTEGER(display), INTEGER(memid),
                         INTEGER(ittn), INTEGER(ittstart),
                         INTEGER(ittlen), REAL(ittdata),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWRIT
*
*  Purpose:
*     F77 to C interface used to read intensity transformation table
*
*  Invocation:
*     CALL IXWRIT( display, memid, ittn, ittstart, ittlen, ittdata, idierr )
*
*  Description:
*     F77 to C interface used to read intensity transformation table
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     ittn = int
*        ITT number
*     ittstart = int
*        ITT offset
*     ittlen = int
*        ITT length
*     ittdata = float
*        ITT data
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*     23-NOV-1992 (NE):
*        Added missing memid argument
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memid)
GENPTR_INTEGER(ittn)
GENPTR_INTEGER(ittstart)
GENPTR_INTEGER(ittlen)
GENPTR_REAL(ittdata)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IILRIT_C( *display, *memid, *ittn, *ittstart, *ittlen, ittdata );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwwlt) ( INTEGER(display), INTEGER(lutn),
                         INTEGER(lutstart), INTEGER(lutlen),
                         REAL_ARRAY(lutdata), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWWLT
*
*  Purpose:
*     F77 to C interface used to write look-up table
*
*  Invocation:
*     CALL IXWWLT( display, lutn, lutstart, lutlen, lutdata, idierr )
*
*  Description:
*     F77 to C interface used to write look-up table
*
*  Arguments:
*     display = int
*        Display identifier
*     lutn = int
*        LUT number
*     lutstart = int
*        LUT offset
*     lutlen = int
*        LUT length
*     lutdata = float[]
*        LUT data
*     idierr = int
*        Status
*
*  Algorithm:
*     Copy input array to local array.
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*     25-NOV-1991 (NE):
*        Increased size of lut0 array
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(lutn)
GENPTR_INTEGER(lutstart)
GENPTR_INTEGER(lutlen)
GENPTR_REAL_ARRAY(lutdata)
GENPTR_INTEGER(idierr)

/* Local variables */
int i;
int ii;
int j;
int jj;
float lut0[768];

/* Convert input array to row-major order */
jj = 0;
for ( j = 0; j < 3; j++ )
   {
   for ( i = 0; i < *lutlen; i++ )
      {
      ii = i * 3 + j;
      lut0[jj] = lutdata[ii];
      jj += 1;
      }
   }

/* Call the C routine */
*idierr = IILWLT_C( *display, *lutn, *lutstart, *lutlen, lut0 );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwrlt) ( INTEGER(display), INTEGER(lutn),
                         INTEGER(lutstart), INTEGER(lutlen),
                         REAL_ARRAY(lutdata), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWRLT
*
*  Purpose:
*     F77 to C interface used to read look-up table
*
*  Invocation:
*     CALL IXWRLT( display, lutn, lutstart, lutlen, lutdata, idierr )
*
*  Description:
*     F77 to C interface used to read look-up table
*
*  Arguments:
*     display = int
*        Display identifier
*     lutn = int
*        LUT number
*     lutstart = int
*        LUT offset
*     lutlen = int
*        LUT length
*     lutdata = float[]
*        LUT data
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*     Copy local array to output array.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*     25-NOV-1991 (NE):
*        Increased size of lut0 array
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(lutn)
GENPTR_INTEGER(lutstart)
GENPTR_INTEGER(lutlen)
GENPTR_REAL_ARRAY(lutdata)
GENPTR_INTEGER(idierr)

/* Local variables */
int i;
int ii;
int j;
int jj;
float lut0[768];

/* Call the C routine */
*idierr = IILRLT_C( *display, *lutn, *lutstart, *lutlen, lut0 );

/* Convert local array from row-major order */
jj = 0;
for ( j = 0; j < 3; j++ )
   {
   for ( i = 0; i < *lutlen; i++ )
      {
      ii = i * 3 + j;
      lutdata[ii] = lut0[jj];
      jj += 1;
      }
   }

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwwzp) ( INTEGER(display), INTEGER(xscr),
                         INTEGER(yscr), INTEGER(zoom),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWWZP
*
*  Purpose:
*     F77 to C interface used to write display zoom and pan
*
*  Invocation:
*     CALL IXWWZP( display, xscr, yscr, zoom, idierr )
*
*  Description:
*     F77 to C interface used to write display zoom and pan
*
*  Arguments:
*     display = int
*        Display identifier
*     xscr = int
*        X scroll
*     yscr = int
*        Y scroll
*     zoom = int
*        Zoom factor
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(xscr)
GENPTR_INTEGER(yscr)
GENPTR_INTEGER(zoom)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIZWZP_C( *display, *xscr, *yscr, *zoom );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwrzp) ( INTEGER(display), INTEGER(xscr),
                         INTEGER(yscr), INTEGER(zoom),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWRZP
*
*  Purpose:
*     F77 to C interface used to read display zoom and pan
*
*  Invocation:
*     CALL IXWRZP( display, xscr, yscr, zoom, idierr )
*
*  Description:
*     F77 to C interface used to read display zoom and pan
*
*  Arguments:
*     display = int
*        Display identifier
*     xscr = int
*        X scroll
*     yscr = int
*        Y scroll
*     zoom = int
*        Zoom factor
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(xscr)
GENPTR_INTEGER(yscr)
GENPTR_INTEGER(zoom)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIZRZP_C( *display, xscr , yscr , zoom );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwinc) ( INTEGER(display), INTEGER(memid),
                         INTEGER(curn), INTEGER(cursh),
                         INTEGER(curcol), INTEGER(xcur),
                         INTEGER(ycur), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWINC
*
*  Purpose:
*     F77 to C interface used to initialize cursors
*
*  Invocation:
*     CALL IXWINC( display, memid, curn, cursh, curcol, xcur, ycur, idierr )
*
*  Description:
*     F77 to C interface used to initialize cursors
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     curn = int
*        Cursor number
*     cursh = int
*        Cursor shape
*     curcol = int
*        Cursor colour
*     xcur = int
*        X cursor position
*     ycur = int
*        Y cursor position
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memid)
GENPTR_INTEGER(curn)
GENPTR_INTEGER(cursh)
GENPTR_INTEGER(curcol)
GENPTR_INTEGER(xcur)
GENPTR_INTEGER(ycur)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IICINC_C( *display, *memid, *curn, *cursh, *curcol, *xcur, *ycur );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwscv) ( INTEGER(display), INTEGER(curn),
                         INTEGER(vis), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWSCV
*
*  Purpose:
*     F77 to C interface used to set cursor visibility
*
*  Invocation:
*     CALL IXWSCV( display, curn, vis, idierr )
*
*  Description:
*     F77 to C interface used to set cursor visibility
*
*  Arguments:
*     display = int
*        Display identifier
*     curn = int
*        Cursor number
*     vis = int
*        Cursor visibility
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(curn)
GENPTR_INTEGER(vis)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IICSCV_C( *display, *curn, *vis );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwrcp) ( INTEGER(display), INTEGER(inmemid),
                         INTEGER(curn), INTEGER(xcur), INTEGER(ycur),
                         INTEGER(outmemid), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWRCP
*
*  Purpose:
*     F77 to C interface used to read cursor position
*
*  Invocation:
*     CALL IXWRCP( display, inmemid, curn, xcur, ycur, outmemid, idierr )
*
*  Description:
*     F77 to C interface used to read cursor position
*
*  Arguments:
*     display = int
*        Display identifier
*     inmemid = int
*        Input memory identifier
*     curn = int
*        Cursor number
*     xcur = int
*        X cursor position
*     ycur = int
*        Y cursor position
*     outmemid = int
*        Output memory identifier
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(inmemid)
GENPTR_INTEGER(curn)
GENPTR_INTEGER(xcur)
GENPTR_INTEGER(ycur)
GENPTR_INTEGER(outmemid)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IICRCP_C( *display, *inmemid, *curn, xcur, ycur, outmemid );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwwcp) ( INTEGER(display), INTEGER(memid),
                         INTEGER(curn), INTEGER(xcur),
                         INTEGER(ycur), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWWCP
*
*  Purpose:
*     F77 to C interface used to write cursor position
*
*  Invocation:
*     CALL IXWWCP( display, memid, curn, xcur, ycur, idierr )
*
*  Description:
*     F77 to C interface used to write cursor position
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     curn = int
*        Cursor number
*     xcur = int
*        X cursor position
*     ycur = int
*        Y cursor position
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memid)
GENPTR_INTEGER(curn)
GENPTR_INTEGER(xcur)
GENPTR_INTEGER(ycur)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IICWCP_C( *display, *memid, *curn, *xcur, *ycur );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwinr) ( INTEGER(display), INTEGER(memid),
                         INTEGER(roicol), INTEGER(roixmin),
                         INTEGER(roiymin), INTEGER(roixmax),
                         INTEGER(roiymax), INTEGER(roiid),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWINR
*
*  Purpose:
*     F77 to C interface used to initialize region of interest
*
*  Invocation:
*     CALL IXWINR( display, memid, roicol, roixmin, roiymin, roixmax,
*                  roiymax, roiid, idierr )
*
*  Description:
*     F77 to C interface used to initialize region of interest
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     roicol = int
*        ROI colour
*     roixmin = int
*        ROI X min
*     roiymin = int
*        ROI Y min
*     roixmax = int
*        ROI X max
*     roiymax = int
*        ROI Y max
*     roiid = int
*        ROI identifier
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memid)
GENPTR_INTEGER(roicol)
GENPTR_INTEGER(roixmin)
GENPTR_INTEGER(roiymin)
GENPTR_INTEGER(roixmax)
GENPTR_INTEGER(roiymax)
GENPTR_INTEGER(roiid)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIRINR_C( *display, *memid, *roicol, *roixmin, *roiymin,
                    *roixmax, *roiymax, roiid );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwrri) ( INTEGER(display), INTEGER(inmemid),
                         INTEGER(roiid), INTEGER(roixmin),
                         INTEGER(roiymin), INTEGER(roixmax),
                         INTEGER(roiymax), INTEGER(outmemid),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWRRI
*
*  Purpose:
*     F77 to C interface used to read ROI position
*
*  Invocation:
*     CALL IXWRRI( display, inmemid, roiid, roixmin, roiymin, roixmax,
*                  roiymax, outmemid, idierr )
*
*  Description:
*     F77 to C interface used to read ROI position
*
*  Arguments:
*     display = int
*        Display identifier
*     inmemid = int
*        Input memory identifier
*     roiid = int
*        ROI identifier
*     roixmin = int
*        ROI X min
*     roiymin = int
*        ROI Y min
*     roixmax = int
*        ROI X max
*     roiymax = int
*        ROI Y max
*     outmemid = int
*        Output memory identifier
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(inmemid)
GENPTR_INTEGER(roiid)
GENPTR_INTEGER(roixmin)
GENPTR_INTEGER(roiymin)
GENPTR_INTEGER(roixmax)
GENPTR_INTEGER(roiymax)
GENPTR_INTEGER(outmemid)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIRRRI_C( *display, *inmemid, *roiid, roixmin, roiymin,
                    roixmax, roiymax, outmemid );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwwri) ( INTEGER(display), INTEGER(memid),
                         INTEGER(roiid), INTEGER(roixmin),
                         INTEGER(roiymin), INTEGER(roixmax),
                         INTEGER(roiymax), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWWRI
*
*  Purpose:
*     F77 to C interface used to write ROI position
*
*  Invocation:
*     CALL IXWWRI ( display, memid, roiid, roixmin, roiymin, roixmax,
*                  roiymax, idierr )
*
*  Description:
*     F77 to C interface used to write ROI position
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     roiid = int
*        ROI identifier
*     roixmin = int
*        ROI X min
*     roiymin = int
*        ROI Y min
*     roixmax = int
*        ROI X max
*     roiymax = int
*        ROI Y max
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memid)
GENPTR_INTEGER(roiid)
GENPTR_INTEGER(roixmin)
GENPTR_INTEGER(roiymin)
GENPTR_INTEGER(roixmax)
GENPTR_INTEGER(roiymax)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIRWRI_C( *display, *memid, *roiid, *roixmin, *roiymin,
                    *roixmax, *roiymax );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwsrv) ( INTEGER(display), INTEGER(roiid),
                         INTEGER(vis), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWSRV
*
*  Purpose:
*     F77 to C interface used to set visibility of region of interest
*
*  Invocation:
*     CALL IXWSRV( display, roiid, vis, idierr )
*
*  Description:
*     F77 to C interface used to set visibility of region of interest
*
*  Arguments:
*     display = int
*        Display identifier
*     roiid = int
*        ROI identifier
*     vis = int
*        ROI visibility
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(roiid)
GENPTR_INTEGER(vis)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIRSRV_C( *display, *roiid, *vis );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixweni) ( INTEGER(display), INTEGER(intype),
                         INTEGER(intid), INTEGER(objtype),
                         INTEGER(objid), INTEGER(oper),
                         INTEGER(trigger), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWENI
*
*  Purpose:
*     F77 to C interface used to enable interaction
*
*  Invocation:
*     CALL IXWENI( display, intype, intid, objtype, objid, oper,
*                  trigger, idierr )
*
*  Description:
*     F77 to C interface used to enable interaction
*
*  Arguments:
*     display = int
*        Display identifier
*     intype = int
*        Interactor type
*     intid = int
*        Interactor identifier
*     objtype = int
*        Object type
*     objid = int
*        Object identifier
*     oper = int
*        Interactive operation
*     trigger = int
*        Exit trigger
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(intype)
GENPTR_INTEGER(intid)
GENPTR_INTEGER(objtype)
GENPTR_INTEGER(objid)
GENPTR_INTEGER(oper)
GENPTR_INTEGER(trigger)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIIENI_C( *display, *intype, *intid, *objtype, *objid,
                    *oper, *trigger );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixweiw) ( INTEGER(display), INTEGER(trgstatus),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWEIW
*
*  Purpose:
*     F77 to C interface used to execute interaction and wait
*
*  Invocation:
*     CALL IXWEIW( display, trgstatus, idierr )
*
*  Description:
*     F77 to C interface used to execute interaction and wait
*
*  Arguments:
*     display = int
*        Display identifier
*     trigstatus = int
*        Trigger status array
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(trgstatus)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIIEIW_C( *display, trgstatus );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwsti) ( INTEGER(display), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWSTI
*
*  Purpose:
*     F77 to C interface used to stop interactions
*
*  Invocation:
*     CALL IXWSTI( display, idierr )
*
*  Description:
*     F77 to C interface used to stop interactions
*
*  Arguments:
*     display = int
*        Display identifier
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIISTI_C( *display );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwqid) ( INTEGER(display), INTEGER(intyp),
                         INTEGER(intid), CHARACTER(intdscr),
                         INTEGER(lendscr), INTEGER(idierr)
                         TRAIL(intdscr) )

/*
*+
*  Name:
*     IXWQID
*
*  Purpose:
*     F77 to C interface used to query interactor description
*
*  Invocation:
*     CALL IXWQID( display, intyp, intid, intdscr, lendscr, idierr )
*
*  Description:
*     F77 to C interface used to query interactor description
*
*  Arguments:
*     display = int
*        Display identifier
*     intyp = int
*        Interactor type
*     intid = int
*        Interactor identifier
*     indscr = char
*        Interactor description
*     lendscr = int
*        Description length
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*     4-SEP-2004 (TIMJ):
*        Protect cnf_exprt if we get bad status from IIIQID_C
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(intyp)
GENPTR_INTEGER(intid)
GENPTR_CHARACTER(intdscr)
GENPTR_INTEGER(lendscr)
GENPTR_INTEGER(idierr)

/* Local variables */
char lintdscr[256];

/* Call the C routine */
*idierr = IIIQID_C( *display, *intyp, *intid, lintdscr, lendscr );

/* Copy the local C string to a FORTRAN string */
if (*idierr == IDI__OK) {
  cnf_exprt( lintdscr, intdscr, *lendscr );
} else {
  *lintdscr = '\0';
  *lendscr = 0;
  cnf_exprt( lintdscr, intdscr, intdscr_length );
}

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwgld) ( INTEGER(display), INTEGER(locn),
                         INTEGER(xdsp), INTEGER(ydsp),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWGLD
*
*  Purpose:
*     F77 to C interface used to get locator displacement
*
*  Invocation:
*     CALL IXWGLD( display, locn, xdsp, ydsp, idierr )
*
*  Description:
*     F77 to C interface used to get locator displacement
*
*  Arguments:
*     display = int
*        Display identifier
*     locn = int
*        Locator number
*     xdsp = int
*        X displacement
*     ydsp = char
*        Y displacement
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(locn)
GENPTR_INTEGER(xdsp)
GENPTR_INTEGER(ydsp)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIIGLD_C( *display, *locn, xdsp, ydsp );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwgie) ( INTEGER(display), INTEGER(evlid),
                         INTEGER(evlival), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWGIE
*
*  Purpose:
*     F77 to C interface used to get integer evaluator
*
*  Invocation:
*     CALL IXWGIE( display, evlid, evlival, idierr )
*
*  Description:
*     F77 to C interface used to get integer evaluator
*
*  Arguments:
*     display = int
*        Display identifier
*     evlid = int
*        Evaluator identifier
*     evlival = int
*        Evaluator integer value
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(evlid)
GENPTR_INTEGER(evlival)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIIGIE_C( *display, *evlid, evlival );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwgre) ( INTEGER(display), INTEGER(evlid),
                         REAL(evlrval), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWGRE
*
*  Purpose:
*     F77 to C interface used to get real evaluator
*
*  Invocation:
*     CALL IXWGRE( display, evlid, evlrval, idierr )
*
*  Description:
*     F77 to C interface used to get real evaluator
*
*  Arguments:
*     display = int
*        Display identifier
*     evlid = int
*        Evaluator identifier
*     evlrval = float
*        Evaluator real value
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(evlid)
GENPTR_REAL(evlrval)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIIGRE_C( *display, *evlid, evlrval );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwgle) ( INTEGER(display), INTEGER(evlid),
                         INTEGER(evllval), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWGLE
*
*  Purpose:
*     F77 to C interface used to get logical evaluator
*
*  Invocation:
*     CALL IXWGLE( display, evlid, evllval, idierr )
*
*  Description:
*     F77 to C interface used to get logical evaluator
*
*  Arguments:
*     display = int
*        Display identifier
*     evlid = int
*        Evaluator identifier
*     evllval = int
*        Evaluator logical value
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(evlid)
GENPTR_INTEGER(evllval)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIIGLE_C( *display, *evlid, evllval );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwgse) ( INTEGER(display), INTEGER(evlid),
                         CHARACTER(evlsval), INTEGER(svallen),
                         INTEGER(idierr) TRAIL(evlsval) )

/*
*+
*  Name:
*     IXWGSE
*
*  Purpose:
*     F77 to C interface used to get string evaluator
*
*  Invocation:
*     CALL IXWGSE( display, evlid, evlsval, svallen, idierr )
*
*  Description:
*     F77 to C interface used to get string evaluator
*
*  Arguments:
*     display = int
*        Display identifier
*     evlid = int
*        Evaluator identifier
*     evlsval = char
*        Evaluator string value
*     svallen = int
*        String length
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(evlid)
GENPTR_CHARACTER(evlsval)
GENPTR_INTEGER(svallen)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIIGSE_C( *display, *evlid, evlsval, svallen );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwsnp) ( INTEGER(display), INTEGER(colmode),
                         INTEGER(npixel), INTEGER(x0), INTEGER(y0),
                         INTEGER(depth), INTEGER(packf),
                         INTEGER(data), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWSNP
*
*  Purpose:
*     F77 to C interface used to create snapshot
*
*  Invocation:
*     CALL IXWSNP( display, colmode, npixel, x0, y0, depth, packf,
*                  data, idierr )
*
*  Description:
*     F77 to C interface used to create snapshot
*
*  Arguments:
*     display = int
*        Display identifier
*     colmode = int
*        Colour mode
*     npixel = int
*        Number of pixels
*     x0 = int
*        X offset
*     y0 = int
*        Y offset
*     depth = int
*        Data depth
*     packf = int
*        Packing factor
*     data = int
*        Image data
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(colmode)
GENPTR_INTEGER(npixel)
GENPTR_INTEGER(x0)
GENPTR_INTEGER(y0)
GENPTR_INTEGER(depth)
GENPTR_INTEGER(packf)
GENPTR_INTEGER(data)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIDSNP_C( *display, *colmode, *npixel, *x0, *y0, *depth,
                    *packf, data );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwblm) ( INTEGER(display), INTEGER(memlist),
                         INTEGER(nmem), REAL(period),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWBLM
*
*  Purpose:
*     F77 to C interface used to blink memories
*
*  Invocation:
*     CALL IXWBLM( display, memlist, nmem, period, idierr )
*
*  Description:
*     F77 to C interface used to blink memories
*
*  Arguments:
*     display = int
*        Display identifier
*     memlist = int
*        Memory identifier list
*     nmem = int
*        Number of memories
*     peroid = float
*        Blink period
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memlist)
GENPTR_INTEGER(nmem)
GENPTR_REAL(period)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIMBLM_C( *display, memlist, *nmem, period );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwsss) ( INTEGER(display), INTEGER(memlist),
                         INTEGER(xoff), INTEGER(yoff),
                         INTEGER(splitf), INTEGER(xsplit),
                         INTEGER(ysplit), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWSSS
*
*  Purpose:
*     F77 to C interface used to define split screen
*
*  Invocation:
*     CALL IXWSSS( display, memlist, xoff, yoff, splitf, xsplit,
*                  ysplit, idierr )
*
*  Description:
*     F77 to C interface used to define split screen
*
*  Arguments:
*     display = int
*        Display identifier
*     memlist = int
*        Memory identifier list
*     xoff = int
*        X offsets
*     yoff = int
*        Y offsets
*     splitf = int
*        Split flag
*     xsplit = int
*        X split value
*     ysplit = int
*        Y split value
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memlist)
GENPTR_INTEGER(xoff)
GENPTR_INTEGER(xoff)
GENPTR_INTEGER(splitf)
GENPTR_INTEGER(xsplit)
GENPTR_INTEGER(ysplit)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIDSSS_C( *display, memlist, xoff, yoff, *splitf,
                    *xsplit, *ysplit );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwsbv) ( INTEGER(display), INTEGER(memid),
                         INTEGER(vis), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWSBV
*
*  Purpose:
*     F77 to C interface used to set intensity bar visibility
*
*  Invocation:
*     CALL IXWSBV( display, memid, vis, idierr )
*
*  Description:
*     F77 to C interface used to set intensity bar visibility
*
*  Arguments:
*     display = int
*        Display identifier
*     memid = int
*        Memory identifier
*     vis = int
*        Visibility
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(memid)
GENPTR_INTEGER(vis)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IILSBV_C( *display, *memid, *vis );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwiag) ( INTEGER(display), INTEGER(outid),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWIAG
*
*  Purpose:
*     F77 to C interface used to define diagnostic output
*
*  Invocation:
*     CALL IXWIAG( display, outid, idierr )
*
*  Description:
*     F77 to C interface used to define diagnostic output
*
*  Arguments:
*     display = int
*        Display identifier
*     outid = int
*        Output identifier
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(outid)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIDIAG_C( *display, *outid );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwenc) ( INTEGER(display), INTEGER(idierr) )

/*
*+
*  Name:
*     IXWENC
*
*  Purpose:
*     F77 to C interface used to enable dynamic memory configuration
*
*  Invocation:
*     CALL IXWENC( display, idierr )
*
*  Description:
*     F77 to C interface used to enable dynamic memory configuration
*
*  Arguments:
*     display = int
*        Display identifier
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIDENC_C( *display );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwamy) ( INTEGER(display), INTEGER(xdim),
                         INTEGER(ydim), INTEGER(mdepth),
                         INTEGER(mtype), INTEGER(memid),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWAMY
*
*  Purpose:
*     F77 to C interface used to allocate memory
*
*  Invocation:
*     CALL IXWAMY( display, xdim, ydim, mdepth, mtype, memid, idierr )
*
*  Description:
*     F77 to C interface used to allocate memory
*
*  Arguments:
*     display = int
*        Display identifier
*     xdim = int
*        X memory size
*     ydim = int
*        Y memory size
*     mdepth = int
*        Memory depth
*     mtype = int
*        Memory type
*     memid = int
*        Memory identifier
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(xdim)
GENPTR_INTEGER(ydim)
GENPTR_INTEGER(mdepth)
GENPTR_INTEGER(mtype)
GENPTR_INTEGER(memid)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIDAMY_C( *display, *xdim, *ydim, *mdepth, *mtype, memid );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwstc) ( INTEGER(display), INTEGER(confid),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWSTC
*
*  Purpose:
*     F77 to C interface used to stop dynamic memory configuration
*
*  Invocation:
*     CALL IXWSTC( display, confid, idierr )
*
*  Description:
*     F77 to C interface used to stop dynamic memory configuration
*
*  Arguments:
*     display = int
*        Display identifier
*     confid = int
*        Configuration number
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(confid)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIDSTC_C( *display, confid );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwrlc) ( INTEGER(display), INTEGER(confid),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWRLC
*
*  Purpose:
*     F77 to C interface used to release dynamic configuration
*
*  Invocation:
*     CALL IXWRLC( display, confid, idierr )
*
*  Description:
*     F77 to C interface used to release dynamic configuration
*
*  Arguments:
*     display = int
*        Display identifier
*     confid = int
*        Configuration number
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_INTEGER(confid)
GENPTR_INTEGER(idierr)

/* Call the C routine */
*idierr = IIDRLC_C( *display, *confid );

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwgep) ( CHARACTER(parnam), INTEGER(lbuf),
                         CHARACTER(buff), INTEGER(idierr)
                         TRAIL(parnam) TRAIL(buff) )

/*
*+
*  Name:
*     IXWGEP
*
*  Purpose:
*     F77 to C interface used to get escape parameter
*
*  Invocation:
*     CALL IXWGEP( parnam, lbuf, buff, idierr )
*
*  Description:
*     F77 to C interface used to get escape parameter
*
*  Arguments:
*     parnam = char
*        Parameter name
*     lbuf = int
*        Length of output buffer
*     buff = char
*        Output buffer
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_CHARACTER(parnam)
GENPTR_INTEGER(lbuf)
GENPTR_CHARACTER(buff)
GENPTR_INTEGER(idierr)

/* Local variables */
int  i;

/*
for ( i = 0; i < *lbuf; i++ )
   buff[i] = ' ';

*idierr = IIEGEP_C( parnam, lbuf, buff );

buff[*lbuf] = ' ';
*/

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwpep) ( CHARACTER(parnam), INTEGER(lbuf),
                         CHARACTER(buff), INTEGER(idierr)
                         TRAIL(parnam) TRAIL(buff) )

/*
*+
*  Name:
*     IXWPEP
*
*  Purpose:
*     F77 to C interface used to put escape parameter
*
*  Invocation:
*     CALL IXWPEP( parnam, lbuf, buff, idierr )
*
*  Description:
*     F77 to C interface used to put escape parameter
*
*  Arguments:
*     parnam = char
*        Parameter name
*     lbuf = int
*        Length of input buffer
*     buff = char
*        Input buffer
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_CHARACTER(parnam)
GENPTR_INTEGER(lbuf)
GENPTR_CHARACTER(buff)
GENPTR_INTEGER(idierr)

/*
*idierr = IIEPEP_C( parnam, *lbuf, buff );
*/

return;
}

/******************************************************************************/

F77_SUBROUTINE(ixwebm) ( INTEGER(display), CHARACTER(bmdscr),
                         INTEGER(lendscr), CHARACTER(bmtype),
                         INTEGER(xdim), INTEGER(ydim),
                         INTEGER(idierr) )

/*
*+
*  Name:
*     IXWEBM
*
*  Purpose:
*     F77 to C interface used to define external bitmap
*
*  Invocation:
*     CALL IXWEBM( display, bmdscr, lendscr, bmtype, xdim, ydim, idierr )
*
*  Description:
*     F77 to C interface used to define external bitmap
*
*  Arguments:
*     display = int
*        Display identifier
*     bmdscr = char
*        Bitmap descriptor
*     lendscr = int
*        Bitmap descriptor length
*     bmtype = char
*        Bitmap type
*     xdim = int
*        X bitmap size
*     ydim = int
*        Y bitmap size
*     idierr = int
*        Status
*
*  Algorithm:
*     Call the C routine.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     27-FEB-1991 (NE):
*        Orignal version
*     23-MAY-1991 (NE):
*        Removed definition of strpbrk
*     19-SEP-1991 (NE):
*        Added CNF interface
*-
*/

{
GENPTR_INTEGER(display)
GENPTR_CHARACTER(bmdscr)
GENPTR_INTEGER(lendscr)
GENPTR_CHARACTER(bmtype)
GENPTR_INTEGER(xdim)
GENPTR_INTEGER(ydim)
GENPTR_INTEGER(idierr)

/* Local variables */
char *bmdscr0;


/* Copy the bitmap descriptor to a local string */
bmdscr0 = cnf_creim( bmdscr, *lendscr );

/* Call the C routine */
*idierr = IIMEBM_C( *display, bmdscr0, *bmtype, xdim, ydim );

return;
}

