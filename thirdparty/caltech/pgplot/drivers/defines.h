
/* defines.h: declarations file for the cgmdraw module.

        Written by G. Edward Johnson <mailto:lorax@nist.gov>
        Date: April 1996
        Copyright: cd software produced by NIST, an agency of the 
	U.S. government, is by statute not subject to copyright
	in the United States. Recipients of this software assume all 
	responsibilities associated with its operation, modification
	and maintenance.
 
*/

#ifndef CDDEF_H
#define CDDEF_H 1

#define b0 01
#define b1 02
#define b2 04
#define b3 010
#define b4 020
#define b5 040
#define b6 0100
#define b7 0200
#define b8 0400
#define b9 01000
#define b10 02000
#define b11 04000
#define b12 010000
#define b13 020000
#define b14 040000
#define b15 0100000

/* Defines the default values for different attributes.  In general,
 * these track the CGM specificaition, so changing them is not a good idea.
 * however, it is generally ok to set them to -1 (undefined) if you want.
 */

#define CDLTYPE 1
#define CDLWIDTH 0
#define CDLCOLOR 1
#define CDSHAPESTYLE 0
#define CDSHAPECOLOR 1
#define CDSHAPEHATCH 1
#define CDEDGETYPE 1
#define CDEDGECOLOR 1
#define CDEDGEWIDTH 1
#define CDEDGEVIS 0
#define CDTEXTFONT 1
#define CDTEXTCOLOR 1
#define CDTEXTHEIGHT -1
#define CDTEXTPATH 0
#define CDMTYPE 1
#define CDMSIZE 0
#define CDMCOLOR 1
#define CDLINESPEC 1
#define CDEDGESPEC 1
#define CDMARKERSPEC 1


#endif
