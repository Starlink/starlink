/* These routines convert fmmo network to host byte order and back.  They */
/* should only be used on machines where the OS does not already define */
/* the relevant functions */

/* Sam Southard, Jr. */
/* Created: 22-Apr-1993 */

unsigned short ntohs (netshort)
unsigned short netshort;
{
	unsigned short retval;

	retval = (((netshort & 0xFF) << 8) | (netshort >> 8));
	return (retval);
}

unsigned short htons (hostshort)
unsigned short hostshort;
{
	return (ntohs (hostshort));
}
