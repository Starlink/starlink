main () {
    /* Are we little or big endian?  From Harbison&Steele.  */
    union
    {
	long l;
	char c[sizeof (long)];
    } u;
    u.l = 1;
    exit (u.c[sizeof (long) - 1] == 1);
    /* Return 0 (success) if we're little-endian,
       or 1 (fail) if we're big-endian */
}
