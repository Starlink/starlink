/*	@(#)expand_name.h 10.6 88/02/08 SMI	*/

/*
 * Copyright (c) 1984 by Sun Microsystems, Inc.
 */

/*	struct namelist *
 *	expand_name(str)
 *	    char	*str;
 *
 *	void
 *	free_namelist(nl)
 *	    struct namelist *nl;
 *
 *  expand_name returns a pointer to a struct namelist containing
 *	the client's shell's idea of what its argument expands to.
 *	If str contains no shell metacharacters, the shell is never
 *	consulted, and the argument string is simply returned
 *	in a singleton namelist.
 *
 *	In case of errors, expand_name() writes an error message to
 *	stderr, and returns NONAMES.
 *
 *  free_namelist
 *	The struct namelist is dynamically allocated, and should be
 *	freed after use by a call to free_namelist().
 */

struct namelist	{
	unsigned	 count;
	char		*names[1];	/* LIE --it's actually names[count]
					 * followed by the strings themselves
					 */
};
#define NONAMES	((struct namelist*) 0)

struct namelist	*expand_name();
void		 free_namelist();

/*
 *	For hysterical reasons, the following defines are also provided:
 */

#define globlist  namelist
#define glob	  expand_name
#define glob_free free_namelist


