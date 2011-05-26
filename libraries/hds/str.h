#if !defined( STR_INCLUDED )	 /* str.h already included?		    */
#define STR_INCLUDED 1

#if !defined( vms )
#define counted_string 1
#endif

/* STR - STRing (character strings are passed as pointers to STR structures) */

#define STR_K_LENGTH	256			/* Standard length of string */

#ifdef vms
#define STR DSC
#endif

#ifdef counted_string
struct STR
	{
	char	body[STR_K_LENGTH+1];	/* Body of string */
	};
#endif

/* Definitions of _str* macros for VMS descriptors */

#ifdef vms

#define _strconst(_name,_body)\
	_dscinit(_name,sizeof(_body)-1,_body)

#define _strinit(_name,_length,_body)\
	_dscinit(_name,_length,_body)

#define _strlen(_name)\
	strlen( (char *) (_name)->body )

#define _strimp(d,c,l)\
	_dscinit(d,*(l) = (c)->length,(c)->body)

#define _strexp(d,c,l)\
	_dscinit(d,*(l) = (c)->length,(c)->body)

#endif

/* Definitions of _str* macros for counted strings */

#ifdef counted_string

#define _strconst(_name,_body)\
	(strncpy((_name)->body,_body,STR_K_LENGTH),\
	 (_name)->body[STR_K_LENGTH] = '\0')

#define _strinit(_name,_length,_body)\
	(_name)->body[_length] = '\0'

#define _strlen(_name)\
	strlen((_name)->body)

#define _strimp(d,c,l)\
	_dscinit(d,*(l),(c)->body)

#define _strexp(d,c,l)\
	_dscinit(d,*(l),(c)->body)

#endif

/* Macros to handle null terminated strings - as in the C interface in HDS 4 */

#define _strcsimp(d,c)\
  _dscinit(d,(c?strlen(c):0),c)

#define _strcsexp(d,c)\
  _dscinit(d,(c?strlen(c):0),c)

#define _strflcsimp(d,c,l)\
        _dscinit(d,l,c)

#define _strflcsexp(d,c,l)\
        _dscinit(d,l,c)
#endif
