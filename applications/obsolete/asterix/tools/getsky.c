/*
 * Get data from the SkyView server. 
 *
 * This is a standalone program which makes a request of the
 * SkyView system based on command line arguments and returns
 * a FITS file on standard output.
 *
 * This version should compile under Ultrix and VMS/Multinet.
 *
 * Created: 1-February-1995 by T. McGlynn
 *          Goddard Space Flight Center
 *          Universities Space Research Association
 *          Code 668.1
 *
 */
#define SKYVIEW "skyview.gsfc.nasa.gov"
#define PORT	80
#define ERROR  -1

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <netdb.h>

/* Define return types of functions */
char	*mo_code();
char	*form_request();
char	*strchr();
void	lowcase();


/* Request buffer */
char	req_str[2048];

/* Output file name.  Defaults to standard out. */
char	*out_file = 0;

/* Is this an informational request */
int	info_req = 0;


main(argc, argv)

int	argc;
char	*argv[];
{
    int		name_given=0;		/* Name or coordinates? */

    /* Request fields */
    char	survey[64], projection[64], equinox[64];
    char	xpixel[64], ypixel[64], size[64], coordinate[64];
    char	coords[1024];

    int		i;
    char	*p;
    
    /* Initialize all fields to zero */
    survey[0]  = 0;
    projection[0] = 0;
    equinox[0] = 0;
    xpixel[0] = 0;
    ypixel[0] = 0;
    size[0] = 0;
    coordinate[0] = 0;
    coords[0] = 0;
    
    /* Check that there are enough arguments  */
    if (argc < 2) 
    {
	printf("Usage: getsky ra dec [optional arguments]  or\n");
	printf("   or  getsky name [optional arguments]\n");
	printf("   or  getsky surveys\n");
	printf("\n");
	printf("   where the optional arguments are of the form\n");
	printf("   keyword=value.  Valid arguments include:\n");
	printf("              survey=\'ROSAT PSPC\'   [SkyView survey]\n");
	printf("              xpixel=300            [Number of pixels in X]\n");
	printf("              ypixel=300            [Number of pixels in Y]\n");
	printf("              image_size=default    [Image size in degrees]\n");
	printf("              projection=Gnomonic   [Map projection used]\n");
	printf("	      coordinate=Equatorial [Coordinate system]\n");
	printf("              equinox=2000          [Equinox of coordinate system]\n");
	printf("       	      file=standard output  [Output file name]\n");
	printf("    with the default values given.\n");
	printf("\n");
	printf("    Where only one number is specifed for in the RA and Dec\n");
        printf("    fields they are assumed to be decimal degrees.  When more\n");
	printf("    than one number is specified they are assumed to be \n");
	printf("    in sexigesimal notation.  For equatorial coordinates\n");
	printf("    sexigesimal RA's are in hours.\n");
	printf("    Object names are resolved by SIMBAD.\n");
	printf("\n");
	printf("    The last form of the command returns the current SkyView survey names\n");
	
	printf("\n");
	printf("Examples:\n");
	printf("    getsky m31 survey=\"digitized sky survey\" image_size=.1 file=f1.fits\n");
	printf("    getsky \"00 40\" \"41 00\" survey=\"ROSAT PSPC\" file=f2.fits\n");
	printf("\n");
	printf("Note that some fields contain internal spaces.\n");
	exit();
    } 

    /* Convert all arguments to lower case */
    for (i=0; i<argc; i++)
    	lowcase(argv[i]);
    
    /* Check if we have an RA and DEC fields or a single field. */
    
    if (argc == 2)
    	name_given=1;
    else if (strchr(argv[2], '='))
        name_given=1;


    if (name_given) {
	i = 2;
	sprintf(coords, "VCOORD=%s", argv[1]);
    }
    else
    {
        i=3;
	sprintf(coords, "VCOORD=%s,%s", argv[1], argv[2]);
    }

    /* Loop over other arguments */	     
    for (;i<argc; i++) 
    {
	
        p = argv[i];    
	
	if (!strncmp(p,"file=", 5) )
	{
	    out_file = strchr(p, '=') + 1;
	}
	
	    
	if (!strncmp(p, "equinox", 7) ) 
        {
	    strcpy(equinox,"EQUINX");
	    strcat(equinox,p+7);
	}
	
	else if (!strncmp(p, "survey",6) )
	{
	    strcpy(survey, "SURVEY=");
	    strcat(survey, p+7);
	}
	
	else if (!strncmp(p, "projection",10) )
	{
	    strcpy(projection, "MAPROJ");
	    strcat(projection, p+10);
	}
	else if (!strncmp(p, "xpixel",6) )
	{
	    strcpy(xpixel, "PIXELX=");
	    strcat(xpixel, p+7);
	}
	else if (!strncmp(p, "ypixel",6) )
	{
	    strcpy(ypixel, "PIXELY=");
	    strcat(ypixel, p+7);
	}
	else if (!strncmp(p, "coordinate",10) )
        {
	    strcpy(coordinate, "SCOORD");
	    strcat(coordinate, p+10);
	}
	else if (!strncmp(p, "image_size",10) )
        {
	    strcpy(size, "SFACTR");
	    strcat(size, p+10);
	}
    }
    

    /* Handle the special case where the user just wants
     * a list of surveys.
     */
    if ((!strcmp(argv[1], "surveys") || !strcmp(argv[1], "SURVEYS")) ) {
	info_req = 1;
	p = "REQUEST=SURVEYS\012\012";
    } else {
	p= form_request(coords, survey, projection, coordinate, xpixel,
          ypixel, size, equinox);
    }

    strcpy(req_str, p);
    getimage();

}

char *form_request(coords, survey, projection, coordinate, xpixel,
		   ypixel, size, equinox)

char	*coords, *survey, *projection, *coordinate, *xpixel;
char	*ypixel, *size, *equinox;

{
    static char buf[2048];
    char	lbuf[128];
    char	*t = buf;
    
    
    /* Concatenate argument fields together.  Add in defaults
     * for arguments not specified.
     */
    strcpy(buf, mo_code(coords));
    
    strcat(buf, "&");
    if (*survey) {
	strcat(buf, mo_code(survey));
    } else {
	strcat(buf, "SURVEY=Digitized+Sky+Survey");
    }

    strcat(buf, "&");
    if (*coordinate) {
	strcat(buf, mo_code(coordinate));
    } else {
	strcat(buf, "SCOORD=Equatorial");
    }
    
    strcat(buf, "&");
    if (*projection) {
	strcat(buf, mo_code(projection));
    } else {
	strcat(buf, "MAPROJ=Gnomonic");
    }
	
    strcat(buf, "&");
    if (*xpixel) {
	strcat(buf, mo_code(xpixel));
    } else {
	strcat(buf, "PIXELX=300");
    }

    strcat(buf, "&");
    if (*ypixel) {
	strcat(buf, mo_code(ypixel));
    } else {
	strcat(buf, "PIXELY=300");
    }

    strcat(buf, "&");
    if (*size) {
	strcat(buf, mo_code(size));
    } else {
	strcat(buf, "SFACTR=Default");
    }

    strcat(buf, "&");
    if (*equinox) {
	strcat(buf, mo_code(equinox));
    } else {
	strcat(buf, "EQUINX=2000");
    }
    
    strcat(buf, "\013\012\012");

    return buf;
}


/* Send request and receive data.
 */
int getimage()

{
	int s, n;
	char buf[16384];
        char nbuf[16384];
   	char nbuf2[16384];

	struct sockaddr_in sin;
	struct hostent *hp;
	struct servent *sp;
        int val;
        int i;

	hp = gethostbyname(SKYVIEW);
	if (hp == NULL) {
		return ERROR;
	}
	/*
	 *  Create an IP-family socket on which to make the connection
	 */

	s = socket(hp->h_addrtype, SOCK_STREAM, 0);
	if (s < 0) {
		return ERROR;
	}


	sin.sin_family = hp->h_addrtype;
	memcpy(&sin.sin_addr, hp->h_addr, hp->h_length);
	sin.sin_port = htons(PORT);

	/*
	 *  Connect to that address...
	 */

	if ((val=connect(s, (struct sockaddr *) &sin, sizeof (sin))) < 0) {
		return ERROR;
	}

	/* Send first part of request */
	strcpy(buf, "POST /cgi-bin/pskcall HTTP/1.0\012");
   
        if (send(s, buf, strlen(buf),0) < 0)
    		return ERROR;

        strcpy(buf, "User_Agent: SkyView Image Selector\012");
        strcat(buf, "Content-type: application/x-www-form-urlencoded\012");
        sprintf(nbuf2, "Content-length: %d\012\012", strlen(req_str));

        strcat(buf, nbuf2);
        strcat(buf, req_str);

        /* Send request contents */
        if (send(s, buf, strlen(buf), 0) < 0)
   		return ERROR;
        /* printf("%s\n",buf); */

	/* Now get back the data */
        if (ptrans(s) < 0)
    		return ERROR;
}

char	*mo_code(string)

char	*string;

{
        /* Perform Mosaic encoding.  Only alphanumerics are
	 * unchanged.  Spaces are replaced by +.  All others by
	 * %xx where xx is the Hex code for the value.
	 * Note this routine uses a static pointer so the value
	 * should be copied before this routine is called again.
	 */
	static char buf[2048];
    	char	*s,*t;
        char	c;
    
    	s = string;
        t = buf;
        do {
	
	    c = *s;
	    *t++ = *s++;
	}
    
    	while (c != '=');
    
    	c = *s;
    	while (c) 
    	{
		if (c == ' ') *t++ = '+';
	    
	        /* Assume ASCII sequencing */
	        else if ( !  ((c >= 'a' && c <= 'z') ||
			 (c >= 'A' && c <= 'Z') ||
			 (c >= '0' && c <= '9')
			 )) {
			sprintf(t, "%%%+2x", c);
		        t += 3;
		} else 
	        {
		    *t = c;
		    t++;
		}
	    
		
	        c = *++s;
	}
    
        *t = 0;
	return buf;
}

/* Read from input until we can fill the buffer */
int getbuf(s, rbuf, qlen)

int s;
char	*rbuf;
int qlen;
{
    

	static int left=0;
        static char bigbuf[32768];
        int	n;
	int	len = qlen;
    

        while (len > left) {
	    n = recv(s, bigbuf+left, sizeof(bigbuf)-left, 0);
	    if (n <= 0) {
		if (left <= 0)
		    	return( -1);
		else {
			len = left;
			break;
		}
	    }
	    else 
	    {
		left = left + n;
	    }
	}
        memcpy(rbuf, bigbuf, len);
        left = left - len;
        if (left > 0)
#if EXT == sun
			bcopy(bigbuf+len, bigbuf, left);
#else
		        memmove(bigbuf, bigbuf+len, left);
#endif
    	return len;
}

/* Transfer data from socket to standard output
 */
int ptrans(s)

int	s;

{
	char	line[16384];
    	int 	lcnt = 0;
    	int	scaling = 0;
        char	lbuf[20];
	char	bigbuf[2880];
    	int	nlset=0;
    	char	c;
    	int	fp=1;

        /* Look for a double newline to signal the beginning of the data */
	/* Now have to skip over carriage returns ('\015') */
        while (1) {
	    if (getbuf(s, &c, 1) < 0) 
		return ERROR;
            if (c == '\015')
		if (getbuf(s, &c, 1) < 0)
		    return ERROR;

	    if (c == '\012') {
		if (nlset)
		    break;
		else
		    nlset = 1;
	    } else {
		nlset = 0;
	    }
	}
    
        if (out_file) if (*out_file) 
        {
	    if (info_req)
		fp = creat(out_file, 0644);
	    else
            {
	        fp = creat(out_file, 0644);
	    }
	    if (fp <=0) 
	    {
		printf("ERROR: Unable to open output file %s\n", out_file);
		perror("  creat");
	    }
	}
    
        while(1) {
	    lcnt=getbuf(s, line, 2880);
	    if (lcnt <= 0) break;
	    write(fp, line, lcnt);
	}
	return 0;
}

void	lowcase(s)

char	*s;

{
    
    /* This procedure converts a string, s, to lower case */
    
    char	*p;
    p = s;
    while (*p) 
    {
	if (isalpha(*p)) *p = tolower(*p);
	p++;
    }
}


