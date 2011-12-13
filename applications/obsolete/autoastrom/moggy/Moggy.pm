#   This file is part of moggy.
#
#   Copyright 2001, Council for the Central Laboratory of the Research Councils
#
#   This program is part of the Starlink Software Distribution: see
#   http://www.starlink.ac.uk
#
#   moggy is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   moggy is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with moggy; if not, write to the Free Software
#   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#
#   The General Public License is distributed along with this
#   program in the file LICENCE.
#
#   Author: Norman Gray <norman@astro.gla.ac.uk>
#   $Id$


#+
# <codegroup>
# <title>Moggy.pm
#
# <description>
#   Provides an interface to the ESO catalogues, via Skycat.
#
#   This works by providing an interface to a slave process running
#   the `moggy' server, which itself uses the Skycat library.
#
#  <authorlist>
#    <author id=ng affiliation='Starlink, Glasgow University' >Norman Gray
#  <history>
#    <change id=ng date='05-Mar-2001'>Original version
#-

package Moggy;
use strict;
use FileHandle;
use Symbol;
use IPC::Open2;
use Carp;

use lib "$ENV{AUTOASTROM_DIR}";
use autoastrom qw(dec2sex);		# for dec2sex

my $DefaultCatalogue = 'usno@eso';
my $MoggyCommand = '$AUTOASTROM_DIR/moggy';
#my $MoggyCommand = '$AUTOASTROM_DIR/test/dummy-slave.pl';
my $MoggyRCSId = '$Id$ ';
# %%VERSION%%
my $VERSION = '0.5-9';


# Declare methods
sub catname ($);
sub catconfig ($);
sub radius ($);
sub columns ($;$);
sub maxrow ($;$);
sub point ($$$);
sub otherpoint ($$$);
sub searchtype ($;$);
sub astinformation ($;$\@);
sub astdomain ($);
sub astconvert ($$$$);
sub query ($);
sub result ($);
sub resultcolumns ($);
sub resulthascolumn ($$);
sub resultnrows ($);
sub resultncols ($);
sub version ($);
sub debug ($;$);
sub status_ok ($);
sub status_continue ($);
sub status_clear ($);
sub current_status ($);
sub current_statusmessage ($);
sub failed_command ($);

# Private methods
sub start_slave_ ($);
sub stop_slave_ ($);
sub send_command_to_slave_ ($;@);
sub send_input_to_slave_ ($;\@);

#+
#  <routinename>new
#
#  <description>Create a new connection to a catalogue.
#
#  <argumentlist>
#  <parameter optional default='usno@eso'>
#    <name>catname
#    <type>string
#    <description>The name of the catalogue to use.
#  <parameter optional default='Skycat built-in default'>
#    <name>config
#    <type>URL
#    <description>An alternative configuration file.
#    See the Skycat documentation for a description of the configuration
#    file format.
#  <parameter optional default='$AUTOASTROM_DIR/moggy'>
#    <name>moggypath
#    <type>Path
#    <description>Path to Moggy program.
#-
sub new {
    my $proto = shift;
    my $catname  = ($#_ >= 0 ? $_[0] : $DefaultCatalogue);
    my $confname = ($#_ >= 1 ? $_[1] : undef);
    my $moggycmd = ($#_ >= 2 ? $_[2] : undef);
    my $class = ref($proto) || $proto;
    my $self = {};

    defined($catname) || return undef;

    # Instance variables relevant to the catalogue.  These have
    # get/set methods with the same names.
    $self->{CATNAME} = $catname;
    $self->{RADIUS} = undef;
    $self->{SEARCHTYPE} = undef;
    $self->{COLUMNS} = undef;	# type SIMPLE or ALL
    $self->{CATCONFIG} = $confname;
    $self->{MAXROW} = undef;	# Maximum number of rows to return
    $self->{POINT} = undef;	# reference to six-element array
    $self->{OTHERPOINT} = undef;# reference to six-element array
    $self->{ASTINFORMATION} = undef;
    $self->{ASTDOMAIN} = 'SKY';

    # Query result
    $self->{RESULT} = undef;	# result of query (reference to array
                                # of array references)
    $self->{NROWS} = undef;	# number of rows in query response
    $self->{NCOLS} = undef;	# number of columns in query response
    $self->{RESULTCOLUMNS} = undef; # reference to array holding column titles

    # Instance variables relevant to the state of the object
    $self->{MOGGYPID} = 0;	# PID of slave -- non-zero if the pipe is open
    $self->{SLAVEREADER} = undef;
    $self->{SLAVEWRITER} = undef;
    $self->{FAILEDCOMMAND} = undef;
    $self->{SLAVESTATUS} = "2--"; # start with OK status, so
                                  # status_ok_ returns true.
    $self->{SLAVEMESSAGE} = undef;
    $self->{MOGGYVERSION} = undef;
    # `Globals'
    $self->{_MOGGYCMD} = (defined($moggycmd) ? $moggycmd : $MoggyCommand);
    $self->{_MOGGYPMVERSION} = \$MoggyRCSId;

    bless ($self, $class);

    # set defaults, incidentally starting up the slave
    $self->send_command_to_slave_("CATCONFIG", $confname)
      if (defined($confname));
    $self->send_command_to_slave_("NAME", $catname);
    $self->columns("SIMPLE");
    $self->searchtype("RADIUS");

    return $self;
}

#+
# <routinename>catname
# <description>Returns the current catalogue name, but
#   doesn't set it.  Setting it can only be done by the first argument to the
#   new method.
# <argumentlist none>
# <returnvalue>Current catalogue name
#-
sub catname ($) {
    my $self = shift;
    return $self->{CATNAME};
}

#+
# <routinename>catconfig
# <description>Returns the current configuration file, but
#   doesn't set it.  Setting it can only be done by the optional second
#   argument to the new method.
# <argumentlist none>
# <returnvalue>Current configuration file
#-
sub catconfig ($) {
    my $self = shift;
    return $self->{CATCONFIG};
}

#+
# <routinename>radius
#
# <description>Gets or, with an argument, sets the radius for a
#   subsequent search.  Note that the argument here is in arcminutes,
#   even if the AST domain is SKY, so that point() and otherpoint()
#   are accepting arguments in pixels.  If you want to specify a
#   radius in pixels, set point() and otherpoint() (in pixels) and set
#   the search type to `radius2'.
#
# <argumentlist>
#  <parameter optional>
#    <name>radius<type>float<description>Search radius
#
# <returnvalue>The current search radius.  If the argument is given, but
#   the value cannot be changed, the radius is set to the undefined value,
#   and it is this which is returned.
#-
sub radius ($) {
    my $self = shift;
    if (@_) {
	my $rad = shift;
	$self->send_command_to_slave_("RADIUS", $rad);
	$self->{RADIUS} = ($self->status_ok() ? $rad : undef);
    }
    return $self->{RADIUS};
}

#+
# <routinename>columns
# <description>Gets or, with an argument, sets the style of columns to be
#  returned in a subsequent search.
# <argumentlist>
#  <parameter optional>
#    <name>cols<type>string<description>The string may be either `ALL'
#    or `SIMPLE' (or indeed any string accepted by the `COLUMNS' command
#    in moggy).
# <returnvalue>The current value.  If the argument is given, but
#   the value cannot be changed, the column style is set to the undefined
#   value, and it is this which is returned.
#-
sub columns ($;$) {
    my $self = shift;
    if (@_) {
	my $cols = shift;
	$self->send_command_to_slave_("COLUMNS", $cols);
	$self->{COLUMNS} = ($self->status_ok() ? $cols : undef);
    }
    return $self->{COLUMNS};
}

#+
# <routinename>maxrow
# <description>Gets or, with an argument, sets the maximum number of rows to be
#  returned in a subsequent search.
# <argumentlist>
#  <parameter optional>
#    <name>maxrow<type>integer<description>The maximum number of columns.
# <returnvalue>The current value.  If the argument is given, but
#   the value cannot be changed, the row limit is set to the undefined value,
#   and it is this which is returned.
#-
sub maxrow ($;$) {
    my $self = shift;
    if (@_) {
	my $maxrow = shift;
	$self->send_command_to_slave_("NROW", $maxrow);
	$self->{MAXROW} = ($self->status_ok() ? $maxrow : undef);
    }
    return $self->{MAXROW};
}

#+ <routinename>point
#  <description>
#
#  <p>Gets or, with an argument, sets
#   the location of the `point' used by moggy.  This point is either
#   the centre of the search, for the `RADIUS' and `RADIUS2' searches,
#   or one corner of the box for the `BOX' search.
#
#  <p>Takes two arguments.  If the current ASTDOMAIN is `SKY', then
#   the arguments may be either decimal degrees or colon-separated
#   sexagesimal angles (RA then Dec).  This method doesn't itself do
#   any checking of the values, but instead relies on moggy to
#   indicate that the values were set successfully.  If the current
#   ASTDOMAIN is not `SKY', then the arguments may indifferently be
#   decimal degrees or pixels.
#
#  <returnvalue>The current value.  If the argument is given, but
#  there is an error setting it, then the point is set to the
#  undefined value, and it is this which is returned.
#-
sub point ($$$) {
    my $self = shift;
    my @args = @_;
    foreach (@args) { s/[\s]//g; }
    if (@args) {
	if ($#args == 1) {
	    my @a = "COORD1";
	    my $badcoord = 0;
	    if ($args[0] =~ /:/ || $args[1] =~ /:/) {
		if ($self->{ASTDOMAIN} eq 'SKY') {
		    # That's OK
		    for my $i (0..1) {
			if ($args[$i] =~ /:/) {
			    push (@a, split (':',$args[$i]));
			} else {
			    # Since one of the coordinates is
			    # sexagesimal, we need to convert the
			    # other one, too.
			    my $sexco = dec2sex ($args[$i],
						 ($i==0?'ra':'dec'),
						 ':');
			    if (defined($sexco)) {
				# split on colons, removing one
				# leading zero on minutes or seconds
				push (@a, split (/:0?/,$sexco));
			    } else {
				carp "Moggy::point: invalid ".
				  ($i==0?"RA":"Dec").": ".
				    $args[$i];
				$badcoord = 1;
			    }
			}
		    }
		} else {
		    # Error -- not a SKY domain
		    carp "Moggy::point: no sexagesimal coords in domain ".
		      $self->{ASTDOMAIN}."\n";
		    $badcoord = 1;
		}
	    } else {
		# Decimal coordinates (in whatever domain).  Just add
		# to the command
		push (@a, @args);
	    }
	    if ($badcoord) {
		# Some problem earlier
		$self->{POINT} = undef;
	    } else {
		$self->send_command_to_slave_(@a);
		shift (@a);
		$self->{POINT} = ($self->status_ok() ? \@a : undef);
	    }
	} else {
	    carp "Moggy::point: Wrong number of arguments ($#args)\n";
	    $self->{POINT} = undef;
	}
    }
    return $self->{POINT};
}

#+
# <routinename>otherpoint
# <description>
#
#  <p>Gets or, with an argument, sets the location of the
#   `otherpoint' used by moggy.  In the case of the `RADIUS' search, this
#   point is not used; for the `RADIUS2' search, the search radius is
#   taken to be the distance between `point' and `otherpoint'; and for the
#   `BOX' search, this specifies the other corner of the search box.
#
#  <p>Takes two arguments.  If the current ASTDOMAIN is `SKY', then
#   the arguments may be either decimal degrees or colon-separated
#   sexagesimal angles (RA then Dec).  This method doesn't itself do
#   any checking of the values, but instead relies on moggy to
#   indicate that the values were set successfully.
#
# <returnvalue>The current value.  If the argument is given, but there is
#   an error setting it, then the point is set to the undefined value, and
#   it is this which is returned.
#-
sub otherpoint ($$$) {
    my $self = shift;
    my @args = @_;
    foreach (@args) { s/[\s]//g; }
    if (@args) {
	if ($#args == 1) {
	    my @a = "COORD2";
	    my $badcoord = 0;
	    if ($args[0] =~ /:/ || $args[1] =~ /:/) {
		if ($self->{ASTDOMAIN} eq 'SKY') {
		    # That's OK
		    for my $i (0..1) {
			if ($args[$i] =~ /:/) {
			    push (@a, split (':',$args[$i]));
			} else {
			    # Since one of the coordinates is
			    # sexagesimal, we need to convert the
			    # other one, too.
			    my $sexco = dec2sex ($args[$i],
						 ($i==0?'ra':'dec'));
			    if (defined($sexco)) {
				push (@a, split (' ',$sexco));
			    } else {
				carp "Moggy::otherpoint: invalid ".
				  ($i==0?"RA":"Dec").": ".
				    $args[$i];
				$badcoord = 1;
			    }
			}
		    }
		} else {
		    # Error -- not a SKY domain
		    carp "Moggy::otherpoint: no sexagesimal coords in domain ".
		      $self->{ASTDOMAIN}."\n";
		    $badcoord = 1;
		}
	    } else {
		# Decimal coordinates (in whatever domain).  Just add
		# to the command
		push (@a, @args);
	    }
	    if ($badcoord) {
		# Some problem earlier
		$self->{OTHERPOINT} = undef;
	    } else {
		$self->send_command_to_slave_(@a);
		shift (@a);
		$self->{OTHERPOINT} = ($self->status_ok() ? \@a : undef);
	    }
	} else {
	    carp "Moggy::otherpoint: Wrong number of arguments ($#args)\n";
	    $self->{OTHERPOINT} = undef;
	}
    }
    return $self->{OTHERPOINT};
}

#+
# <routinename>searchtype
#
# <description>Gets or, with an argument, sets the searchtype.  If the
#   argument is not one of `RADIUS', `RADIUS2' or `BOX', then it is
#   set to be undefined.  Note that, because of the way that
#   individual catalogue handlers implement this, it is not actually
#   guaranteed that the returned results will be in the specified
#   area, so if it matters, the results should be checked by the
#   caller, or otherwise post-processed.
#
# <argumentlist>
#   <parameter optional><name>type<type>string<description>One of
#     `RADIUS', `RADIUS2' or `BOX' (case-insensitive).
#
# <returnvalue>The current value.  If the argument is given, but there is
#   an error setting it, then the value is set to the undefined value, and
#   it is this which is returned.
#-
sub searchtype ($;$) {
    my $self = shift;
    my $type = (@_ ? uc($_[0]) : undef);

    if (defined($type)) {
	if ($type =~ /RADIUS|RADIUS2|BOX/) {
	    $self->send_command_to_slave_ ("TYPE", $type);
	    if ($self->status_ok()) {
		$self->{SEARCHTYPE} = $type;
	    } else {
		$self->{SEARCHTYPE} = undef;
	    }
	} else {
	    carp "Unknown search type $type";
	    $self->{SEARCHTYPE} = undef;
	}
    }

    return $self->{SEARCHTYPE};
}

#+ <routinename>astinformation
# <description>Gets or, with an argument, sets the AST information.
# <argumentlist>
#   <parameter optional>
#     <name>domain
#     <type>string
#     <description>A string giving the domain in which later coordinates will
#       be quoted, and which is the base frame of the AST SkyFrame to follow.
#   <parameter optional>
#     <name>astarray
#     <type>array of strings
#     <description>An array of strings, each element of which represents
#       a line of WCS information as serialised by AST.
# <returnvalue>The current value, as a reference to an array of strings,
#  or undef on error.
#-
sub astinformation ($;$\@) {
    my ($self, $domain, @astarray) = @_;

    if (defined($domain)) {
	if (@astarray) {
	    $domain = uc($domain);
	    $self->send_command_to_slave_ ("AST FRAMESET", $domain);

	    unless ($self->status_continue()) {
		carp "AST FRAMESET failed: ".$self->current_statusmessage();
		return undef;
	    }

	    $self->send_input_to_slave_ (@astarray);

	    unless ($self->status_ok()) {
		carp "Can't sent AST information to slave: ".
		  $self->current_statusmessage();
		return undef;
	    }

	    $self->{ASTINFORMATION} = \@astarray;
	    $self->{ASTDOMAIN} = $domain;
	} else {
	    # Must have two arguments or none
	    carp "Moggy::astinformation: Wrong number of arguments";
	    return undef;
	}
    }

    return $self->{ASTINFORMATION};
}

#+
# <routinename>astdomain
# <description>Returns the current domain, as set by routine astinformation().
# <returnvalue type=string>Name of current domain
#-
sub astdomain ($) {
    my $self = shift;
    return $self->{ASTDOMAIN};
}

#+ <routinename>astconvert
#
# <description>Converts a coordinate pair to or from the SKY domain.
# Must follow an astinformation() method call.
#
# <argumentlist>
#   <parameter>
#     <name>arg1
#     <type>double
#     <description>If the third parameter is false, then we are
#       converting from SKY coordinates to the domain specified by the
#       AST information, and this must be a right-ascention in decimal
#       degrees.  If the third parameter is true, then we are
#       converting <em>to</em> the SKY domain, and this must be the
#       first parameter in the specified domain.
#
#   <parameter>
#     <name>arg2
#     <type>double
#     <description>As with parameter 1, but this must be the
#     declination if we are converting from SKY coordinates, and must
#     be the second domain parameter if we are going the other way.

#   <parameter>
#     <name>tosky
#     <type>boolean
#     <description>If true, then the coordinates are in the domain specified
#       by the AST information and we are to convert the coordinates
#       into the SKY domain.  If false, it's the other way around.
#
# <returnvalue type=arrayref>Reference to a 2-element array containing
# the converted coordinates.  Return undef on error.
#-
sub astconvert ($$$$) {
    my $self = shift;
    my ($arg1, $arg2, $tosky) = @_;

    defined($tosky) || do {
	carp "Moggy::astconvert: wrong number of arguments";
	return undef; # wrong number of arguments
    };
    $self->send_command_to_slave_ ("ast convert", $arg1, $arg2,
				   ($tosky ? "tosky" : "fromsky"));

    $self->status_ok() || do {
	carp "Moggy::astconvert: command failed: ".
	  $self->current_statusmessage();
	return undef;
    };

    # The response from this is a single further line containing two
    # doubles.  Construct an array containing these two doubles, and
    # return a reference to it.
    my $RDR = $self->{SLAVEREADER};
    my $line = <$RDR>;
    my @vals = split (' ', $line);
    # Check that there are precisely two values in the line.
    defined($vals[1]) || do {
	carp "Moggy::astconvert: unexpected response from moggy <$line>";
	return undef;
    };
    if ($#vals > 1) {
	# silently truncate
	$#vals = 1;
    }
    return \@vals;
}

#+
# <routinename>query
# <description>Perform the query which has been set up by calls to other
#   methods.  The query results in a status
#   response indicating success or failure.
#   If the response is a failure status, it might be due to
#   us not having specified enough in the query.
#   <p>If successful, The results can be retrieved by subsequent calls to
#   the `result()', `resultcolumns()', `resultnrows()' and `resultncols()'
#   methods.
# <argumentlist none>
# <returnvalue>1 on success, 0 on failure.
#-
sub query ($) {
    my $self = shift;

    # Don't do any consistency checking here, since moggy can do that more
    # reliably, and return an error if there's a problem.
    $self->send_command_to_slave_ ("SEARCH");

    $self->status_ok() || return 0;

    # If it's a success
    # status, then the format of the rest of the response is: (i) one line
    # giving the number of columns; (ii) that number of lines of column
    # names; (iii) one line giving the number of rows returned; (iv) that
    # number of rows of the given number of columns, with the columns
    # whitespace-separated.
    my $RDR = $self->{SLAVEREADER};
    my $ncols = <$RDR>;		# read number of columns
    (defined($ncols)) || return 0; # failure status
    $ncols =~ s/\s*//g;
    $self->{NCOLS} = $ncols;
    my $columnlist = [];
    my $tmp;
    while ($ncols > 0) {
	($tmp = <$RDR>) =~ s/\s*$//;
	push (@$columnlist, $tmp);
	$ncols--;
    }
    $self->{RESULTCOLUMNS} = $columnlist;

    my $nrows = <$RDR>;		# read number of rows
    $nrows =~ s/\s*//g;
    $self->{NROWS} = $nrows;
    my $rowlist = [];
    while ($nrows > 0) {
	# NB difference between `my @row' here, and `my @row' outside
	# the loop.  The former creates a _new_ array each time round,
	# the latter reuses the same array, losing the results of
	# previous times round.
	my @row = split(' ',<$RDR>);
	push (@$rowlist, \@row);
	$nrows--;
    }
    $self->{RESULT} = $rowlist;

    return 1;			# successful return
}

#+
# <routinename>result
# <description>Returns an array containing the result of a successful query.
# <argumentlist none>
# <returnvalue>A reference to an array (of size `resultnrows()') of references
#   to arrays (of size `resultncols()') containing the result of the query.
#   This may be used by, for example,
#   <verbatim>
#     $resref = $cat->result();
#     $colref = $cat->resultcolumns();
#     $ncols = $cat->resultncols();
#     $nrows = $cat->resultnrows();
#     print "  $nrows x $ncols:";
#     for ($i = 0; $i<=$#{$colref}; $i++) {
#         print " <${$colref}[$i]>";
#     }
#     print "\n";
#     for ($i = 0; $i<$nrows; $i++) {
#         for ($j=0; $j<$ncols; $j++) {
#             print "    ", $resref->[$i]->[$j];
#         }
#         print "\n";
#     }
#   </verbatim>
#-
sub result ($) {
    my $self = shift;
    return $self->{RESULT};
}

#+
# <routinename>resultcolumns
# <description>Returns an array containing the names of the result columns.
# <argumentlist none>
# <returnvalue>A reference to an array of strings containing the column names.
#-
sub resultcolumns ($) {
    my $self = shift;
    return $self->{RESULTCOLUMNS};
}

#+
# <routinename>resulthascolumn
# <description>Does the result have a column with the specified name?
# <argumentlist>
#   <parameter>
#     <name>colname
#     <type>string
#     <description>Name of a column, matched case-insensitively.
#       If the first character of the column
#       name is a slash, then it and any trailing slash, is removed, and
#       the string is interpreted as a regular expression.
# <returnvalue type=integer>
#   If the column is present, returns the (zero-offset) column number
#   corresponding to that column.  If the column is not present, or
#   if no successful query has been made, returns negative.
#-
sub resulthascolumn ($$) {
    my $self = shift;
    my $colname = shift;

    defined($colname) || return -1;
    $colname = lc($colname);
    if ($colname =~ m{^/}) {
	$colname =~ s+^/++;
	$colname =~ s+/$++;
    } else {
	$colname = "^$colname\$"; # '^'.$colname.'$';
    }

    my $cols = $self->{RESULTCOLUMNS};
    defined ($cols) || return -1;

    my $i;
    for ($i=0; $i <= $#{$cols}; $i++) {
	if (lc(${$cols}[$i]) =~ $colname) {
	    return $i;
	}
    }
    return -1;
}

#+
# <routinename>resultnrows
# <description>The number of rows in the query response.
# <argumentlist none>
# <returnvalue>An integer, giving the number of rows in the query response.
#-
sub resultnrows ($) {
    my $self = shift;
    return $self->{NROWS};
}

#+
# <routinename>resultncols
# <description>The number of columns in the query response.
# <argumentlist none>
# <returnvalue>An integer, giving the number of columns in the query response.
#-
sub resultncols ($) {
    my $self = shift;
    return $self->{NCOLS};
}

#+
# <routinename>version
# <description>Shows version information, of both moggy and this interface.
# <argumentlist none>
# <returnvalue>A string, giving version information
#-
sub version ($) {
    my $self = shift;

    $self->{MOGGYVERSION} = $self->send_command_to_slave_ ("VERSION")
      unless (defined($self->{MOGGYVERSION}));

    return "$VERSION, ${$self->{_MOGGYPMVERSION}}, moggy version $self->{MOGGYVERSION}";
}

# Debugging method -- send the debug string to moggy.  Argument is a
# sequence of keywords chosen from `moggy', `commandparse',
# `asthandler' and `cataloguehandler', separated by non-characters.
sub debug ($;$) {
    my $self = shift;
    my $dbgstring = shift;
    my $dbgf = shift;   # may be undefined

    my %kwdtomask = ( "moggy" => 1 ,
		      "commandparse" => 2,
		      "asthandler" => 4,
		      "cataloguehandler" => 8
		      );

    if (defined($dbgstring)) {
	$dbgstring = lc($dbgstring);
	my $mask = 0;
	my $kwd;
	foreach $kwd (split (/[^a-zA-Z]+/, $dbgstring)) {
	    $mask += $kwdtomask{$kwd} if (defined($kwdtomask{$kwd}));
	}
	print STDERR "Debugging: <$dbgstring> -> $mask\n";
	$self->send_command_to_slave_("DEBUG",
                                      $mask,
                                      (defined($dbgf) ?
                                       $dbgf
                                       : "/tmp/moggylog.txt"));
    }

    return "debug";
}

# Start the slave.  Nilpotent -- if the slave is already running, then
# do nothing.  No return: slave's status can be checked by testing the
# value of {SLAVEPID}, which is non-zero if the slave is running
# (cf. stop_slave_).
sub start_slave_ ($) {
    my $self = shift;

    return if ($self->{MOGGYPID});

    $self->{SLAVEREADER} = gensym(); # get anonymous reference to a typeglob
    $self->{SLAVEWRITER} = gensym(); # ditto

    my $cmd = $self->{_MOGGYCMD};
     $self->{MOGGYPID} = open2($self->{SLAVEREADER},
 			      $self->{SLAVEWRITER},
 			      $self->{_MOGGYCMD});
}

sub stop_slave_ ($) {
    my $self = shift;
    if ($self->{MOGGYPID}) {
	$self->send_command_to_slave_ ("quit");
        waitpid($self->{MOGGYPID}, 0);
	close ($self->{SLAVEREADER});
	close ($self->{SLAVEWRITER});
	$self->{MOGGYPID} = 0;
    }
}

# Send a one-line command to the slave, and receive a one-line string
# response.  If the current status is not OK, then do nothing, and
# silently return undef.  If a command fails, then set {FAILEDCOMMAND}
# and return the message.
#
# The argument can be given as either a string or a list.  If there is
# no argument, nothing happens and the method returns undef;
#
# This means that this method may be called repeatedly, without
# checking the status.  If something goes wrong in a sequence of
# commands (as determined by the result code returned), then the
# subsequent commands will not be executed, the status and message
# operated on or returned by status_ok(), current_status(),
# current_statusmessage() and failed_command() will be those corresponding
# to the failed command, and not any subsequent ones.
sub send_command_to_slave_ ($;@) {
    my $self = shift;
    my $cmd = (@_ ? join(' ',@_) : undef);

    defined ($cmd)     || return undef;
    $self->status_ok() || return undef;

    # Start the slave if it's not already running.
    $self->{MOGGYPID} || $self->start_slave_ ();

    if ($self->{MOGGYPID}) {
	my $WTR = $self->{SLAVEWRITER};
	my $RDR = $self->{SLAVEREADER};

	print $WTR "$cmd\r\n";
	my $responseline = <$RDR>;

	if (defined ($responseline)) {
	    # Trim trailing whitespace
	    $responseline =~ s/\s*$//;
	    ($self->{SLAVESTATUS}, $self->{SLAVEMESSAGE})
	      = ($responseline =~ /^\s*(\w*)\s*(.*)/);

	    $self->status_ok() || ($self->{FAILEDCOMMAND} = $cmd);
	} else {
	    $self->{SLAVESTATUS} = "5--"; # pseudo-return-code
                                          # indicating internal error.
	    $self->{SLAVEMESSAGE} = '<no response from moggy>';
	    $self->{FAILEDCOMMAND} = $cmd;
	}
    } else {
	# Slave didn't start up properly
	$self->{SLAVESTATUS} = "5--"; # so status_ok() returns false
	$self->{SLAVEMESSAGE} = '<Unable to start slave>';
	$self->{FAILEDCOMMAND} = '<Unable to start slave>';
    }

    return $self->{SLAVEMESSAGE};
}

# Send a multi-line command to the slave, and receive a one-line string
# response.  If the current status is not OK, then do nothing, and
# silently return undef.  If a command fails, then set {FAILEDCOMMAND}
# and return the message.
#
# The argument must be a reference to an array of strings.  If there is
# no argument, nothing happens and the method returns undef;
#
# This means that this method may be called repeatedly, without
# checking the status.  If something goes wrong in a sequence of
# commands (as determined by the result code returned), then the
# subsequent commands will not be executed, the status and message
# operated on or returned by status_ok(), current_status(),
# current_statusmessage() and failed_command() will be those corresponding
# to the failed command, and not any subsequent ones.
sub send_input_to_slave_ ($;\@) {
    my ($self, @strings) = @_;

    @strings || return undef;
    $self->status_ok() || $self->status_continue() || return undef;

    # Start the slave if it's not already running.
    $self->{MOGGYPID} || $self->start_slave_ ();

    if ($self->{MOGGYPID}) {
	my $WTR = $self->{SLAVEWRITER};
	my $RDR = $self->{SLAVEREADER};
	my $line;

	# Send the @strings array to the slave, a line at a time
	foreach $line (@strings) {
	    print $WTR "$line\r\n";
	}
	print $WTR ".\r\n";

	# ... and get a single line response
	my $responseline = <$RDR>;

	if (defined($responseline)) {
	    # Trim trailing whitespace
	    $responseline =~ s/\s*$//;
	    ($self->{SLAVESTATUS}, $self->{SLAVEMESSAGE})
	      = ($responseline =~ /^\s*(\w*)\s*(.*)/);

	    $self->status_ok() || ($self->{FAILEDCOMMAND} = '<input>');
	} else {
	    $self->{SLAVESTATUS} = "5--"; # pseudo-return-code
                                          # indicating internal error.
	    $self->{SLAVEMESSAGE} = '<no response from moggy>';
	    $self->{FAILEDCOMMAND} = '<input>';
	}
    } else {
	# Slave didn't start up properly
	$self->{SLAVESTATUS} = "5--"; # so status_ok() returns false
	$self->{SLAVEMESSAGE} = '<Unable to start slave>';
	$self->{FAILEDCOMMAND} = '<Unable to start slave>';
    }

    return $self->{SLAVEMESSAGE};
}

#+
# <routinename>status_ok
# <description>Report on the error status of the moggy program.
#   <p>You may use the parameter-setting methods to configure the
#     moggy-catalogue object, even if this status returns false, so that
#     you do not have to check the error status between each of a series
#     of method calls.  If any of the methods fail,
#     subsequent methods do nothing and do not change the status.  You can
#     obtain the current status code with the method `current_status()', and
#     obtain the corresponding message with `current_statusmessage()'.
#     If the status is good, then these correspond to the last command;
#     if the status is bad, then they correspond to the command which
#     failed, which can be examined using `failed_command()'.
#   <p>You can clear the status with a call to the `status_clear()' method.
#   <p>Status codes starting `3', generated in response to the AST command,
#     are not counted as success codes -- use status_continue() to test that.
# <argumentlist none>
# <returnvalue>True if the status is good; false if bad
#-
sub status_ok ($) {
    # Examine the current {SLAVESTATUS} and return true if the status is
    # OK -- ie, if the {SLAVESTATUS} started with `2'.
    my $self = shift;
    return ($self->{SLAVESTATUS} =~ /^2/);
}

#+
# <routinename>status_continue
# <description>Report on the error status of the moggy program.
#   <p>Much as status_ok(), except that this tests whether the current status
#     is a continue status, with a status code starting with digit three.
#   <p>You can clear the status with a call to the `status_clear()' method.
#   <p>Status codes starting `3', generated in response to the AST command,
#     are not counted as success codes -- use status_continue() to test that.
# <argumentlist none>
# <returnvalue>True if the status is good; false if bad
#-
sub status_continue ($) {
    # Examine the current {SLAVESTATUS} and return true if the slave process
    # is expecting more input -- ie, if the {SLAVESTATUS} started with `3'.
    my $self = shift;
    return ($self->{SLAVESTATUS} =~ /^3/);
}

#+
# <routinename>status_clear
# <description>Reset the error status.
# <argumentlist none>
# <returnvalue none>
#-
sub status_clear ($) {
    my $self = shift;
    $self->{FAILEDCOMMAND} = undef;
    $self->{SLAVEMESSAGE} = "<null>";
    $self->{SLAVESTATUS} = "2--";
}

#+
# <routinename>current_status
# <description>Return the current error status
# <argumentlist none>
# <returnvalue>The status code returned from the last command which failed,
#   or the last command, if none have failed.
#-
sub current_status ($) {
    my $self = shift;
    return $self->{SLAVESTATUS};
}

#+
# <routinename>current_statusmessage
# <description>Return the current status message.
# <argumentlist none>
# <returnvalue>The message returned from the last command which failed,
#   or the last command, if none have failed.
#-
sub current_statusmessage ($) {
    my $self = shift;
    return $self->{SLAVEMESSAGE};
}

#+
# <routinename>failed_command
# <description>If the error status is bad, then there is a command which
#   failed, which is returned by this method.  If the status is good, then
#   there is no command which failed, and this method returns undef.
# <argumentlist none>
# <returnvalue>The unsuccessful command sent to moggy.
#-
sub failed_command ($) {
    my $self = shift;
    return $self->{FAILEDCOMMAND};
}

sub DESTROY {
    my $self = shift;
    $self->stop_slave_ ();
}

1;

__END__

=head1 NAME

Moggy -- handler for the moggy program.

=head1 DESCRIPTION

Handles connections with the moggy program.
