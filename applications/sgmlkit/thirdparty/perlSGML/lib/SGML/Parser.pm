##---------------------------------------------------------------------------##
##  File:
##      @(#) Parser.pm 1.12 97/09/15 14:58:22 @(#)
##  Author:
##      Earl Hood			ehood@medusa.acs.uci.edu
##  Description:
##      This file defines the SGML::Parser class: a basic SGML
##	instance parser.
##---------------------------------------------------------------------------##
##  Copyright (C) 1997  Earl Hood, ehood@medusa.acs.uci.edu
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
##---------------------------------------------------------------------------##

##########################################################################
##  Status of module:
##
##	o  Current supported features:
##	    -  Start tags (inluded attribute specification list)
##          -  End tags
##          -  Processing instructions
##          -  Comment declarations
##          -  PCDATA
##	    -  Marked sections (w/parameter ent ref callback method)
##	    -  CDATA/RCDATA elements (mode must be set via callback methods)
##
##	   See parse_data methods for more information.
##########################################################################

package SGML::Parser;

use vars qw(@ISA $VERSION @EXPORT @EXPORT_OK %EXPORT_TAGS);

use Exporter ();
@ISA = qw( Exporter );
$VERSION = "0.11";
@EXPORT = ();
@EXPORT_OK = ();
%EXPORT_TAGS = ();

use SGML::Util;

##########################################################################

##**********************************************************************##
##	Class Variables
##**********************************************************************##

*ModePCData	= \1;
*ModeCData	= \2;
*ModeRCData	= \3;
*ModeIgnore	= \4;
*ModeMSCData	= \5;
*ModeMSRCData	= \6;

*TypeERO	= \1;
*TypeETagO	= \2;
*TypeMDO	= \3;
*TypeMSC	= \4;
*TypePIO	= \5;
*TypeSTagO	= \6;

$namestart	= 'A-Za-z';
$namechars	= '\w\.\-';

##########################################################################

##**********************************************************************##
##	PUBLIC METHODS
##**********************************************************************##

##----------------------------------------------------------------------
##	new() constructor.
##
sub new {
    my $this = { };
    my $class = shift;

    ## Private variables
    $this->{'_input_stack'} = [ ];	# Input stack
    $this->{'_input'} = undef;		# Reference to current input info
    $this->{'_buf'} = '';		# Working buffer
    $this->{'_open_ms'} = 0;		# Number of open marked sections
    $this->{'_open_ms_ign'} = 0;	# Number of open marked sections in
					# 	ignore mared section

    ## Public variables
    $this->{'mode'} = $ModePCData;	# Parsing mode: Can be set
					# by method callbacks to control
					# recognition modes.

    bless $this, $class;
    $this;
}

##----------------------------------------------------------------------
##	parse_data() parses an SGML instance specified by either a
##	reference to a filehandle or a reference to a scalar string.
##      If a scalar string, the input will get modified.  Therefore,
##      the callar may need to make a copy before calling parse_data
##      if the original input is needed afterwards.
##
##	The delimiters defined by the reference concrete syntax are
##	assumed.
##
##	Example usage:
##
##	    $parser->parse_data(\*FILE, "file.sgm", $init_buffer_txt,
##				$line_no_start);
##
##	Only the first argument is required.  The other are optional.
##
##	The routine calls callback methods for the various events
##	that can occur.  It is up to the methods to make sense of
##      the data.
##
##	The following lists the various methods invoked during parsing:
##
##		   $this->cdata($cdata);
##		   $this->char_ref($funcname_or_charnum);
##		   $this->comment_decl(\@comments);
##		   $this->end_tag($gi);
##	    $txt = $this->entity_ref($entname);
##		   $this->ignored_data($data);
##		   $this->marked_sect_close();
##		   $this->marked_sect_open($status_keyword, $status_spec);
##	    $txt = $this->parm_entity_ref($entname);
##		   $this->processing_inst($pidata);
##		   $this->start_tag($gi, $attr_spec);
##
##		   $this->error($message);
##
##	The entity reference methods can return a string.  If so,
##	it is prepended to the current buffer for parsing.  Return
##	the empty string, or undef, if no text should be parsed.
##
##	These methods should be redefined by subclasses to perform
##	whatever parsing tasks are required.
##
sub parse_data {

    my $this = shift;		# Self reference
    my $href = { };

    my $in = shift;		# Input (filehandle or a string reference)
    $href->{'_label'} = shift;	# Input label (Optional)
    my $buf = shift || '';	# Initial buffer (Optional)
    $href->{'_ln'} = shift || 0;# Starting line number (Optional).

    my($before, $after, $type, $tmp);
    my($m1, $gi, $name);

    ## Set values for subsequent calls to _get_line()
    if (ref($in) eq 'SCALAR') {
	$href->{'_string'} = $in;
	$href->{'_fh'} = undef;
    } else {
	$href->{'_string'} = undef;
	$href->{'_fh'} = $in;
    }
    push(@{$this->{'_input_stack'}}, $this->{'_input'})
	if $this->{'_input'};
    $this->{'_input'} = $href;

    $this->{'mode'} = $ModePCData;

    # Eval code to capture die's
    eval {

	## Parse input
	LOOP: while (defined($buf)) {

	    # Fill working buffer if empty
	    if ($buf eq '') {
		last LOOP  unless defined($buf = $this->_get_line());
	    }

	    #--------------------------------------------------------------
	    # Check for markup.  Choose match that occurs earliest in
	    # string.
	    #--------------------------------------------------------------

	    ($before, $after, $type, $m1) = (undef,'','','');

	    # Pcdata mode checks
	    if ($this->{'mode'} == $ModePCData) {
		if ($buf =~ m@<([!?/>$namestart])@o) {
		    $before = $`;  $m1 = $1;  $after = $';
		    BLK: {
			if ($m1 eq '!') { $type = $TypeMDO;  last BLK; }
			if ($m1 eq '?') { $type = $TypePIO;  last BLK; }
			if ($m1 eq '/') { $type = $TypeETagO;  last BLK; }
			if ($m1 eq '>') { $type = $TypeSTagO;  last BLK; }
			$type = $TypeSTagO;
		    }
		}
	    }

	    # Check for entity reference
	    if ($this->{'mode'} == $ModePCData or
		$this->{'mode'} == $ModeRCData or
		$this->{'mode'} == $ModeMSRCData) {

		if ($buf =~ m@\&([#$namestart])@o) {
		    if (!defined($before) or length($before) > length($`)) {
			$before = $`;  $m1 = $1;  $after = $';
			$type = $TypeERO;
		    }
		}
	    }

	    # Check for cdata mode
	    if ($this->{'mode'} == $ModeCData) {
		if ($buf =~ m|<(/)|) {
		    if (!defined($before) or length($before) > length($`)) {
			$before = $`;  $m1 = $1;  $after = $';
			$type = $TypeETagO;
		    }
		}
	    }

	    # Check for marked section close
	    if ($this->{'mode'} != $ModeCData) {

		if ($buf =~ m|\]\]>|) {
		    if (!defined($before) or length($before) > length($`)) {
			$before = $`;  $after = $';
			$type = $TypeMSC;
		    }
		}
	    }

	    # Check for marked section opens while ignoring
	    if ($this->{'mode'} == $ModeIgnore) {
		if ($buf =~ m|<!\[|) {
		    if ($type == $TypeMSC and length($before) > length($`)) {
			$this->{'_open_ms_ign'}++;
		    }
		}
	    }

	    #--------------------------------------------------------------
	    # Now, check what the type is and process accordingly.
	    #--------------------------------------------------------------

	    ## Invoke cdata callback if any before text -------------------
	    if ($before ne '') {
		$this->{'mode'} == $ModeIgnore ?
		    $this->ignored_data($before) : $this->cdata($before);
	    }

	    ## Entity reference -------------------------------------------
	    if ($type == $TypeERO) {
		$buf = $after;
		$name = $m1;

		if ($name eq '#') {	# Character reference
		    if ($buf =~ s/^([$namechars]+);?//o) {
			$name = $1;
		    }
		    $this->char_ref($name);

		} else {		# General entity reference
		    if ($buf =~ s/^([$namechars]*);?//o) {
			$name .= $1;
		    }
		    $buf = $this->entity_ref($name) . $buf;
		}

		next LOOP;
	    }


	    ## End tag ----------------------------------------------------
	    if ($type == $TypeETagO) {
		$buf = $after;
		$gi = '';

		# Get rest of generic identifier
		if ($buf =~ s/^([$namechars]*)\s*//o) {
		    $gi = $1;
		}
		# Read up to tagc
		ETAG: while (1) {
		    if ($buf =~ />/o) { $buf = $'; last ETAG; }
		    if (!defined($buf = $this->_get_line())) {
			$this->error("Unexpected EOF; end tag not closed");
		    }
		}
		$this->end_tag($gi);
		$this->{'mode'} = $ModePCData;
		next LOOP;
	    }

	    ## Start tag --------------------------------------------------
	    if ($type == $TypeSTagO) {
		$gi = $m1;  $buf = $after;

		# Check for null start tag
		if ($gi eq '>') {
		    $this->start_tag('', '');
		    next LOOP;
		}

		# Get rest of generic identifier
		if ($buf =~ s/^([$namechars]*)//o) { $gi .= $1; }

		# Get attribute specification list and tagc
		$attr = '';
		STAG: while (1) {
		    if ($buf =~ />/o) {
			$attr .= $`;  $buf = $';
			if (!SGMLopen_lit($attr)) {
			    last STAG;
			} else {
			    $attr .= '>';
			    next;
			}
		    }
		    if (!defined($tmp = $this->_get_line())) {
			$this->error("Unexpected EOF; " .
				       "start tag not finished");
			$buf = undef;
			last STAG;
		    }
		    $buf .= $tmp;
		}
		$this->start_tag($gi, $attr);
		next LOOP;
	    }

	    ## Processing instruction -------------------------------------
	    if ($type == $TypePIO) {
		$buf = $after;
		$tmp = '';

		# Read up to tagc
		PI: while (1) {
		    if ($buf =~ />/o) { $tmp .= $`;  $buf = $'; last PI; }
		    $tmp .= $buf;
		    if (!defined($buf = $this->_get_line())) {
			$this->error("Unexpected EOF; PI not closed");
		    }
		}
		$this->processing_inst($tmp);
		next LOOP;
	    }

	    ## Marked section end -----------------------------------------
	    if ($type == $TypeMSC) {
		$buf = $after;
		if ($this->{'_open_ms'} == 0) {
		    $this->error("Mark section close w/o a " .
				   "mark section start");
		} else {
		    $this->{'_open_ms'}--;
		}

		# Check for nested ms's in ignore ms
		if ($this->{'mode'} == $ModeIgnore) {
		    $this->{'_open_ms_ign'}--	if $this->{'_open_ms_ign'};
		    next LOOP			if $this->{'_open_ms_ign'};
		}

		if ($this->{'mode'} == $ModeIgnore or
		    $this->{'mode'} == $ModeMSCData or
		    $this->{'mode'} == $ModeMSRCData) {

		    $this->{'mode'} = $ModePCData;
		}
		$this->marked_sect_close();
		next LOOP;
	    }

	    ## Markup declaration -----------------------------------------
	    if ($type == $TypeMDO) {
		$buf = $after;

		if ($buf =~ s/^\[//) {		## Marked section start
		    $tmp = '';
		    # Read up to dso
		    MSO: while (1) {
			if ($buf =~ /\[/o) {
			    $tmp .= $`;  $buf = $'; last MSO;
			}
			$tmp .= $buf;
			if (!defined($buf = $this->_get_line())) {
			    $this->error("Unexpected EOF for " .
					   "marked section start");
			}
		    }

		    if ($tmp =~ /%([$namechars])/o) {
			$keyword = $this->parm_entity_ref($1);
		    } else {
			($keyword = $tmp) =~ s/\s//g;
		    }
		    $keyword = uc $keyword;
		    $this->marked_sect_open($keyword, $tmp);

		    if ($keyword eq "IGNORE") {
			$this->{'mode'} = $ModeIgnore;
		    } elsif ($keyword eq "RCDATA") {
			$this->{'mode'} = $ModeMSRCData;
		    } elsif ($keyword eq "CDATA") {
			$this->{'mode'} = $ModeMSCData;
		    } else {
			$this->{'_open_ms'}++;
		    }
		    next LOOP;

		} # end marked section open

		if ($buf =~ s/^--//) {		## Comment declaration
		    my(@comms) = ();
		    # Outer loop for comment declaration as a whole
		    COMDCL: while (1) {
			$tmp = '';
			# Inner loop for each comment block in declaration
			COMM: while (1) {
			    if ($buf =~ /--/o) {
				$tmp .= $`;  $buf = $'; last COMM;
			    }
			    $tmp .= $buf;
			    if (!defined($buf = $this->_get_line())) {
				$this->error("Unexpected EOF; " .
					       "Comment not closed");
				last COMM;
			    }
			}
			# Push comment block on list
			push(@comms, $tmp);
			last COMM  unless defined($buf);

			# Check for declaration close or another comment block
			while ($buf !~ /\S/) {
			    if (!defined($buf = $this->_get_line())) {
				$this->error("Unexpected EOF; " .
					       "Comment declaration " .
					       "not closed");
				last COMDCL;
			    }
			}
			if ($buf =~ s/^\s*--//o) {
			    next COMDCL;
			} elsif ($buf =~ s/^\s*>//o) {
			    last COMDCL;
			} else {	# punt
			    $this->error("Invalid cdata outside of comment");
			    next COMDCL;
			}
		    }
		    $this->comment_decl(\@comms);

		    next LOOP;
		} # end comment

		$buf = "<!" . $buf;

	    } # end markup declaration


	    ## If not markup, invoke cdata callback -----------------------
	    $this->{'mode'} == $ModeIgnore ?
		$this->ignored_data($buf) :
		$this->cdata($buf);
	    $buf = '';
	}

    }; # End eval

    $this->{'_input'} = pop(@{$this->{'_input_stack'}});

    # Return buffer.  May contain data if parsing was aborted, otherwise
    # should be undef.
    $buf;
}

##----------------------------------------------------------------------
##	get_line_no() retrieves the current line number of the input.
##	Method useful in callback routines.
##
sub get_line_no {
    my $this = shift;
    $this->{'_input'}{'_ln'};
}

##----------------------------------------------------------------------
##	get_input_label() retrieves the label given to the input being
##	parsed.  Label is defined when the parse_data method is called.
##	Method useful in callback routines.
##
sub get_input_label {
    my $this = shift;
    $this->{'_input'}{'_label'};
}

##########################################################################

##**********************************************************************##
##	CALLBACK METHODS
##**********************************************************************##
##	Subclasses are to redefine callback methods to perform
##	whatever actions are desired.
##**********************************************************************##

sub cdata { }
sub char_ref { }
sub comment_decl { }
sub end_tag { }
sub entity_ref { undef }
sub ignored_data { }
sub marked_sect_close { }
sub marked_sect_open { }
sub parm_entity_ref { undef }
sub processing_inst { }
sub start_tag { }
sub error {
    my $this = shift;
    my $label = $this->get_input_label();
    my $line = $this->get_line_no();

    warn(ref($this), ":$label:Line $line:", @_, "\n");
}

##########################################################################

##**********************************************************************##
##	PRIVATE METHODS
##**********************************************************************##

##----------------------------------------------------------------------
##	_get_line() retrieves the next line from input.  undef is
##	returned if end of input is reached.
##
sub _get_line {
    my $this = shift;
    my $ret = undef;
    my $href = $this->{'_input'};
    my($sref, $fh);

    if (defined($fh = $href->{'_fh'})) {
	$href->{'_ln'} = $.  if defined($ret = <$fh>);

    } elsif (defined($sref = $href->{'_string'})) {
        if ($$sref =~ s%(.*?${/})%%o) {
            $ret = $1;
	    $href->{'_ln'}++;
        } elsif ($$sref ne '') {
            $ret = $$sref;
            $href->{'_string'} = undef;
	    $href->{'_ln'}++;
        } else {
            $href->{'_string'} = undef;
	}
    }
    $ret;
}

##########################################################################
1;

