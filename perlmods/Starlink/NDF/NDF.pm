package NDF;
require 5.004;

use strict;
use Carp;
use vars qw($VERSION @ISA %EXPORT_TAGS);


#grep "dat_.*(" NDF.xs | grep -v ';' | awk -F'(' '{print $1}' | grep -v '_r$' | sort | fmt -50

require Exporter;
require DynaLoader;

@ISA = qw(Exporter DynaLoader);

# Version derived from CVS repository:  '$Revision$ '

$VERSION = '1.49';

# Add the following to the 'ndf'=> associative array if you want to
# use ADAM PARAMETERS. Remove the comment field from the entries in the XS
# file:
# ndf_assoc ndf_cinp ndf_creat ndf_crep ndf_exist ndf_prop
# You will also need to link against ndf_link_adam

%EXPORT_TAGS = (
                'ndf'=>[qw/
                            ndf_acget ndf_aclen ndf_acmsg ndf_acput ndf_acre
                            ndf_aform ndf_amap ndf_annul ndf_anorm ndf_arest
                            ndf_asnrm ndf_astat ndf_astyp ndf_atype
                            ndf_aunmp ndf_bad ndf_base ndf_bb ndf_begin
                            ndf_block ndf_bound ndf_cget ndf_chunk 
                            ndf_clen ndf_clone ndf_cmplx ndf_cmsg ndf_copy
                            ndf_cput ndf_delet ndf_dim
                            ndf_end ndf_find ndf_form ndf_ftype
                            ndf_gtune ndf_happn ndf_hcre ndf_hdef ndf_hend
                            ndf_hfind ndf_hinfo ndf_hnrec ndf_hout ndf_hpurg
                            ndf_hput ndf_hsmod ndf_isacc ndf_isbas ndf_istmp
                            ndf_loc ndf_map ndf_mapql ndf_mapz ndf_mbad
                            ndf_mbadn ndf_mbnd ndf_mbndn ndf_msg ndf_mtype
                            ndf_mtypn ndf_nbloc ndf_nchnk ndf_new ndf_newp
                            ndf_noacc ndf_open ndf_place ndf_qmask ndf_qmf
                            ndf_reset ndf_same ndf_sbad ndf_sbb ndf_sbnd
                            ndf_scopy ndf_sect ndf_shift ndf_size ndf_sqmf
                            ndf_ssary ndf_state ndf_stype ndf_temp ndf_tune
                            ndf_type ndf_unmap ndf_valid ndf_xdel ndf_xgt0c
                            ndf_xgt0d ndf_xgt0i ndf_xgt0l ndf_xgt0r ndf_xiary
                            ndf_xloc ndf_xname ndf_xnew ndf_xnumb ndf_xpt0c
                            ndf_xpt0d ndf_xpt0i ndf_xpt0l ndf_xpt0r ndf_xstat
                            ndfGtwcs ndfPtwcs ndf_hsdat
                          /],

                'ary'=>[qw/ary_annul ary_dim ary_find ary_map ary_ndim
                           ary_size ary_unmap/],

                'msg'=>[qw/msg_bell msg_blank msg_fmtc msg_fmtd msg_fmti
                           msg_fmtl msg_fmtr msg_iflev msg_ifset msg_load
                           msg_out msg_outif msg_renew msg_setc msg_setd
                           msg_seti msg_setl msg_setr msg_tune/],

                'err'=>[qw/err_annul err_begin err_end err_facer err_fioer
                           err_flbel err_flush err_level err_load err_mark
                           err_rep err_rlse err_stat err_syser err_tune
                           err_flush_to_string
                          /],
                #ADAM only: err_clear err_start err_stop

                'hds'=>[qw/hds_copy hds_erase hds_ewild hds_flush hds_free
                           hds_group hds_gtune hds_link hds_lock hds_new
                           hds_open hds_show hds_state hds_stop hds_trace
                           hds_tune hds_wild
                          /],

                'dat'=>[qw/dat_alter dat_annul dat_basic dat_ccopy dat_cctyp
                           dat_cell dat_clen dat_clone dat_coerc dat_copy
                           dat_drep dat_erase dat_ermsg dat_find dat_get0c
                           dat_get0d dat_get0i dat_get0l dat_get0r dat_get1c
                           dat_get1d dat_get1i dat_get1r dat_getvc dat_getvd
                           dat_getvi dat_getvr dat_index dat_len dat_map
                           dat_mapc dat_mapd dat_mapi dat_mapl dat_mapr
                           dat_mapv dat_mould dat_move dat_msg dat_name
                           dat_ncomp dat_new dat_new0c dat_new0d dat_new0i
                           dat_new0l dat_new0r dat_new1c dat_new1d dat_new1i
                           dat_new1l dat_new1r dat_newc dat_paren dat_prec
                           dat_prim dat_prmry dat_put0c dat_put0d dat_put0i
                           dat_put0l dat_put0r dat_put1d dat_put1i dat_put1r
                           dat_put1c dat_putc dat_putd dat_puti dat_putr dat_putvc
                           dat_putvd dat_putvi
                           dat_putvr dat_ref dat_refct dat_renam dat_reset
                           dat_retyp dat_shape dat_size dat_slice dat_state
                           dat_struc dat_temp dat_there dat_type dat_unmap
                           dat_valid dat_vec dat_where
                          /],

                'cmp'=>[qw/ cmp_get0c cmp_get0d cmp_get0i cmp_get0l cmp_get0r
                            cmp_get1c cmp_get1d cmp_get1i cmp_get1r cmp_getvc
                            cmp_getvd cmp_getvi cmp_getvr cmp_len cmp_mapv
                            cmp_mod cmp_modc cmp_prim cmp_put0c cmp_put0d
                            cmp_put0i cmp_put0l cmp_put0r cmp_put1c cmp_put1d 
                            cmp_put1i cmp_put1r 
                            cmp_putni cmp_putvc cmp_putvd cmp_putvi cmp_putvr
                            cmp_shape cmp_size cmp_struc cmp_type cmp_unmap
                          /],

                'ndg'=>[qw/ ndgReadProv
                          /],

                'misc'=>[qw/mem2string string2mem array2mem mem2array
                            par_get fits_read_header
                            fits_get_nth_item fits_get_item fits_extract_key_val
                            fits_construct_string/]
               );

Exporter::export_tags('ndf','ary','msg','err','hds','dat','cmp','ndg','misc');

# Autoload constants when required
# We do not forward anything on to AutoLoader on failure. If we do not know
# about it in here we don't want AutoLoader to start trying to dynamically load files.
sub AUTOLOAD {
  my $constname;
  our $AUTOLOAD;
  ($constname = $AUTOLOAD) =~ s/.*:://;
  croak "&NDF::constant not defined" if $constname eq 'constant';
  my ($error, $val) = constant($constname);
  if ($error) { croak $error; }
  {
    no strict 'refs';
    *$AUTOLOAD = sub { $val };
  }
  goto &$AUTOLOAD;
}

bootstrap NDF $VERSION;

# Character arrays need to be packed before being passed to C
# The int and float arrays are dealt with automatically by KGB's arrays.c

sub pack1Dchar {
  my ($arg) = shift;
  return $arg if ref(\$arg) eq "SCALAR"; # packed char string
  my $array_len = $#{$arg} + 1;          # How many members in array
  my ($maxlength, $size);
  # Find maximum length of an array member
  $maxlength = 0;
  foreach (@$arg) {
    $size = length($_);
    $maxlength = $size if $size > $maxlength; 
  }

  return ($maxlength, pack("A$maxlength" x $array_len, @$arg))
    if ref(\$arg) eq "GLOB" ||
      (ref($arg) eq "ARRAY" && ref(\$$arg[0]) eq "SCALAR");
  croak "Something has gone wrong";
}
  


# Now deal with the calls which use character arrays

sub ndf_hput ($$$$\@$$$$$) {
  croak 'Usage: ndf_hput(hmode, appn, repl, nlines, @text, trans, wrap, rjust, indf, status)' if (scalar(@_)!=10);
  my($hmode, $appn, $repl, $nlines, $text, $trans, $wrap, $rjust,
     $indf, $status) = @_;
  $nlines = ($nlines > $#{$text} + 1 ? $#{$text} + 1 : $nlines);
  ndf_hput_r($hmode, $appn, $repl, $nlines, pack1Dchar($text), $trans, 
             $wrap, $rjust, $indf, $status);
  $_[9] = $status;
}

sub dat_putc ($$\@\@$) {
  croak 'Usage: dat_putc(loc, ndim, @dim, @value, status)' if (scalar(@_)!=5);
  my ($loc, $ndim, $dim, $value, $status) = @_;
  dat_putc_r($loc, $ndim, $dim, pack1Dchar($value), $status);
  $_[4] = $status;
}

sub dat_put1c ($$\@$) {
  croak 'Usage: dat_put1c(loc, el, @value, status)' if (scalar(@_)!=4);
  my ($loc, $el, $value, $status) = @_;
  dat_put1c_r($loc, $el, pack1Dchar($value), $status);
  $_[3] = $status;
}

sub dat_putvc ($$\@$) {
  croak 'Usage: dat_putvc(loc, el, @value, status)' if (scalar(@_)!=4);
  my ($loc, $el, $value, $status) = @_;
  dat_putvc_r($loc, $el, pack1Dchar($value), $status);
  $_[3] = $status;
}

sub cmp_put1c ($$$\@$) {
  croak 'Usage: cmp_putvc(loc, name, el, @value, status)' if (scalar(@_)!=5);
  my ($loc, $name, $el, $value, $status) = @_;
  cmp_put1c_r($loc, $name, $el, pack1Dchar($value), $status);
  $_[4] = $status;
}

sub cmp_putvc ($$$\@$) {
  croak 'Usage: cmp_putvc(loc, name, el, @value, status)' if (scalar(@_)!=5);
  my ($loc, $name, $el, $value, $status) = @_;
  cmp_putvc_r($loc, $name, $el, pack1Dchar($value), $status);
  $_[4] = $status;
}

# Add routines which deal with bytes

# NDF_QMASK(QUAL, BADBIT)

sub ndf_qmask ($$) {
  croak 'Usage: ndf_qmask(qual, badbit)' if (scalar(@_)!=2);
  return(1) if (($_[0] & $_[1]) == 0);
  return(0);
}


# Add routines to simplify array mapping

# Map an array to a memory pointer
#   INPUT: ARRAY, Pack type, memory address of mapped array
#   To map arrays need to
#       1.  Pack the array into a string
#       2.  Find out byte size of int, float etc
#       3.  Copy this string to the memory location with string2mem

sub array2mem  (\@$$) {
  my ($ref, $packtype, $pntr) = @_;

  string2mem(pack($packtype, @$ref), &byte_size($packtype) * ($#{$ref}+1), 
             $pntr);
}

# Need to know the size of the array to map it from the pointer to perl
#  INPUT: pointer to mapped memory, pack type, array, size of array

sub mem2array ($$$) {
  my ($pntr, $packtype, $nel) = @_;
  my $mapped;

  mem2string($pntr, &byte_size($packtype) * $nel, $mapped);
  return unpack($packtype, $mapped);

}

# AST wrappers
#  $frameset = ndfGtwcs( $indf, $status );
sub ndfGtwcs {
  require Starlink::AST;
  my $buffer = ndfGtwcs_( $_[0], $_[1]);
  return undef unless $_[1] == &NDF::SAI__OK;
  my @strings = split(/\n/, $buffer);
  my $chan = new Starlink::AST::Channel( 
                                        source => sub { 
                                          return shift(@strings); 
                                        }
                                       );
  return $chan->Read();
}

# ndfPtwcs( $wcs, $indf, $status )
sub ndfPtwcs {
  require Starlink::AST;
  my $wcs = shift;
  my @buffer;
  my $chan = new Starlink::AST::Channel( sink => sub { push(@buffer,$_[0]."\n")});
  $chan->Write( $wcs );
  ndfPtwcs_( \@buffer, $_[0], $_[1]);
}

# Now put in some general PERL sub routines

#+
#  Name:
#    par_get

#  Purpose:
#    To find the values of parameters stored in $ADAM_USER

#  Type of module:
#    Perl 5 script

#  Description:
#    This procedure is a perl5 implementation of Kappa PARGET.
#    The current value of a parameter associated with a Starlink application
#    is returned.
#
#    For monolithic A-tasks an application name of monolith.task
#    is supported.

#  Usage:
#    @values = par_get($par, $file, \$status)

#  Arguments:
#    PAR = scalar (Given)
#      The required parameter
#    FILE = scalar (Given)
#      The requested application
#    STATUS = scalar reference (Given & Returned)
#      Reference to the global status.
#    VALUES = array
#      The values associated with the specified parameter returned
#      in an array context.

#  Example:
#     @values = par_get("nsigma", "drawsig", \$status);
#        Returns the value of the NSIGMA parameter (an array)
#     ($mean) = par_get("mean","kappa_mon.stats", $status);
#        Returns the mean value from the STATS task of the kappa_mon
#        A-task.

#  Prior requirements:
#    - The NDF perl module must exist (but since this is in the NDF module
#      statement is not very helpful).
#    - Either the ADAM_USER or HOME environment variables must be set
#      so that the location of the parameter files can be determined.
#-

sub par_get ($$$) {

  croak 'Usage: par_get(param, application, status)' if (scalar(@_)!=3);
  my  ($parname, $pkg, $status, $path, $filename, $tempstat, $prim,
       $ploc, $loc, $there, $loco, $type, $locs, @cvalues, $el, $size);

  my $SAI__OK = &NDF::SAI__OK;

  ($parname, $pkg, $status) = @_;

  # Just in case a reference isn't passed
  if (!ref($status)) {
    carp "Warning! STATUS will not be returned unless you pass the reference\n";
    $tempstat = $status;
    $status = \$tempstat;

  } elsif (ref($status) ne "SCALAR") {
    carp "ref($status) is not a valid type for STATUS\n";
    return;
  }

  return if $$status != $SAI__OK;

  # Find the location of the SDF files
  # Should be in $ADAM_USER or $HOME/adam

  if (exists $ENV{'ADAM_USER'}) {
    $path = $ENV{'ADAM_USER'};
  } elsif (exists $ENV{'HOME'}) {
    $path = $ENV{'HOME'} . '/adam';
  } else {
    $$status = &NDF::SAI__ERROR;
    err_rep('PARGET_NOPARS','Could not find the location of the ADAM parameter files. Neither ADAM_USER nor HOME were set.',$$status);
    return;
  }

  # Now we need to go through and check that the package name does
  # not include dots (since HDS_OPEN can not handle that)
  # .sdf should not be here either

  $pkg =~ s/\.sdf//;

  # Extract root
  my ($root, @path) = split(/\./,$pkg);

  $filename = "$path/$root";

  # Open the file - $loc is the parent locator
  hds_open($filename, 'READ', $ploc, $$status);
  return if $$status != $SAI__OK;

  # Allow one extra level of nesting (rather than abritrary
  # recursion). Monolith parameter files store parameters for 
  # all actions/tasks in a single file. This means that the 
  # stats parameters are in kappa_mon.stats rather than simply
  # kappa_mon.sdf

  if ($#path > -1) {
    # Look for the task name
    dat_there($ploc, $path[0], $there, $$status);

    if (! $there && $$status == $SAI__OK) {
      $$status = &NDF::SAI__ERROR;
      msg_setc('PAR', $path[0]);
      msg_setc('PATH', $filename);
      err_rep("PARGET_NOTASK", "There is no task \"^PAR\" in monolith file ^PATH", $$status);
    }

    dat_find($ploc, $path[0], $loc, $$status);

    if ($#path > 0 && $$status == $SAI__OK) {
      carp 'Only one level of HDS nesting is supported'
        if $^W;
    }

  } else {
    # Okay this is assumed to be a standard file
    # so just clone the parent locator
    dat_clone($ploc, $loc, $$status);
  }

  # Find the object
  dat_there($loc, $parname, $there, $$status);

  if (! $there && $$status == $SAI__OK) {
    $$status = &NDF::SAI__ERROR;
    msg_setc('PAR', $parname);
    msg_setc('PATH', $filename);
    err_rep("PARGET_NOOBJ", "There is no parameter \"^PAR\" in file ^PATH", $$status);
  }

  # Obtain a locator to the desired value from the primitive object

  dat_find($loc, $parname, $loco, $$status);
  
  # Find out if the object is primitive
  dat_prim($loco, $prim, $$status);

  if (! $prim) {
    dat_annul($loco, $$status);
    dat_find($loc, $parname, $locs, $$status);
    dat_type($locs, $type, $$status);

    if ($type ne "ADAM_PARNAME" && $$status == $SAI__OK) {
      $$status = &NDF::SAI__ERROR;
      msg_setc('PAR', $parname);
      msg_setc('PATH', $filename);
      err_rep('PARGET_NOOBJ', 'Object ^PAR in file ^PATH is an arbitrary structure.', $$status);
      dat_annul($locs, $$status);
      dat_annul($loc, $$status);
      dat_annul($ploc, $$status);
      return;
    } else {

      dat_find($locs, 'NAMEPTR', $loco, $$status);

    }
    dat_annul($locs, $$status);
  }

  # Find the number of elements associated with the object
  dat_size($loco, $size, $$status);

  # Get the values as a character array
  #  - For double precision values we need to use dat_getvd here
  #    so first need to check for this
  my $data_type;
  dat_type($loco, $data_type, $$status);

  if ($data_type eq '_DOUBLE') {
    dat_getvd($loco, $size, \@cvalues, $el, $$status);
  } else {
    dat_getvc($loco, $size, \@cvalues, $el, $$status); # Need to pass reference
                                                       # when in module
  }

  # Finish off

  dat_annul($loco, $$status) if defined $loco;
  dat_annul($loc, $$status) if defined $loc;
  dat_annul($ploc, $$status) if defined $ploc;

  return (@cvalues) if $$status == $SAI__OK;
  return undef;
}

#-----------------------------------------------------------------------

######### F I T S #########

# Routine to read the fits header from an NDF
#
#  Input arguments: NDF name (no sdf)
#  Returns:         Reference to %header, The starlink status

sub fits_read_header ($) {

  #  Usage: ($hashref, $status) = fits_read_header($file)

  # Variable declarations
  my ($indf, %fitsitem, $status);
  my $task = "NDF::fits_read_header";

  my $file = shift;

  # Strip trailing .sdf if one is present
  # File is the first thing before a .
  $file =~ s/\.sdf$//;

  # Setup good status
  my $good = $status = &SAI__OK;

  # Begin NDF and ERR sessions
  err_begin($status);
  ndf_begin();

  # Open the file
  ndf_find(&DAT__ROOT(), $file, $indf, $status);

  if ($status == $good) {

    # Find the FITS extension
    ndf_xloc($indf, 'FITS', 'READ', my $xloc, $status);

    if ($status == $good) {

      # Variables...
      my (@dim, $ndim, $nfits, $maxdim);

      # Get the dimensions of the FITS array
      # Should only be one-dimensional
      $maxdim = 7;
      dat_shape($xloc, $maxdim, \@dim, $ndim, $status);

      if ($status == $good) {

        if ($ndim != 1) {
          $status = &SAI__ERROR;
          err_rep(' ',"$task: Dimensionality of FITS array should be 1 but is $ndim", $status);

        }

      }

      # Set the FITS array to empty
      my @fits = ();       # Note that @fits only exists in this block

      # Read the FITS extension
      dat_get1c($xloc, $dim[0], \@fits, $nfits, $status);

      # Annul the locator
      dat_annul($xloc, $status);

      # Check status and read into hash
      if ($status == $good) {

        # Could use map{} if we were not worried about null keys
        # or the END FITS tag
        for (@fits) {
          my ($item, $value, $comment) = fits_extract_key_val($_);
          next unless defined $item;
          next if $item eq 'END';
          $fitsitem{$item} = $value if defined $value;
        }

      } else {
	
        err_rep(' ',"$task: Error reading FITS array", $status);

      }

    } else {

      # Add my own message to status
      err_rep(' ', "$task: Error locating FITS extension",
              $status);

    }

    # Close the NDF identifier
    ndf_annul($indf, $status);

  } else {

    # An error message to make sure we got here
    err_rep(' ',"$task: Error opening NDF file", $status);

  }


  # End NDF and ERR
  ndf_end($status);
  err_end($status);

  # Return the hash and status
  # Note that the hash may be empty if status was bad
  return \%fitsitem, $status;

}



# Routine to extract nth FITS value and keyword from FITS array
#
#  Input:   Array reference to FITS array
#           Number of FITS item in array

#  Return:  Keyword and value and comment

sub fits_get_nth_item (\@$) {
  return fits_extract_key_val($_[0]->[$_[1]]);
}

# Routine to find FITS value corresponding to given keyword
#
# INPUT:  Array reference to FITS array
#         Keyword

# OUTPUT: Value
#     'NOT FOUND' is returned if the key can not be found

sub fits_get_item (\@$) {

  my ($fitsref, $keyword) = @_;
  my ($name, $value, $nfits, $comment);
  my (@results) = ();

  # Look at each member of the FITS array
  (@results) = grep(/^$keyword/i,@$fitsref);

  ($#results > -1) && do {

    ($name, $value, $comment) = fits_extract_key_val($results[0]);

  } || ($value = "NOT FOUND");

  return($value);
}

# Routine to extract a value and keyword from a FITS-like string
# Fits standard specifies
# Characters 1:8  KEYWORD (trailing spaces)  COMMENT is special
#            9:10 "= "  for a valid value (unless COMMENT keyword)
#            11:80 The Value   "/" used to indicate a comment

# The value can contain:
#  STRINGS:
#      '  starting at position 12
#      A single quote represented as ''
#      Closing quote must be at position 20 or greater (max 80)
#      Trailing blanks are removed. Leading spaces in the quotes
#      are significant
#  LOGICAL
#      T or F in column 30. Translated to 1 or 0
#  Numbers
#      D is an allowed exponent as well as E

# This all means that a quick pattern match is not good enough

# END is a special case and returns undef,undef

sub fits_extract_key_val ($) {

  # Value is only present if an = is found in position 9
  my ($value, $comment) = ('', '');
  my $keyword = uc(substr($_[0], 0, 8));
  $keyword =~ s/\s+$//;

  return ("END",undef,undef) if ($keyword eq 'END');
  return (undef, undef, undef) if length($_[0]) == 0;

  # Check for comment or HISTORY
  if ($keyword eq 'COMMENT' || $keyword eq 'HISTORY' ||
      substr($_[0],8,2) ne "= ") {
    # We have comments
    $comment = substr($_[0],8);
    $comment =~ s/\s+$//;       # Trailing spaces
    $comment =~ s/^\s+\///;     # Leading spaces and slashes
    $comment =~ s/^\s+//;       # Leading space
    return ($keyword, undef, $comment);
  }

  # We must have a value after '= '
  my $rest = substr($_[0],10);

  # Remove leading spaces
  $rest =~ s/^\s+//;

  # Check to see if we have a string
  if (substr($rest,0,1) eq "'") {

    # Check for empty (null) string ''
    if (substr($rest,1,1) eq "'") {
      $value = '';
      $comment = substr($rest,2);
      $comment =~ s/^\s+\///; # Delete everything before the first slash

    } else {
      # '' needs to be treated as an escaped ' when inside the string
      # Use index to search for an isolated single quote
      my $pos = 1;
      my $end = -1;
      while ($pos = index $rest, "'", $pos) {
        last if $pos == -1;     # could not find a close quote

        # Check for the position after this and if it is a '
        # increment and loop again
        if (substr($rest, $pos+1, 1) eq "'") {
          $pos += 2;            # Skip past next one
          next;
        }

        # Isolated ' so this is the end of the string
        $end = $pos;
        last;

      }

      # At this point we should have the end of the string or the
      # position of the last quote
      if ($end != -1) {

        # Value
        $value = substr($rest,1, $pos-1);

        # Replace '' with '
        $value =~ s/''/'/;      #;

        # Special case a blank string
        if ($value =~ /^\s+$/) {
          $value = " ";
        } else {
          # Trim
          $value =~ s/\s+$//;
        }

        # Comment
        $comment = substr($rest,$pos+1); # Extract post string
        $comment =~ s/^\s+\///; # Delete everything before the first slash

      } else {
        # Never found the end so include all of it
        $value = substr($rest,1);
        # Trim
        $value =~ s/\s+$//;

        $comment = '';
      }

    }

  } else {
    # Non string - simply read the first thing before a slash
    my $pos = index($rest, "/");
    if ($pos == 0) {
      # No value at all
      $value  = undef;
      $comment = substr($rest, $pos+2);
    } elsif ($pos != -1) {
      # Found value and comment
      $value = substr($rest, 0, $pos-1);

      # Check for case where / is last character
      if (length($rest) > ($pos + 1)) {
        $comment = substr($rest, $pos+2);
        $comment =~ s/\s+$//;
      } else {
        $comment = undef;
      }

    } else {
      # Only found a value
      $value = $rest;
      $comment = undef;
    }

    if (defined $value) {

      # Replace D or E with and e - D is not allowed as an exponent in perl
      $value =~ tr/DE/ee/;

      # Check for a Logical
      $value = 1 if $value eq 'T';
      $value = 0 if $value eq 'F';

      # Remove trailing spaces
      $value =~ s/\s+$//;
    }
  }

  # Tidy up comment
  if (defined $comment) {
    if ($comment =~ /^\s+$/) {
      $comment  = ' ';
    } else {
      # Trim it 
      $comment =~ s/\s+$//;
      $comment =~ s/^\s+//;
    }
  }

  $keyword = "NONE" unless length($keyword);

  # Value is allowed to be ''
  return($keyword, $value, $comment);
}

# Routine to construct a FITS-like string from a keyword, value and comment
# Packed string is returned

# Special cases the COMMENT and HISTORY keywords
# Single quotes are escaped as ''
# Can not currently handle logicals since there is no way to distinguish
# a T from 'T' or 1. Would need an extra, optional, argument forcing
# whether the value is Logical, String, Number or Comment (ie no =)

sub fits_construct_string ($$$) {
  my ($keyword, $value, $comment) = @_;
  $keyword = uc( $keyword );    # must be upper case

  my ($fitsent);

  if ($keyword eq 'COMMENT' || $keyword eq 'HISTORY') {
    $fitsent = $keyword . "  $comment";

  } else {

    $fitsent = substr($keyword,0,8); # Key must be <= 8 characters

    # Can add the = even if the value is undefined
    # without the = sign a keyword is not really a keyword
    # but is a comment
    $fitsent .= (' 'x(8-length($fitsent))) . "= ";

    # Check that a value is there
    if (defined $value) {
	
      # Check whether we have a number or character string or nothing
      if ($value eq '') {
        $value = "''" . (" " x 18);
      } elsif ($value =~ /^(-?)(\d*)(\.?)(\d*)([EeDd][-\+]?\d+)?$/) {
        # Number (chop to 67 characters)
        $value = substr($value,0,67);
        $value = (' 'x(20-length($value))).$value;

        # Translate lower case e to upper
        # Probably should test length of exponent to decide
        # whether we should be using D instead of E
        # [depends whether the argument is stringified or not]
        $value =~ tr /ed/ED/;

      } else {
        # Character
        # Escape single quotes
        $value =~ s/'/''/g;     #';

        # chop to 65 characters
        $value = substr($value,0, 65);
        $value = "'$value'";
        $value = $value.(' 'x(20-length($value)));
      }

    } else {
      $value = " " x 20;
    }

    # Add optional comment
    if (defined $comment && length($comment) > 0) {
      $fitsent .= $value . ' / ' . $comment;
    } else {
      $fitsent .= $value;
    }

  }

  # Fix at 80 characters
  $fitsent = substr($fitsent,0,80);
  $fitsent .= ' 'x(80-length($fitsent));

  return $fitsent;
}

# err_flush_to_string

# Retrieve all the error messages on the stack and return them
# as an array after calling err_flush to clear status. Useful if
# you want to throw an exception. Returns a single string in
# list context.

#  @errors = err_flush_to_string( $status );

# $status will be good on exit.

sub err_flush_to_string {
  my ( $oplen, @errs );
  do {
    err_load( my $param, my $parlen, my $opstr, $oplen, $_[0] );
    push(@errs, $opstr) if $opstr;
  } until ( $oplen == 1 );
  err_annul($_[0]);
  return (wantarray() ? @errs : join("\n",@errs) );
}

# Helper routines for ndg

package NdgProvenancePtr;

# $keymap = $prov->GetProv( $ianc, $status );
sub GetProv {
  require Starlink::AST;
  my $buffer = $_[0]->GetProv_( $_[1], $_[2]);
  return undef unless $_[2] == &NDF::SAI__OK;
  my @strings = split(/\n/, $buffer);
  my $chan = new Starlink::AST::Channel(
                                        source => sub {
                                          return shift(@strings);
                                        }
                                       );
  return $chan->Read();
}

# $prov->ModifyProv( $ianc, $AstKeyMap, $status );
sub ModifyProv {
  require Starlink::AST;
  my $km = $_[2];
  my @buffer;
  my $chan = Starlink::AST::Channel->new( sink => sub { push(@buffer, $_[0]."\n")});
  $chan->Write( $km );
  $_[0]->ModifyProv_( $_[1], \@buffer, $_[3] );
}

1;

__END__
# This is the documentation

=head1 NAME

NDF - Perl extension for accessing Starlink N-dimensional data structures (NDFs)

=head1 SYNOPSIS

  use NDF;
  use NDF qw(:ndf :dat :ary :hds :cmp :misc :err :msg);

=head1 DESCRIPTION

This module gives access to the Starlink extensible N-dimensional data format (NDF) and related error and message handling routines. Basic hierarchical
data structure (HDS) access routines are also supplied.

The first form of calling the module imports all the function calls
into the main:: namespace. The different sub-packages are:

=over 4

=item :ndf

This imports all the NDF_ functions for use.

=item :msg

This imports all the MSG_ functions.

=item :err

This imports all the Starlink error handling functions (ERR_)

=item :ary

This imports some ARY_ functions.

=item :hds

This imports the HDS_ functions

=item :dat

This imports most of the  DAT_ functions

=item :cmp

This imports most of the CMP_ functions

=item :ndg

NDG provenance routines. The only routine exported is ndgReadProv. This
returns a provenance object and all subsequent interaction with NDG provenance
is through method calls ($prov->CountProv etc)

=item :misc

This imports routines for converting DATA pointers to arrays and routines for 
easy access to FITS arrays commonly found in NDFs. A routine for accessing
parameters stored by ADAM tasks is also available.

=back


Demo programs are available in the C<t> directory of the installation
tree.

=head2 Access to compiler constants

Compiler constants can be accessed via NDF::(constant) and are autoloaded.
e.g. SAI__OK can be accessed as &NDF::SAI__OK, as in:

C<
  S<ndf_find(&NDF::DAT__ROOT, $filename, $access, $status);
  print "An error occurred\n" if ($status != &NDF::SAI__OK);>
>

At present all the DAT__, ERR__, EMS__, MSG__ and SAI__ constants are 
available. More can be included as more C include files are created.
In the absence of the include files a request for a specific compiler
constant will be refused.

=head2 Dealing with MAPPED arrays

Routines such as ndf_map and dat_mapv return pointers to the data
arrays. These pointers are handled differently in perl than in Fortran.

To read a mapped array you must first I<copy> the data from the mapped
array into a perl array. Modifiying this perl array I<will not> change
the mapped array. The perl array must be copied back into the mapped 
array for any changes to become permanent. To reduce memory requirements
you may want to unmap the array as soon as it has been copied to the 
perl array.

In perl the array must first be packed into a single string and this
string must then be copied to the mapped memory location.
For example:

C<
  $mapped_string = pack("i*", @data);
  ndf_map($indf, 'DATA', '_CHARACTER*20', 'WRITE', $pntr, $el, $status);
  string2mem($mapped_string, 4 * $el, $pntr);
>

This maps a perl data array to the NDF DATA_ARRAY. If $el does not equal
$#data + 1 then odd things will happen. 

The reverse process can be achieved using mem2string as follows:

C<
  S<ndf_map($indf, 'DATA', '_INTEGER', 'READ', $pntr, $el, $status);>
  mem2string($pntr, 4*$el, $mapped_string);
  @data = unpack("i*", $mapped_string);
>

Note that both mem2string and string2mem require the I<number of bytes>
to be mapped. The data then has to be unpacked into an array by the
perl unpack command.

Character arrays can be dealt with as follows:

C<
  $mapped_string = pack("A20" x ($#data +1), @data);
  ndf_map($indf, 'DATA', '_CHARACTER*20', 'WRITE', $pntr, $el, $status);
  string2mem($mapped_string, 20*($#data +1), $pntr);
>

This packs an array of perl strings into a fixed length (space
padded) string of length 20 x (size of array) characters. The string is
then copied to the mapped array with string2mem.

Dealing with arrays is made easier by the array2mem and mem2array
routines, which handle the packing for the programmer. At present these
routines do not work with character arrays and the lower level
string2mem and mem2string routines must still be used.

To copy from a mapped array to a perl array you use mem2array. The
routine requires the pointer to the mapped array, the pack type (as
used in the the perl C<pack> command, i.e. "i*" for ints, "f*" for
floats) and the number of elements, and returns the array.

For example:
C<
  ndf_map($indf, 'DATA', '_INTEGER', 'READ', $pntr, $el, $status);
  @data = mem2array($pntr, "i*", $el);
>

To write to a mapped array, you use array2mem:

C<
  S<ndf_map($indf, 'DATA', '_INTEGER', 'WRITE', $pntr, $el, $status);>
  array2mem(@data, "i*", $pntr);
>

Remember that, at present, these routines I<do not> work with
_CHARACTER arrays. In that case you must use the mem2string and
string2mem routines directly.



=head2 Accessing FITS entries

FITS entries are stored in the FITS extension of an NDF. The FITS array
must be accessed by using raw HDS commands as follows:

C<
  ndf_xloc($indf, 'FITS', 'READ', $floc, $status);
  dat_get1c($floc, 200, @fits, $nfits, $status);
>

(The 200 indicates the maximum size of the array - this can be any
value greater than or equal to the actual size of the array).  The FITS
header information is now in the perl @fits array and can be accessed
via fits_get_item and fits_get_nth_item.

For example, to list all the values and keywords:


   for my $i (0..$#fits) {
    ($keyword, $value, $comment) = fits_get_nth_item(@fits, $i);
    print "$i: $keyword\t $value\n";
   }

You can also use fits_extract_key_val to access the nth entry via

C<
  ($keyword, $value, $comment) = fits_extract_key_val($fits[$n]);
>

To find out the value that goes with a keyword:

C<
  $inst = fits_get_item(@fits, 'INSTRUME');
  print "Instrument is $inst\n";
>

Remember that perl has associative arrays, such that


  %fitsitem = ();
  for ($i=0; $i <= $#fits; $i++) {
    ($keyword, $value, $comment) = fits_get_nth_item(@fits, $i);
    $fitsitem{$keyword} = $value;
  }

So that C<'print "$fitsitem{'INSTRUME'}\n";'> will print SCUBA.

A general routine exists for reading the FITS header. fits_read_header
reads the FITS extension and returns the reference to a hash array:

  ($hashref, $status) = fits_read_header($file);

The FITS entries can then be accessed as $$hashref{'ITEM'}.
This takes the filename as an argument and returns the Starlink status.
This routine ignores the FITS header END record.

=head1 NOTES

=head2 Function calls

The commands in this module use  perl prototypes to check argument 
passing. This means that arrays do not have to be referenced when being
used as arguments I<unless> the routine is called with a &.

For example, both

   $value = fits_get_item(@fits, $keyword);

and

   $value = &fits_get_item(\@fits, $keyword);

are correct but 

   $value = &fits_get_item(@fits, $keyword);

will fail because perl will pass all the members of the array and not the
reference to the array. 

=head2 Arrays

Note that the GetNx/PutNx routines have not been implemented in this
module because plain Perl does not really support N-dimensional
arrays.  All data in plain perl is vectorised.  If you wish to use
N-dimensional data structures in perl you must use the PDL module in
conjunction with a vectorised read. (see eg. the rndf command in
perldl which stores all arrays as true N-dimensional data sets). The
PDL module is available from CPAN. Also note that the Get1x/Put1x
routines are implemented but are not necessary since the vectorised
routines can always be used instead.

=head2 Accessing TYPEd data

Although routines are available for accessing data of any type (eg
int, real, double) the untyped nature of perl and the type conversion
inherent in the NDF routines mean that, in practice, only character
handling routines are needed to access data (eg via ndf_xgt0c or
dat_get1c). This does not apply to mapped data arrays where the size
of the data type must be known before unpacking. The double precision
routines may be necessary since the type conversion routines truncate
to REAL precision when converting to CHAR.


=head2 Printing History information

Whilst all the NDF history calls are implemented, NDF_HOUT has only a
minimal functionality since NDF_HOUT requires the name of a Fortran
subroutine.  Calling a perl subroutine from Fortran has not been
implemented and NDF_HECHO is called by default.

=head2 Accessing ADAM parameters from A-tasks

ADAM A-task store their parameters in HDS files in the ADAM_USER
directory (usually ~/adam). This routine retrieves the value of 
a parameter for any ADAM task that uses the ADAM_USER directory.

For example

      ($in) = par_get("data_array","GLOBAL", \$status);

returns the current data set (stored in GLOBAL.sdf). Note that the 
data is returned in an array context. The routine returns without
action if $status is not set to SAI__OK on entry. 

Either the  $ADAM_USER or $HOME environment variable must be set so that
the location of the parameter files can be determined.

For direct access to ADAM monoliths and parameters see L<Starlink::AMS::Task>.

=head2 Error messages

A helper routine called C<err_flush_to_string> can be called to retrieve
all the error messages on the stack followed with a call to err_flush to
reset status. This is useful when you wish to throw an exception with
the relevant message.

  @errors = err_flush_to_string( $status );
  $errstr  = err_flush_to_string( $status );

$status will be cleared.

=head1 Implemented routines

The following routines are available from this module:

=over 6

=item :ndf

All ndf_ routines are implemented except those dealing with ADAM parameters
(eg ndf_assoc). 

=item :ary

ary_annul ary_dim ary_find ary_map ary_ndim ary_size ary_unmap

=item :msg

All msg_ routines are implemented:
msg_bell msg_blank msg_fmtc msg_fmtd msg_fmti msg_fmtl msg_fmtr
msg_iflev msg_ifset msg_load msg_out msg_outif msg_renew msg_setc
msg_setd msg_seti msg_setl msg_setr

=item :err

All err_ routines are implemented:
err_annul err_begin err_end err_facer err_fioer err_flbel err_flush
err_level err_load err_mark err_rep err_rlse err_stat err_syser

=item :hds

All hds_ routines are implemented:
hds_copy hds_erase hds_ewild hds_flush hds_free hds_group hds_gtune
hds_link hds_lock hds_new hds_open hds_show hds_state hds_stop
hds_trace hds_tune hds_wild

=item :dat

dat_alter dat_annul dat_basic dat_ccopy dat_cctyp dat_cell dat_clen
dat_clone dat_coerc dat_copy dat_drep dat_erase dat_ermsg dat_find
dat_get0c dat_get0d dat_get0i dat_get0l dat_get0r dat_get1c dat_get1d
dat_get1i dat_get1r dat_getvc dat_getvd dat_getvi dat_getvr dat_index
dat_len dat_map dat_mapc dat_mapd dat_mapi dat_mapl dat_mapr dat_mapv
dat_mould dat_move dat_msg dat_name dat_ncomp dat_new dat_new0c
dat_new0d dat_new0i dat_new0l dat_new0r dat_new1c dat_new1d dat_new1i
dat_new1l dat_new1r dat_newc dat_paren dat_prec dat_prim dat_prmry
dat_put0c dat_put0d dat_put0i dat_put0l dat_put0r dat_put1c dat_put1d
dat_put1i dat_put1r dat_putd dat_puti dat_putr dat_putvc dat_putvd
dat_putvi dat_putvr dat_ref dat_refct dat_renam dat_reset dat_retyp
dat_shape dat_size dat_slice dat_state dat_struc dat_temp dat_there
dat_type dat_unmap dat_valid dat_vec dat_where


=item :cmp

cmp_get0c cmp_get0d cmp_get0i cmp_get0l cmp_get0r cmp_get1c cmp_get1d
cmp_get1i cmp_get1r cmp_getvc cmp_getvd cmp_getvi cmp_getvr cmp_len
cmp_mapv cmp_mod cmp_modc cmp_prim cmp_put0c cmp_put0d cmp_put0i
cmp_put0l cmp_put0r cmp_put1c cmp_put1d cmp_put1i cmp_put1r cmp_putni
cmp_putvc cmp_putvd cmp_putvi cmp_putvr cmp_shape cmp_size cmp_struc
cmp_type cmp_unmap

=item :ndg

ndgReadProv

=item :misc

mem2string string2mem array2mem mem2array fits_get_nth_item
fits_read_header fits_get_item fits_extract_key_val 
fits_construct_string par_get err_flush_to_string


=back

=head1 AUTHOR

Module created by T. Jenness, E<lt>t.jenness@jach.hawaii.eduE<gt>
(with help from F. Economou, E<lt>frossie@jach.hawaii.eduE<gt>)

=head1 COPYRIGHT

Copyright (C) Science and Technology Facilities Council.
Copyright (C) 1996-2007 Tim Jenness, Frossie Economou and the
UK Particle Physics and Astronomy Research Council.
All Rights Reserved.

=head1 STARLINK

The NDF library is by Rodney Warren-Smith and is part of the Starlink
Software Collection (Starlink Project, Rutherford Appleton Lab).
For more information on Starlink go to http://www.starlink.ac.uk.
See Starlink User Note 33 for details on using the NDF library routines.


=head1 SEE ALSO

L<perl> for an introduction to Perl,
L<perlfunc> for a description of the pack and unpack commands,
L<perlxs> for details of how this module accesses external libraries.

=cut
