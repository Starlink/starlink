""" TeXML Writer and string services """
# $Id: texmlwr.py,v 1.12 2004/04/06 15:27:16 olpa Exp $

#
# Modes of processing of special characters
#
DEFAULT = 0;
TEXT    = 1;
MATH    = 2;
ASIS    = 3;

import unimap
import specmap
import codecs
import os

#
# Writer&Co class
#
class texmlwr:
  
  #
  # Object variables
  #
  # Empty line detection by end-of-line symbols
  # after_ch09
  # after_ch0a
  # Handling of '--', '---' and other ligatures
  # last_char
  #
  # Modes of transformation can be tuned and nested
  # mode
  # mode_stack
  # escape
  # escape_stack
  # ligatures
  # ligatures_stack
  # emptylines
  # emptylines_stack
  
  def __init__(self, stream):
    """ Remember output stream, initialize data structures """
    self.stream = stream
    self.after_char0d     = 1;
    self.after_char0a     = 1;
    self.last_ch          = None;
    self.mode             = TEXT;
    self.mode_stack       = [];
    self.escape           = 1;
    self.escape_stack     = [];
    self.ligatures        = 0;
    self.ligatures_stack  = [];
    self.emptylines       = 0;
    self.emptylines_stack = [];

  def stack_mode(self, mode):
    """ Put new mode into the stack of modes """
    self.mode_stack.append(self.mode)
    if mode != DEFAULT:
      self.mode = mode

  def unstack_mode(self):
    """ Restore mode """
    self.mode = self.mode_stack.pop()

  def stack_escape(self, ifdo):
    """ Set if escaping is required. Remember old value. """
    self.escape_stack.append(self.escape)
    if ifdo != None:
      self.escape = ifdo

  def unstack_escape(self):
    """ Restore old policy of escaping """
    self.escape = self.escape_stack.pop()

  def stack_ligatures(self, ifdo):
    """ Set if breaking of ligatures is required. Remember old value. """
    self.ligatures_stack.append(self.ligatures)
    if ifdo != None:
      self.ligatures = ifdo

  def unstack_ligatures(self):
    """ Restore old policy of breaking ligatures """
    self.ligatures = self.ligatures_stack.pop()

  def stack_emptylines(self, ifdo):
    """ Set if empty lines are required. Remember old value. """
    self.emptylines_stack.append(self.emptylines)
    if ifdo != None:
      self.emptylines = ifdo

  def unstack_emptylines(self):
    """ Restore old policy of handling of empty lines """
    self.emptylines = self.emptylines_stack.pop()
  
  def writech(self, ch, esc_specials):
    """ Write a char, (maybe) escaping specials """
    #
    # Handle ligatures
    #
    if not(self.ligatures):
      if '-' == ch:
	if '-' == self.last_ch:
	  self.stream.write('{}')
      elif "'" == ch:
	if "'" == self.last_ch:
	  self.stream.write('{}')
      elif '`' == ch:
	if ('`' == self.last_ch) or ('!' == self.last_ch) or ('?' == self.last_ch):
	  self.stream.write('{}')
    self.last_ch = ch
    #
    # Handle end-of-line symbols in special way
    # We automagically process "\n", "\r", "\n\r" and "\r\n" line ends
    #
    if ('\n' == ch) or ('\r' == ch):
      #
      # line end symbol of the same type starts new line
      #
      if not self.emptylines:
        if (('\n' == ch) and self.after_char0a) or (('\r' == ch) and self.after_char0d):
          self.stream.write('%')
      #
      # Two different line end symbols clean each other
      #
      if self.after_char0a and self.after_char0d:
        self.after_char0a = 0
        self.after_char0d = 0
      #
      # Remember that end of line has happended
      #
      if '\n' == ch:
        self.after_char0a = 1
      else:
        self.after_char0d = 1
      #
      # Write line end char (only once for \r\n) and return
      #
      if (0 == self.after_char0a) or (0 == self.after_char0d):
        self.stream.write(os.linesep)
      return                                               # return
    #
    # Not end-of-line symbol. If it has to be escaped, we write
    # plain ASCII substitution and return
    #
    self.after_char0d = 0
    self.after_char0a = 0
    #
    # Handle specials
    #
    if esc_specials:
      try:
        if self.mode == TEXT:
          self.stream.write(specmap.textescmap[ch])
        else:
          self.stream.write(specmap.mathescmap[ch])
        return                                             # return
      except:
        pass
    #
    # First attempt to write symbol as-is
    #
    try:
      self.stream.write(ch)
      return                                               # return
    except:
      pass
    #
    # In math mode, we try both math and text rewriting
    #
    chord = ord(ch)
    if self.mode == MATH:
      try:
        self.stream.write(unimap.mathmap[chord])
        return                                             # return
      except:
        pass
    #
    # Find symbol in text map
    #
    try:
      self.stream.write(unimap.textmap[chord])
      return                                               # return
    except:
      pass
    #
    # Finally, write symbol in &#xNNN; form
    #
    self.stream.write('&#x%X;' % chord)

  def write(self, str, escape = None):
    """ Write symbols char-by-char in current mode of escaping """
    if None == escape:
      escape = self.escape
    for ch in str:
      self.writech(ch, escape)
#
# Wrapper over output stream to write is desired encoding
#
class stream_encoder:

  def __init__(self, stream, encoding):
    """ Construct a wrapper by stream and encoding """
    self.stream = stream
    self.encode = codecs.getencoder(encoding)

  def write(self, str):
    """ Write string encoded """
    self.stream.write(self.encode(str)[0])

  def close(self):
    """ Close underlying stream """
    self.stream.close()
