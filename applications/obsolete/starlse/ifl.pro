#+
#  Name:
#     {module_name}.IFL

#  Type of Module:
#     ADAM A-task parameter interface.

#  Authors:
#     {author_identifier}: {authors_name} ({affiliation})
#     {enter_new_authors_here}

#  History:
#     {date} ({author_identifier}):
#        Original version.
#     {enter_changes_here}

#-

interface {module_name}

   helplib '[help_library_specification]'

   parameter {parameter_name}    # [parameter_comment]
      position [parameter_position]
      keyword  [keyword_name]
      type     [parameter_type]
      access   [access_mode]
      vpath    '[value_source]...'
      ppath    '[prompt_source]...'
      default  [default_values]
      range    [lower_bound], {upper_bound}
      in       [included_value]...
      association [operator]{associated_parameter}
      ptype    [parameter_class]
      prompt   '[prompt_string]'
      helpkey  '[help_key]'
   endparameter

   [parameter_specification]...

   message {parameter_name}      # [parameter_comment]
      text     '{message_text}'
   endmessage

   [message_specification]...

endinterface
