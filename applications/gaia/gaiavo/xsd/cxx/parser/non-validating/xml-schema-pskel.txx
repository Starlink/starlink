// file      : xsd/cxx/parser/non-validating/xml-schema-pskel.txx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

namespace xsd
{
  namespace cxx
  {
    namespace parser
    {
      namespace non_validating
      {
        // any_type
        //

        template <typename C>
        bool any_type_pskel<C>::
        _start_element_impl (const ro_string<C>& ns,
                             const ro_string<C>& name,
                             const ro_string<C>* type)
        {
          _start_any_element (ns, name, type);
          this->complex_content<C>::context_.top ().any_ = true;
          return true;
        }

        template <typename C>
        bool any_type_pskel<C>::
        _end_element_impl (const ro_string<C>& ns, const ro_string<C>& name)
        {
          this->complex_content<C>::context_.top ().any_ = false;
          _end_any_element (ns, name);
          return true;
        }


        template <typename C>
        bool any_type_pskel<C>::
        _attribute_impl (const ro_string<C>& ns,
                         const ro_string<C>& name,
                         const ro_string<C>& value)
        {
          _any_attribute (ns, name, value);
          return true;
        }

        template <typename C>
        bool any_type_pskel<C>::
        _characters_impl (const ro_string<C>& s)
        {
          _any_characters (s);
          return true;
        }

        // any_simple_type
        //

        template <typename C>
        bool any_simple_type_pskel<C>::
        _characters_impl (const ro_string<C>& s)
        {
          _any_characters (s);
          return true;
        }
      }
    }
  }
}
