// file      : xsd/cxx/xml/dom/auto-ptr.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#ifndef XSD_CXX_XML_DOM_AUTO_PTR_HXX
#define XSD_CXX_XML_DOM_AUTO_PTR_HXX

namespace xsd
{
  namespace cxx
  {
    namespace xml
    {
      namespace dom
      {
        // Simple auto_ptr version that calls release() instead of delete.
        //

        template <typename X>
        struct remove_c
        {
          typedef X r;
        };

        template <typename X>
        struct remove_c<const X>
        {
          typedef X r;
        };

        template <typename X>
        struct auto_ptr_ref
        {
          X* x_;

          explicit
          auto_ptr_ref (X* x)
              : x_ (x)
          {
          }
        };

        template <typename X>
        struct auto_ptr
        {
          ~auto_ptr ()
          {
            reset ();
          }

          explicit
          auto_ptr (X* x = 0)
              : x_ (x)
          {
          }

          auto_ptr (auto_ptr& y)
              : x_ (y.release ())
          {
          }

          template <typename Y>
          auto_ptr (auto_ptr<Y>& y)
              : x_ (y.release ())
          {
          }

          auto_ptr (auto_ptr_ref<X> r)
              : x_ (r.x_)
          {
          }

          auto_ptr&
          operator= (auto_ptr& y)
          {
            if (x_ != y.x_)
              reset (y.release ());

            return *this;
          }

          template <typename Y>
          auto_ptr&
          operator= (auto_ptr<Y>& y)
          {
            if (x_ != y.x_)
              reset (y.release ());

            return *this;
          }

          auto_ptr&
          operator= (auto_ptr_ref<X> r)
          {
            if (r.x_ != x_)
              reset (r.x_);

            return *this;
          }

          template <typename Y>
          operator auto_ptr_ref<Y> ()
          {
            return auto_ptr_ref<Y> (release ());
          }

          template <typename Y>
          operator auto_ptr<Y> ()
          {
            return auto_ptr<Y> (release ());
          }

        public:
          X&
          operator* () const
          {
            return *x_;
          }

          X*
          operator-> () const
          {
            return x_;
          }

          X*
          get () const
          {
            return x_;
          }

          X*
          release ()
          {
            X* x (x_);
            x_ = 0;
            return x;
          }

          void
          reset (X* x = 0)
          {
            if (x_)
              const_cast<typename remove_c<X>::r*> (x_)->release ();

            x_ = x;
          }

        private:
          X* x_;
        };
      }
    }
  }
}

#endif // XSD_CXX_XML_DOM_AUTO_PTR_HXX
