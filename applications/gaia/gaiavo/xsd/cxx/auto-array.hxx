// file      : xsd/cxx/auto-array.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#ifndef XSD_CXX_AUTO_ARRAY_HXX
#define XSD_CXX_AUTO_ARRAY_HXX

#include <cstddef> // std::size_t

namespace xsd
{
  namespace cxx
  {
    template <typename X>
    struct std_deallocator
    {
      void
      deallocate (X* p)
      {
        delete[] p;
      }
    };

    // Simple automatic array. The second template parameter is
    // an optional deallocator type. If not specified, delete[]
    // is used.
    //
    template <typename X, typename D = std_deallocator<X> >
    struct auto_array
    {
      auto_array (X a[])
          : a_ (a), d_ (0)
      {
      }

      auto_array (X a[], D& d)
          : a_ (a), d_ (&d)
      {
      }

      ~auto_array ()
      {
        if (d_ != 0)
          d_->deallocate (a_);
        else
          delete[] a_;
      }

      X&
      operator[] (std::size_t index) const
      {
        return a_[index];
      }

      X*
      get () const
      {
        return a_;
      }

      X*
      release ()
      {
        X* tmp (a_);
        a_ = 0;
        return tmp;
      }

      void
      reset (X a[] = 0)
      {
        if (a_ != a)
        {
          if (d_ != 0)
            d_->deallocate (a_);
          else
            delete[] a_;

          a_ = a;
        }
      }

      typedef void (auto_array::*bool_convertible)();

      operator bool_convertible () const
      {
        return a_ ? &auto_array<X, D>::true_ : 0;
      }

    private:
      auto_array (const auto_array&);

      auto_array&
      operator= (const auto_array&);

    private:
      void
      true_ ();

    private:
      X* a_;
      D* d_;
    };

    template <typename X, typename D>
    void auto_array<X, D>::
    true_ ()
    {
    }
  }
}

#endif  // XSD_CXX_AUTO_ARRAY_HXX
