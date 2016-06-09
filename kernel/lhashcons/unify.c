#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/address_class.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <stdio.h>
#include <stdlib.h>

#define Assert(X) 1
#define addr void*

// #define PROFILE

#ifdef PROFILE
static long all = 0;
static long hidden = 0;
static long yy = 0;
static long yo = 0;
static long oo = 0;

static void
print_at_exit(void)
{
  fprintf(stderr,
	  "Total (useful) unifications: %ld / %ld (yy: %ld, oo: %ld, yo: %ld),\n",
	  hidden, all, yy, oo, yo);
}

static int posted = 0;
#endif /* PROFILE */

CAMLprim value
caml__unify(value a, value i, value b, value j)
{
  CAMLparam4(a, i, b, j);
#ifdef PROFILE
  if (!posted) {
    posted = 1;
    atexit(print_at_exit);
  }
  all++;
#endif /* PROFILE */

  value *av = &Field(a,Int_val(i));
  value *bv = &Field(b,Int_val(j));
  if (a == b) {
    CAMLreturn(Val_unit);
  }

#ifdef PROFILE
  hidden++;
#endif /* PROFILE */

  if (Is_young(a)) {
    if (Is_young(b)) {
      // both are young, prefer the older one (if possible)
#ifdef PROFILE
      yy++;
#endif /* PROFILE */
      // maybe there is a better direction?
      if (av < bv) {
	caml_modify(bv, *av);
      } else {
	caml_modify(av, *bv);
      }
    } else {
#ifdef PROFILE
      yo++;
#endif /* PROFILE */
      // a is young and b is old, so we make [a] point to [b]
      caml_modify(av, *bv);
    }
  } else if (Is_young(b)) {
#ifdef PROFILE
    yo++;
#endif /* PROFILE */
    // a is old and b is young, so we make b point to a
    caml_modify(bv, *av);
  } else {
    // both are old (prefer the one with the smaller address)
#ifdef PROFILE
    oo++;
#endif /* PROFILE */
    if (av < bv) {
      caml_modify(bv, *av);
    } else {
      caml_modify(av, *bv);
    }
  }

  CAMLreturn(Val_unit);
}

/*
static value
hidden_compare(value* lhs, value* rhs)
{
  if (lhs == rhs)
    return Val_true;

  if (Is_block(*lhs)) {
    if (Is_block(*rhs)) {
      if (Field(*lhs, 1) != Field(*rhs, 1))
	return Val_false;

      value* l = &Field(*lhs,0);
      value* r = &Field(*rhs,0);

      if (Val_true == hidden_compare(l, r)) {
	unify(l, r);
      } else
	return Val_false;

      l = &Field(*lhs,2);
      r = &Field(*rhs,2);
      if (Val_true == hidden_compare(l, r)) {
	unify(l, r);
	return Val_true;
      }
      return Val_false;
    } else
      return Val_false;
  } else if (Is_block(*rhs)) {
    return Val_false;
  } else {
    return Val_true;
  }
}

CAMLprim value
caml__int_tree_eq(value lhs, value rhs)
{
  CAMLparam2(lhs, rhs);
  CAMLreturn (hidden_compare(&lhs, &rhs));
}
*/
