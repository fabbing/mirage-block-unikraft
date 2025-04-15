#include "result.h"

#define CAML_NAME_SPACE
#include <caml/memory.h>
#include <caml/alloc.h>

CAMLprim value alloc_result_ok(void)
{
  CAMLparam0();
  CAMLlocal1(v_result);

  v_result = caml_alloc(1, 0);
  CAMLreturn(v_result);
}

CAMLprim value alloc_result_error(const char *msg)
{
  CAMLparam0();
  CAMLlocal2(v_result, v_error);

  v_error = caml_copy_string(msg);
  v_result = caml_alloc_1(1, v_error);
  CAMLreturn(v_result);
};
