#ifndef RESULT_H
#define RESULT_H

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>

CAMLprim value alloc_result_ok(void);
CAMLprim value alloc_result_error(const char *msg);

#endif /* !RESULT_H */
