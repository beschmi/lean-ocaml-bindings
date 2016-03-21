#include <lean.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  lean_bool b;
  // lean_name name;
  struct _lean_name *name;
  lean_exception ex;
  const char *name_string;

  b = lean_name_mk_anonymous(&name, &ex);
  if (b != lean_true) { printf("lean_name_mk_anonymous failed\n"); exit(1); }
  
  b = lean_name_to_string(name,&name_string,&ex);
  if (b != lean_true) { printf("lean_name_to_string failed\n"); exit(1); }
  
  printf("the lean name is %s", name_string);

  return 0;
}
