%define INITIAL 3

%macro add_four(dst: reg, initial: value)
  mov $dst $initial
  add $dst $initial 4
%endmacro

%add_four(r2, INITIAL)
halt
