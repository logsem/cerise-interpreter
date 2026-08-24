%define INITIAL 3

%macro add_four(dst: reg)
  mov $dst INITIAL
  add $dst $dst 4
%endmacro

%add_four(r2)
halt
