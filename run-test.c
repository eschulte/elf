#include <ecl/ecl.h>

extern int main_dll_ELF(int argc, char **argv);

int main(int argc, char **argv)
{
  main_dll_ELF(argc, argv);
  cl_object elf =
    cl_eval(cl_list(2, c_string_to_object("read-elf"),
                       c_string_to_object("\"hello64\"")));
  cl_object vec =
    cl_eval(cl_list(2, c_string_to_object("show-memory-layout"), elf));
  si_exit(0);
}
