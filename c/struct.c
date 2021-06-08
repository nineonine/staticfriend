#include <stdio.h>

struct Struct {
    int int_field;
    char char_field;
};

int main() {
    struct Struct s;
    s.int_field = 1;
    s.char_field = 'c';
    printf("got %i %c", s.int_field, s.char_field);
    return 0;
}
