#include <stdio.h>

typedef enum {
    OK = 0,
        USAGE = 1   //error
} ExplicitEnumTest;

    int one = 1;     //error

int main() {
    int sum = 0;
        int i;         //error

    for (i = 0; i <= 4; ++i) {
        printf("sum is now %d\n", sum);
       sum += i; //error
    }

    // A really long statement that has to cover lots of lines
    if (sum == 1 || sum == 2 || sum == 3 || sum == 4 || sum == 5 ||
        sum == 6 || sum == 7 || sum == 8 || sum == 9 || sum == 11 || //error
            sum == 12 || sum > 12) {
        printf("something went %s wrong!\n", "terribly");
        printf("oh %s!\n", "noes");
    } else {
    printf("the sum is %d!\n", sum); //error
    }
    
    if (i)
        i++; //good, but brace problem
    else
        i--; //good, but brace problem

    int x = 2, y;
    switch(x) {
        case 0:
                y = 0; //error
                break; //error
        case 1:
        y = 1;         //error
        case 2:
            y = 2;
            break;
        case 3:
        case 4:
            y = 4;
            break;
        default:
            x = 0;
        y = -1;             //error
            break;
    }
    }             //error
