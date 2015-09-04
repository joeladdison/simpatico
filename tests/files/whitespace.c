#include<stdio.h> //error
#include <stdlib.h>

int one =1; //error
int two = 2;
int three= 3; //error
void f() { //error
}
void g() { //error
}
/*still not enough space*/
void h() { //error
}

/* tests spacing around possible unary operators */
void unary_binary_mix() {
    int *p = &one;
    int a = -one;
    a = - one; //error
    a = one - two;
    a = one-two; //error
    a = *p;
    a = one * two;
    a = one*two; //error
    a = * p; //error
    p = & one; //error
    a = +one;
    a = + one; //error
    a = one + two;
    a = one + *p;
    a = one +*p; //error
    a = *p + one;
    a = * p + one; //error
}

/* tests spacing around binary operators */
void binary_operators() {
    int a = 1, b = 1, c = 1;
    a = b / c;
    a = b/c; //error
    a = b/ c; //error
    a = b /c; //error
    a = b%c; //error
    a = b % c;
    a = b && c;
    a = b&&c; //error
    a = b || c;
    a = b||c; //error
    a = b ^ c;
    a = b^c; //error
    a = b & c;
    a = b&c; //error
    a = b | c;
    a = b|c; //error
    a = b & c;
    a = b&c; //error
    a = b < c;
    a = b<c; //error
    a = b <= c;
    a = b<=c; //error

    struct S {
        int a;
    } s, *t;
    t = &s;
    a = s.a;
    a = s . a; //error
    a = t->a;
    a = t -> a; //error
    
}

int main(){ //error
    int sum = 0;
    int    i; //error

    for (i = 0; i <= 4; ++i){ //error
        printf("sum is now %d\n", sum);
        sum += i;
    }

    // A really long statement that has to cover lots of lines=something
    if (sum == 1 || sum == 2 
            ||sum == 3 //error
            || sum == 4 ||sum == 5 || //error
            sum == 6 || sum == 7 
            || sum ==8 //error
            || sum == 9 || sum == 11 ||
            sum == 12 || sum ==13 //error
            || sum== 14 || sum == 15) { //error
        printf("something went %s wrong!\n","terribly"); //error
        printf("oh %s!\n", "noes");
    } else {
        printf( "the sum is %d!\n", sum); //error
    }
    
    if ( i ) { //error
        i++;
        ++i;
    } else {
        ++ i; //error
        i ++; //error
    }
    int x = 2,y; //error
    switch(x) {
        case 0:
            y = 0;
            break;
        case 1:
            y= 1; //error
        case 2:
            y = 2;
            break;
        case 3:
        case'4': //error
            y = 4;
            break;
        default:
            x +=0; //error
            y-= 1; //error
            break;
    }
}

