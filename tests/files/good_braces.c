/* Contains no brace violations of any type.*/

struct bob {
    int charles;
};

typedef struct {
    int woo;
} wooo;

int array[] = {1, 2, 3, 4};
int another[] = {
    1,
    2,
    3,
    4
};
int yetAnother[] = {1, 2, 3,
        4};
int anotherStill[] = {1, 2, 3, \
        4};

void if_continutation(void) {
    if (1 && 2 &&
            3 && 4) {
        return;
    }
}

int fa(int Bad)
{
    if (a) {
        a++;
    } else if (a+1) {
        a /= 2;
    } else {
        a--;
    }
    return fb(a);
}

int fb(int a) {
    if (a) {
        a++;
    } else if (a + 1) {
        a--;
    } else {
        return 0;
    }
    return a;
}

int main(int argc, char **argv) {
    int a;
    int thing[][3] = {
        {1, 2, 3},
        {3, 4, 5}
    };
    switch (a) {
        case 0:
            return 0;
        default:
            return 1;
    }
    for (int i = 0; i < 10; i++) {
        a = i;
    }
    while (!a) {
        a++;
    }
    do {
        --a;
    } while (a);
    if (fa(a)) {
        a = 1;
    } else if (fb(b)) {
        a = 2;
    } else {
        a = 3;
    }

}
