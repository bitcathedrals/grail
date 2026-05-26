#include <stdio.h>

int add(int a, int b) {
<<<<<<< HEAD
    // Version from your current branch (e.g., 'main')
    printf("Logging: Adding %d and %d\n", a, b);
    return a + b;
=======
    // Version from the branch being merged (e.g., 'feature-branch')
    int sum = a + b;
    return sum;
>>>>>>> feature-branch
}
