#include <stdio.h>

double integrate(double (*f)(double), double a, double b,int n) {
    double dx = (b - a) / n;
    double x, sum = 0.0;
    for (int i=0;i<n;i++){
        x = a + i * dx;
        sum += f(x) * dx;
    }
    return sum;
}

double square(double x){
    return x*x;
}

int main() {
    double a = 0.0, b = 2.0;
    int n = 1000;
    printf("integrate(square, 0.0, 2.0)=%f\n", integrate(square, a, b, n));
}