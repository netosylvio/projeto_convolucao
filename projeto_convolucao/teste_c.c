#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define N 1024
#define REP 30

int main(void) {
    size_t total = (size_t)N * N;
    float *a = malloc(total * sizeof(float));
    float *b = calloc(total, sizeof(float));

    if (!a || !b) {
        fprintf(stderr, "Falha ao alocar memoria\n");
        free(a);
        free(b);
        return 1;
    }

    for (size_t i = 0; i < total; i++)
        a[i] = (float)(i % 100) / 100.0f;

    const float k[3][3] = {
        {1.0f/9, 1.0f/9, 1.0f/9},
        {1.0f/9, 1.0f/9, 1.0f/9},
        {1.0f/9, 1.0f/9, 1.0f/9}
    };

    for (int r = 0; r < REP; r++) {
        for (int i = 1; i < N - 1; i++) {
            for (int j = 1; j < N - 1; j++) {
                float sum = 0.0f;
                for (int di = -1; di <= 1; di++)
                    for (int dj = -1; dj <= 1; dj++)
                        sum += a[(size_t)(i+di)*N + j+dj]
                               * k[di+1][dj+1];
                b[(size_t)i*N + j] = sum;
            }
        }
    }

    double checksum = 0.0;
    for (size_t i = 0; i < total; i++)
        checksum += b[i];

    printf("C convolution: N=%d, repeticoes=%d, checksum=%.9f\n",
           N, REP, checksum);

    free(a);
    free(b);
    return 0;
}
