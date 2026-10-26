#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <immintrin.h>

static double elapsed(struct timespec a, struct timespec b)
{
    return (b.tv_sec - a.tv_sec) +
           (b.tv_nsec - a.tv_nsec) / 1e9;
}

static void convolution_avx2(const float *restrict input,
                             float *restrict output, int n)
{
    const __m256 kernel = _mm256_set1_ps(1.0f / 9.0f);

    for (int i = 1; i < n - 1; i++) {
        int j = 1;

        /* Processa 8 resultados por vez usando AVX2. */
        for (; j + 7 < n - 1; j += 8) {
            int p = i * n + j;

            __m256 sum = _mm256_loadu_ps(input + p - n - 1);
            sum = _mm256_add_ps(sum, _mm256_loadu_ps(input + p - n));
            sum = _mm256_add_ps(sum, _mm256_loadu_ps(input + p - n + 1));

            sum = _mm256_add_ps(sum, _mm256_loadu_ps(input + p - 1));
            sum = _mm256_add_ps(sum, _mm256_loadu_ps(input + p));
            sum = _mm256_add_ps(sum, _mm256_loadu_ps(input + p + 1));

            sum = _mm256_add_ps(sum, _mm256_loadu_ps(input + p + n - 1));
            sum = _mm256_add_ps(sum, _mm256_loadu_ps(input + p + n));
            sum = _mm256_add_ps(sum, _mm256_loadu_ps(input + p + n + 1));

            sum = _mm256_mul_ps(sum, kernel);
            _mm256_storeu_ps(output + p, sum);
        }

        /* Trata os últimos elementos que não completam um vetor. */
        for (; j < n - 1; j++) {
            int p = i * n + j;

            float sum =
                input[p-n-1] + input[p-n] + input[p-n+1] +
                input[p-1]   + input[p]   + input[p+1] +
                input[p+n-1] + input[p+n] + input[p+n+1];

            output[p] = sum * (1.0f / 9.0f);
        }
    }
}

int main(int argc, char **argv)
{
    if (argc != 2) {
        fprintf(stderr, "Uso: %s TAMANHO\n", argv[0]);
        return EXIT_FAILURE;
    }

    char *endptr;
    long parsed = strtol(argv[1], &endptr, 10);

    if (*endptr != '\0' ||
        (parsed != 1024 && parsed != 2048 && parsed != 4096)) {
        fprintf(stderr, "Tamanho permitido: 1024, 2048 ou 4096.\n");
        return EXIT_FAILURE;
    }

    int n = (int)parsed;
    size_t count = (size_t)n * n;

    float *input = malloc(count * sizeof(float));
    float *output = calloc(count, sizeof(float));

    if (!input || !output) {
        fprintf(stderr, "Falha ao alocar memoria.\n");
        free(input);
        free(output);
        return EXIT_FAILURE;
    }

    for (size_t k = 0; k < count; k++)
        input[k] = (float)(k % 100) / 100.0f;

    convolution_avx2(input, output, n); /* Aquecimento. */

    struct timespec start, end;
    long iterations = 0;

    clock_gettime(CLOCK_MONOTONIC, &start);

    do {
        convolution_avx2(input, output, n);
        iterations++;
        clock_gettime(CLOCK_MONOTONIC, &end);
    } while (elapsed(start, end) < 2.0);

    double seconds = elapsed(start, end);

    double checksum = 0.0;
    for (size_t k = 0; k < count; k++)
        checksum += output[k];

    printf("Versao: SIMD AVX2 explicito\n");
    printf("Matriz: %d x %d\n", n, n);
    printf("Iteracoes: %ld\n", iterations);
    printf("Tempo total: %.6f s\n", seconds);
    printf("Tempo por iteracao: %.6f ms\n",
           seconds * 1000.0 / iterations);
    printf("Checksum: %.9f\n", checksum);

    free(input);
    free(output);
    return EXIT_SUCCESS;
}
