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

static void convolution_tiling_simd(const float *restrict input,
                                    float *restrict output,
                                    int n, int block)
{
    const __m256 kernel = _mm256_set1_ps(1.0f / 9.0f);

    for (int ii = 1; ii < n - 1; ii += block) {
        int i_end = ii + block;
        if (i_end > n - 1)
            i_end = n - 1;

        for (int jj = 1; jj < n - 1; jj += block) {
            int j_end = jj + block;
            if (j_end > n - 1)
                j_end = n - 1;

            for (int i = ii; i < i_end; i++) {
                int j = jj;

                /* AVX2: processa oito resultados por vetor. */
                for (; j + 7 < j_end; j += 8) {
                    int p = i * n + j;

                    __m256 sum = _mm256_loadu_ps(input + p - n - 1);
                    sum = _mm256_add_ps(sum,
                                        _mm256_loadu_ps(input + p - n));
                    sum = _mm256_add_ps(sum,
                                        _mm256_loadu_ps(input + p - n + 1));

                    sum = _mm256_add_ps(sum,
                                        _mm256_loadu_ps(input + p - 1));
                    sum = _mm256_add_ps(sum,
                                        _mm256_loadu_ps(input + p));
                    sum = _mm256_add_ps(sum,
                                        _mm256_loadu_ps(input + p + 1));

                    sum = _mm256_add_ps(sum,
                                        _mm256_loadu_ps(input + p + n - 1));
                    sum = _mm256_add_ps(sum,
                                        _mm256_loadu_ps(input + p + n));
                    sum = _mm256_add_ps(sum,
                                        _mm256_loadu_ps(input + p + n + 1));

                    sum = _mm256_mul_ps(sum, kernel);
                    _mm256_storeu_ps(output + p, sum);
                }

                /* Elementos restantes dentro do bloco. */
                for (; j < j_end; j++) {
                    int p = i * n + j;

                    float sum =
                        input[p-n-1] + input[p-n] + input[p-n+1] +
                        input[p-1]   + input[p]   + input[p+1] +
                        input[p+n-1] + input[p+n] + input[p+n+1];

                    output[p] = sum * (1.0f / 9.0f);
                }
            }
        }
    }
}

int main(int argc, char **argv)
{
    if (argc != 3) {
        fprintf(stderr, "Uso: %s TAMANHO BLOCO\n", argv[0]);
        return EXIT_FAILURE;
    }

    char *endptr1, *endptr2;
    long parsed_n = strtol(argv[1], &endptr1, 10);
    long parsed_b = strtol(argv[2], &endptr2, 10);

    if (*endptr1 != '\0' || *endptr2 != '\0' ||
        (parsed_n != 1024 && parsed_n != 2048 && parsed_n != 4096) ||
        (parsed_b != 8 && parsed_b != 16 &&
         parsed_b != 32 && parsed_b != 64)) {
        fprintf(stderr,
                "Tamanho: 1024, 2048 ou 4096; bloco: 8, 16, 32 ou 64.\n");
        return EXIT_FAILURE;
    }

    int n = (int)parsed_n;
    int block = (int)parsed_b;
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

    convolution_tiling_simd(input, output, n, block); /* Aquecimento. */

    struct timespec start, end;
    long iterations = 0;

    clock_gettime(CLOCK_MONOTONIC, &start);

    do {
        convolution_tiling_simd(input, output, n, block);
        iterations++;
        clock_gettime(CLOCK_MONOTONIC, &end);
    } while (elapsed(start, end) < 2.0);

    double seconds = elapsed(start, end);

    double checksum = 0.0;
    for (size_t k = 0; k < count; k++)
        checksum += output[k];

    printf("Versao: Tiling + SIMD/AVX2\n");
    printf("Matriz: %d x %d\n", n, n);
    printf("Bloco: %d x %d\n", block, block);
    printf("Iteracoes: %ld\n", iterations);
    printf("Tempo total: %.6f s\n", seconds);
    printf("Tempo por iteracao: %.6f ms\n",
           seconds * 1000.0 / iterations);
    printf("Checksum: %.9f\n", checksum);

    free(input);
    free(output);
    return EXIT_SUCCESS;
}
