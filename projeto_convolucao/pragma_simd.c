#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>

static double elapsed(struct timespec start, struct timespec end)
{
    return (end.tv_sec - start.tv_sec) +
           (end.tv_nsec - start.tv_nsec) / 1e9;
}

static void convolution(const float *restrict input,
                        float *restrict output,
                        int n)
{
    const float kernel = 1.0f / 9.0f;

    /* Percurso por linhas: acesso contíguo à memória. */
    for (int i = 1; i < n - 1; i++) {
        #pragma omp simd
        for (int j = 1; j < n - 1; j++) {
            int p = i * n + j;

            float sum =
                input[p - n - 1] + input[p - n] + input[p - n + 1] +
                input[p - 1]     + input[p]     + input[p + 1] +
                input[p + n - 1] + input[p + n] + input[p + n + 1];

            output[p] = sum * kernel;
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
        fprintf(stderr, "Erro ao alocar memoria para matriz %dx%d.\n", n, n);
        free(input);
        free(output);
        return EXIT_FAILURE;
    }

    /* Entrada determinística, igual para todas as execuções. */
    for (size_t k = 0; k < count; k++) {
        input[k] = (float)(k % 100) / 100.0f;
    }

    /* Aquecimento antes da medição. */
    convolution(input, output, n);

    const double target_seconds = 2.0;
    long iterations = 0;
    struct timespec start, end;

    clock_gettime(CLOCK_MONOTONIC, &start);

    do {
        convolution(input, output, n);
        iterations++;

        clock_gettime(CLOCK_MONOTONIC, &end);
    } while (elapsed(start, end) < target_seconds);

    double seconds = elapsed(start, end);

    /* Checksum para ajudar a verificar e preservar o resultado. */
    double checksum = 0.0;
    for (size_t k = 0; k < count; k++) {
        checksum += output[k];
    }

    double output_pixels = (double)(n - 2) * (n - 2);
    double flops = output_pixels * 17.0 * iterations;
    double mflops = flops / seconds / 1e6;

    printf("Versao: pragma omp simd\n");
    printf("Matriz: %d x %d\n", n, n);
    printf("Iteracoes: %ld\n", iterations);
    printf("Tempo medido: %.6f s\n", seconds);
    printf("Tempo por iteracao: %.6f s\n", seconds / iterations);
    printf("Desempenho estimado: %.2f MFLOP/s\n", mflops);
    printf("Checksum: %.9f\n", checksum);

    free(input);
    free(output);
    return EXIT_SUCCESS;
}
