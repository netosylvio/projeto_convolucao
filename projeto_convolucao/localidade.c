#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

static double elapsed(struct timespec a, struct timespec b)
{
    return (b.tv_sec - a.tv_sec) +
           (b.tv_nsec - a.tv_nsec) / 1e9;
}

/* Versão A: percorre por linhas (row-major). */
static void row_major(const float *restrict input,
                      float *restrict output, int n)
{
    const float c = 1.0f / 9.0f;

    for (int i = 1; i < n - 1; i++) {
        for (int j = 1; j < n - 1; j++) {
            int p = i * n + j;

            float sum =
                input[p-n-1] + input[p-n] + input[p-n+1] +
                input[p-1]   + input[p]   + input[p+1] +
                input[p+n-1] + input[p+n] + input[p+n+1];

            output[p] = sum * c;
        }
    }
}

/* Versão B: percorre por colunas (column-major). */
static void column_major(const float *restrict input,
                         float *restrict output, int n)
{
    const float c = 1.0f / 9.0f;

    for (int j = 1; j < n - 1; j++) {
        for (int i = 1; i < n - 1; i++) {
            int p = i * n + j;

            float sum =
                input[p-n-1] + input[p-n] + input[p-n+1] +
                input[p-1]   + input[p]   + input[p+1] +
                input[p+n-1] + input[p+n] + input[p+n+1];

            output[p] = sum * c;
        }
    }
}

static double checksum(const float *a, int n)
{
    double sum = 0.0;
    for (size_t k = 0; k < (size_t)n * n; k++)
        sum += a[k];
    return sum;
}

int main(int argc, char **argv)
{
    if (argc != 3) {
        fprintf(stderr, "Uso: %s TAMANHO row|column\n", argv[0]);
        return EXIT_FAILURE;
    }

    int n = atoi(argv[1]);
    int use_row;

    if (n != 1024 && n != 2048 && n != 4096) {
        fprintf(stderr, "Tamanho permitido: 1024, 2048 ou 4096.\n");
        return EXIT_FAILURE;
    }

    if (argv[2][0] == 'r' && argv[2][1] == 'o')
        use_row = 1;
    else if (argv[2][0] == 'c' && argv[2][1] == 'o')
        use_row = 0;
    else {
        fprintf(stderr, "Escolha row ou column.\n");
        return EXIT_FAILURE;
    }

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

    void (*run)(const float *, float *, int) =
        use_row ? row_major : column_major;

    run(input, output, n);  /* Aquecimento. */

    struct timespec start, end;
    long iterations = 0;

    clock_gettime(CLOCK_MONOTONIC, &start);

    do {
        run(input, output, n);
        iterations++;
        clock_gettime(CLOCK_MONOTONIC, &end);
    } while (elapsed(start, end) < 2.0);

    double seconds = elapsed(start, end);

    printf("Versao: %s\n", use_row ? "row-major" : "column-major");
    printf("Matriz: %d x %d\n", n, n);
    printf("Iteracoes: %ld\n", iterations);
    printf("Tempo total: %.6f s\n", seconds);
    printf("Tempo por iteracao: %.6f ms\n",
           seconds * 1000.0 / iterations);
    printf("Checksum: %.9f\n", checksum(output, n));

    free(input);
    free(output);
    return EXIT_SUCCESS;
}
