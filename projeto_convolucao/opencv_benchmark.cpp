#include <opencv2/opencv.hpp>
#include <iostream>
#include <iomanip>
#include <chrono>

int main(int argc, char** argv)
{
    if (argc != 2) {
        std::cerr << "Uso: " << argv[0]
                  << " TAMANHO (1024, 2048 ou 4096)\n";
        return 1;
    }

    int N = std::stoi(argv[1]);

    if (N != 1024 && N != 2048 && N != 4096) {
        std::cerr << "Tamanho permitido: 1024, 2048 ou 4096.\n";
        return 1;
    }

    cv::Mat src(N, N, CV_32F);
    cv::Mat dst;

    // Mesma entrada utilizada nos testes em C.
    for (int i = 0; i < N; i++) {
        float* row = src.ptr<float>(i);
        for (int j = 0; j < N; j++) {
            row[j] = static_cast<float>(
                (static_cast<size_t>(i) * N + j) % 100
            ) / 100.0f;
        }
    }

    cv::Mat kernel = cv::Mat::ones(3, 3, CV_32F) / 9.0f;

    // Aquecimento.
    cv::filter2D(src, dst, CV_32F, kernel,
                 cv::Point(-1, -1), 0, cv::BORDER_CONSTANT);

    // Igualar o tratamento das bordas ao código em C.
    dst.row(0).setTo(0);
    dst.row(N - 1).setTo(0);
    dst.col(0).setTo(0);
    dst.col(N - 1).setTo(0);

    const double target_seconds = 2.0;
    long iterations = 0;

    auto start = std::chrono::steady_clock::now();
    auto end = start;

    do {
        cv::filter2D(src, dst, CV_32F, kernel,
                     cv::Point(-1, -1), 0, cv::BORDER_CONSTANT);

        // O custo das bordas é pequeno, mas precisa ser incluído
        // em todas as iterações para preservar a equivalência.
        dst.row(0).setTo(0);
        dst.row(N - 1).setTo(0);
        dst.col(0).setTo(0);
        dst.col(N - 1).setTo(0);

        iterations++;
        end = std::chrono::steady_clock::now();

    } while (std::chrono::duration<double>(end - start).count()
             < target_seconds);

    double seconds =
        std::chrono::duration<double>(end - start).count();

    double checksum = cv::sum(dst)[0];

    std::cout << std::fixed << std::setprecision(9);
    std::cout << "Versao: OpenCV filter2D\n";
    std::cout << "OpenCV: " << CV_VERSION << "\n";
    std::cout << "Matriz: " << N << " x " << N << "\n";
    std::cout << "Iteracoes: " << iterations << "\n";
    std::cout << std::setprecision(6);
    std::cout << "Tempo total: " << seconds << " s\n";
    std::cout << "Tempo por iteracao: "
              << seconds * 1000.0 / iterations << " ms\n";
    std::cout << std::setprecision(9);
    std::cout << "Checksum: " << checksum << "\n";

    return 0;
}
