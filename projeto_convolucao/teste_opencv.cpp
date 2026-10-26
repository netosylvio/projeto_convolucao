#include <opencv2/opencv.hpp>
#include <iostream>

int main() {
    const int N = 1024;
    const int REP = 30;

    cv::Mat src(N, N, CV_32F);
    for (int i = 0; i < N; i++)
        for (int j = 0; j < N; j++)
            src.at<float>(i,j) = float((i*N+j) % 100) / 100.0f;

    cv::Mat kernel = cv::Mat::ones(3, 3, CV_32F) / 9.0f;
    cv::Mat dst;

    for (int r = 0; r < REP; r++)
        cv::filter2D(src, dst, CV_32F, kernel,
                     cv::Point(-1,-1), 0, cv::BORDER_CONSTANT);

    std::cout << "OpenCV version: " << CV_VERSION << "\n";
    std::cout << "OpenCV convolution: N=" << N
              << ", repeticoes=" << REP
              << ", checksum=" << cv::sum(dst)[0] << "\n";
    return 0;
}
