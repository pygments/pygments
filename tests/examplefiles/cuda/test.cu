#include <stdio.h>

// __device__ function
__device__ void func()
{
    short* array0 = (short*)array;
    float* array1 = (float*)&array0[127];
}

/* __global__ function */
__global__ static void reduction(const float* __restrict__ input, float *output, clock_t *timer)
{
    // __shared__ float shared[2 * blockDim.x];
    extern __shared__ float shared[];

    const int tid = threadIdx.x;
    const int bid = blockIdx.x;
    
    if (threadIdx.x == 0) {
        __threadfence();
    }

    // Perform reduction to find minimum.
    for (int d = blockDim.x; d > 0; d /= 2)
    {
        __syncthreads();
    }
}

int main(int argc, char **argv)
{
    dim3 dimBlock(8, 8, 1);

    timedReduction<<<dimBlock, 256, 256, 0>>>(dinput, doutput, dtimer);
    cudaDeviceReset();
}

// Modern CUDA is compiled as C++, so C++ constructs are valid.
namespace example {

template <typename T>
__global__ void scale(T* data, T factor)
{
    constexpr int stride = 32;
    auto idx = blockIdx.x * blockDim.x + threadIdx.x;
    data[idx * stride] *= factor;
}

class Buffer {
public:
    explicit Buffer(int n) noexcept : size_(n), ptr_(nullptr) {}
    ~Buffer() { cudaFree(ptr_); }
private:
    int size_;
    float* ptr_;
};

}  // namespace example
