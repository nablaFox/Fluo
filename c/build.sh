g++ \
  -x c device.c \
  -x c window.c \
  -x c main.c \
  -x c++ vma_impl.cpp \
  -o ../build/main \
  -lvulkan -lglfw

../build/main
