mkdir -p priv

cd c

g++ \
  -fPIC \
  -I/usr/lib/erlang/usr/include \
  -shared \
  -x c device.c \
  -x c window.c \
  -x c fluo_nif.c \
  -x c++ vma_impl.cpp \
  -o ../priv/libfluo_nif.so \
  -lvulkan -lglfw
