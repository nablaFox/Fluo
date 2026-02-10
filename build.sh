mkdir -p priv

cd c

g++ \
  -fPIC \
  -Wall \
  -DDEBUG \
  -I/usr/lib/erlang/usr/include \
  -shared \
  -x c spirv_reflect.c \
  -x c texture.c \
  -x c image.c \
  -x c rendering.c \
  -x c utils.c \
  -x c renderer.c \
  -x c mesh.c \
  -x c buffer.c \
  -x c device.c \
  -x c window.c \
  -x c fluo_nif.c \
  -x c++ vma_impl.cpp \
  -o ../priv/libfluo_nif.so \
  -lvulkan -lglfw

cd ../shaders

glslc shader.frag -o frag.spv
glslc shader.vert -o vert.spv
glslc texture.frag -o texture.spv
