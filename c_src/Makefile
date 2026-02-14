APP_PRIV_DIR := ../priv
LIB_NAME := libfluo_nif
SO := $(APP_PRIV_DIR)/$(LIB_NAME).so
GLSL_SRC := fluo.glsl
GLSL_DST := $(APP_PRIV_DIR)/fluo.glsl

CXX := g++
CXXFLAGS := -fPIC -Wall
LDFLAGS := -shared
LDLIBS := -lvulkan -lglfw

INCLUDES := -I./erl_nif -I./vendor

ifdef DEBUG
CXXFLAGS += -DDEBUG -g -O0
else
CXXFLAGS += -O3
endif

C_SRCS := \
  vendor/spirv_reflect.c \
  texture.c \
  params.c \
  image.c \
  rendering.c \
  utils.c \
  shaders.c \
  renderer.c \
  mesh.c \
  buffer.c \
  device.c \
  window.c \
  fluo_nif.c

CPP_SRCS := \
  mesh_obj.cpp \
  vendor/vma_impl.cpp

all: $(SO)

$(SO): $(C_SRCS) $(CPP_SRCS)
	mkdir -p $(APP_PRIV_DIR)
	$(CXX) $(CXXFLAGS) $(INCLUDES) $(LDFLAGS) \
	  $(addprefix -x c ,$(C_SRCS)) \
	  $(addprefix -x c++ ,$(CPP_SRCS)) \
	  -o $@ \
	  $(LDLIBS)

$(GLSL_DST): $(GLSL_SRC)
	mkdir -p $(APP_PRIV_DIR)
	cp $(GLSL_SRC) $(GLSL_DST)

clean:
	rm -f $(SO) $(GLSL_DST)

.PHONY: all clean
