APP_PRIV_DIR := priv
LIB_NAME := libfluo_nif
SO := $(APP_PRIV_DIR)/$(LIB_NAME).so
GLSL_SRC := fluo.glsl
GLSL_DST := $(APP_PRIV_DIR)/fluo.glsl

CXX := g++
CXXFLAGS := -fPIC -Wall
LDFLAGS := -shared
LDLIBS := -lvulkan -lglfw
INCLUDES := -I./c_src/erl_nif -I./c_src/vendor

ifdef DEBUG
CXXFLAGS += -DDEBUG -g -O0
else
CXXFLAGS += -O3
endif

C_SRCS := \
	c_src/vendor/spirv_reflect.c \
	c_src/keys.c \
	c_src/mouse.c \
	c_src/image_layout.c \
	c_src/image_save.c \
	c_src/command.c \
	c_src/texture.c \
	c_src/params.c \
	c_src/image.c \
	c_src/utils.c \
	c_src/shaders.c \
	c_src/renderer.c \
	c_src/mesh.c \
	c_src/buffer.c \
	c_src/device.c \
	c_src/window.c \
	c_src/fluo_nif.c

CPP_SRCS := \
	c_src/vendor/vma_impl.cpp

all: $(SO) $(GLSL_DST)

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
