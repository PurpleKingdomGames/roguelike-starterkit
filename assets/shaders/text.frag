#version 300 es

precision mediump float;

uniform sampler2D SRC_CHANNEL;

vec4 CHANNEL_0;
vec4 COLOR;
vec2 UV;
vec2 SIZE;

//<indigo-fragment>
layout (std140) uniform RogueLikeTextData {
  vec3 FOREGROUND;
  vec4 BACKGROUND;
  vec4 MASK;
};

void fragment(){

  if(CHANNEL_0 == MASK) {
    COLOR = BACKGROUND;
  } else {
    COLOR = vec4(CHANNEL_0.rgb * (FOREGROUND.rgb * CHANNEL_0.a), CHANNEL_0.a);
  }

}
//</indigo-fragment>
