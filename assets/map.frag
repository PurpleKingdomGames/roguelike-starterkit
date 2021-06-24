#version 300 es

precision mediump float;

uniform sampler2D SRC_CHANNEL;
vec4 CHANNEL_0;
vec4 COLOR;
vec2 SIZE;
vec2 UV;
vec2 CHANNEL_0_TEXTURE_COORDS;
float TIME;

//<indigo-fragment>
layout (std140) uniform MapData {
  vec2 MAP_SIZE;
  vec2 CHAR_SIZE;
  float[9] CHARS;
};

in vec2 TILEMAP_TL_TEX_COORDS;
in vec2 ONE_TEXEL;
in vec2 TEXTURE_SIZE;

void fragment() {

  // Which grid square am I in on the map? e.g. 3x3, coords (1,1)
  vec2 gridSquare = UV * MAP_SIZE;

  // Which sequential box is that? e.g. 4 of 9
  int index = int(floor(gridSquare.y) * MAP_SIZE.x + floor(gridSquare.x));

  // Which character is that? e.g. position 4 in the array is for char 64, which is '@'
  int charIndex = int(CHARS[index]);

  // Where on the texture is the top left of the relevant character cell?
  float cellX = float(charIndex % 16) / 16.0;
  float cellY = floor(float(charIndex) / 16.0) * (1.0 / 16.0);
  vec2 cell = vec2(cellX, cellY);

  // What are the relative UV coords?
  vec2 tileSize = ONE_TEXEL * 10.0;
  vec2 relUV = TILEMAP_TL_TEX_COORDS + (cell * TEXTURE_SIZE) + (tileSize * fract(gridSquare));

  COLOR = texture(SRC_CHANNEL, relUV);
  
}
//</indigo-fragment>
