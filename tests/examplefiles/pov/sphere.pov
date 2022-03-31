#version 3.7;
#include "colors.inc"    

camera {
  sky <0,0,1>
  direction <-1,0,0>
  right <-4/3,0,0>
  location <-300,-800,600>
  look_at <0,0,0>     //Where camera is pointing
  angle 30
  rotate <0,0,-360*(clock+0.10)>
}

global_settings { ambient_light White }
background { color White }

light_source {
  <0,0,500>
  color White*2
  spotlight
  radius 100
  area_light <100, 0, 0>, <0, 0, 100>, 3, 3
  adaptive 1
  jitter
}

union {
  sphere {
    <0, 1, 2>, 2
    texture {
      pigment { color Yellow }
      finish {
        ambient 0.1
        brilliance 1.5
        diffuse .5
      }
      normal {
        scale 0.3
        wrinkles 0.4
      }
    }
  }
  cylinder{-x,x,.57,1 scale <1,2,3>}
}

#macro Tangent(position, row1, row2, row3)
  #if (vlength(row1)!=0 | row2.x <= 7)
    object {
      cylinder { 
        <0, 0, 0>, <0, 0, 1>, 0.7*50
        texture {
          pigment {
            rgb <0.7,0.0,0.3> transmit 0.7
          }
        }
      }
      matrix <row1.x, row2.x, row3.x,
              row1.y, row2.y, row3.y,
              row1.z, row2.z, row3.z,
              0, 0, 0>
      translate position
    }
  #end
#end

Tangent(<-200.0, 42, .75>)

#include concat("data-", str(frame_number, 1, 0), ".inc")

