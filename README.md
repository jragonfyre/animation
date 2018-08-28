# Animation

The ultimate goal of this project is to be a vector graphics based animation library. 
Right now, it is little more than a fairly fully featured vector graphics rasterization engine in a very early beta
phase. 
Before proceeding to the ultimate goal of rendering animations, the short term goal is to put together 
a sophisticated and efficient vector graphics rasterizer.

Current Features:
-----------------
1. Stroke and fill paths and contours described as a series of segments.
2. Segments can be one of the following mathematical objects:
   a. Line segment
   b. (Rational) quadratic Bezier
   c. (Rational) cubic Bezier
   d. Ellipticcal arcs
3. Stroking supports joins and caps. Plan to support at least the full SVG set. Presently implemented are:
   a. Round (circular) joins and caps
   b. Quadratic Bezier joins (essentially a miter join where the three points of the miter are used as the
      three control points of the bezier curve)
   c. Miter joins
   d. Bevel joins
   e. Flat and square caps
   f. Pointed caps
4. Filling is presently very generic (which probably isn't great for efficiency, but it's a beta), but does
   have support for generating gradients
