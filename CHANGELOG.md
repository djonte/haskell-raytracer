# Changelog for `haskell-raytracer`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.1.0.0 - 2024-04-26
- Project Stack init
- Added HIP as a dependancy
    - Generated simple gradient image `grad-mix-brighter.png` (and a few more)

## 0.1.0.1 - 2024-04-26
- Added author name to various places
- Updated readme to include run instructions

## 0.1.0.2 - 2024-04-26
- Fixed run instructions

## 0.1.1.0 - 2024-04-30
### Added
- Created ray data type
- Added constants for image width, height, viewport, camera etc...
- Introduced RayTracer data type to store constants (to avoid global variables and too many arguments to functions that need the constants)
- Added `rayColor` function that produces a cyan-blue gradient depending on the ray's Y-coordinate
- Imports to Linear.Metric, Linear.Vector, usage in vector scalar multiplication among other things
### Fixed
- Fixed image width and height being in the wrong order, causing the image to get wrong dimensions
- Fixed viewport width and height being mixed up


## 0.1.2.0 - 2024-05-03
### Added
- Created hitSphere function. The function takes a circle formed by a center point and a radius and calculates whether a given ray intersects with the circle
### Changed
- Renamed some functions and variables to obey Haskell's naming conventions
- Moved reusable functions to Lib.hs, constants and IO stays in Main.hs

## 0.1.3.0 - 2024-05-03
### Added
- Visualization of surface normals by coloring sphere as a function of sphere's outward facing normals x,y,z-positions
    - 4 picture depicting:
    - sphere colored blue as a function of varying z position in outward facing normal
    - sphere colored green as a function of varying y position in outward facing normal
    - sphere colored red as a function of varying x position in outward facing normal
    - sphere colored rgb as a function of varying x,y,z positions in outward facing normal

## 0.1.4.0 - 2024-05-13
### Added
- Object class with one function, hit
- Sphere data type (Object instance) that implements hit
- Hit normals are adjusted depending on if the hit was from the inside or outside of sphere, this is also stored in the HitRecord data type
- Hittable list implemented, hit function implemented on the list

## 0.1.5.0 - 2024-05-17
### Added
- World HitList to Lib and Main
- Intervals
- Moved constants and rendering logic to Camera.hs
- Antialiasing

### Fixed
- Recursive error because of variable naming inconsistencies