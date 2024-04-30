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

## 0.1.1.0
### Added
- Created ray data type
- Added constants for image width, height, viewport, camera etc...
- Introduced RayTracer data type to store constants (to avoid global variables and too many arguments to functions that need the constants)
- Added `rayColor` function that produces a cyan-blue gradient depending on the ray's Y-coordinate
- Imports to Linear.Metric, Linear.Vector, usage in vector scalar multiplication among other things
### Fixed
- Fixed image width and height being in the wrong order, causing the image to get wrong dimensions
- Fixed viewport width and height being mixed up